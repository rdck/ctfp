(******************************************************************************)
(* CATEGORY THEORY FOR PROGRAMMERS                                            *)
(******************************************************************************)

[@@@ocaml.warnerror  "-unused-value-declaration"]
[@@@ocaml.warning    "-unused-value-declaration"]

open Core

let _ = Random.init 0

(******************************************************************************)
(* CHAPTER 1                                                                  *)
(******************************************************************************)

(* Exercise: Implement the identity function. *)

let id (x : 't) = x

(* Exercise: Implement the composition function. *)

let (|||) (g : 'b -> 'c) (f : 'a -> 'b) (a : 'a) = g (f a)

(* Exercise: Test that your implementation of composition respects identity. *)

let composition_respects_id () =
  let f = (+) (Random.int Int.max_value) in
  let x = Random.int Int.max_value in
  assert (f x = (f ||| id) x) ;
  assert (f x = (id ||| f) x) ;
  Printf.printf "passing\n"

(******************************************************************************)
(* CHAPTER 2                                                                  *)
(******************************************************************************)

(* Exercise: Implement memoization. *)

let memoize (m : 'k Base.Hashtbl.Key.t) (f : 'k -> 'v) =
  let table = Hashtbl.create m in fun (k : 'k) ->
    match Hashtbl.find table k with
    | Some v -> v
    | None -> let r = f k in Hashtbl.set table ~key:k ~data:r ; r

let test_memoized () =
  let slow_id (x : int) = Core_unix.sleep 2 ; x in
  let memoized = memoize (module Int) slow_id in
  let xs = List.init 8 ~f:id in
  let compute _ =
    Printf.printf "computing memoized slow_id 0...\t\t%!" ;
    Printf.printf "result: %d\n" (memoized 0) in
  List.iter xs ~f:compute

(* Exercise: Implement all possible boolean functions. *)

let _ =
  let _idb  (b : bool) = id b   in
  let _not  (b : bool) = not b  in
  let _tt   (_ : bool) = true   in
  let _ff   (_ : bool) = false  in
  ()

(******************************************************************************)
(* CHAPTER 4                                                                  *)
(******************************************************************************)

module type FOUR = sig

  type ('a, 'b) partial = 'a -> 'b option
  type binop = (float, float) partial

  val compose : ('b, 'c) partial -> ('a, 'b) partial -> ('a, 'c) partial

  val reciprocal      : binop
  val root            : binop
  val root_reciprocal : binop

end

module Four : FOUR = struct

  type ('a, 'b) partial = 'a -> 'b option
  type binop = (float, float) partial

  let (=) = Float.(=)
  let (<) = Float.(<)
  let (/) = Float.(/)

  let compose g f x =
    match (f x) with
    | Some r  -> g r
    | None    -> None

  let reciprocal x =
    match x = 0.0 with
    | true  -> None
    | false -> Some (1.0 / x)

  let root x =
    match x < 0.0 with
    | true  -> None
    | false -> Some (sqrt x)

  let root_reciprocal = compose root reciprocal

  let test_chapter_four () =
    let (=) = Option.equal Float.equal in
    assert (root_reciprocal 0.0           = None    ) ;
    assert (root_reciprocal (0.0 -. 4.0)  = None    ) ;
    assert (root_reciprocal (1.0 /. 4.0)  = Some 2.0) ;
    Printf.printf "passing\n"

end
