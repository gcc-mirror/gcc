(* Auto-generate ARM Neon intrinsics header file.
   Copyright (C) 2006, 2007, 2009 Free Software Foundation, Inc.
   Contributed by CodeSourcery.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3, or (at your option) any later
   version.

   GCC is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.

   This is an O'Caml program.  The O'Caml compiler is available from:

     http://caml.inria.fr/

   Or from your favourite OS's friendly packaging system. Tested with version
   3.09.2, though other versions will probably work too.

   Compile with:
     ocamlc -c neon.ml
     ocamlc -o neon-gen neon.cmo neon-gen.ml

   Run with:
     ./neon-gen > arm_neon.h
*)

open Neon

(* The format codes used in the following functions are documented at:
     http://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html\
     #6_printflikefunctionsforprettyprinting
   (one line, remove the backslash.)
*)

(* Following functions can be used to approximate GNU indentation style.  *)
let start_function () =
  Format.printf "@[<v 0>";
  ref 0

let end_function nesting =
  match !nesting with
    0 -> Format.printf "@;@;@]"
  | _ -> failwith ("Bad nesting (ending function at level "
                   ^ (string_of_int !nesting) ^ ")")

let open_braceblock nesting =
  begin match !nesting with
    0 -> Format.printf "@,@<0>{@[<v 2>@,"
  | _ -> Format.printf "@,@[<v 2>  @<0>{@[<v 2>@,"
  end;
  incr nesting

let close_braceblock nesting =
  decr nesting;
  match !nesting with
    0 -> Format.printf "@]@,@<0>}"
  | _ -> Format.printf "@]@,@<0>}@]"

let print_function arity fnname body =
  let ffmt = start_function () in
  Format.printf "__extension__ static __inline ";
  let inl = "__attribute__ ((__always_inline__))" in
  begin match arity with
    Arity0 ret ->
      Format.printf "%s %s@,%s (void)" (string_of_vectype ret) inl fnname
  | Arity1 (ret, arg0) ->
      Format.printf "%s %s@,%s (%s __a)" (string_of_vectype ret) inl fnname
                                        (string_of_vectype arg0)
  | Arity2 (ret, arg0, arg1) ->
      Format.printf "%s %s@,%s (%s __a, %s __b)"
        (string_of_vectype ret) inl fnname (string_of_vectype arg0)
	(string_of_vectype arg1)
  | Arity3 (ret, arg0, arg1, arg2) ->
      Format.printf "%s %s@,%s (%s __a, %s __b, %s __c)"
        (string_of_vectype ret) inl fnname (string_of_vectype arg0)
	(string_of_vectype arg1) (string_of_vectype arg2)
  | Arity4 (ret, arg0, arg1, arg2, arg3) ->
      Format.printf "%s %s@,%s (%s __a, %s __b, %s __c, %s __d)"
        (string_of_vectype ret) inl fnname (string_of_vectype arg0)
	(string_of_vectype arg1) (string_of_vectype arg2)
        (string_of_vectype arg3)
  end;
  open_braceblock ffmt;
  let rec print_lines = function
    [] -> ()
  | [line] -> Format.printf "%s" line
  | line::lines -> Format.printf "%s@," line; print_lines lines in
  print_lines body;
  close_braceblock ffmt;
  end_function ffmt

let return_by_ptr features = List.mem ReturnPtr features

let union_string num elts base =
  let itype = inttype_for_array num elts in
  let iname = string_of_inttype itype
  and sname = string_of_vectype (T_arrayof (num, elts)) in
  Printf.sprintf "union { %s __i; %s __o; } %s" sname iname base

let rec signed_ctype = function
    T_uint8x8 | T_poly8x8 -> T_int8x8
  | T_uint8x16 | T_poly8x16 -> T_int8x16
  | T_uint16x4 | T_poly16x4 -> T_int16x4
  | T_uint16x8 | T_poly16x8 -> T_int16x8
  | T_uint32x2 -> T_int32x2
  | T_uint32x4 -> T_int32x4
  | T_uint64x1 -> T_int64x1
  | T_uint64x2 -> T_int64x2
  (* Cast to types defined by mode in arm.c, not random types pulled in from
     the <stdint.h> header in use. This fixes incompatible pointer errors when
     compiling with C++.  *)
  | T_uint8 | T_int8 -> T_intQI
  | T_uint16 | T_int16 -> T_intHI
  | T_uint32 | T_int32 -> T_intSI
  | T_uint64 | T_int64 -> T_intDI
  | T_poly8 -> T_intQI
  | T_poly16 -> T_intHI
  | T_arrayof (n, elt) -> T_arrayof (n, signed_ctype elt)
  | T_ptrto elt -> T_ptrto (signed_ctype elt)
  | T_const elt -> T_const (signed_ctype elt)
  | x -> x

let add_cast ctype cval =
  let stype = signed_ctype ctype in
  if ctype <> stype then
    Printf.sprintf "(%s) %s" (string_of_vectype stype) cval
  else
    cval

let cast_for_return to_ty = "(" ^ (string_of_vectype to_ty) ^ ")"

(* Return a tuple of a list of declarations to go at the start of the function,
   and a list of statements needed to return THING.  *)
let return arity return_by_ptr thing =
  match arity with
    Arity0 (ret) | Arity1 (ret, _) | Arity2 (ret, _, _) | Arity3 (ret, _, _, _)
  | Arity4 (ret, _, _, _, _) ->
    match ret with
      T_arrayof (num, vec) ->
        if return_by_ptr then
          let sname = string_of_vectype ret in
          [Printf.sprintf "%s __rv;" sname],
          [thing ^ ";"; "return __rv;"]
        else
          let uname = union_string num vec "__rv" in
          [uname ^ ";"], ["__rv.__o = " ^ thing ^ ";"; "return __rv.__i;"]
    | T_void -> [], [thing ^ ";"]
    | _ ->
        [], ["return " ^ (cast_for_return ret) ^ thing ^ ";"]

let rec element_type ctype =
  match ctype with
    T_arrayof (_, v) -> element_type v
  | _ -> ctype

let params return_by_ptr ps =
  let pdecls = ref [] in
  let ptype t p =
    match t with
      T_arrayof (num, elts) ->
        let uname = union_string num elts (p ^ "u") in
        let decl = Printf.sprintf "%s = { %s };" uname p in
        pdecls := decl :: !pdecls;
        p ^ "u.__o"
    | _ -> add_cast t p in
  let plist = match ps with
    Arity0 _ -> []
  | Arity1 (_, t1) -> [ptype t1 "__a"]
  | Arity2 (_, t1, t2) -> [ptype t1 "__a"; ptype t2 "__b"]
  | Arity3 (_, t1, t2, t3) -> [ptype t1 "__a"; ptype t2 "__b"; ptype t3 "__c"]
  | Arity4 (_, t1, t2, t3, t4) ->
      [ptype t1 "__a"; ptype t2 "__b"; ptype t3 "__c"; ptype t4 "__d"] in
  match ps with
    Arity0 ret | Arity1 (ret, _) | Arity2 (ret, _, _) | Arity3 (ret, _, _, _)
  | Arity4 (ret, _, _, _, _) ->
      if return_by_ptr then
        !pdecls, add_cast (T_ptrto (element_type ret)) "&__rv.val[0]" :: plist
      else
        !pdecls, plist

let modify_params features plist =
  let is_flipped =
    List.exists (function Flipped _ -> true | _ -> false) features in
  if is_flipped then
    match plist with
      [ a; b ] -> [ b; a ]
    | _ ->
      failwith ("Don't know how to flip args " ^ (String.concat ", " plist))
  else
    plist

(* !!! Decide whether to add an extra information word based on the shape
   form.  *)
let extra_word shape features paramlist bits =
  let use_word =
    match shape with
      All _ | Long | Long_noreg _ | Wide | Wide_noreg _ | Narrow
    | By_scalar _ | Wide_scalar | Wide_lane | Binary_imm _ | Long_imm
    | Narrow_imm -> true
    | _ -> List.mem InfoWord features
  in
    if use_word then
      paramlist @ [string_of_int bits]
    else
      paramlist

(* Bit 0 represents signed (1) vs unsigned (0), or float (1) vs poly (0).
   Bit 1 represents floats & polynomials (1), or ordinary integers (0).
   Bit 2 represents rounding (1) vs none (0).  *)
let infoword_value elttype features =
  let bits01 =
    match elt_class elttype with
      Signed | ConvClass (Signed, _) | ConvClass (_, Signed) -> 0b001
    | Poly -> 0b010
    | Float -> 0b011
    | _ -> 0b000
  and rounding_bit = if List.mem Rounding features then 0b100 else 0b000 in
  bits01 lor rounding_bit

(* "Cast" type operations will throw an exception in mode_of_elt (actually in
   elt_width, called from there). Deal with that here, and generate a suffix
   with multiple modes (<to><from>).  *)
let rec mode_suffix elttype shape =
  try
    let mode = mode_of_elt elttype shape in
    string_of_mode mode
  with MixedMode (dst, src) ->
    let dstmode = mode_of_elt dst shape
    and srcmode = mode_of_elt src shape in
    string_of_mode dstmode ^ string_of_mode srcmode

let print_variant opcode features shape name (ctype, asmtype, elttype) =
  let bits = infoword_value elttype features in
  let modesuf = mode_suffix elttype shape in
  let return_by_ptr = return_by_ptr features in
  let pdecls, paramlist = params return_by_ptr ctype in
  let paramlist' = modify_params features paramlist in
  let paramlist'' = extra_word shape features paramlist' bits in
  let parstr = String.concat ", " paramlist'' in
  let builtin = Printf.sprintf "__builtin_neon_%s%s (%s)"
                  (builtin_name features name) modesuf parstr in
  let rdecls, stmts = return ctype return_by_ptr builtin in
  let body = pdecls @ rdecls @ stmts
  and fnname = (intrinsic_name name) ^ "_" ^ (string_of_elt elttype) in
  print_function ctype fnname body

(* When this function processes the element types in the ops table, it rewrites
   them in a list of tuples (a,b,c):
     a : C type as an "arity", e.g. Arity1 (T_poly8x8, T_poly8x8)
     b : Asm type : a single, processed element type, e.g. P16. This is the
         type which should be attached to the asm opcode.
     c : Variant type : the unprocessed type for this variant (e.g. in add
         instructions which don't care about the sign, b might be i16 and c
         might be s16.)
*)

let print_op (opcode, features, shape, name, munge, types) =
  let sorted_types = List.sort compare types in
  let munged_types = List.map
    (fun elt -> let c, asm = munge shape elt in c, asm, elt) sorted_types in
  List.iter
    (fun variant -> print_variant opcode features shape name variant)
    munged_types

let print_ops ops =
  List.iter print_op ops

(* Output type definitions. Table entries are:
     cbase : "C" name for the type.
     abase : "ARM" base name for the type (i.e. int in int8x8_t).
     esize : element size.
     enum : element count.
*)

let deftypes () =
  let typeinfo = [
    (* Doubleword vector types.  *)
    "__builtin_neon_qi", "int", 8, 8;
    "__builtin_neon_hi", "int", 16, 4;
    "__builtin_neon_si", "int", 32, 2;
    "__builtin_neon_di", "int", 64, 1;
    "__builtin_neon_sf", "float", 32, 2;
    "__builtin_neon_poly8", "poly", 8, 8;
    "__builtin_neon_poly16", "poly", 16, 4;
    "__builtin_neon_uqi", "uint", 8, 8;
    "__builtin_neon_uhi", "uint", 16, 4;
    "__builtin_neon_usi", "uint", 32, 2;
    "__builtin_neon_udi", "uint", 64, 1;

    (* Quadword vector types.  *)
    "__builtin_neon_qi", "int", 8, 16;
    "__builtin_neon_hi", "int", 16, 8;
    "__builtin_neon_si", "int", 32, 4;
    "__builtin_neon_di", "int", 64, 2;
    "__builtin_neon_sf", "float", 32, 4;
    "__builtin_neon_poly8", "poly", 8, 16;
    "__builtin_neon_poly16", "poly", 16, 8;
    "__builtin_neon_uqi", "uint", 8, 16;
    "__builtin_neon_uhi", "uint", 16, 8;
    "__builtin_neon_usi", "uint", 32, 4;
    "__builtin_neon_udi", "uint", 64, 2
  ] in
  List.iter
    (fun (cbase, abase, esize, enum) ->
      let attr =
        match enum with
          1 -> ""
        | _ -> Printf.sprintf "\t__attribute__ ((__vector_size__ (%d)))"
                              (esize * enum / 8) in
      Format.printf "typedef %s %s%dx%d_t%s;@\n" cbase abase esize enum attr)
    typeinfo;
  Format.print_newline ();
  (* Extra types not in <stdint.h>.  *)
  Format.printf "typedef __builtin_neon_sf float32_t;\n";
  Format.printf "typedef __builtin_neon_poly8 poly8_t;\n";
  Format.printf "typedef __builtin_neon_poly16 poly16_t;\n"

(* Output structs containing arrays, for load & store instructions etc.  *)

let arrtypes () =
  let typeinfo = [
    "int", 8;    "int", 16;
    "int", 32;   "int", 64;
    "uint", 8;   "uint", 16;
    "uint", 32;  "uint", 64;
    "float", 32; "poly", 8;
    "poly", 16
  ] in
  let writestruct elname elsize regsize arrsize =
    let elnum = regsize / elsize in
    let structname =
      Printf.sprintf "%s%dx%dx%d_t" elname elsize elnum arrsize in
    let sfmt = start_function () in
    Format.printf "typedef struct %s" structname;
    open_braceblock sfmt;
    Format.printf "%s%dx%d_t val[%d];" elname elsize elnum arrsize;
    close_braceblock sfmt;
    Format.printf " %s;" structname;
    end_function sfmt;
  in
    for n = 2 to 4 do
      List.iter
        (fun (elname, elsize) ->
          writestruct elname elsize 64 n;
          writestruct elname elsize 128 n)
        typeinfo
    done

let print_lines = List.iter (fun s -> Format.printf "%s@\n" s)

(* Do it.  *)

let _ =
  print_lines [
"/* ARM NEON intrinsics include file. This file is generated automatically";
"   using neon-gen.ml.  Please do not edit manually.";
"";
"   Copyright (C) 2006, 2007, 2009 Free Software Foundation, Inc.";
"   Contributed by CodeSourcery.";
"";
"   This file is part of GCC.";
"";
"   GCC is free software; you can redistribute it and/or modify it";
"   under the terms of the GNU General Public License as published";
"   by the Free Software Foundation; either version 3, or (at your";
"   option) any later version.";
"";
"   GCC is distributed in the hope that it will be useful, but WITHOUT";
"   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY";
"   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public";
"   License for more details.";
"";
"   Under Section 7 of GPL version 3, you are granted additional";
"   permissions described in the GCC Runtime Library Exception, version";
"   3.1, as published by the Free Software Foundation.";
"";
"   You should have received a copy of the GNU General Public License and";
"   a copy of the GCC Runtime Library Exception along with this program;";
"   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see";
"   <http://www.gnu.org/licenses/>.  */";
"";
"#ifndef _GCC_ARM_NEON_H";
"#define _GCC_ARM_NEON_H 1";
"";
"#ifndef __ARM_NEON__";
"#error You must enable NEON instructions (e.g. -mfloat-abi=softfp -mfpu=neon) to use arm_neon.h";
"#else";
"";
"#ifdef __cplusplus";
"extern \"C\" {";
"#endif";
"";
"#include <stdint.h>";
""];
  deftypes ();
  arrtypes ();
  Format.print_newline ();
  print_ops ops;
  Format.print_newline ();
  print_ops reinterp;
  print_lines [
"#ifdef __cplusplus";
"}";
"#endif";
"#endif";
"#endif"]
