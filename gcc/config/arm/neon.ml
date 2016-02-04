(* Common code for ARM NEON header file, documentation and test case
   generators.

   Copyright (C) 2006-2016 Free Software Foundation, Inc.
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
   <http://www.gnu.org/licenses/>.  *)

(* Shorthand types for vector elements.  *)
type elts = S8 | S16 | S32 | S64 | F16 | F32 | U8 | U16 | U32 | U64 | P8 | P16
          | P64 | P128 | I8 | I16 | I32 | I64 | B8 | B16 | B32 | B64 | Conv of elts * elts
          | Cast of elts * elts | NoElts

type eltclass = Signed | Unsigned | Float | Poly | Int | Bits
	      | ConvClass of eltclass * eltclass | NoType

(* These vector types correspond directly to C types.  *)
type vectype = T_int8x8    | T_int8x16
             | T_int16x4   | T_int16x8
	     | T_int32x2   | T_int32x4
	     | T_int64x1   | T_int64x2
	     | T_uint8x8   | T_uint8x16
	     | T_uint16x4  | T_uint16x8
	     | T_uint32x2  | T_uint32x4
	     | T_uint64x1  | T_uint64x2
	     | T_float16x4
	     | T_float32x2 | T_float32x4
	     | T_poly8x8   | T_poly8x16
	     | T_poly16x4  | T_poly16x8
	     | T_immediate of int * int
             | T_int8      | T_int16
             | T_int32     | T_int64
             | T_uint8     | T_uint16
             | T_uint32    | T_uint64
             | T_poly8     | T_poly16
             | T_poly64    | T_poly64x1
             | T_poly64x2  | T_poly128
             | T_float16   | T_float32
             | T_arrayof of int * vectype
             | T_ptrto of vectype | T_const of vectype
             | T_void      | T_intQI
             | T_intHI     | T_intSI
             | T_intDI     | T_intTI
             | T_floatHF   | T_floatSF

(* The meanings of the following are:
     TImode : "Tetra", two registers (four words).
     EImode : "hExa", three registers (six words).
     OImode : "Octa", four registers (eight words).
     CImode : "dodeCa", six registers (twelve words).
     XImode : "heXadeca", eight registers (sixteen words).
*)

type inttype = B_TImode | B_EImode | B_OImode | B_CImode | B_XImode

type shape_elt = Dreg | Qreg | Corereg | Immed | VecArray of int * shape_elt
               | PtrTo of shape_elt | CstPtrTo of shape_elt
	       (* These next ones are used only in the test generator.  *)
	       | Element_of_dreg	(* Used for "lane" variants.  *)
	       | Element_of_qreg	(* Likewise.  *)
	       | All_elements_of_dreg	(* Used for "dup" variants.  *)
	       | Alternatives of shape_elt list (* Used for multiple valid operands *)

type shape_form = All of int * shape_elt
                | Long
		| Long_noreg of shape_elt
		| Wide
		| Wide_noreg of shape_elt
		| Narrow
                | Long_imm
                | Narrow_imm
                | Binary_imm of shape_elt
                | Use_operands of shape_elt array
                | By_scalar of shape_elt
                | Unary_scalar of shape_elt
                | Wide_lane
                | Wide_scalar
                | Pair_result of shape_elt

type arity = Arity0 of vectype
           | Arity1 of vectype * vectype
	   | Arity2 of vectype * vectype * vectype
	   | Arity3 of vectype * vectype * vectype * vectype
           | Arity4 of vectype * vectype * vectype * vectype * vectype

type vecmode = V8QI | V4HI | V4HF |V2SI | V2SF | DI
             | V16QI | V8HI | V4SI | V4SF | V2DI | TI
             | QI | HI | SI | SF

type opcode =
  (* Binary ops.  *)
    Vadd
  | Vmul
  | Vmla
  | Vmls
  | Vfma
  | Vfms
  | Vsub
  | Vceq
  | Vcge
  | Vcgt
  | Vcle
  | Vclt
  | Vcage
  | Vcagt
  | Vcale
  | Vcalt
  | Vtst
  | Vabd
  | Vaba
  | Vmax
  | Vmin
  | Vpadd
  | Vpada
  | Vpmax
  | Vpmin
  | Vrecps
  | Vrsqrts
  | Vshl
  | Vshr_n
  | Vshl_n
  | Vsra_n
  | Vsri
  | Vsli
  (* Logic binops.  *)
  | Vand
  | Vorr
  | Veor
  | Vbic
  | Vorn
  | Vbsl
  (* Ops with scalar.  *)
  | Vmul_lane
  | Vmla_lane
  | Vmls_lane
  | Vmul_n
  | Vmla_n
  | Vmls_n
  | Vmull_n
  | Vmull_lane
  | Vqdmull_n
  | Vqdmull_lane
  | Vqdmulh_n
  | Vqdmulh_lane
  (* Unary ops.  *)
  | Vrintn
  | Vrinta
  | Vrintp
  | Vrintm
  | Vrintz
  | Vabs
  | Vneg
  | Vcls
  | Vclz
  | Vcnt
  | Vrecpe
  | Vrsqrte
  | Vmvn
  (* Vector extract.  *)
  | Vext
  (* Reverse elements.  *)
  | Vrev64
  | Vrev32
  | Vrev16
  (* Transposition ops.  *)
  | Vtrn
  | Vzip
  | Vuzp
  (* Loads and stores (VLD1/VST1/VLD2...), elements and structures.  *)
  | Vldx of int
  | Vstx of int
  | Vldx_lane of int
  | Vldx_dup of int
  | Vstx_lane of int
  (* Set/extract lanes from a vector.  *)
  | Vget_lane
  | Vset_lane
  (* Initialize vector from bit pattern.  *)
  | Vcreate
  (* Set all lanes to same value.  *)
  | Vdup_n
  | Vmov_n  (* Is this the same?  *)
  (* Duplicate scalar to all lanes of vector.  *)
  | Vdup_lane
  (* Combine vectors.  *)
  | Vcombine
  (* Get quadword high/low parts.  *)
  | Vget_high
  | Vget_low
  (* Convert vectors.  *)
  | Vcvt
  | Vcvt_n
  (* Narrow/lengthen vectors.  *)
  | Vmovn
  | Vmovl
  (* Table lookup.  *)
  | Vtbl of int
  | Vtbx of int
  (* Reinterpret casts.  *)
  | Vreinterp

let rev_elems revsize elsize nelts _ =
  let mask = (revsize / elsize) - 1 in
  let arr = Array.init nelts
    (fun i -> i lxor mask) in
  Array.to_list arr

let permute_range i stride nelts increment =
  let rec build i = function
    0 -> []
  | nelts -> i :: (i + stride) :: build (i + increment) (pred nelts) in
  build i nelts

(* Generate a list of integers suitable for vzip.  *)
let zip_range i stride nelts = permute_range i stride nelts 1

(* Generate a list of integers suitable for vunzip.  *)
let uzip_range i stride nelts = permute_range i stride nelts 4

(* Generate a list of integers suitable for trn.  *)
let trn_range i stride nelts = permute_range i stride nelts 2

let zip_elems _ nelts part =
  match part with
    `lo -> zip_range 0 nelts (nelts / 2)
  | `hi -> zip_range (nelts / 2) nelts (nelts / 2)

let uzip_elems _ nelts part =
  match part with
    `lo -> uzip_range 0 2 (nelts / 2)
  | `hi -> uzip_range 1 2 (nelts / 2)

let trn_elems _ nelts part =
  match part with
    `lo -> trn_range 0 nelts (nelts / 2)
  | `hi -> trn_range 1 nelts (nelts / 2)

(* Features used for documentation, to distinguish between some instruction
   variants, and to signal special requirements (e.g. swapping arguments).  *)

type features =
    Halving
  | Rounding
  | Saturating
  | Dst_unsign
  | High_half
  | Doubling
  | Flipped of string  (* Builtin name to use with flipped arguments.  *)
  | InfoWord  (* Pass an extra word for signage/rounding etc. (always passed
                 for All _, Long, Wide, Narrow shape_forms.  *)
    (* Implement builtin as shuffle.  The parameter is a function which returns
       masks suitable for __builtin_shuffle: arguments are (element size,
       number of elements, high/low part selector).  *)
  | Use_shuffle of (int -> int -> [`lo|`hi] -> int list)
    (* A specification as to the shape of instruction expected upon
       disassembly, used if it differs from the shape used to build the
       intrinsic prototype.  Multiple entries in the constructor's argument
       indicate that the intrinsic expands to more than one assembly
       instruction, each with a corresponding shape specified here.  *)
  | Disassembles_as of shape_form list
  | Builtin_name of string  (* Override the name of the builtin.  *)
    (* Override the name of the instruction.  If more than one name
       is specified, it means that the instruction can have any of those
       names.  *)
  | Instruction_name of string list
    (* Mark that the intrinsic yields no instructions, or expands to yield
       behavior that the test generator cannot test.  *)
  | No_op
    (* Mark that the intrinsic has constant arguments that cannot be set
       to the defaults (zero for pointers and one otherwise) in the test
       cases.  The function supplied must return the integer to be written
       into the testcase for the argument number (0-based) supplied to it.  *)
  | Const_valuator of (int -> int)
  | Fixed_vector_reg
  | Fixed_core_reg
    (* Mark that the intrinsic requires __ARM_FEATURE_string to be defined.  *)
  | Requires_feature of string
    (* Mark that the intrinsic requires a particular architecture version.  *)
  | Requires_arch of int
    (* Mark that the intrinsic requires a particular bit in __ARM_FP to
    be set.   *)
  | Requires_FP_bit of int
    (* Compiler optimization level for the test.  *)
  | Compiler_optim of string

exception MixedMode of elts * elts

let rec elt_width = function
    S8 | U8 | P8 | I8 | B8 -> 8
  | S16 | U16 | P16 | I16 | B16 | F16 -> 16
  | S32 | F32 | U32 | I32 | B32 -> 32
  | S64 | U64 | P64 | I64 | B64 -> 64
  | P128 -> 128
  | Conv (a, b) ->
      let wa = elt_width a and wb = elt_width b in
      if wa = wb then wa else raise (MixedMode (a, b))
  | Cast (a, b) -> raise (MixedMode (a, b))
  | NoElts -> failwith "No elts"

let rec elt_class = function
    S8 | S16 | S32 | S64 -> Signed
  | U8 | U16 | U32 | U64 -> Unsigned
  | P8 | P16 | P64 | P128 -> Poly
  | F16 | F32 -> Float
  | I8 | I16 | I32 | I64 -> Int
  | B8 | B16 | B32 | B64 -> Bits
  | Conv (a, b) | Cast (a, b) -> ConvClass (elt_class a, elt_class b)
  | NoElts -> NoType

let elt_of_class_width c w =
  match c, w with
    Signed, 8 -> S8
  | Signed, 16 -> S16
  | Signed, 32 -> S32
  | Signed, 64 -> S64
  | Float, 16 -> F16
  | Float, 32 -> F32
  | Unsigned, 8 -> U8
  | Unsigned, 16 -> U16
  | Unsigned, 32 -> U32
  | Unsigned, 64 -> U64
  | Poly, 8 -> P8
  | Poly, 16 -> P16
  | Poly, 64 -> P64
  | Poly, 128 -> P128
  | Int, 8 -> I8
  | Int, 16 -> I16
  | Int, 32 -> I32
  | Int, 64 -> I64
  | Bits, 8 -> B8
  | Bits, 16 -> B16
  | Bits, 32 -> B32
  | Bits, 64 -> B64
  | _ -> failwith "Bad element type"

(* Return unsigned integer element the same width as argument.  *)
let unsigned_of_elt elt =
  elt_of_class_width Unsigned (elt_width elt)

let signed_of_elt elt =
  elt_of_class_width Signed (elt_width elt)

(* Return untyped bits element the same width as argument.  *)
let bits_of_elt elt =
  elt_of_class_width Bits (elt_width elt)

let non_signed_variant = function
    S8 -> I8
  | S16 -> I16
  | S32 -> I32
  | S64 -> I64
  | U8 -> I8
  | U16 -> I16
  | U32 -> I32
  | U64 -> I64
  | x -> x

let poly_unsigned_variant v =
  let elclass = match elt_class v with
    Poly -> Unsigned
  | x -> x in
  elt_of_class_width elclass (elt_width v)

let widen_elt elt =
  let w = elt_width elt
  and c = elt_class elt in
  elt_of_class_width c (w * 2)

let narrow_elt elt =
  let w = elt_width elt
  and c = elt_class elt in
  elt_of_class_width c (w / 2)

(* If we're trying to find a mode from a "Use_operands" instruction, use the
   last vector operand as the dominant mode used to invoke the correct builtin.
   We must stick to this rule in neon.md.  *)
let find_key_operand operands =
  let rec scan opno =
    match operands.(opno) with
      Qreg -> Qreg
    | Dreg -> Dreg
    | VecArray (_, Qreg) -> Qreg
    | VecArray (_, Dreg) -> Dreg
    | _ -> scan (opno-1)
  in
    scan ((Array.length operands) - 1)

(* Find a vecmode from a shape_elt ELT for an instruction with shape_form
   SHAPE.  For a Use_operands shape, if ARGPOS is passed then return the mode
   for the given argument position, else determine which argument to return a
   mode for automatically.  *)

let rec mode_of_elt ?argpos elt shape =
  let flt = match elt_class elt with
    Float | ConvClass(_, Float) -> true | _ -> false in
  let idx =
    match elt_width elt with
      8 -> 0 | 16 -> 1 | 32 -> 2 | 64 -> 3 | 128 -> 4
    | _ -> failwith "Bad element width"
  in match shape with
    All (_, Dreg) | By_scalar Dreg | Pair_result Dreg | Unary_scalar Dreg
  | Binary_imm Dreg | Long_noreg Dreg | Wide_noreg Dreg ->
      if flt then
        [| V8QI; V4HF; V2SF; DI |].(idx)
      else
        [| V8QI; V4HI; V2SI; DI |].(idx)
  | All (_, Qreg) | By_scalar Qreg | Pair_result Qreg | Unary_scalar Qreg
  | Binary_imm Qreg | Long_noreg Qreg | Wide_noreg Qreg ->
      [| V16QI; V8HI; if flt then V4SF else V4SI; V2DI; TI|].(idx)
  | All (_, (Corereg | PtrTo _ | CstPtrTo _)) ->
      [| QI; HI; if flt then SF else SI; DI |].(idx)
  | Long | Wide | Wide_lane | Wide_scalar
  | Long_imm ->
      [| V8QI; V4HI; V2SI; DI |].(idx)
  | Narrow | Narrow_imm -> [| V16QI; V8HI; V4SI; V2DI |].(idx)
  | Use_operands ops ->
      begin match argpos with
        None -> mode_of_elt ?argpos elt (All (0, (find_key_operand ops)))
      | Some pos -> mode_of_elt ?argpos elt (All (0, ops.(pos)))
      end
  | _ -> failwith "invalid shape"

(* Modify an element type dependent on the shape of the instruction and the
   operand number.  *)

let shapemap shape no =
  let ident = fun x -> x in
  match shape with
    All _ | Use_operands _ | By_scalar _ | Pair_result _ | Unary_scalar _
  | Binary_imm _ -> ident
  | Long | Long_noreg _ | Wide_scalar | Long_imm ->
      [| widen_elt; ident; ident |].(no)
  | Wide | Wide_noreg _ -> [| widen_elt; widen_elt; ident |].(no)
  | Wide_lane -> [| widen_elt; ident; ident; ident |].(no)
  | Narrow | Narrow_imm -> [| narrow_elt; ident; ident |].(no)

(* Register type (D/Q) of an operand, based on shape and operand number.  *)

let regmap shape no =
  match shape with
    All (_, reg) | Long_noreg reg | Wide_noreg reg -> reg
  | Long -> [| Qreg; Dreg; Dreg |].(no)
  | Wide -> [| Qreg; Qreg; Dreg |].(no)
  | Narrow -> [| Dreg; Qreg; Qreg |].(no)
  | Wide_lane -> [| Qreg; Dreg; Dreg; Immed |].(no)
  | Wide_scalar -> [| Qreg; Dreg; Corereg |].(no)
  | By_scalar reg -> [| reg; reg; Dreg; Immed |].(no)
  | Unary_scalar reg -> [| reg; Dreg; Immed |].(no)
  | Pair_result reg -> [| VecArray (2, reg); reg; reg |].(no)
  | Binary_imm reg -> [| reg; reg; Immed |].(no)
  | Long_imm -> [| Qreg; Dreg; Immed |].(no)
  | Narrow_imm -> [| Dreg; Qreg; Immed |].(no)
  | Use_operands these -> these.(no)

let type_for_elt shape elt no =
  let elt = (shapemap shape no) elt in
  let reg = regmap shape no in
  let rec type_for_reg_elt reg elt =
    match reg with
      Dreg ->
        begin match elt with
          S8 -> T_int8x8
        | S16 -> T_int16x4
        | S32 -> T_int32x2
        | S64 -> T_int64x1
        | U8 -> T_uint8x8
        | U16 -> T_uint16x4
        | U32 -> T_uint32x2
        | U64 -> T_uint64x1
        | P64 -> T_poly64x1
        | P128 -> T_poly128
        | F16 -> T_float16x4
        | F32 -> T_float32x2
        | P8 -> T_poly8x8
        | P16 -> T_poly16x4
        | _ -> failwith "Bad elt type for Dreg"
        end
    | Qreg ->
        begin match elt with
          S8 -> T_int8x16
        | S16 -> T_int16x8
        | S32 -> T_int32x4
        | S64 -> T_int64x2
        | U8 -> T_uint8x16
        | U16 -> T_uint16x8
        | U32 -> T_uint32x4
        | U64 -> T_uint64x2
        | F32 -> T_float32x4
        | P8 -> T_poly8x16
        | P16 -> T_poly16x8
        | P64 -> T_poly64x2
        | P128 -> T_poly128
        | _ -> failwith "Bad elt type for Qreg"
        end
    | Corereg ->
        begin match elt with
          S8 -> T_int8
        | S16 -> T_int16
        | S32 -> T_int32
        | S64 -> T_int64
        | U8 -> T_uint8
        | U16 -> T_uint16
        | U32 -> T_uint32
        | U64 -> T_uint64
        | P8 -> T_poly8
        | P16 -> T_poly16
        | P64 -> T_poly64
        | P128 -> T_poly128
        | F32 -> T_float32
        | _ -> failwith "Bad elt type for Corereg"
        end
    | Immed ->
        T_immediate (0, 0)
    | VecArray (num, sub) ->
        T_arrayof (num, type_for_reg_elt sub elt)
    | PtrTo x ->
        T_ptrto (type_for_reg_elt x elt)
    | CstPtrTo x ->
        T_ptrto (T_const (type_for_reg_elt x elt))
    (* Anything else is solely for the use of the test generator.  *)
    | _ -> assert false
  in
    type_for_reg_elt reg elt

(* Return size of a vector type, in bits.  *)
let vectype_size = function
    T_int8x8 | T_int16x4 | T_int32x2 | T_int64x1
  | T_uint8x8 | T_uint16x4 | T_uint32x2 | T_uint64x1
  | T_float32x2 | T_poly8x8 | T_poly64x1 | T_poly16x4 | T_float16x4 -> 64
  | T_int8x16 | T_int16x8 | T_int32x4 | T_int64x2
  | T_uint8x16 | T_uint16x8  | T_uint32x4  | T_uint64x2
  | T_float32x4 | T_poly8x16 | T_poly64x2 | T_poly16x8 -> 128
  | _ -> raise Not_found

let inttype_for_array num elttype =
  let eltsize = vectype_size elttype in
  let numwords = (num * eltsize) / 32 in
  match numwords with
    4 -> B_TImode
  | 6 -> B_EImode
  | 8 -> B_OImode
  | 12 -> B_CImode
  | 16 -> B_XImode
  | _ -> failwith ("no int type for size " ^ string_of_int numwords)

(* These functions return pairs of (internal, external) types, where "internal"
   types are those seen by GCC, and "external" are those seen by the assembler.
   These types aren't necessarily the same, since the intrinsics can munge more
   than one C type into each assembler opcode.  *)

let make_sign_invariant func shape elt =
  let arity, elt' = func shape elt in
  arity, non_signed_variant elt'

(* Don't restrict any types.  *)

let elts_same make_arity shape elt =
  let vtype = type_for_elt shape elt in
  make_arity vtype, elt

(* As sign_invar_*, but when sign matters.  *)
let elts_same_io_lane =
  elts_same (fun vtype -> Arity4 (vtype 0, vtype 0, vtype 1, vtype 2, vtype 3))

let elts_same_io =
  elts_same (fun vtype -> Arity3 (vtype 0, vtype 0, vtype 1, vtype 2))

let elts_same_2_lane =
  elts_same (fun vtype -> Arity3 (vtype 0, vtype 1, vtype 2, vtype 3))

let elts_same_3 = elts_same_2_lane

let elts_same_2 =
  elts_same (fun vtype -> Arity2 (vtype 0, vtype 1, vtype 2))

let elts_same_1 =
  elts_same (fun vtype -> Arity1 (vtype 0, vtype 1))

(* Use for signed/unsigned invariant operations (i.e. where the operation
   doesn't depend on the sign of the data.  *)

let sign_invar_io_lane = make_sign_invariant elts_same_io_lane
let sign_invar_io = make_sign_invariant elts_same_io
let sign_invar_2_lane = make_sign_invariant elts_same_2_lane
let sign_invar_2 = make_sign_invariant elts_same_2
let sign_invar_1 = make_sign_invariant elts_same_1

(* Sign-sensitive comparison.  *)

let cmp_sign_matters shape elt =
  let vtype = type_for_elt shape elt
  and rtype = type_for_elt shape (unsigned_of_elt elt) 0 in
  Arity2 (rtype, vtype 1, vtype 2), elt

(* Signed/unsigned invariant comparison.  *)

let cmp_sign_invar shape elt =
  let shape', elt' = cmp_sign_matters shape elt in
  let elt'' =
    match non_signed_variant elt' with
      P8 -> I8
    | x -> x
  in
    shape', elt''

(* Comparison (VTST) where only the element width matters.  *)

let cmp_bits shape elt =
  let vtype = type_for_elt shape elt
  and rtype = type_for_elt shape (unsigned_of_elt elt) 0
  and bits_only = bits_of_elt elt in
  Arity2 (rtype, vtype 1, vtype 2), bits_only

let reg_shift shape elt =
  let vtype = type_for_elt shape elt
  and op2type = type_for_elt shape (signed_of_elt elt) 2 in
  Arity2 (vtype 0, vtype 1, op2type), elt

(* Genericised constant-shift type-generating function.  *)

let const_shift mkimm ?arity ?result shape elt =
  let op2type = (shapemap shape 2) elt in
  let op2width = elt_width op2type in
  let op2 = mkimm op2width
  and op1 = type_for_elt shape elt 1
  and r_elt =
    match result with
      None -> elt
    | Some restriction -> restriction elt in
  let rtype = type_for_elt shape r_elt 0 in
  match arity with
    None -> Arity2 (rtype, op1, op2), elt
  | Some mkarity -> mkarity rtype op1 op2, elt

(* Use for immediate right-shifts.  *)

let shift_right shape elt =
  const_shift (fun imm -> T_immediate (1, imm)) shape elt

let shift_right_acc shape elt =
  const_shift (fun imm -> T_immediate (1, imm))
    ~arity:(fun dst op1 op2 -> Arity3 (dst, dst, op1, op2)) shape elt

(* Use for immediate right-shifts when the operation doesn't care about
   signedness.  *)

let shift_right_sign_invar =
  make_sign_invariant shift_right

(* Immediate right-shift; result is unsigned even when operand is signed.  *)

let shift_right_to_uns shape elt =
  const_shift (fun imm -> T_immediate (1, imm)) ~result:unsigned_of_elt
    shape elt

(* Immediate left-shift.  *)

let shift_left shape elt =
  const_shift (fun imm -> T_immediate (0, imm - 1)) shape elt

(* Immediate left-shift, unsigned result.  *)

let shift_left_to_uns shape elt =
  const_shift (fun imm -> T_immediate (0, imm - 1)) ~result:unsigned_of_elt
    shape elt

(* Immediate left-shift, don't care about signs.  *)

let shift_left_sign_invar =
  make_sign_invariant shift_left

(* Shift left/right and insert: only element size matters.  *)

let shift_insert shape elt =
  let arity, elt =
    const_shift (fun imm -> T_immediate (1, imm))
    ~arity:(fun dst op1 op2 -> Arity3 (dst, dst, op1, op2)) shape elt in
  arity, bits_of_elt elt

(* Get/set lane.  *)

let get_lane shape elt =
  let vtype = type_for_elt shape elt in
  Arity2 (vtype 0, vtype 1, vtype 2),
    (match elt with P8 -> U8 | P16 -> U16 | S32 | U32 | F32 -> B32 | x -> x)

let set_lane shape elt =
  let vtype = type_for_elt shape elt in
  Arity3 (vtype 0, vtype 1, vtype 2, vtype 3), bits_of_elt elt

let set_lane_notype shape elt =
  let vtype = type_for_elt shape elt in
  Arity3 (vtype 0, vtype 1, vtype 2, vtype 3), NoElts

let create_vector shape elt =
  let vtype = type_for_elt shape U64 1
  and rtype = type_for_elt shape elt 0 in
  Arity1 (rtype, vtype), elt

let conv make_arity shape elt =
  let edest, esrc = match elt with
    Conv (edest, esrc) | Cast (edest, esrc) -> edest, esrc
  | _ -> failwith "Non-conversion element in conversion" in
  let vtype = type_for_elt shape esrc
  and rtype = type_for_elt shape edest 0 in
  make_arity rtype vtype, elt

let conv_1 = conv (fun rtype vtype -> Arity1 (rtype, vtype 1))
let conv_2 = conv (fun rtype vtype -> Arity2 (rtype, vtype 1, vtype 2))

(* Operation has an unsigned result even if operands are signed.  *)

let dst_unsign make_arity shape elt =
  let vtype = type_for_elt shape elt
  and rtype = type_for_elt shape (unsigned_of_elt elt) 0 in
  make_arity rtype vtype, elt

let dst_unsign_1 = dst_unsign (fun rtype vtype -> Arity1 (rtype, vtype 1))

let make_bits_only func shape elt =
  let arity, elt' = func shape elt in
  arity, bits_of_elt elt'

(* Extend operation.  *)

let extend shape elt =
  let vtype = type_for_elt shape elt in
  Arity3 (vtype 0, vtype 1, vtype 2, vtype 3), bits_of_elt elt

(* Table look-up operations. Operand 2 is signed/unsigned for signed/unsigned
   integer ops respectively, or unsigned for polynomial ops.  *)

let table mkarity shape elt =
  let vtype = type_for_elt shape elt in
  let op2 = type_for_elt shape (poly_unsigned_variant elt) 2 in
  mkarity vtype op2, bits_of_elt elt

let table_2 = table (fun vtype op2 -> Arity2 (vtype 0, vtype 1, op2))
let table_io = table (fun vtype op2 -> Arity3 (vtype 0, vtype 0, vtype 1, op2))

(* Operations where only bits matter.  *)

let bits_1 = make_bits_only elts_same_1
let bits_2 = make_bits_only elts_same_2
let bits_3 = make_bits_only elts_same_3

(* Store insns.  *)
let store_1 shape elt =
  let vtype = type_for_elt shape elt in
  Arity2 (T_void, vtype 0, vtype 1), bits_of_elt elt

let store_3 shape elt =
  let vtype = type_for_elt shape elt in
  Arity3 (T_void, vtype 0, vtype 1, vtype 2), bits_of_elt elt

let make_notype func shape elt =
  let arity, _ = func shape elt in
  arity, NoElts

let notype_1 = make_notype elts_same_1
let notype_2 = make_notype elts_same_2
let notype_3 = make_notype elts_same_3

(* Bit-select operations (first operand is unsigned int).  *)

let bit_select shape elt =
  let vtype = type_for_elt shape elt
  and itype = type_for_elt shape (unsigned_of_elt elt) in
  Arity3 (vtype 0, itype 1, vtype 2, vtype 3), NoElts

(* Common lists of supported element types.  *)

let s_8_32 = [S8; S16; S32]
let u_8_32 = [U8; U16; U32]
let su_8_32 = [S8; S16; S32; U8; U16; U32]
let su_8_64 = S64 :: U64 :: su_8_32
let su_16_64 = [S16; S32; S64; U16; U32; U64]
let pf_su_8_16 = [P8; P16; S8; S16; U8; U16]
let pf_su_8_32 = P8 :: P16 :: F32 :: su_8_32
let pf_su_8_64 = P8 :: P16 :: F32 :: su_8_64
let suf_32 = [S32; U32; F32]

let ops =
  [
    (* Addition.  *)
    Vadd, [], All (3, Dreg), "vadd", sign_invar_2, F32 :: su_8_32;
    Vadd, [No_op], All (3, Dreg), "vadd", sign_invar_2, [S64; U64];
    Vadd, [], All (3, Qreg), "vaddQ", sign_invar_2, F32 :: su_8_64;
    Vadd, [], Long, "vaddl", elts_same_2, su_8_32;
    Vadd, [], Wide, "vaddw", elts_same_2, su_8_32;
    Vadd, [Halving], All (3, Dreg), "vhadd", elts_same_2, su_8_32;
    Vadd, [Halving], All (3, Qreg), "vhaddQ", elts_same_2, su_8_32;
    Vadd, [Instruction_name ["vrhadd"]; Rounding; Halving],
      All (3, Dreg), "vRhadd", elts_same_2, su_8_32;
    Vadd, [Instruction_name ["vrhadd"]; Rounding; Halving],
      All (3, Qreg), "vRhaddQ", elts_same_2, su_8_32;
    Vadd, [Saturating], All (3, Dreg), "vqadd", elts_same_2, su_8_64;
    Vadd, [Saturating], All (3, Qreg), "vqaddQ", elts_same_2, su_8_64;
    Vadd, [High_half], Narrow, "vaddhn", sign_invar_2, su_16_64;
    Vadd, [Instruction_name ["vraddhn"]; Rounding; High_half],
      Narrow, "vRaddhn", sign_invar_2, su_16_64;

    (* Multiplication.  *)
    Vmul, [], All (3, Dreg), "vmul", sign_invar_2, P8 :: F32 :: su_8_32;
    Vmul, [], All (3, Qreg), "vmulQ", sign_invar_2, P8 :: F32 :: su_8_32;
    Vmul, [Saturating; Doubling; High_half], All (3, Dreg), "vqdmulh",
      elts_same_2, [S16; S32];
    Vmul, [Saturating; Doubling; High_half], All (3, Qreg), "vqdmulhQ",
      elts_same_2, [S16; S32];
    Vmul,
      [Saturating; Rounding; Doubling; High_half;
       Instruction_name ["vqrdmulh"]],
      All (3, Dreg), "vqRdmulh",
      elts_same_2, [S16; S32];
    Vmul,
      [Saturating; Rounding; Doubling; High_half;
       Instruction_name ["vqrdmulh"]],
      All (3, Qreg), "vqRdmulhQ",
      elts_same_2, [S16; S32];
    Vmul, [], Long, "vmull", elts_same_2, P8 :: su_8_32;
    Vmul, [Saturating; Doubling], Long, "vqdmull", elts_same_2, [S16; S32];

    (* Multiply-accumulate. *)
    Vmla, [], All (3, Dreg), "vmla", sign_invar_io, F32 :: su_8_32;
    Vmla, [], All (3, Qreg), "vmlaQ", sign_invar_io, F32 :: su_8_32;
    Vmla, [], Long, "vmlal", elts_same_io, su_8_32;
    Vmla, [Saturating; Doubling], Long, "vqdmlal", elts_same_io, [S16; S32];

    (* Multiply-subtract.  *)
    Vmls, [], All (3, Dreg), "vmls", sign_invar_io, F32 :: su_8_32;
    Vmls, [], All (3, Qreg), "vmlsQ", sign_invar_io, F32 :: su_8_32;
    Vmls, [], Long, "vmlsl", elts_same_io, su_8_32;
    Vmls, [Saturating; Doubling], Long, "vqdmlsl", elts_same_io, [S16; S32];

    (* Fused-multiply-accumulate. *)
    Vfma, [Requires_feature "FMA"], All (3, Dreg), "vfma", elts_same_io, [F32];
    Vfma, [Requires_feature "FMA"], All (3, Qreg), "vfmaQ", elts_same_io, [F32];
    Vfms, [Requires_feature "FMA"], All (3, Dreg), "vfms", elts_same_io, [F32];
    Vfms, [Requires_feature "FMA"], All (3, Qreg), "vfmsQ", elts_same_io, [F32];

    (* Round to integral. *)
    Vrintn, [Builtin_name "vrintn"; Requires_arch 8], Use_operands [| Dreg; Dreg |],
            "vrndn", elts_same_1, [F32];
    Vrintn, [Builtin_name "vrintn"; Requires_arch 8], Use_operands [| Qreg; Qreg |],
            "vrndqn", elts_same_1, [F32];
    Vrinta, [Builtin_name "vrinta"; Requires_arch 8], Use_operands [| Dreg; Dreg |],
            "vrnda", elts_same_1, [F32];
    Vrinta, [Builtin_name "vrinta"; Requires_arch 8], Use_operands [| Qreg; Qreg |],
            "vrndqa", elts_same_1, [F32];
    Vrintp, [Builtin_name "vrintp"; Requires_arch 8], Use_operands [| Dreg; Dreg |],
            "vrndp", elts_same_1, [F32];
    Vrintp, [Builtin_name "vrintp"; Requires_arch 8], Use_operands [| Qreg; Qreg |],
            "vrndqp", elts_same_1, [F32];
    Vrintm, [Builtin_name "vrintm"; Requires_arch 8], Use_operands [| Dreg; Dreg |],
            "vrndm", elts_same_1, [F32];
    Vrintm, [Builtin_name "vrintm"; Requires_arch 8], Use_operands [| Qreg; Qreg |],
            "vrndqm", elts_same_1, [F32];
    Vrintz, [Builtin_name "vrintz"; Requires_arch 8], Use_operands [| Dreg; Dreg |],
            "vrnd", elts_same_1, [F32];
    Vrintz, [Builtin_name "vrintz"; Requires_arch 8], Use_operands [| Qreg; Qreg |],
            "vrndq", elts_same_1, [F32];
    (* Subtraction.  *)
    Vsub, [], All (3, Dreg), "vsub", sign_invar_2, F32 :: su_8_32;
    Vsub, [No_op], All (3, Dreg), "vsub", sign_invar_2,  [S64; U64];
    Vsub, [], All (3, Qreg), "vsubQ", sign_invar_2, F32 :: su_8_64;
    Vsub, [], Long, "vsubl", elts_same_2, su_8_32;
    Vsub, [], Wide, "vsubw", elts_same_2, su_8_32;
    Vsub, [Halving], All (3, Dreg), "vhsub", elts_same_2, su_8_32;
    Vsub, [Halving], All (3, Qreg), "vhsubQ", elts_same_2, su_8_32;
    Vsub, [Saturating], All (3, Dreg), "vqsub", elts_same_2, su_8_64;
    Vsub, [Saturating], All (3, Qreg), "vqsubQ", elts_same_2, su_8_64;
    Vsub, [High_half], Narrow, "vsubhn", sign_invar_2, su_16_64;
    Vsub, [Instruction_name ["vrsubhn"]; Rounding; High_half],
      Narrow, "vRsubhn", sign_invar_2, su_16_64;

    (* Comparison, equal.  *)
    Vceq, [], All (3, Dreg), "vceq", cmp_sign_invar, P8 :: F32 :: su_8_32;
    Vceq, [], All (3, Qreg), "vceqQ", cmp_sign_invar, P8 :: F32 :: su_8_32;

    (* Comparison, greater-than or equal.  *)
    Vcge, [], All (3, Dreg), "vcge", cmp_sign_matters, F32 :: s_8_32;
    Vcge, [Instruction_name ["vcge"]; Builtin_name "vcgeu"],
      All (3, Dreg), "vcge", cmp_sign_matters,
      u_8_32;
    Vcge, [], All (3, Qreg), "vcgeQ", cmp_sign_matters, F32 :: s_8_32;
    Vcge, [Instruction_name ["vcge"]; Builtin_name "vcgeu"],
      All (3, Qreg), "vcgeQ", cmp_sign_matters,
      u_8_32;

    (* Comparison, less-than or equal.  *)
    Vcle, [Flipped "vcge"], All (3, Dreg), "vcle", cmp_sign_matters,
      F32 :: s_8_32;
    Vcle, [Instruction_name ["vcge"]; Flipped "vcgeu"],
      All (3, Dreg), "vcle", cmp_sign_matters,
      u_8_32;
    Vcle, [Instruction_name ["vcge"]; Flipped "vcgeQ"],
      All (3, Qreg), "vcleQ", cmp_sign_matters,
      F32 :: s_8_32;
    Vcle, [Instruction_name ["vcge"]; Flipped "vcgeuQ"],
      All (3, Qreg), "vcleQ", cmp_sign_matters,
      u_8_32;

    (* Comparison, greater-than.  *)
    Vcgt, [], All (3, Dreg), "vcgt", cmp_sign_matters, F32 :: s_8_32;
    Vcgt, [Instruction_name ["vcgt"]; Builtin_name "vcgtu"],
      All (3, Dreg), "vcgt", cmp_sign_matters,
      u_8_32;
    Vcgt, [], All (3, Qreg), "vcgtQ", cmp_sign_matters, F32 :: s_8_32;
    Vcgt, [Instruction_name ["vcgt"]; Builtin_name "vcgtu"],
      All (3, Qreg), "vcgtQ", cmp_sign_matters,
      u_8_32;

    (* Comparison, less-than.  *)
    Vclt, [Flipped "vcgt"], All (3, Dreg), "vclt", cmp_sign_matters,
      F32 :: s_8_32;
    Vclt, [Instruction_name ["vcgt"]; Flipped "vcgtu"],
      All (3, Dreg), "vclt", cmp_sign_matters,
      u_8_32;
    Vclt, [Instruction_name ["vcgt"]; Flipped "vcgtQ"],
      All (3, Qreg), "vcltQ", cmp_sign_matters,
      F32 :: s_8_32;
    Vclt, [Instruction_name ["vcgt"]; Flipped "vcgtuQ"],
      All (3, Qreg), "vcltQ", cmp_sign_matters,
      u_8_32;

    (* Compare absolute greater-than or equal.  *)
    Vcage, [Instruction_name ["vacge"]],
      All (3, Dreg), "vcage", cmp_sign_matters, [F32];
    Vcage, [Instruction_name ["vacge"]],
      All (3, Qreg), "vcageQ", cmp_sign_matters, [F32];

    (* Compare absolute less-than or equal.  *)
    Vcale, [Instruction_name ["vacge"]; Flipped "vcage"],
      All (3, Dreg), "vcale", cmp_sign_matters, [F32];
    Vcale, [Instruction_name ["vacge"]; Flipped "vcageQ"],
      All (3, Qreg), "vcaleQ", cmp_sign_matters, [F32];

    (* Compare absolute greater-than or equal.  *)
    Vcagt, [Instruction_name ["vacgt"]],
      All (3, Dreg), "vcagt", cmp_sign_matters, [F32];
    Vcagt, [Instruction_name ["vacgt"]],
      All (3, Qreg), "vcagtQ", cmp_sign_matters, [F32];

    (* Compare absolute less-than or equal.  *)
    Vcalt, [Instruction_name ["vacgt"]; Flipped "vcagt"],
      All (3, Dreg), "vcalt", cmp_sign_matters, [F32];
    Vcalt, [Instruction_name ["vacgt"]; Flipped "vcagtQ"],
      All (3, Qreg), "vcaltQ", cmp_sign_matters, [F32];

    (* Test bits.  *)
    Vtst, [], All (3, Dreg), "vtst", cmp_bits, P8 :: su_8_32;
    Vtst, [], All (3, Qreg), "vtstQ", cmp_bits, P8 :: su_8_32;

    (* Absolute difference.  *)
    Vabd, [], All (3, Dreg), "vabd", elts_same_2, F32 :: su_8_32;
    Vabd, [], All (3, Qreg), "vabdQ", elts_same_2, F32 :: su_8_32;
    Vabd, [], Long, "vabdl", elts_same_2, su_8_32;

    (* Absolute difference and accumulate.  *)
    Vaba, [], All (3, Dreg), "vaba", elts_same_io, su_8_32;
    Vaba, [], All (3, Qreg), "vabaQ", elts_same_io, su_8_32;
    Vaba, [], Long, "vabal", elts_same_io, su_8_32;

    (* Max.  *)
    Vmax, [], All (3, Dreg), "vmax", elts_same_2, F32 :: su_8_32;
    Vmax, [], All (3, Qreg), "vmaxQ", elts_same_2, F32 :: su_8_32;

    (* Min.  *)
    Vmin, [], All (3, Dreg), "vmin", elts_same_2, F32 :: su_8_32;
    Vmin, [], All (3, Qreg), "vminQ", elts_same_2, F32 :: su_8_32;

    (* Pairwise add.  *)
    Vpadd, [], All (3, Dreg), "vpadd", sign_invar_2, F32 :: su_8_32;
    Vpadd, [], Long_noreg Dreg, "vpaddl", elts_same_1, su_8_32;
    Vpadd, [], Long_noreg Qreg, "vpaddlQ", elts_same_1, su_8_32;

    (* Pairwise add, widen and accumulate.  *)
    Vpada, [], Wide_noreg Dreg, "vpadal", elts_same_2, su_8_32;
    Vpada, [], Wide_noreg Qreg, "vpadalQ", elts_same_2, su_8_32;

    (* Folding maximum, minimum.  *)
    Vpmax, [], All (3, Dreg), "vpmax", elts_same_2, F32 :: su_8_32;
    Vpmin, [], All (3, Dreg), "vpmin", elts_same_2, F32 :: su_8_32;

    (* Reciprocal step.  *)
    Vrecps, [], All (3, Dreg), "vrecps", elts_same_2, [F32];
    Vrecps, [], All (3, Qreg), "vrecpsQ", elts_same_2, [F32];
    Vrsqrts, [], All (3, Dreg), "vrsqrts", elts_same_2, [F32];
    Vrsqrts, [], All (3, Qreg), "vrsqrtsQ", elts_same_2, [F32];

    (* Vector shift left.  *)
    Vshl, [], All (3, Dreg), "vshl", reg_shift, su_8_64;
    Vshl, [], All (3, Qreg), "vshlQ", reg_shift, su_8_64;
    Vshl, [Instruction_name ["vrshl"]; Rounding],
      All (3, Dreg), "vRshl", reg_shift, su_8_64;
    Vshl, [Instruction_name ["vrshl"]; Rounding],
      All (3, Qreg), "vRshlQ", reg_shift, su_8_64;
    Vshl, [Saturating], All (3, Dreg), "vqshl", reg_shift, su_8_64;
    Vshl, [Saturating], All (3, Qreg), "vqshlQ", reg_shift, su_8_64;
    Vshl, [Instruction_name ["vqrshl"]; Saturating; Rounding],
      All (3, Dreg), "vqRshl", reg_shift, su_8_64;
    Vshl, [Instruction_name ["vqrshl"]; Saturating; Rounding],
      All (3, Qreg), "vqRshlQ", reg_shift, su_8_64;

    (* Vector shift right by constant.  *)
    Vshr_n, [], Binary_imm Dreg, "vshr_n", shift_right, su_8_64;
    Vshr_n, [], Binary_imm Qreg, "vshrQ_n", shift_right, su_8_64;
    Vshr_n, [Instruction_name ["vrshr"]; Rounding], Binary_imm Dreg,
      "vRshr_n", shift_right, su_8_64;
    Vshr_n, [Instruction_name ["vrshr"]; Rounding], Binary_imm Qreg,
      "vRshrQ_n", shift_right, su_8_64;
    Vshr_n, [], Narrow_imm, "vshrn_n", shift_right_sign_invar, su_16_64;
    Vshr_n, [Instruction_name ["vrshrn"]; Rounding], Narrow_imm, "vRshrn_n",
      shift_right_sign_invar, su_16_64;
    Vshr_n, [Saturating], Narrow_imm, "vqshrn_n", shift_right, su_16_64;
    Vshr_n, [Instruction_name ["vqrshrn"]; Saturating; Rounding], Narrow_imm,
      "vqRshrn_n", shift_right, su_16_64;
    Vshr_n, [Saturating; Dst_unsign], Narrow_imm, "vqshrun_n",
      shift_right_to_uns, [S16; S32; S64];
    Vshr_n, [Instruction_name ["vqrshrun"]; Saturating; Dst_unsign; Rounding],
      Narrow_imm, "vqRshrun_n", shift_right_to_uns, [S16; S32; S64];

    (* Vector shift left by constant.  *)
    Vshl_n, [], Binary_imm Dreg, "vshl_n", shift_left_sign_invar, su_8_64;
    Vshl_n, [], Binary_imm Qreg, "vshlQ_n", shift_left_sign_invar, su_8_64;
    Vshl_n, [Saturating], Binary_imm Dreg, "vqshl_n", shift_left, su_8_64;
    Vshl_n, [Saturating], Binary_imm Qreg, "vqshlQ_n", shift_left, su_8_64;
    Vshl_n, [Saturating; Dst_unsign], Binary_imm Dreg, "vqshlu_n",
      shift_left_to_uns, [S8; S16; S32; S64];
    Vshl_n, [Saturating; Dst_unsign], Binary_imm Qreg, "vqshluQ_n",
      shift_left_to_uns, [S8; S16; S32; S64];
    Vshl_n, [], Long_imm, "vshll_n", shift_left, su_8_32;

    (* Vector shift right by constant and accumulate.  *)
    Vsra_n, [], Binary_imm Dreg, "vsra_n", shift_right_acc, su_8_64;
    Vsra_n, [], Binary_imm Qreg, "vsraQ_n", shift_right_acc, su_8_64;
    Vsra_n, [Instruction_name ["vrsra"]; Rounding], Binary_imm Dreg,
      "vRsra_n", shift_right_acc, su_8_64;
    Vsra_n, [Instruction_name ["vrsra"]; Rounding], Binary_imm Qreg,
      "vRsraQ_n", shift_right_acc, su_8_64;

    (* Vector shift right and insert.  *)
    Vsri, [Requires_feature "CRYPTO"], Use_operands [| Dreg; Dreg; Immed |], "vsri_n", shift_insert,
      [P64];
    Vsri, [], Use_operands [| Dreg; Dreg; Immed |], "vsri_n", shift_insert,
      P8 :: P16 :: su_8_64;
    Vsri, [Requires_feature "CRYPTO"], Use_operands [| Qreg; Qreg; Immed |], "vsriQ_n", shift_insert,
      [P64];
    Vsri, [], Use_operands [| Qreg; Qreg; Immed |], "vsriQ_n", shift_insert,
      P8 :: P16 :: su_8_64;

    (* Vector shift left and insert.  *)
    Vsli, [Requires_feature "CRYPTO"], Use_operands [| Dreg; Dreg; Immed |], "vsli_n", shift_insert,
      [P64];
    Vsli, [], Use_operands [| Dreg; Dreg; Immed |], "vsli_n", shift_insert,
      P8 :: P16 :: su_8_64;
    Vsli, [Requires_feature "CRYPTO"], Use_operands [| Qreg; Qreg; Immed |], "vsliQ_n", shift_insert,
      [P64];
    Vsli, [], Use_operands [| Qreg; Qreg; Immed |], "vsliQ_n", shift_insert,
      P8 :: P16 :: su_8_64;

    (* Absolute value.  *)
    Vabs, [], All (2, Dreg), "vabs", elts_same_1, [S8; S16; S32; F32];
    Vabs, [], All (2, Qreg), "vabsQ", elts_same_1, [S8; S16; S32; F32];
    Vabs, [Saturating], All (2, Dreg), "vqabs", elts_same_1, [S8; S16; S32];
    Vabs, [Saturating], All (2, Qreg), "vqabsQ", elts_same_1, [S8; S16; S32];

    (* Negate.  *)
    Vneg, [], All (2, Dreg), "vneg", elts_same_1, [S8; S16; S32; F32];
    Vneg, [], All (2, Qreg), "vnegQ", elts_same_1, [S8; S16; S32; F32];
    Vneg, [Saturating], All (2, Dreg), "vqneg", elts_same_1, [S8; S16; S32];
    Vneg, [Saturating], All (2, Qreg), "vqnegQ", elts_same_1, [S8; S16; S32];

    (* Bitwise not.  *)
    Vmvn, [], All (2, Dreg), "vmvn", notype_1, P8 :: su_8_32;
    Vmvn, [], All (2, Qreg), "vmvnQ", notype_1, P8 :: su_8_32;

    (* Count leading sign bits.  *)
    Vcls, [], All (2, Dreg), "vcls", elts_same_1, [S8; S16; S32];
    Vcls, [], All (2, Qreg), "vclsQ", elts_same_1, [S8; S16; S32];

    (* Count leading zeros.  *)
    Vclz, [], All (2, Dreg), "vclz", sign_invar_1, su_8_32;
    Vclz, [], All (2, Qreg), "vclzQ", sign_invar_1, su_8_32;

    (* Count number of set bits.  *)
    Vcnt, [], All (2, Dreg), "vcnt", bits_1, [P8; S8; U8];
    Vcnt, [], All (2, Qreg), "vcntQ", bits_1, [P8; S8; U8];

    (* Reciprocal estimate.  *)
    Vrecpe, [], All (2, Dreg), "vrecpe", elts_same_1, [U32; F32];
    Vrecpe, [], All (2, Qreg), "vrecpeQ", elts_same_1, [U32; F32];

    (* Reciprocal square-root estimate.  *)
    Vrsqrte, [], All (2, Dreg), "vrsqrte", elts_same_1, [U32; F32];
    Vrsqrte, [], All (2, Qreg), "vrsqrteQ", elts_same_1, [U32; F32];

    (* Get lanes from a vector.  *)
    Vget_lane,
      [InfoWord; Disassembles_as [Use_operands [| Corereg; Element_of_dreg |]];
       Instruction_name ["vmov"]],
      Use_operands [| Corereg; Dreg; Immed |],
      "vget_lane", get_lane, pf_su_8_32;
    Vget_lane,
      [No_op;
       InfoWord;
       Disassembles_as [Use_operands [| Corereg; Corereg; Dreg |]];
       Instruction_name ["vmov"]; Const_valuator (fun _ -> 0)],
      Use_operands [| Corereg; Dreg; Immed |],
      "vget_lane", notype_2, [S64; U64];
    Vget_lane,
      [InfoWord; Disassembles_as [Use_operands [| Corereg; Element_of_dreg |]];
       Instruction_name ["vmov"]],
      Use_operands [| Corereg; Qreg; Immed |],
      "vgetQ_lane", get_lane, pf_su_8_32;
    Vget_lane,
      [InfoWord;
       Disassembles_as [Use_operands [| Corereg; Corereg; Dreg |]];
       Instruction_name ["vmov"; "fmrrd"]; Const_valuator (fun _ -> 0);
       Fixed_core_reg],
      Use_operands [| Corereg; Qreg; Immed |],
      "vgetQ_lane", notype_2, [S64; U64];

    (* Set lanes in a vector.  *)
    Vset_lane, [Disassembles_as [Use_operands [| Element_of_dreg; Corereg |]];
                Instruction_name ["vmov"]],
      Use_operands [| Dreg; Corereg; Dreg; Immed |], "vset_lane",
      set_lane, pf_su_8_32;
    Vset_lane, [No_op;
                Disassembles_as [Use_operands [| Dreg; Corereg; Corereg |]];
                Instruction_name ["vmov"]; Const_valuator (fun _ -> 0)],
      Use_operands [| Dreg; Corereg; Dreg; Immed |], "vset_lane",
      set_lane_notype, [S64; U64];
    Vset_lane, [Disassembles_as [Use_operands [| Element_of_dreg; Corereg |]];
                Instruction_name ["vmov"]],
      Use_operands [| Qreg; Corereg; Qreg; Immed |], "vsetQ_lane",
      set_lane, pf_su_8_32;
    Vset_lane, [Disassembles_as [Use_operands [| Dreg; Corereg; Corereg |]];
                Instruction_name ["vmov"]; Const_valuator (fun _ -> 0)],
      Use_operands [| Qreg; Corereg; Qreg; Immed |], "vsetQ_lane",
      set_lane_notype, [S64; U64];

    (* Create vector from literal bit pattern.  *)
    Vcreate,
      [Requires_feature "CRYPTO"; No_op], (* Not really, but it can yield various things that are too
                                   hard for the test generator at this time.  *)
      Use_operands [| Dreg; Corereg |], "vcreate", create_vector,
      [P64];
    Vcreate,
      [No_op], (* Not really, but it can yield various things that are too
                  hard for the test generator at this time.  *)
      Use_operands [| Dreg; Corereg |], "vcreate", create_vector,
      pf_su_8_64;

    (* Set all lanes to the same value.  *)
    Vdup_n,
      [Disassembles_as [Use_operands [| Dreg;
                                        Alternatives [ Corereg;
                                                       Element_of_dreg ] |]]],
      Use_operands [| Dreg; Corereg |], "vdup_n", bits_1,
      pf_su_8_32;
    Vdup_n,
      [No_op; Requires_feature "CRYPTO";
       Instruction_name ["vmov"];
       Disassembles_as [Use_operands [| Dreg; Corereg; Corereg |]]],
      Use_operands [| Dreg; Corereg |], "vdup_n", notype_1,
      [P64];
    Vdup_n,
      [No_op;
       Instruction_name ["vmov"];
       Disassembles_as [Use_operands [| Dreg; Corereg; Corereg |]]],
      Use_operands [| Dreg; Corereg |], "vdup_n", notype_1,
      [S64; U64];
    Vdup_n,
      [No_op; Requires_feature "CRYPTO";
       Disassembles_as [Use_operands [| Qreg;
                                        Alternatives [ Corereg;
                                                       Element_of_dreg ] |]]],
      Use_operands [| Qreg; Corereg |], "vdupQ_n", bits_1,
      [P64];
    Vdup_n,
      [Disassembles_as [Use_operands [| Qreg;
                                        Alternatives [ Corereg;
                                                       Element_of_dreg ] |]]],
      Use_operands [| Qreg; Corereg |], "vdupQ_n", bits_1,
      pf_su_8_32;
    Vdup_n,
      [No_op;
       Instruction_name ["vmov"];
       Disassembles_as [Use_operands [| Dreg; Corereg; Corereg |];
                        Use_operands [| Dreg; Corereg; Corereg |]]],
      Use_operands [| Qreg; Corereg |], "vdupQ_n", notype_1,
      [S64; U64];

    (* These are just aliases for the above.  *)
    Vmov_n,
      [Builtin_name "vdup_n";
       Disassembles_as [Use_operands [| Dreg;
                                        Alternatives [ Corereg;
                                                       Element_of_dreg ] |]]],
      Use_operands [| Dreg; Corereg |],
      "vmov_n", bits_1, pf_su_8_32;
    Vmov_n,
      [No_op;
       Builtin_name "vdup_n";
       Instruction_name ["vmov"];
       Disassembles_as [Use_operands [| Dreg; Corereg; Corereg |]]],
      Use_operands [| Dreg; Corereg |],
      "vmov_n", notype_1, [S64; U64];
    Vmov_n,
      [Builtin_name "vdupQ_n";
       Disassembles_as [Use_operands [| Qreg;
                                        Alternatives [ Corereg;
                                                       Element_of_dreg ] |]]],
      Use_operands [| Qreg; Corereg |],
      "vmovQ_n", bits_1, pf_su_8_32;
    Vmov_n,
      [No_op;
       Builtin_name "vdupQ_n";
       Instruction_name ["vmov"];
       Disassembles_as [Use_operands [| Dreg; Corereg; Corereg |];
                        Use_operands [| Dreg; Corereg; Corereg |]]],
      Use_operands [| Qreg; Corereg |],
      "vmovQ_n", notype_1, [S64; U64];

    (* Duplicate, lane version.  We can't use Use_operands here because the
       rightmost register (always Dreg) would be picked up by find_key_operand,
       when we want the leftmost register to be used in this case (otherwise
       the modes are indistinguishable in neon.md, etc.  *)
    Vdup_lane,
      [Disassembles_as [Use_operands [| Dreg; Element_of_dreg |]]],
      Unary_scalar Dreg, "vdup_lane", bits_2, pf_su_8_32;
    Vdup_lane,
      [No_op; Requires_feature "CRYPTO"; Const_valuator (fun _ -> 0)],
      Unary_scalar Dreg, "vdup_lane", bits_2, [P64];
    Vdup_lane,
      [No_op; Const_valuator (fun _ -> 0)],
      Unary_scalar Dreg, "vdup_lane", bits_2, [S64; U64];
    Vdup_lane,
      [Disassembles_as [Use_operands [| Qreg; Element_of_dreg |]]],
      Unary_scalar Qreg, "vdupQ_lane", bits_2, pf_su_8_32;
    Vdup_lane,
      [No_op; Requires_feature "CRYPTO"; Const_valuator (fun _ -> 0)],
      Unary_scalar Qreg, "vdupQ_lane", bits_2, [P64];
    Vdup_lane,
      [No_op; Const_valuator (fun _ -> 0)],
      Unary_scalar Qreg, "vdupQ_lane", bits_2, [S64; U64];

    (* Combining vectors.  *)
    Vcombine, [Requires_feature "CRYPTO"; No_op],
      Use_operands [| Qreg; Dreg; Dreg |], "vcombine", notype_2,
      [P64];
    Vcombine, [No_op],
      Use_operands [| Qreg; Dreg; Dreg |], "vcombine", notype_2,
      pf_su_8_64;

    (* Splitting vectors.  *)
    Vget_high, [Requires_feature "CRYPTO"; No_op],
      Use_operands [| Dreg; Qreg |], "vget_high",
      notype_1, [P64];
    Vget_high, [No_op],
      Use_operands [| Dreg; Qreg |], "vget_high",
      notype_1, pf_su_8_64;
    Vget_low, [Instruction_name ["vmov"];
               Disassembles_as [Use_operands [| Dreg; Dreg |]];
	       Fixed_vector_reg],
      Use_operands [| Dreg; Qreg |], "vget_low",
      notype_1, pf_su_8_32;
    Vget_low, [Requires_feature "CRYPTO"; No_op],
      Use_operands [| Dreg; Qreg |], "vget_low",
      notype_1, [P64];
    Vget_low, [No_op],
      Use_operands [| Dreg; Qreg |], "vget_low",
      notype_1, [S64; U64];

    (* Conversions.  *)
    Vcvt, [InfoWord], All (2, Dreg), "vcvt", conv_1,
      [Conv (S32, F32); Conv (U32, F32); Conv (F32, S32); Conv (F32, U32)];
    Vcvt, [InfoWord], All (2, Qreg), "vcvtQ", conv_1,
      [Conv (S32, F32); Conv (U32, F32); Conv (F32, S32); Conv (F32, U32)];
    Vcvt, [Builtin_name "vcvt" ; Requires_FP_bit 1],
          Use_operands [| Dreg; Qreg; |], "vcvt", conv_1, [Conv (F16, F32)];
    Vcvt, [Builtin_name "vcvt" ; Requires_FP_bit 1],
          Use_operands [| Qreg; Dreg; |], "vcvt", conv_1, [Conv (F32, F16)];
    Vcvt_n, [InfoWord], Use_operands [| Dreg; Dreg; Immed |], "vcvt_n", conv_2,
      [Conv (S32, F32); Conv (U32, F32); Conv (F32, S32); Conv (F32, U32)];
    Vcvt_n, [InfoWord], Use_operands [| Qreg; Qreg; Immed |], "vcvtQ_n", conv_2,
      [Conv (S32, F32); Conv (U32, F32); Conv (F32, S32); Conv (F32, U32)];

    (* Move, narrowing.  *)
    Vmovn, [Disassembles_as [Use_operands [| Dreg; Qreg |]]],
      Narrow, "vmovn", sign_invar_1, su_16_64;
    Vmovn, [Disassembles_as [Use_operands [| Dreg; Qreg |]]; Saturating],
      Narrow, "vqmovn", elts_same_1, su_16_64;
    Vmovn,
      [Disassembles_as [Use_operands [| Dreg; Qreg |]]; Saturating; Dst_unsign],
      Narrow, "vqmovun", dst_unsign_1,
      [S16; S32; S64];

    (* Move, long.  *)
    Vmovl, [Disassembles_as [Use_operands [| Qreg; Dreg |]]],
      Long, "vmovl", elts_same_1, su_8_32;

    (* Table lookup.  *)
    Vtbl 1,
      [Instruction_name ["vtbl"];
       Disassembles_as [Use_operands [| Dreg; VecArray (1, Dreg); Dreg |]]],
      Use_operands [| Dreg; Dreg; Dreg |], "vtbl1", table_2, [U8; S8; P8];
    Vtbl 2, [Instruction_name ["vtbl"]],
      Use_operands [| Dreg; VecArray (2, Dreg); Dreg |], "vtbl2", table_2,
      [U8; S8; P8];
    Vtbl 3, [Instruction_name ["vtbl"]],
      Use_operands [| Dreg; VecArray (3, Dreg); Dreg |], "vtbl3", table_2,
      [U8; S8; P8];
    Vtbl 4, [Instruction_name ["vtbl"]],
      Use_operands [| Dreg; VecArray (4, Dreg); Dreg |], "vtbl4", table_2,
      [U8; S8; P8];

    (* Extended table lookup.  *)
    Vtbx 1,
      [Instruction_name ["vtbx"];
       Disassembles_as [Use_operands [| Dreg; VecArray (1, Dreg); Dreg |]]],
      Use_operands [| Dreg; Dreg; Dreg |], "vtbx1", table_io, [U8; S8; P8];
    Vtbx 2, [Instruction_name ["vtbx"]],
      Use_operands [| Dreg; VecArray (2, Dreg); Dreg |], "vtbx2", table_io,
      [U8; S8; P8];
    Vtbx 3, [Instruction_name ["vtbx"]],
      Use_operands [| Dreg; VecArray (3, Dreg); Dreg |], "vtbx3", table_io,
      [U8; S8; P8];
    Vtbx 4, [Instruction_name ["vtbx"]],
      Use_operands [| Dreg; VecArray (4, Dreg); Dreg |], "vtbx4", table_io,
      [U8; S8; P8];

    (* Multiply, lane.  (note: these were undocumented at the time of
       writing).  *)
    Vmul_lane, [], By_scalar Dreg, "vmul_lane", sign_invar_2_lane,
      [S16; S32; U16; U32; F32];
    Vmul_lane, [], By_scalar Qreg, "vmulQ_lane", sign_invar_2_lane,
      [S16; S32; U16; U32; F32];

    (* Multiply-accumulate, lane.  *)
    Vmla_lane, [], By_scalar Dreg, "vmla_lane", sign_invar_io_lane,
      [S16; S32; U16; U32; F32];
    Vmla_lane, [], By_scalar Qreg, "vmlaQ_lane", sign_invar_io_lane,
      [S16; S32; U16; U32; F32];
    Vmla_lane, [], Wide_lane, "vmlal_lane", elts_same_io_lane,
      [S16; S32; U16; U32];
    Vmla_lane, [Saturating; Doubling], Wide_lane, "vqdmlal_lane",
      elts_same_io_lane, [S16; S32];

    (* Multiply-subtract, lane.  *)
    Vmls_lane, [], By_scalar Dreg, "vmls_lane", sign_invar_io_lane,
      [S16; S32; U16; U32; F32];
    Vmls_lane, [], By_scalar Qreg, "vmlsQ_lane", sign_invar_io_lane,
      [S16; S32; U16; U32; F32];
    Vmls_lane, [], Wide_lane, "vmlsl_lane", elts_same_io_lane,
      [S16; S32; U16; U32];
    Vmls_lane, [Saturating; Doubling], Wide_lane, "vqdmlsl_lane",
      elts_same_io_lane, [S16; S32];

    (* Long multiply, lane.  *)
    Vmull_lane, [],
      Wide_lane, "vmull_lane", elts_same_2_lane, [S16; S32; U16; U32];

    (* Saturating doubling long multiply, lane.  *)
    Vqdmull_lane, [Saturating; Doubling],
      Wide_lane, "vqdmull_lane", elts_same_2_lane, [S16; S32];

    (* Saturating doubling long multiply high, lane.  *)
    Vqdmulh_lane, [Saturating; Halving],
      By_scalar Qreg, "vqdmulhQ_lane", elts_same_2_lane, [S16; S32];
    Vqdmulh_lane, [Saturating; Halving],
      By_scalar Dreg, "vqdmulh_lane", elts_same_2_lane, [S16; S32];
    Vqdmulh_lane, [Saturating; Halving; Rounding;
		   Instruction_name ["vqrdmulh"]],
      By_scalar Qreg, "vqRdmulhQ_lane", elts_same_2_lane, [S16; S32];
    Vqdmulh_lane, [Saturating; Halving; Rounding;
		   Instruction_name ["vqrdmulh"]],
      By_scalar Dreg, "vqRdmulh_lane", elts_same_2_lane, [S16; S32];

    (* Vector multiply by scalar.  *)
    Vmul_n, [InfoWord;
             Disassembles_as [Use_operands [| Dreg; Dreg; Element_of_dreg |]]],
             Use_operands [| Dreg; Dreg; Corereg |], "vmul_n",
      sign_invar_2, [S16; S32; U16; U32; F32];
    Vmul_n, [InfoWord;
             Disassembles_as [Use_operands [| Qreg; Qreg; Element_of_dreg |]]],
             Use_operands [| Qreg; Qreg; Corereg |], "vmulQ_n",
      sign_invar_2, [S16; S32; U16; U32; F32];

    (* Vector long multiply by scalar.  *)
    Vmull_n, [Instruction_name ["vmull"];
              Disassembles_as [Use_operands [| Qreg; Dreg; Element_of_dreg |]]],
              Wide_scalar, "vmull_n",
      elts_same_2, [S16; S32; U16; U32];

    (* Vector saturating doubling long multiply by scalar.  *)
    Vqdmull_n, [Saturating; Doubling;
	        Disassembles_as [Use_operands [| Qreg; Dreg;
						 Element_of_dreg |]]],
                Wide_scalar, "vqdmull_n",
      elts_same_2, [S16; S32];

    (* Vector saturating doubling long multiply high by scalar.  *)
    Vqdmulh_n,
      [Saturating; Halving; InfoWord;
       Disassembles_as [Use_operands [| Qreg; Qreg; Element_of_dreg |]]],
      Use_operands [| Qreg; Qreg; Corereg |],
      "vqdmulhQ_n", elts_same_2, [S16; S32];
    Vqdmulh_n,
      [Saturating; Halving; InfoWord;
       Disassembles_as [Use_operands [| Dreg; Dreg; Element_of_dreg |]]],
      Use_operands [| Dreg; Dreg; Corereg |],
      "vqdmulh_n", elts_same_2, [S16; S32];
    Vqdmulh_n,
      [Saturating; Halving; Rounding; InfoWord;
       Instruction_name ["vqrdmulh"];
       Disassembles_as [Use_operands [| Qreg; Qreg; Element_of_dreg |]]],
      Use_operands [| Qreg; Qreg; Corereg |],
      "vqRdmulhQ_n", elts_same_2, [S16; S32];
    Vqdmulh_n,
      [Saturating; Halving; Rounding; InfoWord;
       Instruction_name ["vqrdmulh"];
       Disassembles_as [Use_operands [| Dreg; Dreg; Element_of_dreg |]]],
      Use_operands [| Dreg; Dreg; Corereg |],
      "vqRdmulh_n", elts_same_2, [S16; S32];

    (* Vector multiply-accumulate by scalar.  *)
    Vmla_n, [InfoWord;
             Disassembles_as [Use_operands [| Dreg; Dreg; Element_of_dreg |]]],
      Use_operands [| Dreg; Dreg; Corereg |], "vmla_n",
      sign_invar_io, [S16; S32; U16; U32; F32];
    Vmla_n, [InfoWord;
             Disassembles_as [Use_operands [| Qreg; Qreg; Element_of_dreg |]]],
      Use_operands [| Qreg; Qreg; Corereg |], "vmlaQ_n",
      sign_invar_io, [S16; S32; U16; U32; F32];
    Vmla_n, [], Wide_scalar, "vmlal_n", elts_same_io, [S16; S32; U16; U32];
    Vmla_n, [Saturating; Doubling], Wide_scalar, "vqdmlal_n", elts_same_io,
      [S16; S32];

    (* Vector multiply subtract by scalar.  *)
    Vmls_n, [InfoWord;
             Disassembles_as [Use_operands [| Dreg; Dreg; Element_of_dreg |]]],
      Use_operands [| Dreg; Dreg; Corereg |], "vmls_n",
      sign_invar_io, [S16; S32; U16; U32; F32];
    Vmls_n, [InfoWord;
             Disassembles_as [Use_operands [| Qreg; Qreg; Element_of_dreg |]]],
      Use_operands [| Qreg; Qreg; Corereg |], "vmlsQ_n",
      sign_invar_io, [S16; S32; U16; U32; F32];
    Vmls_n, [], Wide_scalar, "vmlsl_n", elts_same_io, [S16; S32; U16; U32];
    Vmls_n, [Saturating; Doubling], Wide_scalar, "vqdmlsl_n", elts_same_io,
      [S16; S32];

    (* Vector extract.  *)
    Vext, [Requires_feature "CRYPTO"; Const_valuator (fun _ -> 0)],
      Use_operands [| Dreg; Dreg; Dreg; Immed |], "vext", extend,
      [P64];
    Vext, [Const_valuator (fun _ -> 0)],
      Use_operands [| Dreg; Dreg; Dreg; Immed |], "vext", extend,
      pf_su_8_64;
    Vext, [Requires_feature "CRYPTO"; Const_valuator (fun _ -> 0)],
      Use_operands [| Qreg; Qreg; Qreg; Immed |], "vextQ", extend,
      [P64];
    Vext, [Const_valuator (fun _ -> 0)],
      Use_operands [| Qreg; Qreg; Qreg; Immed |], "vextQ", extend,
      pf_su_8_64;

    (* Reverse elements.  *)
    Vrev64, [Use_shuffle (rev_elems 64)], All (2, Dreg), "vrev64", bits_1,
      P8 :: P16 :: F32 :: su_8_32;
    Vrev64, [Use_shuffle (rev_elems 64)], All (2, Qreg), "vrev64Q", bits_1,
      P8 :: P16 :: F32 :: su_8_32;
    Vrev32, [Use_shuffle (rev_elems 32)], All (2, Dreg), "vrev32", bits_1,
      [P8; P16; S8; U8; S16; U16];
    Vrev32, [Use_shuffle (rev_elems 32)], All (2, Qreg), "vrev32Q", bits_1,
      [P8; P16; S8; U8; S16; U16];
    Vrev16, [Use_shuffle (rev_elems 16)], All (2, Dreg), "vrev16", bits_1,
      [P8; S8; U8];
    Vrev16, [Use_shuffle (rev_elems 16)], All (2, Qreg), "vrev16Q", bits_1,
      [P8; S8; U8];

    (* Bit selection.  *)
    Vbsl,
      [Requires_feature "CRYPTO"; Instruction_name ["vbsl"; "vbit"; "vbif"];
       Disassembles_as [Use_operands [| Dreg; Dreg; Dreg |]]],
      Use_operands [| Dreg; Dreg; Dreg; Dreg |], "vbsl", bit_select,
      [P64];
    Vbsl,
      [Instruction_name ["vbsl"; "vbit"; "vbif"];
       Disassembles_as [Use_operands [| Dreg; Dreg; Dreg |]]],
      Use_operands [| Dreg; Dreg; Dreg; Dreg |], "vbsl", bit_select,
      pf_su_8_64;
    Vbsl,
      [Requires_feature "CRYPTO"; Instruction_name ["vbsl"; "vbit"; "vbif"];
       Disassembles_as [Use_operands [| Qreg; Qreg; Qreg |]]],
      Use_operands [| Qreg; Qreg; Qreg; Qreg |], "vbslQ", bit_select,
      [P64];
    Vbsl,
      [Instruction_name ["vbsl"; "vbit"; "vbif"];
       Disassembles_as [Use_operands [| Qreg; Qreg; Qreg |]]],
      Use_operands [| Qreg; Qreg; Qreg; Qreg |], "vbslQ", bit_select,
      pf_su_8_64;

    Vtrn, [Use_shuffle trn_elems], Pair_result Dreg, "vtrn", bits_2, pf_su_8_16;
    Vtrn, [Use_shuffle trn_elems; Instruction_name ["vuzp"]], Pair_result Dreg, "vtrn", bits_2, suf_32;
    Vtrn, [Use_shuffle trn_elems], Pair_result Qreg, "vtrnQ", bits_2, pf_su_8_32;
    (* Zip elements.  *)
    Vzip, [Use_shuffle zip_elems], Pair_result Dreg, "vzip", bits_2, pf_su_8_16;
    Vzip, [Use_shuffle zip_elems; Instruction_name ["vuzp"]], Pair_result Dreg, "vzip", bits_2, suf_32;
    Vzip, [Use_shuffle zip_elems], Pair_result Qreg, "vzipQ", bits_2, pf_su_8_32; 

    (* Unzip elements.  *)
    Vuzp, [Use_shuffle uzip_elems], Pair_result Dreg, "vuzp", bits_2,
      pf_su_8_32;
    Vuzp, [Use_shuffle uzip_elems], Pair_result Qreg, "vuzpQ", bits_2,
      pf_su_8_32;

    (* Element/structure loads.  VLD1 variants.  *)
    Vldx 1,
      [Requires_feature "CRYPTO";
       Disassembles_as [Use_operands [| VecArray (1, Dreg);
                                        CstPtrTo Corereg |]]],
      Use_operands [| Dreg; CstPtrTo Corereg |], "vld1", bits_1,
      [P64];
    Vldx 1,
      [Disassembles_as [Use_operands [| VecArray (1, Dreg);
                                        CstPtrTo Corereg |]]],
      Use_operands [| Dreg; CstPtrTo Corereg |], "vld1", bits_1,
      pf_su_8_64;
    Vldx 1, [Requires_feature "CRYPTO";
             Disassembles_as [Use_operands [| VecArray (2, Dreg);
					      CstPtrTo Corereg |]]],
      Use_operands [| Qreg; CstPtrTo Corereg |], "vld1Q", bits_1,
      [P64];
    Vldx 1, [Disassembles_as [Use_operands [| VecArray (2, Dreg);
					      CstPtrTo Corereg |]]],
      Use_operands [| Qreg; CstPtrTo Corereg |], "vld1Q", bits_1,
      pf_su_8_64;

    Vldx_lane 1,
      [Disassembles_as [Use_operands [| VecArray (1, Element_of_dreg);
                                        CstPtrTo Corereg |]]],
      Use_operands [| Dreg; CstPtrTo Corereg; Dreg; Immed |],
      "vld1_lane", bits_3, pf_su_8_32;
    Vldx_lane 1,
      [Requires_feature "CRYPTO";
       Disassembles_as [Use_operands [| VecArray (1, Dreg);
                                        CstPtrTo Corereg |]];
       Const_valuator (fun _ -> 0)],
      Use_operands [| Dreg; CstPtrTo Corereg; Dreg; Immed |],
      "vld1_lane", bits_3, [P64];
    Vldx_lane 1,
      [Disassembles_as [Use_operands [| VecArray (1, Dreg);
                                        CstPtrTo Corereg |]];
       Const_valuator (fun _ -> 0)],
      Use_operands [| Dreg; CstPtrTo Corereg; Dreg; Immed |],
      "vld1_lane", bits_3, [S64; U64];
    Vldx_lane 1,
      [Disassembles_as [Use_operands [| VecArray (1, Element_of_dreg);
                                        CstPtrTo Corereg |]]],
      Use_operands [| Qreg; CstPtrTo Corereg; Qreg; Immed |],
      "vld1Q_lane", bits_3, pf_su_8_32;
    Vldx_lane 1,
      [Requires_feature "CRYPTO";
       Disassembles_as [Use_operands [| VecArray (1, Dreg);
                                        CstPtrTo Corereg |]]],
      Use_operands [| Qreg; CstPtrTo Corereg; Qreg; Immed |],
      "vld1Q_lane", bits_3, [P64];
    Vldx_lane 1,
      [Disassembles_as [Use_operands [| VecArray (1, Dreg);
                                        CstPtrTo Corereg |]]],
      Use_operands [| Qreg; CstPtrTo Corereg; Qreg; Immed |],
      "vld1Q_lane", bits_3, [S64; U64];

    Vldx_dup 1,
      [Disassembles_as [Use_operands [| VecArray (1, All_elements_of_dreg);
                                        CstPtrTo Corereg |]]],
      Use_operands [| Dreg; CstPtrTo Corereg |], "vld1_dup",
      bits_1, pf_su_8_32;
    Vldx_dup 1,
      [Requires_feature "CRYPTO";
       Disassembles_as [Use_operands [| VecArray (1, Dreg);
                                        CstPtrTo Corereg |]]],
      Use_operands [| Dreg; CstPtrTo Corereg |], "vld1_dup",
      bits_1, [P64];
    Vldx_dup 1,
      [Disassembles_as [Use_operands [| VecArray (1, Dreg);
                                        CstPtrTo Corereg |]]],
      Use_operands [| Dreg; CstPtrTo Corereg |], "vld1_dup",
      bits_1, [S64; U64];
    Vldx_dup 1,
      [Disassembles_as [Use_operands [| VecArray (2, All_elements_of_dreg);
                                        CstPtrTo Corereg |]]],
      Use_operands [| Qreg; CstPtrTo Corereg |], "vld1Q_dup",
      bits_1, pf_su_8_32;
    (* Treated identically to vld1_dup above as we now
       do a single load followed by a duplicate.  *)
    Vldx_dup 1,
      [Requires_feature "CRYPTO";
       Disassembles_as [Use_operands [| VecArray (1, Dreg);
                                        CstPtrTo Corereg |]]],
      Use_operands [| Qreg; CstPtrTo Corereg |], "vld1Q_dup",
      bits_1, [P64];
    Vldx_dup 1,
      [Disassembles_as [Use_operands [| VecArray (1, Dreg);
                                        CstPtrTo Corereg |]]],
      Use_operands [| Qreg; CstPtrTo Corereg |], "vld1Q_dup",
      bits_1, [S64; U64];

    (* VST1 variants.  *)
    Vstx 1, [Requires_feature "CRYPTO";
             Disassembles_as [Use_operands [| VecArray (1, Dreg);
                                              PtrTo Corereg |]]],
      Use_operands [| PtrTo Corereg; Dreg |], "vst1",
      store_1, [P64];
    Vstx 1, [Disassembles_as [Use_operands [| VecArray (1, Dreg);
                                              PtrTo Corereg |]]],
      Use_operands [| PtrTo Corereg; Dreg |], "vst1",
      store_1, pf_su_8_64;
    Vstx 1, [Requires_feature "CRYPTO";
             Disassembles_as [Use_operands [| VecArray (2, Dreg);
					      PtrTo Corereg |]]],
      Use_operands [| PtrTo Corereg; Qreg |], "vst1Q",
      store_1, [P64];
    Vstx 1, [Disassembles_as [Use_operands [| VecArray (2, Dreg);
					      PtrTo Corereg |]]],
      Use_operands [| PtrTo Corereg; Qreg |], "vst1Q",
      store_1, pf_su_8_64;

    Vstx_lane 1,
      [Disassembles_as [Use_operands [| VecArray (1, Element_of_dreg);
                                        CstPtrTo Corereg |]]],
      Use_operands [| PtrTo Corereg; Dreg; Immed |],
      "vst1_lane", store_3, pf_su_8_32;
    Vstx_lane 1,
      [Requires_feature "CRYPTO";
       Disassembles_as [Use_operands [| VecArray (1, Dreg);
                                        CstPtrTo Corereg |]];
       Const_valuator (fun _ -> 0)],
      Use_operands [| PtrTo Corereg; Dreg; Immed |],
      "vst1_lane", store_3, [P64];
    Vstx_lane 1,
      [Disassembles_as [Use_operands [| VecArray (1, Dreg);
                                        CstPtrTo Corereg |]];
       Const_valuator (fun _ -> 0)],
      Use_operands [| PtrTo Corereg; Dreg; Immed |],
      "vst1_lane", store_3, [U64; S64];
    Vstx_lane 1,
      [Disassembles_as [Use_operands [| VecArray (1, Element_of_dreg);
                                        CstPtrTo Corereg |]]],
      Use_operands [| PtrTo Corereg; Qreg; Immed |],
      "vst1Q_lane", store_3, pf_su_8_32;
    Vstx_lane 1,
      [Requires_feature "CRYPTO";
       Disassembles_as [Use_operands [| VecArray (1, Dreg);
                                        CstPtrTo Corereg |]]],
      Use_operands [| PtrTo Corereg; Qreg; Immed |],
      "vst1Q_lane", store_3, [P64];
    Vstx_lane 1,
      [Disassembles_as [Use_operands [| VecArray (1, Dreg);
                                        CstPtrTo Corereg |]]],
      Use_operands [| PtrTo Corereg; Qreg; Immed |],
      "vst1Q_lane", store_3, [U64; S64];

    (* VLD2 variants.  *)
    Vldx 2, [], Use_operands [| VecArray (2, Dreg); CstPtrTo Corereg |],
      "vld2", bits_1, pf_su_8_32;
    Vldx 2, [Requires_feature "CRYPTO"; Instruction_name ["vld1"]],
       Use_operands [| VecArray (2, Dreg); CstPtrTo Corereg |],
      "vld2", bits_1, [P64];
    Vldx 2, [Instruction_name ["vld1"]],
       Use_operands [| VecArray (2, Dreg); CstPtrTo Corereg |],
      "vld2", bits_1, [S64; U64];
    Vldx 2, [Disassembles_as [Use_operands [| VecArray (2, Dreg);
                                              CstPtrTo Corereg |];
                              Use_operands [| VecArray (2, Dreg);
					      CstPtrTo Corereg |]]],
      Use_operands [| VecArray (2, Qreg); CstPtrTo Corereg |],
      "vld2Q", bits_1, pf_su_8_32;

    Vldx_lane 2,
      [Disassembles_as [Use_operands
        [| VecArray (2, Element_of_dreg);
           CstPtrTo Corereg |]]],
      Use_operands [| VecArray (2, Dreg); CstPtrTo Corereg;
                      VecArray (2, Dreg); Immed |],
      "vld2_lane", bits_3, P8 :: P16 :: F32 :: su_8_32;
    Vldx_lane 2,
      [Disassembles_as [Use_operands
        [| VecArray (2, Element_of_dreg);
           CstPtrTo Corereg |]]],
      Use_operands [| VecArray (2, Qreg); CstPtrTo Corereg;
 	              VecArray (2, Qreg); Immed |],
      "vld2Q_lane", bits_3, [P16; F32; U16; U32; S16; S32];

    Vldx_dup 2,
      [Disassembles_as [Use_operands
        [| VecArray (2, All_elements_of_dreg); CstPtrTo Corereg |]]],
      Use_operands [| VecArray (2, Dreg); CstPtrTo Corereg |],
      "vld2_dup", bits_1, pf_su_8_32;
    Vldx_dup 2,
      [Requires_feature "CRYPTO";
       Instruction_name ["vld1"]; Disassembles_as [Use_operands
        [| VecArray (2, Dreg); CstPtrTo Corereg |]]],
      Use_operands [| VecArray (2, Dreg); CstPtrTo Corereg |],
      "vld2_dup", bits_1, [P64];
    Vldx_dup 2,
      [Instruction_name ["vld1"]; Disassembles_as [Use_operands
        [| VecArray (2, Dreg); CstPtrTo Corereg |]]],
      Use_operands [| VecArray (2, Dreg); CstPtrTo Corereg |],
      "vld2_dup", bits_1, [S64; U64];

    (* VST2 variants.  *)
    Vstx 2, [Disassembles_as [Use_operands [| VecArray (2, Dreg);
                                              PtrTo Corereg |]]],
      Use_operands [| PtrTo Corereg; VecArray (2, Dreg) |], "vst2",
      store_1, pf_su_8_32;
    Vstx 2, [Requires_feature "CRYPTO";
             Disassembles_as [Use_operands [| VecArray (2, Dreg);
                                              PtrTo Corereg |]];
             Instruction_name ["vst1"]],
      Use_operands [| PtrTo Corereg; VecArray (2, Dreg) |], "vst2",
      store_1, [P64];
    Vstx 2, [Disassembles_as [Use_operands [| VecArray (2, Dreg);
                                              PtrTo Corereg |]];
             Instruction_name ["vst1"]],
      Use_operands [| PtrTo Corereg; VecArray (2, Dreg) |], "vst2",
      store_1, [S64; U64];
    Vstx 2, [Disassembles_as [Use_operands [| VecArray (2, Dreg);
					      PtrTo Corereg |];
                              Use_operands [| VecArray (2, Dreg);
				              PtrTo Corereg |]]],
      Use_operands [| PtrTo Corereg; VecArray (2, Qreg) |], "vst2Q",
      store_1, pf_su_8_32;

    Vstx_lane 2,
      [Disassembles_as [Use_operands
        [| VecArray (2, Element_of_dreg);
           CstPtrTo Corereg |]]],
      Use_operands [| PtrTo Corereg; VecArray (2, Dreg); Immed |], "vst2_lane",
      store_3, P8 :: P16 :: F32 :: su_8_32;
    Vstx_lane 2,
      [Disassembles_as [Use_operands
        [| VecArray (2, Element_of_dreg);
           CstPtrTo Corereg |]]],
      Use_operands [| PtrTo Corereg; VecArray (2, Qreg); Immed |], "vst2Q_lane",
      store_3, [P16; F32; U16; U32; S16; S32];

    (* VLD3 variants.  *)
    Vldx 3, [], Use_operands [| VecArray (3, Dreg); CstPtrTo Corereg |],
      "vld3", bits_1, pf_su_8_32;
    Vldx 3, [Requires_feature "CRYPTO"; Instruction_name ["vld1"]],
      Use_operands [| VecArray (3, Dreg); CstPtrTo Corereg |],
      "vld3", bits_1, [P64];
    Vldx 3, [Instruction_name ["vld1"]],
      Use_operands [| VecArray (3, Dreg); CstPtrTo Corereg |],
      "vld3", bits_1, [S64; U64];
    Vldx 3, [Disassembles_as [Use_operands [| VecArray (3, Dreg);
					      CstPtrTo Corereg |];
                              Use_operands [| VecArray (3, Dreg);
					      CstPtrTo Corereg |]]],
      Use_operands [| VecArray (3, Qreg); CstPtrTo Corereg |],
      "vld3Q", bits_1, P8 :: P16 :: F32 :: su_8_32;

    Vldx_lane 3,
      [Disassembles_as [Use_operands
        [| VecArray (3, Element_of_dreg);
           CstPtrTo Corereg |]]],
      Use_operands [| VecArray (3, Dreg); CstPtrTo Corereg;
                                     VecArray (3, Dreg); Immed |],
      "vld3_lane", bits_3, P8 :: P16 :: F32 :: su_8_32;
    Vldx_lane 3,
      [Disassembles_as [Use_operands
        [| VecArray (3, Element_of_dreg);
           CstPtrTo Corereg |]]],
      Use_operands [| VecArray (3, Qreg); CstPtrTo Corereg;
				     VecArray (3, Qreg); Immed |],
      "vld3Q_lane", bits_3, [P16; F32; U16; U32; S16; S32];

    Vldx_dup 3,
      [Disassembles_as [Use_operands
        [| VecArray (3, All_elements_of_dreg); CstPtrTo Corereg |]]],
      Use_operands [| VecArray (3, Dreg); CstPtrTo Corereg |],
      "vld3_dup", bits_1, pf_su_8_32;
    Vldx_dup 3,
      [Requires_feature "CRYPTO";
       Instruction_name ["vld1"]; Disassembles_as [Use_operands
        [| VecArray (3, Dreg); CstPtrTo Corereg |]]],
      Use_operands [| VecArray (3, Dreg); CstPtrTo Corereg |],
      "vld3_dup", bits_1, [P64];
    Vldx_dup 3,
      [Instruction_name ["vld1"]; Disassembles_as [Use_operands
        [| VecArray (3, Dreg); CstPtrTo Corereg |]]],
      Use_operands [| VecArray (3, Dreg); CstPtrTo Corereg |],
      "vld3_dup", bits_1, [S64; U64];

    (* VST3 variants.  *)
    Vstx 3, [Disassembles_as [Use_operands [| VecArray (4, Dreg);
                                              PtrTo Corereg |]]],
      Use_operands [| PtrTo Corereg; VecArray (3, Dreg) |], "vst3",
      store_1, pf_su_8_32;
    Vstx 3, [Requires_feature "CRYPTO";
             Disassembles_as [Use_operands [| VecArray (4, Dreg);
                                              PtrTo Corereg |]];
             Instruction_name ["vst1"]],
      Use_operands [| PtrTo Corereg; VecArray (3, Dreg) |], "vst3",
      store_1, [P64];
    Vstx 3, [Disassembles_as [Use_operands [| VecArray (4, Dreg);
                                              PtrTo Corereg |]];
             Instruction_name ["vst1"]],
      Use_operands [| PtrTo Corereg; VecArray (3, Dreg) |], "vst3",
      store_1, [S64; U64];
    Vstx 3, [Disassembles_as [Use_operands [| VecArray (3, Dreg);
					      PtrTo Corereg |];
                              Use_operands [| VecArray (3, Dreg);
					      PtrTo Corereg |]]],
      Use_operands [| PtrTo Corereg; VecArray (3, Qreg) |], "vst3Q",
      store_1, pf_su_8_32;

    Vstx_lane 3,
      [Disassembles_as [Use_operands
        [| VecArray (3, Element_of_dreg);
           CstPtrTo Corereg |]]],
      Use_operands [| PtrTo Corereg; VecArray (3, Dreg); Immed |], "vst3_lane",
      store_3, P8 :: P16 :: F32 :: su_8_32;
    Vstx_lane 3,
      [Disassembles_as [Use_operands
        [| VecArray (3, Element_of_dreg);
           CstPtrTo Corereg |]]],
      Use_operands [| PtrTo Corereg; VecArray (3, Qreg); Immed |], "vst3Q_lane",
      store_3, [P16; F32; U16; U32; S16; S32];

    (* VLD4/VST4 variants.  *)
    Vldx 4, [], Use_operands [| VecArray (4, Dreg); CstPtrTo Corereg |],
      "vld4", bits_1, pf_su_8_32;
    Vldx 4, [Requires_feature "CRYPTO"; Instruction_name ["vld1"]],
      Use_operands [| VecArray (4, Dreg); CstPtrTo Corereg |],
      "vld4", bits_1, [P64];
    Vldx 4, [Instruction_name ["vld1"]],
      Use_operands [| VecArray (4, Dreg); CstPtrTo Corereg |],
      "vld4", bits_1, [S64; U64];
    Vldx 4, [Disassembles_as [Use_operands [| VecArray (4, Dreg);
					      CstPtrTo Corereg |];
                              Use_operands [| VecArray (4, Dreg);
					      CstPtrTo Corereg |]]],
      Use_operands [| VecArray (4, Qreg); CstPtrTo Corereg |],
      "vld4Q", bits_1, P8 :: P16 :: F32 :: su_8_32;

    Vldx_lane 4,
      [Disassembles_as [Use_operands
        [| VecArray (4, Element_of_dreg);
           CstPtrTo Corereg |]]],
      Use_operands [| VecArray (4, Dreg); CstPtrTo Corereg;
                                     VecArray (4, Dreg); Immed |],
      "vld4_lane", bits_3, P8 :: P16 :: F32 :: su_8_32;
    Vldx_lane 4,
      [Disassembles_as [Use_operands
        [| VecArray (4, Element_of_dreg);
           CstPtrTo Corereg |]]],
      Use_operands [| VecArray (4, Qreg); CstPtrTo Corereg;
   	              VecArray (4, Qreg); Immed |],
      "vld4Q_lane", bits_3, [P16; F32; U16; U32; S16; S32];

    Vldx_dup 4,
      [Disassembles_as [Use_operands
        [| VecArray (4, All_elements_of_dreg); CstPtrTo Corereg |]]],
      Use_operands [| VecArray (4, Dreg); CstPtrTo Corereg |],
      "vld4_dup", bits_1, pf_su_8_32;
    Vldx_dup 4,
      [Requires_feature "CRYPTO";
       Instruction_name ["vld1"]; Disassembles_as [Use_operands
        [| VecArray (4, Dreg); CstPtrTo Corereg |]]],
      Use_operands [| VecArray (4, Dreg); CstPtrTo Corereg |],
      "vld4_dup", bits_1, [P64];
    Vldx_dup 4,
      [Instruction_name ["vld1"]; Disassembles_as [Use_operands
        [| VecArray (4, Dreg); CstPtrTo Corereg |]]],
      Use_operands [| VecArray (4, Dreg); CstPtrTo Corereg |],
      "vld4_dup", bits_1, [S64; U64];

    Vstx 4, [Disassembles_as [Use_operands [| VecArray (4, Dreg);
                                              PtrTo Corereg |]]],
      Use_operands [| PtrTo Corereg; VecArray (4, Dreg) |], "vst4",
      store_1, pf_su_8_32;
    Vstx 4, [Requires_feature "CRYPTO";
             Disassembles_as [Use_operands [| VecArray (4, Dreg);
                                              PtrTo Corereg |]];
             Instruction_name ["vst1"]],
      Use_operands [| PtrTo Corereg; VecArray (4, Dreg) |], "vst4",
      store_1, [P64];
    Vstx 4, [Disassembles_as [Use_operands [| VecArray (4, Dreg);
                                              PtrTo Corereg |]];
             Instruction_name ["vst1"]],
      Use_operands [| PtrTo Corereg; VecArray (4, Dreg) |], "vst4",
      store_1, [S64; U64];
    Vstx 4, [Disassembles_as [Use_operands [| VecArray (4, Dreg);
					      PtrTo Corereg |];
                              Use_operands [| VecArray (4, Dreg);
					      PtrTo Corereg |]]],
     Use_operands [| PtrTo Corereg; VecArray (4, Qreg) |], "vst4Q",
      store_1, pf_su_8_32;

    Vstx_lane 4,
      [Disassembles_as [Use_operands
        [| VecArray (4, Element_of_dreg);
           CstPtrTo Corereg |]]],
      Use_operands [| PtrTo Corereg; VecArray (4, Dreg); Immed |], "vst4_lane",
      store_3, P8 :: P16 :: F32 :: su_8_32;
    Vstx_lane 4,
      [Disassembles_as [Use_operands
        [| VecArray (4, Element_of_dreg);
           CstPtrTo Corereg |]]],
      Use_operands [| PtrTo Corereg; VecArray (4, Qreg); Immed |], "vst4Q_lane",
      store_3, [P16; F32; U16; U32; S16; S32];

    (* Logical operations. And.  *)
    Vand, [], All (3, Dreg), "vand", notype_2, su_8_32;
    Vand, [No_op], All (3, Dreg), "vand", notype_2, [S64; U64];
    Vand, [], All (3, Qreg), "vandQ", notype_2, su_8_64;

    (* Or.  *)
    Vorr, [], All (3, Dreg), "vorr", notype_2, su_8_32;
    Vorr, [No_op], All (3, Dreg), "vorr", notype_2, [S64; U64];
    Vorr, [], All (3, Qreg), "vorrQ", notype_2, su_8_64;

    (* Eor.  *)
    Veor, [], All (3, Dreg), "veor", notype_2, su_8_32;
    Veor, [No_op], All (3, Dreg), "veor", notype_2, [S64; U64];
    Veor, [], All (3, Qreg), "veorQ", notype_2, su_8_64;

    (* Bic (And-not).  *)
    Vbic, [Compiler_optim "-O2"], All (3, Dreg), "vbic", notype_2, su_8_32;
    Vbic, [No_op; Compiler_optim "-O2"], All (3, Dreg), "vbic", notype_2, [S64; U64];
    Vbic, [Compiler_optim "-O2"], All (3, Qreg), "vbicQ", notype_2, su_8_64;

    (* Or-not.  *)
    Vorn, [Compiler_optim "-O2"], All (3, Dreg), "vorn", notype_2, su_8_32;
    Vorn, [No_op; Compiler_optim "-O2"], All (3, Dreg), "vorn", notype_2, [S64; U64];
    Vorn, [Compiler_optim "-O2"], All (3, Qreg), "vornQ", notype_2, su_8_64;
  ]

let type_in_crypto_only t
  = (t == P64) || (t == P128)

let cross_product s1 s2
  = List.filter (fun (e, e') -> e <> e')
                (List.concat (List.map (fun e1 -> List.map (fun e2 -> (e1,e2)) s1) s2))

let reinterp =
  let elems = P8 :: P16 :: F32 :: P64 :: su_8_64 in
  let casts = cross_product elems elems in
  List.map
    (fun (convto, convfrom) ->
       Vreinterp, (if (type_in_crypto_only convto) || (type_in_crypto_only convfrom)
                   then [Requires_feature "CRYPTO"] else []) @ [No_op], Use_operands [| Dreg; Dreg |],
                   "vreinterpret", conv_1, [Cast (convto, convfrom)])
    casts

let reinterpq =
  let elems = P8 :: P16 :: F32 :: P64 :: P128 :: su_8_64 in
  let casts = cross_product elems elems in
  List.map
    (fun (convto, convfrom) ->
       Vreinterp, (if (type_in_crypto_only convto) || (type_in_crypto_only convfrom)
                   then [Requires_feature "CRYPTO"] else []) @ [No_op], Use_operands [| Qreg; Qreg |],
                   "vreinterpretQ", conv_1, [Cast (convto, convfrom)])
    casts

(* Output routines.  *)

let rec string_of_elt = function
    S8 -> "s8" | S16 -> "s16" | S32 -> "s32" | S64 -> "s64"
  | U8 -> "u8" | U16 -> "u16" | U32 -> "u32" | U64 -> "u64"
  | I8 -> "i8" | I16 -> "i16" | I32 -> "i32" | I64 -> "i64"
  | B8 -> "8" | B16 -> "16" | B32 -> "32" | B64 -> "64"
  | F16 -> "f16" | F32 -> "f32" | P8 -> "p8" | P16 -> "p16"
  | P64 -> "p64" | P128 -> "p128"
  | Conv (a, b) | Cast (a, b) -> string_of_elt a ^ "_" ^ string_of_elt b
  | NoElts -> failwith "No elts"

let string_of_elt_dots elt =
  match elt with
    Conv (a, b) | Cast (a, b) -> string_of_elt a ^ "." ^ string_of_elt b
  | _ -> string_of_elt elt

let string_of_vectype vt =
  let rec name affix = function
    T_int8x8 -> affix "int8x8"
  | T_int8x16 -> affix "int8x16"
  | T_int16x4 -> affix "int16x4"
  | T_int16x8 -> affix "int16x8"
  | T_int32x2 -> affix "int32x2"
  | T_int32x4 -> affix "int32x4"
  | T_int64x1 -> affix "int64x1"
  | T_int64x2 -> affix "int64x2"
  | T_uint8x8 -> affix "uint8x8"
  | T_uint8x16 -> affix "uint8x16"
  | T_uint16x4 -> affix "uint16x4"
  | T_uint16x8 -> affix "uint16x8"
  | T_uint32x2 -> affix "uint32x2"
  | T_uint32x4 -> affix "uint32x4"
  | T_uint64x1 -> affix "uint64x1"
  | T_uint64x2 -> affix "uint64x2"
  | T_float16x4 -> affix "float16x4"
  | T_float32x2 -> affix "float32x2"
  | T_float32x4 -> affix "float32x4"
  | T_poly8x8 -> affix "poly8x8"
  | T_poly8x16 -> affix "poly8x16"
  | T_poly16x4 -> affix "poly16x4"
  | T_poly16x8 -> affix "poly16x8"
  | T_int8 -> affix "int8"
  | T_int16 -> affix "int16"
  | T_int32 -> affix "int32"
  | T_int64 -> affix "int64"
  | T_uint8 -> affix "uint8"
  | T_uint16 -> affix "uint16"
  | T_uint32 -> affix "uint32"
  | T_uint64 -> affix "uint64"
  | T_poly8 -> affix "poly8"
  | T_poly16 -> affix "poly16"
  | T_poly64 -> affix "poly64"
  | T_poly64x1 -> affix "poly64x1"
  | T_poly64x2 -> affix "poly64x2"
  | T_poly128 -> affix "poly128"
  | T_float16 -> affix "float16"
  | T_float32 -> affix "float32"
  | T_immediate _ -> "const int"
  | T_void -> "void"
  | T_intQI -> "__builtin_neon_qi"
  | T_intHI -> "__builtin_neon_hi"
  | T_intSI -> "__builtin_neon_si"
  | T_intDI -> "__builtin_neon_di"
  | T_intTI -> "__builtin_neon_ti"
  | T_floatHF -> "__builtin_neon_hf"
  | T_floatSF -> "__builtin_neon_sf"
  | T_arrayof (num, base) ->
      let basename = name (fun x -> x) base in
      affix (Printf.sprintf "%sx%d" basename num)
  | T_ptrto x ->
      let basename = name affix x in
      Printf.sprintf "%s *" basename
  | T_const x ->
      let basename = name affix x in
      Printf.sprintf "const %s" basename
  in
    name (fun x -> x ^ "_t") vt

let string_of_inttype = function
    B_TImode -> "__builtin_neon_ti"
  | B_EImode -> "__builtin_neon_ei"
  | B_OImode -> "__builtin_neon_oi"
  | B_CImode -> "__builtin_neon_ci"
  | B_XImode -> "__builtin_neon_xi"

let string_of_mode = function
    V8QI -> "v8qi" | V4HI -> "v4hi" | V4HF  -> "v4hf"  | V2SI -> "v2si"
  | V2SF -> "v2sf" | DI   -> "di"   | V16QI -> "v16qi" | V8HI -> "v8hi"
  | V4SI -> "v4si" | V4SF -> "v4sf" | V2DI  -> "v2di"  | QI   -> "qi"
  | HI -> "hi" | SI -> "si" | SF -> "sf" | TI -> "ti"

(* Use uppercase chars for letters which form part of the intrinsic name, but
   should be omitted from the builtin name (the info is passed in an extra
   argument, instead).  *)
let intrinsic_name name = String.lowercase name

(* Allow the name of the builtin to be overridden by things (e.g. Flipped)
   found in the features list.  *)
let builtin_name features name =
  let name = List.fold_right
               (fun el name ->
                 match el with
                   Flipped x | Builtin_name x -> x
                 | _ -> name)
               features name in
  let islower x = let str = String.make 1 x in (String.lowercase str) = str
  and buf = Buffer.create (String.length name) in
  String.iter (fun c -> if islower c then Buffer.add_char buf c) name;
  Buffer.contents buf

(* Transform an arity into a list of strings.  *)
let strings_of_arity a =
  match a with
  | Arity0 vt -> [string_of_vectype vt]
  | Arity1 (vt1, vt2) -> [string_of_vectype vt1; string_of_vectype vt2]
  | Arity2 (vt1, vt2, vt3) -> [string_of_vectype vt1;
			       string_of_vectype vt2;
                               string_of_vectype vt3]
  | Arity3 (vt1, vt2, vt3, vt4) -> [string_of_vectype vt1;
                                    string_of_vectype vt2;
                                    string_of_vectype vt3;
                                    string_of_vectype vt4]
  | Arity4 (vt1, vt2, vt3, vt4, vt5) -> [string_of_vectype vt1;
                                         string_of_vectype vt2;
                                         string_of_vectype vt3;
                                         string_of_vectype vt4;
                                         string_of_vectype vt5]

(* Suffixes on the end of builtin names that are to be stripped in order
   to obtain the name used as an instruction.  They are only stripped if
   preceded immediately by an underscore.  *)
let suffixes_to_strip = [ "n"; "lane"; "dup" ]

(* Get the possible names of an instruction corresponding to a "name" from the
   ops table.  This is done by getting the equivalent builtin name and
   stripping any suffixes from the list at the top of this file, unless
   the features list presents with an Instruction_name entry, in which
   case that is used; or unless the features list presents with a Flipped
   entry, in which case that is used.  If both such entries are present,
   the first in the list will be chosen.  *)
let get_insn_names features name =
  let names = try
  begin
    match List.find (fun feature -> match feature with
                                      Instruction_name _ -> true
				    | Flipped _ -> true
				    | _ -> false) features
    with
      Instruction_name names -> names
    | Flipped name -> [name]
    | _ -> assert false
  end
  with Not_found -> [builtin_name features name]
  in
  begin
    List.map (fun name' ->
      try
        let underscore = String.rindex name' '_' in
        let our_suffix = String.sub name' (underscore + 1)
                                    ((String.length name') - underscore - 1)
        in
          let rec strip remaining_suffixes =
            match remaining_suffixes with
              [] -> name'
            | s::ss when our_suffix = s -> String.sub name' 0 underscore
            | _::ss -> strip ss
          in
            strip suffixes_to_strip
      with (Not_found | Invalid_argument _) -> name') names
  end

(* Apply a function to each element of a list and then comma-separate
   the resulting strings.  *)
let rec commas f elts acc =
  match elts with
    [] -> acc
  | [elt] -> acc ^ (f elt)
  | elt::elts ->
    commas f elts (acc ^ (f elt) ^ ", ")

(* Given a list of features and the shape specified in the "ops" table, apply
   a function to each possible shape that the instruction may have.
   By default, this is the "shape" entry in "ops".  If the features list
   contains a Disassembles_as entry, the shapes contained in that entry are
   mapped to corresponding outputs and returned in a list.  If there is more
   than one Disassembles_as entry, only the first is used.  *)
let analyze_all_shapes features shape f =
  try
    match List.find (fun feature ->
                       match feature with Disassembles_as _ -> true
                                        | _ -> false)
                    features with
      Disassembles_as shapes -> List.map f shapes
    | _ -> assert false
  with Not_found -> [f shape]

(* The crypto intrinsics have unconventional shapes and are not that
   numerous to be worth the trouble of encoding here.  We implement them
   explicitly here.  *)
let crypto_intrinsics =
"
#ifdef __ARM_FEATURE_CRYPTO

__extension__ static __inline poly128_t __attribute__ ((__always_inline__))
vldrq_p128 (poly128_t const * __ptr)
{
#ifdef __ARM_BIG_ENDIAN
  poly64_t* __ptmp = (poly64_t*) __ptr;
  poly64_t __d0 = vld1_p64 (__ptmp);
  poly64_t __d1 = vld1_p64 (__ptmp + 1);
  return vreinterpretq_p128_p64 (vcombine_p64 (__d1, __d0));
#else
  return vreinterpretq_p128_p64 (vld1q_p64 ((poly64_t*) __ptr));
#endif
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vstrq_p128 (poly128_t * __ptr, poly128_t __val)
{
#ifdef __ARM_BIG_ENDIAN
  poly64x2_t __tmp = vreinterpretq_p64_p128 (__val);
  poly64_t __d0 = vget_high_p64 (__tmp);
  poly64_t __d1 = vget_low_p64 (__tmp);
  vst1q_p64 ((poly64_t*) __ptr, vcombine_p64 (__d0, __d1));
#else
  vst1q_p64 ((poly64_t*) __ptr, vreinterpretq_p64_p128 (__val));
#endif
}

/* The vceq_p64 intrinsic does not map to a single instruction.
   Instead we emulate it by performing a 32-bit variant of the vceq
   and applying a pairwise min reduction to the result.
   vceq_u32 will produce two 32-bit halves, each of which will contain either
   all ones or all zeros depending on whether the corresponding 32-bit
   halves of the poly64_t were equal.  The whole poly64_t values are equal
   if and only if both halves are equal, i.e. vceq_u32 returns all ones.
   If the result is all zeroes for any half then the whole result is zeroes.
   This is what the pairwise min reduction achieves.  */

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vceq_p64 (poly64x1_t __a, poly64x1_t __b)
{
  uint32x2_t __t_a = vreinterpret_u32_p64 (__a);
  uint32x2_t __t_b = vreinterpret_u32_p64 (__b);
  uint32x2_t __c = vceq_u32 (__t_a, __t_b);
  uint32x2_t __m = vpmin_u32 (__c, __c);
  return vreinterpret_u64_u32 (__m);
}

/* The vtst_p64 intrinsic does not map to a single instruction.
   We emulate it in way similar to vceq_p64 above but here we do
   a reduction with max since if any two corresponding bits
   in the two poly64_t's match, then the whole result must be all ones.  */

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vtst_p64 (poly64x1_t __a, poly64x1_t __b)
{
  uint32x2_t __t_a = vreinterpret_u32_p64 (__a);
  uint32x2_t __t_b = vreinterpret_u32_p64 (__b);
  uint32x2_t __c = vtst_u32 (__t_a, __t_b);
  uint32x2_t __m = vpmax_u32 (__c, __c);
  return vreinterpret_u64_u32 (__m);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vaeseq_u8 (uint8x16_t __data, uint8x16_t __key)
{
  return __builtin_arm_crypto_aese (__data, __key);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vaesdq_u8 (uint8x16_t __data, uint8x16_t __key)
{
  return __builtin_arm_crypto_aesd (__data, __key);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vaesmcq_u8 (uint8x16_t __data)
{
  return __builtin_arm_crypto_aesmc (__data);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vaesimcq_u8 (uint8x16_t __data)
{
  return __builtin_arm_crypto_aesimc (__data);
}

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
vsha1h_u32 (uint32_t __hash_e)
{
  uint32x4_t __t = vdupq_n_u32 (0);
  __t = vsetq_lane_u32 (__hash_e, __t, 0);
  __t = __builtin_arm_crypto_sha1h (__t);
  return vgetq_lane_u32 (__t, 0);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vsha1cq_u32 (uint32x4_t __hash_abcd, uint32_t __hash_e, uint32x4_t __wk)
{
  uint32x4_t __t = vdupq_n_u32 (0);
  __t = vsetq_lane_u32 (__hash_e, __t, 0);
  return __builtin_arm_crypto_sha1c (__hash_abcd, __t, __wk);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vsha1pq_u32 (uint32x4_t __hash_abcd, uint32_t __hash_e, uint32x4_t __wk)
{
  uint32x4_t __t = vdupq_n_u32 (0);
  __t = vsetq_lane_u32 (__hash_e, __t, 0);
  return __builtin_arm_crypto_sha1p (__hash_abcd, __t, __wk);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vsha1mq_u32 (uint32x4_t __hash_abcd, uint32_t __hash_e, uint32x4_t __wk)
{
  uint32x4_t __t = vdupq_n_u32 (0);
  __t = vsetq_lane_u32 (__hash_e, __t, 0);
  return __builtin_arm_crypto_sha1m (__hash_abcd, __t, __wk);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vsha1su0q_u32 (uint32x4_t __w0_3, uint32x4_t __w4_7, uint32x4_t __w8_11)
{
  return __builtin_arm_crypto_sha1su0 (__w0_3, __w4_7, __w8_11);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vsha1su1q_u32 (uint32x4_t __tw0_3, uint32x4_t __w12_15)
{
  return __builtin_arm_crypto_sha1su1 (__tw0_3, __w12_15);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vsha256hq_u32 (uint32x4_t __hash_abcd, uint32x4_t __hash_efgh, uint32x4_t __wk)
{
  return __builtin_arm_crypto_sha256h (__hash_abcd, __hash_efgh, __wk);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vsha256h2q_u32 (uint32x4_t __hash_abcd, uint32x4_t __hash_efgh, uint32x4_t __wk)
{
  return __builtin_arm_crypto_sha256h2 (__hash_abcd, __hash_efgh, __wk);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vsha256su0q_u32 (uint32x4_t __w0_3, uint32x4_t __w4_7)
{
  return __builtin_arm_crypto_sha256su0 (__w0_3, __w4_7);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vsha256su1q_u32 (uint32x4_t __tw0_3, uint32x4_t __w8_11, uint32x4_t __w12_15)
{
  return __builtin_arm_crypto_sha256su1 (__tw0_3, __w8_11, __w12_15);
}

__extension__ static __inline poly128_t __attribute__ ((__always_inline__))
vmull_p64 (poly64_t __a, poly64_t __b)
{
  return (poly128_t) __builtin_arm_crypto_vmullp64 ((uint64_t) __a, (uint64_t) __b);
}

__extension__ static __inline poly128_t __attribute__ ((__always_inline__))
vmull_high_p64 (poly64x2_t __a, poly64x2_t __b)
{
  poly64_t __t1 = vget_high_p64 (__a);
  poly64_t __t2 = vget_high_p64 (__b);

  return (poly128_t) __builtin_arm_crypto_vmullp64 ((uint64_t) __t1, (uint64_t) __t2);
}

#endif
"
