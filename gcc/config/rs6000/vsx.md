;; VSX patterns.
;; Copyright (C) 2009-2016 Free Software Foundation, Inc.
;; Contributed by Michael Meissner <meissner@linux.vnet.ibm.com>

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; Iterator for comparison types
(define_code_iterator CMP_TEST [eq lt gt unordered])

;; Iterator for both scalar and vector floating point types supported by VSX
(define_mode_iterator VSX_B [DF V4SF V2DF])

;; Iterator for the 2 64-bit vector types
(define_mode_iterator VSX_D [V2DF V2DI])

;; Iterator for the 2 64-bit vector types + 128-bit types that are loaded with
;; lxvd2x to properly handle swapping words on little endian
(define_mode_iterator VSX_LE [V2DF V2DI V1TI])

;; Mode iterator to handle swapping words on little endian for the 128-bit
;; types that goes in a single vector register.
(define_mode_iterator VSX_LE_128 [(KF   "FLOAT128_VECTOR_P (KFmode)")
				  (TF   "FLOAT128_VECTOR_P (TFmode)")
				  (TI	"TARGET_VSX_TIMODE")])

;; Iterator for the 2 32-bit vector types
(define_mode_iterator VSX_W [V4SF V4SI])

;; Iterator for the DF types
(define_mode_iterator VSX_DF [V2DF DF])

;; Iterator for vector floating point types supported by VSX
(define_mode_iterator VSX_F [V4SF V2DF])

;; Iterator for logical types supported by VSX
(define_mode_iterator VSX_L [V16QI
			     V8HI
			     V4SI
			     V2DI
			     V4SF
			     V2DF
			     V1TI
			     TI
			     (KF	"FLOAT128_VECTOR_P (KFmode)")
			     (TF	"FLOAT128_VECTOR_P (TFmode)")])

;; Iterator for memory moves.
(define_mode_iterator VSX_M [V16QI
			     V8HI
			     V4SI
			     V2DI
			     V4SF
			     V2DF
			     V1TI
			     (KF	"FLOAT128_VECTOR_P (KFmode)")
			     (TF	"FLOAT128_VECTOR_P (TFmode)")
			     (TI	"TARGET_VSX_TIMODE")])

;; Map into the appropriate load/store name based on the type
(define_mode_attr VSm  [(V16QI "vw4")
			(V8HI  "vw4")
			(V4SI  "vw4")
			(V4SF  "vw4")
			(V2DF  "vd2")
			(V2DI  "vd2")
			(DF    "d")
			(TF    "vd2")
			(KF    "vd2")
			(V1TI  "vd2")
			(TI    "vd2")])

;; Map into the appropriate suffix based on the type
(define_mode_attr VSs	[(V16QI "sp")
			 (V8HI  "sp")
			 (V4SI  "sp")
			 (V4SF  "sp")
			 (V2DF  "dp")
			 (V2DI  "dp")
			 (DF    "dp")
			 (SF	"sp")
			 (TF    "dp")
			 (KF    "dp")
			 (V1TI  "dp")
			 (TI    "dp")])

;; Map the register class used
(define_mode_attr VSr	[(V16QI "v")
			 (V8HI  "v")
			 (V4SI  "v")
			 (V4SF  "wf")
			 (V2DI  "wd")
			 (V2DF  "wd")
			 (DI	"wi")
			 (DF    "ws")
			 (SF	"ww")
			 (TF	"wp")
			 (KF	"wq")
			 (V1TI  "v")
			 (TI    "wt")])

;; Map the register class used for float<->int conversions (floating point side)
;; VSr2 is the preferred register class, VSr3 is any register class that will
;; hold the data
(define_mode_attr VSr2	[(V2DF  "wd")
			 (V4SF  "wf")
			 (DF    "ws")
			 (SF	"ww")
			 (DI	"wi")])

(define_mode_attr VSr3	[(V2DF  "wa")
			 (V4SF  "wa")
			 (DF    "ws")
			 (SF	"ww")
			 (DI	"wi")])

;; Map the register class for sp<->dp float conversions, destination
(define_mode_attr VSr4	[(SF	"ws")
			 (DF	"f")
			 (V2DF  "wd")
			 (V4SF	"v")])

;; Map the register class for sp<->dp float conversions, source
(define_mode_attr VSr5	[(SF	"ws")
			 (DF	"f")
			 (V2DF  "v")
			 (V4SF	"wd")])

;; The VSX register class that a type can occupy, even if it is not the
;; preferred register class (VSr is the preferred register class that will get
;; allocated first).
(define_mode_attr VSa	[(V16QI "wa")
			 (V8HI  "wa")
			 (V4SI  "wa")
			 (V4SF  "wa")
			 (V2DI  "wa")
			 (V2DF  "wa")
			 (DI	"wi")
			 (DF    "ws")
			 (SF	"ww")
			 (V1TI	"wa")
			 (TI    "wt")
			 (TF	"wp")
			 (KF	"wq")])

;; Same size integer type for floating point data
(define_mode_attr VSi [(V4SF  "v4si")
		       (V2DF  "v2di")
		       (DF    "di")])

(define_mode_attr VSI [(V4SF  "V4SI")
		       (V2DF  "V2DI")
		       (DF    "DI")])

;; Word size for same size conversion
(define_mode_attr VSc [(V4SF "w")
		       (V2DF "d")
		       (DF   "d")])

;; Map into either s or v, depending on whether this is a scalar or vector
;; operation
(define_mode_attr VSv	[(V16QI "v")
			 (V8HI  "v")
			 (V4SI  "v")
			 (V4SF  "v")
			 (V2DI  "v")
			 (V2DF  "v")
			 (V1TI  "v")
			 (DF    "s")
			 (KF	"v")])

;; Appropriate type for add ops (and other simple FP ops)
(define_mode_attr VStype_simple	[(V2DF "vecdouble")
				 (V4SF "vecfloat")
				 (DF   "fp")])

(define_mode_attr VSfptype_simple [(V2DF "fp_addsub_d")
				   (V4SF "fp_addsub_s")
				   (DF   "fp_addsub_d")])

;; Appropriate type for multiply ops
(define_mode_attr VStype_mul	[(V2DF "vecdouble")
				 (V4SF "vecfloat")
				 (DF   "dmul")])

(define_mode_attr VSfptype_mul	[(V2DF "fp_mul_d")
				 (V4SF "fp_mul_s")
				 (DF   "fp_mul_d")])

;; Appropriate type for divide ops.
(define_mode_attr VStype_div	[(V2DF "vecdiv")
				 (V4SF "vecfdiv")
				 (DF   "ddiv")])

(define_mode_attr VSfptype_div	[(V2DF "fp_div_d")
				 (V4SF "fp_div_s")
				 (DF   "fp_div_d")])

;; Appropriate type for sqrt ops.  For now, just lump the vector sqrt with
;; the scalar sqrt
(define_mode_attr VStype_sqrt	[(V2DF "dsqrt")
				 (V4SF "ssqrt")
				 (DF   "dsqrt")])

(define_mode_attr VSfptype_sqrt	[(V2DF "fp_sqrt_d")
				 (V4SF "fp_sqrt_s")
				 (DF   "fp_sqrt_d")])

;; Iterator and modes for sp<->dp conversions
;; Because scalar SF values are represented internally as double, use the
;; V4SF type to represent this than SF.
(define_mode_iterator VSX_SPDP [DF V4SF V2DF])

(define_mode_attr VS_spdp_res [(DF	"V4SF")
			       (V4SF	"V2DF")
			       (V2DF	"V4SF")])

(define_mode_attr VS_spdp_insn [(DF	"xscvdpsp")
				(V4SF	"xvcvspdp")
				(V2DF	"xvcvdpsp")])

(define_mode_attr VS_spdp_type [(DF	"fp")
				(V4SF	"vecdouble")
				(V2DF	"vecdouble")])

;; Map the scalar mode for a vector type
(define_mode_attr VS_scalar [(V1TI	"TI")
			     (V2DF	"DF")
			     (V2DI	"DI")
			     (V4SF	"SF")
			     (V4SI	"SI")
			     (V8HI	"HI")
			     (V16QI	"QI")])

;; Map to a double-sized vector mode
(define_mode_attr VS_double [(V4SI	"V8SI")
			     (V4SF	"V8SF")
			     (V2DI	"V4DI")
			     (V2DF	"V4DF")
			     (V1TI	"V2TI")])

;; Map register class for 64-bit element in 128-bit vector for direct moves
;; to/from gprs
(define_mode_attr VS_64dm [(V2DF	"wk")
			   (V2DI	"wj")])

;; Map register class for 64-bit element in 128-bit vector for normal register
;; to register moves
(define_mode_attr VS_64reg [(V2DF	"ws")
			    (V2DI	"wi")])

;; Iterators for loading constants with xxspltib
(define_mode_iterator VSINT_84  [V4SI V2DI DI])
(define_mode_iterator VSINT_842 [V8HI V4SI V2DI])

;; Iterator for ISA 3.0 vector extract/insert of integer vectors
(define_mode_iterator VSX_EXTRACT_I [V16QI V8HI V4SI])

(define_mode_attr VSX_EXTRACT_WIDTH [(V16QI "b")
		  		     (V8HI "h")
				     (V4SI "w")])

;; Mode attribute to give the correct predicate for ISA 3.0 vector extract and
;; insert to validate the operand number.
(define_mode_attr VSX_EXTRACT_PREDICATE [(V16QI "const_0_to_15_operand")
					 (V8HI  "const_0_to_7_operand")
					 (V4SI  "const_0_to_3_operand")])

;; Mode attribute to give the constraint for vector extract and insert
;; operations.
(define_mode_attr VSX_EX [(V16QI "v")
			  (V8HI  "v")
			  (V4SI  "wa")])

;; Iterator for the 2 short vector types to do a splat from an integer
(define_mode_iterator VSX_SPLAT_I [V16QI V8HI])

;; Mode attribute to give the count for the splat instruction to splat
;; the value in the 64-bit integer slot
(define_mode_attr VSX_SPLAT_COUNT [(V16QI "7") (V8HI "3")])

;; Mode attribute to give the suffix for the splat instruction
(define_mode_attr VSX_SPLAT_SUFFIX [(V16QI "b") (V8HI "h")])

;; Constants for creating unspecs
(define_c_enum "unspec"
  [UNSPEC_VSX_CONCAT
   UNSPEC_VSX_CVDPSXWS
   UNSPEC_VSX_CVDPUXWS
   UNSPEC_VSX_CVSPDP
   UNSPEC_VSX_CVSPDPN
   UNSPEC_VSX_CVDPSPN
   UNSPEC_VSX_CVSXWDP
   UNSPEC_VSX_CVUXWDP
   UNSPEC_VSX_CVSXDSP
   UNSPEC_VSX_CVUXDSP
   UNSPEC_VSX_CVSPSXDS
   UNSPEC_VSX_CVSPUXDS
   UNSPEC_VSX_TDIV
   UNSPEC_VSX_TSQRT
   UNSPEC_VSX_SET
   UNSPEC_VSX_ROUND_I
   UNSPEC_VSX_ROUND_IC
   UNSPEC_VSX_SLDWI
   UNSPEC_VSX_XXSPLTW
   UNSPEC_VSX_XXSPLTD
   UNSPEC_VSX_DIVSD
   UNSPEC_VSX_DIVUD
   UNSPEC_VSX_MULSD
   UNSPEC_VSX_XVCVSXDDP
   UNSPEC_VSX_XVCVUXDDP
   UNSPEC_VSX_XVCVDPSXDS
   UNSPEC_VSX_XVCVDPUXDS
   UNSPEC_VSX_SIGN_EXTEND
   UNSPEC_P9_MEMORY
   UNSPEC_VSX_VSLO
   UNSPEC_VSX_EXTRACT
   UNSPEC_VSX_SXEXPDP
   UNSPEC_VSX_SXSIGDP
   UNSPEC_VSX_SIEXPDP
   UNSPEC_VSX_SCMPEXPDP
   UNSPEC_VSX_STSTDC
   UNSPEC_VSX_VXEXP
   UNSPEC_VSX_VXSIG
   UNSPEC_VSX_VIEXP
   UNSPEC_VSX_VTSTDC
   UNSPEC_VSX_VEC_INIT
   UNSPEC_LXVL
   UNSPEC_STXVL
   UNSPEC_VCLZLSBB
   UNSPEC_VCTZLSBB
   UNSPEC_VEXTUBLX
   UNSPEC_VEXTUHLX
   UNSPEC_VEXTUWLX
   UNSPEC_VEXTUBRX
   UNSPEC_VEXTUHRX
   UNSPEC_VEXTUWRX
   UNSPEC_VCMPNEB
   UNSPEC_VCMPNEZB
   UNSPEC_VCMPNEH
   UNSPEC_VCMPNEZH
   UNSPEC_VCMPNEW
   UNSPEC_VCMPNEZW
  ])

;; VSX moves

;; The patterns for LE permuted loads and stores come before the general
;; VSX moves so they match first.
(define_insn_and_split "*vsx_le_perm_load_<mode>"
  [(set (match_operand:VSX_LE 0 "vsx_register_operand" "=<VSa>")
        (match_operand:VSX_LE 1 "memory_operand" "Z"))]
  "!BYTES_BIG_ENDIAN && TARGET_VSX && !TARGET_P9_VECTOR"
  "#"
  "!BYTES_BIG_ENDIAN && TARGET_VSX && !TARGET_P9_VECTOR"
  [(set (match_dup 2)
        (vec_select:<MODE>
          (match_dup 1)
          (parallel [(const_int 1) (const_int 0)])))
   (set (match_dup 0)
        (vec_select:<MODE>
          (match_dup 2)
          (parallel [(const_int 1) (const_int 0)])))]
  "
{
  operands[2] = can_create_pseudo_p () ? gen_reg_rtx_and_attrs (operands[0])
                                       : operands[0];
}
  "
  [(set_attr "type" "vecload")
   (set_attr "length" "8")])

(define_insn_and_split "*vsx_le_perm_load_<mode>"
  [(set (match_operand:VSX_W 0 "vsx_register_operand" "=<VSa>")
        (match_operand:VSX_W 1 "memory_operand" "Z"))]
  "!BYTES_BIG_ENDIAN && TARGET_VSX && !TARGET_P9_VECTOR"
  "#"
  "!BYTES_BIG_ENDIAN && TARGET_VSX && !TARGET_P9_VECTOR"
  [(set (match_dup 2)
        (vec_select:<MODE>
          (match_dup 1)
          (parallel [(const_int 2) (const_int 3)
                     (const_int 0) (const_int 1)])))
   (set (match_dup 0)
        (vec_select:<MODE>
          (match_dup 2)
          (parallel [(const_int 2) (const_int 3)
                     (const_int 0) (const_int 1)])))]
  "
{
  operands[2] = can_create_pseudo_p () ? gen_reg_rtx_and_attrs (operands[0])
                                       : operands[0];
}
  "
  [(set_attr "type" "vecload")
   (set_attr "length" "8")])

(define_insn_and_split "*vsx_le_perm_load_v8hi"
  [(set (match_operand:V8HI 0 "vsx_register_operand" "=wa")
        (match_operand:V8HI 1 "memory_operand" "Z"))]
  "!BYTES_BIG_ENDIAN && TARGET_VSX && !TARGET_P9_VECTOR"
  "#"
  "!BYTES_BIG_ENDIAN && TARGET_VSX && !TARGET_P9_VECTOR"
  [(set (match_dup 2)
        (vec_select:V8HI
          (match_dup 1)
          (parallel [(const_int 4) (const_int 5)
                     (const_int 6) (const_int 7)
                     (const_int 0) (const_int 1)
                     (const_int 2) (const_int 3)])))
   (set (match_dup 0)
        (vec_select:V8HI
          (match_dup 2)
          (parallel [(const_int 4) (const_int 5)
                     (const_int 6) (const_int 7)
                     (const_int 0) (const_int 1)
                     (const_int 2) (const_int 3)])))]
  "
{
  operands[2] = can_create_pseudo_p () ? gen_reg_rtx_and_attrs (operands[0])
                                       : operands[0];
}
  "
  [(set_attr "type" "vecload")
   (set_attr "length" "8")])

(define_insn_and_split "*vsx_le_perm_load_v16qi"
  [(set (match_operand:V16QI 0 "vsx_register_operand" "=wa")
        (match_operand:V16QI 1 "memory_operand" "Z"))]
  "!BYTES_BIG_ENDIAN && TARGET_VSX && !TARGET_P9_VECTOR"
  "#"
  "!BYTES_BIG_ENDIAN && TARGET_VSX && !TARGET_P9_VECTOR"
  [(set (match_dup 2)
        (vec_select:V16QI
          (match_dup 1)
          (parallel [(const_int 8) (const_int 9)
                     (const_int 10) (const_int 11)
                     (const_int 12) (const_int 13)
                     (const_int 14) (const_int 15)
                     (const_int 0) (const_int 1)
                     (const_int 2) (const_int 3)
                     (const_int 4) (const_int 5)
                     (const_int 6) (const_int 7)])))
   (set (match_dup 0)
        (vec_select:V16QI
          (match_dup 2)
          (parallel [(const_int 8) (const_int 9)
                     (const_int 10) (const_int 11)
                     (const_int 12) (const_int 13)
                     (const_int 14) (const_int 15)
                     (const_int 0) (const_int 1)
                     (const_int 2) (const_int 3)
                     (const_int 4) (const_int 5)
                     (const_int 6) (const_int 7)])))]
  "
{
  operands[2] = can_create_pseudo_p () ? gen_reg_rtx_and_attrs (operands[0])
                                       : operands[0];
}
  "
  [(set_attr "type" "vecload")
   (set_attr "length" "8")])

(define_insn "*vsx_le_perm_store_<mode>"
  [(set (match_operand:VSX_LE 0 "memory_operand" "=Z")
        (match_operand:VSX_LE 1 "vsx_register_operand" "+<VSa>"))]
  "!BYTES_BIG_ENDIAN && TARGET_VSX && !TARGET_P9_VECTOR"
  "#"
  [(set_attr "type" "vecstore")
   (set_attr "length" "12")])

(define_split
  [(set (match_operand:VSX_LE 0 "memory_operand" "")
        (match_operand:VSX_LE 1 "vsx_register_operand" ""))]
  "!BYTES_BIG_ENDIAN && TARGET_VSX && !TARGET_P9_VECTOR && !reload_completed"
  [(set (match_dup 2)
        (vec_select:<MODE>
          (match_dup 1)
          (parallel [(const_int 1) (const_int 0)])))
   (set (match_dup 0)
        (vec_select:<MODE>
          (match_dup 2)
          (parallel [(const_int 1) (const_int 0)])))]
{
  operands[2] = can_create_pseudo_p () ? gen_reg_rtx_and_attrs (operands[1]) 
                                       : operands[1];
})

;; The post-reload split requires that we re-permute the source
;; register in case it is still live.
(define_split
  [(set (match_operand:VSX_LE 0 "memory_operand" "")
        (match_operand:VSX_LE 1 "vsx_register_operand" ""))]
  "!BYTES_BIG_ENDIAN && TARGET_VSX && !TARGET_P9_VECTOR && reload_completed"
  [(set (match_dup 1)
        (vec_select:<MODE>
          (match_dup 1)
          (parallel [(const_int 1) (const_int 0)])))
   (set (match_dup 0)
        (vec_select:<MODE>
          (match_dup 1)
          (parallel [(const_int 1) (const_int 0)])))
   (set (match_dup 1)
        (vec_select:<MODE>
          (match_dup 1)
          (parallel [(const_int 1) (const_int 0)])))]
  "")

(define_insn "*vsx_le_perm_store_<mode>"
  [(set (match_operand:VSX_W 0 "memory_operand" "=Z")
        (match_operand:VSX_W 1 "vsx_register_operand" "+<VSa>"))]
  "!BYTES_BIG_ENDIAN && TARGET_VSX && !TARGET_P9_VECTOR"
  "#"
  [(set_attr "type" "vecstore")
   (set_attr "length" "12")])

(define_split
  [(set (match_operand:VSX_W 0 "memory_operand" "")
        (match_operand:VSX_W 1 "vsx_register_operand" ""))]
  "!BYTES_BIG_ENDIAN && TARGET_VSX && !TARGET_P9_VECTOR && !reload_completed"
  [(set (match_dup 2)
        (vec_select:<MODE>
          (match_dup 1)
          (parallel [(const_int 2) (const_int 3)
	             (const_int 0) (const_int 1)])))
   (set (match_dup 0)
        (vec_select:<MODE>
          (match_dup 2)
          (parallel [(const_int 2) (const_int 3)
	             (const_int 0) (const_int 1)])))]
{
  operands[2] = can_create_pseudo_p () ? gen_reg_rtx_and_attrs (operands[1]) 
                                       : operands[1];
})

;; The post-reload split requires that we re-permute the source
;; register in case it is still live.
(define_split
  [(set (match_operand:VSX_W 0 "memory_operand" "")
        (match_operand:VSX_W 1 "vsx_register_operand" ""))]
  "!BYTES_BIG_ENDIAN && TARGET_VSX && !TARGET_P9_VECTOR && reload_completed"
  [(set (match_dup 1)
        (vec_select:<MODE>
          (match_dup 1)
          (parallel [(const_int 2) (const_int 3)
	             (const_int 0) (const_int 1)])))
   (set (match_dup 0)
        (vec_select:<MODE>
          (match_dup 1)
          (parallel [(const_int 2) (const_int 3)
	             (const_int 0) (const_int 1)])))
   (set (match_dup 1)
        (vec_select:<MODE>
          (match_dup 1)
          (parallel [(const_int 2) (const_int 3)
	             (const_int 0) (const_int 1)])))]
  "")

(define_insn "*vsx_le_perm_store_v8hi"
  [(set (match_operand:V8HI 0 "memory_operand" "=Z")
        (match_operand:V8HI 1 "vsx_register_operand" "+wa"))]
  "!BYTES_BIG_ENDIAN && TARGET_VSX && !TARGET_P9_VECTOR"
  "#"
  [(set_attr "type" "vecstore")
   (set_attr "length" "12")])

(define_split
  [(set (match_operand:V8HI 0 "memory_operand" "")
        (match_operand:V8HI 1 "vsx_register_operand" ""))]
  "!BYTES_BIG_ENDIAN && TARGET_VSX && !TARGET_P9_VECTOR && !reload_completed"
  [(set (match_dup 2)
        (vec_select:V8HI
          (match_dup 1)
          (parallel [(const_int 4) (const_int 5)
                     (const_int 6) (const_int 7)
                     (const_int 0) (const_int 1)
                     (const_int 2) (const_int 3)])))
   (set (match_dup 0)
        (vec_select:V8HI
          (match_dup 2)
          (parallel [(const_int 4) (const_int 5)
                     (const_int 6) (const_int 7)
                     (const_int 0) (const_int 1)
                     (const_int 2) (const_int 3)])))]
{
  operands[2] = can_create_pseudo_p () ? gen_reg_rtx_and_attrs (operands[1]) 
                                       : operands[1];
})

;; The post-reload split requires that we re-permute the source
;; register in case it is still live.
(define_split
  [(set (match_operand:V8HI 0 "memory_operand" "")
        (match_operand:V8HI 1 "vsx_register_operand" ""))]
  "!BYTES_BIG_ENDIAN && TARGET_VSX && !TARGET_P9_VECTOR && reload_completed"
  [(set (match_dup 1)
        (vec_select:V8HI
          (match_dup 1)
          (parallel [(const_int 4) (const_int 5)
                     (const_int 6) (const_int 7)
                     (const_int 0) (const_int 1)
                     (const_int 2) (const_int 3)])))
   (set (match_dup 0)
        (vec_select:V8HI
          (match_dup 1)
          (parallel [(const_int 4) (const_int 5)
                     (const_int 6) (const_int 7)
                     (const_int 0) (const_int 1)
                     (const_int 2) (const_int 3)])))
   (set (match_dup 1)
        (vec_select:V8HI
          (match_dup 1)
          (parallel [(const_int 4) (const_int 5)
                     (const_int 6) (const_int 7)
                     (const_int 0) (const_int 1)
                     (const_int 2) (const_int 3)])))]
  "")

(define_insn "*vsx_le_perm_store_v16qi"
  [(set (match_operand:V16QI 0 "memory_operand" "=Z")
        (match_operand:V16QI 1 "vsx_register_operand" "+wa"))]
  "!BYTES_BIG_ENDIAN && TARGET_VSX && !TARGET_P9_VECTOR"
  "#"
  [(set_attr "type" "vecstore")
   (set_attr "length" "12")])

(define_split
  [(set (match_operand:V16QI 0 "memory_operand" "")
        (match_operand:V16QI 1 "vsx_register_operand" ""))]
  "!BYTES_BIG_ENDIAN && TARGET_VSX && !TARGET_P9_VECTOR && !reload_completed"
  [(set (match_dup 2)
        (vec_select:V16QI
          (match_dup 1)
          (parallel [(const_int 8) (const_int 9)
                     (const_int 10) (const_int 11)
                     (const_int 12) (const_int 13)
                     (const_int 14) (const_int 15)
                     (const_int 0) (const_int 1)
                     (const_int 2) (const_int 3)
                     (const_int 4) (const_int 5)
                     (const_int 6) (const_int 7)])))
   (set (match_dup 0)
        (vec_select:V16QI
          (match_dup 2)
          (parallel [(const_int 8) (const_int 9)
                     (const_int 10) (const_int 11)
                     (const_int 12) (const_int 13)
                     (const_int 14) (const_int 15)
                     (const_int 0) (const_int 1)
                     (const_int 2) (const_int 3)
                     (const_int 4) (const_int 5)
                     (const_int 6) (const_int 7)])))]
{
  operands[2] = can_create_pseudo_p () ? gen_reg_rtx_and_attrs (operands[1]) 
                                       : operands[1];
})

;; The post-reload split requires that we re-permute the source
;; register in case it is still live.
(define_split
  [(set (match_operand:V16QI 0 "memory_operand" "")
        (match_operand:V16QI 1 "vsx_register_operand" ""))]
  "!BYTES_BIG_ENDIAN && TARGET_VSX && !TARGET_P9_VECTOR && reload_completed"
  [(set (match_dup 1)
        (vec_select:V16QI
          (match_dup 1)
          (parallel [(const_int 8) (const_int 9)
                     (const_int 10) (const_int 11)
                     (const_int 12) (const_int 13)
                     (const_int 14) (const_int 15)
                     (const_int 0) (const_int 1)
                     (const_int 2) (const_int 3)
                     (const_int 4) (const_int 5)
                     (const_int 6) (const_int 7)])))
   (set (match_dup 0)
        (vec_select:V16QI
          (match_dup 1)
          (parallel [(const_int 8) (const_int 9)
                     (const_int 10) (const_int 11)
                     (const_int 12) (const_int 13)
                     (const_int 14) (const_int 15)
                     (const_int 0) (const_int 1)
                     (const_int 2) (const_int 3)
                     (const_int 4) (const_int 5)
                     (const_int 6) (const_int 7)])))
   (set (match_dup 1)
        (vec_select:V16QI
          (match_dup 1)
          (parallel [(const_int 8) (const_int 9)
                     (const_int 10) (const_int 11)
                     (const_int 12) (const_int 13)
                     (const_int 14) (const_int 15)
                     (const_int 0) (const_int 1)
                     (const_int 2) (const_int 3)
                     (const_int 4) (const_int 5)
                     (const_int 6) (const_int 7)])))]
  "")

;; Little endian word swapping for 128-bit types that are either scalars or the
;; special V1TI container class, which it is not appropriate to use vec_select
;; for the type.
(define_insn "*vsx_le_permute_<mode>"
  [(set (match_operand:VSX_LE_128 0 "nonimmediate_operand" "=<VSa>,<VSa>,Z")
	(rotate:VSX_LE_128
	 (match_operand:VSX_LE_128 1 "input_operand" "<VSa>,Z,<VSa>")
	 (const_int 64)))]
  "!BYTES_BIG_ENDIAN && TARGET_VSX && !TARGET_P9_VECTOR"
  "@
   xxpermdi %x0,%x1,%x1,2
   lxvd2x %x0,%y1
   stxvd2x %x1,%y0"
  [(set_attr "length" "4")
   (set_attr "type" "vecperm,vecload,vecstore")])

(define_insn_and_split "*vsx_le_undo_permute_<mode>"
  [(set (match_operand:VSX_LE_128 0 "vsx_register_operand" "=<VSa>,<VSa>")
	(rotate:VSX_LE_128
	 (rotate:VSX_LE_128
	  (match_operand:VSX_LE_128 1 "vsx_register_operand" "0,<VSa>")
	  (const_int 64))
	 (const_int 64)))]
  "!BYTES_BIG_ENDIAN && TARGET_VSX"
  "@
   #
   xxlor %x0,%x1"
  ""
  [(set (match_dup 0) (match_dup 1))]
{
  if (reload_completed && REGNO (operands[0]) == REGNO (operands[1]))
    {
      emit_note (NOTE_INSN_DELETED);
      DONE;
    }
}
  [(set_attr "length" "0,4")
   (set_attr "type" "veclogical")])

(define_insn_and_split "*vsx_le_perm_load_<mode>"
  [(set (match_operand:VSX_LE_128 0 "vsx_register_operand" "=<VSa>")
        (match_operand:VSX_LE_128 1 "memory_operand" "Z"))]
  "!BYTES_BIG_ENDIAN && TARGET_VSX && !TARGET_P9_VECTOR"
  "#"
  "!BYTES_BIG_ENDIAN && TARGET_VSX && !TARGET_P9_VECTOR"
  [(set (match_dup 2)
	(rotate:VSX_LE_128 (match_dup 1)
			   (const_int 64)))
   (set (match_dup 0)
	(rotate:VSX_LE_128 (match_dup 2)
			   (const_int 64)))]
  "
{
  operands[2] = can_create_pseudo_p () ? gen_reg_rtx_and_attrs (operands[0])
                                       : operands[0];
}
  "
  [(set_attr "type" "vecload")
   (set_attr "length" "8")])

(define_insn "*vsx_le_perm_store_<mode>"
  [(set (match_operand:VSX_LE_128 0 "memory_operand" "=Z")
        (match_operand:VSX_LE_128 1 "vsx_register_operand" "+<VSa>"))]
  "!BYTES_BIG_ENDIAN && TARGET_VSX && !TARGET_P9_VECTOR"
  "#"
  [(set_attr "type" "vecstore")
   (set_attr "length" "12")])

(define_split
  [(set (match_operand:VSX_LE_128 0 "memory_operand" "")
        (match_operand:VSX_LE_128 1 "vsx_register_operand" ""))]
  "!BYTES_BIG_ENDIAN && TARGET_VSX && !reload_completed && !TARGET_P9_VECTOR"
  [(set (match_dup 2)
	(rotate:VSX_LE_128 (match_dup 1)
			   (const_int 64)))
   (set (match_dup 0)
	(rotate:VSX_LE_128 (match_dup 2)
			   (const_int 64)))]
{
  operands[2] = can_create_pseudo_p () ? gen_reg_rtx_and_attrs (operands[0])
                                       : operands[0];
})

;; Peephole to catch memory to memory transfers for TImode if TImode landed in
;; VSX registers on a little endian system.  The vector types and IEEE 128-bit
;; floating point are handled by the more generic swap elimination pass.
(define_peephole2
  [(set (match_operand:TI 0 "vsx_register_operand" "")
	(rotate:TI (match_operand:TI 1 "vsx_register_operand" "")
		   (const_int 64)))
   (set (match_operand:TI 2 "vsx_register_operand" "")
	(rotate:TI (match_dup 0)
		   (const_int 64)))]
  "!BYTES_BIG_ENDIAN && TARGET_VSX && TARGET_VSX_TIMODE && !TARGET_P9_VECTOR
   && (rtx_equal_p (operands[0], operands[2])
       || peep2_reg_dead_p (2, operands[0]))"
   [(set (match_dup 2) (match_dup 1))])

;; The post-reload split requires that we re-permute the source
;; register in case it is still live.
(define_split
  [(set (match_operand:VSX_LE_128 0 "memory_operand" "")
        (match_operand:VSX_LE_128 1 "vsx_register_operand" ""))]
  "!BYTES_BIG_ENDIAN && TARGET_VSX && reload_completed && !TARGET_P9_VECTOR"
  [(set (match_dup 1)
	(rotate:VSX_LE_128 (match_dup 1)
			   (const_int 64)))
   (set (match_dup 0)
	(rotate:VSX_LE_128 (match_dup 1)
			   (const_int 64)))
   (set (match_dup 1)
	(rotate:VSX_LE_128 (match_dup 1)
			   (const_int 64)))]
  "")

;; Vector constants that can be generated with XXSPLTIB that was added in ISA
;; 3.0.  Both (const_vector [..]) and (vec_duplicate ...) forms are recognized.
(define_insn "xxspltib_v16qi"
  [(set (match_operand:V16QI 0 "vsx_register_operand" "=wa")
	(vec_duplicate:V16QI (match_operand:SI 1 "s8bit_cint_operand" "n")))]
  "TARGET_P9_VECTOR"
{
  operands[2] = GEN_INT (INTVAL (operands[1]) & 0xff);
  return "xxspltib %x0,%2";
}
  [(set_attr "type" "vecperm")])

(define_insn "xxspltib_<mode>_nosplit"
  [(set (match_operand:VSINT_842 0 "vsx_register_operand" "=wa,wa")
	(match_operand:VSINT_842 1 "xxspltib_constant_nosplit" "jwM,wE"))]
  "TARGET_P9_VECTOR"
{
  rtx op1 = operands[1];
  int value = 256;
  int num_insns = -1;

  if (!xxspltib_constant_p (op1, <MODE>mode, &num_insns, &value)
      || num_insns != 1)
    gcc_unreachable ();

  operands[2] = GEN_INT (value & 0xff);
  return "xxspltib %x0,%2";
}
  [(set_attr "type" "vecperm")])

(define_insn_and_split "*xxspltib_<mode>_split"
  [(set (match_operand:VSINT_842 0 "altivec_register_operand" "=v")
	(match_operand:VSINT_842 1 "xxspltib_constant_split" "wS"))]
  "TARGET_P9_VECTOR"
  "#"
  "&& 1"
  [(const_int 0)]
{
  int value = 256;
  int num_insns = -1;
  rtx op0 = operands[0];
  rtx op1 = operands[1];
  rtx tmp = ((can_create_pseudo_p ())
	     ? gen_reg_rtx (V16QImode)
	     : gen_lowpart (V16QImode, op0));

  if (!xxspltib_constant_p (op1, <MODE>mode, &num_insns, &value)
      || num_insns != 2)
    gcc_unreachable ();

  emit_insn (gen_xxspltib_v16qi (tmp, GEN_INT (value)));

  if (<MODE>mode == V2DImode)
    emit_insn (gen_vsx_sign_extend_qi_v2di (op0, tmp));

  else if (<MODE>mode == V4SImode)
    emit_insn (gen_vsx_sign_extend_qi_v4si (op0, tmp));

  else if (<MODE>mode == V8HImode)
    emit_insn (gen_altivec_vupkhsb  (op0, tmp));

  else
    gcc_unreachable ();

  DONE;
}
  [(set_attr "type" "vecperm")
   (set_attr "length" "8")])


;; Prefer using vector registers over GPRs.  Prefer using ISA 3.0's XXSPLTISB
;; or Altivec VSPLITW 0/-1 over XXLXOR/XXLORC to set a register to all 0's or
;; all 1's, since the machine does not have to wait for the previous
;; instruction using the register being set (such as a store waiting on a slow
;; instruction). But generate XXLXOR/XXLORC if it will avoid a register move.

;;              VSX store  VSX load   VSX move  VSX->GPR   GPR->VSX    LQ (GPR)
;;              STQ (GPR)  GPR load   GPR store GPR move   XXSPLTIB    VSPLTISW
;;              VSX 0/-1   GPR 0/-1   VMX const GPR const  LVX (VMX)   STVX (VMX)
(define_insn "*vsx_mov<mode>_64bit"
  [(set (match_operand:VSX_M 0 "nonimmediate_operand"
               "=ZwO,      <VSa>,     <VSa>,     r,         we,        ?wQ,
                ?&r,       ??r,       ??Y,       ??r,       wo,        v,
                ?<VSa>,    *r,        v,         ??r,       wZ,        v")

	(match_operand:VSX_M 1 "input_operand" 
               "<VSa>,     ZwO,       <VSa>,     we,        r,         r,
                wQ,        Y,         r,         r,         wE,        jwM,
                ?jwM,      jwM,       W,         W,         v,         wZ"))]

  "TARGET_POWERPC64 && VECTOR_MEM_VSX_P (<MODE>mode)
   && (register_operand (operands[0], <MODE>mode) 
       || register_operand (operands[1], <MODE>mode))"
{
  return rs6000_output_move_128bit (operands);
}
  [(set_attr "type"
               "vecstore,  vecload,   vecsimple, mffgpr,    mftgpr,    load,
                store,     load,      store,     *,         vecsimple, vecsimple,
                vecsimple, *,         *,         *,         vecstore,  vecload")

   (set_attr "length"
               "4,         4,         4,         8,         4,         8,
                8,         8,         8,         8,         4,         4,
                4,         8,         20,        20,        4,         4")])

;;              VSX store  VSX load   VSX move   GPR load   GPR store  GPR move
;;              XXSPLTIB   VSPLTISW   VSX 0/-1   GPR 0/-1   VMX const  GPR const
;;              LVX (VMX)  STVX (VMX)
(define_insn "*vsx_mov<mode>_32bit"
  [(set (match_operand:VSX_M 0 "nonimmediate_operand"
               "=ZwO,      <VSa>,     <VSa>,     ??r,       ??Y,       ??r,
                wo,        v,         ?<VSa>,    *r,        v,         ??r,
                wZ,        v")

	(match_operand:VSX_M 1 "input_operand" 
               "<VSa>,     ZwO,       <VSa>,     Y,         r,         r,
                wE,        jwM,       ?jwM,      jwM,       W,         W,
                v,         wZ"))]

  "!TARGET_POWERPC64 && VECTOR_MEM_VSX_P (<MODE>mode)
   && (register_operand (operands[0], <MODE>mode) 
       || register_operand (operands[1], <MODE>mode))"
{
  return rs6000_output_move_128bit (operands);
}
  [(set_attr "type"
               "vecstore,  vecload,   vecsimple, load,      store,    *,
                vecsimple, vecsimple, vecsimple, *,         *,        *,
                vecstore,  vecload")

   (set_attr "length"
               "4,         4,         4,         16,        16,        16,
                4,         4,         4,         16,        20,        32,
                4,         4")])

;; Explicit  load/store expanders for the builtin functions
(define_expand "vsx_load_<mode>"
  [(set (match_operand:VSX_M 0 "vsx_register_operand" "")
	(match_operand:VSX_M 1 "memory_operand" ""))]
  "VECTOR_MEM_VSX_P (<MODE>mode)"
{
  /* Expand to swaps if needed, prior to swap optimization.  */
  if (!BYTES_BIG_ENDIAN && !TARGET_P9_VECTOR)
    {
      rs6000_emit_le_vsx_move (operands[0], operands[1], <MODE>mode);
      DONE;
    }
})

(define_expand "vsx_store_<mode>"
  [(set (match_operand:VSX_M 0 "memory_operand" "")
	(match_operand:VSX_M 1 "vsx_register_operand" ""))]
  "VECTOR_MEM_VSX_P (<MODE>mode)"
{
  /* Expand to swaps if needed, prior to swap optimization.  */
  if (!BYTES_BIG_ENDIAN && !TARGET_P9_VECTOR)
    {
      rs6000_emit_le_vsx_move (operands[0], operands[1], <MODE>mode);
      DONE;
    }
})

;; Explicit load/store expanders for the builtin functions for lxvd2x, etc.,
;; when you really want their element-reversing behavior.
(define_insn "vsx_ld_elemrev_v2di"
  [(set (match_operand:V2DI 0 "vsx_register_operand" "=wa")
        (vec_select:V2DI
	  (match_operand:V2DI 1 "memory_operand" "Z")
	  (parallel [(const_int 1) (const_int 0)])))]
  "VECTOR_MEM_VSX_P (V2DImode) && !BYTES_BIG_ENDIAN"
  "lxvd2x %x0,%y1"
  [(set_attr "type" "vecload")])

(define_insn "vsx_ld_elemrev_v2df"
  [(set (match_operand:V2DF 0 "vsx_register_operand" "=wa")
        (vec_select:V2DF
	  (match_operand:V2DF 1 "memory_operand" "Z")
	  (parallel [(const_int 1) (const_int 0)])))]
  "VECTOR_MEM_VSX_P (V2DFmode) && !BYTES_BIG_ENDIAN"
  "lxvd2x %x0,%y1"
  [(set_attr "type" "vecload")])

(define_insn "vsx_ld_elemrev_v4si"
  [(set (match_operand:V4SI 0 "vsx_register_operand" "=wa")
        (vec_select:V4SI
	  (match_operand:V4SI 1 "memory_operand" "Z")
	  (parallel [(const_int 3) (const_int 2)
	             (const_int 1) (const_int 0)])))]
  "VECTOR_MEM_VSX_P (V4SImode) && !BYTES_BIG_ENDIAN"
  "lxvw4x %x0,%y1"
  [(set_attr "type" "vecload")])

(define_insn "vsx_ld_elemrev_v4sf"
  [(set (match_operand:V4SF 0 "vsx_register_operand" "=wa")
        (vec_select:V4SF
	  (match_operand:V4SF 1 "memory_operand" "Z")
	  (parallel [(const_int 3) (const_int 2)
	             (const_int 1) (const_int 0)])))]
  "VECTOR_MEM_VSX_P (V4SFmode) && !BYTES_BIG_ENDIAN"
  "lxvw4x %x0,%y1"
  [(set_attr "type" "vecload")])

(define_insn "vsx_ld_elemrev_v8hi"
  [(set (match_operand:V8HI 0 "vsx_register_operand" "=wa")
        (vec_select:V8HI
	  (match_operand:V8HI 1 "memory_operand" "Z")
	  (parallel [(const_int 7) (const_int 6)
	             (const_int 5) (const_int 4)
		     (const_int 3) (const_int 2)
	             (const_int 1) (const_int 0)])))]
  "VECTOR_MEM_VSX_P (V8HImode) && !BYTES_BIG_ENDIAN && TARGET_P9_VECTOR"
  "lxvh8x %x0,%y1"
  [(set_attr "type" "vecload")])

(define_insn "vsx_ld_elemrev_v16qi"
  [(set (match_operand:V16QI 0 "vsx_register_operand" "=wa")
        (vec_select:V16QI
	  (match_operand:V16QI 1 "memory_operand" "Z")
	  (parallel [(const_int 15) (const_int 14)
	             (const_int 13) (const_int 12)
		     (const_int 11) (const_int 10)
		     (const_int  9) (const_int  8)
		     (const_int  7) (const_int  6)
	             (const_int  5) (const_int  4)
		     (const_int  3) (const_int  2)
	             (const_int  1) (const_int  0)])))]
  "VECTOR_MEM_VSX_P (V16QImode) && !BYTES_BIG_ENDIAN && TARGET_P9_VECTOR"
  "lxvb16x %x0,%y1"
  [(set_attr "type" "vecload")])

(define_insn "vsx_st_elemrev_v2df"
  [(set (match_operand:V2DF 0 "memory_operand" "=Z")
        (vec_select:V2DF
	  (match_operand:V2DF 1 "vsx_register_operand" "wa")
	  (parallel [(const_int 1) (const_int 0)])))]
  "VECTOR_MEM_VSX_P (V2DFmode) && !BYTES_BIG_ENDIAN"
  "stxvd2x %x1,%y0"
  [(set_attr "type" "vecstore")])

(define_insn "vsx_st_elemrev_v2di"
  [(set (match_operand:V2DI 0 "memory_operand" "=Z")
        (vec_select:V2DI
	  (match_operand:V2DI 1 "vsx_register_operand" "wa")
	  (parallel [(const_int 1) (const_int 0)])))]
  "VECTOR_MEM_VSX_P (V2DImode) && !BYTES_BIG_ENDIAN"
  "stxvd2x %x1,%y0"
  [(set_attr "type" "vecstore")])

(define_insn "vsx_st_elemrev_v4sf"
  [(set (match_operand:V4SF 0 "memory_operand" "=Z")
        (vec_select:V4SF
	  (match_operand:V4SF 1 "vsx_register_operand" "wa")
	  (parallel [(const_int 3) (const_int 2)
	             (const_int 1) (const_int 0)])))]
  "VECTOR_MEM_VSX_P (V4SFmode) && !BYTES_BIG_ENDIAN"
  "stxvw4x %x1,%y0"
  [(set_attr "type" "vecstore")])

(define_insn "vsx_st_elemrev_v4si"
  [(set (match_operand:V4SI 0 "memory_operand" "=Z")
        (vec_select:V4SI
	  (match_operand:V4SI 1 "vsx_register_operand" "wa")
	  (parallel [(const_int 3) (const_int 2)
	             (const_int 1) (const_int 0)])))]
  "VECTOR_MEM_VSX_P (V4SImode) && !BYTES_BIG_ENDIAN"
  "stxvw4x %x1,%y0"
  [(set_attr "type" "vecstore")])

(define_insn "vsx_st_elemrev_v8hi"
  [(set (match_operand:V8HI 0 "memory_operand" "=Z")
        (vec_select:V8HI
	  (match_operand:V8HI 1 "vsx_register_operand" "wa")
	  (parallel [(const_int 7) (const_int 6)
	             (const_int 5) (const_int 4)
		     (const_int 3) (const_int 2)
	             (const_int 1) (const_int 0)])))]
  "VECTOR_MEM_VSX_P (V8HImode) && !BYTES_BIG_ENDIAN && TARGET_P9_VECTOR"
  "stxvh8x %x1,%y0"
  [(set_attr "type" "vecstore")])

(define_insn "vsx_st_elemrev_v16qi"
  [(set (match_operand:V16QI 0 "memory_operand" "=Z")
        (vec_select:V16QI
	  (match_operand:V16QI 1 "vsx_register_operand" "wa")
	  (parallel [(const_int 15) (const_int 14)
	             (const_int 13) (const_int 12)
		     (const_int 11) (const_int 10)
		     (const_int  9) (const_int  8)
	             (const_int  7) (const_int  6)
	             (const_int  5) (const_int  4)
		     (const_int  3) (const_int  2)
	             (const_int  1) (const_int  0)])))]
  "VECTOR_MEM_VSX_P (V16QImode) && !BYTES_BIG_ENDIAN && TARGET_P9_VECTOR"
  "stxvb16x %x1,%y0"
  [(set_attr "type" "vecstore")])


;; VSX vector floating point arithmetic instructions.  The VSX scalar
;; instructions are now combined with the insn for the traditional floating
;; point unit.
(define_insn "*vsx_add<mode>3"
  [(set (match_operand:VSX_F 0 "vsx_register_operand" "=<VSr>,?<VSa>")
        (plus:VSX_F (match_operand:VSX_F 1 "vsx_register_operand" "<VSr>,<VSa>")
		    (match_operand:VSX_F 2 "vsx_register_operand" "<VSr>,<VSa>")))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "xvadd<VSs> %x0,%x1,%x2"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_simple>")])

(define_insn "*vsx_sub<mode>3"
  [(set (match_operand:VSX_F 0 "vsx_register_operand" "=<VSr>,?<VSa>")
        (minus:VSX_F (match_operand:VSX_F 1 "vsx_register_operand" "<VSr>,<VSa>")
		     (match_operand:VSX_F 2 "vsx_register_operand" "<VSr>,<VSa>")))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "xvsub<VSs> %x0,%x1,%x2"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_simple>")])

(define_insn "*vsx_mul<mode>3"
  [(set (match_operand:VSX_F 0 "vsx_register_operand" "=<VSr>,?<VSa>")
        (mult:VSX_F (match_operand:VSX_F 1 "vsx_register_operand" "<VSr>,<VSa>")
		    (match_operand:VSX_F 2 "vsx_register_operand" "<VSr>,<VSa>")))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "xvmul<VSs> %x0,%x1,%x2"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_mul>")])

; Emulate vector with scalar for vec_mul in V2DImode
(define_insn_and_split "vsx_mul_v2di"
  [(set (match_operand:V2DI 0 "vsx_register_operand" "=wa")
        (unspec:V2DI [(match_operand:V2DI 1 "vsx_register_operand" "wa")
                      (match_operand:V2DI 2 "vsx_register_operand" "wa")]
                     UNSPEC_VSX_MULSD))]
  "VECTOR_MEM_VSX_P (V2DImode)"
  "#"
  "VECTOR_MEM_VSX_P (V2DImode) && !reload_completed && !reload_in_progress"
  [(const_int 0)]
  "
{
  rtx op0 = operands[0];
  rtx op1 = operands[1];
  rtx op2 = operands[2];
  rtx op3 = gen_reg_rtx (DImode);
  rtx op4 = gen_reg_rtx (DImode);
  rtx op5 = gen_reg_rtx (DImode);
  emit_insn (gen_vsx_extract_v2di (op3, op1, GEN_INT (0)));
  emit_insn (gen_vsx_extract_v2di (op4, op2, GEN_INT (0)));
  emit_insn (gen_muldi3 (op5, op3, op4));
  emit_insn (gen_vsx_extract_v2di (op3, op1, GEN_INT (1)));
  emit_insn (gen_vsx_extract_v2di (op4, op2, GEN_INT (1)));
  emit_insn (gen_muldi3 (op3, op3, op4));
  emit_insn (gen_vsx_concat_v2di (op0, op5, op3));
  DONE;
}"
  [(set_attr "type" "mul")])

(define_insn "*vsx_div<mode>3"
  [(set (match_operand:VSX_F 0 "vsx_register_operand" "=<VSr>,?<VSa>")
        (div:VSX_F (match_operand:VSX_F 1 "vsx_register_operand" "<VSr>,<VSa>")
		   (match_operand:VSX_F 2 "vsx_register_operand" "<VSr>,<VSa>")))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "xvdiv<VSs> %x0,%x1,%x2"
  [(set_attr "type" "<VStype_div>")
   (set_attr "fp_type" "<VSfptype_div>")])

; Emulate vector with scalar for vec_div in V2DImode
(define_insn_and_split "vsx_div_v2di"
  [(set (match_operand:V2DI 0 "vsx_register_operand" "=wa")
        (unspec:V2DI [(match_operand:V2DI 1 "vsx_register_operand" "wa")
                      (match_operand:V2DI 2 "vsx_register_operand" "wa")]
                     UNSPEC_VSX_DIVSD))]
  "VECTOR_MEM_VSX_P (V2DImode)"
  "#"
  "VECTOR_MEM_VSX_P (V2DImode) && !reload_completed && !reload_in_progress"
  [(const_int 0)]
  "
{
  rtx op0 = operands[0];
  rtx op1 = operands[1];
  rtx op2 = operands[2];
  rtx op3 = gen_reg_rtx (DImode);
  rtx op4 = gen_reg_rtx (DImode);
  rtx op5 = gen_reg_rtx (DImode);
  emit_insn (gen_vsx_extract_v2di (op3, op1, GEN_INT (0)));
  emit_insn (gen_vsx_extract_v2di (op4, op2, GEN_INT (0)));
  emit_insn (gen_divdi3 (op5, op3, op4));
  emit_insn (gen_vsx_extract_v2di (op3, op1, GEN_INT (1)));
  emit_insn (gen_vsx_extract_v2di (op4, op2, GEN_INT (1)));
  emit_insn (gen_divdi3 (op3, op3, op4));
  emit_insn (gen_vsx_concat_v2di (op0, op5, op3));
  DONE;
}"
  [(set_attr "type" "div")])

(define_insn_and_split "vsx_udiv_v2di"
  [(set (match_operand:V2DI 0 "vsx_register_operand" "=wa")
        (unspec:V2DI [(match_operand:V2DI 1 "vsx_register_operand" "wa")
                      (match_operand:V2DI 2 "vsx_register_operand" "wa")]
                     UNSPEC_VSX_DIVUD))]
  "VECTOR_MEM_VSX_P (V2DImode)"
  "#"
  "VECTOR_MEM_VSX_P (V2DImode) && !reload_completed && !reload_in_progress"
  [(const_int 0)]
  "
{
  rtx op0 = operands[0];
  rtx op1 = operands[1];
  rtx op2 = operands[2];
  rtx op3 = gen_reg_rtx (DImode);
  rtx op4 = gen_reg_rtx (DImode);
  rtx op5 = gen_reg_rtx (DImode);
  emit_insn (gen_vsx_extract_v2di (op3, op1, GEN_INT (0)));
  emit_insn (gen_vsx_extract_v2di (op4, op2, GEN_INT (0)));
  emit_insn (gen_udivdi3 (op5, op3, op4));
  emit_insn (gen_vsx_extract_v2di (op3, op1, GEN_INT (1)));
  emit_insn (gen_vsx_extract_v2di (op4, op2, GEN_INT (1)));
  emit_insn (gen_udivdi3 (op3, op3, op4));
  emit_insn (gen_vsx_concat_v2di (op0, op5, op3));
  DONE;
}"
  [(set_attr "type" "div")])

;; *tdiv* instruction returning the FG flag
(define_expand "vsx_tdiv<mode>3_fg"
  [(set (match_dup 3)
	(unspec:CCFP [(match_operand:VSX_B 1 "vsx_register_operand" "")
		      (match_operand:VSX_B 2 "vsx_register_operand" "")]
		     UNSPEC_VSX_TDIV))
   (set (match_operand:SI 0 "gpc_reg_operand" "")
	(gt:SI (match_dup 3)
	       (const_int 0)))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
{
  operands[3] = gen_reg_rtx (CCFPmode);
})

;; *tdiv* instruction returning the FE flag
(define_expand "vsx_tdiv<mode>3_fe"
  [(set (match_dup 3)
	(unspec:CCFP [(match_operand:VSX_B 1 "vsx_register_operand" "")
		      (match_operand:VSX_B 2 "vsx_register_operand" "")]
		     UNSPEC_VSX_TDIV))
   (set (match_operand:SI 0 "gpc_reg_operand" "")
	(eq:SI (match_dup 3)
	       (const_int 0)))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
{
  operands[3] = gen_reg_rtx (CCFPmode);
})

(define_insn "*vsx_tdiv<mode>3_internal"
  [(set (match_operand:CCFP 0 "cc_reg_operand" "=x,x")
	(unspec:CCFP [(match_operand:VSX_B 1 "vsx_register_operand" "<VSr>,<VSa>")
		      (match_operand:VSX_B 2 "vsx_register_operand" "<VSr>,<VSa>")]
		   UNSPEC_VSX_TDIV))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "x<VSv>tdiv<VSs> %0,%x1,%x2"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_simple>")])

(define_insn "vsx_fre<mode>2"
  [(set (match_operand:VSX_F 0 "vsx_register_operand" "=<VSr>,?<VSa>")
	(unspec:VSX_F [(match_operand:VSX_F 1 "vsx_register_operand" "<VSr>,<VSa>")]
		      UNSPEC_FRES))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "xvre<VSs> %x0,%x1"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_simple>")])

(define_insn "*vsx_neg<mode>2"
  [(set (match_operand:VSX_F 0 "vsx_register_operand" "=<VSr>,?<VSa>")
        (neg:VSX_F (match_operand:VSX_F 1 "vsx_register_operand" "<VSr>,<VSa>")))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "xvneg<VSs> %x0,%x1"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_simple>")])

(define_insn "*vsx_abs<mode>2"
  [(set (match_operand:VSX_F 0 "vsx_register_operand" "=<VSr>,?<VSa>")
        (abs:VSX_F (match_operand:VSX_F 1 "vsx_register_operand" "<VSr>,<VSa>")))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "xvabs<VSs> %x0,%x1"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_simple>")])

(define_insn "vsx_nabs<mode>2"
  [(set (match_operand:VSX_F 0 "vsx_register_operand" "=<VSr>,?<VSa>")
        (neg:VSX_F
	 (abs:VSX_F
	  (match_operand:VSX_F 1 "vsx_register_operand" "<VSr>,<VSa>"))))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "xvnabs<VSs> %x0,%x1"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_simple>")])

(define_insn "vsx_smax<mode>3"
  [(set (match_operand:VSX_F 0 "vsx_register_operand" "=<VSr>,?<VSa>")
        (smax:VSX_F (match_operand:VSX_F 1 "vsx_register_operand" "<VSr>,<VSa>")
		    (match_operand:VSX_F 2 "vsx_register_operand" "<VSr>,<VSa>")))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "xvmax<VSs> %x0,%x1,%x2"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_simple>")])

(define_insn "*vsx_smin<mode>3"
  [(set (match_operand:VSX_F 0 "vsx_register_operand" "=<VSr>,?<VSa>")
        (smin:VSX_F (match_operand:VSX_F 1 "vsx_register_operand" "<VSr>,<VSa>")
		    (match_operand:VSX_F 2 "vsx_register_operand" "<VSr>,<VSa>")))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "xvmin<VSs> %x0,%x1,%x2"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_simple>")])

(define_insn "*vsx_sqrt<mode>2"
  [(set (match_operand:VSX_F 0 "vsx_register_operand" "=<VSr>,?<VSa>")
        (sqrt:VSX_F (match_operand:VSX_F 1 "vsx_register_operand" "<VSr>,<VSa>")))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "xvsqrt<VSs> %x0,%x1"
  [(set_attr "type" "<VStype_sqrt>")
   (set_attr "fp_type" "<VSfptype_sqrt>")])

(define_insn "*vsx_rsqrte<mode>2"
  [(set (match_operand:VSX_F 0 "vsx_register_operand" "=<VSr>,?<VSa>")
	(unspec:VSX_F [(match_operand:VSX_F 1 "vsx_register_operand" "<VSr>,<VSa>")]
		      UNSPEC_RSQRT))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "xvrsqrte<VSs> %x0,%x1"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_simple>")])

;; *tsqrt* returning the fg flag
(define_expand "vsx_tsqrt<mode>2_fg"
  [(set (match_dup 3)
	(unspec:CCFP [(match_operand:VSX_B 1 "vsx_register_operand" "")]
		     UNSPEC_VSX_TSQRT))
   (set (match_operand:SI 0 "gpc_reg_operand" "")
	(gt:SI (match_dup 3)
	       (const_int 0)))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
{
  operands[3] = gen_reg_rtx (CCFPmode);
})

;; *tsqrt* returning the fe flag
(define_expand "vsx_tsqrt<mode>2_fe"
  [(set (match_dup 3)
	(unspec:CCFP [(match_operand:VSX_B 1 "vsx_register_operand" "")]
		     UNSPEC_VSX_TSQRT))
   (set (match_operand:SI 0 "gpc_reg_operand" "")
	(eq:SI (match_dup 3)
	       (const_int 0)))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
{
  operands[3] = gen_reg_rtx (CCFPmode);
})

(define_insn "*vsx_tsqrt<mode>2_internal"
  [(set (match_operand:CCFP 0 "cc_reg_operand" "=x,x")
	(unspec:CCFP [(match_operand:VSX_B 1 "vsx_register_operand" "<VSr>,<VSa>")]
		     UNSPEC_VSX_TSQRT))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "x<VSv>tsqrt<VSs> %0,%x1"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_simple>")])

;; Fused vector multiply/add instructions. Support the classical Altivec
;; versions of fma, which allows the target to be a separate register from the
;; 3 inputs.  Under VSX, the target must be either the addend or the first
;; multiply.

(define_insn "*vsx_fmav4sf4"
  [(set (match_operand:V4SF 0 "vsx_register_operand" "=wf,wf,?wa,?wa,v")
	(fma:V4SF
	  (match_operand:V4SF 1 "vsx_register_operand" "%wf,wf,wa,wa,v")
	  (match_operand:V4SF 2 "vsx_register_operand" "wf,0,wa,0,v")
	  (match_operand:V4SF 3 "vsx_register_operand" "0,wf,0,wa,v")))]
  "VECTOR_UNIT_VSX_P (V4SFmode)"
  "@
   xvmaddasp %x0,%x1,%x2
   xvmaddmsp %x0,%x1,%x3
   xvmaddasp %x0,%x1,%x2
   xvmaddmsp %x0,%x1,%x3
   vmaddfp %0,%1,%2,%3"
  [(set_attr "type" "vecfloat")])

(define_insn "*vsx_fmav2df4"
  [(set (match_operand:V2DF 0 "vsx_register_operand" "=wd,wd,?wa,?wa")
	(fma:V2DF
	  (match_operand:V2DF 1 "vsx_register_operand" "%wd,wd,wa,wa")
	  (match_operand:V2DF 2 "vsx_register_operand" "wd,0,wa,0")
	  (match_operand:V2DF 3 "vsx_register_operand" "0,wd,0,wa")))]
  "VECTOR_UNIT_VSX_P (V2DFmode)"
  "@
   xvmaddadp %x0,%x1,%x2
   xvmaddmdp %x0,%x1,%x3
   xvmaddadp %x0,%x1,%x2
   xvmaddmdp %x0,%x1,%x3"
  [(set_attr "type" "vecdouble")])

(define_insn "*vsx_fms<mode>4"
  [(set (match_operand:VSX_F 0 "vsx_register_operand" "=<VSr>,<VSr>,?<VSa>,?<VSa>")
	(fma:VSX_F
	  (match_operand:VSX_F 1 "vsx_register_operand" "%<VSr>,<VSr>,<VSa>,<VSa>")
	  (match_operand:VSX_F 2 "vsx_register_operand" "<VSr>,0,<VSa>,0")
	  (neg:VSX_F
	    (match_operand:VSX_F 3 "vsx_register_operand" "0,<VSr>,0,<VSa>"))))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "@
   xvmsuba<VSs> %x0,%x1,%x2
   xvmsubm<VSs> %x0,%x1,%x3
   xvmsuba<VSs> %x0,%x1,%x2
   xvmsubm<VSs> %x0,%x1,%x3"
  [(set_attr "type" "<VStype_mul>")])

(define_insn "*vsx_nfma<mode>4"
  [(set (match_operand:VSX_F 0 "vsx_register_operand" "=<VSr>,<VSr>,?<VSa>,?<VSa>")
	(neg:VSX_F
	 (fma:VSX_F
	  (match_operand:VSX_F 1 "vsx_register_operand" "<VSr>,<VSr>,<VSa>,<VSa>")
	  (match_operand:VSX_F 2 "vsx_register_operand" "<VSr>,0,<VSa>,0")
	  (match_operand:VSX_F 3 "vsx_register_operand" "0,<VSr>,0,<VSa>"))))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "@
   xvnmadda<VSs> %x0,%x1,%x2
   xvnmaddm<VSs> %x0,%x1,%x3
   xvnmadda<VSs> %x0,%x1,%x2
   xvnmaddm<VSs> %x0,%x1,%x3"
  [(set_attr "type" "<VStype_mul>")
   (set_attr "fp_type" "<VSfptype_mul>")])

(define_insn "*vsx_nfmsv4sf4"
  [(set (match_operand:V4SF 0 "vsx_register_operand" "=wf,wf,?wa,?wa,v")
	(neg:V4SF
	 (fma:V4SF
	   (match_operand:V4SF 1 "vsx_register_operand" "%wf,wf,wa,wa,v")
	   (match_operand:V4SF 2 "vsx_register_operand" "wf,0,wa,0,v")
	   (neg:V4SF
	     (match_operand:V4SF 3 "vsx_register_operand" "0,wf,0,wa,v")))))]
  "VECTOR_UNIT_VSX_P (V4SFmode)"
  "@
   xvnmsubasp %x0,%x1,%x2
   xvnmsubmsp %x0,%x1,%x3
   xvnmsubasp %x0,%x1,%x2
   xvnmsubmsp %x0,%x1,%x3
   vnmsubfp %0,%1,%2,%3"
  [(set_attr "type" "vecfloat")])

(define_insn "*vsx_nfmsv2df4"
  [(set (match_operand:V2DF 0 "vsx_register_operand" "=wd,wd,?wa,?wa")
	(neg:V2DF
	 (fma:V2DF
	   (match_operand:V2DF 1 "vsx_register_operand" "%wd,wd,wa,wa")
	   (match_operand:V2DF 2 "vsx_register_operand" "wd,0,wa,0")
	   (neg:V2DF
	     (match_operand:V2DF 3 "vsx_register_operand" "0,wd,0,wa")))))]
  "VECTOR_UNIT_VSX_P (V2DFmode)"
  "@
   xvnmsubadp %x0,%x1,%x2
   xvnmsubmdp %x0,%x1,%x3
   xvnmsubadp %x0,%x1,%x2
   xvnmsubmdp %x0,%x1,%x3"
  [(set_attr "type" "vecdouble")])

;; Vector conditional expressions (no scalar version for these instructions)
(define_insn "vsx_eq<mode>"
  [(set (match_operand:VSX_F 0 "vsx_register_operand" "=<VSr>,?<VSa>")
	(eq:VSX_F (match_operand:VSX_F 1 "vsx_register_operand" "<VSr>,<VSa>")
		  (match_operand:VSX_F 2 "vsx_register_operand" "<VSr>,<VSa>")))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "xvcmpeq<VSs> %x0,%x1,%x2"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_simple>")])

(define_insn "vsx_gt<mode>"
  [(set (match_operand:VSX_F 0 "vsx_register_operand" "=<VSr>,?<VSa>")
	(gt:VSX_F (match_operand:VSX_F 1 "vsx_register_operand" "<VSr>,<VSa>")
		  (match_operand:VSX_F 2 "vsx_register_operand" "<VSr>,<VSa>")))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "xvcmpgt<VSs> %x0,%x1,%x2"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_simple>")])

(define_insn "*vsx_ge<mode>"
  [(set (match_operand:VSX_F 0 "vsx_register_operand" "=<VSr>,?<VSa>")
	(ge:VSX_F (match_operand:VSX_F 1 "vsx_register_operand" "<VSr>,<VSa>")
		  (match_operand:VSX_F 2 "vsx_register_operand" "<VSr>,<VSa>")))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "xvcmpge<VSs> %x0,%x1,%x2"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_simple>")])

;; Compare vectors producing a vector result and a predicate, setting CR6 to
;; indicate a combined status
(define_insn "*vsx_eq_<mode>_p"
  [(set (reg:CC CR6_REGNO)
	(unspec:CC
	 [(eq:CC (match_operand:VSX_F 1 "vsx_register_operand" "<VSr>,?<VSa>")
		 (match_operand:VSX_F 2 "vsx_register_operand" "<VSr>,?<VSa>"))]
	 UNSPEC_PREDICATE))
   (set (match_operand:VSX_F 0 "vsx_register_operand" "=<VSr>,?<VSa>")
	(eq:VSX_F (match_dup 1)
		  (match_dup 2)))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "xvcmpeq<VSs>. %x0,%x1,%x2"
  [(set_attr "type" "<VStype_simple>")])

(define_insn "*vsx_gt_<mode>_p"
  [(set (reg:CC CR6_REGNO)
	(unspec:CC
	 [(gt:CC (match_operand:VSX_F 1 "vsx_register_operand" "<VSr>,?<VSa>")
		 (match_operand:VSX_F 2 "vsx_register_operand" "<VSr>,?<VSa>"))]
	 UNSPEC_PREDICATE))
   (set (match_operand:VSX_F 0 "vsx_register_operand" "=<VSr>,?<VSa>")
	(gt:VSX_F (match_dup 1)
		  (match_dup 2)))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "xvcmpgt<VSs>. %x0,%x1,%x2"
  [(set_attr "type" "<VStype_simple>")])

(define_insn "*vsx_ge_<mode>_p"
  [(set (reg:CC CR6_REGNO)
	(unspec:CC
	 [(ge:CC (match_operand:VSX_F 1 "vsx_register_operand" "<VSr>,?<VSa>")
		 (match_operand:VSX_F 2 "vsx_register_operand" "<VSr>,?<VSa>"))]
	 UNSPEC_PREDICATE))
   (set (match_operand:VSX_F 0 "vsx_register_operand" "=<VSr>,?<VSa>")
	(ge:VSX_F (match_dup 1)
		  (match_dup 2)))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "xvcmpge<VSs>. %x0,%x1,%x2"
  [(set_attr "type" "<VStype_simple>")])

;; Vector select
(define_insn "*vsx_xxsel<mode>"
  [(set (match_operand:VSX_L 0 "vsx_register_operand" "=<VSr>,?<VSa>")
	(if_then_else:VSX_L
	 (ne:CC (match_operand:VSX_L 1 "vsx_register_operand" "<VSr>,<VSa>")
		(match_operand:VSX_L 4 "zero_constant" ""))
	 (match_operand:VSX_L 2 "vsx_register_operand" "<VSr>,<VSa>")
	 (match_operand:VSX_L 3 "vsx_register_operand" "<VSr>,<VSa>")))]
  "VECTOR_MEM_VSX_P (<MODE>mode)"
  "xxsel %x0,%x3,%x2,%x1"
  [(set_attr "type" "vecmove")])

(define_insn "*vsx_xxsel<mode>_uns"
  [(set (match_operand:VSX_L 0 "vsx_register_operand" "=<VSr>,?<VSa>")
	(if_then_else:VSX_L
	 (ne:CCUNS (match_operand:VSX_L 1 "vsx_register_operand" "<VSr>,<VSa>")
		   (match_operand:VSX_L 4 "zero_constant" ""))
	 (match_operand:VSX_L 2 "vsx_register_operand" "<VSr>,<VSa>")
	 (match_operand:VSX_L 3 "vsx_register_operand" "<VSr>,<VSa>")))]
  "VECTOR_MEM_VSX_P (<MODE>mode)"
  "xxsel %x0,%x3,%x2,%x1"
  [(set_attr "type" "vecmove")])

;; Copy sign
(define_insn "vsx_copysign<mode>3"
  [(set (match_operand:VSX_F 0 "vsx_register_operand" "=<VSr>,?<VSa>")
	(unspec:VSX_F
	 [(match_operand:VSX_F 1 "vsx_register_operand" "<VSr>,<VSa>")
	  (match_operand:VSX_F 2 "vsx_register_operand" "<VSr>,<VSa>")]
	 UNSPEC_COPYSIGN))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "xvcpsgn<VSs> %x0,%x2,%x1"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_simple>")])

;; For the conversions, limit the register class for the integer value to be
;; the fprs because we don't want to add the altivec registers to movdi/movsi.
;; For the unsigned tests, there isn't a generic double -> unsigned conversion
;; in rs6000.md so don't test VECTOR_UNIT_VSX_P, just test against VSX.
;; Don't use vsx_register_operand here, use gpc_reg_operand to match rs6000.md
;; in allowing virtual registers.
(define_insn "vsx_float<VSi><mode>2"
  [(set (match_operand:VSX_F 0 "gpc_reg_operand" "=<VSr>,?<VSa>")
	(float:VSX_F (match_operand:<VSI> 1 "gpc_reg_operand" "<VSr2>,<VSr3>")))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "xvcvsx<VSc><VSs> %x0,%x1"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_simple>")])

(define_insn "vsx_floatuns<VSi><mode>2"
  [(set (match_operand:VSX_F 0 "gpc_reg_operand" "=<VSr>,?<VSa>")
	(unsigned_float:VSX_F (match_operand:<VSI> 1 "gpc_reg_operand" "<VSr2>,<VSr3>")))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "xvcvux<VSc><VSs> %x0,%x1"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_simple>")])

(define_insn "vsx_fix_trunc<mode><VSi>2"
  [(set (match_operand:<VSI> 0 "gpc_reg_operand" "=<VSr2>,?<VSr3>")
	(fix:<VSI> (match_operand:VSX_F 1 "gpc_reg_operand" "<VSr>,<VSa>")))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "x<VSv>cv<VSs>sx<VSc>s %x0,%x1"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_simple>")])

(define_insn "vsx_fixuns_trunc<mode><VSi>2"
  [(set (match_operand:<VSI> 0 "gpc_reg_operand" "=<VSr2>,?<VSr3>")
	(unsigned_fix:<VSI> (match_operand:VSX_F 1 "gpc_reg_operand" "<VSr>,<VSa>")))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "x<VSv>cv<VSs>ux<VSc>s %x0,%x1"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_simple>")])

;; Math rounding functions
(define_insn "vsx_x<VSv>r<VSs>i"
  [(set (match_operand:VSX_B 0 "vsx_register_operand" "=<VSr>,?<VSa>")
	(unspec:VSX_B [(match_operand:VSX_B 1 "vsx_register_operand" "<VSr>,<VSa>")]
		      UNSPEC_VSX_ROUND_I))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "x<VSv>r<VSs>i %x0,%x1"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_simple>")])

(define_insn "vsx_x<VSv>r<VSs>ic"
  [(set (match_operand:VSX_B 0 "vsx_register_operand" "=<VSr>,?<VSa>")
	(unspec:VSX_B [(match_operand:VSX_B 1 "vsx_register_operand" "<VSr>,<VSa>")]
		      UNSPEC_VSX_ROUND_IC))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "x<VSv>r<VSs>ic %x0,%x1"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_simple>")])

(define_insn "vsx_btrunc<mode>2"
  [(set (match_operand:VSX_F 0 "vsx_register_operand" "=<VSr>,?<VSa>")
	(fix:VSX_F (match_operand:VSX_F 1 "vsx_register_operand" "<VSr>,<VSa>")))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "xvr<VSs>iz %x0,%x1"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_simple>")])

(define_insn "*vsx_b2trunc<mode>2"
  [(set (match_operand:VSX_B 0 "vsx_register_operand" "=<VSr>,?<VSa>")
	(unspec:VSX_B [(match_operand:VSX_B 1 "vsx_register_operand" "<VSr>,<VSa>")]
		      UNSPEC_FRIZ))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "x<VSv>r<VSs>iz %x0,%x1"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_simple>")])

(define_insn "vsx_floor<mode>2"
  [(set (match_operand:VSX_F 0 "vsx_register_operand" "=<VSr>,?<VSa>")
	(unspec:VSX_F [(match_operand:VSX_F 1 "vsx_register_operand" "<VSr>,<VSa>")]
		      UNSPEC_FRIM))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "xvr<VSs>im %x0,%x1"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_simple>")])

(define_insn "vsx_ceil<mode>2"
  [(set (match_operand:VSX_F 0 "vsx_register_operand" "=<VSr>,?<VSa>")
	(unspec:VSX_F [(match_operand:VSX_F 1 "vsx_register_operand" "<VSr>,<VSa>")]
		      UNSPEC_FRIP))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "xvr<VSs>ip %x0,%x1"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_simple>")])


;; VSX convert to/from double vector

;; Convert between single and double precision
;; Don't use xscvspdp and xscvdpsp for scalar conversions, since the normal
;; scalar single precision instructions internally use the double format.
;; Prefer the altivec registers, since we likely will need to do a vperm
(define_insn "vsx_<VS_spdp_insn>"
  [(set (match_operand:<VS_spdp_res> 0 "vsx_register_operand" "=<VSr4>,?<VSa>")
	(unspec:<VS_spdp_res> [(match_operand:VSX_SPDP 1 "vsx_register_operand" "<VSr5>,<VSa>")]
			      UNSPEC_VSX_CVSPDP))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "<VS_spdp_insn> %x0,%x1"
  [(set_attr "type" "<VS_spdp_type>")])

;; xscvspdp, represent the scalar SF type as V4SF
(define_insn "vsx_xscvspdp"
  [(set (match_operand:DF 0 "vsx_register_operand" "=ws")
	(unspec:DF [(match_operand:V4SF 1 "vsx_register_operand" "wa")]
		   UNSPEC_VSX_CVSPDP))]
  "VECTOR_UNIT_VSX_P (V4SFmode)"
  "xscvspdp %x0,%x1"
  [(set_attr "type" "fp")])

;; xscvdpsp used for splat'ing a scalar to V4SF, knowing that the internal SF
;; format of scalars is actually DF.
(define_insn "vsx_xscvdpsp_scalar"
  [(set (match_operand:V4SF 0 "vsx_register_operand" "=wa")
	(unspec:V4SF [(match_operand:SF 1 "vsx_register_operand" "f")]
		     UNSPEC_VSX_CVSPDP))]
  "VECTOR_UNIT_VSX_P (V4SFmode)"
  "xscvdpsp %x0,%x1"
  [(set_attr "type" "fp")])

;; Same as vsx_xscvspdp, but use SF as the type
(define_insn "vsx_xscvspdp_scalar2"
  [(set (match_operand:SF 0 "vsx_register_operand" "=ww")
	(unspec:SF [(match_operand:V4SF 1 "vsx_register_operand" "wa")]
		   UNSPEC_VSX_CVSPDP))]
  "VECTOR_UNIT_VSX_P (V4SFmode)"
  "xscvspdp %x0,%x1"
  [(set_attr "type" "fp")])

;; ISA 2.07 xscvdpspn/xscvspdpn that does not raise an error on signalling NaNs
(define_insn "vsx_xscvdpspn"
  [(set (match_operand:V4SF 0 "vsx_register_operand" "=ww,?ww")
	(unspec:V4SF [(match_operand:DF 1 "vsx_register_operand" "wd,wa")]
		     UNSPEC_VSX_CVDPSPN))]
  "TARGET_XSCVDPSPN"
  "xscvdpspn %x0,%x1"
  [(set_attr "type" "fp")])

(define_insn "vsx_xscvspdpn"
  [(set (match_operand:DF 0 "vsx_register_operand" "=ws,?ws")
	(unspec:DF [(match_operand:V4SF 1 "vsx_register_operand" "wf,wa")]
		   UNSPEC_VSX_CVSPDPN))]
  "TARGET_XSCVSPDPN"
  "xscvspdpn %x0,%x1"
  [(set_attr "type" "fp")])

(define_insn "vsx_xscvdpspn_scalar"
  [(set (match_operand:V4SF 0 "vsx_register_operand" "=wf,?wa")
	(unspec:V4SF [(match_operand:SF 1 "vsx_register_operand" "ww,ww")]
		     UNSPEC_VSX_CVDPSPN))]
  "TARGET_XSCVDPSPN"
  "xscvdpspn %x0,%x1"
  [(set_attr "type" "fp")])

;; Used by direct move to move a SFmode value from GPR to VSX register
(define_insn "vsx_xscvspdpn_directmove"
  [(set (match_operand:SF 0 "vsx_register_operand" "=wa")
	(unspec:SF [(match_operand:SF 1 "vsx_register_operand" "wa")]
		   UNSPEC_VSX_CVSPDPN))]
  "TARGET_XSCVSPDPN"
  "xscvspdpn %x0,%x1"
  [(set_attr "type" "fp")])

;; Convert and scale (used by vec_ctf, vec_cts, vec_ctu for double/long long)

(define_expand "vsx_xvcvsxddp_scale"
  [(match_operand:V2DF 0 "vsx_register_operand" "")
   (match_operand:V2DI 1 "vsx_register_operand" "")
   (match_operand:QI 2 "immediate_operand" "")]
  "VECTOR_UNIT_VSX_P (V2DFmode)"
{
  rtx op0 = operands[0];
  rtx op1 = operands[1];
  int scale = INTVAL(operands[2]);
  emit_insn (gen_vsx_xvcvsxddp (op0, op1));
  if (scale != 0)
    rs6000_scale_v2df (op0, op0, -scale);
  DONE;
})

(define_insn "vsx_xvcvsxddp"
  [(set (match_operand:V2DF 0 "vsx_register_operand" "=wa")
        (unspec:V2DF [(match_operand:V2DI 1 "vsx_register_operand" "wa")]
                     UNSPEC_VSX_XVCVSXDDP))]
  "VECTOR_UNIT_VSX_P (V2DFmode)"
  "xvcvsxddp %x0,%x1"
  [(set_attr "type" "vecdouble")])

(define_expand "vsx_xvcvuxddp_scale"
  [(match_operand:V2DF 0 "vsx_register_operand" "")
   (match_operand:V2DI 1 "vsx_register_operand" "")
   (match_operand:QI 2 "immediate_operand" "")]
  "VECTOR_UNIT_VSX_P (V2DFmode)"
{
  rtx op0 = operands[0];
  rtx op1 = operands[1];
  int scale = INTVAL(operands[2]);
  emit_insn (gen_vsx_xvcvuxddp (op0, op1));
  if (scale != 0)
    rs6000_scale_v2df (op0, op0, -scale);
  DONE;
})

(define_insn "vsx_xvcvuxddp"
  [(set (match_operand:V2DF 0 "vsx_register_operand" "=wa")
        (unspec:V2DF [(match_operand:V2DI 1 "vsx_register_operand" "wa")]
                     UNSPEC_VSX_XVCVUXDDP))]
  "VECTOR_UNIT_VSX_P (V2DFmode)"
  "xvcvuxddp %x0,%x1"
  [(set_attr "type" "vecdouble")])

(define_expand "vsx_xvcvdpsxds_scale"
  [(match_operand:V2DI 0 "vsx_register_operand" "")
   (match_operand:V2DF 1 "vsx_register_operand" "")
   (match_operand:QI 2 "immediate_operand" "")]
  "VECTOR_UNIT_VSX_P (V2DFmode)"
{
  rtx op0 = operands[0];
  rtx op1 = operands[1];
  rtx tmp;
  int scale = INTVAL (operands[2]);
  if (scale == 0)
    tmp = op1;
  else
    {
      tmp  = gen_reg_rtx (V2DFmode);
      rs6000_scale_v2df (tmp, op1, scale);
    }
  emit_insn (gen_vsx_xvcvdpsxds (op0, tmp));
  DONE;
})

(define_insn "vsx_xvcvdpsxds"
  [(set (match_operand:V2DI 0 "vsx_register_operand" "=wa")
        (unspec:V2DI [(match_operand:V2DF 1 "vsx_register_operand" "wa")]
                     UNSPEC_VSX_XVCVDPSXDS))]
  "VECTOR_UNIT_VSX_P (V2DFmode)"
  "xvcvdpsxds %x0,%x1"
  [(set_attr "type" "vecdouble")])

(define_expand "vsx_xvcvdpuxds_scale"
  [(match_operand:V2DI 0 "vsx_register_operand" "")
   (match_operand:V2DF 1 "vsx_register_operand" "")
   (match_operand:QI 2 "immediate_operand" "")]
  "VECTOR_UNIT_VSX_P (V2DFmode)"
{
  rtx op0 = operands[0];
  rtx op1 = operands[1];
  rtx tmp;
  int scale = INTVAL (operands[2]);
  if (scale == 0)
    tmp = op1;
  else
    {
      tmp = gen_reg_rtx (V2DFmode);
      rs6000_scale_v2df (tmp, op1, scale);
    }
  emit_insn (gen_vsx_xvcvdpuxds (op0, tmp));
  DONE;
})

(define_insn "vsx_xvcvdpuxds"
  [(set (match_operand:V2DI 0 "vsx_register_operand" "=wa")
        (unspec:V2DI [(match_operand:V2DF 1 "vsx_register_operand" "wa")]
                     UNSPEC_VSX_XVCVDPUXDS))]
  "VECTOR_UNIT_VSX_P (V2DFmode)"
  "xvcvdpuxds %x0,%x1"
  [(set_attr "type" "vecdouble")])

;; Convert from 64-bit to 32-bit types
;; Note, favor the Altivec registers since the usual use of these instructions
;; is in vector converts and we need to use the Altivec vperm instruction.

(define_insn "vsx_xvcvdpsxws"
  [(set (match_operand:V4SI 0 "vsx_register_operand" "=v,?wa")
	(unspec:V4SI [(match_operand:V2DF 1 "vsx_register_operand" "wd,wa")]
		     UNSPEC_VSX_CVDPSXWS))]
  "VECTOR_UNIT_VSX_P (V2DFmode)"
  "xvcvdpsxws %x0,%x1"
  [(set_attr "type" "vecdouble")])

(define_insn "vsx_xvcvdpuxws"
  [(set (match_operand:V4SI 0 "vsx_register_operand" "=v,?wa")
	(unspec:V4SI [(match_operand:V2DF 1 "vsx_register_operand" "wd,wa")]
		     UNSPEC_VSX_CVDPUXWS))]
  "VECTOR_UNIT_VSX_P (V2DFmode)"
  "xvcvdpuxws %x0,%x1"
  [(set_attr "type" "vecdouble")])

(define_insn "vsx_xvcvsxdsp"
  [(set (match_operand:V4SI 0 "vsx_register_operand" "=wd,?wa")
	(unspec:V4SI [(match_operand:V2DF 1 "vsx_register_operand" "wf,wa")]
		     UNSPEC_VSX_CVSXDSP))]
  "VECTOR_UNIT_VSX_P (V2DFmode)"
  "xvcvsxdsp %x0,%x1"
  [(set_attr "type" "vecfloat")])

(define_insn "vsx_xvcvuxdsp"
  [(set (match_operand:V4SI 0 "vsx_register_operand" "=wd,?wa")
	(unspec:V4SI [(match_operand:V2DF 1 "vsx_register_operand" "wf,wa")]
		     UNSPEC_VSX_CVUXDSP))]
  "VECTOR_UNIT_VSX_P (V2DFmode)"
  "xvcvuxwdp %x0,%x1"
  [(set_attr "type" "vecdouble")])

;; Convert from 32-bit to 64-bit types
(define_insn "vsx_xvcvsxwdp"
  [(set (match_operand:V2DF 0 "vsx_register_operand" "=wd,?wa")
	(unspec:V2DF [(match_operand:V4SI 1 "vsx_register_operand" "wf,wa")]
		     UNSPEC_VSX_CVSXWDP))]
  "VECTOR_UNIT_VSX_P (V2DFmode)"
  "xvcvsxwdp %x0,%x1"
  [(set_attr "type" "vecdouble")])

(define_insn "vsx_xvcvuxwdp"
  [(set (match_operand:V2DF 0 "vsx_register_operand" "=wd,?wa")
	(unspec:V2DF [(match_operand:V4SI 1 "vsx_register_operand" "wf,wa")]
		     UNSPEC_VSX_CVUXWDP))]
  "VECTOR_UNIT_VSX_P (V2DFmode)"
  "xvcvuxwdp %x0,%x1"
  [(set_attr "type" "vecdouble")])

(define_insn "vsx_xvcvspsxds"
  [(set (match_operand:V2DI 0 "vsx_register_operand" "=v,?wa")
	(unspec:V2DI [(match_operand:V4SF 1 "vsx_register_operand" "wd,wa")]
		     UNSPEC_VSX_CVSPSXDS))]
  "VECTOR_UNIT_VSX_P (V2DFmode)"
  "xvcvspsxds %x0,%x1"
  [(set_attr "type" "vecdouble")])

(define_insn "vsx_xvcvspuxds"
  [(set (match_operand:V2DI 0 "vsx_register_operand" "=v,?wa")
	(unspec:V2DI [(match_operand:V4SF 1 "vsx_register_operand" "wd,wa")]
		     UNSPEC_VSX_CVSPUXDS))]
  "VECTOR_UNIT_VSX_P (V2DFmode)"
  "xvcvspuxds %x0,%x1"
  [(set_attr "type" "vecdouble")])

;; Only optimize (float (fix x)) -> frz if we are in fast-math mode, since
;; since the xvrdpiz instruction does not truncate the value if the floating
;; point value is < LONG_MIN or > LONG_MAX.
(define_insn "*vsx_float_fix_v2df2"
  [(set (match_operand:V2DF 0 "vsx_register_operand" "=wd,?wa")
	(float:V2DF
	 (fix:V2DI
	  (match_operand:V2DF 1 "vsx_register_operand" "wd,?wa"))))]
  "TARGET_HARD_FLOAT && TARGET_FPRS && TARGET_DOUBLE_FLOAT
   && VECTOR_UNIT_VSX_P (V2DFmode) && flag_unsafe_math_optimizations
   && !flag_trapping_math && TARGET_FRIZ"
  "xvrdpiz %x0,%x1"
  [(set_attr "type" "vecdouble")
   (set_attr "fp_type" "fp_addsub_d")])


;; Permute operations

;; Build a V2DF/V2DI vector from two scalars
(define_insn "vsx_concat_<mode>"
  [(set (match_operand:VSX_D 0 "gpc_reg_operand" "=<VSa>,we")
	(vec_concat:VSX_D
	 (match_operand:<VS_scalar> 1 "gpc_reg_operand" "<VS_64reg>,b")
	 (match_operand:<VS_scalar> 2 "gpc_reg_operand" "<VS_64reg>,b")))]
  "VECTOR_MEM_VSX_P (<MODE>mode)"
{
  if (which_alternative == 0)
    return (BYTES_BIG_ENDIAN
	    ? "xxpermdi %x0,%x1,%x2,0"
	    : "xxpermdi %x0,%x2,%x1,0");

  else if (which_alternative == 1)
    return (BYTES_BIG_ENDIAN
	    ? "mtvsrdd %x0,%1,%2"
	    : "mtvsrdd %x0,%2,%1");

  else
    gcc_unreachable ();
}
  [(set_attr "type" "vecperm")])

;; Special purpose concat using xxpermdi to glue two single precision values
;; together, relying on the fact that internally scalar floats are represented
;; as doubles.  This is used to initialize a V4SF vector with 4 floats
(define_insn "vsx_concat_v2sf"
  [(set (match_operand:V2DF 0 "vsx_register_operand" "=wa")
	(unspec:V2DF
	 [(match_operand:SF 1 "vsx_register_operand" "ww")
	  (match_operand:SF 2 "vsx_register_operand" "ww")]
	 UNSPEC_VSX_CONCAT))]
  "VECTOR_MEM_VSX_P (V2DFmode)"
{
  if (BYTES_BIG_ENDIAN)
    return "xxpermdi %x0,%x1,%x2,0";
  else
    return "xxpermdi %x0,%x2,%x1,0";
}
  [(set_attr "type" "vecperm")])

;; V4SImode initialization splitter
(define_insn_and_split "vsx_init_v4si"
  [(set (match_operand:V4SI 0 "gpc_reg_operand" "=&r")
	(unspec:V4SI
	 [(match_operand:SI 1 "reg_or_cint_operand" "rn")
	  (match_operand:SI 2 "reg_or_cint_operand" "rn")
	  (match_operand:SI 3 "reg_or_cint_operand" "rn")
	  (match_operand:SI 4 "reg_or_cint_operand" "rn")]
	 UNSPEC_VSX_VEC_INIT))
   (clobber (match_scratch:DI 5 "=&r"))
   (clobber (match_scratch:DI 6 "=&r"))]
   "VECTOR_MEM_VSX_P (V4SImode) && TARGET_DIRECT_MOVE_64BIT"
   "#"
   "&& reload_completed"
   [(const_int 0)]
{
  rs6000_split_v4si_init (operands);
  DONE;
})

;; xxpermdi for little endian loads and stores.  We need several of
;; these since the form of the PARALLEL differs by mode.
(define_insn "*vsx_xxpermdi2_le_<mode>"
  [(set (match_operand:VSX_LE 0 "vsx_register_operand" "=<VSa>")
        (vec_select:VSX_LE
          (match_operand:VSX_LE 1 "vsx_register_operand" "<VSa>")
          (parallel [(const_int 1) (const_int 0)])))]
  "!BYTES_BIG_ENDIAN && VECTOR_MEM_VSX_P (<MODE>mode)"
  "xxpermdi %x0,%x1,%x1,2"
  [(set_attr "type" "vecperm")])

(define_insn "*vsx_xxpermdi4_le_<mode>"
  [(set (match_operand:VSX_W 0 "vsx_register_operand" "=<VSa>")
        (vec_select:VSX_W
          (match_operand:VSX_W 1 "vsx_register_operand" "<VSa>")
          (parallel [(const_int 2) (const_int 3)
                     (const_int 0) (const_int 1)])))]
  "!BYTES_BIG_ENDIAN && VECTOR_MEM_VSX_P (<MODE>mode)"
  "xxpermdi %x0,%x1,%x1,2"
  [(set_attr "type" "vecperm")])

(define_insn "*vsx_xxpermdi8_le_V8HI"
  [(set (match_operand:V8HI 0 "vsx_register_operand" "=wa")
        (vec_select:V8HI
          (match_operand:V8HI 1 "vsx_register_operand" "wa")
          (parallel [(const_int 4) (const_int 5)
                     (const_int 6) (const_int 7)
                     (const_int 0) (const_int 1)
                     (const_int 2) (const_int 3)])))]
  "!BYTES_BIG_ENDIAN && VECTOR_MEM_VSX_P (V8HImode)"
  "xxpermdi %x0,%x1,%x1,2"
  [(set_attr "type" "vecperm")])

(define_insn "*vsx_xxpermdi16_le_V16QI"
  [(set (match_operand:V16QI 0 "vsx_register_operand" "=wa")
        (vec_select:V16QI
          (match_operand:V16QI 1 "vsx_register_operand" "wa")
          (parallel [(const_int 8) (const_int 9)
                     (const_int 10) (const_int 11)
                     (const_int 12) (const_int 13)
                     (const_int 14) (const_int 15)
                     (const_int 0) (const_int 1)
                     (const_int 2) (const_int 3)
                     (const_int 4) (const_int 5)
                     (const_int 6) (const_int 7)])))]
  "!BYTES_BIG_ENDIAN && VECTOR_MEM_VSX_P (V16QImode)"
  "xxpermdi %x0,%x1,%x1,2"
  [(set_attr "type" "vecperm")])

;; lxvd2x for little endian loads.  We need several of
;; these since the form of the PARALLEL differs by mode.
(define_insn "*vsx_lxvd2x2_le_<mode>"
  [(set (match_operand:VSX_LE 0 "vsx_register_operand" "=<VSa>")
        (vec_select:VSX_LE
          (match_operand:VSX_LE 1 "memory_operand" "Z")
          (parallel [(const_int 1) (const_int 0)])))]
  "!BYTES_BIG_ENDIAN && VECTOR_MEM_VSX_P (<MODE>mode) && !TARGET_P9_VECTOR"
  "lxvd2x %x0,%y1"
  [(set_attr "type" "vecload")])

(define_insn "*vsx_lxvd2x4_le_<mode>"
  [(set (match_operand:VSX_W 0 "vsx_register_operand" "=<VSa>")
        (vec_select:VSX_W
          (match_operand:VSX_W 1 "memory_operand" "Z")
          (parallel [(const_int 2) (const_int 3)
                     (const_int 0) (const_int 1)])))]
  "!BYTES_BIG_ENDIAN && VECTOR_MEM_VSX_P (<MODE>mode) && !TARGET_P9_VECTOR"
  "lxvd2x %x0,%y1"
  [(set_attr "type" "vecload")])

(define_insn "*vsx_lxvd2x8_le_V8HI"
  [(set (match_operand:V8HI 0 "vsx_register_operand" "=wa")
        (vec_select:V8HI
          (match_operand:V8HI 1 "memory_operand" "Z")
          (parallel [(const_int 4) (const_int 5)
                     (const_int 6) (const_int 7)
                     (const_int 0) (const_int 1)
                     (const_int 2) (const_int 3)])))]
  "!BYTES_BIG_ENDIAN && VECTOR_MEM_VSX_P (V8HImode) && !TARGET_P9_VECTOR"
  "lxvd2x %x0,%y1"
  [(set_attr "type" "vecload")])

(define_insn "*vsx_lxvd2x16_le_V16QI"
  [(set (match_operand:V16QI 0 "vsx_register_operand" "=wa")
        (vec_select:V16QI
          (match_operand:V16QI 1 "memory_operand" "Z")
          (parallel [(const_int 8) (const_int 9)
                     (const_int 10) (const_int 11)
                     (const_int 12) (const_int 13)
                     (const_int 14) (const_int 15)
                     (const_int 0) (const_int 1)
                     (const_int 2) (const_int 3)
                     (const_int 4) (const_int 5)
                     (const_int 6) (const_int 7)])))]
  "!BYTES_BIG_ENDIAN && VECTOR_MEM_VSX_P (V16QImode) && !TARGET_P9_VECTOR"
  "lxvd2x %x0,%y1"
  [(set_attr "type" "vecload")])

;; stxvd2x for little endian stores.  We need several of
;; these since the form of the PARALLEL differs by mode.
(define_insn "*vsx_stxvd2x2_le_<mode>"
  [(set (match_operand:VSX_LE 0 "memory_operand" "=Z")
        (vec_select:VSX_LE
          (match_operand:VSX_LE 1 "vsx_register_operand" "<VSa>")
          (parallel [(const_int 1) (const_int 0)])))]
  "!BYTES_BIG_ENDIAN && VECTOR_MEM_VSX_P (<MODE>mode) && !TARGET_P9_VECTOR"
  "stxvd2x %x1,%y0"
  [(set_attr "type" "vecstore")])

(define_insn "*vsx_stxvd2x4_le_<mode>"
  [(set (match_operand:VSX_W 0 "memory_operand" "=Z")
        (vec_select:VSX_W
          (match_operand:VSX_W 1 "vsx_register_operand" "<VSa>")
          (parallel [(const_int 2) (const_int 3)
                     (const_int 0) (const_int 1)])))]
  "!BYTES_BIG_ENDIAN && VECTOR_MEM_VSX_P (<MODE>mode) && !TARGET_P9_VECTOR"
  "stxvd2x %x1,%y0"
  [(set_attr "type" "vecstore")])

(define_insn "*vsx_stxvd2x8_le_V8HI"
  [(set (match_operand:V8HI 0 "memory_operand" "=Z")
        (vec_select:V8HI
          (match_operand:V8HI 1 "vsx_register_operand" "wa")
          (parallel [(const_int 4) (const_int 5)
                     (const_int 6) (const_int 7)
                     (const_int 0) (const_int 1)
                     (const_int 2) (const_int 3)])))]
  "!BYTES_BIG_ENDIAN && VECTOR_MEM_VSX_P (V8HImode) && !TARGET_P9_VECTOR"
  "stxvd2x %x1,%y0"
  [(set_attr "type" "vecstore")])

(define_insn "*vsx_stxvd2x16_le_V16QI"
  [(set (match_operand:V16QI 0 "memory_operand" "=Z")
        (vec_select:V16QI
          (match_operand:V16QI 1 "vsx_register_operand" "wa")
          (parallel [(const_int 8) (const_int 9)
                     (const_int 10) (const_int 11)
                     (const_int 12) (const_int 13)
                     (const_int 14) (const_int 15)
                     (const_int 0) (const_int 1)
                     (const_int 2) (const_int 3)
                     (const_int 4) (const_int 5)
                     (const_int 6) (const_int 7)])))]
  "!BYTES_BIG_ENDIAN && VECTOR_MEM_VSX_P (V16QImode) && !TARGET_P9_VECTOR"
  "stxvd2x %x1,%y0"
  [(set_attr "type" "vecstore")])

;; Convert a TImode value into V1TImode
(define_expand "vsx_set_v1ti"
  [(match_operand:V1TI 0 "nonimmediate_operand" "")
   (match_operand:V1TI 1 "nonimmediate_operand" "")
   (match_operand:TI 2 "input_operand" "")
   (match_operand:QI 3 "u5bit_cint_operand" "")]
  "VECTOR_MEM_VSX_P (V1TImode)"
{
  if (operands[3] != const0_rtx)
    gcc_unreachable ();

  emit_move_insn (operands[0], gen_lowpart (V1TImode, operands[1]));
  DONE;
})

;; Set the element of a V2DI/VD2F mode
(define_insn "vsx_set_<mode>"
  [(set (match_operand:VSX_D 0 "vsx_register_operand" "=wd,?<VSa>")
	(unspec:VSX_D
	 [(match_operand:VSX_D 1 "vsx_register_operand" "wd,<VSa>")
	  (match_operand:<VS_scalar> 2 "vsx_register_operand" "<VS_64reg>,<VSa>")
	  (match_operand:QI 3 "u5bit_cint_operand" "i,i")]
	 UNSPEC_VSX_SET))]
  "VECTOR_MEM_VSX_P (<MODE>mode)"
{
  int idx_first = BYTES_BIG_ENDIAN ? 0 : 1;
  if (INTVAL (operands[3]) == idx_first)
    return \"xxpermdi %x0,%x2,%x1,1\";
  else if (INTVAL (operands[3]) == 1 - idx_first)
    return \"xxpermdi %x0,%x1,%x2,0\";
  else
    gcc_unreachable ();
}
  [(set_attr "type" "vecperm")])

;; Extract a DF/DI element from V2DF/V2DI
;; Optimize cases were we can do a simple or direct move.
;; Or see if we can avoid doing the move at all

;; There are some unresolved problems with reload that show up if an Altivec
;; register was picked.  Limit the scalar value to FPRs for now.

(define_insn "vsx_extract_<mode>"
  [(set (match_operand:<VS_scalar> 0 "gpc_reg_operand" "=d,    d,     wr, wr")

	(vec_select:<VS_scalar>
	 (match_operand:VSX_D 1 "gpc_reg_operand"      "<VSa>, <VSa>, wm, wo")

	 (parallel
	  [(match_operand:QI 2 "const_0_to_1_operand"  "wD,    n,     wD, n")])))]
  "VECTOR_MEM_VSX_P (<MODE>mode)"
{
  int element = INTVAL (operands[2]);
  int op0_regno = REGNO (operands[0]);
  int op1_regno = REGNO (operands[1]);
  int fldDM;

  gcc_assert (IN_RANGE (element, 0, 1));
  gcc_assert (VSX_REGNO_P (op1_regno));

  if (element == VECTOR_ELEMENT_SCALAR_64BIT)
    {
      if (op0_regno == op1_regno)
	return ASM_COMMENT_START " vec_extract to same register";

      else if (INT_REGNO_P (op0_regno) && TARGET_DIRECT_MOVE
	       && TARGET_POWERPC64)
	return "mfvsrd %0,%x1";

      else if (FP_REGNO_P (op0_regno) && FP_REGNO_P (op1_regno))
	return "fmr %0,%1";

      else if (VSX_REGNO_P (op0_regno))
	return "xxlor %x0,%x1,%x1";

      else
	gcc_unreachable ();
    }

  else if (element == VECTOR_ELEMENT_MFVSRLD_64BIT && INT_REGNO_P (op0_regno)
	   && TARGET_P9_VECTOR && TARGET_POWERPC64 && TARGET_DIRECT_MOVE)
    return "mfvsrld %0,%x1";

  else if (VSX_REGNO_P (op0_regno))
    {
      fldDM = element << 1;
      if (!BYTES_BIG_ENDIAN)
	fldDM = 3 - fldDM;
      operands[3] = GEN_INT (fldDM);
      return "xxpermdi %x0,%x1,%x1,%3";
    }

  else
    gcc_unreachable ();
}
  [(set_attr "type" "veclogical,mftgpr,mftgpr,vecperm")])

;; Optimize extracting a single scalar element from memory.
(define_insn_and_split "*vsx_extract_<P:mode>_<VSX_D:mode>_load"
  [(set (match_operand:<VS_scalar> 0 "register_operand" "=<VSX_D:VS_64reg>,wr")
	(vec_select:<VSX_D:VS_scalar>
	 (match_operand:VSX_D 1 "memory_operand" "m,m")
	 (parallel [(match_operand:QI 2 "const_0_to_1_operand" "n,n")])))
   (clobber (match_scratch:P 3 "=&b,&b"))]
  "VECTOR_MEM_VSX_P (<VSX_D:MODE>mode)"
  "#"
  "&& reload_completed"
  [(set (match_dup 0) (match_dup 4))]
{
  operands[4] = rs6000_adjust_vec_address (operands[0], operands[1], operands[2],
					   operands[3], <VSX_D:VS_scalar>mode);
}
  [(set_attr "type" "fpload,load")
   (set_attr "length" "8")])

;; Optimize storing a single scalar element that is the right location to
;; memory
(define_insn "*vsx_extract_<mode>_store"
  [(set (match_operand:<VS_scalar> 0 "memory_operand" "=m,Z,o")
	(vec_select:<VS_scalar>
	 (match_operand:VSX_D 1 "register_operand" "d,wv,wb")
	 (parallel [(match_operand:QI 2 "vsx_scalar_64bit" "wD,wD,wD")])))]
  "VECTOR_MEM_VSX_P (<MODE>mode)"
  "@
   stfd%U0%X0 %1,%0
   stxsd%U0x %x1,%y0
   stxsd %1,%0"
  [(set_attr "type" "fpstore")
   (set_attr "length" "4")])

;; Variable V2DI/V2DF extract shift
(define_insn "vsx_vslo_<mode>"
  [(set (match_operand:<VS_scalar> 0 "gpc_reg_operand" "=v")
	(unspec:<VS_scalar> [(match_operand:VSX_D 1 "gpc_reg_operand" "v")
			     (match_operand:V2DI 2 "gpc_reg_operand" "v")]
			    UNSPEC_VSX_VSLO))]
  "VECTOR_MEM_VSX_P (<MODE>mode) && TARGET_DIRECT_MOVE_64BIT"
  "vslo %0,%1,%2"
  [(set_attr "type" "vecperm")])

;; Variable V2DI/V2DF extract
(define_insn_and_split "vsx_extract_<mode>_var"
  [(set (match_operand:<VS_scalar> 0 "gpc_reg_operand" "=v,<VSa>,r")
	(unspec:<VS_scalar> [(match_operand:VSX_D 1 "input_operand" "v,m,m")
			     (match_operand:DI 2 "gpc_reg_operand" "r,r,r")]
			    UNSPEC_VSX_EXTRACT))
   (clobber (match_scratch:DI 3 "=r,&b,&b"))
   (clobber (match_scratch:V2DI 4 "=&v,X,X"))]
  "VECTOR_MEM_VSX_P (<MODE>mode) && TARGET_DIRECT_MOVE_64BIT"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  rs6000_split_vec_extract_var (operands[0], operands[1], operands[2],
				operands[3], operands[4]);
  DONE;
})

;; Extract a SF element from V4SF
(define_insn_and_split "vsx_extract_v4sf"
  [(set (match_operand:SF 0 "vsx_register_operand" "=ww")
	(vec_select:SF
	 (match_operand:V4SF 1 "vsx_register_operand" "wa")
	 (parallel [(match_operand:QI 2 "u5bit_cint_operand" "n")])))
   (clobber (match_scratch:V4SF 3 "=0"))]
  "VECTOR_UNIT_VSX_P (V4SFmode)"
  "#"
  "&& 1"
  [(const_int 0)]
{
  rtx op0 = operands[0];
  rtx op1 = operands[1];
  rtx op2 = operands[2];
  rtx op3 = operands[3];
  rtx tmp;
  HOST_WIDE_INT ele = BYTES_BIG_ENDIAN ? INTVAL (op2) : 3 - INTVAL (op2);

  if (ele == 0)
    tmp = op1;
  else
    {
      if (GET_CODE (op3) == SCRATCH)
	op3 = gen_reg_rtx (V4SFmode);
      emit_insn (gen_vsx_xxsldwi_v4sf (op3, op1, op1, GEN_INT (ele)));
      tmp = op3;
    }
  emit_insn (gen_vsx_xscvspdp_scalar2 (op0, tmp));
  DONE;
}
  [(set_attr "length" "8")
   (set_attr "type" "fp")])

(define_insn_and_split "*vsx_extract_v4sf_<mode>_load"
  [(set (match_operand:SF 0 "register_operand" "=f,wv,wb,?r")
	(vec_select:SF
	 (match_operand:V4SF 1 "memory_operand" "m,Z,m,m")
	 (parallel [(match_operand:QI 2 "const_0_to_3_operand" "n,n,n,n")])))
   (clobber (match_scratch:P 3 "=&b,&b,&b,&b"))]
  "VECTOR_MEM_VSX_P (V4SFmode)"
  "#"
  "&& reload_completed"
  [(set (match_dup 0) (match_dup 4))]
{
  operands[4] = rs6000_adjust_vec_address (operands[0], operands[1], operands[2],
					   operands[3], SFmode);
}
  [(set_attr "type" "fpload,fpload,fpload,load")
   (set_attr "length" "8")])

;; Variable V4SF extract
(define_insn_and_split "vsx_extract_v4sf_var"
  [(set (match_operand:SF 0 "gpc_reg_operand" "=ww,ww,?r")
	(unspec:SF [(match_operand:V4SF 1 "input_operand" "v,m,m")
		    (match_operand:DI 2 "gpc_reg_operand" "r,r,r")]
		   UNSPEC_VSX_EXTRACT))
   (clobber (match_scratch:DI 3 "=r,&b,&b"))
   (clobber (match_scratch:V2DI 4 "=&v,X,X"))]
  "VECTOR_MEM_VSX_P (V4SFmode) && TARGET_DIRECT_MOVE_64BIT
   && TARGET_UPPER_REGS_SF"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  rs6000_split_vec_extract_var (operands[0], operands[1], operands[2],
				operands[3], operands[4]);
  DONE;
})

;; Expand the builtin form of xxpermdi to canonical rtl.
(define_expand "vsx_xxpermdi_<mode>"
  [(match_operand:VSX_L 0 "vsx_register_operand" "")
   (match_operand:VSX_L 1 "vsx_register_operand" "")
   (match_operand:VSX_L 2 "vsx_register_operand" "")
   (match_operand:QI 3 "u5bit_cint_operand" "")]
  "VECTOR_MEM_VSX_P (<MODE>mode)"
{
  rtx target = operands[0];
  rtx op0 = operands[1];
  rtx op1 = operands[2];
  int mask = INTVAL (operands[3]);
  rtx perm0 = GEN_INT ((mask >> 1) & 1);
  rtx perm1 = GEN_INT ((mask & 1) + 2);
  rtx (*gen) (rtx, rtx, rtx, rtx, rtx);

  if (<MODE>mode == V2DFmode)
    gen = gen_vsx_xxpermdi2_v2df_1;
  else
    {
      gen = gen_vsx_xxpermdi2_v2di_1;
      if (<MODE>mode != V2DImode)
	{
	  target = gen_lowpart (V2DImode, target);
	  op0 = gen_lowpart (V2DImode, op0);
	  op1 = gen_lowpart (V2DImode, op1);
	}
    }
  /* In little endian mode, vsx_xxpermdi2_<mode>_1 will perform a
     transformation we don't want; it is necessary for
     rs6000_expand_vec_perm_const_1 but not for this use.  So we
     prepare for that by reversing the transformation here.  */
  if (BYTES_BIG_ENDIAN)
    emit_insn (gen (target, op0, op1, perm0, perm1));
  else
    {
      rtx p0 = GEN_INT (3 - INTVAL (perm1));
      rtx p1 = GEN_INT (3 - INTVAL (perm0));
      emit_insn (gen (target, op1, op0, p0, p1));
    }
  DONE;
})

(define_insn "vsx_xxpermdi2_<mode>_1"
  [(set (match_operand:VSX_D 0 "vsx_register_operand" "=wd")
	(vec_select:VSX_D
	  (vec_concat:<VS_double>
	    (match_operand:VSX_D 1 "vsx_register_operand" "wd")
	    (match_operand:VSX_D 2 "vsx_register_operand" "wd"))
	  (parallel [(match_operand 3 "const_0_to_1_operand" "")
		     (match_operand 4 "const_2_to_3_operand" "")])))]
  "VECTOR_MEM_VSX_P (<MODE>mode)"
{
  int op3, op4, mask;

  /* For little endian, swap operands and invert/swap selectors
     to get the correct xxpermdi.  The operand swap sets up the
     inputs as a little endian array.  The selectors are swapped
     because they are defined to use big endian ordering.  The
     selectors are inverted to get the correct doublewords for
     little endian ordering.  */
  if (BYTES_BIG_ENDIAN)
    {
      op3 = INTVAL (operands[3]);
      op4 = INTVAL (operands[4]);
    }
  else
    {
      op3 = 3 - INTVAL (operands[4]);
      op4 = 3 - INTVAL (operands[3]);
    }

  mask = (op3 << 1) | (op4 - 2);
  operands[3] = GEN_INT (mask);

  if (BYTES_BIG_ENDIAN)
    return "xxpermdi %x0,%x1,%x2,%3";
  else
    return "xxpermdi %x0,%x2,%x1,%3";
}
  [(set_attr "type" "vecperm")])

(define_expand "vec_perm_const<mode>"
  [(match_operand:VSX_D 0 "vsx_register_operand" "")
   (match_operand:VSX_D 1 "vsx_register_operand" "")
   (match_operand:VSX_D 2 "vsx_register_operand" "")
   (match_operand:V2DI  3 "" "")]
  "VECTOR_MEM_VSX_P (<MODE>mode)"
{
  if (rs6000_expand_vec_perm_const (operands))
    DONE;
  else
    FAIL;
})

;; Extraction of a single element in a small integer vector.  None of the small
;; types are currently allowed in a vector register, so we extract to a DImode
;; and either do a direct move or store.
(define_expand  "vsx_extract_<mode>"
  [(parallel [(set (match_operand:<VS_scalar> 0 "nonimmediate_operand")
		   (vec_select:<VS_scalar>
		    (match_operand:VSX_EXTRACT_I 1 "gpc_reg_operand")
		    (parallel [(match_operand:QI 2 "const_int_operand")])))
	      (clobber (match_dup 3))])]
  "VECTOR_MEM_VSX_P (<MODE>mode) && TARGET_DIRECT_MOVE_64BIT"
{
  operands[3] = gen_rtx_SCRATCH ((TARGET_VEXTRACTUB) ? DImode : <MODE>mode);
})

;; Under ISA 3.0, we can use the byte/half-word/word integer stores if we are
;; extracting a vector element and storing it to memory, rather than using
;; direct move to a GPR and a GPR store.
(define_insn_and_split  "*vsx_extract_<mode>_p9"
  [(set (match_operand:<VS_scalar> 0 "nonimmediate_operand" "=r,Z")
	(vec_select:<VS_scalar>
	 (match_operand:VSX_EXTRACT_I 1 "gpc_reg_operand" "<VSX_EX>,<VSX_EX>")
	 (parallel [(match_operand:QI 2 "<VSX_EXTRACT_PREDICATE>" "n,n")])))
   (clobber (match_scratch:DI 3 "=<VSX_EX>,<VSX_EX>"))]
  "VECTOR_MEM_VSX_P (<MODE>mode) && TARGET_VEXTRACTUB"
  "#"
  "&& (reload_completed || MEM_P (operands[0]))"
  [(const_int 0)]
{
  rtx dest = operands[0];
  rtx src = operands[1];
  rtx element = operands[2];
  rtx di_tmp = operands[3];

  if (GET_CODE (di_tmp) == SCRATCH)
    di_tmp = gen_reg_rtx (DImode);

  emit_insn (gen_vsx_extract_<mode>_di (di_tmp, src, element));

  if (REG_P (dest))
    emit_move_insn (gen_rtx_REG (DImode, REGNO (dest)), di_tmp);
  else if (SUBREG_P (dest))
    emit_move_insn (gen_rtx_REG (DImode, subreg_regno (dest)), di_tmp);
  else if (MEM_P (operands[0]))
    {
      if (can_create_pseudo_p ())
	dest = rs6000_address_for_fpconvert (dest);

      if (<MODE>mode == V16QImode)
	emit_insn (gen_p9_stxsibx (dest, di_tmp));
      else if (<MODE>mode == V8HImode)
	emit_insn (gen_p9_stxsihx (dest, di_tmp));
      else if (<MODE>mode == V4SImode)
	emit_insn (gen_stfiwx (dest, di_tmp));
      else
	gcc_unreachable ();
    }
  else
    gcc_unreachable ();

  DONE;
}
  [(set_attr "type" "vecsimple,fpstore")])

(define_insn  "vsx_extract_<mode>_di"
  [(set (match_operand:DI 0 "gpc_reg_operand" "=<VSX_EX>")
	(zero_extend:DI
	 (vec_select:<VS_scalar>
	  (match_operand:VSX_EXTRACT_I 1 "gpc_reg_operand" "<VSX_EX>")
	  (parallel [(match_operand:QI 2 "<VSX_EXTRACT_PREDICATE>" "n")]))))]
  "VECTOR_MEM_VSX_P (<MODE>mode) && TARGET_VEXTRACTUB"
{
  int element = INTVAL (operands[2]);
  int unit_size = GET_MODE_UNIT_SIZE (<MODE>mode);
  int offset = ((VECTOR_ELT_ORDER_BIG)
		? unit_size * element
		: unit_size * (GET_MODE_NUNITS (<MODE>mode) - 1 - element));

  operands[2] = GEN_INT (offset);
  if (unit_size == 4)
    return "xxextractuw %x0,%x1,%2";
  else
    return "vextractu<wd> %0,%1,%2";
}
  [(set_attr "type" "vecsimple")])

(define_insn_and_split  "*vsx_extract_<mode>_p8"
  [(set (match_operand:<VS_scalar> 0 "nonimmediate_operand" "=r")
	(vec_select:<VS_scalar>
	 (match_operand:VSX_EXTRACT_I 1 "gpc_reg_operand" "v")
	 (parallel [(match_operand:QI 2 "<VSX_EXTRACT_PREDICATE>" "n")])))
   (clobber (match_scratch:VSX_EXTRACT_I 3 "=v"))]
  "VECTOR_MEM_VSX_P (<MODE>mode) && TARGET_DIRECT_MOVE_64BIT"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  rtx dest = operands[0];
  rtx src = operands[1];
  rtx element = operands[2];
  rtx vec_tmp = operands[3];
  int value;

  if (!VECTOR_ELT_ORDER_BIG)
    element = GEN_INT (GET_MODE_NUNITS (<MODE>mode) - 1 - INTVAL (element));

  /* If the value is in the correct position, we can avoid doing the VSPLT<x>
     instruction.  */
  value = INTVAL (element);
  if (<MODE>mode == V16QImode)
    {
      if (value != 7)
	emit_insn (gen_altivec_vspltb_direct (vec_tmp, src, element));
      else
	vec_tmp = src;
    }
  else if (<MODE>mode == V8HImode)
    {
      if (value != 3)
	emit_insn (gen_altivec_vsplth_direct (vec_tmp, src, element));
      else
	vec_tmp = src;
    }
  else if (<MODE>mode == V4SImode)
    {
      if (value != 1)
	emit_insn (gen_altivec_vspltw_direct (vec_tmp, src, element));
      else
	vec_tmp = src;
    }
  else
    gcc_unreachable ();

  emit_move_insn (gen_rtx_REG (DImode, REGNO (dest)),
		  gen_rtx_REG (DImode, REGNO (vec_tmp)));
  DONE;
}
  [(set_attr "type" "mftgpr")])

;; Optimize extracting a single scalar element from memory.
(define_insn_and_split "*vsx_extract_<mode>_load"
  [(set (match_operand:<VS_scalar> 0 "register_operand" "=r")
	(vec_select:<VS_scalar>
	 (match_operand:VSX_EXTRACT_I 1 "memory_operand" "m")
	 (parallel [(match_operand:QI 2 "<VSX_EXTRACT_PREDICATE>" "n")])))
   (clobber (match_scratch:DI 3 "=&b"))]
  "VECTOR_MEM_VSX_P (<MODE>mode) && TARGET_DIRECT_MOVE_64BIT"
  "#"
  "&& reload_completed"
  [(set (match_dup 0) (match_dup 4))]
{
  operands[4] = rs6000_adjust_vec_address (operands[0], operands[1], operands[2],
					   operands[3], <VS_scalar>mode);
}
  [(set_attr "type" "load")
   (set_attr "length" "8")])

;; Variable V16QI/V8HI/V4SI extract
(define_insn_and_split "vsx_extract_<mode>_var"
  [(set (match_operand:<VS_scalar> 0 "gpc_reg_operand" "=r,r")
	(unspec:<VS_scalar>
	 [(match_operand:VSX_EXTRACT_I 1 "input_operand" "v,m")
	  (match_operand:DI 2 "gpc_reg_operand" "r,r")]
	 UNSPEC_VSX_EXTRACT))
   (clobber (match_scratch:DI 3 "=r,&b"))
   (clobber (match_scratch:V2DI 4 "=&v,X"))]
  "VECTOR_MEM_VSX_P (<MODE>mode) && TARGET_DIRECT_MOVE_64BIT"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  rs6000_split_vec_extract_var (operands[0], operands[1], operands[2],
				operands[3], operands[4]);
  DONE;
})

;; Expanders for builtins
(define_expand "vsx_mergel_<mode>"
  [(use (match_operand:VSX_D 0 "vsx_register_operand" ""))
   (use (match_operand:VSX_D 1 "vsx_register_operand" ""))
   (use (match_operand:VSX_D 2 "vsx_register_operand" ""))]
  "VECTOR_MEM_VSX_P (<MODE>mode)"
{
  rtvec v;
  rtx x;

  /* Special handling for LE with -maltivec=be.  */
  if (!BYTES_BIG_ENDIAN && VECTOR_ELT_ORDER_BIG)
    {
      v = gen_rtvec (2, GEN_INT (0), GEN_INT (2));
      x = gen_rtx_VEC_CONCAT (<VS_double>mode, operands[2], operands[1]);
    }
  else
    {
      v = gen_rtvec (2, GEN_INT (1), GEN_INT (3));
      x = gen_rtx_VEC_CONCAT (<VS_double>mode, operands[1], operands[2]);
    }

  x = gen_rtx_VEC_SELECT (<MODE>mode, x, gen_rtx_PARALLEL (VOIDmode, v));
  emit_insn (gen_rtx_SET (operands[0], x));
  DONE;
})

(define_expand "vsx_mergeh_<mode>"
  [(use (match_operand:VSX_D 0 "vsx_register_operand" ""))
   (use (match_operand:VSX_D 1 "vsx_register_operand" ""))
   (use (match_operand:VSX_D 2 "vsx_register_operand" ""))]
  "VECTOR_MEM_VSX_P (<MODE>mode)"
{
  rtvec v;
  rtx x;

  /* Special handling for LE with -maltivec=be.  */
  if (!BYTES_BIG_ENDIAN && VECTOR_ELT_ORDER_BIG)
    {
      v = gen_rtvec (2, GEN_INT (1), GEN_INT (3));
      x = gen_rtx_VEC_CONCAT (<VS_double>mode, operands[2], operands[1]);
    }
  else
    {
      v = gen_rtvec (2, GEN_INT (0), GEN_INT (2));
      x = gen_rtx_VEC_CONCAT (<VS_double>mode, operands[1], operands[2]);
    }

  x = gen_rtx_VEC_SELECT (<MODE>mode, x, gen_rtx_PARALLEL (VOIDmode, v));
  emit_insn (gen_rtx_SET (operands[0], x));
  DONE;
})

;; V2DF/V2DI splat
(define_insn "vsx_splat_<mode>"
  [(set (match_operand:VSX_D 0 "vsx_register_operand" "=<VSa>,<VSa>,we")
	(vec_duplicate:VSX_D
	 (match_operand:<VS_scalar> 1 "splat_input_operand" "<VS_64reg>,Z,b")))]
  "VECTOR_MEM_VSX_P (<MODE>mode)"
  "@
   xxpermdi %x0,%x1,%x1,0
   lxvdsx %x0,%y1
   mtvsrdd %x0,%1,%1"
  [(set_attr "type" "vecperm,vecload,vecperm")])

;; V4SI splat support
(define_insn "vsx_splat_v4si"
  [(set (match_operand:V4SI 0 "vsx_register_operand" "=we,we")
	(vec_duplicate:V4SI
	 (match_operand:SI 1 "splat_input_operand" "r,Z")))]
  "TARGET_P9_VECTOR"
  "@
   mtvsrws %x0,%1
   lxvwsx %x0,%y1"
  [(set_attr "type" "vecperm,vecload")])

;; SImode is not currently allowed in vector registers.  This pattern
;; allows us to use direct move to get the value in a vector register
;; so that we can use XXSPLTW
(define_insn "vsx_splat_v4si_di"
  [(set (match_operand:V4SI 0 "vsx_register_operand" "=wa,we")
	(vec_duplicate:V4SI
	 (truncate:SI
	  (match_operand:DI 1 "gpc_reg_operand" "wj,r"))))]
  "VECTOR_MEM_VSX_P (V4SImode) && TARGET_DIRECT_MOVE_64BIT"
  "@
   xxspltw %x0,%x1,1
   mtvsrws %x0,%1"
  [(set_attr "type" "vecperm")])

;; V4SF splat (ISA 3.0)
(define_insn_and_split "vsx_splat_v4sf"
  [(set (match_operand:V4SF 0 "vsx_register_operand" "=wa,wa,wa")
	(vec_duplicate:V4SF
	 (match_operand:SF 1 "splat_input_operand" "Z,wy,r")))]
  "TARGET_P9_VECTOR"
  "@
   lxvwsx %x0,%y1
   #
   mtvsrws %x0,%1"
  "&& reload_completed && vsx_register_operand (operands[1], SFmode)"
  [(set (match_dup 0)
	(unspec:V4SF [(match_dup 1)] UNSPEC_VSX_CVDPSPN))
   (set (match_dup 0)
	(unspec:V4SF [(match_dup 0)
		      (const_int 0)] UNSPEC_VSX_XXSPLTW))]
  ""
  [(set_attr "type" "vecload,vecperm,mftgpr")
   (set_attr "length" "4,8,4")])

;; V4SF/V4SI splat from a vector element
(define_insn "vsx_xxspltw_<mode>"
  [(set (match_operand:VSX_W 0 "vsx_register_operand" "=<VSa>")
	(vec_duplicate:VSX_W
	 (vec_select:<VS_scalar>
	  (match_operand:VSX_W 1 "vsx_register_operand" "<VSa>")
	  (parallel
	   [(match_operand:QI 2 "u5bit_cint_operand" "n")]))))]
  "VECTOR_MEM_VSX_P (<MODE>mode)"
{
  if (!BYTES_BIG_ENDIAN)
    operands[2] = GEN_INT (3 - INTVAL (operands[2]));

  return "xxspltw %x0,%x1,%2";
}
  [(set_attr "type" "vecperm")])

(define_insn "vsx_xxspltw_<mode>_direct"
  [(set (match_operand:VSX_W 0 "vsx_register_operand" "=<VSa>")
        (unspec:VSX_W [(match_operand:VSX_W 1 "vsx_register_operand" "<VSa>")
                       (match_operand:QI 2 "u5bit_cint_operand" "i")]
                      UNSPEC_VSX_XXSPLTW))]
  "VECTOR_MEM_VSX_P (<MODE>mode)"
  "xxspltw %x0,%x1,%2"
  [(set_attr "type" "vecperm")])

;; V16QI/V8HI splat support on ISA 2.07
(define_insn "vsx_vsplt<VSX_SPLAT_SUFFIX>_di"
  [(set (match_operand:VSX_SPLAT_I 0 "altivec_register_operand" "=v")
	(vec_duplicate:VSX_SPLAT_I
	 (truncate:<VS_scalar>
	  (match_operand:DI 1 "altivec_register_operand" "v"))))]
  "VECTOR_MEM_VSX_P (<MODE>mode) && TARGET_DIRECT_MOVE_64BIT"
  "vsplt<VSX_SPLAT_SUFFIX> %0,%1,<VSX_SPLAT_COUNT>"
  [(set_attr "type" "vecperm")])

;; V2DF/V2DI splat for use by vec_splat builtin
(define_insn "vsx_xxspltd_<mode>"
  [(set (match_operand:VSX_D 0 "vsx_register_operand" "=wa")
        (unspec:VSX_D [(match_operand:VSX_D 1 "vsx_register_operand" "wa")
	               (match_operand:QI 2 "u5bit_cint_operand" "i")]
                      UNSPEC_VSX_XXSPLTD))]
  "VECTOR_MEM_VSX_P (<MODE>mode)"
{
  if ((VECTOR_ELT_ORDER_BIG && INTVAL (operands[2]) == 0)
      || (!VECTOR_ELT_ORDER_BIG && INTVAL (operands[2]) == 1))
    return "xxpermdi %x0,%x1,%x1,0";
  else
    return "xxpermdi %x0,%x1,%x1,3";
}
  [(set_attr "type" "vecperm")])

;; V4SF/V4SI interleave
(define_insn "vsx_xxmrghw_<mode>"
  [(set (match_operand:VSX_W 0 "vsx_register_operand" "=wf,?<VSa>")
        (vec_select:VSX_W
	  (vec_concat:<VS_double>
	    (match_operand:VSX_W 1 "vsx_register_operand" "wf,<VSa>")
	    (match_operand:VSX_W 2 "vsx_register_operand" "wf,<VSa>"))
	  (parallel [(const_int 0) (const_int 4)
		     (const_int 1) (const_int 5)])))]
  "VECTOR_MEM_VSX_P (<MODE>mode)"
{
  if (BYTES_BIG_ENDIAN)
    return "xxmrghw %x0,%x1,%x2";
  else
    return "xxmrglw %x0,%x2,%x1";
}
  [(set_attr "type" "vecperm")])

(define_insn "vsx_xxmrglw_<mode>"
  [(set (match_operand:VSX_W 0 "vsx_register_operand" "=wf,?<VSa>")
	(vec_select:VSX_W
	  (vec_concat:<VS_double>
	    (match_operand:VSX_W 1 "vsx_register_operand" "wf,<VSa>")
	    (match_operand:VSX_W 2 "vsx_register_operand" "wf,?<VSa>"))
	  (parallel [(const_int 2) (const_int 6)
		     (const_int 3) (const_int 7)])))]
  "VECTOR_MEM_VSX_P (<MODE>mode)"
{
  if (BYTES_BIG_ENDIAN)
    return "xxmrglw %x0,%x1,%x2";
  else
    return "xxmrghw %x0,%x2,%x1";
}
  [(set_attr "type" "vecperm")])

;; Shift left double by word immediate
(define_insn "vsx_xxsldwi_<mode>"
  [(set (match_operand:VSX_L 0 "vsx_register_operand" "=<VSa>")
	(unspec:VSX_L [(match_operand:VSX_L 1 "vsx_register_operand" "<VSa>")
		       (match_operand:VSX_L 2 "vsx_register_operand" "<VSa>")
		       (match_operand:QI 3 "u5bit_cint_operand" "i")]
		      UNSPEC_VSX_SLDWI))]
  "VECTOR_MEM_VSX_P (<MODE>mode)"
  "xxsldwi %x0,%x1,%x2,%3"
  [(set_attr "type" "vecperm")])


;; Vector reduction insns and splitters

(define_insn_and_split "vsx_reduc_<VEC_reduc_name>_v2df"
  [(set (match_operand:V2DF 0 "vfloat_operand" "=&wd,&?wa,wd,?wa")
	(VEC_reduc:V2DF
	 (vec_concat:V2DF
	  (vec_select:DF
	   (match_operand:V2DF 1 "vfloat_operand" "wd,wa,wd,wa")
	   (parallel [(const_int 1)]))
	  (vec_select:DF
	   (match_dup 1)
	   (parallel [(const_int 0)])))
	 (match_dup 1)))
   (clobber (match_scratch:V2DF 2 "=0,0,&wd,&wa"))]
  "VECTOR_UNIT_VSX_P (V2DFmode)"
  "#"
  ""
  [(const_int 0)]
  "
{
  rtx tmp = (GET_CODE (operands[2]) == SCRATCH)
	     ? gen_reg_rtx (V2DFmode)
	     : operands[2];
  emit_insn (gen_vsx_xxsldwi_v2df (tmp, operands[1], operands[1], const2_rtx));
  emit_insn (gen_<VEC_reduc_rtx>v2df3 (operands[0], tmp, operands[1]));
  DONE;
}"
  [(set_attr "length" "8")
   (set_attr "type" "veccomplex")])

(define_insn_and_split "vsx_reduc_<VEC_reduc_name>_v4sf"
  [(set (match_operand:V4SF 0 "vfloat_operand" "=wf,?wa")
	(VEC_reduc:V4SF
	 (unspec:V4SF [(const_int 0)] UNSPEC_REDUC)
	 (match_operand:V4SF 1 "vfloat_operand" "wf,wa")))
   (clobber (match_scratch:V4SF 2 "=&wf,&wa"))
   (clobber (match_scratch:V4SF 3 "=&wf,&wa"))]
  "VECTOR_UNIT_VSX_P (V4SFmode)"
  "#"
  ""
  [(const_int 0)]
  "
{
  rtx op0 = operands[0];
  rtx op1 = operands[1];
  rtx tmp2, tmp3, tmp4;

  if (can_create_pseudo_p ())
    {
      tmp2 = gen_reg_rtx (V4SFmode);
      tmp3 = gen_reg_rtx (V4SFmode);
      tmp4 = gen_reg_rtx (V4SFmode);
    }
  else
    {
      tmp2 = operands[2];
      tmp3 = operands[3];
      tmp4 = tmp2;
    }

  emit_insn (gen_vsx_xxsldwi_v4sf (tmp2, op1, op1, const2_rtx));
  emit_insn (gen_<VEC_reduc_rtx>v4sf3 (tmp3, tmp2, op1));
  emit_insn (gen_vsx_xxsldwi_v4sf (tmp4, tmp3, tmp3, GEN_INT (3)));
  emit_insn (gen_<VEC_reduc_rtx>v4sf3 (op0, tmp4, tmp3));
  DONE;
}"
  [(set_attr "length" "16")
   (set_attr "type" "veccomplex")])

;; Combiner patterns with the vector reduction patterns that knows we can get
;; to the top element of the V2DF array without doing an extract.

(define_insn_and_split "*vsx_reduc_<VEC_reduc_name>_v2df_scalar"
  [(set (match_operand:DF 0 "vfloat_operand" "=&ws,&?ws,ws,?ws")
	(vec_select:DF
	 (VEC_reduc:V2DF
	  (vec_concat:V2DF
	   (vec_select:DF
	    (match_operand:V2DF 1 "vfloat_operand" "wd,wa,wd,wa")
	    (parallel [(const_int 1)]))
	   (vec_select:DF
	    (match_dup 1)
	    (parallel [(const_int 0)])))
	  (match_dup 1))
	 (parallel [(const_int 1)])))
   (clobber (match_scratch:DF 2 "=0,0,&wd,&wa"))]
  "VECTOR_UNIT_VSX_P (V2DFmode)"
  "#"
  ""
  [(const_int 0)]
  "
{
  rtx hi = gen_highpart (DFmode, operands[1]);
  rtx lo = (GET_CODE (operands[2]) == SCRATCH)
	    ? gen_reg_rtx (DFmode)
	    : operands[2];

  emit_insn (gen_vsx_extract_v2df (lo, operands[1], const1_rtx));
  emit_insn (gen_<VEC_reduc_rtx>df3 (operands[0], hi, lo));
  DONE;
}"
  [(set_attr "length" "8")
   (set_attr "type" "veccomplex")])

(define_insn_and_split "*vsx_reduc_<VEC_reduc_name>_v4sf_scalar"
  [(set (match_operand:SF 0 "vfloat_operand" "=f,?f")
	(vec_select:SF
	 (VEC_reduc:V4SF
	  (unspec:V4SF [(const_int 0)] UNSPEC_REDUC)
	  (match_operand:V4SF 1 "vfloat_operand" "wf,wa"))
	 (parallel [(const_int 3)])))
   (clobber (match_scratch:V4SF 2 "=&wf,&wa"))
   (clobber (match_scratch:V4SF 3 "=&wf,&wa"))
   (clobber (match_scratch:V4SF 4 "=0,0"))]
  "VECTOR_UNIT_VSX_P (V4SFmode)"
  "#"
  ""
  [(const_int 0)]
  "
{
  rtx op0 = operands[0];
  rtx op1 = operands[1];
  rtx tmp2, tmp3, tmp4, tmp5;

  if (can_create_pseudo_p ())
    {
      tmp2 = gen_reg_rtx (V4SFmode);
      tmp3 = gen_reg_rtx (V4SFmode);
      tmp4 = gen_reg_rtx (V4SFmode);
      tmp5 = gen_reg_rtx (V4SFmode);
    }
  else
    {
      tmp2 = operands[2];
      tmp3 = operands[3];
      tmp4 = tmp2;
      tmp5 = operands[4];
    }

  emit_insn (gen_vsx_xxsldwi_v4sf (tmp2, op1, op1, const2_rtx));
  emit_insn (gen_<VEC_reduc_rtx>v4sf3 (tmp3, tmp2, op1));
  emit_insn (gen_vsx_xxsldwi_v4sf (tmp4, tmp3, tmp3, GEN_INT (3)));
  emit_insn (gen_<VEC_reduc_rtx>v4sf3 (tmp5, tmp4, tmp3));
  emit_insn (gen_vsx_xscvspdp_scalar2 (op0, tmp5));
  DONE;
}"
  [(set_attr "length" "20")
   (set_attr "type" "veccomplex")])


;; Power8 Vector fusion.  The fused ops must be physically adjacent.
(define_peephole
  [(set (match_operand:P 0 "base_reg_operand" "")
	(match_operand:P 1 "short_cint_operand" ""))
   (set (match_operand:VSX_M 2 "vsx_register_operand" "")
	(mem:VSX_M (plus:P (match_dup 0)
			   (match_operand:P 3 "int_reg_operand" ""))))]
  "TARGET_VSX && TARGET_P8_FUSION && !TARGET_P9_VECTOR"
  "li %0,%1\t\t\t# vector load fusion\;lx<VSX_M:VSm>x %x2,%0,%3"  
  [(set_attr "length" "8")
   (set_attr "type" "vecload")])

(define_peephole
  [(set (match_operand:P 0 "base_reg_operand" "")
	(match_operand:P 1 "short_cint_operand" ""))
   (set (match_operand:VSX_M 2 "vsx_register_operand" "")
	(mem:VSX_M (plus:P (match_operand:P 3 "int_reg_operand" "")
			   (match_dup 0))))]
  "TARGET_VSX && TARGET_P8_FUSION && !TARGET_P9_VECTOR"
  "li %0,%1\t\t\t# vector load fusion\;lx<VSX_M:VSm>x %x2,%0,%3"  
  [(set_attr "length" "8")
   (set_attr "type" "vecload")])


;; ISA 3.0 vector extend sign support

(define_insn "vsx_sign_extend_qi_<mode>"
  [(set (match_operand:VSINT_84 0 "vsx_register_operand" "=v")
	(unspec:VSINT_84
	 [(match_operand:V16QI 1 "vsx_register_operand" "v")]
	 UNSPEC_VSX_SIGN_EXTEND))]
  "TARGET_P9_VECTOR"
  "vextsb2<wd> %0,%1"
  [(set_attr "type" "vecexts")])

(define_insn "vsx_sign_extend_hi_<mode>"
  [(set (match_operand:VSINT_84 0 "vsx_register_operand" "=v")
	(unspec:VSINT_84
	 [(match_operand:V8HI 1 "vsx_register_operand" "v")]
	 UNSPEC_VSX_SIGN_EXTEND))]
  "TARGET_P9_VECTOR"
  "vextsh2<wd> %0,%1"
  [(set_attr "type" "vecexts")])

(define_insn "*vsx_sign_extend_si_v2di"
  [(set (match_operand:V2DI 0 "vsx_register_operand" "=v")
	(unspec:V2DI [(match_operand:V4SI 1 "vsx_register_operand" "v")]
		     UNSPEC_VSX_SIGN_EXTEND))]
  "TARGET_P9_VECTOR"
  "vextsw2d %0,%1"
  [(set_attr "type" "vecexts")])


;; ISA 3.0 memory operations
(define_insn "p9_lxsi<wd>zx"
  [(set (match_operand:DI 0 "vsx_register_operand" "=wi")
	(unspec:DI [(zero_extend:DI
		     (match_operand:QHI 1 "indexed_or_indirect_operand" "Z"))]
		   UNSPEC_P9_MEMORY))]
  "TARGET_P9_VECTOR"
  "lxsi<wd>zx %x0,%y1"
  [(set_attr "type" "fpload")])

(define_insn "p9_stxsi<wd>x"
  [(set (match_operand:QHI 0 "reg_or_indexed_operand" "=r,Z")
	(unspec:QHI [(match_operand:DI 1 "vsx_register_operand" "wi,wi")]
		    UNSPEC_P9_MEMORY))]
  "TARGET_P9_VECTOR"
  "@
   mfvsrd %0,%x1
   stxsi<wd>x %x1,%y0"
  [(set_attr "type" "mffgpr,fpstore")])

;; ISA 3.0 Binary Floating-Point Support

;; VSX Scalar Extract Exponent Double-Precision
(define_insn "xsxexpdp"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(unspec:DI [(match_operand:DF 1 "vsx_register_operand" "wa")]
	 UNSPEC_VSX_SXEXPDP))]
  "TARGET_P9_VECTOR && TARGET_64BIT"
  "xsxexpdp %0,%x1"
  [(set_attr "type" "integer")])

;; VSX Scalar Extract Significand Double-Precision
(define_insn "xsxsigdp"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(unspec:DI [(match_operand:DF 1 "vsx_register_operand" "wa")]
	 UNSPEC_VSX_SXSIGDP))]
  "TARGET_P9_VECTOR && TARGET_64BIT"
  "xsxsigdp %0,%x1"
  [(set_attr "type" "integer")])

;; VSX Scalar Insert Exponent Double-Precision
(define_insn "xsiexpdp"
  [(set (match_operand:DF 0 "vsx_register_operand" "=wa")
	(unspec:DF [(match_operand:DI 1 "register_operand" "r")
		    (match_operand:DI 2 "register_operand" "r")]
	 UNSPEC_VSX_SIEXPDP))]
  "TARGET_P9_VECTOR && TARGET_64BIT"
  "xsiexpdp %x0,%1,%2"
  [(set_attr "type" "fpsimple")])

;; VSX Scalar Compare Exponents Double-Precision
(define_expand "xscmpexpdp_<code>"
  [(set (match_dup 3)
	(compare:CCFP
	 (unspec:DF
	  [(match_operand:DF 1 "vsx_register_operand" "wa")
	   (match_operand:DF 2 "vsx_register_operand" "wa")]
	  UNSPEC_VSX_SCMPEXPDP)
	 (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(CMP_TEST:SI (match_dup 3)
		     (const_int 0)))]
  "TARGET_P9_VECTOR"
{
  operands[3] = gen_reg_rtx (CCFPmode);
})

(define_insn "*xscmpexpdp"
  [(set (match_operand:CCFP 0 "cc_reg_operand" "=y")
	(compare:CCFP
	 (unspec:DF [(match_operand:DF 1 "vsx_register_operand" "wa")
		     (match_operand:DF 2 "vsx_register_operand" "wa")]
	  UNSPEC_VSX_SCMPEXPDP)
	 (match_operand:SI 3 "zero_constant" "j")))]
  "TARGET_P9_VECTOR"
  "xscmpexpdp %0,%x1,%x2"
  [(set_attr "type" "fpcompare")])

;; VSX Scalar Test Data Class Double- and Single-Precision
;;  (The lt bit is set if operand 1 is negative.  The eq bit is set
;;   if any of the conditions tested by operand 2 are satisfied.
;;   The gt and unordered bits are cleared to zero.)
(define_expand "xststdc<Fvsx>"
  [(set (match_dup 3)
	(compare:CCFP
	 (unspec:SFDF
	  [(match_operand:SFDF 1 "vsx_register_operand" "wa")
	   (match_operand:SI 2 "u7bit_cint_operand" "n")]
	  UNSPEC_VSX_STSTDC)
	 (match_dup 4)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(eq:SI (match_dup 3)
	       (const_int 0)))]
  "TARGET_P9_VECTOR"
{
  operands[3] = gen_reg_rtx (CCFPmode);
  operands[4] = CONST0_RTX (SImode);
})

;; The VSX Scalar Test Data Class Double- and Single-Precision
;; instruction may also be used to test for negative value.
(define_expand "xststdcneg<Fvsx>"
  [(set (match_dup 2)
	(compare:CCFP
	 (unspec:SFDF
	  [(match_operand:SFDF 1 "vsx_register_operand" "wa")
	   (const_int 0)]
	  UNSPEC_VSX_STSTDC)
	 (match_dup 3)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(lt:SI (match_dup 2)
	       (const_int 0)))]
  "TARGET_P9_VECTOR"
{
  operands[2] = gen_reg_rtx (CCFPmode);
  operands[3] = CONST0_RTX (SImode);
})

(define_insn "*xststdc<Fvsx>"
  [(set (match_operand:CCFP 0 "" "=y")
	(compare:CCFP
	 (unspec:SFDF [(match_operand:SFDF 1 "vsx_register_operand" "wa")
		       (match_operand:SI 2 "u7bit_cint_operand" "n")]
	  UNSPEC_VSX_STSTDC)
	 (match_operand:SI 3 "zero_constant" "j")))]
  "TARGET_P9_VECTOR"
  "xststdc<Fvsx> %0,%x1,%2"
  [(set_attr "type" "fpcompare")])

;; VSX Vector Extract Exponent Double and Single Precision
(define_insn "xvxexp<VSs>"
  [(set (match_operand:VSX_F 0 "vsx_register_operand" "=wa")
	(unspec:VSX_F
	 [(match_operand:VSX_F 1 "vsx_register_operand" "wa")]
	 UNSPEC_VSX_VXEXP))]
  "TARGET_P9_VECTOR"
  "xvxexp<VSs> %x0,%x1"
  [(set_attr "type" "vecsimple")])

;; VSX Vector Extract Significand Double and Single Precision
(define_insn "xvxsig<VSs>"
  [(set (match_operand:VSX_F 0 "vsx_register_operand" "=wa")
	(unspec:VSX_F
	 [(match_operand:VSX_F 1 "vsx_register_operand" "wa")]
	 UNSPEC_VSX_VXSIG))]
  "TARGET_P9_VECTOR"
  "xvxsig<VSs> %x0,%x1"
  [(set_attr "type" "vecsimple")])

;; VSX Vector Insert Exponent Double and Single Precision
(define_insn "xviexp<VSs>"
  [(set (match_operand:VSX_F 0 "vsx_register_operand" "=wa")
	(unspec:VSX_F
	 [(match_operand:VSX_F 1 "vsx_register_operand" "wa")
	  (match_operand:VSX_F 2 "vsx_register_operand" "wa")]
	 UNSPEC_VSX_VIEXP))]
  "TARGET_P9_VECTOR"
  "xviexp<VSs> %x0,%x1,%x2"
  [(set_attr "type" "vecsimple")])

;; VSX Vector Test Data Class Double and Single Precision
;; The corresponding elements of the result vector are all ones
;; if any of the conditions tested by operand 3 are satisfied.
(define_insn "xvtstdc<VSs>"
  [(set (match_operand:<VSI> 0 "vsx_register_operand" "=wa")
	(unspec:<VSI>
	 [(match_operand:VSX_F 1 "vsx_register_operand" "wa")
	  (match_operand:SI 2 "u7bit_cint_operand" "n")]
	 UNSPEC_VSX_VTSTDC))]
  "TARGET_P9_VECTOR"
  "xvtstdc<VSs> %x0,%x1,%2"
  [(set_attr "type" "vecsimple")])

;; ISA 3.0 String Operations Support

;; Compare vectors producing a vector result and a predicate, setting CR6
;; to indicate a combined status.  This pattern matches v16qi, v8hi, and
;; v4si modes.  It does not match v2df, v4sf, or v2di modes.  There's no
;; need to match the v2di mode because that is expanded into v4si.
(define_insn "*vsx_ne_<mode>_p"
  [(set (reg:CC CR6_REGNO)
	(unspec:CC
	 [(ne:CC (match_operand:VSX_EXTRACT_I 1 "gpc_reg_operand" "v")
		 (match_operand:VSX_EXTRACT_I 2 "gpc_reg_operand" "v"))]
	 UNSPEC_PREDICATE))
   (set (match_operand:VSX_EXTRACT_I 0 "gpc_reg_operand" "=v")
	(ne:VSX_EXTRACT_I (match_dup 1)
			  (match_dup 2)))]
  "TARGET_P9_VECTOR"
  "xvcmpne<VSX_EXTRACT_WIDTH>. %0,%1,%2"
  [(set_attr "type" "vecsimple")])

;; Compare vectors producing a vector result and a predicate, setting CR6
;; to indicate a combined status, for v4sf and v2df operands.
(define_insn "*vsx_ne_<mode>_p"
  [(set (reg:CC CR6_REGNO)
	(unspec:CC [(ne:CC
		     (match_operand:VSX_F 1 "vsx_register_operand" "wa")
		     (match_operand:VSX_F 2 "vsx_register_operand" "wa"))]
	 UNSPEC_PREDICATE))
   (set (match_operand:VSX_F 0 "vsx_register_operand" "=wa")
	(ne:VSX_F (match_dup 1)
		  (match_dup 2)))]
  "TARGET_P9_VECTOR"
  "xvcmpne<VSs>. %x0,%x1,%x2"
  [(set_attr "type" "vecsimple")])

(define_insn "*vector_nez_<mode>_p"
  [(set (reg:CC CR6_REGNO)
	(unspec:CC [(unspec:VI
		     [(match_operand:VI 1 "gpc_reg_operand" "v")
		      (match_operand:VI 2 "gpc_reg_operand" "v")]
		     UNSPEC_NEZ_P)]
	 UNSPEC_PREDICATE))
   (set (match_operand:VI 0 "gpc_reg_operand" "=v")
	(unspec:VI [(match_dup 1)
		    (match_dup 2)]
	 UNSPEC_NEZ_P))]
  "TARGET_P9_VECTOR"
  "vcmpnez<VSX_EXTRACT_WIDTH>. %0,%1,%2"
  [(set_attr "type" "vecsimple")])

;; Load VSX Vector with Length
(define_expand "lxvl"
  [(set (match_dup 3)
        (match_operand:DI 2 "register_operand"))
   (set (match_operand:V16QI 0 "vsx_register_operand")
	(unspec:V16QI
	 [(match_operand:DI 1 "gpc_reg_operand")
	  (match_dup 3)]
	 UNSPEC_LXVL))]
  "TARGET_P9_VECTOR && TARGET_64BIT"
{
  operands[3] = gen_reg_rtx (DImode);
})

(define_insn "*lxvl"
  [(set (match_operand:V16QI 0 "vsx_register_operand" "=wa")
	(unspec:V16QI
	 [(match_operand:DI 1 "gpc_reg_operand" "b")
	  (match_operand:DI 2 "register_operand" "+r")]
	 UNSPEC_LXVL))]
  "TARGET_P9_VECTOR && TARGET_64BIT"
  "sldi %2,%2, 56\; lxvl %x0,%1,%2"
  [(set_attr "length" "8")
   (set_attr "type" "vecload")])

;; Store VSX Vector with Length
(define_expand "stxvl"
  [(set (match_dup 3)
	(match_operand:DI 2 "register_operand"))
   (set (mem:V16QI (match_operand:DI 1 "gpc_reg_operand"))
	(unspec:V16QI
	 [(match_operand:V16QI 0 "vsx_register_operand")
	  (match_dup 3)]
	 UNSPEC_STXVL))]
  "TARGET_P9_VECTOR && TARGET_64BIT"
{
  operands[3] = gen_reg_rtx (DImode);
})

(define_insn "*stxvl"
  [(set (mem:V16QI (match_operand:DI 1 "gpc_reg_operand" "b"))
	(unspec:V16QI
	 [(match_operand:V16QI 0 "vsx_register_operand" "wa")
	  (match_operand:DI 2 "register_operand" "+r")]
	 UNSPEC_STXVL))]
  "TARGET_P9_VECTOR && TARGET_64BIT"
  "sldi %2,%2\;stxvl %x0,%1,%2"
  [(set_attr "length" "8")
   (set_attr "type" "vecstore")])

;; Vector Compare Not Equal Byte
(define_insn "vcmpneb"
  [(set (match_operand:V16QI 0 "altivec_register_operand" "=v")
	(unspec:V16QI [(match_operand:V16QI 1 "altivec_register_operand" "v")
		       (match_operand:V16QI 2 "altivec_register_operand" "v")]
	 UNSPEC_VCMPNEB))]
  "TARGET_P9_VECTOR"
  "vcmpneb %0,%1,%2"
  [(set_attr "type" "vecsimple")])

;; Vector Compare Not Equal or Zero Byte
(define_insn "vcmpnezb"
  [(set (match_operand:V16QI 0 "altivec_register_operand" "=v")
	(unspec:V16QI
	 [(match_operand:V16QI 1 "altivec_register_operand" "v")
	  (match_operand:V16QI 2 "altivec_register_operand" "v")]
	 UNSPEC_VCMPNEZB))]
  "TARGET_P9_VECTOR"
  "vcmpnezb %0,%1,%2"
  [(set_attr "type" "vecsimple")])

;; Vector Compare Not Equal Half Word
(define_insn "vcmpneh"
  [(set (match_operand:V8HI 0 "altivec_register_operand" "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "altivec_register_operand" "v")
		      (match_operand:V8HI 2 "altivec_register_operand" "v")]
	 UNSPEC_VCMPNEH))]
  "TARGET_P9_VECTOR"
  "vcmpneh %0,%1,%2"
  [(set_attr "type" "vecsimple")])

;; Vector Compare Not Equal or Zero Half Word
(define_insn "vcmpnezh"
  [(set (match_operand:V8HI 0 "altivec_register_operand" "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "altivec_register_operand" "v")
		      (match_operand:V8HI 2 "altivec_register_operand" "v")]
	 UNSPEC_VCMPNEZH))]
  "TARGET_P9_VECTOR"
  "vcmpnezh %0,%1,%2"
  [(set_attr "type" "vecsimple")])

;; Vector Compare Not Equal Word
(define_insn "vcmpnew"
  [(set (match_operand:V4SI 0 "altivec_register_operand" "=v")
	(unspec:V4SI
	 [(match_operand:V4SI 1 "altivec_register_operand" "v")
	  (match_operand:V4SI 2 "altivec_register_operand" "v")]
	 UNSPEC_VCMPNEH))]
  "TARGET_P9_VECTOR"
  "vcmpnew %0,%1,%2"
  [(set_attr "type" "vecsimple")])

;; Vector Compare Not Equal Float or Double
(define_insn "vcmpne<VSs>"
  [(set (match_operand:<VSI> 0 "vsx_register_operand" "=wa")
	(unspec:<VSI>
	 [(match_operand:VSX_F 1 "vsx_register_operand" "wa")
	  (match_operand:VSX_F 2 "vsx_register_operand" "wa")]
	 UNSPEC_VCMPNEH))]
  "TARGET_P9_VECTOR"
  "xvcmpne<VSs> %x0,%x1,%x2"
  [(set_attr "type" "vecsimple")])

;; Vector Compare Not Equal or Zero Word
(define_insn "vcmpnezw"
  [(set (match_operand:V4SI 0 "altivec_register_operand" "=v")
	(unspec:V4SI [(match_operand:V4SI 1 "altivec_register_operand" "v")
		      (match_operand:V4SI 2 "altivec_register_operand" "v")]
	 UNSPEC_VCMPNEZW))]
  "TARGET_P9_VECTOR"
  "vcmpnezw %0,%1,%2"
  [(set_attr "type" "vecsimple")])

;; Vector Count Leading Zero Least-Significant Bits Byte
(define_insn "vclzlsbb"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI
	 [(match_operand:V16QI 1 "altivec_register_operand" "v")]
	 UNSPEC_VCLZLSBB))]
  "TARGET_P9_VECTOR"
  "vclzlsbb %0,%1"
  [(set_attr "type" "vecsimple")])

;; Vector Count Trailing Zero Least-Significant Bits Byte
(define_insn "vctzlsbb"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI
	 [(match_operand:V16QI 1 "altivec_register_operand" "v")]
	 UNSPEC_VCTZLSBB))]
  "TARGET_P9_VECTOR"
  "vctzlsbb %0,%1"
  [(set_attr "type" "vecsimple")])

;; Vector Extract Unsigned Byte Left-Indexed
(define_insn "vextublx"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI
	 [(match_operand:SI 1 "register_operand" "r")
	  (match_operand:V16QI 2 "altivec_register_operand" "v")]
	 UNSPEC_VEXTUBLX))]
  "TARGET_P9_VECTOR"
  "vextublx %0,%1,%2"
  [(set_attr "type" "vecsimple")])

;; Vector Extract Unsigned Byte Right-Indexed
(define_insn "vextubrx"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI
	 [(match_operand:SI 1 "register_operand" "r")
	  (match_operand:V16QI 2 "altivec_register_operand" "v")]
	 UNSPEC_VEXTUBRX))]
  "TARGET_P9_VECTOR"
  "vextubrx %0,%1,%2"
  [(set_attr "type" "vecsimple")])

;; Vector Extract Unsigned Half Word Left-Indexed
(define_insn "vextuhlx"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI
	 [(match_operand:SI 1 "register_operand" "r")
	  (match_operand:V16QI 2 "altivec_register_operand" "v")]
	 UNSPEC_VEXTUHLX))]
  "TARGET_P9_VECTOR"
  "vextuhlx %0,%1,%2"
  [(set_attr "type" "vecsimple")])

;; Vector Extract Unsigned Half Word Right-Indexed
(define_insn "vextuhrx"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI
	 [(match_operand:SI 1 "register_operand" "r")
	  (match_operand:V16QI 2 "altivec_register_operand" "v")]
	 UNSPEC_VEXTUHRX))]
  "TARGET_P9_VECTOR"
  "vextuhrx %0,%1,%2"
  [(set_attr "type" "vecsimple")])

;; Vector Extract Unsigned Word Left-Indexed
(define_insn "vextuwlx"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI
	 [(match_operand:SI 1 "register_operand" "r")
	  (match_operand:V16QI 2 "altivec_register_operand" "v")]
	 UNSPEC_VEXTUWLX))]
  "TARGET_P9_VECTOR"
  "vextuwlx %0,%1,%2"
  [(set_attr "type" "vecsimple")])

;; Vector Extract Unsigned Word Right-Indexed
(define_insn "vextuwrx"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI
	 [(match_operand:SI 1 "register_operand" "r")
	  (match_operand:V16QI 2 "altivec_register_operand" "v")]
	 UNSPEC_VEXTUWRX))]
  "TARGET_P9_VECTOR"
  "vextuwrx %0,%1,%2"
  [(set_attr "type" "vecsimple")])
