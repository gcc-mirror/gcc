;; VSX patterns.
;; Copyright (C) 2009-2013 Free Software Foundation, Inc.
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

;; Iterator for both scalar and vector floating point types supported by VSX
(define_mode_iterator VSX_B [DF V4SF V2DF])

;; Iterator for the 2 64-bit vector types
(define_mode_iterator VSX_D [V2DF V2DI])

;; Iterator for the 2 32-bit vector types
(define_mode_iterator VSX_W [V4SF V4SI])

;; Iterator for the DF types
(define_mode_iterator VSX_DF [V2DF DF])

;; Iterator for vector floating point types supported by VSX
(define_mode_iterator VSX_F [V4SF V2DF])

;; Iterator for logical types supported by VSX
(define_mode_iterator VSX_L [V16QI V8HI V4SI V2DI V4SF V2DF TI])

;; Iterator for memory move.  Handle TImode specially to allow
;; it to use gprs as well as vsx registers.
(define_mode_iterator VSX_M [V16QI V8HI V4SI V2DI V4SF V2DF])

(define_mode_iterator VSX_M2 [V16QI
			      V8HI
			      V4SI
			      V2DI
			      V4SF
			      V2DF
			      (TI	"TARGET_VSX_TIMODE")])

;; Map into the appropriate load/store name based on the type
(define_mode_attr VSm  [(V16QI "vw4")
			(V8HI  "vw4")
			(V4SI  "vw4")
			(V4SF  "vw4")
			(V2DF  "vd2")
			(V2DI  "vd2")
			(DF    "d")
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
			 (TI    "dp")])

;; Map the register class used
(define_mode_attr VSr	[(V16QI "v")
			 (V8HI  "v")
			 (V4SI  "v")
			 (V4SF  "wf")
			 (V2DI  "wd")
			 (V2DF  "wd")
			 (DF    "ws")
			 (SF	"d")
			 (TI    "wt")])

;; Map the register class used for float<->int conversions
(define_mode_attr VSr2	[(V2DF  "wd")
			 (V4SF  "wf")
			 (DF    "ws")])

(define_mode_attr VSr3	[(V2DF  "wa")
			 (V4SF  "wa")
			 (DF    "ws")])

;; Map the register class for sp<->dp float conversions, destination
(define_mode_attr VSr4	[(SF	"ws")
			 (DF	"f")
			 (V2DF  "wd")
			 (V4SF	"v")])

;; Map the register class for sp<->dp float conversions, destination
(define_mode_attr VSr5	[(SF	"ws")
			 (DF	"f")
			 (V2DF  "v")
			 (V4SF	"wd")])

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
			 (DF    "s")])

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
(define_mode_attr VS_scalar [(V2DF	"DF")
			     (V2DI	"DI")
			     (V4SF	"SF")
			     (V4SI	"SI")
			     (V8HI	"HI")
			     (V16QI	"QI")])

;; Map to a double-sized vector mode
(define_mode_attr VS_double [(V4SI	"V8SI")
			     (V4SF	"V8SF")
			     (V2DI	"V4DI")
			     (V2DF	"V4DF")])

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
  ])

;; VSX moves
(define_insn "*vsx_mov<mode>"
  [(set (match_operand:VSX_M 0 "nonimmediate_operand" "=Z,<VSr>,<VSr>,?Z,?wa,?wa,wQ,?&r,??Y,??r,??r,<VSr>,?wa,*r,v,wZ, v")
	(match_operand:VSX_M 1 "input_operand" "<VSr>,Z,<VSr>,wa,Z,wa,r,wQ,r,Y,r,j,j,j,W,v,wZ"))]
  "VECTOR_MEM_VSX_P (<MODE>mode)
   && (register_operand (operands[0], <MODE>mode) 
       || register_operand (operands[1], <MODE>mode))"
{
  return rs6000_output_move_128bit (operands);
}
  [(set_attr "type" "vecstore,vecload,vecsimple,vecstore,vecload,vecsimple,load,store,store,load, *,vecsimple,vecsimple,*, *,vecstore,vecload")
   (set_attr "length" "4,4,4,4,4,4,12,12,12,12,16,4,4,*,16,4,4")])

;; Unlike other VSX moves, allow the GPRs even for reloading, since a normal
;; use of TImode is for unions.  However for plain data movement, slightly
;; favor the vector loads
(define_insn "*vsx_movti_64bit"
  [(set (match_operand:TI 0 "nonimmediate_operand" "=Z,wa,wa,wa,v,v,wZ,wQ,&r,Y,r,r,?r")
	(match_operand:TI 1 "input_operand" "wa,Z,wa,O,W,wZ,v,r,wQ,r,Y,r,n"))]
  "TARGET_POWERPC64 && VECTOR_MEM_VSX_P (TImode)
   && (register_operand (operands[0], TImode) 
       || register_operand (operands[1], TImode))"
{
  return rs6000_output_move_128bit (operands);
}
  [(set_attr "type" "vecstore,vecload,vecsimple,vecsimple,vecsimple,vecstore,vecload,store,load,store,load,*,*")
   (set_attr "length" "4,4,4,4,16,4,4,8,8,8,8,8,8")])

(define_insn "*vsx_movti_32bit"
  [(set (match_operand:TI 0 "nonimmediate_operand" "=Z,wa,wa,wa,v, v,wZ,Q,Y,????r,????r,????r,r")
	(match_operand:TI 1 "input_operand"        "wa, Z,wa, O,W,wZ, v,r,r,    Q,    Y,    r,n"))]
  "! TARGET_POWERPC64 && VECTOR_MEM_VSX_P (TImode)
   && (register_operand (operands[0], TImode)
       || register_operand (operands[1], TImode))"
{
  switch (which_alternative)
    {
    case 0:
      return "stxvd2x %x1,%y0";

    case 1:
      return "lxvd2x %x0,%y1";

    case 2:
      return "xxlor %x0,%x1,%x1";

    case 3:
      return "xxlxor %x0,%x0,%x0";

    case 4:
      return output_vec_const_move (operands);

    case 5:
      return "stvx %1,%y0";

    case 6:
      return "lvx %0,%y1";

    case 7:
      if (TARGET_STRING)
        return \"stswi %1,%P0,16\";

    case 8:
      return \"#\";

    case 9:
      /* If the address is not used in the output, we can use lsi.  Otherwise,
	 fall through to generating four loads.  */
      if (TARGET_STRING
          && ! reg_overlap_mentioned_p (operands[0], operands[1]))
	return \"lswi %0,%P1,16\";
      /* ... fall through ...  */

    case 10:
    case 11:
    case 12:
      return \"#\";
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "type" "vecstore,vecload,vecsimple,vecsimple,vecsimple,vecstore,vecload,store_ux,store_ux,load_ux,load_ux, *, *")
   (set_attr "length" "     4,      4,        4,       4,         8,       4,      4,      16,      16,     16,     16,16,16")
   (set (attr "cell_micro") (if_then_else (match_test "TARGET_STRING")
   			                  (const_string "always")
					  (const_string "conditional")))])

;; Explicit  load/store expanders for the builtin functions
(define_expand "vsx_load_<mode>"
  [(set (match_operand:VSX_M 0 "vsx_register_operand" "")
	(match_operand:VSX_M 1 "memory_operand" ""))]
  "VECTOR_MEM_VSX_P (<MODE>mode)"
  "")

(define_expand "vsx_store_<mode>"
  [(set (match_operand:VSX_M 0 "memory_operand" "")
	(match_operand:VSX_M 1 "vsx_register_operand" ""))]
  "VECTOR_MEM_VSX_P (<MODE>mode)"
  "")


;; VSX vector floating point arithmetic instructions.  The VSX scalar
;; instructions are now combined with the insn for the traditional floating
;; point unit.
(define_insn "*vsx_add<mode>3"
  [(set (match_operand:VSX_F 0 "vsx_register_operand" "=<VSr>,?wa")
        (plus:VSX_F (match_operand:VSX_F 1 "vsx_register_operand" "<VSr>,wa")
		    (match_operand:VSX_F 2 "vsx_register_operand" "<VSr>,wa")))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "xvadd<VSs> %x0,%x1,%x2"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_simple>")])

(define_insn "*vsx_sub<mode>3"
  [(set (match_operand:VSX_F 0 "vsx_register_operand" "=<VSr>,?wa")
        (minus:VSX_F (match_operand:VSX_F 1 "vsx_register_operand" "<VSr>,wa")
		     (match_operand:VSX_F 2 "vsx_register_operand" "<VSr>,wa")))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "xvsub<VSs> %x0,%x1,%x2"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_simple>")])

(define_insn "*vsx_mul<mode>3"
  [(set (match_operand:VSX_F 0 "vsx_register_operand" "=<VSr>,?wa")
        (mult:VSX_F (match_operand:VSX_F 1 "vsx_register_operand" "<VSr>,wa")
		    (match_operand:VSX_F 2 "vsx_register_operand" "<VSr>,wa")))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "xvmul<VSs> %x0,%x1,%x2"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_mul>")])

(define_insn "*vsx_div<mode>3"
  [(set (match_operand:VSX_F 0 "vsx_register_operand" "=<VSr>,?wa")
        (div:VSX_F (match_operand:VSX_F 1 "vsx_register_operand" "<VSr>,wa")
		   (match_operand:VSX_F 2 "vsx_register_operand" "<VSr>,wa")))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "xvdiv<VSs> %x0,%x1,%x2"
  [(set_attr "type" "<VStype_div>")
   (set_attr "fp_type" "<VSfptype_div>")])

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
	(unspec:CCFP [(match_operand:VSX_B 1 "vsx_register_operand" "<VSr>,wa")
		      (match_operand:VSX_B 2 "vsx_register_operand" "<VSr>,wa")]
		   UNSPEC_VSX_TDIV))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "x<VSv>tdiv<VSs> %0,%x1,%x2"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_simple>")])

(define_insn "vsx_fre<mode>2"
  [(set (match_operand:VSX_F 0 "vsx_register_operand" "=<VSr>,?wa")
	(unspec:VSX_F [(match_operand:VSX_F 1 "vsx_register_operand" "<VSr>,wa")]
		      UNSPEC_FRES))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "xvre<VSs> %x0,%x1"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_simple>")])

(define_insn "*vsx_neg<mode>2"
  [(set (match_operand:VSX_F 0 "vsx_register_operand" "=<VSr>,?wa")
        (neg:VSX_F (match_operand:VSX_F 1 "vsx_register_operand" "<VSr>,wa")))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "xvneg<VSs> %x0,%x1"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_simple>")])

(define_insn "*vsx_abs<mode>2"
  [(set (match_operand:VSX_F 0 "vsx_register_operand" "=<VSr>,?wa")
        (abs:VSX_F (match_operand:VSX_F 1 "vsx_register_operand" "<VSr>,wa")))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "xvabs<VSs> %x0,%x1"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_simple>")])

(define_insn "vsx_nabs<mode>2"
  [(set (match_operand:VSX_F 0 "vsx_register_operand" "=<VSr>,?wa")
        (neg:VSX_F
	 (abs:VSX_F
	  (match_operand:VSX_F 1 "vsx_register_operand" "<VSr>,wa"))))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "xvnabs<VSs> %x0,%x1"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_simple>")])

(define_insn "vsx_smax<mode>3"
  [(set (match_operand:VSX_F 0 "vsx_register_operand" "=<VSr>,?wa")
        (smax:VSX_F (match_operand:VSX_F 1 "vsx_register_operand" "<VSr>,wa")
		    (match_operand:VSX_F 2 "vsx_register_operand" "<VSr>,wa")))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "xvmax<VSs> %x0,%x1,%x2"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_simple>")])

(define_insn "*vsx_smin<mode>3"
  [(set (match_operand:VSX_F 0 "vsx_register_operand" "=<VSr>,?wa")
        (smin:VSX_F (match_operand:VSX_F 1 "vsx_register_operand" "<VSr>,wa")
		    (match_operand:VSX_F 2 "vsx_register_operand" "<VSr>,wa")))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "xvmin<VSs> %x0,%x1,%x2"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_simple>")])

(define_insn "*vsx_sqrt<mode>2"
  [(set (match_operand:VSX_F 0 "vsx_register_operand" "=<VSr>,?wa")
        (sqrt:VSX_F (match_operand:VSX_F 1 "vsx_register_operand" "<VSr>,wa")))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "xvsqrt<VSs> %x0,%x1"
  [(set_attr "type" "<VStype_sqrt>")
   (set_attr "fp_type" "<VSfptype_sqrt>")])

(define_insn "*vsx_rsqrte<mode>2"
  [(set (match_operand:VSX_F 0 "vsx_register_operand" "=<VSr>,?wa")
	(unspec:VSX_F [(match_operand:VSX_F 1 "vsx_register_operand" "<VSr>,wa")]
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
	(unspec:CCFP [(match_operand:VSX_B 1 "vsx_register_operand" "<VSr>,wa")]
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
  [(set (match_operand:V4SF 0 "vsx_register_operand" "=ws,ws,?wa,?wa,v")
	(fma:V4SF
	  (match_operand:V4SF 1 "vsx_register_operand" "%ws,ws,wa,wa,v")
	  (match_operand:V4SF 2 "vsx_register_operand" "ws,0,wa,0,v")
	  (match_operand:V4SF 3 "vsx_register_operand" "0,ws,0,wa,v")))]
  "VECTOR_UNIT_VSX_P (V4SFmode)"
  "@
   xvmaddasp %x0,%x1,%x2
   xvmaddmsp %x0,%x1,%x3
   xvmaddasp %x0,%x1,%x2
   xvmaddmsp %x0,%x1,%x3
   vmaddfp %0,%1,%2,%3"
  [(set_attr "type" "vecfloat")])

(define_insn "*vsx_fmav2df4"
  [(set (match_operand:V2DF 0 "vsx_register_operand" "=ws,ws,?wa,?wa")
	(fma:V2DF
	  (match_operand:V2DF 1 "vsx_register_operand" "%ws,ws,wa,wa")
	  (match_operand:V2DF 2 "vsx_register_operand" "ws,0,wa,0")
	  (match_operand:V2DF 3 "vsx_register_operand" "0,ws,0,wa")))]
  "VECTOR_UNIT_VSX_P (V2DFmode)"
  "@
   xvmaddadp %x0,%x1,%x2
   xvmaddmdp %x0,%x1,%x3
   xvmaddadp %x0,%x1,%x2
   xvmaddmdp %x0,%x1,%x3"
  [(set_attr "type" "vecdouble")])

(define_insn "*vsx_fms<mode>4"
  [(set (match_operand:VSX_F 0 "vsx_register_operand" "=<VSr>,<VSr>,?wa,?wa")
	(fma:VSX_F
	  (match_operand:VSX_F 1 "vsx_register_operand" "%<VSr>,<VSr>,wa,wa")
	  (match_operand:VSX_F 2 "vsx_register_operand" "<VSr>,0,wa,0")
	  (neg:VSX_F
	    (match_operand:VSX_F 3 "vsx_register_operand" "0,<VSr>,0,wa"))))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "@
   xvmsuba<VSs> %x0,%x1,%x2
   xvmsubm<VSs> %x0,%x1,%x3
   xvmsuba<VSs> %x0,%x1,%x2
   xvmsubm<VSs> %x0,%x1,%x3"
  [(set_attr "type" "<VStype_mul>")])

(define_insn "*vsx_nfma<mode>4"
  [(set (match_operand:VSX_F 0 "vsx_register_operand" "=<VSr>,<VSr>,?wa,?wa")
	(neg:VSX_F
	 (fma:VSX_F
	  (match_operand:VSX_F 1 "vsx_register_operand" "<VSr>,<VSr>,wa,wa")
	  (match_operand:VSX_F 2 "vsx_register_operand" "<VSr>,0,wa,0")
	  (match_operand:VSX_F 3 "vsx_register_operand" "0,<VSr>,0,wa"))))]
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
  [(set (match_operand:VSX_F 0 "vsx_register_operand" "=<VSr>,?wa")
	(eq:VSX_F (match_operand:VSX_F 1 "vsx_register_operand" "<VSr>,wa")
		  (match_operand:VSX_F 2 "vsx_register_operand" "<VSr>,wa")))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "xvcmpeq<VSs> %x0,%x1,%x2"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_simple>")])

(define_insn "vsx_gt<mode>"
  [(set (match_operand:VSX_F 0 "vsx_register_operand" "=<VSr>,?wa")
	(gt:VSX_F (match_operand:VSX_F 1 "vsx_register_operand" "<VSr>,wa")
		  (match_operand:VSX_F 2 "vsx_register_operand" "<VSr>,wa")))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "xvcmpgt<VSs> %x0,%x1,%x2"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_simple>")])

(define_insn "*vsx_ge<mode>"
  [(set (match_operand:VSX_F 0 "vsx_register_operand" "=<VSr>,?wa")
	(ge:VSX_F (match_operand:VSX_F 1 "vsx_register_operand" "<VSr>,wa")
		  (match_operand:VSX_F 2 "vsx_register_operand" "<VSr>,wa")))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "xvcmpge<VSs> %x0,%x1,%x2"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_simple>")])

;; Compare vectors producing a vector result and a predicate, setting CR6 to
;; indicate a combined status
(define_insn "*vsx_eq_<mode>_p"
  [(set (reg:CC 74)
	(unspec:CC
	 [(eq:CC (match_operand:VSX_F 1 "vsx_register_operand" "<VSr>,?wa")
		 (match_operand:VSX_F 2 "vsx_register_operand" "<VSr>,?wa"))]
	 UNSPEC_PREDICATE))
   (set (match_operand:VSX_F 0 "vsx_register_operand" "=<VSr>,?wa")
	(eq:VSX_F (match_dup 1)
		  (match_dup 2)))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "xvcmpeq<VSs>. %x0,%x1,%x2"
  [(set_attr "type" "<VStype_simple>")])

(define_insn "*vsx_gt_<mode>_p"
  [(set (reg:CC 74)
	(unspec:CC
	 [(gt:CC (match_operand:VSX_F 1 "vsx_register_operand" "<VSr>,?wa")
		 (match_operand:VSX_F 2 "vsx_register_operand" "<VSr>,?wa"))]
	 UNSPEC_PREDICATE))
   (set (match_operand:VSX_F 0 "vsx_register_operand" "=<VSr>,?wa")
	(gt:VSX_F (match_dup 1)
		  (match_dup 2)))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "xvcmpgt<VSs>. %x0,%x1,%x2"
  [(set_attr "type" "<VStype_simple>")])

(define_insn "*vsx_ge_<mode>_p"
  [(set (reg:CC 74)
	(unspec:CC
	 [(ge:CC (match_operand:VSX_F 1 "vsx_register_operand" "<VSr>,?wa")
		 (match_operand:VSX_F 2 "vsx_register_operand" "<VSr>,?wa"))]
	 UNSPEC_PREDICATE))
   (set (match_operand:VSX_F 0 "vsx_register_operand" "=<VSr>,?wa")
	(ge:VSX_F (match_dup 1)
		  (match_dup 2)))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "xvcmpge<VSs>. %x0,%x1,%x2"
  [(set_attr "type" "<VStype_simple>")])

;; Vector select
(define_insn "*vsx_xxsel<mode>"
  [(set (match_operand:VSX_L 0 "vsx_register_operand" "=<VSr>,?wa")
	(if_then_else:VSX_L
	 (ne:CC (match_operand:VSX_L 1 "vsx_register_operand" "<VSr>,wa")
		(match_operand:VSX_L 4 "zero_constant" ""))
	 (match_operand:VSX_L 2 "vsx_register_operand" "<VSr>,wa")
	 (match_operand:VSX_L 3 "vsx_register_operand" "<VSr>,wa")))]
  "VECTOR_MEM_VSX_P (<MODE>mode)"
  "xxsel %x0,%x3,%x2,%x1"
  [(set_attr "type" "vecperm")])

(define_insn "*vsx_xxsel<mode>_uns"
  [(set (match_operand:VSX_L 0 "vsx_register_operand" "=<VSr>,?wa")
	(if_then_else:VSX_L
	 (ne:CCUNS (match_operand:VSX_L 1 "vsx_register_operand" "<VSr>,wa")
		   (match_operand:VSX_L 4 "zero_constant" ""))
	 (match_operand:VSX_L 2 "vsx_register_operand" "<VSr>,wa")
	 (match_operand:VSX_L 3 "vsx_register_operand" "<VSr>,wa")))]
  "VECTOR_MEM_VSX_P (<MODE>mode)"
  "xxsel %x0,%x3,%x2,%x1"
  [(set_attr "type" "vecperm")])

;; Copy sign
(define_insn "vsx_copysign<mode>3"
  [(set (match_operand:VSX_F 0 "vsx_register_operand" "=<VSr>,?wa")
	(unspec:VSX_F
	 [(match_operand:VSX_F 1 "vsx_register_operand" "<VSr>,wa")
	  (match_operand:VSX_F 2 "vsx_register_operand" "<VSr>,wa")]
	 UNSPEC_COPYSIGN))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "xvcpsgn<VSs> %x0,%x2,%x1"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_simple>")])

;; For the conversions, limit the register class for the integer value to be
;; the fprs because we don't want to add the altivec registers to movdi/movsi.
;; For the unsigned tests, there isn't a generic double -> unsigned conversion
;; in rs6000.md so don't test VECTOR_UNIT_VSX_P, just test against VSX.
;; Don't use vsx_register_operand here, use gpc_reg_operand to match rs6000.md.
(define_insn "vsx_float<VSi><mode>2"
  [(set (match_operand:VSX_B 0 "gpc_reg_operand" "=<VSr>,?wa")
	(float:VSX_B (match_operand:<VSI> 1 "gpc_reg_operand" "<VSr2>,<VSr3>")))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "x<VSv>cvsx<VSc><VSs> %x0,%x1"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_simple>")])

(define_insn "vsx_floatuns<VSi><mode>2"
  [(set (match_operand:VSX_B 0 "gpc_reg_operand" "=<VSr>,?wa")
	(unsigned_float:VSX_B (match_operand:<VSI> 1 "gpc_reg_operand" "<VSr2>,<VSr3>")))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "x<VSv>cvux<VSc><VSs> %x0,%x1"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_simple>")])

(define_insn "vsx_fix_trunc<mode><VSi>2"
  [(set (match_operand:<VSI> 0 "gpc_reg_operand" "=<VSr2>,?<VSr3>")
	(fix:<VSI> (match_operand:VSX_B 1 "gpc_reg_operand" "<VSr>,wa")))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "x<VSv>cv<VSs>sx<VSc>s %x0,%x1"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_simple>")])

(define_insn "vsx_fixuns_trunc<mode><VSi>2"
  [(set (match_operand:<VSI> 0 "gpc_reg_operand" "=<VSr2>,?<VSr3>")
	(unsigned_fix:<VSI> (match_operand:VSX_B 1 "gpc_reg_operand" "<VSr>,wa")))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "x<VSv>cv<VSs>ux<VSc>s %x0,%x1"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_simple>")])

;; Math rounding functions
(define_insn "vsx_x<VSv>r<VSs>i"
  [(set (match_operand:VSX_B 0 "vsx_register_operand" "=<VSr>,?wa")
	(unspec:VSX_B [(match_operand:VSX_B 1 "vsx_register_operand" "<VSr>,wa")]
		      UNSPEC_VSX_ROUND_I))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "x<VSv>r<VSs>i %x0,%x1"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_simple>")])

(define_insn "vsx_x<VSv>r<VSs>ic"
  [(set (match_operand:VSX_B 0 "vsx_register_operand" "=<VSr>,?wa")
	(unspec:VSX_B [(match_operand:VSX_B 1 "vsx_register_operand" "<VSr>,wa")]
		      UNSPEC_VSX_ROUND_IC))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "x<VSv>r<VSs>ic %x0,%x1"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_simple>")])

(define_insn "vsx_btrunc<mode>2"
  [(set (match_operand:VSX_F 0 "vsx_register_operand" "=<VSr>,?wa")
	(fix:VSX_F (match_operand:VSX_F 1 "vsx_register_operand" "<VSr>,wa")))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "xvr<VSs>iz %x0,%x1"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_simple>")])

(define_insn "*vsx_b2trunc<mode>2"
  [(set (match_operand:VSX_B 0 "vsx_register_operand" "=<VSr>,?wa")
	(unspec:VSX_B [(match_operand:VSX_B 1 "vsx_register_operand" "<VSr>,wa")]
		      UNSPEC_FRIZ))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "x<VSv>r<VSs>iz %x0,%x1"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_simple>")])

(define_insn "vsx_floor<mode>2"
  [(set (match_operand:VSX_F 0 "vsx_register_operand" "=<VSr>,?wa")
	(unspec:VSX_F [(match_operand:VSX_F 1 "vsx_register_operand" "<VSr>,wa")]
		      UNSPEC_FRIM))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "xvr<VSs>im %x0,%x1"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_simple>")])

(define_insn "vsx_ceil<mode>2"
  [(set (match_operand:VSX_F 0 "vsx_register_operand" "=<VSr>,?wa")
	(unspec:VSX_F [(match_operand:VSX_F 1 "vsx_register_operand" "<VSr>,wa")]
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
  [(set (match_operand:<VS_spdp_res> 0 "vsx_register_operand" "=<VSr4>,?wa")
	(unspec:<VS_spdp_res> [(match_operand:VSX_SPDP 1 "vsx_register_operand" "<VSr5>,wa")]
			      UNSPEC_VSX_CVSPDP))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "<VS_spdp_insn> %x0,%x1"
  [(set_attr "type" "<VS_spdp_type>")])

;; xscvspdp, represent the scalar SF type as V4SF
(define_insn "vsx_xscvspdp"
  [(set (match_operand:DF 0 "vsx_register_operand" "=ws,?wa")
	(unspec:DF [(match_operand:V4SF 1 "vsx_register_operand" "wa,wa")]
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
  [(set (match_operand:SF 0 "vsx_register_operand" "=f")
	(unspec:SF [(match_operand:V4SF 1 "vsx_register_operand" "wa")]
		   UNSPEC_VSX_CVSPDP))]
  "VECTOR_UNIT_VSX_P (V4SFmode)"
  "xscvspdp %x0,%x1"
  [(set_attr "type" "fp")])

;; ISA 2.07 xscvdpspn/xscvspdpn that does not raise an error on signalling NaNs
(define_insn "vsx_xscvdpspn"
  [(set (match_operand:V4SF 0 "vsx_register_operand" "=ws,?wa")
	(unspec:V4SF [(match_operand:DF 1 "vsx_register_operand" "wd,wa")]
		     UNSPEC_VSX_CVDPSPN))]
  "TARGET_XSCVDPSPN"
  "xscvdpspn %x0,%x1"
  [(set_attr "type" "fp")])

(define_insn "vsx_xscvspdpn"
  [(set (match_operand:DF 0 "vsx_register_operand" "=ws,?wa")
	(unspec:DF [(match_operand:V4SF 1 "vsx_register_operand" "wa,wa")]
		   UNSPEC_VSX_CVSPDPN))]
  "TARGET_XSCVSPDPN"
  "xscvspdpn %x0,%x1"
  [(set_attr "type" "fp")])

(define_insn "vsx_xscvdpspn_scalar"
  [(set (match_operand:V4SF 0 "vsx_register_operand" "=wa")
	(unspec:V4SF [(match_operand:SF 1 "vsx_register_operand" "f")]
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
;; since the xsrdpiz instruction does not truncate the value if the floating
;; point value is < LONG_MIN or > LONG_MAX.
(define_insn "*vsx_float_fix_<mode>2"
  [(set (match_operand:VSX_DF 0 "vsx_register_operand" "=<VSr>,?wa")
	(float:VSX_DF
	 (fix:<VSI>
	  (match_operand:VSX_DF 1 "vsx_register_operand" "<VSr>,?wa"))))]
  "TARGET_HARD_FLOAT && TARGET_FPRS && TARGET_DOUBLE_FLOAT
   && VECTOR_UNIT_VSX_P (<MODE>mode) && flag_unsafe_math_optimizations
   && !flag_trapping_math && TARGET_FRIZ"
  "x<VSv>r<VSs>iz %x0,%x1"
  [(set_attr "type" "<VStype_simple>")
   (set_attr "fp_type" "<VSfptype_simple>")])


;; Permute operations

;; Build a V2DF/V2DI vector from two scalars
(define_insn "vsx_concat_<mode>"
  [(set (match_operand:VSX_D 0 "vsx_register_operand" "=<VSr>,?wa")
	(vec_concat:VSX_D
	 (match_operand:<VS_scalar> 1 "vsx_register_operand" "ws,wa")
	 (match_operand:<VS_scalar> 2 "vsx_register_operand" "ws,wa")))]
  "VECTOR_MEM_VSX_P (<MODE>mode)"
  "xxpermdi %x0,%x1,%x2,0"
  [(set_attr "type" "vecperm")])

;; Special purpose concat using xxpermdi to glue two single precision values
;; together, relying on the fact that internally scalar floats are represented
;; as doubles.  This is used to initialize a V4SF vector with 4 floats
(define_insn "vsx_concat_v2sf"
  [(set (match_operand:V2DF 0 "vsx_register_operand" "=wd,?wa")
	(unspec:V2DF
	 [(match_operand:SF 1 "vsx_register_operand" "f,f")
	  (match_operand:SF 2 "vsx_register_operand" "f,f")]
	 UNSPEC_VSX_CONCAT))]
  "VECTOR_MEM_VSX_P (V2DFmode)"
  "xxpermdi %x0,%x1,%x2,0"
  [(set_attr "type" "vecperm")])

;; Set the element of a V2DI/VD2F mode
(define_insn "vsx_set_<mode>"
  [(set (match_operand:VSX_D 0 "vsx_register_operand" "=wd,?wa")
	(unspec:VSX_D [(match_operand:VSX_D 1 "vsx_register_operand" "wd,wa")
		       (match_operand:<VS_scalar> 2 "vsx_register_operand" "ws,wa")
		       (match_operand:QI 3 "u5bit_cint_operand" "i,i")]
		      UNSPEC_VSX_SET))]
  "VECTOR_MEM_VSX_P (<MODE>mode)"
{
  if (INTVAL (operands[3]) == 0)
    return \"xxpermdi %x0,%x2,%x1,1\";
  else if (INTVAL (operands[3]) == 1)
    return \"xxpermdi %x0,%x1,%x2,0\";
  else
    gcc_unreachable ();
}
  [(set_attr "type" "vecperm")])

;; Extract a DF/DI element from V2DF/V2DI
(define_insn "vsx_extract_<mode>"
  [(set (match_operand:<VS_scalar> 0 "vsx_register_operand" "=ws,d,?wa")
	(vec_select:<VS_scalar> (match_operand:VSX_D 1 "vsx_register_operand" "wd,wd,wa")
		       (parallel
			[(match_operand:QI 2 "u5bit_cint_operand" "i,i,i")])))]
  "VECTOR_MEM_VSX_P (<MODE>mode)"
{
  gcc_assert (UINTVAL (operands[2]) <= 1);
  operands[3] = GEN_INT (INTVAL (operands[2]) << 1);
  return \"xxpermdi %x0,%x1,%x1,%3\";
}
  [(set_attr "type" "vecperm")])

;; Optimize extracting element 0 from memory
(define_insn "*vsx_extract_<mode>_zero"
  [(set (match_operand:<VS_scalar> 0 "vsx_register_operand" "=ws,d,?wa")
	(vec_select:<VS_scalar>
	 (match_operand:VSX_D 1 "indexed_or_indirect_operand" "Z,Z,Z")
	 (parallel [(const_int 0)])))]
  "VECTOR_MEM_VSX_P (<MODE>mode) && WORDS_BIG_ENDIAN"
  "lxsd%U1x %x0,%y1"
  [(set (attr "type")
      (if_then_else
	(match_test "update_indexed_address_mem (operands[1], VOIDmode)")
	(const_string "fpload_ux")
	(const_string "fpload")))
   (set_attr "length" "4")])  

;; Extract a SF element from V4SF
(define_insn_and_split "vsx_extract_v4sf"
  [(set (match_operand:SF 0 "vsx_register_operand" "=f,f")
	(vec_select:SF
	 (match_operand:V4SF 1 "vsx_register_operand" "wa,wa")
	 (parallel [(match_operand:QI 2 "u5bit_cint_operand" "O,i")])))
   (clobber (match_scratch:V4SF 3 "=X,0"))]
  "VECTOR_UNIT_VSX_P (V4SFmode)"
  "@
   xscvspdp %x0,%x1
   #"
  ""
  [(const_int 0)]
  "
{
  rtx op0 = operands[0];
  rtx op1 = operands[1];
  rtx op2 = operands[2];
  rtx op3 = operands[3];
  rtx tmp;
  HOST_WIDE_INT ele = INTVAL (op2);

  if (ele == 0)
    tmp = op1;
  else
    {
      if (GET_CODE (op3) == SCRATCH)
	op3 = gen_reg_rtx (V4SFmode);
      emit_insn (gen_vsx_xxsldwi_v4sf (op3, op1, op1, op2));
      tmp = op3;
    }
  emit_insn (gen_vsx_xscvspdp_scalar2 (op0, tmp));
  DONE;
}"
  [(set_attr "length" "4,8")
   (set_attr "type" "fp")])

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
  emit_insn (gen (target, op0, op1, perm0, perm1));
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
  int mask = (INTVAL (operands[3]) << 1) | (INTVAL (operands[4]) - 2);
  operands[3] = GEN_INT (mask);
  return "xxpermdi %x0,%x1,%x2,%3";
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

;; Expanders for builtins
(define_expand "vsx_mergel_<mode>"
  [(set (match_operand:VSX_D 0 "vsx_register_operand" "")
	(vec_select:VSX_D
	  (vec_concat:<VS_double>
	    (match_operand:VSX_D 1 "vsx_register_operand" "")
	    (match_operand:VSX_D 2 "vsx_register_operand" ""))
	  (parallel [(const_int 1) (const_int 3)])))]
  "VECTOR_MEM_VSX_P (<MODE>mode)"
  "")

(define_expand "vsx_mergeh_<mode>"
  [(set (match_operand:VSX_D 0 "vsx_register_operand" "")
	(vec_select:VSX_D
	  (vec_concat:<VS_double>
	    (match_operand:VSX_D 1 "vsx_register_operand" "")
	    (match_operand:VSX_D 2 "vsx_register_operand" ""))
	  (parallel [(const_int 0) (const_int 2)])))]
  "VECTOR_MEM_VSX_P (<MODE>mode)"
  "")

;; V2DF/V2DI splat
(define_insn "vsx_splat_<mode>"
  [(set (match_operand:VSX_D 0 "vsx_register_operand" "=wd,wd,wd,?wa,?wa,?wa")
	(vec_duplicate:VSX_D
	 (match_operand:<VS_scalar> 1 "splat_input_operand" "ws,f,Z,wa,wa,Z")))]
  "VECTOR_MEM_VSX_P (<MODE>mode)"
  "@
   xxpermdi %x0,%x1,%x1,0
   xxpermdi %x0,%x1,%x1,0
   lxvdsx %x0,%y1
   xxpermdi %x0,%x1,%x1,0
   xxpermdi %x0,%x1,%x1,0
   lxvdsx %x0,%y1"
  [(set_attr "type" "vecperm,vecperm,vecload,vecperm,vecperm,vecload")])

;; V4SF/V4SI splat
(define_insn "vsx_xxspltw_<mode>"
  [(set (match_operand:VSX_W 0 "vsx_register_operand" "=wf,?wa")
	(vec_duplicate:VSX_W
	 (vec_select:<VS_scalar>
	  (match_operand:VSX_W 1 "vsx_register_operand" "wf,wa")
	  (parallel
	   [(match_operand:QI 2 "u5bit_cint_operand" "i,i")]))))]
  "VECTOR_MEM_VSX_P (<MODE>mode)"
  "xxspltw %x0,%x1,%2"
  [(set_attr "type" "vecperm")])

;; V4SF/V4SI interleave
(define_insn "vsx_xxmrghw_<mode>"
  [(set (match_operand:VSX_W 0 "vsx_register_operand" "=wf,?wa")
        (vec_select:VSX_W
	  (vec_concat:<VS_double>
	    (match_operand:VSX_W 1 "vsx_register_operand" "wf,wa")
	    (match_operand:VSX_W 2 "vsx_register_operand" "wf,wa"))
	  (parallel [(const_int 0) (const_int 4)
		     (const_int 1) (const_int 5)])))]
  "VECTOR_MEM_VSX_P (<MODE>mode)"
  "xxmrghw %x0,%x1,%x2"
  [(set_attr "type" "vecperm")])

(define_insn "vsx_xxmrglw_<mode>"
  [(set (match_operand:VSX_W 0 "vsx_register_operand" "=wf,?wa")
	(vec_select:VSX_W
	  (vec_concat:<VS_double>
	    (match_operand:VSX_W 1 "vsx_register_operand" "wf,wa")
	    (match_operand:VSX_W 2 "vsx_register_operand" "wf,?wa"))
	  (parallel [(const_int 2) (const_int 6)
		     (const_int 3) (const_int 7)])))]
  "VECTOR_MEM_VSX_P (<MODE>mode)"
  "xxmrglw %x0,%x1,%x2"
  [(set_attr "type" "vecperm")])

;; Shift left double by word immediate
(define_insn "vsx_xxsldwi_<mode>"
  [(set (match_operand:VSX_L 0 "vsx_register_operand" "=wa")
	(unspec:VSX_L [(match_operand:VSX_L 1 "vsx_register_operand" "wa")
		       (match_operand:VSX_L 2 "vsx_register_operand" "wa")
		       (match_operand:QI 3 "u5bit_cint_operand" "i")]
		      UNSPEC_VSX_SLDWI))]
  "VECTOR_MEM_VSX_P (<MODE>mode)"
  "xxsldwi %x0,%x1,%x2,%3"
  [(set_attr "type" "vecperm")])


;; Vector reduction insns and splitters

(define_insn_and_split "*vsx_reduc_<VEC_reduc_name>_v2df"
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

(define_insn_and_split "*vsx_reduc_<VEC_reduc_name>_v4sf"
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
  [(set (match_operand:DF 0 "vfloat_operand" "=&ws,&?wa,ws,?wa")
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
   (set (match_operand:VSX_M2 2 "vsx_register_operand" "")
	(mem:VSX_M2 (plus:P (match_dup 0)
			    (match_operand:P 3 "int_reg_operand" ""))))]
  "TARGET_P8_FUSION"
  "li %0,%1\t\t\t# vector load fusion\;lx<VSX_M2:VSm>x %x2,%0,%3"  
  [(set_attr "length" "8")
   (set_attr "type" "vecload")])

(define_peephole
  [(set (match_operand:P 0 "base_reg_operand" "")
	(match_operand:P 1 "short_cint_operand" ""))
   (set (match_operand:VSX_M2 2 "vsx_register_operand" "")
	(mem:VSX_M2 (plus:P (match_operand:P 3 "int_reg_operand" "")
			    (match_dup 0))))]
  "TARGET_P8_FUSION"
  "li %0,%1\t\t\t# vector load fusion\;lx<VSX_M2:VSm>x %x2,%0,%3"  
  [(set_attr "length" "8")
   (set_attr "type" "vecload")])
