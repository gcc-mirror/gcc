;; C-SKY FPU instruction descriptions.
;; Copyright (C) 2018-2024 Free Software Foundation, Inc.
;; Contributed by C-SKY Microsystems and Mentor Graphics.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.  */

(define_c_enum "unspec" [
  UNSPEC_FLOOR
  UNSPEC_CEIL
  UNSPEC_BTRUNC
  UNSPEC_RINT
])

(define_c_enum "unspecv" [
  VUNSPEC_GET_FCR     ; Represent fetch of FCR content.
  VUNSPEC_SET_FCR     ; Represent assign of FCR content.
  VUNSPEC_INS_FCR     ; Represent insert of FCR content.
])

(define_mode_iterator F3ANY [HF SF DF])
(define_mode_attr f3t [(HF "16") (SF "32") (DF "64")])

(define_mode_iterator SFDF [SF DF])
(define_mode_attr f2t [(SF "32") (DF "64")])

(define_code_iterator FCMPZ [ne ge lt gt le])
(define_code_attr zero_inst [(ne "nez") (ge "hsz") (lt "ltz") (gt "hz") (le "lsz")])

(define_code_iterator FCMP [ne ge lt])
(define_code_attr reg_inst [(ne "ne") (ge "hs") (lt "lt")])

(define_code_iterator FIX_SU [fix unsigned_fix])
(define_code_attr fixsuop [(fix "")  (unsigned_fix "uns")])
(define_code_attr fixsu   [(fix "s") (unsigned_fix "u")])

(define_code_iterator FLOAT_SU [float unsigned_float])
(define_code_attr floatsuop [(float "")  (unsigned_float "uns")])
(define_code_attr floatsu   [(float "s") (unsigned_float "u")])

(define_int_iterator FRM  [UNSPEC_FLOOR
			   UNSPEC_CEIL UNSPEC_RINT])

(define_int_iterator FRMF [UNSPEC_FLOOR
			   UNSPEC_CEIL UNSPEC_BTRUNC])

(define_int_attr frm_pattern [(UNSPEC_FLOOR "floor")
			      (UNSPEC_CEIL "ceil")   (UNSPEC_BTRUNC "btrunc")
			      (UNSPEC_RINT "rint")])

(define_int_attr rm [(UNSPEC_FLOOR ".rni")
		     (UNSPEC_CEIL ".rpi")  (UNSPEC_BTRUNC ".rz")
		     (UNSPEC_RINT "")])


;; -------------------------------------------------------------------------
;; Float mov instructions
;; -------------------------------------------------------------------------

(define_expand "movhf"
  [(set (match_operand:HF 0 "general_operand" "")
	(match_operand:HF 1 "general_operand" ""))]
  "CSKY_ISA_FEATURE(fpv3_hf)"
  "
  {
    if (GET_CODE(operands[0]) == MEM && can_create_pseudo_p ())
      {
	operands[1] = force_reg (HFmode, operands[1]);
      }
  }
")

(define_expand "mov<mode>"
  [(set (match_operand:SFDF 0 "general_operand" "")
	(match_operand:SFDF 1 "general_operand" ""))]
  "CSKY_ISA_FEATURE(fpv2_<mode>)
   || CSKY_ISA_FEATURE(fpv3_<mode>)"
  "
  {
    if (GET_CODE(operands[0]) == MEM && can_create_pseudo_p ())
      {
	operands[1] = force_reg (<MODE>mode, operands[1]);
      }
  }
")

;; Move float value with general register.

(define_insn "*e2_movsf"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=b,r,r,r, m")
       (match_operand:SF 1 "general_operand"      " b,r,m,mF,r"))]
  "CSKY_ISA_FEATURE (E2)
   && !CSKY_ISA_FEATURE (fpv2_sf)
   && !CSKY_ISA_FEATURE (fpv3_sf)"
  "* return csky_output_move (insn, operands, SFmode);"
 [(set_attr "length" "2,4,4,4,4")
  (set_attr "type" "alu,alu,load,load,store")]
)

(define_insn "*e2_movdf"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=b,r,r,r, m")
       (match_operand:DF 1 "general_operand"      " b,r,m,mF,r"))]
  "CSKY_ISA_FEATURE (E2)
   && !CSKY_ISA_FEATURE (fpv2_df)
   && !CSKY_ISA_FEATURE (fpv3_df)"
  "* return csky_output_movedouble (operands, DFmode);"
 [(set_attr "length" "4,8,8,8,8")
  (set_attr "type" "alu,alu,load,load,store")]
)

(define_insn "*e1_movsf"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=r,r,r, m")
	(match_operand:SF 1 "general_operand"      " r,m,mF,r"))]
  "CSKY_ISA_FEATURE (E1)"
  "* return csky_output_ck801_move (insn, operands, SFmode);"
  [(set_attr "length" "2,4,4,4")
   (set_attr "type" "alu,load,load,store")]
)

(define_insn "*e1_movdf"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=r,r,r, m")
	(match_operand:DF 1 "general_operand"      " r,m,mF,r"))]
  "CSKY_ISA_FEATURE (E1)"
  "* return csky_output_ck801_movedouble (operands, DFmode);"
  [(set_attr "length" "4,8,8,8")
   (set_attr "type" "alu,load,load,store")]
)

;; -------------------------------------------------------------------------
;; Float Mul instructions
;; -------------------------------------------------------------------------

(define_expand "mulhf3"
  [(set (match_operand:HF	    0 "register_operand" "=v")
	(mult:HF (match_operand:HF   1 "register_operand" "v")
		 (match_operand:HF   2 "register_operand" "v")))]
  "CSKY_ISA_FEATURE(fpv3_hf)"
  "")

(define_expand "mul<mode>3"
  [(set (match_operand:SFDF	    0 "register_operand" "=v")
	(mult:SFDF (match_operand:SFDF   1 "register_operand" "v")
		 (match_operand:SFDF   2 "register_operand" "v")))]
  "CSKY_ISA_FEATURE(fpv2_<mode>)
  || CSKY_ISA_FEATURE(fpv3_<mode>)"
  "")

(define_expand "fma<mode>4"
  [(set (match_operand:F3ANY	    0 "register_operand" "=v")
	(fma:F3ANY (match_operand:F3ANY  1 "register_operand" "v")
		   (match_operand:F3ANY  2 "register_operand" "v")
		   (match_operand:F3ANY  3 "register_operand" "0")))]
  "CSKY_ISA_FEATURE(fpv3_<mode>)"
  "")

;; -------------------------------------------------------------------------
;; Float ADD SUB NEG ABS instructions
;; -------------------------------------------------------------------------

(define_expand "addhf3"
  [(set (match_operand:HF	   0 "register_operand" "")
	(plus:HF (match_operand:HF  1 "register_operand" "")
		 (match_operand:HF  2 "register_operand" "")))]
  "CSKY_ISA_FEATURE(fpv3_hf)"
  ""
)

(define_expand "add<mode>3"
  [(set (match_operand:SFDF	     0 "register_operand" "")
	(plus:SFDF (match_operand:SFDF  1 "register_operand" "")
		   (match_operand:SFDF  2 "register_operand" "")))]
  "CSKY_ISA_FEATURE(fpv2_<mode>) || CSKY_ISA_FEATURE(fpv3_<mode>)"
  ""
)

(define_expand "subhf3"
  [(set (match_operand:HF	    0 "register_operand" "")
	(minus:HF (match_operand:HF  1 "register_operand" "")
		  (match_operand:HF  2 "register_operand" "")))]
  "CSKY_ISA_FEATURE(fpv3_hf)"
  ""
)

(define_expand "sub<mode>3"
  [(set (match_operand:SFDF	      0 "register_operand" "")
	(minus:SFDF (match_operand:SFDF  1 "register_operand" "")
		    (match_operand:SFDF  2 "register_operand" "")))]
  "CSKY_ISA_FEATURE(fpv2_<mode>) || CSKY_ISA_FEATURE(fpv3_<mode>)"
  ""
)

(define_expand "abshf2"
  [(set (match_operand:HF	   0 "register_operand" "")
	(abs:HF (match_operand:HF   1 "register_operand" "")))]
  "CSKY_ISA_FEATURE(fpv3_hf)"
  ""
)

(define_expand "abs<mode>2"
  [(set (match_operand:SFDF	     0 "register_operand" "")
	(abs:SFDF (match_operand:SFDF   1 "register_operand" "")))]
  "CSKY_ISA_FEATURE(fpv2_<mode>) || CSKY_ISA_FEATURE(fpv3_<mode>)"
  ""
)

(define_expand "neghf2"
  [(set (match_operand:HF	   0 "register_operand" "")
	(neg:HF (match_operand:HF   1 "register_operand" "")))]
  "CSKY_ISA_FEATURE(fpv3_hf)"
  ""
)

(define_expand "neg<mode>2"
  [(set (match_operand:SFDF	   0 "register_operand" "")
	(neg:SFDF (match_operand:SFDF 1 "register_operand" "")))]
  "CSKY_ISA_FEATURE(fpv2_<mode>) || CSKY_ISA_FEATURE(fpv3_<mode>)"
  ""
)

(define_expand "sqrthf2"
  [(set (match_operand:HF	   0 "register_operand" "")
	(sqrt:HF (match_operand:HF  1 "register_operand" "")))]
  "CSKY_ISA_FEATURE(fpv3_hf)"
  ""
)

(define_expand "sqrt<mode>2"
  [(set (match_operand:SFDF	    0 "register_operand" "")
	(sqrt:SFDF (match_operand:SFDF 1 "register_operand" "")))]
  "CSKY_ISA_FEATURE(fpv2_<mode>) || CSKY_ISA_FEATURE(fpv3_<mode>)"
  ""
)

;; -------------------------------------------------------------------------
;; Float div instructions
;; -------------------------------------------------------------------------

(define_expand "div<mode>3"
  [(set (match_operand:SFDF	   0 "register_operand" "")
	(div:SFDF (match_operand:SFDF 1 "csky_arith_float1_operand" "")
		  (match_operand:SFDF 2 "register_operand" "")))]
  "CSKY_ISA_FEATURE(fpv2_<mode>) || CSKY_ISA_FEATURE(fpv3_<mode>)"
  "")

(define_expand "divhf3"
 [(set (match_operand:HF 0 "register_operand" "")
       (div:HF (match_operand:HF 1 "csky_arith_float1_operand" "")
	       (match_operand:HF 2 "register_operand" "")))]
 "CSKY_ISA_FEATURE(fpv3_hf)"
 "")

;; -------------------------------------------------------------------------
;; Float compare instructions
;; -------------------------------------------------------------------------

(define_expand "cbranch<mode>4"
  [(set (pc) (if_then_else (match_operator 0 "csky_float_comparison_operator"
			    [(match_operand:SFDF 1 "register_operand")
			     (match_operand:SFDF 2 "csky_compare_operand_float")])
			   (label_ref (match_operand 3 ""))
			   (pc)))]
"CSKY_ISA_FEATURE(fpv2_<mode>) || CSKY_ISA_FEATURE(fpv3_<mode>)"
"{
  enum rtx_code code = GET_CODE (operands[0]);
  bool invert;

  invert = csky_emit_compare_float (code, operands[1], operands[2]);

  if (invert)
    emit_jump_insn (gen_csky_jbf (operands[3]));
  else
    emit_jump_insn (gen_csky_jbt (operands[3]));

  DONE;

}")

(define_expand "cbranchhf4"
  [(set (pc) (if_then_else (match_operator 0 "csky_float_comparison_operator"
			    [(match_operand:HF 1 "register_operand")
			     (match_operand:HF 2 "csky_compare_operand_float")])
			   (label_ref (match_operand 3 ""))
			   (pc)))]
"CSKY_ISA_FEATURE(fpv3_hf)"
"{
  enum rtx_code code = GET_CODE (operands[0]);
  bool invert;

  invert = csky_emit_compare_float (code, operands[1], operands[2]);

  if (invert)
    emit_jump_insn (gen_csky_jbf (operands[3]));
  else
    emit_jump_insn (gen_csky_jbt (operands[3]));

  DONE;

}")

;; -------------------------------------------------------------------------
;; Instructions for float cstore
;; -------------------------------------------------------------------------

(define_expand "cstore<mode>4"
  [(set (match_operand:SI 0 "register_operand" "")
	(match_operator   1 "csky_float_comparison_operator"
	  [(match_operand:SFDF 2 "register_operand" "")
	   (match_operand:SFDF 3 "csky_compare_operand_float" "")]))]
  "CSKY_ISA_FEATURE (fpv2_<mode>) || CSKY_ISA_FEATURE(fpv3_<mode>)"
  "{
    bool invert;

    invert = csky_emit_compare_float (GET_CODE (operands[1]),
				 operands[2], operands[3]);
    if(invert)
      emit_insn (gen_mvcv (operands[0]));
    else
      emit_insn (gen_mvc (operands[0]));
    DONE;
  }"
)

(define_expand "cstorehf4"
  [(set (match_operand:SI 0 "register_operand" "")
	(match_operator   1 "csky_float_comparison_operator"
	  [(match_operand:HF 2 "register_operand" "")
	   (match_operand:HF 3 "csky_compare_operand_float" "")]))]
  "CSKY_ISA_FEATURE(fpv3_hf)"
  "{
    bool invert;

    invert = csky_emit_compare_float (GET_CODE (operands[1]),
				 operands[2], operands[3]);
    if(invert)
      emit_insn (gen_mvcv (operands[0]));
    else
      emit_insn (gen_mvc (operands[0]));
    DONE;
  }"
)

;; -------------------------------------------------------------------------
;; Float convert instructions
;; -------------------------------------------------------------------------

;; SF <- HF
(define_expand "extendhfsf2"
  [(set (match_operand:SF		  0 "register_operand" "")
	(float_extend:SF (match_operand:HF 1 "register_operand" "")))]
  "CSKY_ISA_FEATURE(fpv3_hf)"
  "")

;; HF <- SF
(define_expand "truncsfhf2"
  [(set (match_operand:HF		     0 "register_operand" "")
	(float_truncate:HF (match_operand:SF 1 "register_operand" "")))]
  "CSKY_ISA_FEATURE(fpv3_hf)"
  "")

;; DF <- SF
(define_expand "extendsfdf2"
  [(set (match_operand:DF		  0 "register_operand" "")
	(float_extend:DF (match_operand:SF 1 "register_operand" "")))]
  "CSKY_ISA_FEATURE(fpv2_df) || CSKY_ISA_FEATURE(fpv3_df)"
  "")

;; SF <- DF
(define_expand "truncdfsf2"
  [(set (match_operand:SF		    0 "register_operand" "")
	(float_truncate:SF (match_operand:DF 1 "register_operand" "")))]
  "CSKY_ISA_FEATURE(fpv2_df) || CSKY_ISA_FEATURE(fpv3_df)"
  "")

;; HF <- unsigned SI,SI
(define_expand "float<floatsuop>sihf2"
  [(set (match_operand:HF	   0 "register_operand" "")
	(FLOAT_SU:HF (match_operand:SI 1 "register_operand" "")))]
  "CSKY_ISA_FEATURE(fpv3_hf)"
  "")

;; DF,SF <- unsigned SI,SI
(define_expand "float<floatsuop>si<mode>2"
  [(set (match_operand:SFDF		    0 "register_operand" "")
	(FLOAT_SU:SFDF (match_operand:SI 1 "register_operand" "")))]
  "CSKY_ISA_FEATURE(fpv2_<mode>) || CSKY_ISA_FEATURE(fpv3_<mode>)"
  "")

;; HF <- unsigned HI,HI
(define_expand "float<floatsuop>hihf2"
  [(set (match_operand:HF	   0 "register_operand" "")
	(FLOAT_SU:HF (match_operand:HI 1 "register_operand" "")))]
  "CSKY_ISA_FEATURE(fpv3_hi) && CSKY_ISA_FEATURE(fpv3_hf)"
  "")

;; unsigned SI,SI <- HF
(define_expand "fix<fixsuop>_trunchfsi2"
  [(set (match_operand:SI	    0 "register_operand" "")
	(FIX_SU:SI (fix:HF (match_operand:HF 1 "register_operand" ""))))]
  "CSKY_ISA_FEATURE(fpv3_hf)"
  "")

;; unsigned SI,SI <- DF,SF
(define_expand "fix<fixsuop>_trunc<mode>si2"
  [(set (match_operand:SI	    0 "register_operand" "")
	(FIX_SU:SI (fix:SFDF (match_operand:SFDF 1 "register_operand" ""))))]
  "CSKY_ISA_FEATURE(fpv2_<mode>) || CSKY_ISA_FEATURE(fpv3_<mode>)"
  "")

(include "csky_insn_fpuv3.md")
(include "csky_insn_fpuv2.md")
