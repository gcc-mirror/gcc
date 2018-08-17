;; C-SKY FPU instruction descriptions.
;; Copyright (C) 2018 Free Software Foundation, Inc.
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

;; -------------------------------------------------------------------------
;; Float Abs instructions
;; -------------------------------------------------------------------------

(define_insn "abssf2"
  [(set (match_operand:SF	  0 "register_operand" "=v,r")
	(abs:SF (match_operand:SF 1 "register_operand" "v, r")))]
  "CSKY_ISA_FEATURE (fpv2_sf)"
  "@
    fabss\t%0, %1
    bclri\t%0, %1, 31")

(define_insn "absdf2"
  [(set (match_operand:DF	  0 "register_operand" "=v")
	(abs:DF (match_operand:DF 1 "register_operand" "v")))]
  "CSKY_ISA_FEATURE (fpv2_df)"
  "fabsd\t%0, %1")


;; -------------------------------------------------------------------------
;; Float Neg instructions
;; -------------------------------------------------------------------------

(define_insn "negsf2"
  [(set (match_operand:SF	  0 "register_operand" "=v")
	(neg:SF (match_operand:SF 1 "register_operand" "v")))]
  "CSKY_ISA_FEATURE (fpv2_sf)"
  "fnegs\t%0, %1")

(define_insn "negdf2"
  [(set (match_operand:DF	  0 "register_operand" "=v")
	(neg:DF (match_operand:DF 1 "register_operand" "v")))]
  "CSKY_ISA_FEATURE (fpv2_df)"
  "fnegd\t%0, %1")


;; -------------------------------------------------------------------------
;; Float Sqrt instructions
;; -------------------------------------------------------------------------

(define_insn "sqrtsf2"
  [(set (match_operand:SF	   0 "register_operand" "=v")
	(sqrt:SF (match_operand:SF 1 "register_operand" "v")))]
  "CSKY_ISA_FEATURE (fpv2_sf)"
  "fsqrts\t%0, %1")

(define_insn "sqrtdf2"
  [(set (match_operand:DF	   0 "register_operand" "=v")
	(sqrt:DF (match_operand:DF 1 "register_operand" "v")))]
 "CSKY_ISA_FEATURE (fpv2_divd)"
 "fsqrtd\t%0, %1")


;; -------------------------------------------------------------------------
;; Float Add instructions
;; -------------------------------------------------------------------------

(define_insn "addsf3"
  [(set (match_operand:SF	   0 "register_operand" "=v")
	(plus:SF (match_operand:SF 1 "register_operand" "v")
		 (match_operand:SF 2 "register_operand" "v")))]
  "CSKY_ISA_FEATURE (fpv2_sf)"
  "fadds\t%0, %1, %2")

(define_insn "adddf3"
  [(set (match_operand:DF	   0 "register_operand" "=v")
	(plus:DF (match_operand:DF 1 "register_operand" "v")
		 (match_operand:DF 2 "register_operand" "v")))]
 "CSKY_ISA_FEATURE (fpv2_df)"
 "faddd\t%0, %1, %2")


;; -------------------------------------------------------------------------
;; Float Sub instructions
;; -------------------------------------------------------------------------

(define_insn "subsf3"
  [(set (match_operand:SF	    0 "register_operand" "=v")
	(minus:SF (match_operand:SF 1 "register_operand" "v")
		  (match_operand:SF 2 "register_operand" "v")))]
  "CSKY_ISA_FEATURE (fpv2_sf)"
  "fsubs\t%0, %1, %2")

(define_insn "subdf3"
  [(set (match_operand:DF	    0 "register_operand" "=v")
	(minus:DF (match_operand:DF 1 "register_operand" "v")
		  (match_operand:DF 2 "register_operand" "v")))]
  "CSKY_ISA_FEATURE (fpv2_df)"
  "fsubd\t%0, %1, %2")


;; -------------------------------------------------------------------------
;; Float Mul instructions
;; -------------------------------------------------------------------------

(define_insn "mulsf3"
  [(set (match_operand:SF	   0 "register_operand" "=v")
	(mult:SF (match_operand:SF 1 "register_operand" "v")
		 (match_operand:SF 2 "register_operand" "v")))]
  "CSKY_ISA_FEATURE (fpv2_sf)"
  "fmuls\t%0, %1, %2")

(define_insn "muldf3"
  [(set (match_operand:DF	   0 "register_operand" "=v")
	(mult:DF (match_operand:DF 1 "register_operand" "v")
		 (match_operand:DF 2 "register_operand" "v")))]
  "CSKY_ISA_FEATURE (fpv2_df)"
  "fmuld\t%0, %1, %2")

(define_insn "*fpuv2_nmulsf3_1"
  [(set (match_operand:SF		   0 "register_operand" "=v")
	(mult:SF (neg:SF (match_operand:SF 1 "register_operand" "%v"))
		 (match_operand:SF	   2 "register_operand" "v")))]
  "CSKY_ISA_FEATURE (fpv2_sf)"
  "fnmuls\t%0, %1, %2")

(define_insn "*fpuv2_nmulsf3_2"
  [(set (match_operand:SF		   0 "register_operand" "=v")
	(neg:SF (mult:SF (match_operand:SF 1 "register_operand" "v")
			 (match_operand:SF 2 "register_operand" "v"))))]
  "CSKY_ISA_FEATURE (fpv2_sf)"
  "fnmuls\t%0, %1, %2")

(define_insn "*fpuv2_nmuldf3_1"
  [(set (match_operand:DF		   0 "register_operand" "=v")
	(mult:DF (neg:DF (match_operand:DF 1 "register_operand" "%v"))
		 (match_operand:DF	   2 "register_operand" "v")))]
 "CSKY_ISA_FEATURE (fpv2_df)"
 "fnmuld\t%0, %1, %2")

(define_insn "*fpuv2_nmuldf3_2"
  [(set (match_operand:DF		   0 "register_operand" "=v")
	(neg:DF (mult:DF (match_operand:DF 1 "register_operand" "v")
			 (match_operand:DF 2 "register_operand" "v"))))]
  "CSKY_ISA_FEATURE (fpv2_df)"
  "fnmuld\t%0, %1, %2")


;; -------------------------------------------------------------------------
;; Float Div instructions
;; -------------------------------------------------------------------------

(define_expand "divsf3"
  [(set (match_operand:SF	  0 "register_operand" "")
	(div:SF (match_operand:SF 1 "csky_arith_float1_operand" "")
		(match_operand:SF 2 "register_operand" "")))]
  "CSKY_ISA_FEATURE (fpv2_sf)"
  "")

(define_insn "*fpuv2_divsf3"
  [(set (match_operand:SF	  0 "register_operand" "=v")
	(div:SF (match_operand:SF 1 "register_operand" "v")
		(match_operand:SF 2 "register_operand" "v")))]
  "CSKY_ISA_FEATURE (fpv2_sf)"
  "fdivs\t%0, %1, %2")

(define_insn "*fpuv2_1_divsf3"
  [(set (match_operand:SF	  0 "register_operand"		"=v")
	(div:SF (match_operand:SF 1 "csky_const_float1_operand" "i")
		(match_operand:SF 2 "register_operand"		"v")))]
  "CSKY_ISA_FEATURE (fpv2_sf)"
  "frecips\t%0, %2")


(define_expand "divdf3"
  [(set (match_operand:DF 0 "register_operand" "")
	(div:DF (match_operand:DF 1 "csky_arith_float1_operand" "")
		(match_operand:DF 2 "register_operand" "")))]
  "CSKY_ISA_FEATURE (fpv2_divd)"
  "")

(define_insn "*fpuv2_divdf3"
  [(set (match_operand:DF	  0 "register_operand" "=v")
	(div:DF (match_operand:DF 1 "register_operand" "v")
		(match_operand:DF 2 "register_operand" "v")))]
  "CSKY_ISA_FEATURE (fpv2_divd)"
  "fdivd\t%0, %1, %2")

(define_insn "*fpuv2_1_divdf3"
  [(set (match_operand:DF	  0 "register_operand"		"=v")
	(div:DF (match_operand:DF 1 "csky_const_float1_operand" "i")
		(match_operand:DF 2 "register_operand"		"v")))]
  "CSKY_ISA_FEATURE (fpv2_divd)"
  "frecipd\t%0, %2")


;; -------------------------------------------------------------------------
;; Float add(sub) with mult instructions
;; -------------------------------------------------------------------------

;; vrz <= vrz + vrx * vry
(define_insn "*fpuv2_fmacs"
  [(set (match_operand:SF		    0 "register_operand" "=v")
	(plus:SF (mult:SF (match_operand:SF 1 "register_operand" "v")
			  (match_operand:SF 2 "register_operand" "v"))
		 (match_operand:SF	    3 "register_operand" "0")))]
  "CSKY_ISA_FEATURE (fpv2_sf)"
  "fmacs\t%0, %1, %2")

(define_insn "*fpuv2_fmacd"
  [(set (match_operand:DF		    0 "register_operand" "=v")
	(plus:DF (mult:DF (match_operand:DF 1 "register_operand" "v")
			  (match_operand:DF 2 "register_operand" "v"))
		 (match_operand:DF	    3 "register_operand" "0")))]
  "CSKY_ISA_FEATURE (fpv2_df)"
  "fmacd\t%0, %1, %2")

;; vrz <= vrz - vrx * vry
(define_insn "*fpuv2_fnmacs"
  [(set (match_operand:SF		     0 "register_operand" "=v")
	(minus:SF (match_operand:SF	     1 "register_operand" "0")
		  (mult:SF (match_operand:SF 2 "register_operand" "v")
			   (match_operand:SF 3 "register_operand" "v"))))]
  "CSKY_ISA_FEATURE (fpv2_sf)"
  "fnmacs\t%0, %2, %3")

(define_insn "*fpuv2_fnmacd"
  [(set (match_operand:DF		     0 "register_operand" "=v")
	(minus:DF (match_operand:DF	     1 "register_operand" "0")
		  (mult:DF (match_operand:DF 2 "register_operand" "v")
			   (match_operand:DF 3 "register_operand" "v"))))]
  "CSKY_ISA_FEATURE (fpv2_df)"
  "fnmacd\t%0, %2, %3")

;; vrz <= vrx * vry - vrz
(define_insn "*fpuv2_fmscs"
  [(set (match_operand:SF		     0 "register_operand" "=v")
	(minus:SF (mult:SF (match_operand:SF 1 "register_operand" "v")
			   (match_operand:SF 2 "register_operand" "v"))
		  (match_operand:SF	     3 "register_operand" "0")))]
  "CSKY_ISA_FEATURE (fpv2_sf)"
  "fmscs\t%0, %1, %2")

(define_insn "*fpuv2_fmscd"
  [(set (match_operand:DF 0 "register_operand" "=v")
	(minus:DF (mult:DF (match_operand:DF 1 "register_operand" "v")
			   (match_operand:DF 2 "register_operand" "v"))
		  (match_operand:DF 3 "register_operand" "0")))]
  "CSKY_ISA_FEATURE (fpv2_df)"
  "fmscd\t%0, %1, %2")

;; vrz = - (vrz + vrx * vry)
(define_insn "*fpuv2_fnmscs_1"
  [(set (match_operand:SF			     0 "register_operand" "=v")
	(minus:SF (mult:SF (neg:SF (match_operand:SF 1 "register_operand" "%v"))
			   (match_operand:SF	     2 "register_operand" "v"))
		  (match_operand:SF		     3 "register_operand" "0")))]
  "CSKY_ISA_FEATURE (fpv2_sf)"
  "fnmscs\t%0, %1, %2")

(define_insn "*fpuv2_fnmscs_2"
  [(set (match_operand:SF			    0 "register_operand" "=v")
	(neg:SF (plus:SF (mult:SF (match_operand:SF 1 "register_operand" "v")
				  (match_operand:SF 2 "register_operand" "v"))
			 (match_operand:SF	    3 "register_operand" "0"))))]
  "CSKY_ISA_FEATURE (fpv2_sf)"
  "fnmscs\t%0, %1, %2")

(define_insn "*fpuv2_fnmscd_1"
  [(set (match_operand:DF 0 "register_operand" "=v")
	(minus:DF (mult:DF (neg:DF (match_operand:DF 1 "register_operand" "%v"))
			   (match_operand:DF 2 "register_operand" "v"))
		  (match_operand:DF 3 "register_operand" "0")))]
  "CSKY_ISA_FEATURE (fpv2_df)"
  "fnmscd\t%0, %1, %2")

(define_insn "*fpuv2_fnmscd_2"
  [(set (match_operand:DF 0 "register_operand" "=v")
	(neg:DF (plus:DF (mult:DF (match_operand:DF 1 "register_operand" "v")
				  (match_operand:DF 2 "register_operand" "v"))
			 (match_operand:DF 3 "register_operand" "0"))))]
  "CSKY_ISA_FEATURE (fpv2_df)"
  "fnmscd\t%0, %1, %2")


;; -------------------------------------------------------------------------
;; Float compare instructions
;; -------------------------------------------------------------------------

(define_expand "cbranchsf4"
  [(set (pc) (if_then_else (match_operator 0 "csky_float_comparison_operator"
			     [(match_operand:SF 1 "register_operand")
			      (match_operand:SF 2 "csky_compare_operand_float")])
			   (label_ref (match_operand 3 ""))
			   (pc)))]
  "CSKY_ISA_FEATURE (fpv2_sf)"
  "
  {
    enum rtx_code code = GET_CODE (operands[0]);
    bool invert = csky_emit_compare_float (code, operands[1], operands[2]);

    if (invert)
      emit_jump_insn (gen_csky_jbf (operands[3]));
    else
      emit_jump_insn (gen_csky_jbt (operands[3]));

    DONE;
  }")

(define_insn "*fpuv2_unordered"
  [(set (reg:CC 33) (unordered:CC (match_operand:SF 0 "register_operand" "v")
				  (match_operand:SF 1 "register_operand" "v")))]
  "CSKY_ISA_FEATURE (fpv2_sf)"
  "fcmpuos\t%0, %1")

(define_insn "*fpuv2_unordered_zero"
  [(set (reg:CC 33) (unordered:CC (match_operand:SF 0 "register_operand" "v")
				  (match_operand:SF 1 "csky_const_float0_operand" "i")))]
  "CSKY_ISA_FEATURE (fpv2_sf)"
  "fcmpuos\t%0, %0")

(define_insn "*fpuv2_ne"
  [(set (reg:CC 33) (ne:CC (match_operand:SF 0 "register_operand" "v")
			   (match_operand:SF 1 "register_operand" "v")))]
  "CSKY_ISA_FEATURE (fpv2_sf)"
  "fcmpnes\t%0, %1")

(define_insn "*fpuv2_gt"
  [(set (reg:CC 33) (gt:CC (match_operand:SF 0 "register_operand" "v")
			   (match_operand:SF 1 "register_operand" "v")))]
  "CSKY_ISA_FEATURE (fpv2_sf)"
  "fcmplts\t%1, %0")

(define_insn "*fpuv2_ge"
  [(set (reg:CC 33) (ge:CC (match_operand:SF 0 "register_operand" "v")
			   (match_operand:SF 1 "register_operand" "v")))]
 "CSKY_ISA_FEATURE (fpv2_sf)"
 "fcmphss\t%0, %1")

(define_insn "*fpuv2_lt"
  [(set (reg:CC 33) (lt:CC (match_operand:SF 0 "register_operand" "v")
			   (match_operand:SF 1 "register_operand" "v")))]
  "CSKY_ISA_FEATURE (fpv2_sf)"
  "fcmplts\t%0, %1")

(define_insn "*fpuv2_le"
  [(set (reg:CC 33) (le:CC (match_operand:SF 0 "register_operand" "v")
			   (match_operand:SF 1 "register_operand" "v")))]
 "CSKY_ISA_FEATURE (fpv2_sf)"
 "fcmphss\t%1, %0")

(define_insn "*fpuv2_gez"
  [(set (reg:CC 33) (ge:CC (match_operand:SF 0 "register_operand"	   "v")
			   (match_operand:SF 1 "csky_const_float0_operand" "i")))]
  "CSKY_ISA_FEATURE (fpv2_sf)"
  "fcmpzhss\t%0")

(define_insn "*fpuv2_nez"
  [(set (reg:CC 33) (ne:CC (match_operand:SF 0 "register_operand"	   "v")
			   (match_operand:SF 1 "csky_const_float0_operand" "i")))]
  "CSKY_ISA_FEATURE (fpv2_sf)"
  "fcmpznes\t%0")


(define_expand "cbranchdf4"
  [(set (pc) (if_then_else (match_operator 0 "csky_float_comparison_operator"
			     [(match_operand:DF 1 "register_operand")
			      (match_operand:DF 2 "csky_compare_operand_float")])
			   (label_ref (match_operand 3 ""))
			   (pc)))]
  "CSKY_ISA_FEATURE (fpv2_df)"
  "
  {
    enum rtx_code code = GET_CODE (operands[0]);
    bool invert = csky_emit_compare_float (code, operands[1], operands[2]);

    if (invert)
      emit_jump_insn (gen_csky_jbf (operands[3]));
    else
      emit_jump_insn (gen_csky_jbt (operands[3]));

    DONE;
}")

(define_insn "*fpuv2_dunordered"
  [(set (reg:CC 33) (unordered:CC (match_operand:DF 0 "register_operand" "v")
				  (match_operand:DF 1 "register_operand" "v")))]
  "CSKY_ISA_FEATURE (fpv2_df)"
  "fcmpuod\t%0, %1")

(define_insn "*fpuv2_dunordered_zero"
  [(set (reg:CC 33) (unordered:CC (match_operand:DF 0 "register_operand" "v")
				  (match_operand:DF 1 "csky_const_float0_operand" "i")))]
  "CSKY_ISA_FEATURE (fpv2_df)"
  "fcmpuod\t%0, %0")

(define_insn "*fpuv2_dne"
  [(set (reg:CC 33) (ne:CC (match_operand:DF 0 "register_operand" "v")
			   (match_operand:DF 1 "register_operand" "v")))]
  "CSKY_ISA_FEATURE (fpv2_df)"
  "fcmpned\t%0, %1")

(define_insn "*fpuv2_dgt"
  [(set (reg:CC 33) (gt:CC (match_operand:DF 0 "register_operand" "v")
			   (match_operand:DF 1 "register_operand" "v")))]
  "CSKY_ISA_FEATURE (fpv2_df)"
  "fcmpltd\t%1, %0")

(define_insn "*fpuv2_dge"
  [(set (reg:CC 33) (ge:CC (match_operand:DF 0 "register_operand" "v")
			   (match_operand:DF 1 "register_operand" "v")))]
  "CSKY_ISA_FEATURE (fpv2_df)"
  "fcmphsd\t%0, %1")

(define_insn "*fpuv2_dlt"
  [(set (reg:CC 33) (lt:CC (match_operand:DF 0 "register_operand" "v")
			   (match_operand:DF 1 "register_operand" "v")))]
 "CSKY_ISA_FEATURE (fpv2_df)"
 "fcmpltd\t%0, %1")

(define_insn "*fpuv2_dle"
  [(set (reg:CC 33) (le:CC (match_operand:DF 0 "register_operand" "v")
			   (match_operand:DF 1 "register_operand" "v")))]
  "CSKY_ISA_FEATURE (fpv2_df)"
  "fcmphsd\t%1, %0")

(define_insn "*fpuv2_dgez"
  [(set (reg:CC 33) (ge:CC (match_operand:DF 0 "register_operand"	   "v")
			   (match_operand:DF 1 "csky_const_float0_operand" "i")))]
 "CSKY_ISA_FEATURE (fpv2_df)"
 "fcmpzhsd\t%0")

(define_insn "*fpuv2_dnez"
  [(set (reg:CC 33) (ne:CC (match_operand:DF 0 "register_operand"	   "v")
			   (match_operand:DF 1 "csky_const_float0_operand" "i")))]
  "CSKY_ISA_FEATURE (fpv2_df)"
  "fcmpzned\t%0")


;; -------------------------------------------------------------------------
;; Float convert instructions
;; -------------------------------------------------------------------------

;; DF <- SF
(define_insn "extendsfdf2"
  [(set (match_operand:DF		   0 "register_operand" "=v")
	(float_extend:DF (match_operand:SF 1 "register_operand" "v")))]
  "CSKY_ISA_FEATURE (fpv2_df)"
  "fstod\t%0, %1")

;; SF <- DF
(define_insn "truncdfsf2"
  [(set (match_operand:SF		     0 "register_operand" "=v")
	(float_truncate:SF (match_operand:DF 1 "register_operand" "v")))]
  "CSKY_ISA_FEATURE (fpv2_df)"
  "fdtos\t%0, %1")

;; SF <- SI
(define_insn "floatsisf2"
  [(set (match_operand:SF	    0 "register_operand" "=v")
	(float:SF (match_operand:SI 1 "register_operand" "v")))]
  "CSKY_ISA_FEATURE (fpv2_sf)"
  "fsitos\t%0, %1")

;; DF <- SI
(define_insn "floatsidf2"
  [(set (match_operand:DF	    0 "register_operand" "=v")
	(float:DF (match_operand:SI 1 "register_operand" "v")))]
  "CSKY_ISA_FEATURE (fpv2_df)"
  "fsitod\t%0, %1")

;; SF <- unsigned SI
(define_insn "floatunssisf2"
  [(set (match_operand:SF		     0 "register_operand" "=v")
	(unsigned_float:SF (match_operand:SI 1 "register_operand" "v")))]
  "CSKY_ISA_FEATURE (fpv2_sf)"
  "fuitos\t%0, %1")

;; DF <- unsigned SI
(define_insn "floatunssidf2"
  [(set (match_operand:DF		     0 "register_operand" "=v")
	(unsigned_float:DF (match_operand:SI 1 "register_operand" "v")))]
  "CSKY_ISA_FEATURE (fpv2_df)"
  "fuitod\t%0, %1")

;; SI <- SF
(define_insn "fix_truncsfsi2"
  [(set (match_operand:SI	  0 "register_operand" "=v")
	(fix:SI (match_operand:SF 1 "register_operand" "v")))]
  "CSKY_ISA_FEATURE (fpv2_sf)"
  "fstosi.rz\t%0, %1")

;; SI <- DF
(define_insn "fix_truncdfsi2"
  [(set (match_operand:SI	  0 "register_operand" "=v")
	(fix:SI (match_operand:DF 1 "register_operand" "v")))]
  "CSKY_ISA_FEATURE (fpv2_df)"
  "fdtosi.rz\t%0, %1")

;; unsigned SI <- SF
(define_insn "fixuns_truncsfsi2"
  [(set (match_operand:SI		   0 "register_operand" "=v")
	(unsigned_fix:SI (match_operand:SF 1 "register_operand" "v")))]
  "CSKY_ISA_FEATURE (fpv2_sf)"
  "fstoui.rz\t%0, %1")

;; unsigned SI <- DF
(define_insn "fixuns_truncdfsi2"
  [(set (match_operand:SI		   0 "register_operand" "=v")
	(unsigned_fix:SI (match_operand:DF 1 "register_operand" "v")))]
  "CSKY_ISA_FEATURE (fpv2_df)"
  "fdtoui.rz\t%0, %1")


;; -------------------------------------------------------------------------
;; Float mov instructions
;; -------------------------------------------------------------------------

;; Note:  movsf and movdf patterns are in csky.md.

;; cstore SF
(define_expand "cstoresf4"
  [(set (match_operand:SI 0 "register_operand" "")
	(match_operator	  1 "ordered_comparison_operator"
	  [(match_operand:SF 2 "register_operand" "")
	   (match_operand:SF 3 "csky_compare_operand_float" "")]))]
  "CSKY_ISA_FEATURE (fpv2_sf)"
  "
  {
    bool invert = csky_emit_compare_float (GET_CODE (operands[1]),
					   operands[2], operands[3]);
    if (invert)
      emit_insn (gen_mvcv (operands[0]));
    else
      emit_insn (gen_mvc (operands[0]));
    DONE;
  }"
)

;; cstore DF
(define_expand "cstoredf4"
  [(set (match_operand:SI 0 "register_operand" "")
	(match_operator 1 "ordered_comparison_operator"
	  [(match_operand:DF 2 "register_operand" "")
	   (match_operand:DF 3 "csky_compare_operand_float" "")]))]
  "CSKY_ISA_FEATURE (fpv2_df)"
  "
  {
    bool invert = csky_emit_compare_float (GET_CODE (operands[1]),
					   operands[2], operands[3]);
    if (invert)
      emit_insn (gen_mvcv (operands[0]));
    else
      emit_insn (gen_mvc (operands[0]));
    DONE;
  }"
)
