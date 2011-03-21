;; Copyright (C) 2007, 2008, 2009, 2010 Free Software Foundation, Inc.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; For the internal conditional math routines:

;; operand 0 is always the result
;; operand 1 is always the predicate
;; operand 2, 3, and sometimes 4 are the input values.
;; operand 4 or 5 is the floating point status register to use.
;; operand 5 or 6 is the rounding to do. (0 = single, 1 = double, 2 = none)
;;
;; addrf3_cond   - F0 = F2 + F3
;; subrf3_cond   - F0 = F2 - F3
;; mulrf3_cond   - F0 = F2 * F3
;; nmulrf3_cond  - F0 = - (F2 * F3)
;; m1addrf4_cond - F0 = (F2 * F3) + F4
;; m1subrf4_cond - F0 = (F2 * F3) - F4
;; m2addrf4_cond - F0 = F2 + (F3 * F4)
;; m2subrf4_cond - F0 = F2 - (F3 * F4)

;; Basic plus/minus/mult operations

(define_insn "addrf3_cond"
  [(set (match_operand:RF 0 "fr_register_operand" "=f,f")
        (if_then_else:RF (ne:RF (match_operand:CCI 1 "register_operand"  "c,c")
                                (const_int 0))
          (plus:RF
            (match_operand:RF 2 "fr_reg_or_fp01_operand" "fG,fG")
            (match_operand:RF 3 "fr_reg_or_fp01_operand" "fG,fG"))
          (match_operand:RF 4 "fr_reg_or_0_operand" "0,H")))
   (use (match_operand:SI 5 "const_int_operand" ""))
   (use (match_operand:SI 6 "const_int_operand" ""))]
  ""
  "(%1) fadd%R6.s%5 %0 = %F2, %F3"
  [(set_attr "itanium_class" "fmac")
   (set_attr "predicable" "no")])

(define_insn "subrf3_cond"
  [(set (match_operand:RF 0 "fr_register_operand" "=f,f")
        (if_then_else:RF (ne:RF (match_operand:CCI 1 "register_operand"  "c,c")
                                (const_int 0))
          (minus:RF
            (match_operand:RF 2 "fr_reg_or_fp01_operand" "fG,fG")
            (match_operand:RF 3 "fr_reg_or_fp01_operand" "fG,fG"))
          (match_operand:RF 4 "fr_reg_or_0_operand" "0,H")))
   (use (match_operand:SI 5 "const_int_operand" ""))
   (use (match_operand:SI 6 "const_int_operand" ""))]
  ""
  "(%1) fsub%R6.s%5 %0 = %F2, %F3"
  [(set_attr "itanium_class" "fmac")
   (set_attr "predicable" "no")])

(define_insn "mulrf3_cond"
  [(set (match_operand:RF 0 "fr_register_operand" "=f,f")
        (if_then_else:RF (ne:RF (match_operand:CCI 1 "register_operand"  "c,c")
                                (const_int 0))
          (mult:RF
            (match_operand:RF 2 "fr_reg_or_fp01_operand" "fG,fG")
            (match_operand:RF 3 "fr_reg_or_fp01_operand" "fG,fG"))
          (match_operand:RF 4 "fr_reg_or_0_operand" "0,H")))
   (use (match_operand:SI 5 "const_int_operand" ""))
   (use (match_operand:SI 6 "const_int_operand" ""))]
  ""
  "(%1) fmpy%R6.s%5 %0 = %F2, %F3"
  [(set_attr "itanium_class" "fmac")
   (set_attr "predicable" "no")])

;; neg-mult operation

(define_insn "nmulrf3_cond"
  [(set (match_operand:RF 0 "fr_register_operand" "=f,f")
        (if_then_else:RF (ne:RF (match_operand:CCI 1 "register_operand"  "c,c")
                                (const_int 0))
          (neg:RF (mult:RF
            (match_operand:RF 2 "fr_reg_or_fp01_operand" "fG,fG")
            (match_operand:RF 3 "fr_reg_or_fp01_operand" "fG,fG")))
          (match_operand:RF 4 "fr_reg_or_0_operand" "0,H")))
   (use (match_operand:SI 5 "const_int_operand" ""))
   (use (match_operand:SI 6 "const_int_operand" ""))]
  ""
  "(%1) fnmpy%R6.s%5 %0 = %F2, %F3"
  [(set_attr "itanium_class" "fmac")
   (set_attr "predicable" "no")])

;; add-mult/sub-mult operations (mult as op1)

(define_insn "m1addrf4_cond"
  [(set (match_operand:RF 0 "fr_register_operand" "=f,f")
        (if_then_else:RF (ne:RF (match_operand:CCI 1 "register_operand"  "c,c")
                                (const_int 0))
          (plus:RF
            (mult:RF
              (match_operand:RF 2 "fr_reg_or_fp01_operand" "fG,fG")
              (match_operand:RF 3 "fr_reg_or_fp01_operand" "fG,fG"))
            (match_operand:RF 4 "fr_reg_or_fp01_operand" "fG,fG"))
          (match_operand:RF 5 "fr_reg_or_0_operand" "0,H")))
   (use (match_operand:SI 6 "const_int_operand" ""))
   (use (match_operand:SI 7 "const_int_operand" ""))]
  ""
  "(%1) fma%R7.s%6 %0 = %F2, %F3, %F4"
  [(set_attr "itanium_class" "fmac")
   (set_attr "predicable" "no")])

(define_insn "m1subrf4_cond"
  [(set (match_operand:RF 0 "fr_register_operand" "=f,f")
        (if_then_else:RF (ne:RF (match_operand:CCI 1 "register_operand"  "c,c")
                                (const_int 0))
          (minus:RF
            (mult:RF
              (match_operand:RF 2 "fr_reg_or_fp01_operand" "fG,fG")
              (match_operand:RF 3 "fr_reg_or_fp01_operand" "fG,fG"))
            (match_operand:RF 4 "fr_reg_or_fp01_operand" "fG,fG"))
          (match_operand:RF 5 "fr_reg_or_0_operand" "0,H")))
   (use (match_operand:SI 6 "const_int_operand" ""))
   (use (match_operand:SI 7 "const_int_operand" ""))]
  ""
  "(%1) fms%R7.s%6 %0 = %F2, %F3, %F4"
  [(set_attr "itanium_class" "fmac")
   (set_attr "predicable" "no")])

;; add-mult/sub-mult operations (mult as op2)

(define_insn "m2addrf4_cond"
  [(set (match_operand:RF 0 "fr_register_operand" "=f,f")
        (if_then_else:RF (ne:RF (match_operand:CCI 1 "register_operand"  "c,c")
                                (const_int 0))
          (plus:RF
            (match_operand:RF 2 "fr_reg_or_fp01_operand" "fG,fG")
            (mult:RF
              (match_operand:RF 3 "fr_reg_or_fp01_operand" "fG,fG")
              (match_operand:RF 4 "fr_reg_or_fp01_operand" "fG,fG")))
          (match_operand:RF 5 "fr_reg_or_0_operand" "0,H")))
   (use (match_operand:SI 6 "const_int_operand" ""))
   (use (match_operand:SI 7 "const_int_operand" ""))]
  ""
  "(%1) fma%R7.s%6 %0 = %F3, %F4, %F2"
  [(set_attr "itanium_class" "fmac")
   (set_attr "predicable" "no")])

(define_insn "m2subrf4_cond"
  [(set (match_operand:RF 0 "fr_register_operand" "=f,f")
        (if_then_else:RF (ne:RF (match_operand:CCI 1 "register_operand"  "c,c")
                                (const_int 0))
          (minus:RF
            (match_operand:RF 2 "fr_reg_or_fp01_operand" "fG,fG")
            (mult:RF
              (match_operand:RF 3 "fr_reg_or_fp01_operand" "fG,fG")
              (match_operand:RF 4 "fr_reg_or_fp01_operand" "fG,fG")))
          (match_operand:RF 5 "fr_reg_or_0_operand" "0,H")))
   (use (match_operand:SI 6 "const_int_operand" ""))
   (use (match_operand:SI 7 "const_int_operand" ""))]
  ""
  "(%1) fnma%R7.s%6 %0 = %F3, %F4, %F2"
  [(set_attr "itanium_class" "fmac")
   (set_attr "predicable" "no")])

;; Conversions to/from RF and SF/DF/XF
;; These conversions should not generate any code but make it possible
;; for all the instructions used to implement floating point division
;; to be written for RFmode only and to not have to handle multiple
;; modes or to have to handle a register in more than one mode.

(define_mode_iterator SDX_F [SF DF XF])

(define_insn "extend<mode>rf2"
  [(set (match_operand:RF 0 "fr_register_operand" "=f")
        (float_extend:RF (match_operand:SDX_F 1 "fr_reg_or_fp01_operand" "fG")))]
  ""
  "#"
  [(set_attr "itanium_class" "fmisc")
   (set_attr "predicable" "yes")])

(define_split
  [(set (match_operand:RF 0 "fr_register_operand" "")
        (float_extend:RF (match_operand:SDX_F 1 "fr_reg_or_fp01_operand" "")))]
   "reload_completed"
   [(set (match_dup 0) (match_dup 2))]
{
   if (operands[1] == CONST0_RTX (<MODE>mode))
     operands[2] = gen_rtx_REG (RFmode, FR_REG (0));
   else if (operands[1] == CONST1_RTX (<MODE>mode))
     operands[2] = gen_rtx_REG (RFmode, FR_REG (1));
   else
     operands[2] = gen_rtx_REG (RFmode, REGNO (operands[1]));
})


(define_insn "truncrf<mode>2"
  [(set (match_operand:SDX_F 0 "fr_register_operand" "=f")
        (float_truncate:SDX_F (match_operand:RF 1 "fr_reg_or_fp01_operand" "fG")))]
  ""
  "#"
  [(set_attr "itanium_class" "fmisc")
   (set_attr "predicable" "yes")])

(define_split
  [(set (match_operand:SDX_F 0 "fr_register_operand" "")
        (float_truncate:SDX_F (match_operand:RF 1 "fr_reg_or_fp01_operand" "")))]
   "reload_completed"
   [(set (match_dup 0) (match_dup 2))]
{
   if (operands[1] == CONST0_RTX (RFmode))
     operands[2] = gen_rtx_REG (<MODE>mode, FR_REG (0));
   else if (operands[1] == CONST1_RTX (RFmode))
     operands[2] = gen_rtx_REG (<MODE>mode, FR_REG (1));
   else
     operands[2] = gen_rtx_REG (<MODE>mode, REGNO (operands[1]));
})

;; Float to integer truncations using an alternative status register. 

(define_insn "fix_truncrfdi2_alts"
  [(set (match_operand:DI 0 "fr_register_operand" "=f")
        (fix:DI (match_operand:RF 1 "fr_register_operand" "f")))
   (use (match_operand:SI 2 "const_int_operand" ""))]
  ""
  "fcvt.fx.trunc.s%2 %0 = %1"
  [(set_attr "itanium_class" "fcvtfx")])

(define_insn "fixuns_truncrfdi2_alts"
  [(set (match_operand:DI 0 "fr_register_operand" "=f")
        (unsigned_fix:DI (match_operand:RF 1 "fr_register_operand" "f")))
   (use (match_operand:SI 2 "const_int_operand" ""))]
  ""
  "fcvt.fxu.trunc.s%2 %0 = %1"
  [(set_attr "itanium_class" "fcvtfx")])

(define_insn "setf_exp_rf"
  [(set (match_operand:RF 0 "fr_register_operand" "=f")
        (unspec:RF [(match_operand:DI 1 "register_operand" "r")]
                  UNSPEC_SETF_EXP))]
  ""
  "setf.exp %0 = %1"
  [(set_attr "itanium_class" "frfr")])

;; Reciprocal approximation

(define_insn "recip_approx_rf"
  [(set (match_operand:RF 0 "fr_register_operand" "=f")
        (unspec:RF [(match_operand:RF 1 "fr_reg_or_fp01_operand" "fG")
		    (match_operand:RF 2 "fr_reg_or_fp01_operand" "fG")]
		   UNSPEC_FR_RECIP_APPROX_RES))
   (set (match_operand:CCI 3 "register_operand" "=c")
        (unspec:CCI [(match_dup 1) (match_dup 2)] UNSPEC_FR_RECIP_APPROX))
   (use (match_operand:SI 4 "const_int_operand" ""))]
  ""
  "frcpa.s%4 %0, %3 = %F1, %F2"
  [(set_attr "itanium_class" "fmisc")
   (set_attr "predicable" "no")])

;; Single precision floating point division

(define_expand "divsf3"
  [(set (match_operand:SF 0 "fr_register_operand" "")
	(div:SF (match_operand:SF 1 "fr_reg_or_fp01_operand" "")
		(match_operand:SF 2 "fr_reg_or_fp01_operand" "")))]
  "TARGET_INLINE_FLOAT_DIV"
{
  rtx insn;
  if (TARGET_INLINE_FLOAT_DIV == INL_MIN_LAT)
    insn = gen_divsf3_internal_lat (operands[0], operands[1], operands[2]);
  else
    insn = gen_divsf3_internal_thr (operands[0], operands[1], operands[2]);
  emit_insn (insn);
  DONE;
})

;; Single precision floating point division (maximum throughput algorithm).

(define_expand "divsf3_internal_thr"
  [(set (match_operand:SF 0 "fr_register_operand" "")
        (div:SF (match_operand:SF 1 "fr_reg_or_fp01_operand" "")
                (match_operand:SF 2 "fr_reg_or_fp01_operand" "")))]
  "TARGET_INLINE_FLOAT_DIV"
{
  rtx y     = gen_reg_rtx (RFmode);
  rtx a     = gen_reg_rtx (RFmode);
  rtx b     = gen_reg_rtx (RFmode);
  rtx e     = gen_reg_rtx (RFmode);
  rtx y1    = gen_reg_rtx (RFmode);
  rtx y2    = gen_reg_rtx (RFmode);
  rtx q     = gen_reg_rtx (RFmode);
  rtx r     = gen_reg_rtx (RFmode);
  rtx q_res = gen_reg_rtx (RFmode);
  rtx cond  = gen_reg_rtx (CCImode);
  rtx zero    = CONST0_RTX (RFmode);
  rtx one     = CONST1_RTX (RFmode);
  rtx status0 = CONST0_RTX (SImode);
  rtx status1 = CONST1_RTX (SImode);
  rtx trunc_sgl = CONST0_RTX (SImode);
  rtx trunc_off    = CONST2_RTX (SImode);

  /* Empty conversions to put inputs into RFmode.  */
  emit_insn (gen_extendsfrf2 (a, operands[1]));
  emit_insn (gen_extendsfrf2 (b, operands[2]));
  /* y = 1 / b				*/
  emit_insn (gen_recip_approx_rf (y, a, b, cond, status0));
  /* e = 1 - (b * y)			*/
  emit_insn (gen_m2subrf4_cond (e, cond, one, b, y, zero, status1, trunc_off));
  /* y1 = y + (y * e)			*/
  emit_insn (gen_m2addrf4_cond (y1, cond, y, y, e, zero, status1, trunc_off));
  /* y2 = y + (y1 * e)			*/
  emit_insn (gen_m2addrf4_cond (y2, cond, y, y1, e, zero, status1, trunc_off));
  /* q = single(a * y2)			*/
  emit_insn (gen_mulrf3_cond (q, cond, a, y2, zero, status1, trunc_sgl));
  /* r = a - (q * b)			*/
  emit_insn (gen_m2subrf4_cond (r, cond, a, q, b, zero, status1, trunc_off));
  /* Q = single (q + (r * y2))		*/
  emit_insn (gen_m2addrf4_cond (q_res, cond, q, r, y2, y, status0, trunc_sgl));
  /* Conversion back into SFmode.	*/
  emit_insn (gen_truncrfsf2 (operands[0], q_res));
  DONE;
})

;; Single precision floating point division (minimum latency algorithm).

(define_expand "divsf3_internal_lat"
  [(set (match_operand:SF 0 "fr_register_operand" "")
        (div:SF (match_operand:SF 1 "fr_reg_or_fp01_operand" "")
                (match_operand:SF 2 "fr_reg_or_fp01_operand" "")))]
  "TARGET_INLINE_FLOAT_DIV"
{
  rtx y         = gen_reg_rtx (RFmode);
  rtx a         = gen_reg_rtx (RFmode);
  rtx b         = gen_reg_rtx (RFmode);
  rtx e         = gen_reg_rtx (RFmode);
  rtx q         = gen_reg_rtx (RFmode);
  rtx e1        = gen_reg_rtx (RFmode);
  rtx y1        = gen_reg_rtx (RFmode);
  rtx q1        = gen_reg_rtx (RFmode);
  rtx r         = gen_reg_rtx (RFmode);
  rtx q_res     = gen_reg_rtx (RFmode);
  rtx cond      = gen_reg_rtx (CCImode);
  rtx zero      = CONST0_RTX (RFmode);
  rtx one       = CONST1_RTX (RFmode);
  rtx status0   = CONST0_RTX (SImode);
  rtx status1   = CONST1_RTX (SImode);
  rtx trunc_sgl = CONST0_RTX (SImode);
  rtx trunc_off = CONST2_RTX (SImode);

  /* Empty conversions to put inputs into RFmode.  */
  emit_insn (gen_extendsfrf2 (a, operands[1]));
  emit_insn (gen_extendsfrf2 (b, operands[2]));
  /* y = 1 / b				*/
  emit_insn (gen_recip_approx_rf (y, a, b, cond, status0));
  /* q = a * y				*/
  emit_insn (gen_mulrf3_cond (q, cond, a, y, zero, status1, trunc_off));
  /* e = 1 - (b * y)			*/
  emit_insn (gen_m2subrf4_cond (e, cond, one, b, y, zero, status1, trunc_off));
  /* e1 = e + (e * e)			*/
  emit_insn (gen_m2addrf4_cond (e1, cond, e, e, e, zero, status1, trunc_off));
  /* q1 = single(q + (q * e1))		*/
  emit_insn (gen_m2addrf4_cond (q1, cond, q, q, e1, zero, status1, trunc_sgl));
  /* y1 = y + (y * e1)			*/
  emit_insn (gen_m2addrf4_cond (y1, cond, y, y, e1, zero, status1, trunc_off));
  /* r = a - (q1 * b)			*/
  emit_insn (gen_m2subrf4_cond (r, cond, a, q1, b, zero, status1, trunc_off));
  /* Q = single (q1 + (r * y1))		*/
  emit_insn (gen_m2addrf4_cond (q_res, cond, q1, r, y1, y, status0, trunc_sgl));
  /* Conversion back into SFmode.	*/
  emit_insn (gen_truncrfsf2 (operands[0], q_res));
  DONE;
})

;; Double precision floating point division

(define_expand "divdf3"
  [(set (match_operand:DF 0 "fr_register_operand" "")
	(div:DF (match_operand:DF 1 "fr_reg_or_fp01_operand" "")
		(match_operand:DF 2 "fr_reg_or_fp01_operand" "")))]
  "TARGET_INLINE_FLOAT_DIV"
{
  rtx insn;
  if (TARGET_INLINE_FLOAT_DIV == INL_MIN_LAT)
    insn = gen_divdf3_internal_lat (operands[0], operands[1], operands[2]);
  else
    insn = gen_divdf3_internal_thr (operands[0], operands[1], operands[2]);
  emit_insn (insn);
  DONE;
})

;; Double precision floating point division (maximum throughput algorithm).

(define_expand "divdf3_internal_thr"
  [(set (match_operand:DF 0 "fr_register_operand" "")
        (div:DF (match_operand:DF 1 "fr_reg_or_fp01_operand" "")
                (match_operand:DF 2 "fr_reg_or_fp01_operand" "")))]
  "TARGET_INLINE_FLOAT_DIV"
{
  rtx q_res = gen_reg_rtx (RFmode);
  rtx a     = gen_reg_rtx (RFmode);
  rtx b     = gen_reg_rtx (RFmode);
  rtx y     = gen_reg_rtx (RFmode);
  rtx e     = gen_reg_rtx (RFmode);
  rtx y1    = gen_reg_rtx (RFmode);
  rtx e1    = gen_reg_rtx (RFmode);
  rtx y2    = gen_reg_rtx (RFmode);
  rtx e2    = gen_reg_rtx (RFmode);
  rtx y3    = gen_reg_rtx (RFmode);
  rtx q     = gen_reg_rtx (RFmode);
  rtx r     = gen_reg_rtx (RFmode);
  rtx cond  = gen_reg_rtx (CCImode);
  rtx zero    = CONST0_RTX (RFmode);
  rtx one     = CONST1_RTX (RFmode);
  rtx status0 = CONST0_RTX (SImode);
  rtx status1 = CONST1_RTX (SImode);
  rtx trunc_dbl = CONST1_RTX (SImode);
  rtx trunc_off = CONST2_RTX (SImode);
  /* Empty conversions to put inputs into RFmode */
  emit_insn (gen_extenddfrf2 (a, operands[1]));
  emit_insn (gen_extenddfrf2 (b, operands[2]));
  /* y  = 1 / b			*/
  emit_insn (gen_recip_approx_rf (y, a, b, cond, status0));
  /* e  = 1 - (b * y)		*/
  emit_insn (gen_m2subrf4_cond (e, cond, one, b, y, zero, status1, trunc_off));
  /* y1 = y + (y * e)		*/
  emit_insn (gen_m2addrf4_cond (y1, cond, y, y, e, zero, status1, trunc_off));
  /* e1 = e * e			*/
  emit_insn (gen_mulrf3_cond (e1, cond, e, e, zero, status1, trunc_off));
  /* y2 = y1 + (y1 * e1)	*/
  emit_insn (gen_m2addrf4_cond (y2, cond, y1, y1, e1, zero, status1, trunc_off));
  /* e2 = e1 * e1		*/
  emit_insn (gen_mulrf3_cond (e2, cond, e1, e1, zero, status1, trunc_off));
  /* y3 = y2 + (y2 * e2)	*/
  emit_insn (gen_m2addrf4_cond (y3, cond, y2, y2, e2, zero, status1, trunc_off));
  /* q  = double (a * y3)	*/
  emit_insn (gen_mulrf3_cond (q, cond, a, y3, zero, status1, trunc_dbl));
  /* r  = a - (b * q)		*/
  emit_insn (gen_m2subrf4_cond (r, cond, a, b, q, zero, status1, trunc_off));
  /* Q  = double (q + (r * y3))	*/
  emit_insn (gen_m2addrf4_cond (q_res, cond, q, r, y3, y, status0, trunc_dbl));
  /* Conversion back into DFmode */
  emit_insn (gen_truncrfdf2 (operands[0], q_res));
  DONE;
})

;; Double precision floating point division (minimum latency algorithm).

(define_expand "divdf3_internal_lat"
  [(set (match_operand:DF 0 "fr_register_operand" "")
        (div:DF (match_operand:DF 1 "fr_reg_or_fp01_operand" "")
                (match_operand:DF 2 "fr_reg_or_fp01_operand" "")))]
  "TARGET_INLINE_FLOAT_DIV"
{
  rtx q_res     = gen_reg_rtx (RFmode);
  rtx a         = gen_reg_rtx (RFmode);
  rtx b         = gen_reg_rtx (RFmode);
  rtx y         = gen_reg_rtx (RFmode);
  rtx e         = gen_reg_rtx (RFmode);
  rtx y1        = gen_reg_rtx (RFmode);
  rtx e1        = gen_reg_rtx (RFmode);
  rtx q1        = gen_reg_rtx (RFmode);
  rtx y2        = gen_reg_rtx (RFmode);
  rtx e2        = gen_reg_rtx (RFmode);
  rtx q2        = gen_reg_rtx (RFmode);
  rtx e3        = gen_reg_rtx (RFmode);
  rtx q         = gen_reg_rtx (RFmode);
  rtx r1        = gen_reg_rtx (RFmode);
  rtx cond      = gen_reg_rtx (CCImode);
  rtx zero      = CONST0_RTX (RFmode);
  rtx one       = CONST1_RTX (RFmode);
  rtx status0   = CONST0_RTX (SImode);
  rtx status1   = CONST1_RTX (SImode);
  rtx trunc_dbl = CONST1_RTX (SImode);
  rtx trunc_off = CONST2_RTX (SImode);

  /* Empty conversions to put inputs into RFmode */
  emit_insn (gen_extenddfrf2 (a, operands[1]));
  emit_insn (gen_extenddfrf2 (b, operands[2]));
  /* y  = 1 / b			*/
  emit_insn (gen_recip_approx_rf (y, a, b, cond, status0));
  /* e  = 1 - (b * y)		*/
  emit_insn (gen_m2subrf4_cond (e, cond, one, b, y, zero, status1, trunc_off));
  /* q  = a * y                 */
  emit_insn (gen_mulrf3_cond (q, cond, a, y, zero, status1, trunc_off));
  /* e2 = e + (e * e)		*/
  emit_insn (gen_m2addrf4_cond (e2, cond, e, e, e, zero, status1, trunc_off));
  /* e1 = e * e                 */
  emit_insn (gen_mulrf3_cond (e1, cond, e, e, zero, status1, trunc_off));
  /* e3 = e + (e1 * e1)		*/
  emit_insn (gen_m2addrf4_cond (e3, cond, e, e1, e1, zero, status1, trunc_off));
  /* q1 = q + (q * e2)		*/
  emit_insn (gen_m2addrf4_cond (q1, cond, q, q, e2, zero, status1, trunc_off));
  /* y1 = y + (y * e2)		*/
  emit_insn (gen_m2addrf4_cond (y1, cond, y, y, e2, zero, status1, trunc_off));
  /* q2 = double(q + (q1 * e3))	*/
  emit_insn (gen_m2addrf4_cond (q2, cond, q, q1, e3, zero, status1, trunc_dbl));
  /* y2 = y + (y1 * e3)		*/
  emit_insn (gen_m2addrf4_cond (y2, cond, y, y1, e3, zero, status1, trunc_off));
  /* r1  = a - (b * q2)		*/
  emit_insn (gen_m2subrf4_cond (r1, cond, a, b, q2, zero, status1, trunc_off));
  /* Q  = double (q2 + (r1 * y2))	*/
  emit_insn (gen_m2addrf4_cond (q_res, cond, q2, r1, y2, y, status0, trunc_dbl));
  /* Conversion back into DFmode */
  emit_insn (gen_truncrfdf2 (operands[0], q_res));
  DONE;
})

;; Extended precision floating point division.

(define_expand "divxf3"
  [(set (match_operand:XF 0 "fr_register_operand" "")
        (div:XF (match_operand:XF 1 "fr_reg_or_fp01_operand" "")
                (match_operand:XF 2 "fr_reg_or_fp01_operand" "")))]
  "TARGET_INLINE_FLOAT_DIV"
{
  rtx q_res     = gen_reg_rtx (RFmode);
  rtx a         = gen_reg_rtx (RFmode);
  rtx b         = gen_reg_rtx (RFmode);
  rtx y         = gen_reg_rtx (RFmode);
  rtx e         = gen_reg_rtx (RFmode);
  rtx y1        = gen_reg_rtx (RFmode);
  rtx e1        = gen_reg_rtx (RFmode);
  rtx q1        = gen_reg_rtx (RFmode);
  rtx y2        = gen_reg_rtx (RFmode);
  rtx e2        = gen_reg_rtx (RFmode);
  rtx y3        = gen_reg_rtx (RFmode);
  rtx e3        = gen_reg_rtx (RFmode);
  rtx e4        = gen_reg_rtx (RFmode);
  rtx q         = gen_reg_rtx (RFmode);
  rtx r         = gen_reg_rtx (RFmode);
  rtx r1        = gen_reg_rtx (RFmode);
  rtx cond      = gen_reg_rtx (CCImode);
  rtx zero      = CONST0_RTX (RFmode);
  rtx one       = CONST1_RTX (RFmode);
  rtx status0   = CONST0_RTX (SImode);
  rtx status1   = CONST1_RTX (SImode);
  rtx trunc_off = CONST2_RTX (SImode);

  /* Empty conversions to put inputs into RFmode */
  emit_insn (gen_extendxfrf2 (a, operands[1]));
  emit_insn (gen_extendxfrf2 (b, operands[2]));
  /* y  = 1 / b			*/
  emit_insn (gen_recip_approx_rf (y, a, b, cond, status0));
  /* e  = 1 - (b * y)		*/
  emit_insn (gen_m2subrf4_cond (e, cond, one, b, y, zero, status1, trunc_off));
  /* q  = a * y                 */
  emit_insn (gen_mulrf3_cond (q, cond, a, y, zero, status1, trunc_off));
  /* e2 = e + (e * e)		*/
  emit_insn (gen_m2addrf4_cond (e2, cond, e, e, e, zero, status1, trunc_off));
  /* e1 = e * e                 */
  emit_insn (gen_mulrf3_cond (e1, cond, e, e, zero, status1, trunc_off));
  /* y1 = y + (y * e2)		*/
  emit_insn (gen_m2addrf4_cond (y1, cond, y, y, e2, zero, status1, trunc_off));
  /* e3 = e + (e1 * e1)		*/
  emit_insn (gen_m2addrf4_cond (e3, cond, e, e1, e1, zero, status1, trunc_off));
  /* y2 = y + (y1 * e3)		*/
  emit_insn (gen_m2addrf4_cond (y2, cond, y, y1, e3, zero, status1, trunc_off));
  /* r  = a - (b * q)		*/
  emit_insn (gen_m2subrf4_cond (r, cond, a, b, q, zero, status1, trunc_off));
  /* e4  = 1 - (b * y2)		*/
  emit_insn (gen_m2subrf4_cond (e4, cond, one, b, y2, zero, status1, trunc_off));
  /* q1 = q + (r * y2)		*/
  emit_insn (gen_m2addrf4_cond (q1, cond, q, r, y2, zero, status1, trunc_off));
  /* y3 = y2 + (y2 * e4)	*/
  emit_insn (gen_m2addrf4_cond (y3, cond, y2, y2, e4, zero, status1, trunc_off));
  /* r1  = a - (b * q1)		*/
  emit_insn (gen_m2subrf4_cond (r1, cond, a, b, q1, zero, status1, trunc_off));
  /* Q  = q1 + (r1 * y3)	*/
  emit_insn (gen_m2addrf4_cond (q_res, cond, q1, r1, y3, y, status0, trunc_off));
  /* Conversion back into XFmode */
  emit_insn (gen_truncrfxf2 (operands[0], q_res));
  DONE;
})


;; Integer division operations

(define_expand "divsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(div:SI (match_operand:SI 1 "general_operand" "")
		(match_operand:SI 2 "general_operand" "")))]
  "TARGET_INLINE_INT_DIV"
{
  rtx op1_rf, op2_rf, op0_rf, op0_di;

  op0_rf = gen_reg_rtx (RFmode);
  op0_di = gen_reg_rtx (DImode);

  if (! register_operand (operands[1], SImode))
    operands[1] = force_reg (SImode, operands[1]);
  op1_rf = gen_reg_rtx (RFmode);
  expand_float (op1_rf, operands[1], 0);

  if (! register_operand (operands[2], SImode))
    operands[2] = force_reg (SImode, operands[2]);
  op2_rf = gen_reg_rtx (RFmode);
  expand_float (op2_rf, operands[2], 0);

  emit_insn (gen_cond_trap (EQ, operands[2], CONST0_RTX (SImode),
			    CONST1_RTX (SImode)));
  
  emit_insn (gen_divsi3_internal (op0_rf, op1_rf, op2_rf));

  emit_insn (gen_fix_truncrfdi2_alts (op0_di, op0_rf, const1_rtx));
  emit_move_insn (operands[0], gen_lowpart (SImode, op0_di));
  DONE;
})

(define_expand "modsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(mod:SI (match_operand:SI 1 "general_operand" "")
		(match_operand:SI 2 "general_operand" "")))]
  "TARGET_INLINE_INT_DIV"
{
  rtx op2_neg, op1_di, div;

  div = gen_reg_rtx (SImode);
  emit_insn (gen_divsi3 (div, operands[1], operands[2]));

  op2_neg = expand_unop (SImode, neg_optab, operands[2], NULL_RTX, 0);

  /* This is a trick to get us to reuse the value that we're sure to
     have already copied to the FP regs.  */
  op1_di = gen_reg_rtx (DImode);
  convert_move (op1_di, operands[1], 0);

  emit_insn (gen_maddsi4 (operands[0], div, op2_neg,
			  gen_lowpart (SImode, op1_di)));
  DONE;
})

(define_expand "udivsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(udiv:SI (match_operand:SI 1 "general_operand" "")
		 (match_operand:SI 2 "general_operand" "")))]
  "TARGET_INLINE_INT_DIV"
{
  rtx op1_rf, op2_rf, op0_rf, op0_di;

  op0_rf = gen_reg_rtx (RFmode);
  op0_di = gen_reg_rtx (DImode);

  if (! register_operand (operands[1], SImode))
    operands[1] = force_reg (SImode, operands[1]);
  op1_rf = gen_reg_rtx (RFmode);
  expand_float (op1_rf, operands[1], 1);

  if (! register_operand (operands[2], SImode))
    operands[2] = force_reg (SImode, operands[2]);
  op2_rf = gen_reg_rtx (RFmode);
  expand_float (op2_rf, operands[2], 1);

  emit_insn (gen_cond_trap (EQ, operands[2], CONST0_RTX (SImode),
                            CONST1_RTX (SImode)));
  
  emit_insn (gen_divsi3_internal (op0_rf, op1_rf, op2_rf));

  emit_insn (gen_fixuns_truncrfdi2_alts (op0_di, op0_rf, const1_rtx));
  emit_move_insn (operands[0], gen_lowpart (SImode, op0_di));
  DONE;
})

(define_expand "umodsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(umod:SI (match_operand:SI 1 "general_operand" "")
		 (match_operand:SI 2 "general_operand" "")))]
  "TARGET_INLINE_INT_DIV"
{
  rtx op2_neg, op1_di, div;

  div = gen_reg_rtx (SImode);
  emit_insn (gen_udivsi3 (div, operands[1], operands[2]));

  op2_neg = expand_unop (SImode, neg_optab, operands[2], NULL_RTX, 0);

  /* This is a trick to get us to reuse the value that we're sure to
     have already copied to the FP regs.  */
  op1_di = gen_reg_rtx (DImode);
  convert_move (op1_di, operands[1], 1);

  emit_insn (gen_maddsi4 (operands[0], div, op2_neg,
			  gen_lowpart (SImode, op1_di)));
  DONE;
})

(define_expand "divsi3_internal"
  [(set (match_operand:RF 0 "fr_register_operand" "")
        (float:RF (div:SI (match_operand:RF 1 "fr_register_operand" "")
                          (match_operand:RF 2 "fr_register_operand" ""))))]
  "TARGET_INLINE_INT_DIV"
{
  rtx a         = operands[1];
  rtx b         = operands[2];
  rtx y         = gen_reg_rtx (RFmode);
  rtx e         = gen_reg_rtx (RFmode);
  rtx e1        = gen_reg_rtx (RFmode);
  rtx q         = gen_reg_rtx (RFmode);
  rtx q1        = gen_reg_rtx (RFmode);
  rtx cond      = gen_reg_rtx (CCImode);
  rtx zero      = CONST0_RTX (RFmode);
  rtx one       = CONST1_RTX (RFmode);
  rtx status1   = CONST1_RTX (SImode);
  rtx trunc_off = CONST2_RTX (SImode);
  rtx twon34_exp = gen_reg_rtx (DImode);
  rtx twon34    = gen_reg_rtx (RFmode);

  /* Load cosntant 2**(-34) */
  emit_move_insn (twon34_exp, GEN_INT (65501));
  emit_insn (gen_setf_exp_rf (twon34, twon34_exp));

  /* y  = 1 / b			*/
  emit_insn (gen_recip_approx_rf (y, a, b, cond, status1));
  /* e  = 1 - (b * y)		*/
  emit_insn (gen_m2subrf4_cond (e, cond, one, b, y, zero, status1, trunc_off));
  /* q  = a * y                 */
  emit_insn (gen_mulrf3_cond (q, cond, a, y, zero, status1, trunc_off));
  /* q1 = q + (q * e)		*/
  emit_insn (gen_m2addrf4_cond (q1, cond, q, q, e, zero, status1, trunc_off));
  /* e1 = (2**-34) + (e * e)		*/
  emit_insn (gen_m2addrf4_cond (e1, cond, twon34, e, e, zero, status1, trunc_off));
  /* q2 = q1 + (e1 * q1)		*/
  emit_insn (gen_m2addrf4_cond (operands[0], cond, q1, e1, q1, y, status1, trunc_off));
  DONE;
})

(define_expand "divdi3"
  [(set (match_operand:DI 0 "register_operand" "")
	(div:DI (match_operand:DI 1 "general_operand" "")
		(match_operand:DI 2 "general_operand" "")))]
  "TARGET_INLINE_INT_DIV"
{
  rtx op1_rf, op2_rf, op0_rf;

  op0_rf = gen_reg_rtx (RFmode);

  if (! register_operand (operands[1], DImode))
    operands[1] = force_reg (DImode, operands[1]);
  op1_rf = gen_reg_rtx (RFmode);
  expand_float (op1_rf, operands[1], 0);

  if (! register_operand (operands[2], DImode))
    operands[2] = force_reg (DImode, operands[2]);
  op2_rf = gen_reg_rtx (RFmode);
  expand_float (op2_rf, operands[2], 0);

  emit_insn (gen_cond_trap (EQ, operands[2], CONST0_RTX (DImode),
                            CONST1_RTX (DImode)));

  if (TARGET_INLINE_INT_DIV == INL_MIN_LAT)
    emit_insn (gen_divdi3_internal_lat (op0_rf, op1_rf, op2_rf));
  else
    emit_insn (gen_divdi3_internal_thr (op0_rf, op1_rf, op2_rf));

  emit_insn (gen_fix_truncrfdi2_alts (operands[0], op0_rf, const1_rtx));
  DONE;
})

(define_expand "moddi3"
  [(set (match_operand:DI 0 "register_operand" "")
	(mod:SI (match_operand:DI 1 "general_operand" "")
		(match_operand:DI 2 "general_operand" "")))]
  "TARGET_INLINE_INT_DIV"
{
  rtx op2_neg, div;

  div = gen_reg_rtx (DImode);
  emit_insn (gen_divdi3 (div, operands[1], operands[2]));

  op2_neg = expand_unop (DImode, neg_optab, operands[2], NULL_RTX, 0);

  emit_insn (gen_madddi4 (operands[0], div, op2_neg, operands[1]));
  DONE;
})

(define_expand "udivdi3"
  [(set (match_operand:DI 0 "register_operand" "")
	(udiv:DI (match_operand:DI 1 "general_operand" "")
		 (match_operand:DI 2 "general_operand" "")))]
  "TARGET_INLINE_INT_DIV"
{
  rtx op1_rf, op2_rf, op0_rf;

  op0_rf = gen_reg_rtx (RFmode);

  if (! register_operand (operands[1], DImode))
    operands[1] = force_reg (DImode, operands[1]);
  op1_rf = gen_reg_rtx (RFmode);
  expand_float (op1_rf, operands[1], 1);

  if (! register_operand (operands[2], DImode))
    operands[2] = force_reg (DImode, operands[2]);
  op2_rf = gen_reg_rtx (RFmode);
  expand_float (op2_rf, operands[2], 1);

  emit_insn (gen_cond_trap (EQ, operands[2], CONST0_RTX (DImode),
                            CONST1_RTX (DImode)));

  if (TARGET_INLINE_INT_DIV == INL_MIN_LAT)
    emit_insn (gen_divdi3_internal_lat (op0_rf, op1_rf, op2_rf));
  else
    emit_insn (gen_divdi3_internal_thr (op0_rf, op1_rf, op2_rf));

  emit_insn (gen_fixuns_truncrfdi2_alts (operands[0], op0_rf, const1_rtx));
  DONE;
})

(define_expand "umoddi3"
  [(set (match_operand:DI 0 "register_operand" "")
	(umod:DI (match_operand:DI 1 "general_operand" "")
		 (match_operand:DI 2 "general_operand" "")))]
  "TARGET_INLINE_INT_DIV"
{
  rtx op2_neg, div;

  div = gen_reg_rtx (DImode);
  emit_insn (gen_udivdi3 (div, operands[1], operands[2]));

  op2_neg = expand_unop (DImode, neg_optab, operands[2], NULL_RTX, 0);

  emit_insn (gen_madddi4 (operands[0], div, op2_neg, operands[1]));
  DONE;
})

(define_expand "divdi3_internal_lat"
  [(set (match_operand:RF 0 "fr_register_operand" "")
        (float:RF (div:DI (match_operand:RF 1 "fr_register_operand" "")
                          (match_operand:RF 2 "fr_register_operand" ""))))]
  "TARGET_INLINE_INT_DIV"
{
  rtx a         = operands[1];
  rtx b         = operands[2];
  rtx y         = gen_reg_rtx (RFmode);
  rtx y1        = gen_reg_rtx (RFmode);
  rtx y2        = gen_reg_rtx (RFmode);
  rtx e         = gen_reg_rtx (RFmode);
  rtx e1        = gen_reg_rtx (RFmode);
  rtx q         = gen_reg_rtx (RFmode);
  rtx q1        = gen_reg_rtx (RFmode);
  rtx q2        = gen_reg_rtx (RFmode);
  rtx r         = gen_reg_rtx (RFmode);
  rtx cond      = gen_reg_rtx (CCImode);
  rtx zero      = CONST0_RTX (RFmode);
  rtx one       = CONST1_RTX (RFmode);
  rtx status1   = CONST1_RTX (SImode);
  rtx trunc_off = CONST2_RTX (SImode);

  /* y  = 1 / b			*/
  emit_insn (gen_recip_approx_rf (y, a, b, cond, status1));
  /* e  = 1 - (b * y)		*/
  emit_insn (gen_m2subrf4_cond (e, cond, one, b, y, zero, status1, trunc_off));
  /* q  = a * y                 */
  emit_insn (gen_mulrf3_cond (q, cond, a, y, zero, status1, trunc_off));
  /* q1 = q + (q * e)		*/
  emit_insn (gen_m2addrf4_cond (q1, cond, q, q, e, zero, status1, trunc_off));
  /* e1 = e * e			*/
  emit_insn (gen_mulrf3_cond (e1, cond, e, e, zero, status1, trunc_off));
  /* q2 = q1 + (e1 * q1)	*/
  emit_insn (gen_m2addrf4_cond (q2, cond, q1, e1, q1, zero, status1, trunc_off));
  /* y1 = y + (y * e)		*/
  emit_insn (gen_m2addrf4_cond (y1, cond, y, y, e, zero, status1, trunc_off));
  /* r  = a - (b * q2)		*/
  emit_insn (gen_m2subrf4_cond (r, cond, a, b, q2, zero, status1, trunc_off));
  /* y2 = y1 + (y1 * e1)	*/
  emit_insn (gen_m2addrf4_cond (y2, cond, y1, y1, e1, zero, status1, trunc_off));
  /* q3 = q2 + (r * y2)		*/
  emit_insn (gen_m2addrf4_cond (operands[0], cond, q2, r, y2, y, status1, trunc_off));
  DONE;
})

(define_expand "divdi3_internal_thr"
  [(set (match_operand:RF 0 "fr_register_operand" "")
        (float:RF (div:DI (match_operand:RF 1 "fr_register_operand" "")
                          (match_operand:RF 2 "fr_register_operand" ""))))]
  "TARGET_INLINE_INT_DIV"
{
  rtx a         = operands[1];
  rtx b         = operands[2];
  rtx y         = gen_reg_rtx (RFmode);
  rtx y1        = gen_reg_rtx (RFmode);
  rtx y2        = gen_reg_rtx (RFmode);
  rtx e         = gen_reg_rtx (RFmode);
  rtx e1        = gen_reg_rtx (RFmode);
  rtx q2        = gen_reg_rtx (RFmode);
  rtx r         = gen_reg_rtx (RFmode);
  rtx cond      = gen_reg_rtx (CCImode);
  rtx zero      = CONST0_RTX (RFmode);
  rtx one       = CONST1_RTX (RFmode);
  rtx status1   = CONST1_RTX (SImode);
  rtx trunc_off = CONST2_RTX (SImode);

  /* y  = 1 / b			*/
  emit_insn (gen_recip_approx_rf (y, a, b, cond, status1));
  /* e  = 1 - (b * y)		*/
  emit_insn (gen_m2subrf4_cond (e, cond, one, b, y, zero, status1, trunc_off));
  /* y1 = y + (y * e)		*/
  emit_insn (gen_m2addrf4_cond (y1, cond, y, y, e, zero, status1, trunc_off));
  /* e1 = e * e			*/
  emit_insn (gen_mulrf3_cond (e1, cond, e, e, zero, status1, trunc_off));
  /* y2 = y1 + (y1 * e1)	*/
  emit_insn (gen_m2addrf4_cond (y2, cond, y1, y1, e1, zero, status1, trunc_off));
  /* q2 = y2 * a		*/
  emit_insn (gen_mulrf3_cond (q2, cond, y2, a, zero, status1, trunc_off));
  /* r  = a - (b * q2)		*/
  emit_insn (gen_m2subrf4_cond (r, cond, a, b, q2, zero, status1, trunc_off));
  /* q3 = q2 + (r * y2)		*/
  emit_insn (gen_m2addrf4_cond (operands[0], cond, q2, r, y2, y, status1, trunc_off));
  DONE;
})

;; SQRT operations


(define_insn "sqrt_approx_rf"
  [(set (match_operand:RF 0 "fr_register_operand" "=f")
                (unspec:RF [(match_operand:RF 1 "fr_reg_or_fp01_operand" "fG")]
			   UNSPEC_FR_SQRT_RECIP_APPROX_RES))
   (set (match_operand:CCI 2 "register_operand" "=c")
        (unspec:CCI [(match_dup 1)] UNSPEC_FR_SQRT_RECIP_APPROX))
   (use (match_operand:SI 3 "const_int_operand" ""))]
  ""
  "frsqrta.s%3 %0, %2 = %F1"
  [(set_attr "itanium_class" "fmisc")
   (set_attr "predicable" "no")])

(define_expand "sqrtsf2"
  [(set (match_operand:SF 0 "fr_register_operand" "=&f")
	(sqrt:SF (match_operand:SF 1 "fr_reg_or_fp01_operand" "fG")))]
  "TARGET_INLINE_SQRT"
{
  rtx insn;
  if (TARGET_INLINE_SQRT == INL_MIN_LAT)
    insn = gen_sqrtsf2_internal_lat (operands[0], operands[1]);
  else
    insn = gen_sqrtsf2_internal_thr (operands[0], operands[1]);
  emit_insn (insn);
  DONE;
})

(define_expand "sqrtsf2_internal_thr"
  [(set (match_operand:SF 0 "fr_register_operand" "")
        (sqrt:SF (match_operand:SF 1 "fr_register_operand" "")))]
  "TARGET_INLINE_SQRT"
{
  rtx y         = gen_reg_rtx (RFmode);
  rtx b         = gen_reg_rtx (RFmode);
  rtx g         = gen_reg_rtx (RFmode);
  rtx e         = gen_reg_rtx (RFmode);
  rtx s         = gen_reg_rtx (RFmode);
  rtx f         = gen_reg_rtx (RFmode);
  rtx y1        = gen_reg_rtx (RFmode);
  rtx g1        = gen_reg_rtx (RFmode);
  rtx h         = gen_reg_rtx (RFmode);
  rtx d         = gen_reg_rtx (RFmode);
  rtx g2        = gen_reg_rtx (RFmode);
  rtx cond      = gen_reg_rtx (CCImode);
  rtx zero      = CONST0_RTX (RFmode);
  rtx one       = CONST1_RTX (RFmode);
  rtx c1        = ia64_dconst_0_5();
  rtx c2        = ia64_dconst_0_375();
  rtx reg_df_c1	= gen_reg_rtx (DFmode);
  rtx reg_df_c2	= gen_reg_rtx (DFmode);
  rtx reg_rf_c1 = gen_reg_rtx (RFmode);
  rtx reg_rf_c2 = gen_reg_rtx (RFmode);
  rtx status0   = CONST0_RTX (SImode);
  rtx status1   = CONST1_RTX (SImode);
  rtx trunc_sgl = CONST0_RTX (SImode);
  rtx trunc_off = CONST2_RTX (SImode);

  /* Put needed constants into registers.	 */
  emit_insn (gen_movdf (reg_df_c1, c1));
  emit_insn (gen_movdf (reg_df_c2, c2));
  emit_insn (gen_extenddfrf2 (reg_rf_c1, reg_df_c1));
  emit_insn (gen_extenddfrf2 (reg_rf_c2, reg_df_c2));
  /* Empty conversion to put input into RFmode.  */
  emit_insn (gen_extendsfrf2 (b, operands[1]));
  /* y = sqrt (1 / b)			*/
  emit_insn (gen_sqrt_approx_rf (y, b, cond, status0));
  /* g = b * y				*/
  emit_insn (gen_mulrf3_cond (g, cond, b, y, zero, status1, trunc_off));
  /* e = 1 - (g * y)			*/
  emit_insn (gen_m2subrf4_cond (e, cond, one, g, y, zero, status1, trunc_off));
  /* s = 0.5 + (0.375 * e)		*/
  emit_insn (gen_m2addrf4_cond (s, cond, reg_rf_c1, reg_rf_c2, e, zero, status1, trunc_off));
  /* f = y * e				*/
  emit_insn (gen_mulrf3_cond (f, cond, y, e, zero, status1, trunc_off));
  /* y1 = y + (f * s)			*/
  emit_insn (gen_m2addrf4_cond (y1, cond, y, f, s, zero, status1, trunc_off));
  /* g1 = single (b * y1)		*/
  emit_insn (gen_mulrf3_cond (g1, cond, b, y1, zero, status1, trunc_sgl));
  /* h = 0.5 * y1			*/
  emit_insn (gen_mulrf3_cond (h, cond, reg_rf_c1, y1, zero, status1, trunc_off));
  /* d = b - g1 * g1			*/
  emit_insn (gen_m2subrf4_cond (d, cond, b, g1, g1, zero, status1, trunc_off));
  /* g2 = single(g1 + (d * h))		*/
  emit_insn (gen_m2addrf4_cond (g2, cond, g1, d, h, y, status0, trunc_sgl));
  /* Conversion back into SFmode.       */
  emit_insn (gen_truncrfsf2 (operands[0], g2));
  DONE;
})

(define_expand "sqrtsf2_internal_lat"
  [(set (match_operand:SF 0 "fr_register_operand" "")
        (sqrt:SF (match_operand:SF 1 "fr_register_operand" "")))]
  "TARGET_INLINE_SQRT"
{
  rtx y         = gen_reg_rtx (RFmode);
  rtx b         = gen_reg_rtx (RFmode);
  rtx g         = gen_reg_rtx (RFmode);
  rtx g1        = gen_reg_rtx (RFmode);
  rtx g2        = gen_reg_rtx (RFmode);
  rtx e         = gen_reg_rtx (RFmode);
  rtx s         = gen_reg_rtx (RFmode);
  rtx f         = gen_reg_rtx (RFmode);
  rtx f1        = gen_reg_rtx (RFmode);
  rtx h         = gen_reg_rtx (RFmode);
  rtx h1        = gen_reg_rtx (RFmode);
  rtx d         = gen_reg_rtx (RFmode);
  rtx cond      = gen_reg_rtx (CCImode);
  rtx zero      = CONST0_RTX (RFmode);
  rtx one       = CONST1_RTX (RFmode);
  rtx c1        = ia64_dconst_0_5();
  rtx c2        = ia64_dconst_0_375();
  rtx reg_df_c1	= gen_reg_rtx (DFmode);
  rtx reg_df_c2	= gen_reg_rtx (DFmode);
  rtx reg_rf_c1 = gen_reg_rtx (RFmode);
  rtx reg_rf_c2 = gen_reg_rtx (RFmode);
  rtx status0   = CONST0_RTX (SImode);
  rtx status1   = CONST1_RTX (SImode);
  rtx trunc_sgl = CONST0_RTX (SImode);
  rtx trunc_off = CONST2_RTX (SImode);

  /* Put needed constants into registers.	 */
  emit_insn (gen_movdf (reg_df_c1, c1));
  emit_insn (gen_movdf (reg_df_c2, c2));
  emit_insn (gen_extenddfrf2 (reg_rf_c1, reg_df_c1));
  emit_insn (gen_extenddfrf2 (reg_rf_c2, reg_df_c2));
  /* Empty conversion to put input into RFmode.  */
  emit_insn (gen_extendsfrf2 (b, operands[1]));
  /* y = sqrt (1 / b)			*/
  emit_insn (gen_sqrt_approx_rf (y, b, cond, status0));
  /* g = b * y				*/
  emit_insn (gen_mulrf3_cond (g, cond, b, y, zero, status1, trunc_off));
  /* e = 1 - (g * y)			*/
  emit_insn (gen_m2subrf4_cond (e, cond, one, g, y, zero, status1, trunc_off));
  /* h = 0.5 * y			*/
  emit_insn (gen_mulrf3_cond (h, cond, reg_rf_c1, y, zero, status1, trunc_off));
  /* s = 0.5 + (0.375 * e)		*/
  emit_insn (gen_m2addrf4_cond (s, cond, reg_rf_c1, reg_rf_c2, e, zero, status1, trunc_off));
  /* f = e * g				*/
  emit_insn (gen_mulrf3_cond (f, cond, e, g, zero, status1, trunc_off));
  /* g1 = single (g + (f * s))		*/
  emit_insn (gen_m2addrf4_cond (g1, cond, g, f, s, zero, status1, trunc_sgl));
  /* f1 = e * h				*/
  emit_insn (gen_mulrf3_cond (f1, cond, e, h, zero, status1, trunc_off));
  /* d = b - g1 * g1			*/
  emit_insn (gen_m2subrf4_cond (d, cond, b, g1, g1, zero, status1, trunc_off));
  /* h1 = h + (f1 * s)			*/
  emit_insn (gen_m2addrf4_cond (h1, cond, h, f1, s, zero, status1, trunc_off));
  /* g2 = single(g1 + (d * h1))		*/
  emit_insn (gen_m2addrf4_cond (g2, cond, g1, d, h1, y, status0, trunc_sgl));
  /* Conversion back into SFmode.       */
  emit_insn (gen_truncrfsf2 (operands[0], g2));
  DONE;
})

(define_expand "sqrtdf2"
  [(set (match_operand:DF 0 "fr_register_operand" "=&f")
	(sqrt:DF (match_operand:DF 1 "fr_reg_or_fp01_operand" "fG")))]
  "TARGET_INLINE_SQRT"
{
  rtx insn;
#if 0
  if (TARGET_INLINE_SQRT == INL_MIN_LAT)
    insn = gen_sqrtdf2_internal_lat (operands[0], operands[1]);
  else
#endif
  insn = gen_sqrtdf2_internal_thr (operands[0], operands[1]);
  emit_insn (insn);
  DONE;
})

(define_expand "sqrtdf2_internal_thr"
  [(set (match_operand:DF 0 "fr_register_operand" "")
        (sqrt:DF (match_operand:DF 1 "fr_register_operand" "")))]
  "TARGET_INLINE_SQRT"
{
  rtx y         = gen_reg_rtx (RFmode);
  rtx b         = gen_reg_rtx (RFmode);
  rtx g         = gen_reg_rtx (RFmode);
  rtx g1        = gen_reg_rtx (RFmode);
  rtx g2        = gen_reg_rtx (RFmode);
  rtx g3        = gen_reg_rtx (RFmode);
  rtx g4        = gen_reg_rtx (RFmode);
  rtx r         = gen_reg_rtx (RFmode);
  rtx r1        = gen_reg_rtx (RFmode);
  rtx h         = gen_reg_rtx (RFmode);
  rtx h1        = gen_reg_rtx (RFmode);
  rtx h2        = gen_reg_rtx (RFmode);
  rtx d         = gen_reg_rtx (RFmode);
  rtx d1        = gen_reg_rtx (RFmode);
  rtx cond      = gen_reg_rtx (CCImode);
  rtx zero      = CONST0_RTX (RFmode);
  rtx c1        = ia64_dconst_0_5();
  rtx reg_df_c1	= gen_reg_rtx (DFmode);
  rtx reg_rf_c1 = gen_reg_rtx (RFmode);
  rtx status0   = CONST0_RTX (SImode);
  rtx status1   = CONST1_RTX (SImode);
  rtx trunc_dbl = CONST1_RTX (SImode);
  rtx trunc_off = CONST2_RTX (SImode);

  /* Put needed constants into registers.	 */
  emit_insn (gen_movdf (reg_df_c1, c1));
  emit_insn (gen_extenddfrf2 (reg_rf_c1, reg_df_c1));
  /* Empty conversion to put input into RFmode.  */
  emit_insn (gen_extenddfrf2 (b, operands[1]));
  /* y = sqrt (1 / b)			*/
  emit_insn (gen_sqrt_approx_rf (y, b, cond, status0));
  /* g = b * y				*/
  emit_insn (gen_mulrf3_cond (g, cond, b, y, zero, status1, trunc_off));
  /* h = 0.5 * y			*/
  emit_insn (gen_mulrf3_cond (h, cond, reg_rf_c1, y, zero, status1, trunc_off));
  /* r = 0.5 - (g * h)			*/
  emit_insn (gen_m2subrf4_cond (r, cond, reg_rf_c1, g, h, zero, status1, trunc_off));
  /* g1 = g + (g * r)			*/
  emit_insn (gen_m2addrf4_cond (g1, cond, g, g, r, zero, status1, trunc_off));
  /* h1 = h + (h * r)			*/
  emit_insn (gen_m2addrf4_cond (h1, cond, h, h, r, zero, status1, trunc_off));
  /* r1 = 0.5 - (g1 * h1)		*/
  emit_insn (gen_m2subrf4_cond (r1, cond, reg_rf_c1, g1, h1, zero, status1, trunc_off));
  /* g2 = g1 + (g1 * r1)		*/
  emit_insn (gen_m2addrf4_cond (g2, cond, g1, g1, r1, zero, status1, trunc_off));
  /* h2 = h1 + (h1 * r1)		*/
  emit_insn (gen_m2addrf4_cond (h2, cond, h1, h1, r1, zero, status1, trunc_off));
  /* d = b - (g2 * g2)			*/
  emit_insn (gen_m2subrf4_cond (d, cond, b, g2, g2, zero, status1, trunc_off));
  /* g3 = g2 + (d * h2)			*/
  emit_insn (gen_m2addrf4_cond (g3, cond, g2, d, h2, zero, status1, trunc_off));
  /* d1 = b - (g3 * g3)			*/
  emit_insn (gen_m2subrf4_cond (d1, cond, b, g3, g3, zero, status1, trunc_off));
  /* g4 = g3 + (d1 * h2)		*/
  emit_insn (gen_m2addrf4_cond (g4, cond, g3, d1, h2, y, status1, trunc_dbl));
  /* Conversion back into SFmode.       */
  emit_insn (gen_truncrfdf2 (operands[0], g4));
  DONE;
})

(define_expand "sqrtxf2"
  [(set (match_operand:XF 0 "fr_register_operand" "")
        (sqrt:XF (match_operand:XF 1 "fr_register_operand" "")))]
  "TARGET_INLINE_SQRT"
{
  rtx y         = gen_reg_rtx (RFmode);
  rtx b         = gen_reg_rtx (RFmode);
  rtx g         = gen_reg_rtx (RFmode);
  rtx g1        = gen_reg_rtx (RFmode);
  rtx g2        = gen_reg_rtx (RFmode);
  rtx g3        = gen_reg_rtx (RFmode);
  rtx g4        = gen_reg_rtx (RFmode);
  rtx e         = gen_reg_rtx (RFmode);
  rtx e1        = gen_reg_rtx (RFmode);
  rtx e2        = gen_reg_rtx (RFmode);
  rtx h         = gen_reg_rtx (RFmode);
  rtx h1        = gen_reg_rtx (RFmode);
  rtx h2        = gen_reg_rtx (RFmode);
  rtx h3        = gen_reg_rtx (RFmode);
  rtx d         = gen_reg_rtx (RFmode);
  rtx d1        = gen_reg_rtx (RFmode);
  rtx cond      = gen_reg_rtx (CCImode);
  rtx zero      = CONST0_RTX (RFmode);
  rtx c1        = ia64_dconst_0_5();
  rtx reg_df_c1	= gen_reg_rtx (DFmode);
  rtx reg_rf_c1 = gen_reg_rtx (RFmode);
  rtx status0   = CONST0_RTX (SImode);
  rtx status1   = CONST1_RTX (SImode);
  rtx trunc_off = CONST2_RTX (SImode);

  /* Put needed constants into registers.	 */
  emit_insn (gen_movdf (reg_df_c1, c1));
  emit_insn (gen_extenddfrf2 (reg_rf_c1, reg_df_c1));
  /* Empty conversion to put input into RFmode.  */
  emit_insn (gen_extendxfrf2 (b, operands[1]));
  /* y = sqrt (1 / b)			*/
  emit_insn (gen_sqrt_approx_rf (y, b, cond, status0));
  /* g = b * y				*/
  emit_insn (gen_mulrf3_cond (g, cond, b, y, zero, status1, trunc_off));
  /* h = 0.5 * y			*/
  emit_insn (gen_mulrf3_cond (h, cond, reg_rf_c1, y, zero, status1, trunc_off));
  /* e = 0.5 - (g * h)			*/
  emit_insn (gen_m2subrf4_cond (e, cond, reg_rf_c1, g, h, zero, status1, trunc_off));
  /* g1 = g + (g * e)			*/
  emit_insn (gen_m2addrf4_cond (g1, cond, g, g, e, zero, status1, trunc_off));
  /* h1 = h + (h * e)			*/
  emit_insn (gen_m2addrf4_cond (h1, cond, h, h, e, zero, status1, trunc_off));
  /* e1 = 0.5 - (g1 * h1)		*/
  emit_insn (gen_m2subrf4_cond (e1, cond, reg_rf_c1, g1, h1, zero, status1, trunc_off));
  /* g2 = g1 + (g1 * e1)		*/
  emit_insn (gen_m2addrf4_cond (g2, cond, g1, g1, e1, zero, status1, trunc_off));
  /* h2 = h1 + (h1 * e1)		*/
  emit_insn (gen_m2addrf4_cond (h2, cond, h1, h1, e1, zero, status1, trunc_off));
  /* d = b - (g2 * g2)			*/
  emit_insn (gen_m2subrf4_cond (d, cond, b, g2, g2, zero, status1, trunc_off));
  /* e2 = 0.5 - (g2 * h2)		*/
  emit_insn (gen_m2subrf4_cond (e2, cond, reg_rf_c1, g2, h2, zero, status1, trunc_off));
  /* g3 = g2 + (d * h2)			*/
  emit_insn (gen_m2addrf4_cond (g3, cond, g2, d, h2, zero, status1, trunc_off));
  /* h3 = h2 + (e2 * h2)		*/
  emit_insn (gen_m2addrf4_cond (h3, cond, h2, e2, h2, zero, status1, trunc_off));
  /* d1 = b - (g3 * g3)			*/
  emit_insn (gen_m2subrf4_cond (d1, cond, b, g3, g3, zero, status1, trunc_off));
  /* g4 = g3 + (d1 * h3)		*/
  emit_insn (gen_m2addrf4_cond (g4, cond, g3, d1, h3, y, status1, trunc_off));
  /* Conversion back into SFmode.       */
  emit_insn (gen_truncrfxf2 (operands[0], g4));
  DONE;
})
