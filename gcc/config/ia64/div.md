
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
        (if_then_else:RF (ne:RF (match_operand:BI 1 "register_operand"  "c,c")
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
        (if_then_else:RF (ne:RF (match_operand:BI 1 "register_operand"  "c,c")
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
        (if_then_else:RF (ne:RF (match_operand:BI 1 "register_operand"  "c,c")
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
        (if_then_else:RF (ne:RF (match_operand:BI 1 "register_operand"  "c,c")
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
        (if_then_else:RF (ne:RF (match_operand:BI 1 "register_operand"  "c,c")
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
        (if_then_else:RF (ne:RF (match_operand:BI 1 "register_operand"  "c,c")
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
        (if_then_else:RF (ne:RF (match_operand:BI 1 "register_operand"  "c,c")
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
        (if_then_else:RF (ne:RF (match_operand:BI 1 "register_operand"  "c,c")
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
        (float_extend:RF (match_operand:SDX_F 1 "fr_register_operand" "f")))]
  ""
  "#"
  [(set_attr "itanium_class" "fmisc")
   (set_attr "predicable" "yes")])

(define_split
  [(set (match_operand:RF 0 "fr_register_operand" "")
        (float_extend:RF (match_operand:SDX_F 1 "fr_register_operand" "")))]
   "reload_completed"
   [(set (match_dup 0) (match_dup 2))]
{
   operands[2] = gen_rtx_REG (RFmode, REGNO (operands[1]));
})


(define_insn "truncrf<mode>2"
  [(set (match_operand:SDX_F 0 "fr_register_operand" "=f")
        (float_truncate:SDX_F (match_operand:RF 1 "fr_register_operand" "f")))]
  ""
  "#"
  [(set_attr "itanium_class" "fmisc")
   (set_attr "predicable" "yes")])

(define_split
  [(set (match_operand:SDX_F 0 "fr_register_operand" "")
        (float_truncate:SDX_F (match_operand:RF 1 "fr_register_operand" "")))]
   "reload_completed"
   [(set (match_dup 0) (match_dup 2))]
{
   operands[2] = gen_rtx_REG (<MODE>mode, REGNO (operands[1]));
})

;; Reciprocal approximation

(define_insn "recip_approx_rf"
  [(set (match_operand:RF 0 "fr_register_operand" "=f")
        (unspec:RF [(match_operand:RF 1 "fr_register_operand" "f")
		    (match_operand:RF 2 "fr_register_operand" "f")]
		   UNSPEC_FR_RECIP_APPROX_RES))
   (set (match_operand:BI 3 "register_operand" "=c")
        (unspec:BI [(match_dup 1) (match_dup 2)] UNSPEC_FR_RECIP_APPROX))
   (use (match_operand:SI 4 "const_int_operand" ""))]
  ""
  "frcpa.s%4 %0, %3 = %1, %2"
  [(set_attr "itanium_class" "fmisc")
   (set_attr "predicable" "no")])

;; Single precision floating point division (maximum throughput algorithm).

(define_expand "divsf3_internal_thr"
  [(set (match_operand:SF 0 "fr_register_operand" "")
        (div:SF (match_operand:SF 1 "fr_register_operand" "")
                (match_operand:SF 2 "fr_register_operand" "")))]
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
  rtx cond  = gen_reg_rtx (BImode);
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


;; Double precision floating point division (maximum throughput algorithm).

(define_expand "divdf3_internal_thr"
  [(set (match_operand:DF 0 "fr_register_operand" "")
        (div:DF (match_operand:DF 1 "fr_register_operand" "")
                (match_operand:DF 2 "fr_register_operand" "")))]
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
  rtx cond  = gen_reg_rtx (BImode);
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
