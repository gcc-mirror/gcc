;; Machine description for AArch64 SVE2.
;; Copyright (C) 2019 Free Software Foundation, Inc.
;; Contributed by ARM Ltd.
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
;; <http://www.gnu.org/licenses/>.

;; Integer average (floor).
(define_expand "<u>avg<mode>3_floor"
  [(set (match_operand:SVE_FULL_I 0 "register_operand")
	(unspec:SVE_FULL_I
	  [(match_dup 3)
	   (unspec:SVE_FULL_I
	     [(match_operand:SVE_FULL_I 1 "register_operand")
	      (match_operand:SVE_FULL_I 2 "register_operand")]
	     HADD)]
	  UNSPEC_PRED_X))]
  "TARGET_SVE2"
  {
    operands[3] = force_reg (<VPRED>mode, CONSTM1_RTX (<VPRED>mode));
  }
)

;; Integer average (rounding).
(define_expand "<u>avg<mode>3_ceil"
  [(set (match_operand:SVE_FULL_I 0 "register_operand")
	(unspec:SVE_FULL_I
	  [(match_dup 3)
	   (unspec:SVE_FULL_I
	     [(match_operand:SVE_FULL_I 1 "register_operand")
	      (match_operand:SVE_FULL_I 2 "register_operand")]
	     RHADD)]
	  UNSPEC_PRED_X))]
  "TARGET_SVE2"
  {
    operands[3] = force_reg (<VPRED>mode, CONSTM1_RTX (<VPRED>mode));
  }
)

;; Predicated halving addsub.
(define_insn "*<sur>h<addsub><mode>"
  [(set (match_operand:SVE_FULL_I 0 "register_operand" "=w, ?&w")
	(unspec:SVE_FULL_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (unspec:SVE_FULL_I
	     [(match_operand:SVE_FULL_I 2 "register_operand" "%0, w")
	      (match_operand:SVE_FULL_I 3 "register_operand" "w, w")]
	     HADDSUB)]
	  UNSPEC_PRED_X))]
  "TARGET_SVE2"
  "@
   <sur>h<addsub>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   movprfx\t%0, %2\;<sur>h<addsub>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>"
  [(set_attr "movprfx" "*,yes")]
)

;; Multiply long top / bottom.
(define_insn "<su>mull<bt><Vwide>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
	(unspec:<VWIDE>
	  [(match_operand:SVE_FULL_BHSI 1 "register_operand" "w")
	   (match_operand:SVE_FULL_BHSI 2 "register_operand" "w")]
	  MULLBT))]
  "TARGET_SVE2"
  "<su>mull<bt>\t%0.<Vewtype>, %1.<Vetype>, %2.<Vetype>"
)

;; (Rounding) Right shift narrow bottom.
(define_insn "<r>shrnb<mode>"
  [(set (match_operand:SVE_FULL_BHSI 0 "register_operand" "=w")
        (unspec:SVE_FULL_BHSI
	  [(match_operand:<VWIDE> 1 "register_operand" "w")
	   (match_operand 2 "aarch64_simd_shift_imm_offset_<Vel>" "")]
	  SHRNB))]
  "TARGET_SVE2"
  "<r>shrnb\t%0.<Vetype>, %1.<Vewtype>, #%2"
)

;; (Rounding) Right shift narrow top.
(define_insn "<r>shrnt<mode>"
  [(set (match_operand:SVE_FULL_BHSI 0 "register_operand" "=w")
	(unspec:SVE_FULL_BHSI
	  [(match_operand:SVE_FULL_BHSI 1 "register_operand" "0")
	   (match_operand:<VWIDE> 2 "register_operand" "w")
	   (match_operand 3 "aarch64_simd_shift_imm_offset_<Vel>" "i")]
	  SHRNT))]
  "TARGET_SVE2"
  "<r>shrnt\t%0.<Vetype>, %2.<Vewtype>, #%3"
)

;; Unpredicated integer multiply-high-with-(round-and-)scale.
(define_expand "<su>mulh<r>s<mode>3"
  [(set (match_operand:SVE_FULL_BHSI 0 "register_operand")
	(unspec:SVE_FULL_BHSI
	  [(match_dup 3)
	   (unspec:SVE_FULL_BHSI
	     [(match_operand:SVE_FULL_BHSI 1 "register_operand")
	      (match_operand:SVE_FULL_BHSI 2 "register_operand")]
	     MULHRS)]
	  UNSPEC_PRED_X))]
  "TARGET_SVE2"
  {
    operands[3] = aarch64_ptrue_reg (<VPRED>mode);

    rtx prod_b = gen_reg_rtx (<VWIDE>mode);
    rtx prod_t = gen_reg_rtx (<VWIDE>mode);
    emit_insn (gen_<su>mullb<Vwide> (prod_b, operands[1], operands[2]));
    emit_insn (gen_<su>mullt<Vwide> (prod_t, operands[1], operands[2]));

    rtx shift = GEN_INT (GET_MODE_UNIT_BITSIZE (<MODE>mode) - 1);
    emit_insn (gen_<r>shrnb<mode> (operands[0], prod_b, shift));
    emit_insn (gen_<r>shrnt<mode> (operands[0], operands[0], prod_t, shift));

    DONE;
  }
)

;; Unpredicated signed / unsigned shift-right accumulate.
(define_insn_and_rewrite "*aarch64_sve2_sra<mode>"
  [(set (match_operand:SVE_FULL_I 0 "register_operand" "=w")
	(plus:SVE_FULL_I
	  (unspec:SVE_FULL_I
	    [(match_operand 4)
	     (SHIFTRT:SVE_FULL_I
	       (match_operand:SVE_FULL_I 2 "register_operand" "w")
	       (match_operand:SVE_FULL_I 3 "aarch64_simd_rshift_imm" "Dr"))]
	    UNSPEC_PRED_X)
	 (match_operand:SVE_FULL_I 1 "register_operand" "0")))]
  "TARGET_SVE2"
  "<sra_op>sra\t%0.<Vetype>, %2.<Vetype>, #%3"
  "&& !CONSTANT_P (operands[4])"
  {
    operands[4] = CONSTM1_RTX (<VPRED>mode);
  }
)

;; Unpredicated 3-way exclusive OR.
(define_insn "*aarch64_sve2_eor3<mode>"
  [(set (match_operand:SVE_FULL_I 0 "register_operand" "=w, w, w, ?&w")
	(xor:SVE_FULL_I
	  (xor:SVE_FULL_I
	    (match_operand:SVE_FULL_I 1 "register_operand" "0, w, w, w")
	    (match_operand:SVE_FULL_I 2 "register_operand" "w, 0, w, w"))
	  (match_operand:SVE_FULL_I 3 "register_operand" "w, w, 0, w")))]
  "TARGET_SVE2"
  "@
  eor3\t%0.d, %0.d, %2.d, %3.d
  eor3\t%0.d, %0.d, %1.d, %3.d
  eor3\t%0.d, %0.d, %1.d, %2.d
  movprfx\t%0, %1\;eor3\t%0.d, %0.d, %2.d, %3.d"
  [(set_attr "movprfx" "*,*,*,yes")]
)

;; Use NBSL for vector NOR.
(define_insn_and_rewrite "*aarch64_sve2_nor<mode>"
  [(set (match_operand:SVE_FULL_I 0 "register_operand" "=w, ?&w")
	(unspec:SVE_FULL_I
	  [(match_operand 3)
	   (and:SVE_FULL_I
	     (not:SVE_FULL_I
	       (match_operand:SVE_FULL_I 1 "register_operand" "%0, w"))
	     (not:SVE_FULL_I
	       (match_operand:SVE_FULL_I 2 "register_operand" "w, w")))]
	  UNSPEC_PRED_X))]
  "TARGET_SVE2"
  "@
  nbsl\t%0.d, %0.d, %2.d, %0.d
  movprfx\t%0, %1\;nbsl\t%0.d, %0.d, %2.d, %0.d"
  "&& !CONSTANT_P (operands[3])"
  {
    operands[3] = CONSTM1_RTX (<VPRED>mode);
  }
  [(set_attr "movprfx" "*,yes")]
)

;; Use NBSL for vector NAND.
(define_insn_and_rewrite "*aarch64_sve2_nand<mode>"
  [(set (match_operand:SVE_FULL_I 0 "register_operand" "=w, ?&w")
	(unspec:SVE_FULL_I
	  [(match_operand 3)
	   (ior:SVE_FULL_I
	     (not:SVE_FULL_I
	       (match_operand:SVE_FULL_I 1 "register_operand" "%0, w"))
	     (not:SVE_FULL_I
	       (match_operand:SVE_FULL_I 2 "register_operand" "w, w")))]
	  UNSPEC_PRED_X))]
  "TARGET_SVE2"
  "@
  nbsl\t%0.d, %0.d, %2.d, %2.d
  movprfx\t%0, %1\;nbsl\t%0.d, %0.d, %2.d, %2.d"
  "&& !CONSTANT_P (operands[3])"
  {
    operands[3] = CONSTM1_RTX (<VPRED>mode);
  }
  [(set_attr "movprfx" "*,yes")]
)

;; Unpredicated bitwise select.
;; (op3 ? bsl_mov : bsl_dup) == (((bsl_mov ^ bsl_dup) & op3) ^ bsl_dup)
(define_insn "*aarch64_sve2_bsl<mode>"
  [(set (match_operand:SVE_FULL_I 0 "register_operand" "=w, ?&w")
	(xor:SVE_FULL_I
	  (and:SVE_FULL_I
	    (xor:SVE_FULL_I
	      (match_operand:SVE_FULL_I 1 "register_operand" "<bsl_1st>, w")
	      (match_operand:SVE_FULL_I 2 "register_operand" "<bsl_2nd>, w"))
	    (match_operand:SVE_FULL_I 3 "register_operand" "w, w"))
	  (match_dup BSL_DUP)))]
  "TARGET_SVE2"
  "@
  bsl\t%0.d, %0.d, %<bsl_dup>.d, %3.d
  movprfx\t%0, %<bsl_mov>\;bsl\t%0.d, %0.d, %<bsl_dup>.d, %3.d"
  [(set_attr "movprfx" "*,yes")]
)

;; Unpredicated bitwise inverted select.
;; (~(op3 ? bsl_mov : bsl_dup)) == (~(((bsl_mov ^ bsl_dup) & op3) ^ bsl_dup))
(define_insn_and_rewrite "*aarch64_sve2_nbsl<mode>"
  [(set (match_operand:SVE_FULL_I 0 "register_operand" "=w, ?&w")
	(unspec:SVE_FULL_I
	  [(match_operand 4)
	   (not:SVE_FULL_I
	     (xor:SVE_FULL_I
	       (and:SVE_FULL_I
		 (xor:SVE_FULL_I
		   (match_operand:SVE_FULL_I 1 "register_operand" "<bsl_1st>, w")
		   (match_operand:SVE_FULL_I 2 "register_operand" "<bsl_2nd>, w"))
		 (match_operand:SVE_FULL_I 3 "register_operand" "w, w"))
	       (match_dup BSL_DUP)))]
	  UNSPEC_PRED_X))]
  "TARGET_SVE2"
  "@
  nbsl\t%0.d, %0.d, %<bsl_dup>.d, %3.d
  movprfx\t%0, %<bsl_mov>\;nbsl\t%0.d, %0.d, %<bsl_dup>.d, %3.d"
  "&& !CONSTANT_P (operands[4])"
  {
    operands[4] = CONSTM1_RTX (<VPRED>mode);
  }
  [(set_attr "movprfx" "*,yes")]
)

;; Unpredicated bitwise select with inverted first operand.
;; (op3 ? ~bsl_mov : bsl_dup) == ((~(bsl_mov ^ bsl_dup) & op3) ^ bsl_dup)
(define_insn_and_rewrite "*aarch64_sve2_bsl1n<mode>"
  [(set (match_operand:SVE_FULL_I 0 "register_operand" "=w, ?&w")
	(xor:SVE_FULL_I
	  (and:SVE_FULL_I
	    (unspec:SVE_FULL_I
	      [(match_operand 4)
	       (not:SVE_FULL_I
		 (xor:SVE_FULL_I
		   (match_operand:SVE_FULL_I 1 "register_operand" "<bsl_1st>, w")
		   (match_operand:SVE_FULL_I 2 "register_operand" "<bsl_2nd>, w")))]
	      UNSPEC_PRED_X)
	    (match_operand:SVE_FULL_I 3 "register_operand" "w, w"))
	  (match_dup BSL_DUP)))]
  "TARGET_SVE2"
  "@
  bsl1n\t%0.d, %0.d, %<bsl_dup>.d, %3.d
  movprfx\t%0, %<bsl_mov>\;bsl1n\t%0.d, %0.d, %<bsl_dup>.d, %3.d"
  "&& !CONSTANT_P (operands[4])"
  {
    operands[4] = CONSTM1_RTX (<VPRED>mode);
  }
  [(set_attr "movprfx" "*,yes")]
)

;; Unpredicated bitwise select with inverted second operand.
;; (bsl_dup ? bsl_mov : ~op3) == ((bsl_dup & bsl_mov) | (~op3 & ~bsl_dup))
(define_insn_and_rewrite "*aarch64_sve2_bsl2n<mode>"
  [(set (match_operand:SVE_FULL_I 0 "register_operand" "=w, ?&w")
	(ior:SVE_FULL_I
	  (and:SVE_FULL_I
	    (match_operand:SVE_FULL_I 1 "register_operand" "<bsl_1st>, w")
	    (match_operand:SVE_FULL_I 2 "register_operand" "<bsl_2nd>, w"))
	  (unspec:SVE_FULL_I
	    [(match_operand 4)
	     (and:SVE_FULL_I
	       (not:SVE_FULL_I
		 (match_operand:SVE_FULL_I 3 "register_operand" "w, w"))
	       (not:SVE_FULL_I
		 (match_dup BSL_DUP)))]
	    UNSPEC_PRED_X)))]
  "TARGET_SVE2"
  "@
  bsl2n\t%0.d, %0.d, %3.d, %<bsl_dup>.d
  movprfx\t%0, %<bsl_mov>\;bsl2n\t%0.d, %0.d, %3.d, %<bsl_dup>.d"
  "&& !CONSTANT_P (operands[4])"
  {
    operands[4] = CONSTM1_RTX (<VPRED>mode);
  }
  [(set_attr "movprfx" "*,yes")]
)

;; Unpredicated bitwise select with inverted second operand, alternative form.
;; (bsl_dup ? bsl_mov : ~op3) == ((bsl_dup & bsl_mov) | (~bsl_dup & ~op3))
(define_insn_and_rewrite "*aarch64_sve2_bsl2n<mode>"
  [(set (match_operand:SVE_FULL_I 0 "register_operand" "=w, ?&w")
	(ior:SVE_FULL_I
	  (and:SVE_FULL_I
	    (match_operand:SVE_FULL_I 1 "register_operand" "<bsl_1st>, w")
	    (match_operand:SVE_FULL_I 2 "register_operand" "<bsl_2nd>, w"))
	  (unspec:SVE_FULL_I
	    [(match_operand 4)
	     (and:SVE_FULL_I
	       (not:SVE_FULL_I
		 (match_dup BSL_DUP))
	       (not:SVE_FULL_I
		 (match_operand:SVE_FULL_I 3 "register_operand" "w, w")))]
	    UNSPEC_PRED_X)))]
  "TARGET_SVE2"
  "@
  bsl2n\t%0.d, %0.d, %3.d, %<bsl_dup>.d
  movprfx\t%0, %<bsl_mov>\;bsl2n\t%0.d, %0.d, %3.d, %<bsl_dup>.d"
  "&& !CONSTANT_P (operands[4])"
  {
    operands[4] = CONSTM1_RTX (<VPRED>mode);
  }
  [(set_attr "movprfx" "*,yes")]
)

;; Use WHILERW and WHILEWR to accelerate alias checks.  This is only
;; possible if the accesses we're checking are exactly the same size
;; as an SVE vector.
(define_expand "check_<raw_war>_ptrs<mode>"
  [(match_operand:GPI 0 "register_operand")
   (unspec:VNx16BI
     [(match_operand:GPI 1 "register_operand")
      (match_operand:GPI 2 "register_operand")
      (match_operand:GPI 3 "aarch64_bytes_per_sve_vector_operand")
      (match_operand:GPI 4 "const_int_operand")]
     SVE2_WHILE_PTR)]
  "TARGET_SVE2"
{
  /* Use the widest predicate mode we can.  */
  unsigned int align = INTVAL (operands[4]);
  if (align > 8)
    align = 8;
  machine_mode pred_mode = aarch64_sve_pred_mode (align).require ();

  /* Emit a WHILERW or WHILEWR, setting the condition codes based on
     the result.  */
  emit_insn (gen_aarch64_sve2_while_ptest
	     (<SVE2_WHILE_PTR:unspec>, <MODE>mode, pred_mode,
	      gen_rtx_SCRATCH (pred_mode), operands[1], operands[2],
	      CONSTM1_RTX (VNx16BImode), CONSTM1_RTX (pred_mode)));

  /* Set operand 0 to true if the last bit of the predicate result is set,
     i.e. if all elements are free of dependencies.  */
  rtx cc_reg = gen_rtx_REG (CC_NZCmode, CC_REGNUM);
  rtx cmp = gen_rtx_LTU (<MODE>mode, cc_reg, const0_rtx);
  emit_insn (gen_aarch64_cstore<mode> (operands[0], cmp, cc_reg));
  DONE;
})

;; A WHILERW or WHILEWR in which only the flags result is interesting.
(define_insn_and_rewrite "@aarch64_sve2_while<cmp_op><GPI:mode><PRED_ALL:mode>_ptest"
  [(set (reg:CC_NZC CC_REGNUM)
	(unspec:CC_NZC
	  [(match_operand 3)
	   (match_operand 4)
	   (const_int SVE_KNOWN_PTRUE)
	   (unspec:PRED_ALL
	     [(match_operand:GPI 1 "register_operand" "r")
	      (match_operand:GPI 2 "register_operand" "r")]
	     SVE2_WHILE_PTR)]
	  UNSPEC_PTEST))
   (clobber (match_scratch:PRED_ALL 0 "=Upa"))]
  "TARGET_SVE2"
  "while<cmp_op>\t%0.<PRED_ALL:Vetype>, %x1, %x2"
  ;; Force the compiler to drop the unused predicate operand, so that we
  ;; don't have an unnecessary PTRUE.
  "&& (!CONSTANT_P (operands[3]) || !CONSTANT_P (operands[4]))"
  {
    operands[3] = CONSTM1_RTX (VNx16BImode);
    operands[4] = CONSTM1_RTX (<PRED_ALL:MODE>mode);
  }
)
