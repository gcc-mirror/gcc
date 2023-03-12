;; Machine description for AArch64 SVE2.
;; Copyright (C) 2019-2023 Free Software Foundation, Inc.
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

;; The file is organised into the following sections (search for the full
;; line):
;;
;; == Moves
;; ---- Non-temporal gather loads
;; ---- Non-temporal scatter stores
;;
;; == Uniform binary arithmnetic
;; ---- [INT] Multiplication
;; ---- [INT] Scaled high-part multiplication
;; ---- [INT] General binary arithmetic that maps to unspecs
;; ---- [INT] Saturating binary arithmetic
;; ---- [INT] Saturating left shifts
;;
;; == Uniform ternary arithmnetic
;; ---- [INT] General ternary arithmetic that maps to unspecs
;; ---- [INT] Multiply-and-accumulate operations
;; ---- [INT] Binary logic operations with rotation
;; ---- [INT] Ternary logic operations
;; ---- [INT] Shift-and-accumulate operations
;; ---- [INT] Shift-and-insert operations
;; ---- [INT] Sum of absolute differences
;;
;; == Extending arithmetic
;; ---- [INT] Wide binary arithmetic
;; ---- [INT] Long binary arithmetic
;; ---- [INT] Long left shifts
;; ---- [INT] Long binary arithmetic with accumulation
;; ---- [FP] Long multiplication with accumulation
;;
;; == Narrowing arithnetic
;; ---- [INT] Narrowing unary arithmetic
;; ---- [INT] Narrowing binary arithmetic
;; ---- [INT] Narrowing right shifts
;;
;; == Pairwise arithmetic
;; ---- [INT] Pairwise arithmetic
;; ---- [FP] Pairwise arithmetic
;; ---- [INT] Pairwise arithmetic with accumulation
;;
;; == Complex arithmetic
;; ---- [INT] Complex binary operations
;; ---- [INT] Complex ternary operations
;; ---- [INT] Complex dot product
;;
;; == Conversions
;; ---- [FP<-FP] Widening conversions
;; ---- [FP<-FP] Narrowing conversions
;;
;; == Other arithmetic
;; ---- [INT] Reciprocal approximation
;; ---- [INT<-FP] Base-2 logarithm
;; ---- [INT] Polynomial multiplication
;;
;; == Permutation
;; ---- [INT,FP] General permutes
;; ---- [INT] Optional bit-permute extensions
;;
;; == General
;; ---- Check for aliases between pointers
;; ---- Histogram processing
;; ---- String matching
;;
;; == Cryptographic extensions
;; ---- Optional AES extensions
;; ---- Optional SHA-3 extensions
;; ---- Optional SM4 extensions

;; =========================================================================
;; == Moves
;; =========================================================================

;; -------------------------------------------------------------------------
;; ---- Non-temporal gather loads
;; -------------------------------------------------------------------------
;; Includes gather forms of:
;; - LDNT1B
;; - LDNT1D
;; - LDNT1H
;; - LDNT1W
;; -------------------------------------------------------------------------

;; Non-extending loads.
(define_insn "@aarch64_gather_ldnt<mode>"
  [(set (match_operand:SVE_FULL_SD 0 "register_operand" "=w, w")
	(unspec:SVE_FULL_SD
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (match_operand:DI 2 "aarch64_reg_or_zero" "Z, r")
	   (match_operand:<V_INT_EQUIV> 3 "register_operand" "w, w")
	   (mem:BLK (scratch))]
	  UNSPEC_LDNT1_GATHER))]
  "TARGET_SVE2"
  "@
   ldnt1<Vesize>\t%0.<Vetype>, %1/z, [%3.<Vetype>]
   ldnt1<Vesize>\t%0.<Vetype>, %1/z, [%3.<Vetype>, %2]"
)

;; Extending loads.
(define_insn_and_rewrite "@aarch64_gather_ldnt_<ANY_EXTEND:optab><SVE_FULL_SDI:mode><SVE_PARTIAL_I:mode>"
  [(set (match_operand:SVE_FULL_SDI 0 "register_operand" "=w, w")
	(unspec:SVE_FULL_SDI
	  [(match_operand:<SVE_FULL_SDI:VPRED> 4 "general_operand" "UplDnm, UplDnm")
	   (ANY_EXTEND:SVE_FULL_SDI
	     (unspec:SVE_PARTIAL_I
	       [(match_operand:<SVE_FULL_SDI:VPRED> 1 "register_operand" "Upl, Upl")
		(match_operand:DI 2 "aarch64_reg_or_zero" "Z, r")
		(match_operand:<SVE_FULL_SDI:V_INT_EQUIV> 3 "register_operand" "w, w")
		(mem:BLK (scratch))]
	       UNSPEC_LDNT1_GATHER))]
	  UNSPEC_PRED_X))]
  "TARGET_SVE2
   && (~<SVE_FULL_SDI:narrower_mask> & <SVE_PARTIAL_I:self_mask>) == 0"
  "@
   ldnt1<ANY_EXTEND:s><SVE_PARTIAL_I:Vesize>\t%0.<SVE_FULL_SDI:Vetype>, %1/z, [%3.<SVE_FULL_SDI:Vetype>]
   ldnt1<ANY_EXTEND:s><SVE_PARTIAL_I:Vesize>\t%0.<SVE_FULL_SDI:Vetype>, %1/z, [%3.<SVE_FULL_SDI:Vetype>, %2]"
  "&& !CONSTANT_P (operands[4])"
  {
    operands[4] = CONSTM1_RTX (<SVE_FULL_SDI:VPRED>mode);
  }
)

;; -------------------------------------------------------------------------
;; ---- Non-temporal scatter stores
;; -------------------------------------------------------------------------
;; Includes scatter forms of:
;; - STNT1B
;; - STNT1D
;; - STNT1H
;; - STNT1W
;; -------------------------------------------------------------------------

;; Non-truncating stores.
(define_insn "@aarch64_scatter_stnt<mode>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_operand:<VPRED> 0 "register_operand" "Upl, Upl")
	   (match_operand:DI 1 "aarch64_reg_or_zero" "Z, r")
	   (match_operand:<V_INT_EQUIV> 2 "register_operand" "w, w")
	   (match_operand:SVE_FULL_SD 3 "register_operand" "w, w")]

	  UNSPEC_STNT1_SCATTER))]
  "TARGET_SVE"
  "@
   stnt1<Vesize>\t%3.<Vetype>, %0, [%2.<Vetype>]
   stnt1<Vesize>\t%3.<Vetype>, %0, [%2.<Vetype>, %1]"
)

;; Truncating stores.
(define_insn "@aarch64_scatter_stnt_<SVE_FULL_SDI:mode><SVE_PARTIAL_I:mode>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_operand:<SVE_FULL_SDI:VPRED> 0 "register_operand" "Upl, Upl")
	   (match_operand:DI 1 "aarch64_reg_or_zero" "Z, r")
	   (match_operand:<SVE_FULL_SDI:V_INT_EQUIV> 2 "register_operand" "w, w")
	   (truncate:SVE_PARTIAL_I
	     (match_operand:SVE_FULL_SDI 3 "register_operand" "w, w"))]
	  UNSPEC_STNT1_SCATTER))]
  "TARGET_SVE2
   && (~<SVE_FULL_SDI:narrower_mask> & <SVE_PARTIAL_I:self_mask>) == 0"
  "@
   stnt1<SVE_PARTIAL_I:Vesize>\t%3.<SVE_FULL_SDI:Vetype>, %0, [%2.<SVE_FULL_SDI:Vetype>]
   stnt1<SVE_PARTIAL_I:Vesize>\t%3.<SVE_FULL_SDI:Vetype>, %0, [%2.<SVE_FULL_SDI:Vetype>, %1]"
)

;; =========================================================================
;; == Uniform binary arithmnetic
;; =========================================================================

;; -------------------------------------------------------------------------
;; ---- [INT] Multiplication
;; -------------------------------------------------------------------------
;; Includes the lane forms of:
;; - MUL
;; -------------------------------------------------------------------------

(define_insn "@aarch64_mul_lane_<mode>"
  [(set (match_operand:SVE_FULL_HSDI 0 "register_operand" "=w")
	(mult:SVE_FULL_HSDI
	  (unspec:SVE_FULL_HSDI
	    [(match_operand:SVE_FULL_HSDI 2 "register_operand" "<sve_lane_con>")
	     (match_operand:SI 3 "const_int_operand")]
	    UNSPEC_SVE_LANE_SELECT)
	  (match_operand:SVE_FULL_HSDI 1 "register_operand" "w")))]
  "TARGET_SVE2"
  "mul\t%0.<Vetype>, %1.<Vetype>, %2.<Vetype>[%3]"
)

;; -------------------------------------------------------------------------
;; ---- [INT] Scaled high-part multiplication
;; -------------------------------------------------------------------------
;; The patterns in this section are synthetic.
;; -------------------------------------------------------------------------

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
    emit_insn (gen_aarch64_sve_<su>mullb<Vwide> (prod_b, operands[1],
						 operands[2]));
    emit_insn (gen_aarch64_sve_<su>mullt<Vwide> (prod_t, operands[1],
						 operands[2]));

    rtx shift = GEN_INT (GET_MODE_UNIT_BITSIZE (<MODE>mode) - 1);
    emit_insn (gen_aarch64_sve_<r>shrnb<Vwide> (operands[0], prod_b, shift));
    emit_insn (gen_aarch64_sve_<r>shrnt<Vwide> (operands[0], operands[0],
						prod_t, shift));

    DONE;
  }
)

;; -------------------------------------------------------------------------
;; ---- [INT] General binary arithmetic that maps to unspecs
;; -------------------------------------------------------------------------
;; Includes:
;; - SHADD
;; - SHSUB
;; - SHSUBR
;; - SQRSHL
;; - SQRSHLR
;; - SRHADD
;; - SRSHL
;; - SRSHLR
;; - SUQADD
;; - UHADD
;; - UHSUB
;; - UHSUBR
;; - UQRSHL
;; - UQRSHLR
;; - URHADD
;; - URSHL
;; - URSHLR
;; - USQADD
;; -------------------------------------------------------------------------

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

;; The immediate form of SQADD acts as an immediate form of SUQADD
;; over its full range.  In contrast to the ss_plus pattern, we do
;; not need to treat byte immediates specially.  E.g.:
;;
;;	SQADD	Z0.B, Z0.B, #128
;;
;; is equivalent to:
;;
;;	MOV	Z1.B, #128
;;	SUQADD	Z0.B, P0/M, Z0.B, Z1.B
;;
;; even though it's not equivalent to:
;;
;;	MOV	Z1.B, #128
;;	SQADD	Z0.B, P0/M, Z0.B, Z1.B	// Saturating subtraction of 128
(define_insn "@aarch64_sve_suqadd<mode>_const"
  [(set (match_operand:SVE_FULL_I 0 "register_operand" "=w, ?&w")
	(unspec:SVE_FULL_I
	  [(match_operand:SVE_FULL_I 1 "register_operand" "0, w")
	   (match_operand:SVE_FULL_I 2 "aarch64_sve_arith_immediate")]
	  UNSPEC_SUQADD))]
  "TARGET_SVE2"
  "@
   sqadd\t%0.<Vetype>, %0.<Vetype>, #%D2
   movprfx\t%0, %1\;sqadd\t%0.<Vetype>, %0.<Vetype>, #%D2"
  [(set_attr "movprfx" "*,yes")]
)

;; General predicated binary arithmetic.  All operations handled here
;; are commutative or have a reversed form.
(define_insn "@aarch64_pred_<sve_int_op><mode>"
  [(set (match_operand:SVE_FULL_I 0 "register_operand" "=w, w, ?&w")
	(unspec:SVE_FULL_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl")
	   (unspec:SVE_FULL_I
	     [(match_operand:SVE_FULL_I 2 "register_operand" "0, w, w")
	      (match_operand:SVE_FULL_I 3 "register_operand" "w, 0, w")]
	     SVE2_COND_INT_BINARY_REV)]
	  UNSPEC_PRED_X))]
  "TARGET_SVE2"
  "@
   <sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   <sve_int_op_rev>\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>
   movprfx\t%0, %2\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>"
  [(set_attr "movprfx" "*,*,yes")]
)

;; Predicated binary arithmetic with merging.
(define_expand "@cond_<sve_int_op><mode>"
  [(set (match_operand:SVE_FULL_I 0 "register_operand")
	(unspec:SVE_FULL_I
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_I
	     [(match_dup 5)
	      (unspec:SVE_FULL_I
		[(match_operand:SVE_FULL_I 2 "register_operand")
		 (match_operand:SVE_FULL_I 3 "register_operand")]
		SVE2_COND_INT_BINARY)]
	     UNSPEC_PRED_X)
	   (match_operand:SVE_FULL_I 4 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE2"
  {
    operands[5] = CONSTM1_RTX (<MODE>mode);
  }
)

;; Predicated binary arithmetic, merging with the first input.
(define_insn_and_rewrite "*cond_<sve_int_op><mode>_2"
  [(set (match_operand:SVE_FULL_I 0 "register_operand" "=w, ?&w")
	(unspec:SVE_FULL_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (unspec:SVE_FULL_I
	     [(match_operand 4)
	      (unspec:SVE_FULL_I
		[(match_operand:SVE_FULL_I 2 "register_operand" "0, w")
		 (match_operand:SVE_FULL_I 3 "register_operand" "w, w")]
		SVE2_COND_INT_BINARY)]
	     UNSPEC_PRED_X)
	   (match_dup 2)]
	  UNSPEC_SEL))]
  "TARGET_SVE2"
  "@
   <sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   movprfx\t%0, %2\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>"
  "&& !CONSTANT_P (operands[4])"
  {
    operands[4] = CONSTM1_RTX (<VPRED>mode);
  }
  [(set_attr "movprfx" "*,yes")]
)

;; Predicated binary arithmetic, merging with the second input.
(define_insn_and_rewrite "*cond_<sve_int_op><mode>_3"
  [(set (match_operand:SVE_FULL_I 0 "register_operand" "=w, ?&w")
	(unspec:SVE_FULL_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (unspec:SVE_FULL_I
	     [(match_operand 4)
	      (unspec:SVE_FULL_I
		[(match_operand:SVE_FULL_I 2 "register_operand" "w, w")
		 (match_operand:SVE_FULL_I 3 "register_operand" "0, w")]
		SVE2_COND_INT_BINARY_REV)]
	     UNSPEC_PRED_X)
	   (match_dup 3)]
	  UNSPEC_SEL))]
  "TARGET_SVE2"
  "@
   <sve_int_op_rev>\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>
   movprfx\t%0, %3\;<sve_int_op_rev>\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>"
  "&& !CONSTANT_P (operands[4])"
  {
    operands[4] = CONSTM1_RTX (<VPRED>mode);
  }
  [(set_attr "movprfx" "*,yes")]
)

;; Predicated binary operations, merging with an independent value.
(define_insn_and_rewrite "*cond_<sve_int_op><mode>_any"
  [(set (match_operand:SVE_FULL_I 0 "register_operand" "=&w, &w, &w, &w, ?&w")
	(unspec:SVE_FULL_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl, Upl, Upl")
	   (unspec:SVE_FULL_I
	     [(match_operand 5)
	      (unspec:SVE_FULL_I
		[(match_operand:SVE_FULL_I 2 "register_operand" "0, w, w, w, w")
		 (match_operand:SVE_FULL_I 3 "register_operand" "w, 0, w, w, w")]
		SVE2_COND_INT_BINARY_REV)]
	     UNSPEC_PRED_X)
	   (match_operand:SVE_FULL_I 4 "aarch64_simd_reg_or_zero" "Dz, Dz, Dz, 0, w")]
	  UNSPEC_SEL))]
  "TARGET_SVE2
   && !rtx_equal_p (operands[2], operands[4])
   && !rtx_equal_p (operands[3], operands[4])"
  "@
   movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;<sve_int_op_rev>\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>
   movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   movprfx\t%0.<Vetype>, %1/m, %2.<Vetype>\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   #"
  "&& 1"
  {
    if (reload_completed
        && register_operand (operands[4], <MODE>mode)
        && !rtx_equal_p (operands[0], operands[4]))
      {
	emit_insn (gen_vcond_mask_<mode><vpred> (operands[0], operands[2],
						 operands[4], operands[1]));
	operands[4] = operands[2] = operands[0];
      }
    else if (!CONSTANT_P (operands[5]))
      operands[5] = CONSTM1_RTX (<VPRED>mode);
    else
      FAIL;
  }
  [(set_attr "movprfx" "yes")]
)

;; Predicated binary operations with no reverse form, merging with zero.
;; At present we don't generate these patterns via a cond_* optab,
;; so there's no correctness requirement to handle merging with an
;; independent value.
(define_insn_and_rewrite "*cond_<sve_int_op><mode>_z"
  [(set (match_operand:SVE_FULL_I 0 "register_operand" "=&w, &w")
	(unspec:SVE_FULL_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (unspec:SVE_FULL_I
	     [(match_operand 5)
	      (unspec:SVE_FULL_I
		[(match_operand:SVE_FULL_I 2 "register_operand" "0, w")
		 (match_operand:SVE_FULL_I 3 "register_operand" "w, w")]
		SVE2_COND_INT_BINARY_NOREV)]
	     UNSPEC_PRED_X)
	   (match_operand:SVE_FULL_I 4 "aarch64_simd_imm_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE2"
  "@
   movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>"
  "&& !CONSTANT_P (operands[5])"
  {
    operands[5] = CONSTM1_RTX (<VPRED>mode);
  }
  [(set_attr "movprfx" "yes")]
)

;; -------------------------------------------------------------------------
;; ---- [INT] Saturating binary arithmetic
;; -------------------------------------------------------------------------
;; Includes:
;; - SQDMULH
;; - SQRDMULH
;; -------------------------------------------------------------------------

(define_insn "@aarch64_sve_<sve_int_op><mode>"
  [(set (match_operand:SVE_FULL_I 0 "register_operand" "=w")
	(unspec:SVE_FULL_I
	  [(match_operand:SVE_FULL_I 1 "register_operand" "w")
	   (match_operand:SVE_FULL_I 2 "register_operand" "w")]
	  SVE2_INT_BINARY))]
  "TARGET_SVE2"
  "<sve_int_op>\t%0.<Vetype>, %1.<Vetype>, %2.<Vetype>"
)

(define_insn "@aarch64_sve_<sve_int_op>_lane_<mode>"
  [(set (match_operand:SVE_FULL_HSDI 0 "register_operand" "=w")
	(unspec:SVE_FULL_HSDI
	  [(match_operand:SVE_FULL_HSDI 1 "register_operand" "w")
	   (unspec:SVE_FULL_HSDI
	     [(match_operand:SVE_FULL_HSDI 2 "register_operand" "<sve_lane_con>")
	      (match_operand:SI 3 "const_int_operand")]
	     UNSPEC_SVE_LANE_SELECT)]
	  SVE2_INT_BINARY_LANE))]
  "TARGET_SVE2"
  "<sve_int_op>\t%0.<Vetype>, %1.<Vetype>, %2.<Vetype>[%3]"
)

;; -------------------------------------------------------------------------
;; ---- [INT] Saturating left shifts
;; -------------------------------------------------------------------------
;; Includes:
;; - SQSHL
;; - SQSHLR
;; - UQSHL
;; - UQSHLR
;; -------------------------------------------------------------------------

;; Predicated left shifts.
(define_insn "@aarch64_pred_<sve_int_op><mode>"
  [(set (match_operand:SVE_FULL_I 0 "register_operand" "=w, w, w, ?&w, ?&w")
	(unspec:SVE_FULL_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl, Upl, Upl")
	   (unspec:SVE_FULL_I
	     [(match_operand:SVE_FULL_I 2 "register_operand" "0, 0, w, w, w")
	      (match_operand:SVE_FULL_I 3 "aarch64_sve_<lr>shift_operand" "D<lr>, w, 0, D<lr>, w")]
	     SVE2_COND_INT_SHIFT)]
	  UNSPEC_PRED_X))]
  "TARGET_SVE2"
  "@
   <sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
   <sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   <sve_int_op>r\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>
   movprfx\t%0, %2\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
   movprfx\t%0, %2\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>"
  [(set_attr "movprfx" "*,*,*,yes,yes")]
)

;; Predicated left shifts with merging.
(define_expand "@cond_<sve_int_op><mode>"
  [(set (match_operand:SVE_FULL_I 0 "register_operand")
	(unspec:SVE_FULL_I
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_I
	     [(match_dup 5)
	      (unspec:SVE_FULL_I
		[(match_operand:SVE_FULL_I 2 "register_operand")
		 (match_operand:SVE_FULL_I 3 "aarch64_sve_<lr>shift_operand")]
		SVE2_COND_INT_SHIFT)]
	     UNSPEC_PRED_X)
	   (match_operand:SVE_FULL_I 4 "register_operand")]
	  UNSPEC_SEL))]
  "TARGET_SVE2"
  {
    operands[5] = CONSTM1_RTX (<VPRED>mode);
  }
)

;; Predicated left shifts, merging with the first input.
(define_insn_and_rewrite "*cond_<sve_int_op><mode>_2"
  [(set (match_operand:SVE_FULL_I 0 "register_operand" "=w, w, ?&w, ?&w")
	(unspec:SVE_FULL_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl, Upl")
	   (unspec:SVE_FULL_I
	     [(match_operand 4)
	      (unspec:SVE_FULL_I
		[(match_operand:SVE_FULL_I 2 "register_operand" "0, 0, w, w")
		 (match_operand:SVE_FULL_I 3 "aarch64_sve_<lr>shift_operand" "D<lr>, w, D<lr>, w")]
		SVE2_COND_INT_SHIFT)]
	     UNSPEC_PRED_X)
	   (match_dup 2)]
	  UNSPEC_SEL))]
  "TARGET_SVE2"
  "@
   <sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
   <sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   movprfx\t%0, %2\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
   movprfx\t%0, %2\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>"
  "&& !CONSTANT_P (operands[4])"
  {
    operands[4] = CONSTM1_RTX (<VPRED>mode);
  }
  [(set_attr "movprfx" "*,*,yes,yes")]
)

;; Predicated left shifts, merging with the second input.
(define_insn_and_rewrite "*cond_<sve_int_op><mode>_3"
  [(set (match_operand:SVE_FULL_I 0 "register_operand" "=w, ?&w")
	(unspec:SVE_FULL_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (unspec:SVE_FULL_I
	     [(match_operand 4)
	      (unspec:SVE_FULL_I
		[(match_operand:SVE_FULL_I 2 "register_operand" "w, w")
		 (match_operand:SVE_FULL_I 3 "register_operand" "0, w")]
		SVE2_COND_INT_SHIFT)]
	     UNSPEC_PRED_X)
	   (match_dup 3)]
	  UNSPEC_SEL))]
  "TARGET_SVE2"
  "@
   <sve_int_op>r\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>
   movprfx\t%0, %3\;<sve_int_op>r\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>"
  "&& !CONSTANT_P (operands[4])"
  {
    operands[4] = CONSTM1_RTX (<VPRED>mode);
  }
  [(set_attr "movprfx" "*,yes")]
)

;; Predicated left shifts, merging with an independent value.
(define_insn_and_rewrite "*cond_<sve_int_op><mode>_any"
  [(set (match_operand:SVE_FULL_I 0 "register_operand" "=&w, &w, &w, &w, &w, &w, &w, ?&w, ?&w")
	(unspec:SVE_FULL_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl, Upl, Upl, Upl, Upl, Upl, Upl")
	   (unspec:SVE_FULL_I
	     [(match_operand 5)
	      (unspec:SVE_FULL_I
		[(match_operand:SVE_FULL_I 2 "register_operand" "0, 0, w, w, w, w, w, w, w")
		 (match_operand:SVE_FULL_I 3 "aarch64_sve_<lr>shift_operand" "D<lr>, w, 0, D<lr>, w, D<lr>, w, D<lr>, w")]
		SVE2_COND_INT_SHIFT)]
	     UNSPEC_PRED_X)
	   (match_operand:SVE_FULL_I 4 "aarch64_simd_reg_or_zero" "Dz, Dz, Dz, Dz, Dz, 0, 0, w, w")]
	  UNSPEC_SEL))]
  "TARGET_SVE2
   && !rtx_equal_p (operands[2], operands[4])
   && (CONSTANT_P (operands[4]) || !rtx_equal_p (operands[3], operands[4]))"
  "@
   movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
   movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;<sve_int_op>r\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>
   movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
   movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   movprfx\t%0.<Vetype>, %1/m, %2.<Vetype>\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
   movprfx\t%0.<Vetype>, %1/m, %2.<Vetype>\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
  #
  #"
  "&& 1"
  {
    if (reload_completed
        && register_operand (operands[4], <MODE>mode)
        && !rtx_equal_p (operands[0], operands[4]))
      {
	emit_insn (gen_vcond_mask_<mode><vpred> (operands[0], operands[2],
						 operands[4], operands[1]));
	operands[4] = operands[2] = operands[0];
      }
    else if (!CONSTANT_P (operands[5]))
      operands[5] = CONSTM1_RTX (<VPRED>mode);
    else
      FAIL;
  }
  [(set_attr "movprfx" "yes")]
)

;; =========================================================================
;; == Uniform ternary arithmnetic
;; =========================================================================

;; -------------------------------------------------------------------------
;; ---- [INT] General ternary arithmetic that maps to unspecs
;; -------------------------------------------------------------------------
;; Includes:
;; - ADCLB
;; - ADCLT
;; - EORBT
;; - EORTB
;; - SBCLB
;; - SBCLT
;; - SQRDMLAH
;; - SQRDMLSH
;; -------------------------------------------------------------------------

(define_insn "@aarch64_sve_<sve_int_op><mode>"
  [(set (match_operand:SVE_FULL_I 0 "register_operand" "=w, ?&w")
	(unspec:SVE_FULL_I
	  [(match_operand:SVE_FULL_I 2 "register_operand" "w, w")
	   (match_operand:SVE_FULL_I 3 "register_operand" "w, w")
	   (match_operand:SVE_FULL_I 1 "register_operand" "0, w")]
	  SVE2_INT_TERNARY))]
  "TARGET_SVE2"
  "@
   <sve_int_op>\t%0.<Vetype>, %2.<Vetype>, %3.<Vetype>
   movprfx\t%0, %1\;<sve_int_op>\t%0.<Vetype>, %2.<Vetype>, %3.<Vetype>"
  [(set_attr "movprfx" "*,yes")]
)

(define_insn "@aarch64_sve_<sve_int_op>_lane_<mode>"
  [(set (match_operand:SVE_FULL_HSDI 0 "register_operand" "=w, ?&w")
	(unspec:SVE_FULL_HSDI
	  [(match_operand:SVE_FULL_HSDI 2 "register_operand" "w, w")
	   (unspec:SVE_FULL_HSDI
	     [(match_operand:SVE_FULL_HSDI 3 "register_operand" "<sve_lane_con>, <sve_lane_con>")
	      (match_operand:SI 4 "const_int_operand")]
	     UNSPEC_SVE_LANE_SELECT)
	   (match_operand:SVE_FULL_HSDI 1 "register_operand" "0, w")]
	  SVE2_INT_TERNARY_LANE))]
  "TARGET_SVE2"
  "@
   <sve_int_op>\t%0.<Vetype>, %2.<Vetype>, %3.<Vetype>[%4]
   movprfx\t%0, %1\;<sve_int_op>\t%0.<Vetype>, %2.<Vetype>, %3.<Vetype>[%4]"
  [(set_attr "movprfx" "*,yes")]
)

;; -------------------------------------------------------------------------
;; ---- [INT] Multiply-and-accumulate operations
;; -------------------------------------------------------------------------
;; Includes the lane forms of:
;; - MLA
;; - MLS
;; -------------------------------------------------------------------------

(define_insn "@aarch64_sve_add_mul_lane_<mode>"
  [(set (match_operand:SVE_FULL_HSDI 0 "register_operand" "=w, ?&w")
	(plus:SVE_FULL_HSDI
	  (mult:SVE_FULL_HSDI
	    (unspec:SVE_FULL_HSDI
	      [(match_operand:SVE_FULL_HSDI 3 "register_operand" "<sve_lane_con>, <sve_lane_con>")
	       (match_operand:SI 4 "const_int_operand")]
	      UNSPEC_SVE_LANE_SELECT)
	    (match_operand:SVE_FULL_HSDI 2 "register_operand" "w, w"))
	  (match_operand:SVE_FULL_HSDI 1 "register_operand" "0, w")))]
  "TARGET_SVE2"
  "@
   mla\t%0.<Vetype>, %2.<Vetype>, %3.<Vetype>[%4]
   movprfx\t%0, %1\;mla\t%0.<Vetype>, %2.<Vetype>, %3.<Vetype>[%4]"
  [(set_attr "movprfx" "*,yes")]
)

(define_insn "@aarch64_sve_sub_mul_lane_<mode>"
  [(set (match_operand:SVE_FULL_HSDI 0 "register_operand" "=w, ?&w")
	(minus:SVE_FULL_HSDI
	  (match_operand:SVE_FULL_HSDI 1 "register_operand" "0, w")
	  (mult:SVE_FULL_HSDI
	    (unspec:SVE_FULL_HSDI
	      [(match_operand:SVE_FULL_HSDI 3 "register_operand" "<sve_lane_con>, <sve_lane_con>")
	       (match_operand:SI 4 "const_int_operand")]
	      UNSPEC_SVE_LANE_SELECT)
	    (match_operand:SVE_FULL_HSDI 2 "register_operand" "w, w"))))]
  "TARGET_SVE2"
  "@
   mls\t%0.<Vetype>, %2.<Vetype>, %3.<Vetype>[%4]
   movprfx\t%0, %1\;mls\t%0.<Vetype>, %2.<Vetype>, %3.<Vetype>[%4]"
  [(set_attr "movprfx" "*,yes")]
)

;; -------------------------------------------------------------------------
;; ---- [INT] Binary logic operations with rotation
;; -------------------------------------------------------------------------
;; Includes:
;; - XAR
;; -------------------------------------------------------------------------

(define_insn "@aarch64_sve2_xar<mode>"
  [(set (match_operand:SVE_FULL_I 0 "register_operand" "=w, ?&w")
	(rotatert:SVE_FULL_I
	  (xor:SVE_FULL_I
	    (match_operand:SVE_FULL_I 1 "register_operand" "%0, w")
	    (match_operand:SVE_FULL_I 2 "register_operand" "w, w"))
	  (match_operand:SVE_FULL_I 3 "aarch64_simd_rshift_imm")))]
  "TARGET_SVE2"
  "@
  xar\t%0.<Vetype>, %0.<Vetype>, %2.<Vetype>, #%3
  movprfx\t%0, %1\;xar\t%0.<Vetype>, %0.<Vetype>, %2.<Vetype>, #%3"
  [(set_attr "movprfx" "*,yes")]
)

;; -------------------------------------------------------------------------
;; ---- [INT] Ternary logic operations
;; -------------------------------------------------------------------------
;; Includes:
;; - BCAX
;; - BSL
;; - BSL1N
;; - BSL2N
;; - EOR3
;; - NBSL
;; -------------------------------------------------------------------------

;; Unpredicated exclusive OR of AND.
(define_expand "@aarch64_sve2_bcax<mode>"
  [(set (match_operand:SVE_FULL_I 0 "register_operand")
	(xor:SVE_FULL_I
	  (and:SVE_FULL_I
	    (unspec:SVE_FULL_I
	      [(match_dup 4)
	       (not:SVE_FULL_I
		 (match_operand:SVE_FULL_I 3 "register_operand"))]
	      UNSPEC_PRED_X)
	    (match_operand:SVE_FULL_I 2 "register_operand"))
	  (match_operand:SVE_FULL_I 1 "register_operand")))]
  "TARGET_SVE2"
  {
    operands[4] = CONSTM1_RTX (<VPRED>mode);
  }
)

(define_insn_and_rewrite "*aarch64_sve2_bcax<mode>"
  [(set (match_operand:SVE_FULL_I 0 "register_operand" "=w, ?&w")
	(xor:SVE_FULL_I
	  (and:SVE_FULL_I
	    (unspec:SVE_FULL_I
	      [(match_operand 4)
	       (not:SVE_FULL_I
		 (match_operand:SVE_FULL_I 3 "register_operand" "w, w"))]
	      UNSPEC_PRED_X)
	    (match_operand:SVE_FULL_I 2 "register_operand" "w, w"))
	  (match_operand:SVE_FULL_I 1 "register_operand" "0, w")))]
  "TARGET_SVE2"
  "@
  bcax\t%0.d, %0.d, %2.d, %3.d
  movprfx\t%0, %1\;bcax\t%0.d, %0.d, %2.d, %3.d"
  "&& !CONSTANT_P (operands[4])"
  {
    operands[4] = CONSTM1_RTX (<VPRED>mode);
  }
  [(set_attr "movprfx" "*,yes")]
)

;; Unpredicated 3-way exclusive OR.
(define_insn "@aarch64_sve2_eor3<mode>"
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
(define_expand "@aarch64_sve2_bsl<mode>"
  [(set (match_operand:SVE_FULL_I 0 "register_operand")
	(xor:SVE_FULL_I
	  (and:SVE_FULL_I
	    (xor:SVE_FULL_I
	      (match_operand:SVE_FULL_I 1 "register_operand")
	      (match_operand:SVE_FULL_I 2 "register_operand"))
	    (match_operand:SVE_FULL_I 3 "register_operand"))
	  (match_dup 2)))]
  "TARGET_SVE2"
)

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
(define_expand "@aarch64_sve2_nbsl<mode>"
  [(set (match_operand:SVE_FULL_I 0 "register_operand")
	(unspec:SVE_FULL_I
	  [(match_dup 4)
	   (not:SVE_FULL_I
	     (xor:SVE_FULL_I
	       (and:SVE_FULL_I
		 (xor:SVE_FULL_I
		   (match_operand:SVE_FULL_I 1 "register_operand")
		   (match_operand:SVE_FULL_I 2 "register_operand"))
		 (match_operand:SVE_FULL_I 3 "register_operand"))
	       (match_dup 2)))]
	  UNSPEC_PRED_X))]
  "TARGET_SVE2"
  {
    operands[4] = CONSTM1_RTX (<VPRED>mode);
  }
)

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
(define_expand "@aarch64_sve2_bsl1n<mode>"
  [(set (match_operand:SVE_FULL_I 0 "register_operand")
	(xor:SVE_FULL_I
	  (and:SVE_FULL_I
	    (unspec:SVE_FULL_I
	      [(match_dup 4)
	       (not:SVE_FULL_I
		 (xor:SVE_FULL_I
		   (match_operand:SVE_FULL_I 1 "register_operand")
		   (match_operand:SVE_FULL_I 2 "register_operand")))]
	      UNSPEC_PRED_X)
	    (match_operand:SVE_FULL_I 3 "register_operand"))
	  (match_dup 2)))]
  "TARGET_SVE2"
  {
    operands[4] = CONSTM1_RTX (<VPRED>mode);
  }
)

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
(define_expand "@aarch64_sve2_bsl2n<mode>"
  [(set (match_operand:SVE_FULL_I 0 "register_operand")
	(ior:SVE_FULL_I
	  (and:SVE_FULL_I
	    (match_operand:SVE_FULL_I 1 "register_operand")
	    (match_operand:SVE_FULL_I 3 "register_operand"))
	  (unspec:SVE_FULL_I
	    [(match_dup 4)
	     (and:SVE_FULL_I
	       (not:SVE_FULL_I
		 (match_operand:SVE_FULL_I 2 "register_operand"))
	       (not:SVE_FULL_I
		 (match_dup 3)))]
	    UNSPEC_PRED_X)))]
  "TARGET_SVE2"
  {
    operands[4] = CONSTM1_RTX (<VPRED>mode);
  }
)

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

;; -------------------------------------------------------------------------
;; ---- [INT] Shift-and-accumulate operations
;; -------------------------------------------------------------------------
;; Includes:
;; - SRSRA
;; - SSRA
;; - URSRA
;; - USRA
;; -------------------------------------------------------------------------

;; Provide the natural unpredicated interface for SSRA and USRA.
(define_expand "@aarch64_sve_add_<sve_int_op><mode>"
  [(set (match_operand:SVE_FULL_I 0 "register_operand")
	(plus:SVE_FULL_I
	  (unspec:SVE_FULL_I
	    [(match_dup 4)
	     (SHIFTRT:SVE_FULL_I
	       (match_operand:SVE_FULL_I 2 "register_operand")
	       (match_operand:SVE_FULL_I 3 "aarch64_simd_rshift_imm"))]
	    UNSPEC_PRED_X)
	 (match_operand:SVE_FULL_I 1 "register_operand")))]
  "TARGET_SVE2"
  {
    operands[4] = CONSTM1_RTX (<VPRED>mode);
  }
)

;; Pattern-match SSRA and USRA as a predicated operation whose predicate
;; isn't needed.
(define_insn_and_rewrite "*aarch64_sve2_sra<mode>"
  [(set (match_operand:SVE_FULL_I 0 "register_operand" "=w, ?&w")
	(plus:SVE_FULL_I
	  (unspec:SVE_FULL_I
	    [(match_operand 4)
	     (SHIFTRT:SVE_FULL_I
	       (match_operand:SVE_FULL_I 2 "register_operand" "w, w")
	       (match_operand:SVE_FULL_I 3 "aarch64_simd_rshift_imm"))]
	    UNSPEC_PRED_X)
	 (match_operand:SVE_FULL_I 1 "register_operand" "0, w")))]
  "TARGET_SVE2"
  "@
   <sra_op>sra\t%0.<Vetype>, %2.<Vetype>, #%3
   movprfx\t%0, %1\;<sra_op>sra\t%0.<Vetype>, %2.<Vetype>, #%3"
  "&& !CONSTANT_P (operands[4])"
  {
    operands[4] = CONSTM1_RTX (<VPRED>mode);
  }
  [(set_attr "movprfx" "*,yes")]
)

;; SRSRA and URSRA.
(define_insn "@aarch64_sve_add_<sve_int_op><mode>"
  [(set (match_operand:SVE_FULL_I 0 "register_operand" "=w, ?&w")
	(plus:SVE_FULL_I
	  (unspec:SVE_FULL_I
	    [(match_operand:SVE_FULL_I 2 "register_operand" "w, w")
	     (match_operand:SVE_FULL_I 3 "aarch64_simd_rshift_imm")]
	    VRSHR_N)
	 (match_operand:SVE_FULL_I 1 "register_operand" "0, w")))]
  "TARGET_SVE2"
  "@
   <sur>sra\t%0.<Vetype>, %2.<Vetype>, #%3
   movprfx\t%0, %1\;<sur>sra\t%0.<Vetype>, %2.<Vetype>, #%3"
  [(set_attr "movprfx" "*,yes")]
)

;; -------------------------------------------------------------------------
;; ---- [INT] Shift-and-insert operations
;; -------------------------------------------------------------------------
;; Includes:
;; - SLI
;; - SRI
;; -------------------------------------------------------------------------

;; These instructions do not take MOVPRFX.
(define_insn "@aarch64_sve_<sve_int_op><mode>"
  [(set (match_operand:SVE_FULL_I 0 "register_operand" "=w")
	(unspec:SVE_FULL_I
	  [(match_operand:SVE_FULL_I 1 "register_operand" "0")
	   (match_operand:SVE_FULL_I 2 "register_operand" "w")
	   (match_operand:SVE_FULL_I 3 "aarch64_simd_<lr>shift_imm")]
	  SVE2_INT_SHIFT_INSERT))]
  "TARGET_SVE2"
  "<sve_int_op>\t%0.<Vetype>, %2.<Vetype>, #%3"
)

;; -------------------------------------------------------------------------
;; ---- [INT] Sum of absolute differences
;; -------------------------------------------------------------------------
;; Includes:
;; - SABA
;; - UABA
;; -------------------------------------------------------------------------

;; Provide the natural unpredicated interface for SABA and UABA.
(define_expand "@aarch64_sve2_<su>aba<mode>"
  [(set (match_operand:SVE_FULL_I 0 "register_operand" "=w, ?&w")
	(plus:SVE_FULL_I
	  (minus:SVE_FULL_I
	    (unspec:SVE_FULL_I
	      [(match_dup 4)
	       (USMAX:SVE_FULL_I
		 (match_operand:SVE_FULL_I 2 "register_operand" "w, w")
		 (match_operand:SVE_FULL_I 3 "register_operand" "w, w"))]
	      UNSPEC_PRED_X)
	    (unspec:SVE_FULL_I
	      [(match_dup 4)
	       (<max_opp>:SVE_FULL_I
		 (match_dup 2)
		 (match_dup 3))]
	      UNSPEC_PRED_X))
	  (match_operand:SVE_FULL_I 1 "register_operand" "0, w")))]
  "TARGET_SVE2"
  {
    operands[4] = CONSTM1_RTX (<VPRED>mode);
  }
)

;; Pattern-match SABA and UABA as an absolute-difference-and-accumulate
;; operation whose predicates aren't needed.
(define_insn "*aarch64_sve2_<su>aba<mode>"
  [(set (match_operand:SVE_FULL_I 0 "register_operand" "=w, ?&w")
	(plus:SVE_FULL_I
	  (minus:SVE_FULL_I
	    (unspec:SVE_FULL_I
	      [(match_operand 4)
	       (USMAX:SVE_FULL_I
		 (match_operand:SVE_FULL_I 2 "register_operand" "w, w")
		 (match_operand:SVE_FULL_I 3 "register_operand" "w, w"))]
	      UNSPEC_PRED_X)
	    (unspec:SVE_FULL_I
	      [(match_operand 5)
	       (<max_opp>:SVE_FULL_I
		 (match_dup 2)
		 (match_dup 3))]
	      UNSPEC_PRED_X))
	  (match_operand:SVE_FULL_I 1 "register_operand" "0, w")))]
  "TARGET_SVE2"
  "@
   <su>aba\t%0.<Vetype>, %2.<Vetype>, %3.<Vetype>
   movprfx\t%0, %1\;<su>aba\t%0.<Vetype>, %2.<Vetype>, %3.<Vetype>"
  [(set_attr "movprfx" "*,yes")]
)

;; =========================================================================
;; == Extending arithmetic
;; =========================================================================

;; -------------------------------------------------------------------------
;; ---- [INT] Wide binary arithmetic
;; -------------------------------------------------------------------------
;; Includes:
;; - SADDWB
;; - SADDWT
;; - SSUBWB
;; - SSUBWT
;; - UADDWB
;; - UADDWT
;; - USUBWB
;; - USUBWT
;; -------------------------------------------------------------------------

(define_insn "@aarch64_sve_<sve_int_op><mode>"
  [(set (match_operand:SVE_FULL_HSDI 0 "register_operand" "=w")
	(unspec:SVE_FULL_HSDI
	  [(match_operand:SVE_FULL_HSDI 1 "register_operand" "w")
	   (match_operand:<VNARROW> 2 "register_operand" "w")]
	  SVE2_INT_BINARY_WIDE))]
  "TARGET_SVE2"
  "<sve_int_op>\t%0.<Vetype>, %1.<Vetype>, %2.<Ventype>"
)

;; -------------------------------------------------------------------------
;; ---- [INT] Long binary arithmetic
;; -------------------------------------------------------------------------
;; Includes:
;; - SABDLB
;; - SABDLT
;; - SADDLB
;; - SADDLBT
;; - SADDLT
;; - SMULLB
;; - SMULLT
;; - SQDMULLB
;; - SQDMULLT
;; - SSUBLB
;; - SSUBLBT
;; - SSUBLT
;; - SSUBLTB
;; - UABDLB
;; - UABDLT
;; - UADDLB
;; - UADDLT
;; - UMULLB
;; - UMULLT
;; - USUBLB
;; - USUBLT
;; -------------------------------------------------------------------------

(define_insn "@aarch64_sve_<sve_int_op><mode>"
  [(set (match_operand:SVE_FULL_HSDI 0 "register_operand" "=w")
	(unspec:SVE_FULL_HSDI
	  [(match_operand:<VNARROW> 1 "register_operand" "w")
	   (match_operand:<VNARROW> 2 "register_operand" "w")]
	  SVE2_INT_BINARY_LONG))]
  "TARGET_SVE2"
  "<sve_int_op>\t%0.<Vetype>, %1.<Ventype>, %2.<Ventype>"
)

(define_insn "@aarch64_sve_<sve_int_op>_lane_<mode>"
  [(set (match_operand:SVE_FULL_SDI 0 "register_operand" "=w")
	(unspec:SVE_FULL_SDI
	  [(match_operand:<VNARROW> 1 "register_operand" "w")
	   (unspec:<VNARROW>
	     [(match_operand:<VNARROW> 2 "register_operand" "<sve_lane_con>")
	      (match_operand:SI 3 "const_int_operand")]
	     UNSPEC_SVE_LANE_SELECT)]
	  SVE2_INT_BINARY_LONG_LANE))]
  "TARGET_SVE2"
  "<sve_int_op>\t%0.<Vetype>, %1.<Ventype>, %2.<Ventype>[%3]"
)

;; -------------------------------------------------------------------------
;; ---- [INT] Long left shifts
;; -------------------------------------------------------------------------
;; Includes:
;; - SSHLLB
;; - SSHLLT
;; - USHLLB
;; - USHLLT
;; -------------------------------------------------------------------------

;; The immediate range is enforced before generating the instruction.
(define_insn "@aarch64_sve_<sve_int_op><mode>"
  [(set (match_operand:SVE_FULL_HSDI 0 "register_operand" "=w")
	(unspec:SVE_FULL_HSDI
	  [(match_operand:<VNARROW> 1 "register_operand" "w")
	   (match_operand:DI 2 "const_int_operand")]
	  SVE2_INT_SHIFT_IMM_LONG))]
  "TARGET_SVE2"
  "<sve_int_op>\t%0.<Vetype>, %1.<Ventype>, #%2"
)

;; -------------------------------------------------------------------------
;; ---- [INT] Long binary arithmetic with accumulation
;; -------------------------------------------------------------------------
;; Includes:
;; - SABALB
;; - SABALT
;; - SMLALB
;; - SMLALT
;; - SMLSLB
;; - SMLSLT
;; - SQDMLALB
;; - SQDMLALBT
;; - SQDMLALT
;; - SQDMLSLB
;; - SQDMLSLBT
;; - SQDMLSLT
;; - UABALB
;; - UABALT
;; - UMLALB
;; - UMLALT
;; - UMLSLB
;; - UMLSLT
;; -------------------------------------------------------------------------

;; Non-saturating MLA operations.
(define_insn "@aarch64_sve_add_<sve_int_op><mode>"
  [(set (match_operand:SVE_FULL_HSDI 0 "register_operand" "=w, ?&w")
	(plus:SVE_FULL_HSDI
	  (unspec:SVE_FULL_HSDI
	    [(match_operand:<VNARROW> 2 "register_operand" "w, w")
	     (match_operand:<VNARROW> 3 "register_operand" "w, w")]
	    SVE2_INT_ADD_BINARY_LONG)
	  (match_operand:SVE_FULL_HSDI 1 "register_operand" "0, w")))]
  "TARGET_SVE2"
  "@
   <sve_int_add_op>\t%0.<Vetype>, %2.<Ventype>, %3.<Ventype>
   movprfx\t%0, %1\;<sve_int_add_op>\t%0.<Vetype>, %2.<Ventype>, %3.<Ventype>"
  [(set_attr "movprfx" "*,yes")]
)

;; Non-saturating MLA operations with lane select.
(define_insn "@aarch64_sve_add_<sve_int_op>_lane_<mode>"
  [(set (match_operand:SVE_FULL_SDI 0 "register_operand" "=w, ?&w")
	(plus:SVE_FULL_SDI
	  (unspec:SVE_FULL_SDI
	    [(match_operand:<VNARROW> 2 "register_operand" "w, w")
	     (unspec:<VNARROW>
	       [(match_operand:<VNARROW> 3 "register_operand" "<sve_lane_con>, <sve_lane_con>")
		(match_operand:SI 4 "const_int_operand")]
	       UNSPEC_SVE_LANE_SELECT)]
	    SVE2_INT_ADD_BINARY_LONG_LANE)
	  (match_operand:SVE_FULL_SDI 1 "register_operand" "0, w")))]
  "TARGET_SVE2"
  "@
   <sve_int_add_op>\t%0.<Vetype>, %2.<Ventype>, %3.<Ventype>[%4]
   movprfx\t%0, %1\;<sve_int_add_op>\t%0.<Vetype>, %2.<Ventype>, %3.<Ventype>[%4]"
  [(set_attr "movprfx" "*,yes")]
)

;; Saturating MLA operations.
(define_insn "@aarch64_sve_qadd_<sve_int_op><mode>"
  [(set (match_operand:SVE_FULL_HSDI 0 "register_operand" "=w, ?&w")
	(ss_plus:SVE_FULL_HSDI
	  (unspec:SVE_FULL_HSDI
	    [(match_operand:<VNARROW> 2 "register_operand" "w, w")
	     (match_operand:<VNARROW> 3 "register_operand" "w, w")]
	    SVE2_INT_QADD_BINARY_LONG)
	  (match_operand:SVE_FULL_HSDI 1 "register_operand" "0, w")))]
  "TARGET_SVE2"
  "@
   <sve_int_qadd_op>\t%0.<Vetype>, %2.<Ventype>, %3.<Ventype>
   movprfx\t%0, %1\;<sve_int_qadd_op>\t%0.<Vetype>, %2.<Ventype>, %3.<Ventype>"
  [(set_attr "movprfx" "*,yes")]
)

;; Saturating MLA operations with lane select.
(define_insn "@aarch64_sve_qadd_<sve_int_op>_lane_<mode>"
  [(set (match_operand:SVE_FULL_SDI 0 "register_operand" "=w, ?&w")
	(ss_plus:SVE_FULL_SDI
	  (unspec:SVE_FULL_SDI
	    [(match_operand:<VNARROW> 2 "register_operand" "w, w")
	     (unspec:<VNARROW>
	       [(match_operand:<VNARROW> 3 "register_operand" "<sve_lane_con>, <sve_lane_con>")
		(match_operand:SI 4 "const_int_operand")]
	       UNSPEC_SVE_LANE_SELECT)]
	    SVE2_INT_QADD_BINARY_LONG_LANE)
	  (match_operand:SVE_FULL_SDI 1 "register_operand" "0, w")))]
  "TARGET_SVE2"
  "@
   <sve_int_qadd_op>\t%0.<Vetype>, %2.<Ventype>, %3.<Ventype>[%4]
   movprfx\t%0, %1\;<sve_int_qadd_op>\t%0.<Vetype>, %2.<Ventype>, %3.<Ventype>[%4]"
  [(set_attr "movprfx" "*,yes")]
)

;; Non-saturating MLS operations.
(define_insn "@aarch64_sve_sub_<sve_int_op><mode>"
  [(set (match_operand:SVE_FULL_HSDI 0 "register_operand" "=w, ?&w")
	(minus:SVE_FULL_HSDI
	  (match_operand:SVE_FULL_HSDI 1 "register_operand" "0, w")
	  (unspec:SVE_FULL_HSDI
	    [(match_operand:<VNARROW> 2 "register_operand" "w, w")
	     (match_operand:<VNARROW> 3 "register_operand" "w, w")]
	    SVE2_INT_SUB_BINARY_LONG)))]
  "TARGET_SVE2"
  "@
   <sve_int_sub_op>\t%0.<Vetype>, %2.<Ventype>, %3.<Ventype>
   movprfx\t%0, %1\;<sve_int_sub_op>\t%0.<Vetype>, %2.<Ventype>, %3.<Ventype>"
  [(set_attr "movprfx" "*,yes")]
)

;; Non-saturating MLS operations with lane select.
(define_insn "@aarch64_sve_sub_<sve_int_op>_lane_<mode>"
  [(set (match_operand:SVE_FULL_SDI 0 "register_operand" "=w, ?&w")
	(minus:SVE_FULL_SDI
	  (match_operand:SVE_FULL_SDI 1 "register_operand" "0, w")
	  (unspec:SVE_FULL_SDI
	    [(match_operand:<VNARROW> 2 "register_operand" "w, w")
	     (unspec:<VNARROW>
	       [(match_operand:<VNARROW> 3 "register_operand" "<sve_lane_con>, <sve_lane_con>")
		(match_operand:SI 4 "const_int_operand")]
	       UNSPEC_SVE_LANE_SELECT)]
	    SVE2_INT_SUB_BINARY_LONG_LANE)))]
  "TARGET_SVE2"
  "@
   <sve_int_sub_op>\t%0.<Vetype>, %2.<Ventype>, %3.<Ventype>[%4]
   movprfx\t%0, %1\;<sve_int_sub_op>\t%0.<Vetype>, %2.<Ventype>, %3.<Ventype>[%4]"
  [(set_attr "movprfx" "*,yes")]
)

;; Saturating MLS operations.
(define_insn "@aarch64_sve_qsub_<sve_int_op><mode>"
  [(set (match_operand:SVE_FULL_HSDI 0 "register_operand" "=w, ?&w")
	(ss_minus:SVE_FULL_HSDI
	  (match_operand:SVE_FULL_HSDI 1 "register_operand" "0, w")
	  (unspec:SVE_FULL_HSDI
	    [(match_operand:<VNARROW> 2 "register_operand" "w, w")
	     (match_operand:<VNARROW> 3 "register_operand" "w, w")]
	    SVE2_INT_QSUB_BINARY_LONG)))]
  "TARGET_SVE2"
  "@
   <sve_int_qsub_op>\t%0.<Vetype>, %2.<Ventype>, %3.<Ventype>
   movprfx\t%0, %1\;<sve_int_qsub_op>\t%0.<Vetype>, %2.<Ventype>, %3.<Ventype>"
  [(set_attr "movprfx" "*,yes")]
)

;; Saturating MLS operations with lane select.
(define_insn "@aarch64_sve_qsub_<sve_int_op>_lane_<mode>"
  [(set (match_operand:SVE_FULL_SDI 0 "register_operand" "=w, ?&w")
	(ss_minus:SVE_FULL_SDI
	  (match_operand:SVE_FULL_SDI 1 "register_operand" "0, w")
	  (unspec:SVE_FULL_SDI
	    [(match_operand:<VNARROW> 2 "register_operand" "w, w")
	     (unspec:<VNARROW>
	       [(match_operand:<VNARROW> 3 "register_operand" "<sve_lane_con>, <sve_lane_con>")
		(match_operand:SI 4 "const_int_operand")]
	       UNSPEC_SVE_LANE_SELECT)]
	    SVE2_INT_QSUB_BINARY_LONG_LANE)))]
  "TARGET_SVE2"
  "@
   <sve_int_qsub_op>\t%0.<Vetype>, %2.<Ventype>, %3.<Ventype>[%4]
   movprfx\t%0, %1\;<sve_int_qsub_op>\t%0.<Vetype>, %2.<Ventype>, %3.<Ventype>[%4]"
  [(set_attr "movprfx" "*,yes")]
)
;; -------------------------------------------------------------------------
;; ---- [FP] Long multiplication with accumulation
;; -------------------------------------------------------------------------
;; Includes:
;; - FMLALB
;; - FMLALT
;; - FMLSLB
;; - FMLSLT
;; -------------------------------------------------------------------------

(define_insn "@aarch64_sve_<sve_fp_op><mode>"
  [(set (match_operand:VNx4SF_ONLY 0 "register_operand" "=w, ?&w")
	(unspec:VNx4SF_ONLY
	  [(match_operand:<VNARROW> 1 "register_operand" "w, w")
	   (match_operand:<VNARROW> 2 "register_operand" "w, w")
	   (match_operand:VNx4SF_ONLY 3 "register_operand" "0, w")]
	  SVE2_FP_TERNARY_LONG))]
  "TARGET_SVE2"
  "@
   <sve_fp_op>\t%0.<Vetype>, %1.<Ventype>, %2.<Ventype>
   movprfx\t%0, %3\;<sve_fp_op>\t%0.<Vetype>, %1.<Ventype>, %2.<Ventype>"
  [(set_attr "movprfx" "*,yes")]
)

(define_insn "@aarch64_<sve_fp_op>_lane_<mode>"
  [(set (match_operand:VNx4SF_ONLY 0 "register_operand" "=w, ?&w")
	(unspec:VNx4SF_ONLY
	  [(match_operand:<VNARROW> 1 "register_operand" "w, w")
	   (unspec:<VNARROW>
	     [(match_operand:<VNARROW> 2 "register_operand" "<sve_lane_con>, <sve_lane_con>")
	      (match_operand:SI 3 "const_int_operand")]
	     UNSPEC_SVE_LANE_SELECT)
	   (match_operand:VNx4SF_ONLY 4 "register_operand" "0, w")]
	  SVE2_FP_TERNARY_LONG_LANE))]
  "TARGET_SVE2"
  "@
   <sve_fp_op>\t%0.<Vetype>, %1.<Ventype>, %2.<Ventype>[%3]
   movprfx\t%0, %4\;<sve_fp_op>\t%0.<Vetype>, %1.<Ventype>, %2.<Ventype>[%3]"
  [(set_attr "movprfx" "*,yes")]
)

;; =========================================================================
;; == Narrowing arithnetic
;; =========================================================================

;; -------------------------------------------------------------------------
;; ---- [INT] Narrowing unary arithmetic
;; -------------------------------------------------------------------------
;; Includes:
;; - SQXTNB
;; - SQXTNT
;; - SQXTUNB
;; - SQXTUNT
;; - UQXTNB
;; - UQXTNT
;; -------------------------------------------------------------------------

(define_insn "@aarch64_sve_<sve_int_op><mode>"
  [(set (match_operand:<VNARROW> 0 "register_operand" "=w")
	(unspec:<VNARROW>
	  [(match_operand:SVE_FULL_HSDI 1 "register_operand" "w")]
	  SVE2_INT_UNARY_NARROWB))]
  "TARGET_SVE2"
  "<sve_int_op>\t%0.<Ventype>, %1.<Vetype>"
)

;; These instructions do not take MOVPRFX.
(define_insn "@aarch64_sve_<sve_int_op><mode>"
  [(set (match_operand:<VNARROW> 0 "register_operand" "=w")
	(unspec:<VNARROW>
	  [(match_operand:<VNARROW> 1 "register_operand" "0")
	   (match_operand:SVE_FULL_HSDI 2 "register_operand" "w")]
	  SVE2_INT_UNARY_NARROWT))]
  "TARGET_SVE2"
  "<sve_int_op>\t%0.<Ventype>, %2.<Vetype>"
)

;; -------------------------------------------------------------------------
;; ---- [INT] Narrowing binary arithmetic
;; -------------------------------------------------------------------------
;; Includes:
;; - ADDHNB
;; - ADDHNT
;; - RADDHNB
;; - RADDHNT
;; - RSUBHNB
;; - RSUBHNT
;; - SUBHNB
;; - SUBHNT
;; -------------------------------------------------------------------------

(define_insn "@aarch64_sve_<sve_int_op><mode>"
  [(set (match_operand:<VNARROW> 0 "register_operand" "=w")
	(unspec:<VNARROW>
	  [(match_operand:SVE_FULL_HSDI 1 "register_operand" "w")
	   (match_operand:SVE_FULL_HSDI 2 "register_operand" "w")]
	  SVE2_INT_BINARY_NARROWB))]
  "TARGET_SVE2"
  "<sve_int_op>\t%0.<Ventype>, %1.<Vetype>, %2.<Vetype>"
)

;; These instructions do not take MOVPRFX.
(define_insn "@aarch64_sve_<sve_int_op><mode>"
  [(set (match_operand:<VNARROW> 0 "register_operand" "=w")
	(unspec:<VNARROW>
	  [(match_operand:<VNARROW> 1 "register_operand" "0")
	   (match_operand:SVE_FULL_HSDI 2 "register_operand" "w")
	   (match_operand:SVE_FULL_HSDI 3 "register_operand" "w")]
	  SVE2_INT_BINARY_NARROWT))]
  "TARGET_SVE2"
  "<sve_int_op>\t%0.<Ventype>, %2.<Vetype>, %3.<Vetype>"
)

;; Optimize ((a + b) >> n) where n is half the bitsize of the vector
(define_insn "*bitmask_shift_plus<mode>"
  [(set (match_operand:SVE_FULL_HSDI 0 "register_operand" "=w")
	(unspec:SVE_FULL_HSDI
	   [(match_operand:<VPRED> 1)
	    (lshiftrt:SVE_FULL_HSDI
	      (plus:SVE_FULL_HSDI
		(match_operand:SVE_FULL_HSDI 2 "register_operand" "w")
		(match_operand:SVE_FULL_HSDI 3 "register_operand" "w"))
	      (match_operand:SVE_FULL_HSDI 4
		 "aarch64_simd_shift_imm_vec_exact_top" ""))]
          UNSPEC_PRED_X))]
  "TARGET_SVE2"
  "addhnb\t%0.<Ventype>, %2.<Vetype>, %3.<Vetype>"
)

;; -------------------------------------------------------------------------
;; ---- [INT] Narrowing right shifts
;; -------------------------------------------------------------------------
;; Includes:
;; - RSHRNB
;; - RSHRNT
;; - SHRNB
;; - SHRNT
;; - SQRSHRNB
;; - SQRSHRNT
;; - SQRSHRUNB
;; - SQRSHRUNT
;; - SQSHRNB
;; - SQSHRNT
;; - SQSHRUNB
;; - SQSHRUNT
;; - UQRSHRNB
;; - UQRSHRNT
;; - UQSHRNB
;; - UQSHRNT
;; -------------------------------------------------------------------------

;; The immediate range is enforced before generating the instruction.
(define_insn "@aarch64_sve_<sve_int_op><mode>"
  [(set (match_operand:<VNARROW> 0 "register_operand" "=w")
	(unspec:<VNARROW>
	  [(match_operand:SVE_FULL_HSDI 1 "register_operand" "w")
	   (match_operand:DI 2 "const_int_operand")]
	  SVE2_INT_SHIFT_IMM_NARROWB))]
  "TARGET_SVE2"
  "<sve_int_op>\t%0.<Ventype>, %1.<Vetype>, #%2"
)

;; The immediate range is enforced before generating the instruction.
;; These instructions do not take MOVPRFX.
(define_insn "@aarch64_sve_<sve_int_op><mode>"
  [(set (match_operand:<VNARROW> 0 "register_operand" "=w")
	(unspec:<VNARROW>
	  [(match_operand:<VNARROW> 1 "register_operand" "0")
	   (match_operand:SVE_FULL_HSDI 2 "register_operand" "w")
	   (match_operand:DI 3 "const_int_operand")]
	  SVE2_INT_SHIFT_IMM_NARROWT))]
  "TARGET_SVE2"
  "<sve_int_op>\t%0.<Ventype>, %2.<Vetype>, #%3"
)

;; =========================================================================
;; == Pairwise arithmetic
;; =========================================================================

;; -------------------------------------------------------------------------
;; ---- [INT] Pairwise arithmetic
;; -------------------------------------------------------------------------
;; Includes:
;; - ADDP
;; - SMAXP
;; - SMINP
;; - UMAXP
;; - UMINP
;; -------------------------------------------------------------------------

(define_insn "@aarch64_pred_<sve_int_op><mode>"
  [(set (match_operand:SVE_FULL_I 0 "register_operand" "=w, ?&w")
	(unspec:SVE_FULL_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (match_operand:SVE_FULL_I 2 "register_operand" "0, w")
	   (match_operand:SVE_FULL_I 3 "register_operand" "w, w")]
	  SVE2_INT_BINARY_PAIR))]
  "TARGET_SVE2"
  "@
   <sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   movprfx\t%0, %2\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>"
  [(set_attr "movprfx" "*,yes")]
)

;; -------------------------------------------------------------------------
;; ---- [FP] Pairwise arithmetic
;; -------------------------------------------------------------------------
;; Includes:
;; - FADDP
;; - FMAXP
;; - FMAXNMP
;; - FMINP
;; - FMINNMP
;; -------------------------------------------------------------------------

(define_insn "@aarch64_pred_<sve_fp_op><mode>"
  [(set (match_operand:SVE_FULL_F 0 "register_operand" "=w, ?&w")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (match_operand:SVE_FULL_F 2 "register_operand" "0, w")
	   (match_operand:SVE_FULL_F 3 "register_operand" "w, w")]
	  SVE2_FP_BINARY_PAIR))]
  "TARGET_SVE2"
  "@
   <sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   movprfx\t%0, %2\;<sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>"
  [(set_attr "movprfx" "*,yes")]
)

;; -------------------------------------------------------------------------
;; ---- [INT] Pairwise arithmetic with accumulation
;; -------------------------------------------------------------------------
;; Includes:
;; - SADALP
;; - UADALP
;; -------------------------------------------------------------------------

;; Predicated pairwise absolute difference and accumulate with merging.
(define_expand "@cond_<sve_int_op><mode>"
  [(set (match_operand:SVE_FULL_HSDI 0 "register_operand")
	(unspec:SVE_FULL_HSDI
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_HSDI
	     [(match_dup 1)
	      (match_operand:SVE_FULL_HSDI 2 "register_operand")
	      (match_operand:<VNARROW> 3 "register_operand")]
	     SVE2_INT_BINARY_PAIR_LONG)
	   (match_operand:SVE_FULL_HSDI 4 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE2"
{
  /* Only target code is aware of these operations, so we don't need
     to handle the fully-general case.  */
  gcc_assert (rtx_equal_p (operands[2], operands[4])
	      || CONSTANT_P (operands[4]));
})

;; Predicated pairwise absolute difference and accumulate, merging with
;; the first input.
(define_insn_and_rewrite "*cond_<sve_int_op><mode>_2"
  [(set (match_operand:SVE_FULL_HSDI 0 "register_operand" "=w, ?&w")
	(unspec:SVE_FULL_HSDI
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (unspec:SVE_FULL_HSDI
	     [(match_operand 4)
	      (match_operand:SVE_FULL_HSDI 2 "register_operand" "0, w")
	      (match_operand:<VNARROW> 3 "register_operand" "w, w")]
	     SVE2_INT_BINARY_PAIR_LONG)
	   (match_dup 2)]
	  UNSPEC_SEL))]
  "TARGET_SVE2"
  "@
   <sve_int_op>\t%0.<Vetype>, %1/m, %3.<Ventype>
   movprfx\t%0, %2\;<sve_int_op>\t%0.<Vetype>, %1/m, %3.<Ventype>"
  "&& !CONSTANT_P (operands[4])"
  {
    operands[4] = CONSTM1_RTX (<VPRED>mode);
  }
  [(set_attr "movprfx" "*,yes")]
)

;; Predicated pairwise absolute difference and accumulate, merging with zero.
(define_insn_and_rewrite "*cond_<sve_int_op><mode>_z"
  [(set (match_operand:SVE_FULL_HSDI 0 "register_operand" "=&w, &w")
	(unspec:SVE_FULL_HSDI
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (unspec:SVE_FULL_HSDI
	     [(match_operand 5)
	      (match_operand:SVE_FULL_HSDI 2 "register_operand" "0, w")
	      (match_operand:<VNARROW> 3 "register_operand" "w, w")]
	     SVE2_INT_BINARY_PAIR_LONG)
	   (match_operand:SVE_FULL_HSDI 4 "aarch64_simd_imm_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE2"
  "@
   movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;<sve_int_op>\t%0.<Vetype>, %1/m, %3.<Ventype>
   movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;<sve_int_op>\t%0.<Vetype>, %1/m, %3.<Ventype>"
  "&& !CONSTANT_P (operands[5])"
  {
    operands[5] = CONSTM1_RTX (<VPRED>mode);
  }
  [(set_attr "movprfx" "yes")]
)

;; =========================================================================
;; == Complex arithmetic
;; =========================================================================

;; -------------------------------------------------------------------------
;; ---- [INT] Complex binary operations
;; -------------------------------------------------------------------------
;; Includes:
;; - CADD
;; - SQCADD
;; -------------------------------------------------------------------------

(define_insn "@aarch64_sve_<optab><mode>"
  [(set (match_operand:SVE_FULL_I 0 "register_operand" "=w, ?&w")
	(unspec:SVE_FULL_I
	  [(match_operand:SVE_FULL_I 1 "register_operand" "0, w")
	   (match_operand:SVE_FULL_I 2 "register_operand" "w, w")]
	  SVE2_INT_CADD))]
  "TARGET_SVE2"
  "@
   <sve_int_op>\t%0.<Vetype>, %0.<Vetype>, %2.<Vetype>, #<rot>
   movprfx\t%0, %1\;<sve_int_op>\t%0.<Vetype>, %0.<Vetype>, %2.<Vetype>, #<rot>"
  [(set_attr "movprfx" "*,yes")]
)

;; unpredicated optab pattern for auto-vectorizer
(define_expand "cadd<rot><mode>3"
  [(set (match_operand:SVE_FULL_I 0 "register_operand")
	(unspec:SVE_FULL_I
	  [(match_operand:SVE_FULL_I 1 "register_operand")
	   (match_operand:SVE_FULL_I 2 "register_operand")]
	  SVE2_INT_CADD_OP))]
  "TARGET_SVE2"
)

;; -------------------------------------------------------------------------
;; ---- [INT] Complex ternary operations
;; -------------------------------------------------------------------------
;; Includes:
;; - CMLA
;; - SQRDCMLA
;; -------------------------------------------------------------------------

(define_insn "@aarch64_sve_<optab><mode>"
  [(set (match_operand:SVE_FULL_I 0 "register_operand" "=w, ?&w")
	(unspec:SVE_FULL_I
	  [(match_operand:SVE_FULL_I 1 "register_operand" "0, w")
	   (match_operand:SVE_FULL_I 2 "register_operand" "w, w")
	   (match_operand:SVE_FULL_I 3 "register_operand" "w, w")]
	  SVE2_INT_CMLA))]
  "TARGET_SVE2"
  "@
   <sve_int_op>\t%0.<Vetype>, %2.<Vetype>, %3.<Vetype>, #<rot>
   movprfx\t%0, %1\;<sve_int_op>\t%0.<Vetype>, %2.<Vetype>, %3.<Vetype>, #<rot>"
  [(set_attr "movprfx" "*,yes")]
)

(define_insn "@aarch64_<optab>_lane_<mode>"
  [(set (match_operand:SVE_FULL_HSI 0 "register_operand" "=w, ?&w")
	(unspec:SVE_FULL_HSI
	  [(match_operand:SVE_FULL_HSI 1 "register_operand" "0, w")
	   (match_operand:SVE_FULL_HSI 2 "register_operand" "w, w")
	   (unspec:SVE_FULL_HSI
	     [(match_operand:SVE_FULL_HSI 3 "register_operand" "<sve_lane_con>, <sve_lane_con>")
	      (match_operand:SI 4 "const_int_operand")]
	     UNSPEC_SVE_LANE_SELECT)]
	  SVE2_INT_CMLA))]
  "TARGET_SVE2"
  "@
   <sve_int_op>\t%0.<Vetype>, %2.<Vetype>, %3.<Vetype>[%4], #<rot>
   movprfx\t%0, %1\;<sve_int_op>\t%0.<Vetype>, %2.<Vetype>, %3.<Vetype>[%4], #<rot>"
  [(set_attr "movprfx" "*,yes")]
)

;; unpredicated optab pattern for auto-vectorizer
;; The complex mla/mls operations always need to expand to two instructions.
;; The first operation does half the computation and the second does the
;; remainder.  Because of this, expand early.
(define_expand "cml<fcmac1><conj_op><mode>4"
  [(set (match_operand:SVE_FULL_I 0 "register_operand")
	(plus:SVE_FULL_I (match_operand:SVE_FULL_I 1 "register_operand")
	  (unspec:SVE_FULL_I
	    [(match_operand:SVE_FULL_I 2 "register_operand")
	     (match_operand:SVE_FULL_I 3 "register_operand")]
	    SVE2_INT_CMLA_OP)))]
  "TARGET_SVE2"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_aarch64_sve_cmla<sve_rot1><mode> (tmp, operands[1],
						   operands[3], operands[2]));
  emit_insn (gen_aarch64_sve_cmla<sve_rot2><mode> (operands[0], tmp,
						   operands[3], operands[2]));
  DONE;
})

;; unpredicated optab pattern for auto-vectorizer
;; The complex mul operations always need to expand to two instructions.
;; The first operation does half the computation and the second does the
;; remainder.  Because of this, expand early.
(define_expand "cmul<conj_op><mode>3"
  [(set (match_operand:SVE_FULL_I 0 "register_operand")
	(unspec:SVE_FULL_I
	  [(match_operand:SVE_FULL_I 1 "register_operand")
	   (match_operand:SVE_FULL_I 2 "register_operand")]
	  SVE2_INT_CMUL_OP))]
  "TARGET_SVE2"
{
  rtx accum = force_reg (<MODE>mode, CONST0_RTX (<MODE>mode));
  rtx tmp = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_aarch64_sve_cmla<sve_rot1><mode> (tmp, accum,
						   operands[2], operands[1]));
  emit_insn (gen_aarch64_sve_cmla<sve_rot2><mode> (operands[0], tmp,
						   operands[2], operands[1]));
  DONE;
})

;; -------------------------------------------------------------------------
;; ---- [INT] Complex dot product
;; -------------------------------------------------------------------------
;; Includes:
;; - CDOT
;; -------------------------------------------------------------------------

(define_insn "@aarch64_sve_<optab><mode>"
  [(set (match_operand:SVE_FULL_SDI 0 "register_operand" "=w, ?&w")
	(unspec:SVE_FULL_SDI
	  [(match_operand:SVE_FULL_SDI 1 "register_operand" "0, w")
	   (match_operand:<VSI2QI> 2 "register_operand" "w, w")
	   (match_operand:<VSI2QI> 3 "register_operand" "w, w")]
	  SVE2_INT_CDOT))]
  "TARGET_SVE2"
  "@
   <sve_int_op>\t%0.<Vetype>, %2.<Vetype_fourth>, %3.<Vetype_fourth>, #<rot>
   movprfx\t%0, %1\;<sve_int_op>\t%0.<Vetype>, %2.<Vetype_fourth>, %3.<Vetype_fourth>, #<rot>"
  [(set_attr "movprfx" "*,yes")]
)

(define_insn "@aarch64_<optab>_lane_<mode>"
  [(set (match_operand:SVE_FULL_SDI 0 "register_operand" "=w, ?&w")
	(unspec:SVE_FULL_SDI
	  [(match_operand:SVE_FULL_SDI 1 "register_operand" "0, w")
	   (match_operand:<VSI2QI> 2 "register_operand" "w, w")
	   (unspec:<VSI2QI>
	     [(match_operand:<VSI2QI> 3 "register_operand" "<sve_lane_con>, <sve_lane_con>")
	      (match_operand:SI 4 "const_int_operand")]
	     UNSPEC_SVE_LANE_SELECT)]
	  SVE2_INT_CDOT))]
  "TARGET_SVE2"
  "@
   <sve_int_op>\t%0.<Vetype>, %2.<Vetype_fourth>, %3.<Vetype_fourth>[%4], #<rot>
   movprfx\t%0, %1\;<sve_int_op>\t%0.<Vetype>, %2.<Vetype_fourth>, %3.<Vetype_fourth>[%4], #<rot>"
  [(set_attr "movprfx" "*,yes")]
)

;; =========================================================================
;; == Conversions
;; =========================================================================

;; -------------------------------------------------------------------------
;; ---- [FP<-FP] Widening conversions
;; -------------------------------------------------------------------------
;; Includes:
;; - FCVTLT
;; -------------------------------------------------------------------------

;; Predicated convert long top.
(define_insn "@aarch64_pred_<sve_fp_op><mode>"
  [(set (match_operand:SVE_FULL_SDF 0 "register_operand" "=w")
	(unspec:SVE_FULL_SDF
	  [(match_operand:<VPRED> 1 "register_operand" "Upl")
	   (match_operand:SI 3 "aarch64_sve_gp_strictness")
	   (match_operand:<VNARROW> 2 "register_operand" "0")]
	  SVE2_COND_FP_UNARY_LONG))]
  "TARGET_SVE2"
  "<sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Ventype>"
)

;; Predicated convert long top with merging.
(define_expand "@cond_<sve_fp_op><mode>"
  [(set (match_operand:SVE_FULL_SDF 0 "register_operand")
	(unspec:SVE_FULL_SDF
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_SDF
	     [(match_dup 1)
	      (const_int SVE_STRICT_GP)
	      (match_operand:<VNARROW> 2 "register_operand")]
	     SVE2_COND_FP_UNARY_LONG)
	   (match_operand:SVE_FULL_SDF 3 "register_operand")]
	  UNSPEC_SEL))]
  "TARGET_SVE2"
)

;; These instructions do not take MOVPRFX.
(define_insn_and_rewrite "*cond_<sve_fp_op><mode>_relaxed"
  [(set (match_operand:SVE_FULL_SDF 0 "register_operand" "=w")
	(unspec:SVE_FULL_SDF
	  [(match_operand:<VPRED> 1 "register_operand" "Upl")
	   (unspec:SVE_FULL_SDF
	     [(match_operand 4)
	      (const_int SVE_RELAXED_GP)
	      (match_operand:<VNARROW> 2 "register_operand" "w")]
	     SVE2_COND_FP_UNARY_LONG)
	   (match_operand:SVE_FULL_SDF 3 "register_operand" "0")]
	  UNSPEC_SEL))]
  "TARGET_SVE2"
  "<sve_fp_op>\t%0.<Vetype>, %1/m, %2.<Ventype>"
  "&& !rtx_equal_p (operands[1], operands[4])"
  {
    operands[4] = copy_rtx (operands[1]);
  }
)

(define_insn "*cond_<sve_fp_op><mode>_strict"
  [(set (match_operand:SVE_FULL_SDF 0 "register_operand" "=w")
	(unspec:SVE_FULL_SDF
	  [(match_operand:<VPRED> 1 "register_operand" "Upl")
	   (unspec:SVE_FULL_SDF
	     [(match_dup 1)
	      (const_int SVE_STRICT_GP)
	      (match_operand:<VNARROW> 2 "register_operand" "w")]
	     SVE2_COND_FP_UNARY_LONG)
	   (match_operand:SVE_FULL_SDF 3 "register_operand" "0")]
	  UNSPEC_SEL))]
  "TARGET_SVE2"
  "<sve_fp_op>\t%0.<Vetype>, %1/m, %2.<Ventype>"
)

;; -------------------------------------------------------------------------
;; ---- [FP<-FP] Narrowing conversions
;; -------------------------------------------------------------------------
;; Includes:
;; - FCVTNT
;; - FCVTX
;; - FCVTXNT
;; -------------------------------------------------------------------------

;; Predicated FCVTNT.  This doesn't give a natural aarch64_pred_*/cond_*
;; pair because the even elements always have to be supplied for active
;; elements, even if the inactive elements don't matter.
;;
;; These instructions do not take MOVPRFX.
(define_insn "@aarch64_sve_cvtnt<mode>"
  [(set (match_operand:SVE_FULL_HSF 0 "register_operand" "=w")
	(unspec:SVE_FULL_HSF
	  [(match_operand:<VWIDE_PRED> 2 "register_operand" "Upl")
	   (const_int SVE_STRICT_GP)
	   (match_operand:SVE_FULL_HSF 1 "register_operand" "0")
	   (match_operand:<VWIDE> 3 "register_operand" "w")]
	  UNSPEC_COND_FCVTNT))]
  "TARGET_SVE2"
  "fcvtnt\t%0.<Vetype>, %2/m, %3.<Vewtype>"
)

;; Predicated FCVTX (equivalent to what would be FCVTXNB, except that
;; it supports MOVPRFX).
(define_insn "@aarch64_pred_<sve_fp_op><mode>"
  [(set (match_operand:VNx4SF_ONLY 0 "register_operand" "=w, ?&w")
	(unspec:VNx4SF_ONLY
	  [(match_operand:<VWIDE_PRED> 1 "register_operand" "Upl, Upl")
	   (match_operand:SI 3 "aarch64_sve_gp_strictness")
	   (match_operand:<VWIDE> 2 "register_operand" "0, w")]
	  SVE2_COND_FP_UNARY_NARROWB))]
  "TARGET_SVE2"
  "@
   <sve_fp_op>\t%0.<Vetype>, %1/m, %2.<Vewtype>
   movprfx\t%0, %2\;<sve_fp_op>\t%0.<Vetype>, %1/m, %2.<Vewtype>"
  [(set_attr "movprfx" "*,yes")]
)

;; Predicated FCVTX with merging.
(define_expand "@cond_<sve_fp_op><mode>"
  [(set (match_operand:VNx4SF_ONLY 0 "register_operand")
	(unspec:VNx4SF_ONLY
	  [(match_operand:<VWIDE_PRED> 1 "register_operand")
	   (unspec:VNx4SF_ONLY
	     [(match_dup 1)
	      (const_int SVE_STRICT_GP)
	      (match_operand:<VWIDE> 2 "register_operand")]
	     SVE2_COND_FP_UNARY_NARROWB)
	   (match_operand:VNx4SF_ONLY 3 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE2"
)

(define_insn_and_rewrite "*cond_<sve_fp_op><mode>_any_relaxed"
  [(set (match_operand:VNx4SF_ONLY 0 "register_operand" "=&w, &w, &w")
	(unspec:VNx4SF_ONLY
	  [(match_operand:<VWIDE_PRED> 1 "register_operand" "Upl, Upl, Upl")
	   (unspec:VNx4SF_ONLY
	     [(match_operand 4)
	      (const_int SVE_RELAXED_GP)
	      (match_operand:<VWIDE> 2 "register_operand" "w, w, w")]
	     SVE2_COND_FP_UNARY_NARROWB)
	   (match_operand:VNx4SF_ONLY 3 "aarch64_simd_reg_or_zero" "0, Dz, w")]
	  UNSPEC_SEL))]
  "TARGET_SVE2 && !rtx_equal_p (operands[2], operands[3])"
  "@
   <sve_fp_op>\t%0.<Vetype>, %1/m, %2.<Vewtype>
   movprfx\t%0.<Vewtype>, %1/z, %2.<Vewtype>\;<sve_fp_op>\t%0.<Vetype>, %1/m, %2.<Vewtype>
   movprfx\t%0, %3\;<sve_fp_op>\t%0.<Vetype>, %1/m, %2.<Vewtype>"
  "&& !rtx_equal_p (operands[1], operands[4])"
  {
    operands[4] = copy_rtx (operands[1]);
  }
  [(set_attr "movprfx" "*,yes,yes")]
)

(define_insn "*cond_<sve_fp_op><mode>_any_strict"
  [(set (match_operand:VNx4SF_ONLY 0 "register_operand" "=&w, &w, &w")
	(unspec:VNx4SF_ONLY
	  [(match_operand:<VWIDE_PRED> 1 "register_operand" "Upl, Upl, Upl")
	   (unspec:VNx4SF_ONLY
	     [(match_dup 1)
	      (const_int SVE_STRICT_GP)
	      (match_operand:<VWIDE> 2 "register_operand" "w, w, w")]
	     SVE2_COND_FP_UNARY_NARROWB)
	   (match_operand:VNx4SF_ONLY 3 "aarch64_simd_reg_or_zero" "0, Dz, w")]
	  UNSPEC_SEL))]
  "TARGET_SVE2 && !rtx_equal_p (operands[2], operands[3])"
  "@
   <sve_fp_op>\t%0.<Vetype>, %1/m, %2.<Vewtype>
   movprfx\t%0.<Vewtype>, %1/z, %2.<Vewtype>\;<sve_fp_op>\t%0.<Vetype>, %1/m, %2.<Vewtype>
   movprfx\t%0, %3\;<sve_fp_op>\t%0.<Vetype>, %1/m, %2.<Vewtype>"
  [(set_attr "movprfx" "*,yes,yes")]
)

;; Predicated FCVTXNT.  This doesn't give a natural aarch64_pred_*/cond_*
;; pair because the even elements always have to be supplied for active
;; elements, even if the inactive elements don't matter.
;;
;; These instructions do not take MOVPRFX.
(define_insn "@aarch64_sve2_cvtxnt<mode>"
  [(set (match_operand:<VNARROW> 0 "register_operand" "=w")
	(unspec:<VNARROW>
	  [(match_operand:<VPRED> 2 "register_operand" "Upl")
	   (const_int SVE_STRICT_GP)
	   (match_operand:<VNARROW> 1 "register_operand" "0")
	   (match_operand:VNx2DF_ONLY 3 "register_operand" "w")]
	  UNSPEC_COND_FCVTXNT))]
  "TARGET_SVE2"
  "fcvtxnt\t%0.<Ventype>, %2/m, %3.<Vetype>"
)

;; =========================================================================
;; == Other arithmetic
;; =========================================================================

;; -------------------------------------------------------------------------
;; ---- [INT] Reciprocal approximation
;; -------------------------------------------------------------------------
;; Includes:
;; - URECPE
;; - URSQRTE
;; -------------------------------------------------------------------------

;; Predicated integer unary operations.
(define_insn "@aarch64_pred_<sve_int_op><mode>"
  [(set (match_operand:VNx4SI_ONLY 0 "register_operand" "=w, ?&w")
	(unspec:VNx4SI_ONLY
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (unspec:VNx4SI_ONLY
	     [(match_operand:VNx4SI_ONLY 2 "register_operand" "0, w")]
	     SVE2_U32_UNARY)]
	  UNSPEC_PRED_X))]
  "TARGET_SVE2"
  "@
   <sve_int_op>\t%0.<Vetype>, %1/m, %2.<Vetype>
   movprfx\t%0, %2\;<sve_int_op>\t%0.<Vetype>, %1/m, %2.<Vetype>"
  [(set_attr "movprfx" "*,yes")]
)

;; Predicated integer unary operations with merging.
(define_expand "@cond_<sve_int_op><mode>"
  [(set (match_operand:VNx4SI_ONLY 0 "register_operand")
	(unspec:VNx4SI_ONLY
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:VNx4SI_ONLY
	     [(match_dup 4)
	      (unspec:VNx4SI_ONLY
		[(match_operand:VNx4SI_ONLY 2 "register_operand")]
		SVE2_U32_UNARY)]
	     UNSPEC_PRED_X)
	   (match_operand:VNx4SI_ONLY 3 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE2"
  {
    operands[4] = CONSTM1_RTX (<MODE>mode);
  }
)

(define_insn_and_rewrite "*cond_<sve_int_op><mode>"
  [(set (match_operand:VNx4SI_ONLY 0 "register_operand" "=w, ?&w, ?&w")
	(unspec:VNx4SI_ONLY
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl")
	   (unspec:VNx4SI_ONLY
	     [(match_operand 4)
	      (unspec:VNx4SI_ONLY
		[(match_operand:VNx4SI_ONLY 2 "register_operand" "w, w, w")]
		SVE2_U32_UNARY)]
	     UNSPEC_PRED_X)
	   (match_operand:VNx4SI_ONLY 3 "aarch64_simd_reg_or_zero" "0, Dz, w")]
	  UNSPEC_SEL))]
  "TARGET_SVE2"
  "@
   <sve_int_op>\t%0.<Vetype>, %1/m, %2.<Vetype>
   movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;<sve_int_op>\t%0.<Vetype>, %1/m, %2.<Vetype>
   movprfx\t%0, %3\;<sve_int_op>\t%0.<Vetype>, %1/m, %2.<Vetype>"
  "&& !CONSTANT_P (operands[4])"
  {
    operands[4] = CONSTM1_RTX (<VPRED>mode);
  }
  [(set_attr "movprfx" "*,yes,yes")]
)

;; -------------------------------------------------------------------------
;; ---- [INT<-FP] Base-2 logarithm
;; -------------------------------------------------------------------------
;; Includes:
;; - FLOGB
;; -------------------------------------------------------------------------

;; Predicated FLOGB.
(define_insn "@aarch64_pred_<sve_fp_op><mode>"
  [(set (match_operand:<V_INT_EQUIV> 0 "register_operand" "=w, ?&w")
	(unspec:<V_INT_EQUIV>
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (match_operand:SI 3 "aarch64_sve_gp_strictness")
	   (match_operand:SVE_FULL_F 2 "register_operand" "0, w")]
	  SVE2_COND_INT_UNARY_FP))]
  "TARGET_SVE2"
  "@
   <sve_fp_op>\t%0.<Vetype>, %1/m, %2.<Vetype>
   movprfx\t%0, %2\;<sve_fp_op>\t%0.<Vetype>, %1/m, %2.<Vetype>"
  [(set_attr "movprfx" "*,yes")]
)

;; Predicated FLOGB with merging.
(define_expand "@cond_<sve_fp_op><mode>"
  [(set (match_operand:<V_INT_EQUIV> 0 "register_operand")
	(unspec:<V_INT_EQUIV>
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:<V_INT_EQUIV>
	     [(match_dup 1)
	      (const_int SVE_STRICT_GP)
	      (match_operand:SVE_FULL_F 2 "register_operand")]
	     SVE2_COND_INT_UNARY_FP)
	   (match_operand:<V_INT_EQUIV> 3 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE2"
)

(define_insn_and_rewrite "*cond_<sve_fp_op><mode>"
  [(set (match_operand:<V_INT_EQUIV> 0 "register_operand" "=&w, ?&w, ?&w")
	(unspec:<V_INT_EQUIV>
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl")
	   (unspec:<V_INT_EQUIV>
	     [(match_operand 4)
	      (const_int SVE_RELAXED_GP)
	      (match_operand:SVE_FULL_F 2 "register_operand" "w, w, w")]
	     SVE2_COND_INT_UNARY_FP)
	   (match_operand:<V_INT_EQUIV> 3 "aarch64_simd_reg_or_zero" "0, Dz, w")]
	  UNSPEC_SEL))]
  "TARGET_SVE2 && !rtx_equal_p (operands[2], operands[3])"
  "@
   <sve_fp_op>\t%0.<Vetype>, %1/m, %2.<Vetype>
   movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;<sve_fp_op>\t%0.<Vetype>, %1/m, %2.<Vetype>
   movprfx\t%0, %3\;<sve_fp_op>\t%0.<Vetype>, %1/m, %2.<Vetype>"
  "&& !rtx_equal_p (operands[1], operands[4])"
  {
    operands[4] = copy_rtx (operands[1]);
  }
  [(set_attr "movprfx" "*,yes,yes")]
)

(define_insn "*cond_<sve_fp_op><mode>_strict"
  [(set (match_operand:<V_INT_EQUIV> 0 "register_operand" "=&w, ?&w, ?&w")
	(unspec:<V_INT_EQUIV>
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl")
	   (unspec:<V_INT_EQUIV>
	     [(match_dup 1)
	      (const_int SVE_STRICT_GP)
	      (match_operand:SVE_FULL_F 2 "register_operand" "w, w, w")]
	     SVE2_COND_INT_UNARY_FP)
	   (match_operand:<V_INT_EQUIV> 3 "aarch64_simd_reg_or_zero" "0, Dz, w")]
	  UNSPEC_SEL))]
  "TARGET_SVE2 && !rtx_equal_p (operands[2], operands[3])"
  "@
   <sve_fp_op>\t%0.<Vetype>, %1/m, %2.<Vetype>
   movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;<sve_fp_op>\t%0.<Vetype>, %1/m, %2.<Vetype>
   movprfx\t%0, %3\;<sve_fp_op>\t%0.<Vetype>, %1/m, %2.<Vetype>"
  [(set_attr "movprfx" "*,yes,yes")]
)

;; -------------------------------------------------------------------------
;; ---- [INT] Polynomial multiplication
;; -------------------------------------------------------------------------
;; Includes:
;; - PMUL
;; - PMULLB
;; - PMULLT
;; -------------------------------------------------------------------------

;; Uniform PMUL.
(define_insn "@aarch64_sve2_pmul<mode>"
  [(set (match_operand:VNx16QI_ONLY 0 "register_operand" "=w")
	(unspec:VNx16QI_ONLY
	  [(match_operand:VNx16QI_ONLY 1 "register_operand" "w")
	   (match_operand:VNx16QI_ONLY 2 "register_operand" "w")]
	  UNSPEC_PMUL))]
  "TARGET_SVE2"
  "pmul\t%0.<Vetype>, %1.<Vetype>, %2.<Vetype>"
)

;; Extending PMUL, with the results modeled as wider vectors.
;; This representation is only possible for .H and .D, not .Q.
(define_insn "@aarch64_sve_<optab><mode>"
  [(set (match_operand:SVE_FULL_HDI 0 "register_operand" "=w")
	(unspec:SVE_FULL_HDI
	  [(match_operand:<VNARROW> 1 "register_operand" "w")
	   (match_operand:<VNARROW> 2 "register_operand" "w")]
	  SVE2_PMULL))]
  "TARGET_SVE2"
  "<sve_int_op>\t%0.<Vetype>, %1.<Ventype>, %2.<Ventype>"
)

;; Extending PMUL, with the results modeled as pairs of values.
;; This representation works for .H, .D and .Q, with .Q requiring
;; the AES extension.  (This is enforced by the mode iterator.)
(define_insn "@aarch64_sve_<optab><mode>"
  [(set (match_operand:SVE2_PMULL_PAIR_I 0 "register_operand" "=w")
	(unspec:SVE2_PMULL_PAIR_I
	  [(match_operand:SVE2_PMULL_PAIR_I 1 "register_operand" "w")
	   (match_operand:SVE2_PMULL_PAIR_I 2 "register_operand" "w")]
	  SVE2_PMULL_PAIR))]
  "TARGET_SVE2"
  "<sve_int_op>\t%0.<Vewtype>, %1.<Vetype>, %2.<Vetype>"
)

;; =========================================================================
;; == Permutation
;; =========================================================================

;; -------------------------------------------------------------------------
;; ---- [INT,FP] General permutes
;; -------------------------------------------------------------------------
;; Includes:
;; - TBL (vector pair form)
;; - TBX
;; -------------------------------------------------------------------------

;; TBL on a pair of data vectors.
(define_insn "@aarch64_sve2_tbl2<mode>"
  [(set (match_operand:SVE_FULL 0 "register_operand" "=w")
	(unspec:SVE_FULL
	  [(match_operand:<VDOUBLE> 1 "register_operand" "w")
	   (match_operand:<V_INT_EQUIV> 2 "register_operand" "w")]
	  UNSPEC_TBL2))]
  "TARGET_SVE2"
  "tbl\t%0.<Vetype>, %1, %2.<Vetype>"
)

;; TBX.  These instructions do not take MOVPRFX.
(define_insn "@aarch64_sve2_tbx<mode>"
  [(set (match_operand:SVE_FULL 0 "register_operand" "=w")
	(unspec:SVE_FULL
	  [(match_operand:SVE_FULL 1 "register_operand" "0")
	   (match_operand:SVE_FULL 2 "register_operand" "w")
	   (match_operand:<V_INT_EQUIV> 3 "register_operand" "w")]
	  UNSPEC_TBX))]
  "TARGET_SVE2"
  "tbx\t%0.<Vetype>, %2.<Vetype>, %3.<Vetype>"
)

;; -------------------------------------------------------------------------
;; ---- [INT] Optional bit-permute extensions
;; -------------------------------------------------------------------------
;; Includes:
;; - BDEP
;; - BEXT
;; - BGRP
;; -------------------------------------------------------------------------

(define_insn "@aarch64_sve_<sve_int_op><mode>"
  [(set (match_operand:SVE_FULL_I 0 "register_operand" "=w")
	(unspec:SVE_FULL_I
	  [(match_operand:SVE_FULL_I 1 "register_operand" "w")
	   (match_operand:SVE_FULL_I 2 "register_operand" "w")]
	  SVE2_INT_BITPERM))]
  "TARGET_SVE2_BITPERM"
  "<sve_int_op>\t%0.<Vetype>, %1.<Vetype>, %2.<Vetype>"
)

;; =========================================================================
;; == General
;; =========================================================================

;; -------------------------------------------------------------------------
;; ---- Check for aliases between pointers
;; -------------------------------------------------------------------------
;; The patterns in this section are synthetic: WHILERW and WHILEWR are
;; defined in aarch64-sve.md instead.
;; -------------------------------------------------------------------------

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
  emit_insn (gen_while_ptest
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

;; -------------------------------------------------------------------------
;; ---- Histogram processing
;; -------------------------------------------------------------------------
;; Includes:
;; - HISTCNT
;; - HISTSEG
;; -------------------------------------------------------------------------

(define_insn "@aarch64_sve2_histcnt<mode>"
  [(set (match_operand:SVE_FULL_SDI 0 "register_operand" "=w")
	(unspec:SVE_FULL_SDI
	  [(match_operand:<VPRED> 1 "register_operand" "Upl")
	   (match_operand:SVE_FULL_SDI 2 "register_operand" "w")
	   (match_operand:SVE_FULL_SDI 3 "register_operand" "w")]
	  UNSPEC_HISTCNT))]
  "TARGET_SVE2"
  "histcnt\t%0.<Vetype>, %1/z, %2.<Vetype>, %3.<Vetype>"
)

(define_insn "@aarch64_sve2_histseg<mode>"
  [(set (match_operand:VNx16QI_ONLY 0 "register_operand" "=w")
	(unspec:VNx16QI_ONLY
	  [(match_operand:VNx16QI_ONLY 1 "register_operand" "w")
	   (match_operand:VNx16QI_ONLY 2 "register_operand" "w")]
	  UNSPEC_HISTSEG))]
  "TARGET_SVE2"
  "histseg\t%0.<Vetype>, %1.<Vetype>, %2.<Vetype>"
)

;; -------------------------------------------------------------------------
;; ---- String matching
;; -------------------------------------------------------------------------
;; Includes:
;; - MATCH
;; - NMATCH
;; -------------------------------------------------------------------------

;; Predicated string matching.
(define_insn "@aarch64_pred_<sve_int_op><mode>"
  [(set (match_operand:<VPRED> 0 "register_operand" "=Upa")
	(unspec:<VPRED>
	  [(match_operand:<VPRED> 1 "register_operand" "Upl")
	   (match_operand:SI 2 "aarch64_sve_ptrue_flag")
	   (unspec:<VPRED>
	     [(match_operand:SVE_FULL_BHI 3 "register_operand" "w")
	      (match_operand:SVE_FULL_BHI 4 "register_operand" "w")]
	     SVE2_MATCH)]
	  UNSPEC_PRED_Z))
   (clobber (reg:CC_NZC CC_REGNUM))]
  "TARGET_SVE2"
  "<sve_int_op>\t%0.<Vetype>, %1/z, %3.<Vetype>, %4.<Vetype>"
)

;; Predicated string matching in which both the flag and predicate results
;; are interesting.
(define_insn_and_rewrite "*aarch64_pred_<sve_int_op><mode>_cc"
  [(set (reg:CC_NZC CC_REGNUM)
	(unspec:CC_NZC
	  [(match_operand:VNx16BI 1 "register_operand" "Upl")
	   (match_operand 4)
	   (match_operand:SI 5 "aarch64_sve_ptrue_flag")
	   (unspec:<VPRED>
	     [(match_operand 6)
	      (match_operand:SI 7 "aarch64_sve_ptrue_flag")
	      (unspec:<VPRED>
		[(match_operand:SVE_FULL_BHI 2 "register_operand" "w")
		 (match_operand:SVE_FULL_BHI 3 "register_operand" "w")]
		SVE2_MATCH)]
	     UNSPEC_PRED_Z)]
	  UNSPEC_PTEST))
   (set (match_operand:<VPRED> 0 "register_operand" "=Upa")
	(unspec:<VPRED>
	  [(match_dup 6)
	   (match_dup 7)
	   (unspec:<VPRED>
	     [(match_dup 2)
	      (match_dup 3)]
	     SVE2_MATCH)]
	  UNSPEC_PRED_Z))]
  "TARGET_SVE2
   && aarch64_sve_same_pred_for_ptest_p (&operands[4], &operands[6])"
  "<sve_int_op>\t%0.<Vetype>, %1/z, %2.<Vetype>, %3.<Vetype>"
  "&& !rtx_equal_p (operands[4], operands[6])"
  {
    operands[6] = copy_rtx (operands[4]);
    operands[7] = operands[5];
  }
)

;; Predicated string matching in which only the flags result is interesting.
(define_insn_and_rewrite "*aarch64_pred_<sve_int_op><mode>_ptest"
  [(set (reg:CC_NZC CC_REGNUM)
	(unspec:CC_NZC
	  [(match_operand:VNx16BI 1 "register_operand" "Upl")
	   (match_operand 4)
	   (match_operand:SI 5 "aarch64_sve_ptrue_flag")
	   (unspec:<VPRED>
	     [(match_operand 6)
	      (match_operand:SI 7 "aarch64_sve_ptrue_flag")
	      (unspec:<VPRED>
		[(match_operand:SVE_FULL_BHI 2 "register_operand" "w")
		 (match_operand:SVE_FULL_BHI 3 "register_operand" "w")]
		SVE2_MATCH)]
	     UNSPEC_PRED_Z)]
	  UNSPEC_PTEST))
   (clobber (match_scratch:<VPRED> 0 "=Upa"))]
  "TARGET_SVE2
   && aarch64_sve_same_pred_for_ptest_p (&operands[4], &operands[6])"
  "<sve_int_op>\t%0.<Vetype>, %1/z, %2.<Vetype>, %3.<Vetype>"
  "&& !rtx_equal_p (operands[4], operands[6])"
  {
    operands[6] = copy_rtx (operands[4]);
    operands[7] = operands[5];
  }
)

;; =========================================================================
;; == Cryptographic extensions
;; =========================================================================

;; -------------------------------------------------------------------------
;; ---- Optional AES extensions
;; -------------------------------------------------------------------------
;; Includes:
;; - AESD
;; - AESE
;; - AESIMC
;; - AESMC
;; -------------------------------------------------------------------------

;; AESD and AESE.
(define_insn "aarch64_sve2_aes<aes_op>"
  [(set (match_operand:VNx16QI 0 "register_operand" "=w")
	(unspec:VNx16QI
	  [(xor:VNx16QI
	     (match_operand:VNx16QI 1 "register_operand" "%0")
	     (match_operand:VNx16QI 2 "register_operand" "w"))]
          CRYPTO_AES))]
  "TARGET_SVE2_AES"
  "aes<aes_op>\t%0.b, %0.b, %2.b"
  [(set_attr "type" "crypto_aese")]
)

;; AESMC and AESIMC.  These instructions do not take MOVPRFX.
(define_insn "aarch64_sve2_aes<aesmc_op>"
  [(set (match_operand:VNx16QI 0 "register_operand" "=w")
	(unspec:VNx16QI
	  [(match_operand:VNx16QI 1 "register_operand" "0")]
	  CRYPTO_AESMC))]
  "TARGET_SVE2_AES"
  "aes<aesmc_op>\t%0.b, %0.b"
  [(set_attr "type" "crypto_aesmc")]
)

;; When AESE/AESMC and AESD/AESIMC fusion is enabled, we really want
;; to keep the two together and enforce the register dependency without
;; scheduling or register allocation messing up the order or introducing
;; moves inbetween.  Mash the two together during combine.

(define_insn "*aarch64_sve2_aese_fused"
  [(set (match_operand:VNx16QI 0 "register_operand" "=w")
	(unspec:VNx16QI
	  [(unspec:VNx16QI
	     [(xor:VNx16QI
		(match_operand:VNx16QI 1 "register_operand" "%0")
		(match_operand:VNx16QI 2 "register_operand" "w"))]
	     UNSPEC_AESE)]
	  UNSPEC_AESMC))]
  "TARGET_SVE2_AES && aarch64_fusion_enabled_p (AARCH64_FUSE_AES_AESMC)"
  "aese\t%0.b, %0.b, %2.b\;aesmc\t%0.b, %0.b"
  [(set_attr "type" "crypto_aese")
   (set_attr "length" "8")]
)

(define_insn "*aarch64_sve2_aesd_fused"
  [(set (match_operand:VNx16QI 0 "register_operand" "=w")
	(unspec:VNx16QI
	  [(unspec:VNx16QI
	     [(xor:VNx16QI
		(match_operand:VNx16QI 1 "register_operand" "%0")
		(match_operand:VNx16QI 2 "register_operand" "w"))]
	     UNSPEC_AESD)]
	  UNSPEC_AESIMC))]
  "TARGET_SVE2_AES && aarch64_fusion_enabled_p (AARCH64_FUSE_AES_AESMC)"
  "aesd\t%0.b, %0.b, %2.b\;aesimc\t%0.b, %0.b"
  [(set_attr "type" "crypto_aese")
   (set_attr "length" "8")]
)

;; -------------------------------------------------------------------------
;; ---- Optional SHA-3 extensions
;; -------------------------------------------------------------------------
;; Includes:
;; - RAX1
;; -------------------------------------------------------------------------

(define_insn "aarch64_sve2_rax1"
  [(set (match_operand:VNx2DI 0 "register_operand" "=w")
	(xor:VNx2DI
	  (rotate:VNx2DI
	    (match_operand:VNx2DI 2 "register_operand" "w")
	    (const_int 1))
	  (match_operand:VNx2DI 1 "register_operand" "w")))]
  "TARGET_SVE2_SHA3"
  "rax1\t%0.d, %1.d, %2.d"
  [(set_attr "type" "crypto_sha3")]
)

;; -------------------------------------------------------------------------
;; ---- Optional SM4 extensions
;; -------------------------------------------------------------------------
;; Includes:
;; - SM4E
;; - SM4EKEY
;; -------------------------------------------------------------------------

;; These instructions do not take MOVPRFX.
(define_insn "aarch64_sve2_sm4e"
  [(set (match_operand:VNx4SI 0 "register_operand" "=w")
	(unspec:VNx4SI
	  [(match_operand:VNx4SI 1 "register_operand" "0")
	   (match_operand:VNx4SI 2 "register_operand" "w")]
	  UNSPEC_SM4E))]
  "TARGET_SVE2_SM4"
  "sm4e\t%0.s, %0.s, %2.s"
  [(set_attr "type" "crypto_sm4")]
)

(define_insn "aarch64_sve2_sm4ekey"
  [(set (match_operand:VNx4SI 0 "register_operand" "=w")
	(unspec:VNx4SI
	  [(match_operand:VNx4SI 1 "register_operand" "w")
	   (match_operand:VNx4SI 2 "register_operand" "w")]
	  UNSPEC_SM4EKEY))]
  "TARGET_SVE2_SM4"
  "sm4ekey\t%0.s, %1.s, %2.s"
  [(set_attr "type" "crypto_sm4")]
)
