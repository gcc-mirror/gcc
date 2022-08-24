;; Constraint definitions for ARM and Thumb
;; Copyright (C) 2006-2022 Free Software Foundation, Inc.
;; Contributed by ARM Ltd.

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

;; The following register constraints have been used:
;; - in ARM/Thumb-2 state: t, w, x, y, z
;; - in Thumb state: h, b
;; - in both states: l, c, k, q, Cs, Ts, US
;; In ARM state, 'l' is an alias for 'r'
;; 'f' and 'v' were previously used for FPA and MAVERICK registers.

;; The following normal constraints have been used:
;; in ARM/Thumb-2 state: G, I, j, J, K, L, M
;; in Thumb-1 state: I, J, K, L, M, N, O
;; 'H' was previously used for FPA.

;; The following multi-letter normal constraints have been used:
;; in ARM/Thumb-2 state: Da, Db, Dc, Dd, Dn, DN, Dm, Dl, DL, Do, Dv, Dy, Di,
;;			 Dj, Ds, Dt, Dp, Dz, Tu, Te
;; in Thumb-1 state: Pa, Pb, Pc, Pd, Pe
;; in Thumb-2 state: Ha, Pj, PJ, Ps, Pt, Pu, Pv, Pw, Px, Py, Pz, Rd, Rf, Rb, Ra,
;;		     Rg, Ri
;; in all states: Pf, Pg

;; The following memory constraints have been used:
;; in ARM/Thumb-2 state: Uh, Ut, Uv, Uy, Un, Um, Us, Up, Uf, Ux, Ul
;; in ARM state: Uq
;; in Thumb state: Uu, Uw
;; in all states: Q

(define_register_constraint "Up" "TARGET_HAVE_MVE ? VPR_REG : NO_REGS"
  "MVE VPR register")

(define_memory_constraint "Ul"
 "@internal
  In ARM/Thumb-2 state a valid address for load instruction with XEXP (op, 0)
  being label of the literal data item to be loaded."
 (and (match_code "mem")
      (match_test "TARGET_HAVE_MVE && reload_completed
		   && (GET_CODE (XEXP (op, 0)) == LABEL_REF
		       || (GET_CODE (XEXP (op, 0)) == CONST
			   && GET_CODE (XEXP (XEXP (op, 0), 0)) == PLUS
			   && GET_CODE (XEXP (XEXP (XEXP (op, 0), 0), 0)) == LABEL_REF
			   && CONST_INT_P (XEXP (XEXP (XEXP (op, 0), 0), 1))))")))

(define_register_constraint "Uf" "TARGET_HAVE_MVE ? VFPCC_REG : NO_REGS"
  "MVE FPCCR register")

(define_register_constraint "Te" "TARGET_HAVE_MVE ? EVEN_REG : NO_REGS"
  "EVEN core registers @code{r0}, @code{r2}, @code{r4}, @code{r6}, @code{r8},
   @code{r10}, @code{r12}, @code{r14}")

(define_constraint "Rd"
  "@internal In Thumb-2 state a constant in range 1 to 16"
  (and (match_code "const_int")
       (match_test "TARGET_HAVE_MVE && ival >= 1 && ival <= 16")))

(define_constraint "Ra"
  "@internal In Thumb-2 state a constant in range 0 to 7"
  (and (match_code "const_int")
       (match_test "TARGET_HAVE_MVE && ival >= 0 && ival <= 7")))

(define_constraint "Rb"
  "@internal In Thumb-2 state a constant in range 1 to 8"
  (and (match_code "const_int")
       (match_test "TARGET_HAVE_MVE && ival >= 1 && ival <= 8")))

(define_constraint "Rc"
  "@internal In Thumb-2 state a constant in range 0 to 15"
  (and (match_code "const_int")
       (match_test "TARGET_HAVE_MVE && ival >= 0 && ival <= 15")))

(define_constraint "Re"
  "@internal In Thumb-2 state a constant in range 0 to 31"
  (and (match_code "const_int")
       (match_test "TARGET_HAVE_MVE && ival >= 0 && ival <= 31")))

(define_constraint "Rf"
  "@internal In Thumb-2 state a constant in range 1 to 32"
  (and (match_code "const_int")
       (match_test "TARGET_HAVE_MVE && ival >= 1 && ival <= 32")))

(define_constraint "Rg"
  "@internal In Thumb-2 state a constant is one among 1, 2, 4 and 8"
  (and (match_code "const_int")
       (match_test "TARGET_HAVE_MVE && ((ival == 1) || (ival == 2)
				       || (ival == 4) || (ival == 8))")))

;; True if the immediate is multiple of 8 and in range of -/+ 1016 for MVE.
(define_predicate "mve_vldrd_immediate"
  (match_test "satisfies_constraint_Ri (op)"))

(define_register_constraint "t" "TARGET_32BIT ? VFP_LO_REGS : NO_REGS"
 "The VFP registers @code{s0}-@code{s31}.")

(define_register_constraint "w"
  "TARGET_32BIT ? (TARGET_VFPD32 ? VFP_REGS : VFP_LO_REGS) : NO_REGS"
 "The VFP registers @code{d0}-@code{d15}, or @code{d0}-@code{d31} for VFPv3.")

(define_register_constraint "x" "TARGET_32BIT ? VFP_D0_D7_REGS : NO_REGS"
 "The VFP registers @code{d0}-@code{d7}.")

(define_register_constraint "y" "TARGET_REALLY_IWMMXT ? IWMMXT_REGS : NO_REGS"
 "The Intel iWMMX co-processor registers.")

(define_register_constraint "z"
 "TARGET_REALLY_IWMMXT ? IWMMXT_GR_REGS : NO_REGS"
 "The Intel iWMMX GR registers.")

(define_register_constraint "l" "TARGET_THUMB ? LO_REGS : GENERAL_REGS"
 "In Thumb state the core registers @code{r0}-@code{r7}.")

(define_register_constraint "h" "TARGET_THUMB ? HI_REGS : NO_REGS"
 "In Thumb state the core registers @code{r8}-@code{r15}.")

(define_constraint "j"
 "A constant suitable for a MOVW instruction. (ARM/Thumb-2)"
 (and (match_test "TARGET_HAVE_MOVT")
      (ior (and (match_code "high")
		(match_test "arm_valid_symbolic_address_p (XEXP (op, 0))"))
	   (and (match_code "const_int")
                (match_test "(ival & 0xffff0000) == 0")))))

(define_constraint "Pj"
 "@internal A 12-bit constant suitable for an ADDW or SUBW instruction. (Thumb-2)"
 (and (match_code "const_int")
      (and (match_test "TARGET_THUMB2")
	   (match_test "(ival & 0xfffff000) == 0"))))

(define_constraint "PJ"
 "@internal A constant that satisfies the Pj constrant if negated."
 (and (match_code "const_int")
      (and (match_test "TARGET_THUMB2")
	   (match_test "((-ival) & 0xfffff000) == 0"))))

(define_register_constraint "k" "STACK_REG"
 "@internal The stack register.")

(define_register_constraint "b" "TARGET_THUMB ? BASE_REGS : NO_REGS"
 "@internal
  Thumb only.  The union of the low registers and the stack register.")

(define_constraint "c"
 "@internal The condition code register."
 (match_operand 0 "cc_register"))

(define_register_constraint "Cs" "CALLER_SAVE_REGS"
 "@internal The caller save registers.  Useful for sibcalls.")

(define_constraint "I"
 "In ARM/Thumb-2 state a constant that can be used as an immediate value in a
  Data Processing instruction.  In Thumb-1 state a constant in the range
  0-255."
 (and (match_code "const_int")
      (match_test "TARGET_32BIT ? const_ok_for_arm (ival)
		   : ival >= 0 && ival <= 255")))

(define_constraint "J"
 "In ARM/Thumb-2 state a constant in the range @minus{}4095-4095.  In Thumb-1
  state a constant in the range @minus{}255-@minus{}1."
 (and (match_code "const_int")
      (match_test "TARGET_32BIT ? (ival >= -4095 && ival <= 4095)
		   : (ival >= -255 && ival <= -1)")))

(define_constraint "K"
 "In ARM/Thumb-2 state a constant that satisfies the @code{I} constraint if
  inverted.  In Thumb-1 state a constant that satisfies the @code{I}
  constraint multiplied by any power of 2."
 (and (match_code "const_int")
      (match_test "TARGET_32BIT ? const_ok_for_arm (~ival)
		   : thumb_shiftable_const (ival)")))

(define_constraint "L"
 "In ARM/Thumb-2 state a constant that satisfies the @code{I} constraint if
  negated.  In Thumb-1 state a constant in the range @minus{}7-7."
 (and (match_code "const_int")
      (match_test "TARGET_32BIT ? const_ok_for_arm (-ival)
		   : (ival >= -7 && ival <= 7)")))

;; The ARM state version is internal...
;; @internal In ARM/Thumb-2 state a constant in the range 0-32 or any
;; power of 2.
(define_constraint "M"
 "In Thumb-1 state a constant that is a multiple of 4 in the range 0-1020."
 (and (match_code "const_int")
      (match_test "TARGET_32BIT ? ((ival >= 0 && ival <= 32)
				 || (((ival & (ival - 1)) & 0xFFFFFFFF) == 0))
		   : ival >= 0 && ival <= 1020 && (ival & 3) == 0")))

(define_constraint "N"
 "Thumb-1 state a constant in the range 0-31."
 (and (match_code "const_int")
      (match_test "!TARGET_32BIT && (ival >= 0 && ival <= 31)")))

(define_constraint "O"
 "In Thumb-1 state a constant that is a multiple of 4 in the range
  @minus{}508-508."
 (and (match_code "const_int")
      (match_test "TARGET_THUMB1 && ival >= -508 && ival <= 508
		   && ((ival & 3) == 0)")))

(define_constraint "Pa"
  "@internal In Thumb-1 state a constant in the range -510 to +510"
  (and (match_code "const_int")
       (match_test "TARGET_THUMB1 && ival >= -510 && ival <= 510
		    && (ival > 255 || ival < -255)")))

(define_constraint "Pb"
  "@internal In Thumb-1 state a constant in the range -262 to +262"
  (and (match_code "const_int")
       (match_test "TARGET_THUMB1 && ival >= -262 && ival <= 262
		    && (ival > 255 || ival < -255)")))

(define_constraint "Pc"
  "@internal In Thumb-1 state a constant that is in the range 1021 to 1275"
  (and (match_code "const_int")
       (match_test "TARGET_THUMB1
  		    && ival > 1020 && ival <= 1275")))

(define_constraint "Pd"
  "@internal In Thumb state a constant in the range 0 to 7"
  (and (match_code "const_int")
       (match_test "TARGET_THUMB && ival >= 0 && ival <= 7")))

(define_constraint "Pe"
  "@internal In Thumb-1 state a constant in the range 256 to +510"
  (and (match_code "const_int")
       (match_test "TARGET_THUMB1 && ival >= 256 && ival <= 510")))

(define_constraint "Pf"
  "Memory models except relaxed, consume or release ones."
  (and (match_code "const_int")
       (match_test "!is_mm_relaxed (memmodel_from_int (ival))
		    && !is_mm_consume (memmodel_from_int (ival))
		    && !is_mm_release (memmodel_from_int (ival))")))

(define_constraint "Pg"
  "@internal In Thumb-2 state a constant in range 1 to 32"
  (and (match_code "const_int")
       (match_test "TARGET_THUMB2 && ival >= 1 && ival <= 32")))

(define_constraint "Ps"
  "@internal In Thumb-2 state a constant in the range -255 to +255"
  (and (match_code "const_int")
       (match_test "TARGET_THUMB2 && ival >= -255 && ival <= 255")))

(define_constraint "Pt"
  "@internal In Thumb-2 state a constant in the range -7 to +7"
  (and (match_code "const_int")
       (match_test "TARGET_THUMB2 && ival >= -7 && ival <= 7")))

(define_constraint "Pu"
  "@internal In Thumb-2 state a constant in the range +1 to +8"
  (and (match_code "const_int")
       (match_test "TARGET_THUMB2 && ival >= 1 && ival <= 8")))

(define_constraint "Pv"
  "@internal In Thumb-2 state a constant in the range -255 to 0"
  (and (match_code "const_int")
       (match_test "TARGET_THUMB2 && ival >= -255 && ival <= 0")))

(define_constraint "Pw"
  "@internal In Thumb-2 state a constant in the range -255 to -1"
  (and (match_code "const_int")
       (match_test "TARGET_THUMB2 && ival >= -255 && ival <= -1")))

(define_constraint "Px"
  "@internal In Thumb-2 state a constant in the range -7 to -1"
  (and (match_code "const_int")
       (match_test "TARGET_THUMB2 && ival >= -7 && ival <= -1")))

(define_constraint "Py"
  "@internal In Thumb-2 state a constant in the range 0 to 255"
  (and (match_code "const_int")
       (match_test "TARGET_THUMB2 && ival >= 0 && ival <= 255")))

(define_constraint "Pz"
  "@internal In Thumb-2 state the constant 0"
  (and (match_code "const_int")
       (match_test "TARGET_THUMB2 && (ival == 0)")))

(define_constraint "G"
 "In ARM/Thumb-2 state the floating-point constant 0."
 (and (match_code "const_double")
      (match_test "TARGET_32BIT && arm_const_double_rtx (op)")))

(define_constraint "Ha"
  "@internal In ARM / Thumb-2 a float constant iff literal pools are allowed."
  (and (match_code "const_double")
       (match_test "satisfies_constraint_E (op)")
       (match_test "!arm_disable_literal_pool")))

(define_constraint "Dz"
 "@internal
  In ARM/Thumb-2 state a vector of constant zeros."
 (and (match_code "const_vector")
      (match_test "(TARGET_NEON || TARGET_HAVE_MVE) && op == CONST0_RTX (mode)")))

(define_constraint "DB"
 "@internal
  In ARM/Thumb-2 state with MVE a constant vector of booleans."
 (and (match_code "const_vector")
      (match_test "TARGET_HAVE_MVE && GET_MODE_CLASS (mode) == MODE_VECTOR_BOOL")))

(define_constraint "Da"
 "@internal
  In ARM/Thumb-2 state a const_int, const_double or const_vector that can
  be generated with two Data Processing insns."
 (and (match_code "const_double,const_int,const_vector")
      (match_test "TARGET_32BIT && arm_const_double_inline_cost (op) == 2")))

(define_constraint "Db"
 "@internal
  In ARM/Thumb-2 state a const_int, const_double or const_vector that can
  be generated with three Data Processing insns."
 (and (match_code "const_double,const_int,const_vector")
      (match_test "TARGET_32BIT && arm_const_double_inline_cost (op) == 3")))

(define_constraint "Dc"
 "@internal
  In ARM/Thumb-2 state a const_int, const_double or const_vector that can
  be generated with four Data Processing insns.  This pattern is disabled
  if optimizing for space or when we have load-delay slots to fill."
 (and (match_code "const_double,const_int,const_vector")
      (match_test "TARGET_32BIT && arm_const_double_inline_cost (op) == 4
		   && !(optimize_size || arm_ld_sched)")))

(define_constraint "Dd"
 "@internal
  In ARM/Thumb-2 state a const_int that can be used by insn adddi."
 (and (match_code "const_int")
      (match_test "TARGET_32BIT && const_ok_for_dimode_op (ival, PLUS)")))

(define_constraint "Di"
 "@internal
  In ARM/Thumb-2 state a const_int or const_double where both the high
  and low SImode words can be generated as immediates in 32-bit instructions."
 (and (match_code "const_double,const_int")
      (match_test "TARGET_32BIT && arm_const_double_by_immediates (op)")))

(define_constraint "Dj"
  "@internal
   In cores with the v6t2 ISA, a constant with exactly one consecutive
   string of zero bits."
  (and (match_code "const_int")
       (match_test "arm_arch_thumb2
		    && exact_log2 (~ival + (~ival & -~ival)) >= 0")))

(define_constraint "Dm"
 "@internal
  In ARM/Thumb-2 state a const_vector which can be loaded with a Neon vmov
  immediate instruction."
 (and (match_code "const_vector")
      (match_test "TARGET_32BIT
		   && imm_for_neon_mov_operand (op, GET_MODE (op))")))

(define_constraint "Dn"
 "@internal
  In ARM/Thumb-2 state a DImode const_int which can be loaded with a Neon vmov
  immediate instruction."
 (and (match_code "const_int")
      (match_test "TARGET_32BIT && imm_for_neon_mov_operand (op, DImode)")))

(define_constraint "DN"
 "@internal
  In ARM/Thumb-2 state a TImode const_int which can be loaded with a Neon vmov
  immediate instruction."
 (and (match_code "const_int")
      (match_test "TARGET_32BIT && imm_for_neon_mov_operand (op, TImode)")))

(define_constraint "Dl"
 "@internal
  In ARM/Thumb-2 state a const_vector which can be used with a Neon vorr or
  vbic instruction."
 (and (match_code "const_vector")
      (match_test "TARGET_32BIT
		   && imm_for_neon_logic_operand (op, GET_MODE (op))")))

(define_constraint "DL"
 "@internal
  In ARM/Thumb-2 state a const_vector which can be used with a Neon vorn or
  vand instruction."
 (and (match_code "const_vector")
      (match_test "TARGET_32BIT
		   && imm_for_neon_inv_logic_operand (op, GET_MODE (op))")))

(define_constraint "Do"
 "@internal
  In ARM/Thumb2 state valid offset for an ldrd/strd instruction."
 (and (match_code "const_int")
      (match_test "TARGET_LDRD && offset_ok_for_ldrd_strd (ival)")))

(define_constraint "Dv"
 "@internal
  In ARM/Thumb-2 state a const_double which can be used with a VFP fconsts
  instruction."
 (and (match_code "const_double")
      (match_test "TARGET_32BIT && vfp3_const_double_rtx (op)")))

(define_constraint "Dy"
 "@internal
  In ARM/Thumb-2 state a const_double which can be used with a VFP fconstd
  instruction."
 (and (match_code "const_double")
      (match_test "TARGET_32BIT && TARGET_VFP_DOUBLE && vfp3_const_double_rtx (op)")))

(define_constraint "Dt"
 "@internal
  In ARM/ Thumb2 a const_double which can be used with a vcvt.f32.s32 with fract bits operation"
  (and (match_code "const_double")
       (match_test "TARGET_32BIT && vfp3_const_double_for_fract_bits (op)")))

(define_constraint "Ds"
 "@internal
  In ARM/Thumb-2 state a const_vector which can be used as immediate
  in vshl instruction."
 (and (match_code "const_vector")
      (match_test "TARGET_32BIT
		   && imm_for_neon_lshift_operand (op, GET_MODE (op))")))

(define_constraint "Dp"
 "@internal
  In ARM/ Thumb2 a const_double which can be used with a vcvt.s32.f32 with bits operation"
  (and (match_code "const_double")
       (match_test "TARGET_32BIT
		    && vfp3_const_double_for_bits (op) > 0")))

(define_constraint "Tu"
  "@internal In ARM / Thumb-2 an integer constant iff literal pools are
   allowed."
  (and (match_test "CONSTANT_P (op)")
       (match_test "!arm_disable_literal_pool")))

(define_register_constraint "Ts" "(arm_restrict_it) ? LO_REGS : GENERAL_REGS"
 "For arm_restrict_it the core registers @code{r0}-@code{r7}.  GENERAL_REGS otherwise.")

(define_memory_constraint "Ua"
 "@internal
  An address valid for loading/storing register exclusive"
 (match_operand 0 "mem_noofs_operand"))

(define_memory_constraint "Uh"
 "@internal
  An address suitable for byte and half-word loads which does not point inside a constant pool"
 (and (match_code "mem")
      (match_test "arm_legitimate_address_p (GET_MODE (op), XEXP (op, 0), false) && !arm_is_constant_pool_ref (op)")))

(define_memory_constraint "Ut"
 "@internal
  In ARM/Thumb-2 state an address valid for loading/storing opaque structure
  types wider than TImode."
 (and (match_code "mem")
      (match_test "TARGET_32BIT && neon_struct_mem_operand (op)")))

(define_memory_constraint "Uv"
 "@internal
  In ARM/Thumb-2 state a valid VFP load/store address."
 (and (match_code "mem")
      (match_test "TARGET_32BIT && arm_coproc_mem_operand (op, FALSE)")))

(define_memory_constraint "Uj"
 "@internal
  In ARM/Thumb-2 state a VFP load/store address that supports writeback
  for Neon but not for MVE"
 (and (match_code "mem")
      (match_test "TARGET_32BIT")
      (match_test "TARGET_HAVE_MVE
                   ? arm_coproc_mem_operand_no_writeback (op)
                   : neon_vector_mem_operand (op, 2, true)")))

(define_memory_constraint "Uy"
 "@internal
  In ARM/Thumb-2 state a valid iWMMX load/store address."
 (and (match_code "mem")
      (match_test "TARGET_32BIT && arm_coproc_mem_operand (op, TRUE)")))

(define_memory_constraint "Un"
 "@internal
  In ARM/Thumb-2 state a valid address for Neon doubleword vector
  load/store instructions."
 (and (match_code "mem")
      (match_test "TARGET_32BIT && neon_vector_mem_operand (op, 0, true)")))

(define_memory_constraint "Um"
 "@internal
  In ARM/Thumb-2 state a valid address for Neon element and structure
  load/store instructions."
 (and (match_code "mem")
      (match_test "TARGET_32BIT && neon_vector_mem_operand (op, 2, true)")))

(define_memory_constraint "Us"
 "@internal
  In ARM/Thumb-2 state a valid address for non-offset loads/stores of
  quad-word values in four ARM registers."
 (and (match_code "mem")
      (match_test "TARGET_32BIT && neon_vector_mem_operand (op, 1, true)")))

(define_memory_constraint "Ux"
 "@internal
  In ARM/Thumb-2 state a valid address and load into CORE regs or only to
  LO_REGS based on mode of op."
 (and (match_code "mem")
      (match_test "(TARGET_HAVE_MVE || TARGET_HAVE_MVE_FLOAT)
		   && mve_vector_mem_operand (GET_MODE (op),
					      XEXP (op, 0), true)")))

(define_constraint "Ui"
  "@internal
   Match a constant (as per the 'i' constraint) provided that we have the
   literal pool available.  This is useful for load insns that would need
   to move such constants to the literal pool after RA."
 (match_test "!arm_disable_literal_pool && satisfies_constraint_i (op)"))

(define_memory_constraint "Uq"
 "@internal
  In ARM state an address valid in ldrsb instructions."
 (and (match_code "mem")
      (match_test "TARGET_ARM
		   && arm_legitimate_address_outer_p (GET_MODE (op), XEXP (op, 0),
						      SIGN_EXTEND, 0)
		   && !arm_is_constant_pool_ref (op)")))

(define_memory_constraint "Q"
 "@internal
  An address that is a single base register."
 (and (match_code "mem")
      (match_test "REG_P (XEXP (op, 0))")))

(define_memory_constraint "Uu"
 "@internal
  In Thumb state an address that is valid in 16bit encoding."
 (and (match_code "mem")
      (match_test "TARGET_THUMB
		   && thumb1_legitimate_address_p (GET_MODE (op), XEXP (op, 0),
						   0)")))

; The 16-bit post-increment LDR/STR accepted by thumb1_legitimate_address_p
; are actually LDM/STM instructions, so cannot be used to access unaligned
; data.
(define_memory_constraint "Uw"
 "@internal
  In Thumb state an address that is valid in 16bit encoding, and that can be
  used for unaligned accesses."
 (and (match_code "mem")
      (match_test "TARGET_THUMB
		   && thumb1_legitimate_address_p (GET_MODE (op), XEXP (op, 0),
						   0)
		   && GET_CODE (XEXP (op, 0)) != POST_INC")))

(define_constraint "US"
 "@internal
  US is a symbol reference."
 (match_code "symbol_ref")
)

(define_memory_constraint "Uz"
 "@internal
  A memory access that is accessible as an LDC/STC operand"
 (and (match_code "mem")
      (match_test "arm_coproc_ldc_stc_legitimate_address (op)")))

;; We used to have constraint letters for S and R in ARM state, but
;; all uses of these now appear to have been removed.

;; Additionally, we used to have a Q constraint in Thumb state, but
;; this wasn't really a valid memory constraint.  Again, all uses of
;; this now seem to have been removed.

