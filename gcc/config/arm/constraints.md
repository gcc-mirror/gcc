;; Constraint definitions for ARM and Thumb
;; Copyright (C) 2006, 2007, 2008 Free Software Foundation, Inc.
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
;; - in ARM/Thumb-2 state: f, t, v, w, x, y, z
;; - in Thumb state: h, b
;; - in both states: l, c, k
;; In ARM state, 'l' is an alias for 'r'

;; The following normal constraints have been used:
;; in ARM/Thumb-2 state: G, H, I, J, K, L, M
;; in Thumb-1 state: I, J, K, L, M, N, O

;; The following multi-letter normal constraints have been used:
;; in ARM/Thumb-2 state: Da, Db, Dc, Dn, Dl, DL, Dv
;; in Thumb-2 state: Ps, Pt

;; The following memory constraints have been used:
;; in ARM/Thumb-2 state: Q, Ut, Uv, Uy, Un, Us
;; in ARM state: Uq


(define_register_constraint "f" "TARGET_ARM ? FPA_REGS : NO_REGS"
 "Legacy FPA registers @code{f0}-@code{f7}.")

(define_register_constraint "t" "TARGET_32BIT ? VFP_LO_REGS : NO_REGS"
 "The VFP registers @code{s0}-@code{s31}.")

(define_register_constraint "v" "TARGET_ARM ? CIRRUS_REGS : NO_REGS"
 "The Cirrus Maverick co-processor registers.")

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

(define_register_constraint "k" "STACK_REG"
 "@internal The stack register.")

(define_register_constraint "b" "TARGET_THUMB ? BASE_REGS : NO_REGS"
 "@internal
  Thumb only.  The union of the low registers and the stack register.")

(define_register_constraint "c" "CC_REG"
 "@internal The condition code register.")

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
				 || ((ival & (ival - 1)) == 0))
		   : ((ival >= 0 && ival <= 1020) && ((ival & 3) == 0))")))

(define_constraint "N"
 "In ARM/Thumb-2 state a constant suitable for a MOVW instruction.
  In Thumb-1 state a constant in the range 0-31."
 (and (match_code "const_int")
      (match_test "TARGET_32BIT ? arm_arch_thumb2 && ((ival & 0xffff0000) == 0)
				: (ival >= 0 && ival <= 31)")))

(define_constraint "O"
 "In Thumb-1 state a constant that is a multiple of 4 in the range
  @minus{}508-508."
 (and (match_code "const_int")
      (match_test "TARGET_THUMB1 && ival >= -508 && ival <= 508
		   && ((ival & 3) == 0)")))

(define_constraint "Ps"
  "@internal In Thumb-2 state a constant in the range -255 to +255"
  (and (match_code "const_int")
       (match_test "TARGET_THUMB2 && ival >= -255 && ival <= 255")))

(define_constraint "Pt"
  "@internal In Thumb-2 state a constant in the range -7 to +7"
  (and (match_code "const_int")
       (match_test "TARGET_THUMB2 && ival >= -7 && ival <= 7")))

(define_constraint "G"
 "In ARM/Thumb-2 state a valid FPA immediate constant."
 (and (match_code "const_double")
      (match_test "TARGET_32BIT && arm_const_double_rtx (op)")))

(define_constraint "H"
 "In ARM/Thumb-2 state a valid FPA immediate constant when negated."
 (and (match_code "const_double")
      (match_test "TARGET_32BIT && neg_const_double_rtx_ok_for_fpa (op)")))

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

(define_constraint "Dn"
 "@internal
  In ARM/Thumb-2 state a const_vector which can be loaded with a Neon vmov
  immediate instruction."
 (and (match_code "const_vector")
      (match_test "TARGET_32BIT
		   && imm_for_neon_mov_operand (op, GET_MODE (op))")))

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

(define_constraint "Dv"
 "@internal
  In ARM/Thumb-2 state a const_double which can be used with a VFP fconsts
  or fconstd instruction."
 (and (match_code "const_double")
      (match_test "TARGET_32BIT && vfp3_const_double_rtx (op)")))

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

(define_memory_constraint "Uy"
 "@internal
  In ARM/Thumb-2 state a valid iWMMX load/store address."
 (and (match_code "mem")
      (match_test "TARGET_32BIT && arm_coproc_mem_operand (op, TRUE)")))

(define_memory_constraint "Un"
 "@internal
  In ARM/Thumb-2 state a valid address for Neon element and structure
  load/store instructions."
 (and (match_code "mem")
      (match_test "TARGET_32BIT && neon_vector_mem_operand (op, FALSE)")))

(define_memory_constraint "Us"
 "@internal
  In ARM/Thumb-2 state a valid address for non-offset loads/stores of
  quad-word values in four ARM registers."
 (and (match_code "mem")
      (match_test "TARGET_32BIT && neon_vector_mem_operand (op, TRUE)")))

(define_memory_constraint "Uq"
 "@internal
  In ARM state an address valid in ldrsb instructions."
 (and (match_code "mem")
      (match_test "TARGET_ARM
		   && arm_legitimate_address_p (GET_MODE (op), XEXP (op, 0),
						SIGN_EXTEND, 0)")))

(define_memory_constraint "Q"
 "@internal
  In ARM/Thumb-2 state an address that is a single base register."
 (and (match_code "mem")
      (match_test "REG_P (XEXP (op, 0))")))

;; We used to have constraint letters for S and R in ARM state, but
;; all uses of these now appear to have been removed.

;; Additionally, we used to have a Q constraint in Thumb state, but
;; this wasn't really a valid memory constraint.  Again, all uses of
;; this now seem to have been removed.
