;; Predicate definitions for OpenRISC
;; Copyright (C) 2018-2024 Free Software Foundation, Inc.
;; Contributed by Stafford Horne

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

;; -------------------------------------------------------------------------
;; Predicates
;; -------------------------------------------------------------------------

(define_predicate "input_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "memory_operand")
       (and (match_code "const_int")
	    (match_test "satisfies_constraint_I (op)
			 || satisfies_constraint_K (op)
			 || satisfies_constraint_M (op)"))))

(define_predicate "const0_operand"
  (and (match_code "const_int,const_wide_int,const_double,const_vector")
       (match_test "op == CONST0_RTX (mode)")))

(define_predicate "reg_or_0_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "const0_operand")))

(define_predicate "reg_or_u6_operand"
  (if_then_else (match_code "const_int")
    (match_test "INTVAL (op) >= 0 && INTVAL (op) <= 0x3f")
    (match_operand 0 "register_operand")))

(define_predicate "reg_or_u16_operand"
  (if_then_else (match_code "const_int")
    (match_test "INTVAL (op) >= 0 && INTVAL (op) <= 0xffff")
    (match_operand 0 "register_operand")))

(define_predicate "reg_or_s16_operand"
  (if_then_else (match_code "const_int")
    (match_test "INTVAL (op) >= -32768 && INTVAL (op) <= 32767")
    (match_operand 0 "register_operand")))

(define_predicate "ror_reg_or_u6_operand"
  (if_then_else (match_code "const_int")
    (and (match_test "INTVAL (op) >= 0 && INTVAL (op) <= 0x3f")
	 (match_test "TARGET_RORI"))
    (and (match_operand 0 "register_operand")
	 (match_test "TARGET_ROR"))))

(define_predicate "call_insn_operand"
  (ior (match_code "symbol_ref")
       (match_operand 0 "register_operand")))

(define_predicate "high_operand"
  (match_code "symbol_ref,label_ref,const,unspec"))

;; Return true for relocations that must use MOVHI+ADDI
(define_predicate "losum_add_operand"
  (match_code "symbol_ref,label_ref,const,unspec"))

;; Return true for relocations that must use MOVHI+ORI
(define_predicate "losum_ior_operand"
  (and (match_code "unspec")
       (match_test "XINT(op, 1) == UNSPEC_TLSGD")))

;; Return true for a "virtual" or "soft" register that will be
;; adjusted to a "soft" or "hard" register during elimination.
(define_predicate "virtual_frame_reg_operand"
  (match_code "reg")
{
  unsigned regno = REGNO (op);
  return (regno != STACK_POINTER_REGNUM
	  && regno != HARD_FRAME_POINTER_REGNUM
	  && REGNO_PTR_FRAME_P (regno));
})

(define_predicate "equality_comparison_operator"
  (match_code "ne,eq"))

(define_predicate "fp_comparison_operator"
  (if_then_else (match_test "TARGET_FP_UNORDERED")
    (match_operand 0 "comparison_operator")
    (match_operand 0 "ordered_comparison_operator")))

;; Borrowed from rs6000
;; Return true if the operand is in volatile memory.  Note that during the
;; RTL generation phase, memory_operand does not return TRUE for volatile
;; memory references.  So this function allows us to recognize volatile
;; references where it's safe.
(define_predicate "volatile_mem_operand"
  (and (match_code "mem")
       (match_test "MEM_VOLATILE_P (op)")
       (if_then_else (match_test "reload_completed")
	 (match_operand 0 "memory_operand")
	 (match_test "memory_address_p (mode, XEXP (op, 0))"))))

;; Return true if the operand is a register or memory; including volatile
;; memory.
(define_predicate "reg_or_mem_operand"
  (ior (match_operand 0 "nonimmediate_operand")
       (match_operand 0 "volatile_mem_operand")))
