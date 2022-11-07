;; Predicate description for RISC-V target.
;; Copyright (C) 2011-2022 Free Software Foundation, Inc.
;; Contributed by Andrew Waterman (andrew@sifive.com).
;; Based on MIPS target for GNU compiler.
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

(define_predicate "const_arith_operand"
  (and (match_code "const_int")
       (match_test "SMALL_OPERAND (INTVAL (op))")))

(define_predicate "arith_operand"
  (ior (match_operand 0 "const_arith_operand")
       (match_operand 0 "register_operand")))

(define_predicate "lui_operand"
  (and (match_code "const_int")
       (match_test "LUI_OPERAND (INTVAL (op))")))

(define_predicate "sfb_alu_operand"
  (ior (match_operand 0 "arith_operand")
       (match_operand 0 "lui_operand")))

(define_predicate "const_csr_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 0, 31)")))

(define_predicate "csr_operand"
  (ior (match_operand 0 "const_csr_operand")
       (match_operand 0 "register_operand")))

(define_predicate "sle_operand"
  (and (match_code "const_int")
       (match_test "SMALL_OPERAND (INTVAL (op) + 1)")))

(define_predicate "sleu_operand"
  (and (match_operand 0 "sle_operand")
       (match_test "INTVAL (op) + 1 != 0")))

(define_predicate "const_0_operand"
  (and (match_code "const_int,const_wide_int,const_vector")
       (match_test "op == CONST0_RTX (GET_MODE (op))")))

(define_predicate "reg_or_0_operand"
  (ior (match_operand 0 "const_0_operand")
       (match_operand 0 "register_operand")))

;; Only use branch-on-bit sequences when the mask is not an ANDI immediate.
(define_predicate "branch_on_bit_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) >= IMM_BITS - 1")))

;; A legitimate CONST_INT operand that takes more than one instruction
;; to load.
(define_predicate "splittable_const_int_operand"
  (match_code "const_int")
{
  /* Don't handle multi-word moves this way; we don't want to introduce
     the individual word-mode moves until after reload.  */
  if (GET_MODE_SIZE (mode).to_constant () > UNITS_PER_WORD)
    return false;

  /* Check whether the constant can be loaded in a single
     instruction with zbs extensions.  */
  if (TARGET_ZBS && SINGLE_BIT_MASK_OPERAND (INTVAL (op)))
    return false;

  /* Otherwise check whether the constant can be loaded in a single
     instruction.  */
  return !LUI_OPERAND (INTVAL (op)) && !SMALL_OPERAND (INTVAL (op));
})

(define_predicate "p2m1_shift_operand"
  (match_code "const_int")
{
  int val = exact_log2 (INTVAL (op) + 1);
  if (val < 12)
    return false;
  return true;
 })

(define_predicate "high_mask_shift_operand"
  (match_code "const_int")
{
  int val1 = clz_hwi (~ INTVAL (op));
  int val0 = ctz_hwi (INTVAL (op));
  if ((val0 + val1 == BITS_PER_WORD)
      && val0 > 31 && val0 < 64)
    return true;
  return false;
})

(define_predicate "move_operand"
  (match_operand 0 "general_operand")
{
  enum riscv_symbol_type symbol_type;

  /* The thinking here is as follows:

     (1) The move expanders should split complex load sequences into
	 individual instructions.  Those individual instructions can
	 then be optimized by all rtl passes.

     (2) The target of pre-reload load sequences should not be used
	 to store temporary results.  If the target register is only
	 assigned one value, reload can rematerialize that value
	 on demand, rather than spill it to the stack.

     (3) If we allowed pre-reload passes like combine and cse to recreate
	 complex load sequences, we would want to be able to split the
	 sequences before reload as well, so that the pre-reload scheduler
	 can see the individual instructions.  This falls foul of (2);
	 the splitter would be forced to reuse the target register for
	 intermediate results.

     (4) We want to define complex load splitters for combine.  These
	 splitters can request a temporary scratch register, which avoids
	 the problem in (2).  They allow things like:

	      (set (reg T1) (high SYM))
	      (set (reg T2) (low (reg T1) SYM))
	      (set (reg X) (plus (reg T2) (const_int OFFSET)))

	 to be combined into:

	      (set (reg T3) (high SYM+OFFSET))
	      (set (reg X) (lo_sum (reg T3) SYM+OFFSET))

	 if T2 is only used this once.  */
  switch (GET_CODE (op))
    {
    case CONST_INT:
      return !splittable_const_int_operand (op, mode);

    case CONST_POLY_INT:
      return known_eq (rtx_to_poly_int64 (op), BYTES_PER_RISCV_VECTOR);

    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
      return riscv_symbolic_constant_p (op, &symbol_type)
	      && !riscv_split_symbol_type (symbol_type);

    case HIGH:
      op = XEXP (op, 0);
      return riscv_symbolic_constant_p (op, &symbol_type)
	      && riscv_split_symbol_type (symbol_type)
	      && symbol_type != SYMBOL_PCREL;

    default:
      return true;
    }
})

(define_predicate "symbolic_operand"
  (match_code "const,symbol_ref,label_ref")
{
  enum riscv_symbol_type type;
  return riscv_symbolic_constant_p (op, &type);
})

(define_predicate "absolute_symbolic_operand"
  (match_code "const,symbol_ref,label_ref")
{
  enum riscv_symbol_type type;
  return (riscv_symbolic_constant_p (op, &type)
	  && (type == SYMBOL_ABSOLUTE || type == SYMBOL_PCREL));
})

(define_predicate "plt_symbolic_operand"
  (match_code "const,symbol_ref,label_ref")
{
  enum riscv_symbol_type type;
  return (riscv_symbolic_constant_p (op, &type)
	  && type == SYMBOL_GOT_DISP && !SYMBOL_REF_WEAK (op) && TARGET_PLT);
})

(define_predicate "call_insn_operand"
  (ior (match_operand 0 "absolute_symbolic_operand")
       (match_operand 0 "plt_symbolic_operand")
       (match_operand 0 "register_operand")))

(define_predicate "modular_operator"
  (match_code "plus,minus,mult,ashift"))

(define_predicate "equality_operator"
  (match_code "eq,ne"))

(define_predicate "order_operator"
  (match_code "eq,ne,lt,ltu,le,leu,ge,geu,gt,gtu"))

(define_predicate "signed_order_operator"
  (match_code "eq,ne,lt,le,ge,gt"))

(define_predicate "subreg_lowpart_operator"
  (ior (match_code "truncate")
       (and (match_code "subreg")
            (match_test "subreg_lowpart_p (op)"))))

(define_predicate "fp_native_comparison"
  (match_code "eq,lt,le,gt,ge"))

(define_predicate "fp_scc_comparison"
  (match_code "unordered,ordered,unlt,unge,unle,ungt,ltgt,ne,eq,lt,le,gt,ge"))

(define_predicate "fp_branch_comparison"
  (match_code "unordered,ordered,unlt,unge,unle,ungt,uneq,ltgt,ne,eq,lt,le,gt,ge"))

(define_special_predicate "gpr_save_operation"
  (match_code "parallel")
{
  return riscv_gpr_save_operation_p (op);
})

;; Predicates for the ZBS extension.
(define_predicate "single_bit_mask_operand"
  (and (match_code "const_int")
       (match_test "SINGLE_BIT_MASK_OPERAND (UINTVAL (op))")))

(define_predicate "not_single_bit_mask_operand"
  (and (match_code "const_int")
       (match_test "SINGLE_BIT_MASK_OPERAND (~UINTVAL (op))")))

(define_predicate "const31_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) == 31")))

(define_predicate "const63_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) == 63")))

(define_predicate "imm5_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) < 5")))

;; A const_int for sh1add/sh2add/sh3add
(define_predicate "imm123_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 1, 3)")))

;; A CONST_INT operand that consists of a single run of consecutive set bits.
(define_predicate "consecutive_bits_operand"
  (match_code "const_int")
{
	unsigned HOST_WIDE_INT val = UINTVAL (op);
	if (exact_log2 ((val >> ctz_hwi (val)) + 1) < 0)
	        return false;

	return true;
})

;; Predicates for the V extension.
(define_special_predicate "vector_length_operand"
  (ior (match_operand 0 "pmode_register_operand")
       (match_operand 0 "const_csr_operand")))

(define_predicate "reg_or_mem_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "memory_operand")))

(define_predicate "vector_move_operand"
  (ior (match_operand 0 "nonimmediate_operand")
       (match_code "const_vector")))

(define_predicate "vector_mask_operand"
  (ior (match_operand 0 "register_operand")
       (match_test "op == CONSTM1_RTX (GET_MODE (op))")))

(define_predicate "vector_merge_operand"
  (ior (match_operand 0 "memory_operand")
       (ior (match_operand 0 "register_operand")
	    (match_test "GET_CODE (op) == UNSPEC
			 && (XINT (op, 1) == UNSPEC_VUNDEF)"))))
