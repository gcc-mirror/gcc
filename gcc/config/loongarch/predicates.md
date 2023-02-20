;; Predicate definitions for LoongArch target.
;; Copyright (C) 2021-2023 Free Software Foundation, Inc.
;; Contributed by Loongson Ltd.
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

(define_predicate "const_uns_arith_operand"
  (and (match_code "const_int")
       (match_test "IMM12_OPERAND_UNSIGNED (INTVAL (op))")))

(define_predicate "uns_arith_operand"
  (ior (match_operand 0 "const_uns_arith_operand")
       (match_operand 0 "register_operand")))

(define_predicate "const_lu32i_operand"
  (and (match_code "const_int")
       (match_test "LU32I_OPERAND (INTVAL (op))")))

(define_predicate "const_lu52i_operand"
  (and (match_code "const_int")
       (match_test "LU52I_OPERAND (INTVAL (op))")))

(define_predicate "const_arith_operand"
  (and (match_code "const_int")
       (match_test "IMM12_OPERAND (INTVAL (op))")))

(define_predicate "const_imm16_operand"
  (and (match_code "const_int")
       (match_test "IMM16_OPERAND (INTVAL (op))")))

(define_predicate "arith_operand"
  (ior (match_operand 0 "const_arith_operand")
       (match_operand 0 "register_operand")))

(define_predicate "const_immalsl_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 1, 4)")))

(define_predicate "const_uimm5_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 0, 31)")))

(define_predicate "const_uimm14_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 0, 16383)")))

(define_predicate "const_uimm15_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 0, 32767)")))

(define_predicate "const_imm12_operand"
  (and (match_code "const_int")
       (match_test "IMM12_OPERAND (INTVAL (op))")))

(define_predicate "sle_operand"
  (and (match_code "const_int")
       (match_test "IMM12_OPERAND (INTVAL (op) + 1)")))

(define_predicate "sleu_operand"
  (and (match_operand 0 "sle_operand")
       (match_test "INTVAL (op) + 1 != 0")))

(define_predicate "const_0_operand"
  (and (match_code "const_int,const_double,const_vector")
       (match_test "op == CONST0_RTX (GET_MODE (op))")))

(define_predicate "reg_or_0_operand"
  (ior (match_operand 0 "const_0_operand")
       (match_operand 0 "register_operand")))

(define_predicate "const_1_operand"
  (and (match_code "const_int,const_double,const_vector")
       (match_test "op == CONST1_RTX (GET_MODE (op))")))

(define_predicate "reg_or_1_operand"
  (ior (match_operand 0 "const_1_operand")
       (match_operand 0 "register_operand")))

(define_predicate "lu52i_mask_operand"
  (and (match_code "const_int")
       (match_test "UINTVAL (op) == 0xfffffffffffff")))

(define_predicate "low_bitmask_operand"
  (and (match_code "const_int")
       (match_test "low_bitmask_len (mode, INTVAL (op)) > 12")))

(define_predicate "const_call_insn_operand"
  (match_code "const,symbol_ref,label_ref")
{
  /* Split symbol to high and low if return false.
     If defined TARGET_CMODEL_EXTREME, all symbol would be splited,
     else if offset is not zero, the symbol would be splited.  */

  enum loongarch_symbol_type symbol_type;
  loongarch_symbolic_constant_p (op, &symbol_type);

  rtx offset, x = op;
  split_const (x, &x, &offset);

  if (offset != const0_rtx)
    return false;

  /* When compiling with '-mcmodel=medium -mexplicit-relocs'
     symbols are splited in loongarch_legitimize_call_address.

     When compiling with '-mcmodel=medium -mno-explicit-relocs',
     first obtain the symbolic address or the address of the
     plt entry, and then perform an indirect jump, so return false.  */

  switch (symbol_type)
    {
    case SYMBOL_PCREL:
      if (TARGET_CMODEL_EXTREME
	  || (TARGET_CMODEL_MEDIUM && !TARGET_EXPLICIT_RELOCS))
	return false;
      else
	return 1;

    case SYMBOL_GOT_DISP:
      if (TARGET_CMODEL_EXTREME
	  || !flag_plt
	  || (flag_plt && TARGET_CMODEL_MEDIUM
	      && !TARGET_EXPLICIT_RELOCS))
	return false;
      else
	return 1;

    default:
      return false;
    }
})

(define_predicate "call_insn_operand"
  (ior (match_operand 0 "const_call_insn_operand")
       (match_operand 0 "register_operand")))

(define_predicate "is_const_call_local_symbol"
  (and (match_operand 0 "const_call_insn_operand")
       (ior (match_test "loongarch_global_symbol_p (op) == 0")
	    (match_test "loongarch_symbol_binds_local_p (op) != 0"))
       (match_test "CONSTANT_P (op)")))

(define_predicate "is_const_call_no_local_symbol"
  (and (match_operand 0 "const_call_insn_operand")
       (ior (match_test "loongarch_global_symbol_p (op) != 0")
	    (match_test "loongarch_symbol_binds_local_p (op) == 0")
       (match_test "loongarch_weak_symbol_p (op) != 0"))
       (match_test "CONSTANT_P (op)")))

;; A legitimate CONST_INT operand that takes more than one instruction
;; to load.
(define_predicate "splittable_const_int_operand"
  (match_code "const_int")
{
  /* Don't handle multi-word moves this way; we don't want to introduce
     the individual word-mode moves until after reload.  */
  if (GET_MODE_SIZE (mode) > UNITS_PER_WORD)
    return false;

  /* Otherwise check whether the constant can be loaded in a single
     instruction.  */
  return !LU12I_INT (op) && !IMM12_INT (op) && !IMM12_INT_UNSIGNED (op)
	 && !LU52I_INT (op);
})

(define_predicate "move_operand"
  (match_operand 0 "general_operand")
{
  enum loongarch_symbol_type symbol_type;

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
      return true;

    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
      return (loongarch_symbolic_constant_p (op, &symbol_type)
	      && (!TARGET_EXPLICIT_RELOCS
		  || !loongarch_split_symbol_type (symbol_type)));

    case HIGH:
      /* '-mno-explicit-relocs' don't generate high/low pairs.  */
      if (!TARGET_EXPLICIT_RELOCS)
	return false;

      op = XEXP (op, 0);
      return (loongarch_symbolic_constant_p (op, &symbol_type)
	      && loongarch_split_symbol_type (symbol_type));

    default:
      return true;
    }
})

(define_predicate "symbolic_operand"
  (match_code "const,symbol_ref,label_ref")
{
  enum loongarch_symbol_type type;
  return loongarch_symbolic_constant_p (op, &type);
})

(define_predicate "equality_operator"
  (match_code "eq,ne"))

(define_predicate "order_operator"
  (match_code "lt,ltu,le,leu,ge,geu,gt,gtu"))

;; For NE, cstore uses sltu instructions in which the first operand is $0.

(define_predicate "loongarch_cstore_operator"
  (match_code "ne,eq,gt,gtu,ge,geu,lt,ltu,le,leu"))

(define_predicate "small_data_pattern"
  (and (match_code "set,parallel,unspec,unspec_volatile,prefetch")
       (match_test "loongarch_small_data_pattern_p (op)")))

;; Return 1 if the operand is in non-volatile memory.
(define_predicate "non_volatile_mem_operand"
  (and (match_operand 0 "memory_operand")
       (not (match_test "MEM_VOLATILE_P (op)"))))
