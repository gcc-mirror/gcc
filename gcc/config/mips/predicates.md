;; Predicate definitions for MIPS.
;; Copyright (C) 2004, 2007, 2008 Free Software Foundation, Inc.
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
       (match_test "SMALL_OPERAND_UNSIGNED (INTVAL (op))")))

(define_predicate "uns_arith_operand"
  (ior (match_operand 0 "const_uns_arith_operand")
       (match_operand 0 "register_operand")))

(define_predicate "const_arith_operand"
  (and (match_code "const_int")
       (match_test "SMALL_OPERAND (INTVAL (op))")))

(define_predicate "arith_operand"
  (ior (match_operand 0 "const_arith_operand")
       (match_operand 0 "register_operand")))

(define_predicate "const_uimm6_operand"
  (and (match_code "const_int")
       (match_test "UIMM6_OPERAND (INTVAL (op))")))

(define_predicate "const_imm10_operand"
  (and (match_code "const_int")
       (match_test "IMM10_OPERAND (INTVAL (op))")))

(define_predicate "reg_imm10_operand"
  (ior (match_operand 0 "const_imm10_operand")
       (match_operand 0 "register_operand")))

(define_predicate "sle_operand"
  (and (match_code "const_int")
       (match_test "SMALL_OPERAND (INTVAL (op) + 1)")))

(define_predicate "sleu_operand"
  (and (match_operand 0 "sle_operand")
       (match_test "INTVAL (op) + 1 != 0")))

(define_predicate "const_0_operand"
  (and (match_code "const_int,const_double,const_vector")
       (match_test "op == CONST0_RTX (GET_MODE (op))")))

(define_predicate "reg_or_0_operand"
  (ior (and (match_operand 0 "const_0_operand")
	    (not (match_test "TARGET_MIPS16")))
       (match_operand 0 "register_operand")))

(define_predicate "const_1_operand"
  (and (match_code "const_int,const_double,const_vector")
       (match_test "op == CONST1_RTX (GET_MODE (op))")))

(define_predicate "reg_or_1_operand"
  (ior (match_operand 0 "const_1_operand")
       (match_operand 0 "register_operand")))

;; This is used for indexing into vectors, and hence only accepts const_int.
(define_predicate "const_0_or_1_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 0, 1)")))

(define_predicate "const_2_or_3_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 2, 3)")))

(define_predicate "const_0_to_3_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 0, 3)")))

(define_predicate "qi_mask_operand"
  (and (match_code "const_int")
       (match_test "UINTVAL (op) == 0xff")))

(define_predicate "hi_mask_operand"
  (and (match_code "const_int")
       (match_test "UINTVAL (op) == 0xffff")))

(define_predicate "si_mask_operand"
  (and (match_code "const_int")
       (match_test "UINTVAL (op) == 0xffffffff")))

(define_predicate "and_load_operand"
  (ior (match_operand 0 "qi_mask_operand")
       (match_operand 0 "hi_mask_operand")
       (match_operand 0 "si_mask_operand")))

(define_predicate "low_bitmask_operand"
  (and (match_test "ISA_HAS_EXT_INS")
       (match_code "const_int")
       (match_test "low_bitmask_len (mode, INTVAL (op)) > 16")))

(define_predicate "and_reg_operand"
  (ior (match_operand 0 "register_operand")
       (and (not (match_test "TARGET_MIPS16"))
	    (match_operand 0 "const_uns_arith_operand"))
       (match_operand 0 "low_bitmask_operand")
       (match_operand 0 "si_mask_operand")))

(define_predicate "and_operand"
  (ior (match_operand 0 "and_load_operand")
       (match_operand 0 "and_reg_operand")))

(define_predicate "d_operand"
  (and (match_code "reg")
       (match_test "TARGET_MIPS16
		    ? M16_REG_P (REGNO (op))
		    : GP_REG_P (REGNO (op))")))

(define_predicate "lo_operand"
  (and (match_code "reg")
       (match_test "REGNO (op) == LO_REGNUM")))

(define_predicate "hilo_operand"
  (and (match_code "reg")
       (match_test "MD_REG_P (REGNO (op))")))

(define_predicate "fcc_reload_operand"
  (and (match_code "reg,subreg")
       (match_test "ST_REG_P (true_regnum (op))")))

(define_predicate "muldiv_target_operand"
  (if_then_else (match_test "TARGET_MIPS16")
		(match_operand 0 "hilo_operand")
		(match_operand 0 "register_operand")))

(define_special_predicate "pc_or_label_operand"
  (match_code "pc,label_ref"))

(define_predicate "const_call_insn_operand"
  (match_code "const,symbol_ref,label_ref")
{
  enum mips_symbol_type symbol_type;

  if (!mips_symbolic_constant_p (op, SYMBOL_CONTEXT_CALL, &symbol_type))
    return false;

  switch (symbol_type)
    {
    case SYMBOL_ABSOLUTE:
      /* We can only use direct calls if we're sure that the target
	 function does not need $25 to be valid on entry.  */
      if (mips_use_pic_fn_addr_reg_p (op))
	return false;

      /* If -mlong-calls or if this function has an explicit long_call
	 attribute, we must use register addressing.  The
	 SYMBOL_FLAG_LONG_CALL bit is set by mips_encode_section_info.  */
      return !(GET_CODE (op) == SYMBOL_REF && SYMBOL_REF_LONG_CALL_P (op));

    case SYMBOL_GOT_DISP:
      /* Without explicit relocs, there is no special syntax for
	 loading the address of a call destination into a register.
	 Using "la $25,foo; jal $25" would prevent the lazy binding
	 of "foo", so keep the address of global symbols with the
	 jal macro.  */
      return !TARGET_EXPLICIT_RELOCS;

    default:
      return false;
    }
})

(define_predicate "call_insn_operand"
  (ior (match_operand 0 "const_call_insn_operand")
       (match_operand 0 "register_operand")))

;; A legitimate CONST_INT operand that takes more than one instruction
;; to load.
(define_predicate "splittable_const_int_operand"
  (match_code "const_int")
{
  /* When generating mips16 code, TARGET_LEGITIMATE_CONSTANT_P rejects
     CONST_INTs that can't be loaded using simple insns.  */
  if (TARGET_MIPS16)
    return false;

  /* Don't handle multi-word moves this way; we don't want to introduce
     the individual word-mode moves until after reload.  */
  if (GET_MODE_SIZE (mode) > UNITS_PER_WORD)
    return false;

  /* Otherwise check whether the constant can be loaded in a single
     instruction.  */
  return !LUI_INT (op) && !SMALL_INT (op) && !SMALL_INT_UNSIGNED (op);
})

(define_predicate "move_operand"
  ;; Allow HI and LO to be used as the source of a MIPS16 move.
  (ior (match_operand 0 "general_operand")
       (match_operand 0 "hilo_operand"))
{
  enum mips_symbol_type symbol_type;

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

    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
      if (CONST_GP_P (op))
	return true;
      return (mips_symbolic_constant_p (op, SYMBOL_CONTEXT_LEA, &symbol_type)
	      && !mips_split_p[symbol_type]);

    case HIGH:
      op = XEXP (op, 0);
      return (mips_symbolic_constant_p (op, SYMBOL_CONTEXT_LEA, &symbol_type)
	      && !mips_split_hi_p[symbol_type]);

    default:
      return true;
    }
})

(define_predicate "cprestore_save_slot_operand"
  (and (match_code "mem")
       (match_test "mips_cprestore_address_p (XEXP (op, 0), false)")))

(define_predicate "cprestore_load_slot_operand"
  (and (match_code "mem")
       (match_test "mips_cprestore_address_p (XEXP (op, 0), true)")))

(define_predicate "consttable_operand"
  (match_test "CONSTANT_P (op)"))

(define_predicate "symbolic_operand"
  (match_code "const,symbol_ref,label_ref")
{
  enum mips_symbol_type type;
  return mips_symbolic_constant_p (op, SYMBOL_CONTEXT_LEA, &type);
})

(define_predicate "absolute_symbolic_operand"
  (match_code "const,symbol_ref,label_ref")
{
  enum mips_symbol_type type;
  return (mips_symbolic_constant_p (op, SYMBOL_CONTEXT_LEA, &type)
	  && type == SYMBOL_ABSOLUTE);
})

(define_predicate "symbolic_operand_with_high"
  (match_code "const,symbol_ref,label_ref")
{
  enum mips_symbol_type type;
  return (mips_symbolic_constant_p (op, SYMBOL_CONTEXT_LEA, &type)
	  && mips_hi_relocs[(int) type]);
})

(define_predicate "force_to_mem_operand"
  (match_code "const,symbol_ref,label_ref")
{
  enum mips_symbol_type symbol_type;
  return (mips_symbolic_constant_p (op, SYMBOL_CONTEXT_LEA, &symbol_type)
	  && mips_use_pcrel_pool_p[(int) symbol_type]);
})

(define_predicate "got_disp_operand"
  (match_code "const,symbol_ref,label_ref")
{
  enum mips_symbol_type type;
  return (mips_symbolic_constant_p (op, SYMBOL_CONTEXT_LEA, &type)
	  && type == SYMBOL_GOT_DISP);
})

(define_predicate "got_page_ofst_operand"
  (match_code "const,symbol_ref,label_ref")
{
  enum mips_symbol_type type;
  return (mips_symbolic_constant_p (op, SYMBOL_CONTEXT_LEA, &type)
	  && type == SYMBOL_GOT_PAGE_OFST);
})

(define_predicate "tls_reloc_operand"
  (match_code "const,symbol_ref,label_ref")
{
  enum mips_symbol_type type;
  return (mips_symbolic_constant_p (op, SYMBOL_CONTEXT_LEA, &type)
	  && (type == SYMBOL_DTPREL || type == SYMBOL_TPREL));
})

(define_predicate "symbol_ref_operand"
  (match_code "symbol_ref"))

(define_predicate "stack_operand"
  (and (match_code "mem")
       (match_test "mips_stack_address_p (XEXP (op, 0), GET_MODE (op))")))

(define_predicate "macc_msac_operand"
  (ior (and (match_code "plus") (match_test "ISA_HAS_MACC"))
       (and (match_code "minus") (match_test "ISA_HAS_MSAC")))
{
  rtx mult = XEXP (op, GET_CODE (op) == PLUS ? 0 : 1);
  rtx accum = XEXP (op, GET_CODE (op) == PLUS ? 1 : 0);
  return (GET_CODE (mult) == MULT
	  && REG_P (XEXP (mult, 0))
	  && REG_P (XEXP (mult, 1))
	  && REG_P (accum));
})


(define_predicate "equality_operator"
  (match_code "eq,ne"))

(define_predicate "extend_operator"
  (match_code "zero_extend,sign_extend"))

(define_predicate "trap_comparison_operator"
  (match_code "eq,ne,lt,ltu,ge,geu"))

(define_predicate "order_operator"
  (match_code "lt,ltu,le,leu,ge,geu,gt,gtu"))

;; For NE, cstore uses sltu instructions in which the first operand is $0.
;; This isn't possible in mips16 code.

(define_predicate "mips_cstore_operator"
  (ior (match_code "eq,gt,gtu,ge,geu,lt,ltu,le,leu")
       (and (match_code "ne") (not (match_test "TARGET_MIPS16")))))

(define_predicate "small_data_pattern"
  (and (match_code "set,parallel,unspec,unspec_volatile,prefetch")
       (match_test "mips_small_data_pattern_p (op)")))
