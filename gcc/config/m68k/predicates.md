;; Predicate definitions for Motorola 68000.
;; Copyright (C) 2005-2019 Free Software Foundation, Inc.
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

;; Special case of a general operand that's used as a source
;; operand. Use this to permit reads from PC-relative memory when
;; -mpcrel is specified.

(define_predicate "general_src_operand"
  (match_code "const_int,const_double,const,symbol_ref,label_ref,subreg,reg,mem")
{
  if (TARGET_PCREL
      && GET_CODE (op) == MEM
      && (GET_CODE (XEXP (op, 0)) == SYMBOL_REF
	  || GET_CODE (XEXP (op, 0)) == LABEL_REF
	  || GET_CODE (XEXP (op, 0)) == CONST))
    return 1;
  return general_operand (op, mode);
})

;; Special case of a nonimmediate operand that's used as a source. Use
;; this to permit reads from PC-relative memory when -mpcrel is
;; specified.

(define_predicate "nonimmediate_src_operand"
  (match_code "subreg,reg,mem")
{
  if (TARGET_PCREL && GET_CODE (op) == MEM
      && (GET_CODE (XEXP (op, 0)) == SYMBOL_REF
	  || GET_CODE (XEXP (op, 0)) == LABEL_REF
	  || GET_CODE (XEXP (op, 0)) == CONST))
    return 1;
  return nonimmediate_operand (op, mode);
})

;; Special case of a memory operand that's used as a source. Use this
;; to permit reads from PC-relative memory when -mpcrel is specified.

(define_predicate "memory_src_operand"
  (match_code "subreg,mem")
{
  if (TARGET_PCREL && GET_CODE (op) == MEM
      && (GET_CODE (XEXP (op, 0)) == SYMBOL_REF
	  || GET_CODE (XEXP (op, 0)) == LABEL_REF
	  || GET_CODE (XEXP (op, 0)) == CONST))
    return 1;
  return memory_operand (op, mode);
})

;; Similar to general_operand, but exclude stack_pointer_rtx.

(define_predicate "not_sp_operand"
  (match_code "subreg,reg,mem")
{
  return op != stack_pointer_rtx && nonimmediate_operand (op, mode);
})

;; Predicate that accepts only a pc-relative address.  This is needed
;; because pc-relative addresses don't satisfy the predicate
;; "general_src_operand".

(define_predicate "pcrel_address"
  (match_code "symbol_ref,label_ref,const"))

;; Accept integer operands in the range 0..0xffffffff.  We have to
;; check the range carefully since this predicate is used in DImode
;; contexts.  Also, we need some extra crud to make it work when
;; hosted on 64-bit machines.

(define_predicate "const_uint32_operand"
  (match_code "const_int,const_double")
{
  /* It doesn't make sense to ask this question with a mode that is
     not larger than 32 bits.  */
  gcc_assert (GET_MODE_BITSIZE (mode) > 32);

#if HOST_BITS_PER_WIDE_INT > 32
  /* All allowed constants will fit a CONST_INT.  */
  return (GET_CODE (op) == CONST_INT
	  && (INTVAL (op) >= 0 && INTVAL (op) <= 0xffffffffL));
#else
  return (GET_CODE (op) == CONST_INT
	  || (GET_CODE (op) == CONST_DOUBLE && CONST_DOUBLE_HIGH (op) == 0));
#endif
})

;; Accept integer operands in the range -0x80000000..0x7fffffff.  We
;; have to check the range carefully since this predicate is used in
;; DImode contexts.

(define_predicate "const_sint32_operand"
  (match_code "const_int")
{
  /* It doesn't make sense to ask this question with a mode that is
     not larger than 32 bits.  */
  gcc_assert (GET_MODE_BITSIZE (mode) > 32);

  /* All allowed constants will fit a CONST_INT.  */
  return (GET_CODE (op) == CONST_INT
	  && (INTVAL (op) >= (-0x7fffffff - 1) && INTVAL (op) <= 0x7fffffff));
})

(define_predicate "m68k_cstore_comparison_operator"
  (if_then_else (match_test "TARGET_68881")
	        (match_operand 0 "comparison_operator")
		(match_operand 0 "ordered_comparison_operator")))

;; Check for sign_extend or zero_extend.  Used for bit-count operands.

(define_predicate "extend_operator"
  (match_code "sign_extend,zero_extend"))

;; Returns true if OP is either a symbol reference or a sum of a
;; symbol reference and a constant.  This predicate is for "raw"
;; symbol references not yet processed by legitimize*_address,
;; hence we do not handle UNSPEC_{XGOT, TLS, XTLS} here.

(define_predicate "symbolic_operand"
  (match_code "symbol_ref,label_ref,const")
{
  switch (GET_CODE (op))
    {
    case SYMBOL_REF:
    case LABEL_REF:
      return true;

    case CONST:
      op = XEXP (op, 0);
      return ((GET_CODE (XEXP (op, 0)) == SYMBOL_REF
	       || GET_CODE (XEXP (op, 0)) == LABEL_REF)
	      && GET_CODE (XEXP (op, 1)) == CONST_INT);

#if 0 /* Deleted, with corresponding change in m68k.h,
	 so as to fit the specs.  No CONST_DOUBLE is ever symbolic.  */
    case CONST_DOUBLE:
      return GET_MODE (op) == mode;
#endif

    default:
      return false;
    }
})

;; A constant that can be used the address in a call insn
(define_predicate "const_call_operand"
  (ior (match_operand 0 "const_int_operand")
       (and (match_test "m68k_symbolic_call != NULL")
	    (match_operand 0 "symbolic_operand"))))

;; An operand that can be used as the address in a call insn.
(define_predicate "call_operand"
  (ior (match_operand 0 "const_call_operand")
       (match_operand 0 "register_operand")))

;; A constant that can be used the address in a sibcall insn
(define_predicate "const_sibcall_operand"
  (ior (match_operand 0 "const_int_operand")
       (and (match_test "m68k_symbolic_jump != NULL")
	    (match_operand 0 "symbolic_operand"))))

;; An operand that can be used as the address in a sibcall insn.
(define_predicate "sibcall_operand"
  (ior (match_operand 0 "const_sibcall_operand")
       (and (match_code "reg")
	    (match_test "REGNO (op) == STATIC_CHAIN_REGNUM"))))

;; TODO: Add a comment here.

(define_predicate "post_inc_operand"
  (and (match_code "mem")
       (match_test "GET_CODE (XEXP (op, 0)) == POST_INC")))

;; TODO: Add a comment here.

(define_predicate "pre_dec_operand"
  (and (match_code "mem")
       (match_test "GET_CODE (XEXP (op, 0)) == PRE_DEC")))

;; A zero constant.
(define_predicate "const0_operand"
  (and (match_code "const_int,const_double,const_vector")
       (match_test "op == CONST0_RTX (mode)")))

;; A one constant (operand for conditional_trap).
(define_predicate "const1_operand"
  (and (match_code "const_int")
       (match_test "op == const1_rtx")))

;; A valid operand for a conditional operation.
;; ColdFire has tst patterns for HImode and QImode, but not cmp patterns.
(define_predicate "m68k_comparison_operand"
  (if_then_else (match_test "TARGET_COLDFIRE && mode != SImode")
                (and (match_code "const_int")
		     (match_test "op == const0_rtx"))
		(match_operand 0 "general_src_operand")))

;; An operand for movsi_const0 pattern.
(define_predicate "movsi_const0_operand"
  (and (match_operand 0 "nonimmediate_operand")
       (match_test "(TARGET_68010 || TARGET_COLDFIRE)
                    || !(MEM_P (op) && MEM_VOLATILE_P (op))")))
 
;; A non-symbolic call operand.
;; We need to special case 'const_int' to ignore its mode while matching.
(define_predicate "non_symbolic_call_operand"
  (and (match_operand 0 "call_operand")
       (ior (and (match_code "const_int")
 		 (match_test "!symbolic_operand (op, mode)"))
 	    (match_test "!symbolic_operand (op,mode)"))))

;; Special case of general_src_operand, which rejects a few fp
;; constants (which we prefer in registers) before reload.
;; Used only in comparisons, and we do want to allow zero.

(define_predicate "fp_src_operand"
  (match_operand 0 "general_src_operand")
{
  return (!CONSTANT_P (op)
	  || op == CONST0_RTX (mode)
	  || (TARGET_68881
	      && (!standard_68881_constant_p (op)
		  || reload_in_progress
		  || reload_completed)));
})

;; Used to detect constants that are valid for addq/subq instructions
(define_predicate "addq_subq_operand"
  (match_code "const_int")
{
  return ((INTVAL (op) <= 8 && INTVAL (op) > 0)
	  || (INTVAL (op) >= -8 && INTVAL (op) < 0));
})

;; Used to detect equality and non-equality operators
(define_predicate "equality_comparison_operator"
  (match_code "eq,ne"))

;; Used to detect when an operand is either a register
;; or a constant that is all ones in its lower bits.
;; Used by insv pattern to help detect when we're initializing
;; a bitfield to all ones.

(define_predicate "reg_or_pow2_m1_operand"
  (match_code "reg,const_int")
{
  return (REG_P (op) || pow2_m1_operand (op, VOIDmode));
})

;; Used to detect a constant that is all ones in its lower bits.
(define_predicate "pow2_m1_operand"
  (match_code "const_int")
{
  return (GET_CODE (op) == CONST_INT && exact_log2 (INTVAL (op) + 1) >= 0);
})

;; Used to detect valid targets for conditional branches
;; Used to detect (pc) or (label_ref) in some jumping patterns to cut down
(define_predicate "pc_or_label_operand"
  (match_code "pc,label_ref"))

(define_predicate "swap_peephole_relational_operator"
  (match_code "gtu,leu,gt,le"))

(define_predicate "address_reg_operand"
  (match_test ("ADDRESS_REG_P (op)")))
