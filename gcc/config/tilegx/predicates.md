;; Predicate definitions for Tilera TILE-Gx.
;; Copyright (C) 2011, 2012
;; Free Software Foundation, Inc.
;; Contributed by Walter Lee (walt@tilera.com)
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; Return true if OP is the zero constant for MODE.
(define_predicate "const_zero_operand"
  (and (match_code "const_int,const_double,const_vector")
       (match_test "op == CONST0_RTX (mode)")))

;; Returns true if OP is either the constant zero or a register.
(define_predicate "reg_or_0_operand"
  (and (ior (match_operand 0 "register_operand")
	    (match_operand 0 "const_zero_operand"))
       (match_test "GET_MODE_SIZE (mode) <= UNITS_PER_WORD")))

; Return 1 if OP is a valid Pmode pointer.
(define_predicate "pointer_operand"
  (and (match_operand 0 "address_operand")
       (ior (match_operand 0 "pmode_register_operand")
	    (match_operand 0 "const_zero_operand"))))

; Return 1 if OP is a network register identifier.
(define_predicate "netreg_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 0, 5)")))

; Return 1 if OP is an unsigned 6-bit constant.
(define_predicate "u6bit_cint_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) == (INTVAL (op) & 0x3F)")))

;; Return 1 if OP is an unsigned 16-bit constant.
(define_predicate "u16bit_cint_operand"
  (and (match_code "const_int")
       (match_test "(unsigned HOST_WIDE_INT)INTVAL (op) < (1U << 16)")))

;; Return 1 if OP is a signed 8-bit constant.
(define_predicate "s8bit_cint_operand"
  (and (match_code "const_int")
       (match_test "satisfies_constraint_I (op)")))

;; Return 1 if OP is a signed 16-bit constant.
(define_predicate "s16bit_cint_operand"
  (and (match_code "const_int")
       (match_test "satisfies_constraint_J (op)")))

;; Return 1 if OP is an unsigned 14-bit constant.
(define_predicate "u14bit_cint_operand"
  (and (match_code "const_int")
       (match_test "(unsigned HOST_WIDE_INT)INTVAL (op) < (1U << 14)")))

;; Return 1 if OP is a constant or any register.
(define_predicate "reg_or_cint_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "const_int_operand")))

;; Returns 1 if OP is a "last" unspec wrapper for a symbol, got, or
;; tls reference.
(define_predicate "const_last_symbolic_operand"
  (and (match_code "const")
       (match_test "GET_CODE (XEXP (op,0)) == UNSPEC")
       (ior (match_test "XINT (XEXP (op,0), 1) == UNSPEC_HW0_LAST")
	    (match_test "XINT (XEXP (op,0), 1) == UNSPEC_HW1_LAST")
	    (match_test "XINT (XEXP (op,0), 1) == UNSPEC_HW2_LAST")
	    (match_test "XINT (XEXP (op,0), 1) == UNSPEC_HW1_LAST_PCREL")
	    (match_test "XINT (XEXP (op,0), 1) == UNSPEC_HW0_LAST_GOT")
	    (match_test "XINT (XEXP (op,0), 1) == UNSPEC_HW1_LAST_GOT")
	    (match_test "XINT (XEXP (op,0), 1) == UNSPEC_HW1_LAST_TLS_GD")
	    (match_test "XINT (XEXP (op,0), 1) == UNSPEC_HW1_LAST_TLS_IE")
	    (match_test "XINT (XEXP (op,0), 1) == UNSPEC_HW1_LAST_TLS_LE"))))

;; Returns 1 if OP is an unspec wrapper for a symbol, got, or tls
;; reference.
(define_predicate "const_symbolic_operand"
  (and (match_code "const")
       (match_test "GET_CODE (XEXP (op,0)) == UNSPEC")
       (ior (match_test "XINT (XEXP (op,0), 1) == UNSPEC_HW0")
	    (match_test "XINT (XEXP (op,0), 1) == UNSPEC_HW1")
	    (match_test "XINT (XEXP (op,0), 1) == UNSPEC_HW2")
	    (match_test "XINT (XEXP (op,0), 1) == UNSPEC_HW3")
	    (match_test "XINT (XEXP (op,0), 1) == UNSPEC_HW0_PCREL")
	    (match_test "XINT (XEXP (op,0), 1) == UNSPEC_HW0_GOT")
	    (match_test "XINT (XEXP (op,0), 1) == UNSPEC_HW0_TLS_GD")
	    (match_test "XINT (XEXP (op,0), 1) == UNSPEC_HW0_TLS_IE")
	    (match_test "XINT (XEXP (op,0), 1) == UNSPEC_HW0_TLS_LE"))))

;; Return 1 if OP is a 8-element vector constant with identical signed
;; 8-bit elements or any register.
(define_predicate "reg_or_v8s8bit_operand"
  (ior (match_operand 0 "register_operand")
       (and (match_code "const_vector")
	    (match_test "CONST_VECTOR_NUNITS (op) == 8
                         && satisfies_constraint_I (CONST_VECTOR_ELT (op, 0))
                         && CONST_VECTOR_ELT (op, 0) == CONST_VECTOR_ELT (op, 1)
                         && CONST_VECTOR_ELT (op, 0) == CONST_VECTOR_ELT (op, 2)
                         && CONST_VECTOR_ELT (op, 0) == CONST_VECTOR_ELT (op, 3)
                         && CONST_VECTOR_ELT (op, 0) == CONST_VECTOR_ELT (op, 4)
                         && CONST_VECTOR_ELT (op, 0) == CONST_VECTOR_ELT (op, 5)
                         && CONST_VECTOR_ELT (op, 0) == CONST_VECTOR_ELT (op, 6)
                         && CONST_VECTOR_ELT (op, 0) == CONST_VECTOR_ELT (op, 7)"))))

;; Return 1 if OP is a 4-element vector constant with identical signed
;; 8-bit elements or any register.
(define_predicate "reg_or_v4s8bit_operand"
  (ior (match_operand 0 "register_operand")
       (and (match_code "const_vector")
	    (match_test "CONST_VECTOR_NUNITS (op) == 4
                         && satisfies_constraint_I (CONST_VECTOR_ELT (op, 0))
                         && CONST_VECTOR_ELT (op, 0) == CONST_VECTOR_ELT (op, 1)
                         && CONST_VECTOR_ELT (op, 0) == CONST_VECTOR_ELT (op, 2)
                         && CONST_VECTOR_ELT (op, 0) == CONST_VECTOR_ELT (op, 3)"))))

;; Return 1 if the operand is a valid second operand to an add insn.
(define_predicate "add_operand"
  (if_then_else (match_code "const_int")
    (match_test "satisfies_constraint_J (op)")
    (ior (match_operand 0 "register_operand")
	 (match_operand 0 "const_last_symbolic_operand"))))

;; Return 1 if the operand is a register or signed 8-bit immediate operand.
(define_predicate "reg_or_s8bit_operand"
  (if_then_else (match_code "const_int")
    (match_test "satisfies_constraint_I (op)")
    (match_operand 0 "register_operand")))

;; Return 1 if the operand is a register or unsigned 5-bit immediate operand.
(define_predicate "reg_or_u5bit_operand"
  (if_then_else (match_code "const_int")
    (match_test "INTVAL (op) == (INTVAL (op) & 0x1F)")
    (match_operand 0 "register_operand")))

;; Return 1 if the operand is a register or unsigned 6-bit immediate operand.
(define_predicate "reg_or_u6bit_operand"
  (if_then_else (match_code "const_int")
    (match_test "INTVAL (op) == (INTVAL (op) & 0x3F)")
    (match_operand 0 "register_operand")))

;; Return 1 for an operand suitable for ANDing with a register.
(define_predicate "and_operand"
  (if_then_else (match_code "const_int")
    (match_test "satisfies_constraint_I (op) || satisfies_constraint_M (op)")
    (match_operand 0 "register_operand")))

; Return 1 if the operand is 2, 4 or 8.
(define_predicate "cint_248_operand"
  (and (match_code "const_int")
       (match_test
        "INTVAL (op) == 2 || INTVAL (op) == 4 || INTVAL (op) == 8")))

;; Return true if OP is a TLS symbolic operand.
(define_predicate "tls_symbolic_operand"
  (and (match_code "symbol_ref")
       (match_test "SYMBOL_REF_TLS_MODEL (op) !=  TLS_MODEL_NONE")))

;; Return true if OP is a symbolic operand for the TLS Global Dynamic model.
(define_predicate "tls_gd_symbolic_operand"
  (and (match_code "symbol_ref")
       (match_test "SYMBOL_REF_TLS_MODEL (op) == TLS_MODEL_GLOBAL_DYNAMIC")))

;; Return true if OP is a symbolic operand for the TLS Local Dynamic model.
(define_predicate "tls_ld_symbolic_operand"
  (and (match_code "symbol_ref")
       (match_test "SYMBOL_REF_TLS_MODEL (op) == TLS_MODEL_LOCAL_DYNAMIC")))

;; Return true if OP is a symbolic operand that can be used for the
;; TLS Initial Exec model.
(define_predicate "tls_ie_symbolic_operand"
  (and (match_code "symbol_ref")
       (ior (match_test "SYMBOL_REF_TLS_MODEL (op) == TLS_MODEL_INITIAL_EXEC")
            (match_test "SYMBOL_REF_TLS_MODEL (op) == TLS_MODEL_LOCAL_EXEC"))))

;; Return true if OP is a symbolic operand for the TLS Local Exec model.
(define_predicate "tls_le_symbolic_operand"
  (and (match_code "symbol_ref")
       (match_test "SYMBOL_REF_TLS_MODEL (op) == TLS_MODEL_LOCAL_EXEC")))

;; Returns true if OP is any general operand except for an
;; auto-incrementing address operand.
(define_predicate "nonautoinc_operand"
  (and (match_operand 0 "general_operand")
       (not (ior (match_code "pre_dec") (match_code "pre_inc")
		 (match_code "post_dec") (match_code "post_inc")
		 (match_code "post_modify") (match_code "pre_modify")))))
 
;; Returns true if OP is a non-auto-incrementing memory operand.
(define_predicate "nonautoincmem_operand"
  (match_operand 0 "memory_operand")
{
  return nonautoinc_operand (XEXP (op, 0), GET_MODE (XEXP (op, 0)));
})

;; Returns true if OP is a non-auto-incrementing memory, general
;; operand.
(define_predicate "nonautoincmem_general_operand"
  (match_operand 0 "general_operand")
{
  if (memory_operand (op, mode))
    return nonautoinc_operand (XEXP (op, 0), GET_MODE (XEXP (op, 0)));
  else
    return true;
})
 
;; Returns true if OP is a non-auto-incrementing memory, non-immediate
;; operand.
(define_predicate "nonautoincmem_nonimmediate_operand"
  (match_operand 0 "nonimmediate_operand")
{
  if (memory_operand (op, mode))
    return nonautoinc_operand (XEXP (op, 0), GET_MODE (XEXP (op, 0)));
  else
    return true;
})
 
;; Return true if OP is a valid operand for the source of a move insn.
(define_predicate "move_operand"
  (match_operand 0 "general_operand")
{
  /* If both modes are non-void they must be the same.  */
  if (mode != VOIDmode && GET_MODE (op) != VOIDmode && mode != GET_MODE (op))
    return false;

  switch (GET_CODE (op))
    {
    case CONST_INT:
      return (satisfies_constraint_J (op)
              || satisfies_constraint_K (op)
              || (mode == DImode &&
                  (satisfies_constraint_N (op)
                   || satisfies_constraint_P (op))));

    case MEM:
      return memory_address_p (mode, XEXP (op, 0));

    case CONST:
      return const_last_symbolic_operand (op, mode);

    default:
      return register_operand (op, mode);
    }
})

;; Returns 1 if OP is a symbolic operand, i.e. a symbol_ref or a label_ref,
;; possibly with an offset.
(define_predicate "symbolic_operand"
  (ior (match_code "symbol_ref,label_ref")
       (and (match_code "const")
	    (match_test "GET_CODE (XEXP (op,0)) == PLUS
			 && (GET_CODE (XEXP (XEXP (op,0), 0)) == SYMBOL_REF
			     || GET_CODE (XEXP (XEXP (op,0), 0)) == LABEL_REF)
			 && CONST_INT_P (XEXP (XEXP (op,0), 1))"))))

;; Return 1 for an unsigned 16 bit or a const symbolc operand.
(define_predicate "u16bit_or_const_symbolic_operand"
  (ior (match_operand 0 "u16bit_cint_operand")
       (match_operand 0 "const_symbolic_operand")))

;; Return true if OP is an address suitable for a call insn.
;; Call insn on TILE can take a PC-relative constant address
;; or any regular memory address.
(define_predicate "call_address_operand"
  (ior (match_operand 0 "symbolic_operand")
       (match_test "memory_address_p (Pmode, op)")))

;; Return true if OP is an operand suitable for a call insn.
(define_predicate "call_operand"
  (and (match_code "mem")
       (match_test "call_address_operand (XEXP (op, 0), mode)")))

;; Return 1 if OP is a signed comparison operation.
;; We can use these directly in compares against zero.
(define_predicate "signed_comparison_operator"
  (match_code "eq,ne,le,lt,ge,gt"))

;; Return 1 if OP is a equal or not-equal operation.
(define_predicate "eqne_operator"
  (match_code "eq,ne"))
