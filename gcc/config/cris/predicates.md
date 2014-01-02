;; Operand and operator predicates for the GCC CRIS port.
;; Copyright (C) 2005-2014 Free Software Foundation, Inc.

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


;; Operator predicates.

(define_predicate "cris_orthogonal_operator"
  (match_code "plus, minus, ior, and, umin"))

(define_predicate "cris_commutative_orth_op"
  (match_code "plus, ior, and, umin"))

;; By the name, you might think we should include MULT.  We don't because
;; it doesn't accept the same addressing modes as the others (only
;; registers) and there's also the problem of handling TARGET_MUL_BUG.

(define_predicate "cris_operand_extend_operator"
  (match_code "plus, minus, umin"))

(define_predicate "cris_additive_operand_extend_operator"
  (match_code "plus, minus"))

(define_predicate "cris_extend_operator"
  (match_code "zero_extend, sign_extend"))

(define_predicate "cris_plus_or_bound_operator"
  (match_code "plus, umin"))

;; Used as an operator to get a handle on a already-known-valid MEM rtx:es
;; (no need to validate the address), where some address expression parts
;; have their own match_operand.

(define_predicate "cris_mem_op"
  (match_code "mem"))

(define_predicate "cris_load_multiple_op"
  (and (match_code "parallel")
       (match_test "cris_movem_load_rest_p (op, 0)")))

(define_predicate "cris_store_multiple_op"
  (and (match_code "parallel")
       (match_test "cris_store_multiple_op_p (op)")))


;; Operand helper predicates.

(define_predicate "cris_bdap_const_operand"
  (and (match_code "label_ref, symbol_ref, const_int, const_double, const")
       (ior (not (match_test "flag_pic"))
	    (match_test "cris_valid_pic_const (op, true)"))))

(define_predicate "cris_simple_address_operand"
  (ior (match_operand:SI 0 "register_operand")
       (and (match_code "post_inc")
	    (match_test "register_operand (XEXP (op, 0), Pmode)"))))

(define_predicate "cris_simple_operand"
  (ior (match_operand 0 "register_operand")
       (and (match_code "mem")
	    (match_test "cris_simple_address_operand (XEXP (op, 0),
						      Pmode)"))))

(define_predicate "cris_nonsp_register_operand"
  (and (match_operand 0 "register_operand")
       (match_test "op != stack_pointer_rtx")))

;; The caller needs to use :SI.
(define_predicate "cris_bdap_sign_extend_operand"
; Disabled until <URL:http://gcc.gnu.org/ml/gcc-patches/2005-10/msg01376.html>
; or <URL:http://gcc.gnu.org/ml/gcc-patches/2005-10/msg00940.html> is committed.
  (match_test "0"))
;  (and (match_code "sign_extend")
;       (and (match_test "MEM_P (XEXP (op, 0))")
;	    (match_test "cris_simple_address_operand (XEXP (XEXP (op, 0), 0),
;						      Pmode)"))))

;; FIXME: Should not have to test for 1.
(define_predicate "cris_scale_int_operand"
  (and (match_code "const_int")
       (ior (ior (match_test "op == GEN_INT (4)")
		 (match_test "op == const2_rtx"))
	    (match_test "op == const1_rtx"))))

;; FIXME: Should be able to assume (reg int).
(define_predicate "cris_biap_mult_operand"
  (and (match_code "mult")
       (ior (and (match_test "register_operand (XEXP (op, 0), Pmode)")
		 (match_test "cris_scale_int_operand (XEXP (op, 1), Pmode)"))
	    (and (match_test "cris_scale_int_operand (XEXP (op, 0), Pmode)")
		 (match_test "register_operand (XEXP (op, 1), Pmode)")))))


;; Operand predicates.

;; This checks a part of an address, the one that is not a plain register
;; for an addressing mode using BDAP.
;; Allowed operands are either:
;; a) a register
;; b) a CONST operand (but not a symbol when generating PIC)
;; c) a [r] or [r+] in SImode, or sign-extend from HI or QI.

(define_predicate "cris_bdap_operand"
  (ior (match_operand 0 "cris_bdap_const_operand")
       (ior (match_operand:SI 0 "cris_simple_operand")
	    (match_operand:SI 0 "cris_bdap_sign_extend_operand"))))

;; This is similar to cris_bdap_operand:
;; It checks a part of an address, the one that is not a plain register
;; for an addressing mode using BDAP or BIAP.
;; Allowed operands are either:
;; a) a register
;; b) a CONST operand (but not a symbol when generating PIC)
;; c) a mult of (1, 2 or 4) and a register
;; d) a [r] or [r+] in SImode, or sign-extend from HI or QI.  */

(define_predicate "cris_bdap_biap_operand"
  (ior (match_operand 0 "cris_bdap_operand")
       (match_operand 0 "cris_biap_mult_operand")))

;; Since with -fPIC, not all symbols are valid PIC symbols or indeed
;; general_operands, we have to have a predicate that matches it for the
;; "movsi" expander.
;; FIXME: Can s/special_// when PR 20413 is fixed.

(define_special_predicate "cris_general_operand_or_symbol"
  (ior (match_operand 0 "general_operand")
       (and (match_code "const, symbol_ref, label_ref")
       	    ; The following test is actually just an assertion.
	    (match_test "cris_pic_symbol_type_of (op) != cris_no_symbol"))))

;; A predicate for the anon movsi expansion, one that fits a PCREL
;; operand as well as general_operand.

(define_special_predicate "cris_general_operand_or_pic_source"
  (ior (match_operand 0 "general_operand")
       (and (match_test "flag_pic")
	    (match_test "cris_valid_pic_const (op, false)"))))

;; Since a PLT symbol is not a general_operand, we have to have a
;; predicate that matches it when we need it.  We use this in the expanded
;; "call" and "call_value" anonymous patterns.

(define_predicate "cris_nonmemory_operand_or_callable_symbol"
  (ior (match_operand 0 "nonmemory_operand")
       (and (match_code "const")
	    (and
	     (match_test "GET_CODE (XEXP (op, 0)) == UNSPEC")
	     (ior
	      (match_test "XINT (XEXP (op, 0), 1) == CRIS_UNSPEC_PLT_PCREL")
	      (match_test "XINT (XEXP (op, 0), 1) == CRIS_UNSPEC_PCREL"))))))

;; This matches a (MEM (general_operand)) or
;; (MEM (cris_general_operand_or_symbol)).  The second one isn't a valid
;; memory_operand, so we need this predicate to recognize call
;; destinations before we change them to a PLT operand (by wrapping in
;; UNSPEC CRIS_UNSPEC_PLT).

(define_predicate "cris_mem_call_operand"
  (and (match_code "mem")
       (ior (match_operand 0 "memory_operand")
	    (match_test "cris_general_operand_or_symbol (XEXP (op, 0),
							 Pmode)"))))
