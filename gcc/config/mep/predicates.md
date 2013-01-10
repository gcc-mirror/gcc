;; Toshiba Media Processor Machine predicates
;; Copyright (C) 2009-2013 Free Software Foundation, Inc.
;; Contributed by Red Hat Inc.

;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.  */

;; (define_predicate "cgen_h_uint_7a1_immediate"
;;    (match_code "const_int"))
;; (define_predicate "cgen_h_uint_6a2_immediate"
;;    (match_code "const_int"))
;; (define_predicate "cgen_h_uint_22a4_immediate"
;;    (match_code "const_int"))
;; (define_predicate "cgen_h_sint_2a1_immediate"
;;    (match_code "const_int"))
;; (define_predicate "cgen_h_uint_24a1_immediate"
;;    (match_code "const_int"))
;; (define_predicate "cgen_h_sint_6a1_immediate"
;;    (match_code "const_int"))
;; (define_predicate "cgen_h_uint_5a4_immediate"
;;    (match_code "const_int"))
;; (define_predicate "cgen_h_uint_2a1_immediate"
;;    (match_code "const_int"))
;; (define_predicate "cgen_h_uint_16a1_immediate"
;;    (match_code "const_int"))
;; (define_predicate "cgen_h_uint_3a1_immediate"
;;    (match_code "const_int"))
;; (define_predicate "cgen_h_uint_5a1_immediate"
;;    (match_code "const_int"))
;; (define_predicate "cgen_h_sint_16a1_immediate"
;;    (match_code "const_int"))
;; (define_predicate "cgen_h_sint_5a8_immediate"
;;    (match_code "const_int"))
;; (define_predicate "cgen_h_uint_4a1_immediate"
;;    (match_code "const_int"))

(define_predicate "cgen_h_sint_7a2_immediate"
   (match_code "const_int")
   { int i = INTVAL (op);
     return ((i & 1) == 0 && i >= -128 && i < 128);
   })

(define_predicate "cgen_h_sint_6a4_immediate"
   (match_code "const_int")
   { int i = INTVAL (op);
     return ((i & 3) == 0 && i >= -256 && i < 256);
   })

;; This is used below, to simplify things.
(define_predicate "mep_subreg_operand"
  (ior
   (and (and (and (match_code "subreg")
		  (match_code "reg" "0"))
	     (match_test "REGNO (SUBREG_REG (op)) >= FIRST_PSEUDO_REGISTER"))
	(match_test "!(reload_completed || reload_in_progress)"))
   (and (match_code "reg")
	(match_test "REGNO (op) >= FIRST_PSEUDO_REGISTER"))))

(define_predicate "symbolic_operand"
  (match_code "const,symbol_ref,label_ref"))

(define_predicate "mep_farsym_operand"
  (and (match_code "const,symbol_ref")
       (match_test "mep_section_tag (op) == 'f'")))

(define_predicate "mep_nearsym_operand"
  (and (match_code "const,symbol_ref,label_ref")
       (match_test "mep_section_tag (op) != 'f'")))

(define_predicate "mep_movdest_operand"
  (and (match_test "mep_section_tag (op) != 'f'")
       (match_operand 0 "nonimmediate_operand")))

(define_predicate "mep_r0_15_operand"
  (ior (match_operand 0 "mep_subreg_operand")
       (and (match_code "reg")
	    (match_test "GR_REGNO_P (REGNO (op))"))))

(define_predicate "mep_r0_operand"
  (and (match_code "reg")
       (ior (match_test "REGNO (op) == 0")
	    (match_test "!(reload_completed || reload_in_progress)
		         && REGNO (op) >= FIRST_PSEUDO_REGISTER"))))

(define_predicate "mep_hi_operand"
  (ior (match_operand 0 "mep_subreg_operand")
       (and (match_code "reg")
	    (match_test "REGNO (op) == HI_REGNO"))))

(define_predicate "mep_lo_operand"
  (ior (match_operand 0 "mep_subreg_operand")
       (and (match_code "reg")
	    (match_test "REGNO (op) == LO_REGNO"))))

(define_predicate "mep_tp_operand"
  (ior (match_operand 0 "mep_subreg_operand")
       (and (match_code "reg")
	    (match_test "REGNO (op) == TP_REGNO"))))

(define_predicate "mep_gp_operand"
  (ior (match_operand 0 "mep_subreg_operand")
       (and (match_code "reg")
	    (match_test "REGNO (op) == GP_REGNO"))))

(define_predicate "mep_sp_operand"
  (match_test "op == stack_pointer_rtx"))

(define_predicate "mep_tprel_operand"
  (ior (match_operand 0 "mep_subreg_operand")
       (and (match_code "reg")
	    (match_test "REGNO (op) < 8"))))

(define_predicate "mep_call_address_operand"
  (and (match_test "mep_section_tag (op) != 'f'")
       (and (ior (not (match_code "symbol_ref"))
		 (match_test "mep_section_tag (DECL_RTL (cfun->decl)) != 'f'
			      && !mep_lookup_pragma_call (XSTR (op, 0))"))
	    (match_code "symbol_ref,reg"))))

(define_predicate "mep_Y_operand"
  (and (match_code "mem")
       (match_code "reg" "0")))

(define_predicate "mep_imm4_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) >= 0 && INTVAL (op) <= 15")))

(define_predicate "mep_reg_or_imm4_operand"
  (ior (match_code "reg")
       (and (match_code "const_int")
	    (match_test "INTVAL (op) >= 0 && INTVAL (op) <= 15"))))

(define_predicate "mep_imm7a4_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) >= 0 && INTVAL (op) < 128 && INTVAL (op) % 4 == 0")))

(define_predicate "mep_slad_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) == 2 || INTVAL (op) == 4")))

(define_predicate "mep_add_operand"
  (ior (and (match_code "const")
	    (and (match_operand 0 "symbolic_operand")
		 (and (match_test "mep_section_tag(op) == 'b' || mep_section_tag(op) == 't'")
		      (ior (match_code "unspec" "0")
			   (and (match_code "plus" "0")
				(match_code "unspec" "00"))))))
       (match_code "const_int,reg")))

;; Return true if OP is an integer in the range 0..7 inclusive.
;; On the MeP-h1, shifts by such constants execute in a single stage
;; and shifts by larger values execute in two.
(define_predicate "mep_single_shift_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) >= 0 && INTVAL (op) <= 7")))

;; Return true if OP is an operation that can be performed using bsetm,
;; bclrm or bnotm.  The possibilities are:

;; bsetm: (ior X Y), Y has one bit set
;; bclrm: (and X Y), Y has one bit clear
;; bnotm: (xor X Y), Y has one bit set.
(define_predicate "mep_bit_operator"
  (and (match_code "and,ior,xor")
       (match_test "mep_bit_position_p (XEXP (op, 1), GET_CODE (op) != AND)")))

(define_predicate "mep_reload_operand"
  (ior (and (match_code "reg")
	    (match_test "!ANY_CONTROL_REGNO_P (REGNO (op))"))
       (and (match_code "mem,symbol_ref")
	    (match_test "mep_section_tag (op) != 'f'"))))
