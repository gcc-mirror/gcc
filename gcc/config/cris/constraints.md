;; Constraint definitions for CRIS.
;; Copyright (C) 2011 Free Software Foundation, Inc.
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

;; Register constraints.
(define_register_constraint "a" "ACR_REGS"
  "@internal")

(define_register_constraint "b" "GENNONACR_REGS"
  "@internal")

(define_register_constraint "h" "MOF_REGS"
  "@internal")

(define_register_constraint "x" "SPECIAL_REGS"
  "@internal")

(define_register_constraint "c" "CC0_REGS"
  "@internal")

;; Integer constraints.
(define_constraint "I"
  "MOVEQ, CMPQ, ANDQ, ORQ."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -32, 31)")))

(define_constraint "J"
  "ADDQ, SUBQ."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 0, 63)")))

(define_constraint "Kc"
  "ASRQ, BTSTQ, LSRQ, LSLQ."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 0, 31)")))

(define_constraint "Kp"
  "A power of two."
  (and (match_code "const_int")
       (match_test "exact_log2 (ival) >= 0")))

(define_constraint "L"
  "A 16-bit signed number."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -32768, 32767)")))

(define_constraint "M"
  "The constant 0 for CLEAR."
  (and (match_code "const_int")
       (match_test "ival == 0")))

(define_constraint "N"
  "A negative ADDQ or SUBQ."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -63, -1)")))

(define_constraint "O"
  "Quickened ints, QI and HI."
  (and (match_code "const_int")
       (ior (match_test "IN_RANGE (ival, (65535 - 31), 65535)")
	    (match_test "IN_RANGE (ival, (255 - 31), 255)"))))

(define_constraint "P"
  "A 16-bit number signed *or* unsigned."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -32768, 65535)")))

;; Floating-point constant constraints.
(define_constraint "G"
  "The floating point zero constant"
  (and (match_code "const_double")
       (match_test "GET_MODE_CLASS (mode) == MODE_FLOAT")
       (match_test "op == CONST0_RTX (mode)")))

;; Memory constraints.

;; Just an indirect register (happens to also be "all" slottable
;; memory addressing modes not covered by other constraints, i.e. '>').
(define_memory_constraint "Q"
  "@internal"
  (and (match_code "mem")
       (match_test "cris_base_p (XEXP (op, 0), reload_in_progress
					       || reload_completed)")))

;; Extra constraints.
(define_constraint "R"
  "An operand to BDAP or BIAP."
       ;; A BIAP; r.S?
  (ior (match_test "cris_biap_index_p (op, reload_in_progress
					   || reload_completed)")
       ;; A [reg] or (int) [reg], maybe with post-increment.
       (match_test "cris_bdap_index_p (op, reload_in_progress
					   || reload_completed)")
       (match_test "cris_constant_index_p (op)")))

(define_constraint "T"
  "Memory three-address operand."
  ;; All are indirect-memory:
  (and (match_code "mem")
	    ;; Double indirect: [[reg]] or [[reg+]]?
       (ior (and (match_code "mem" "0")
		 (match_test "cris_base_or_autoincr_p (XEXP (XEXP (op, 0), 0),
						       reload_in_progress
						       || reload_completed)"))
	    ;; Just an explicit indirect reference: [const]?
	    (match_test "CONSTANT_P (XEXP (op, 0))")
	    ;; Something that is indexed; [...+...]?
	    (and (match_code "plus" "0")
		      ;; A BDAP constant: [reg+(8|16|32)bit offset]?
		 (ior (and (match_test "cris_base_p (XEXP (XEXP (op, 0), 0),
						     reload_in_progress
						     || reload_completed)")
			   (match_test "cris_constant_index_p (XEXP (XEXP (op, 0), 1))"))
		      ;; A BDAP register: [reg+[reg(+)].S]?
		      (and (match_test "cris_base_p (XEXP (XEXP (op, 0), 0),
						     reload_in_progress
						     || reload_completed)")
			   (match_test "cris_bdap_index_p (XEXP (XEXP (op, 0), 1),
							   reload_in_progress
							   || reload_completed)"))
		      ;; Same, but with swapped arguments (no canonical
		      ;; ordering between e.g. REG and MEM as of LAST_UPDATED
		      ;; "Thu May 12 03:59:11 UTC 2005").
		      (and (match_test "cris_base_p (XEXP (XEXP (op, 0), 1),
						     reload_in_progress
						     || reload_completed)")
			   (match_test "cris_bdap_index_p (XEXP (XEXP (op, 0), 0),
							   reload_in_progress
							   || reload_completed)"))
		      ;; A BIAP: [reg+reg.S] (MULT comes first).
		      (and (match_test "cris_base_p (XEXP (XEXP (op, 0), 1),
						     reload_in_progress
						     || reload_completed)")
			   (match_test "cris_biap_index_p (XEXP (XEXP (op, 0), 0),
							   reload_in_progress
							   || reload_completed)")))))))

(define_constraint "S"
  "PIC-constructs for symbols."
  (and (match_test "flag_pic")
       (match_code "const")
       (match_test "cris_valid_pic_const (op, false)")))

(define_constraint "U"
  "@internal"
  (and (match_test "flag_pic")
       (match_test "CONSTANT_P (op)")
       (match_operand 0 "cris_nonmemory_operand_or_callable_symbol")))

