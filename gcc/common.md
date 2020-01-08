;; Common GCC machine description file, shared by all targets.
;; Copyright (C) 2014-2020 Free Software Foundation, Inc.
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
;; <http://www.gnu.org/licenses/>.  */

(define_register_constraint "r" "GENERAL_REGS"
  "Matches any general register.")

(define_memory_constraint "TARGET_MEM_CONSTRAINT"
  "Matches any valid memory."
  (and (match_code "mem")
       (match_test "memory_address_addr_space_p (GET_MODE (op), XEXP (op, 0),
						 MEM_ADDR_SPACE (op))")))

(define_memory_constraint "o"
  "Matches an offsettable memory reference."
  (and (match_code "mem")
       (match_test "offsettable_nonstrict_memref_p (op)")))

;; "V" matches TARGET_MEM_CONSTRAINTs that are rejected by "o".
;; This means that it is not a memory constraint in the usual sense,
;; since reloading the address into a base register would make the
;; address offsettable.
(define_constraint "V"
  "Matches a non-offsettable memory reference."
  (and (match_code "mem")
       (match_test "memory_address_addr_space_p (GET_MODE (op), XEXP (op, 0),
						 MEM_ADDR_SPACE (op))")
       (not (match_test "offsettable_nonstrict_memref_p (op)"))))

;; Like "V", this is not a memory constraint, since reloading the address
;; into a base register would cause it not to match.
(define_constraint "<"
  "Matches a pre-dec or post-dec operand."
  (and (match_code "mem")
       (ior (match_test "GET_CODE (XEXP (op, 0)) == PRE_DEC")
       	    (match_test "GET_CODE (XEXP (op, 0)) == POST_DEC"))))

;; See the comment for "<".
(define_constraint ">"
  "Matches a pre-inc or post-inc operand."
  (and (match_code "mem")
       (ior (match_test "GET_CODE (XEXP (op, 0)) == PRE_INC")
       	    (match_test "GET_CODE (XEXP (op, 0)) == POST_INC"))))

(define_address_constraint "p"
  "Matches a general address."
  (match_test "address_operand (op, VOIDmode)"))

(define_constraint "i"
  "Matches a general integer constant."
  (and (match_test "CONSTANT_P (op)")
       (match_test "!flag_pic || LEGITIMATE_PIC_OPERAND_P (op)")))

(define_constraint "s"
  "Matches a symbolic integer constant."
  (and (match_test "CONSTANT_P (op)")
       (match_test "!CONST_SCALAR_INT_P (op)")
       (match_test "!flag_pic || LEGITIMATE_PIC_OPERAND_P (op)")))

(define_constraint "n"
  "Matches a non-symbolic integer constant."
  (and (match_test "CONST_SCALAR_INT_P (op)")
       (match_test "!flag_pic || LEGITIMATE_PIC_OPERAND_P (op)")))

(define_constraint "E"
  "Matches a floating-point constant."
  (ior (match_test "CONST_DOUBLE_AS_FLOAT_P (op)")
       (match_test "GET_CODE (op) == CONST_VECTOR
		    && GET_MODE_CLASS (GET_MODE (op)) == MODE_VECTOR_FLOAT")))

;; There is no longer a distinction between "E" and "F".
(define_constraint "F"
  "Matches a floating-point constant."
  (ior (match_test "CONST_DOUBLE_AS_FLOAT_P (op)")
       (match_test "GET_CODE (op) == CONST_VECTOR
		    && GET_MODE_CLASS (GET_MODE (op)) == MODE_VECTOR_FLOAT")))

(define_constraint "X"
  "Matches anything."
  (match_test "true"))
