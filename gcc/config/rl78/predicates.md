;;  Machine Description for Renesas RL78 processors
;;  Copyright (C) 2011-2020 Free Software Foundation, Inc.
;;  Contributed by Red Hat.

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.


(define_predicate "rl78_volatile_memory_operand"
  (and (match_code "mem")
       (match_test ("memory_address_addr_space_p (GET_MODE (op), XEXP (op, 0), MEM_ADDR_SPACE (op))")))
)

; TRUE for any valid general operand.  We do this because
; general_operand refuses to match volatile memory refs.

(define_predicate "rl78_general_operand"
  (ior (match_operand 0 "general_operand")
       (match_operand 0 "rl78_volatile_memory_operand"))
)

; Likewise for nonimmediate_operand.

(define_predicate "rl78_nonimmediate_operand"
  (ior (match_operand 0 "nonimmediate_operand")
       (match_operand 0 "rl78_volatile_memory_operand"))
)

(define_predicate "rl78_nonfar_operand"
  (and (match_operand 0 "rl78_general_operand")
       (not (match_test "rl78_far_p (op)")))
)

(define_predicate "rl78_nonfar_nonimm_operand"
  (and (match_operand 0 "rl78_nonimmediate_operand")
       (not (match_test "rl78_far_p (op)")))
)

(define_predicate "rl78_near_mem_operand"
  (and (match_code "mem")
       (match_test "!rl78_far_p (op) && rl78_as_legitimate_address (VOIDmode, XEXP (op, 0), true, ADDR_SPACE_GENERIC)"))
)

(define_predicate "ubyte_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 0, 255)")))

(define_predicate "rl78_incdec_memory_operand"
  (and (match_code "mem")
       (match_test "rl78_far_p (op)
|| satisfies_constraint_Wsa (op)
|| satisfies_constraint_Whl (op)
|| satisfies_constraint_Wh1 (op)
|| satisfies_constraint_Wab (op)")
  )
)

(define_predicate "rl78_1_2_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 1, 2)
		   || IN_RANGE (INTVAL (op), -2, -1)")))

(define_predicate "rl78_24_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) == 2 || INTVAL (op) == 4")))

(define_predicate "uword_operand"
  (ior (ior (ior (match_code "const")
		 (and (match_code "const_int")
		      (match_test "IN_RANGE (INTVAL (op), 0, 65536)")))
	    (and (match_code "subreg")
		 (ior (match_code "symbol_ref" "0")
		      (match_code "const" "0"))))
       (match_code "symbol_ref")
       ))

(define_predicate "rl78_cmp_operator_signed"
  (match_code "gt,ge,lt,le"))
(define_predicate "rl78_cmp_operator_real"
  (match_code "eq,ne,gtu,ltu,geu,leu"))
(define_predicate "rl78_cmp_operator"
  (match_code "eq,ne,gtu,ltu,geu,leu,gt,lt,ge,le"))

(define_predicate "rl78_ax_operand"
  (and (match_code "reg")
       (match_test "REGNO (op) == AX_REG || REGNO (op) >= FIRST_PSEUDO_REGISTER")))

(define_predicate "rl78_addw_operand"
  (and (match_code "reg")
       (match_test "REGNO (op) == AX_REG || REGNO (op) == SP_REG || REGNO (op) >= FIRST_PSEUDO_REGISTER")))

(define_predicate "rl78_stack_based_mem"
  (and (match_code "mem")
       (ior (and (match_code "reg" "0")
		 (match_test "REGNO (XEXP (op, 0)) == SP_REG"))
	    (and (match_code "plus" "0")
		 (and (match_code "reg" "00")
		      (match_test "REGNO (XEXP (XEXP (op, 0), 0)) == SP_REG")
		      (and (match_code "const_int" "01")
			   (match_test "IN_RANGE (INTVAL (XEXP (XEXP (op, 0), 1)), 0, 256 - GET_MODE_SIZE (GET_MODE (op)))"))
			   )))))
