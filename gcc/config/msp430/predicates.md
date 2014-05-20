;;  Machine Description for TI MSP43* processors
;;  Copyright (C) 2013-2014 Free Software Foundation, Inc.
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

(define_predicate "msp_volatile_memory_operand"
  (and (match_code "mem")
       (match_test ("memory_address_addr_space_p (GET_MODE (op), XEXP (op, 0), MEM_ADDR_SPACE (op))")))
)

; TRUE for any valid general operand.  We do this because
; general_operand refuses to match volatile memory refs.

(define_predicate "msp_general_operand"
  (ior (match_operand 0 "general_operand")
       (match_operand 0 "msp_volatile_memory_operand"))
)

; Likewise for nonimmediate_operand.

(define_predicate "msp_nonimmediate_operand"
  (ior (match_operand 0 "nonimmediate_operand")
       (match_operand 0 "msp_volatile_memory_operand"))
)

(define_predicate "ubyte_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 0, 255)")))

; TRUE for comparisons we support.
(define_predicate "msp430_cmp_operator"
  (match_code "eq,ne,lt,ltu,ge,geu"))

; TRUE for comparisons we need to reverse.
(define_predicate "msp430_reversible_cmp_operator"
  (match_code "gt,gtu,le,leu"))

; TRUE for constants the constant generator can produce
(define_predicate "msp430_constgen_operator"
  (and (match_code "const_int")
       (match_test ("   INTVAL (op) == 0
		     || INTVAL (op) == 1
		     || INTVAL (op) == 2
		     || INTVAL (op) == 4
		     || INTVAL (op) == 8
		     || INTVAL (op) == -1 "))))

; TRUE for constants the constant generator can produce
(define_predicate "msp430_inv_constgen_operator"
  (and (match_code "const_int")
       (match_test ("   INTVAL (op) == ~0
		     || INTVAL (op) == ~1
		     || INTVAL (op) == ~2
		     || INTVAL (op) == ~4
		     || INTVAL (op) == ~8
		     || INTVAL (op) == ~(-1) "))))

(define_predicate "msp430_nonsubreg_operand"
  (match_code "reg,mem"))

(define_predicate "msp430_nonsubreg_or_imm_operand"
  (ior (match_operand 0 "msp430_nonsubreg_operand")
       (match_operand 0 "immediate_operand")))

; TRUE for constants which are bit positions for zero_extract
(define_predicate "msp430_bitpos"
  (and (match_code "const_int")
       (match_test ("   INTVAL (op) >= 0
		     && INTVAL (op) <= 15 "))))
