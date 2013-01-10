;;  Machine Description for Renesas RL78 processors
;;  Copyright (C) 2011-2013 Free Software Foundation, Inc.
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

(define_predicate "rl78_any_operand"
  (ior (match_operand 0 "general_operand")
       (match_code "mem,const_int,const_double,reg"))
)

(define_predicate "rl78_nonfar_operand"
  (and (match_operand 0 "general_operand")
       (not (match_test "rl78_far_p (op)")))
)

(define_predicate "rl78_nonfar_nonimm_operand"
  (and (match_operand 0 "nonimmediate_operand")
       (not (match_test "rl78_far_p (op)")))
)

(define_predicate "ubyte_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 0, 255)")))

(define_predicate "rl78_24_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) == 2 || INTVAL (op) == 4")))

(define_predicate "uword_operand"
  (ior (match_code "const")
       (and (match_code "const_int")
	    (match_test "IN_RANGE (INTVAL (op), 0, 65536)"))))

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
