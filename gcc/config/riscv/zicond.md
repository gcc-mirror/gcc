;; Machine description for the RISC-V Zicond extension
;; Copyright (C) 2022-23 Free Software Foundation, Inc.

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

(define_code_iterator eq_or_ne [eq ne])
(define_code_attr eqz [(eq "nez") (ne "eqz")])
(define_code_attr nez [(eq "eqz") (ne "nez")])

;; Zicond
(define_insn "*czero.<eqz>.<GPR:mode><X:mode>"
  [(set (match_operand:GPR 0 "register_operand"                      "=r")
        (if_then_else:GPR (eq_or_ne (match_operand:X 1 "register_operand" "r")
                                    (const_int 0))
                          (match_operand:GPR 2 "register_operand"    "r")
                          (const_int 0)))]
  "TARGET_ZICOND"
  "czero.<eqz>\t%0,%2,%1"
)

(define_insn "*czero.<nez>.<GPR:mode><X:mode>"
  [(set (match_operand:GPR 0 "register_operand"                     "=r")
        (if_then_else:GPR (eq_or_ne (match_operand:X 1 "register_operand" "r")
                                    (const_int 0))
                          (const_int 0)
                          (match_operand:GPR 2 "register_operand"   "r")))]
  "TARGET_ZICOND"
  "czero.<nez>\t%0,%2,%1"
)

;; Special optimization under eq/ne in primitive semantics
(define_insn "*czero.eqz.<GPR:mode><X:mode>.opt1"
  [(set (match_operand:GPR 0 "register_operand"                   "=r")
        (if_then_else:GPR (eq (match_operand:X 1 "register_operand" "r")
                              (const_int 0))
                          (match_operand:GPR 2 "register_operand" "1")
                          (match_operand:GPR 3 "register_operand" "r")))]
  "TARGET_ZICOND && rtx_equal_p (operands[1], operands[2])"
  "czero.eqz\t%0,%3,%1"
)

(define_insn "*czero.nez.<GPR:mode><X:mode>.opt2"
  [(set (match_operand:GPR 0 "register_operand"                   "=r")
        (if_then_else:GPR (ne (match_operand:X 1 "register_operand" "r")
                              (const_int 0))
                          (match_operand:GPR 2 "register_operand" "r")
                          (match_operand:GPR 3 "register_operand" "1")))]
  "TARGET_ZICOND && rtx_equal_p (operands[1], operands[3])"
  "czero.nez\t%0,%2,%1"
)
