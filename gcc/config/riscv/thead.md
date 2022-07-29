;; Machine description for T-Head vendor extensions
;; Copyright (C) 2021-2022 Free Software Foundation, Inc.

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

;; XTheadBa

(define_insn "*th_addsl<mode>4"
  [(set (match_operand:X 0 "register_operand" "=r")
	(plus:X (ashift:X (match_operand:X 1 "register_operand" "r")
			  (match_operand 2 "const_int_operand" "n"))
		(match_operand:X 3 "register_operand" "r")))]
  "TARGET_XTHEADBA
   && (INTVAL (operands[2]) >= 0) && (INTVAL (operands[2]) <= 3)"
  "th.addsl\t%0,%3,%1,%2"
  [(set_attr "type" "bitmanip")
   (set_attr "mode" "<X:MODE>")])

;; XTheadBb

(define_insn "*th_srri<mode>3"
  [(set (match_operand:GPR 0 "register_operand" "=r")
	(rotatert:GPR (match_operand:GPR 1 "register_operand" "r")
		     (match_operand 2 "const_int_operand" "n")))]
  "TARGET_XTHEADBB && (TARGET_64BIT || <MODE>mode == SImode)"
  {
    bool wform = TARGET_64BIT && (<MODE>mode == SImode);
    operands[2] = GEN_INT (INTVAL (operands[2])
                  & (GET_MODE_BITSIZE (<MODE>mode) - 1));
    return wform ? "th.srriw\t%0,%1,%2" : "th.srri\t%0,%1,%2";
  }
  [(set_attr "type" "bitmanip")
   (set_attr "mode" "<GPR:MODE>")])

(define_insn "*th_ext<mode>4"
  [(set (match_operand:GPR 0 "register_operand" "=r")
	(sign_extract:GPR (match_operand:GPR 1 "register_operand" "r")
			(match_operand 2 "const_int_operand")
			(match_operand 3 "const_int_operand")))]
  "TARGET_XTHEADBB"
{
  operands[2] = GEN_INT (INTVAL (operands[2]) + INTVAL (operands[3]) - 1);
  return "th.ext\t%0,%1,%2,%3";
}
  [(set_attr "type" "bitmanip")
   (set_attr "mode" "<GPR:MODE>")])

(define_insn "*th_extu<mode>4"
  [(set (match_operand:GPR 0 "register_operand" "=r")
	(zero_extract:GPR (match_operand:GPR 1 "register_operand" "r")
			(match_operand 2 "const_int_operand")
			(match_operand 3 "const_int_operand")))]
  "TARGET_XTHEADBB"
{
  operands[2] = GEN_INT (INTVAL (operands[2]) + INTVAL (operands[3]) - 1);
  return "th.extu\t%0,%1,%2,%3";
}
  [(set_attr "type" "bitmanip")
   (set_attr "mode" "<GPR:MODE>")])

(define_insn "*th_clz<mode>2"
  [(set (match_operand:X 0 "register_operand" "=r")
	(clz:X (match_operand:X 1 "register_operand" "r")))]
  "TARGET_XTHEADBB"
  "th.ff1\t%0,%1"
  [(set_attr "type" "bitmanip")
   (set_attr "mode" "<X:MODE>")])

(define_insn "*th_rev<mode>2"
  [(set (match_operand:GPR 0 "register_operand" "=r")
	(bswap:GPR (match_operand:GPR 1 "register_operand" "r")))]
  "TARGET_XTHEADBB && (TARGET_64BIT || <MODE>mode == SImode)"
  {
    bool wform = TARGET_64BIT && (<MODE>mode == SImode);
    return wform ? "th.revw\t%0,%1" : "th.rev\t%0,%1";
  }
  [(set_attr "type" "bitmanip")
   (set_attr "mode" "<GPR:MODE>")])

;; XTheadBs

(define_insn "*th_tst<mode>3"
  [(set (match_operand:X 0 "register_operand" "=r")
	(zero_extract:X (match_operand:X 1 "register_operand" "r")
			(const_int 1)
			(match_operand 2 "const_int_operand" "n")))]
  "TARGET_XTHEADBS && UINTVAL (operands[2]) < GET_MODE_BITSIZE (<MODE>mode)"
  "th.tst\t%0,%1,%2"
  [(set_attr "type" "bitmanip")])
