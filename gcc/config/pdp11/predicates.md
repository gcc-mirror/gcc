;;- Predicate definitions for the pdp11 for GNU C compiler
;; Copyright (C) 1994-2016 Free Software Foundation, Inc.
;; Contributed by Michael K. Gschwind (mike@vlsivie.tuwien.ac.at).

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

;; Match CONST_DOUBLE zero for tstd/tstf.
(define_predicate "register_or_const0_operand"
  (ior (match_operand 0 "register_operand")
       (match_test "op == CONST0_RTX (GET_MODE (op))")))

;; Accept integer arguments in the range -4..-2 and 2..4, which are the
;; shift counts for which we unroll a shift.  This matches the rule for
;; the "O" constraint.
(define_predicate "expand_shift_operand"
  (match_code "const_int")
{
  int sh;

  sh = INTVAL (op);
  return (abs (sh) > 1 && abs (sh) <= 4);
})

;; Accept anything general_operand accepts, except that registers must
;; be FPU registers.
(define_predicate "float_operand"
  (if_then_else (match_code "reg")
		(ior 
		 (match_test "REGNO_REG_CLASS (REGNO (op)) == LOAD_FPU_REGS")
		 (match_test "REGNO_REG_CLASS (REGNO (op)) == NO_LOAD_FPU_REGS"))
		(match_operand 0 "general_operand")))

;; Accept anything nonimmediate_operand accepts, except that registers must
;; be FPU registers.
(define_predicate "float_nonimm_operand"
  (if_then_else (match_code "reg")
		(ior 
		 (match_test "REGNO_REG_CLASS (REGNO (op)) == LOAD_FPU_REGS")
		 (match_test "REGNO_REG_CLASS (REGNO (op)) == NO_LOAD_FPU_REGS"))
		(match_operand 0 "nonimmediate_operand")))
