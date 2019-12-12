;; GCC machine description for m68k synchronization instructions.
;; Copyright (C) 2011-2019 Free Software Foundation, Inc.
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


(define_expand "atomic_compare_and_swap<mode>"
  [(match_operand:QI 0 "register_operand" "")		;; bool success output
   (match_operand:I 1 "register_operand" "")		;; oldval output
   (match_operand:I 2 "memory_operand" "")		;; memory
   (match_operand:I 3 "register_operand" "")		;; expected input
   (match_operand:I 4 "register_operand" "")		;; newval input
   (match_operand:SI 5 "const_int_operand" "")		;; is_weak
   (match_operand:SI 6 "const_int_operand" "")		;; success model
   (match_operand:SI 7 "const_int_operand" "")]		;; failure model
  "TARGET_CAS"
{
  emit_insn (gen_atomic_compare_and_swap<mode>_1
	     (operands[0], operands[1], operands[2],
	      operands[3], operands[4]));
  emit_insn (gen_negqi2 (operands[0], operands[0]));
  DONE;
})

(define_insn "atomic_compare_and_swap<mode>_1"
  [(set (match_operand:I 1 "register_operand" "=d")
	(unspec_volatile:I
	  [(match_operand:I 2 "memory_operand" "+m")
	   (match_operand:I 3 "register_operand" "1")
	   (match_operand:I 4 "register_operand" "d")]
	  UNSPECV_CAS_1))
   (set (match_dup 2)
	(unspec_volatile:I
	  [(match_dup 2) (match_dup 3) (match_dup 4)]
	  UNSPECV_CAS_2))
   (set (match_operand:QI 0 "register_operand" "=d")
	(unspec_volatile:QI
	  [(match_dup 2) (match_dup 3) (match_dup 4)]
	  UNSPECV_CAS_2))]
  "TARGET_CAS"
  ;; Elide the seq if operands[0] is dead.
  "cas<sz> %1,%4,%2\;seq %0")

(define_expand "atomic_test_and_set"
  [(match_operand:QI 0 "register_operand" "")		;; bool success output
   (match_operand:QI 1 "memory_operand" "")		;; memory
   (match_operand:SI 2 "const_int_operand" "")]		;; model
  "ISA_HAS_TAS"
{
  rtx t = gen_reg_rtx (QImode);
  emit_insn (gen_atomic_test_and_set_1 (t, operands[1]));
  t = expand_simple_unop (QImode, NEG, t, operands[0], 0);
  if (t != operands[0])
    emit_move_insn (operands[0], t);
  DONE;
})

(define_insn "atomic_test_and_set_1"
  [(set (match_operand:QI 0 "register_operand" "=d")
	(unspec_volatile:QI
	  [(match_operand:QI 1 "memory_operand" "+m")]
	  UNSPECV_TAS_1))
   (set (match_dup 1)
	(unspec_volatile:QI [(match_dup 1)] UNSPECV_TAS_2))]
  "ISA_HAS_TAS"
  "tas %1\;sne %0")
