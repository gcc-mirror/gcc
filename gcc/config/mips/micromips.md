;; Copyright (C) 2013-2021 Free Software Foundation, Inc.
;;
;; micromips.md   Machine Description for the microMIPS instruction set
;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

(define_insn "*store_word_multiple"
  [(match_parallel 0 ""
       [(set (match_operand:SI 1 "memory_operand")
	     (match_operand:SI 2 "register_operand"))])]
  "TARGET_MICROMIPS
   && umips_save_restore_pattern_p (true, operands[0])"
  { return umips_output_save_restore (true, operands[0]); }
  [(set_attr "type" "multimem")
   (set_attr "mode" "SI")
   (set_attr "can_delay" "no")])

(define_insn "*load_word_multiple"
  [(match_parallel 0 ""
       [(set (match_operand:SI 1 "register_operand")
	     (match_operand:SI 2 "memory_operand"))])]
  "TARGET_MICROMIPS
   && umips_save_restore_pattern_p (false, operands[0])"
  { return umips_output_save_restore (false, operands[0]); }
  [(set_attr "type" "multimem")
   (set_attr "mode" "SI")
   (set_attr "can_delay" "no")])

;; For LWP.
(define_peephole2
  [(set (match_operand:SI 0 "d_operand" "")
        (match_operand:SI 1 "non_volatile_mem_operand" ""))
   (set (match_operand:SI 2 "d_operand" "")
        (match_operand:SI 3 "non_volatile_mem_operand" ""))]
  "TARGET_MICROMIPS
   && umips_load_store_pair_p (true, operands)"
  [(parallel [(set (match_dup 0) (match_dup 1))
              (set (match_dup 2) (match_dup 3))])])

;; The behavior of the LWP insn is undefined if placed in a delay slot.
(define_insn "*lwp"
  [(parallel [(set (match_operand:SI 0 "d_operand")
		   (match_operand:SI 1 "non_volatile_mem_operand"))
	      (set (match_operand:SI 2 "d_operand")
		   (match_operand:SI 3 "non_volatile_mem_operand"))])]

  "TARGET_MICROMIPS
   && umips_load_store_pair_p (true, operands)"
{
  umips_output_load_store_pair (true, operands);
  return "";
}
  [(set_attr "type" "load")
   (set_attr "mode" "SI")
   (set_attr "can_delay" "no")])

;; For SWP.
(define_peephole2
  [(set (match_operand:SI 0 "non_volatile_mem_operand" "")
        (match_operand:SI 1 "d_operand" ""))
   (set (match_operand:SI 2 "non_volatile_mem_operand" "")
        (match_operand:SI 3 "d_operand" ""))]
  "TARGET_MICROMIPS
   && umips_load_store_pair_p (false, operands)"
  [(parallel [(set (match_dup 0) (match_dup 1))
              (set (match_dup 2) (match_dup 3))])])

;; The behavior of the SWP insn is undefined if placed in a delay slot.
(define_insn "*swp"
  [(set (match_operand:SI 0 "non_volatile_mem_operand")
	(match_operand:SI 1 "d_operand"))
   (set (match_operand:SI 2 "non_volatile_mem_operand")
	(match_operand:SI 3 "d_operand"))]
  "TARGET_MICROMIPS
   && umips_load_store_pair_p (false, operands)"
{
  umips_output_load_store_pair (false, operands);
  return "";
}
  [(set_attr "type" "store")
   (set_attr "mode" "SI")
   (set_attr "can_delay" "no")])

;; For JRADDIUSP.
(define_insn "jraddiusp"
  [(return)
   (use (reg:SI 31))
   (set (reg:SI 29)
	(plus:SI (reg:SI 29)
		 (match_operand 0 "uw5_operand")))]
  "TARGET_MICROMIPS"
  "jraddiusp\t%0"
  [(set_attr "type"	"trap")
   (set_attr "can_delay" "no")
   (set_attr "mode"	"SI")])

;; For MOVEP.
(define_peephole2
  [(set (match_operand:MOVEP1 0 "register_operand" "")
        (match_operand:MOVEP1 1 "movep_src_operand" ""))
   (set (match_operand:MOVEP2 2 "register_operand" "")
        (match_operand:MOVEP2 3 "movep_src_operand" ""))]
  "TARGET_MICROMIPS
   && umips_movep_target_p (operands[0], operands[2])"
  [(parallel [(set (match_dup 0) (match_dup 1))
              (set (match_dup 2) (match_dup 3))])])

;; The behavior of the MOVEP insn is undefined if placed in a delay slot.
(define_insn "*movep<MOVEP1:mode><MOVEP2:mode>"
  [(set (match_operand:MOVEP1 0 "register_operand")
	(match_operand:MOVEP1 1 "movep_src_operand"))
   (set (match_operand:MOVEP2 2 "register_operand")
	(match_operand:MOVEP2 3 "movep_src_operand"))]
  "TARGET_MICROMIPS
   && umips_movep_target_p (operands[0], operands[2])"
{
  if (REGNO (operands[0]) < REGNO (operands[2]))
    return "movep\t%0,%2,%z1,%z3";
  else
    return "movep\t%2,%0,%z3,%z1";
}
  [(set_attr "type" "move")
   (set_attr "mode" "<MOVEP1:MODE>")
   (set_attr "can_delay" "no")])
