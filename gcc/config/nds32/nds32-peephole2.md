;; define_peephole2 optimization patterns of Andes NDS32 cpu for GNU compiler
;; Copyright (C) 2012-2021 Free Software Foundation, Inc.
;; Contributed by Andes Technology Corporation.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.


;; Use define_peephole2 to handle possible target-specific optimization.

;; ------------------------------------------------------------------------
;; Try to utilize 16-bit instruction by swap operand if possible.
;; ------------------------------------------------------------------------

;; Try to make add as add45.
(define_peephole2
  [(set (match_operand:QIHISI 0 "register_operand"              "")
	(plus:QIHISI (match_operand:QIHISI 1 "register_operand" "")
		     (match_operand:QIHISI 2 "register_operand" "")))]
  "reload_completed
   && TARGET_16_BIT
   && REGNO (operands[0]) == REGNO (operands[2])
   && REGNO (operands[0]) != REGNO (operands[1])
   && TEST_HARD_REG_BIT (reg_class_contents[MIDDLE_REGS], REGNO (operands[0]))"
  [(set (match_dup 0) (plus:QIHISI (match_dup 2) (match_dup 1)))])

;; Try to make xor/ior/and/mult as xor33/ior33/and33/mult33.
(define_peephole2
  [(set (match_operand:SI 0 "register_operand"    "")
	(match_operator:SI 1 "nds32_have_33_inst_operator"
	  [(match_operand:SI 2 "register_operand" "")
	   (match_operand:SI 3 "register_operand" "")]))]
  "reload_completed
   && TARGET_16_BIT
   && REGNO (operands[0]) == REGNO (operands[3])
   && REGNO (operands[0]) != REGNO (operands[2])
   && TEST_HARD_REG_BIT (reg_class_contents[LOW_REGS], REGNO (operands[0]))
   && TEST_HARD_REG_BIT (reg_class_contents[LOW_REGS], REGNO (operands[2]))"
  [(set (match_dup 0) (match_op_dup 1 [(match_dup 3) (match_dup 2)]))])

(define_peephole
  [(set (match_operand:SI 0 "register_operand" "")
	(match_operand:SI 1 "register_operand" ""))
   (set (match_operand:SI 2 "register_operand" "")
	(match_operand:SI 3 "register_operand" ""))]
  "TARGET_16_BIT
   && !TARGET_ISA_V2
   && NDS32_IS_GPR_REGNUM (REGNO (operands[0]))
   && NDS32_IS_GPR_REGNUM (REGNO (operands[1]))
   && ((REGNO (operands[0]) & 0x1) == 0)
   && ((REGNO (operands[1]) & 0x1) == 0)
   && (REGNO (operands[0]) + 1) == REGNO (operands[2])
   && (REGNO (operands[1]) + 1) == REGNO (operands[3])"
  "movd44\t%0, %1"
  [(set_attr "type"   "alu")
   (set_attr "length" "2")])

;; Merge two fcpyss to fcpysd.
(define_peephole2
  [(set (match_operand:SF 0 "float_even_register_operand" "")
	(match_operand:SF 1 "float_even_register_operand" ""))
   (set (match_operand:SF 2 "float_odd_register_operand"  "")
	(match_operand:SF 3 "float_odd_register_operand"  ""))]
  "(TARGET_FPU_SINGLE || TARGET_FPU_DOUBLE)
   && REGNO (operands[0]) == REGNO (operands[2]) - 1
   && REGNO (operands[1]) == REGNO (operands[3]) - 1"
  [(set (match_dup 4) (match_dup 5))]
  {
    operands[4] = gen_rtx_REG (DFmode, REGNO (operands[0]));
    operands[5] = gen_rtx_REG (DFmode, REGNO (operands[1]));
  })

(define_peephole2
  [(set (match_operand:SF 0 "float_odd_register_operand"  "")
	(match_operand:SF 1 "float_odd_register_operand"  ""))
   (set (match_operand:SF 2 "float_even_register_operand" "")
	(match_operand:SF 3 "float_even_register_operand" ""))]
  "(TARGET_FPU_SINGLE || TARGET_FPU_DOUBLE)
   && REGNO (operands[2]) == REGNO (operands[0]) - 1
   && REGNO (operands[3]) == REGNO (operands[1]) - 1"
  [(set (match_dup 4) (match_dup 5))]
  {
    operands[4] = gen_rtx_REG (DFmode, REGNO (operands[2]));
    operands[5] = gen_rtx_REG (DFmode, REGNO (operands[3]));
  })

;; ------------------------------------------------------------------------
;; GCC will prefer [u]divmodsi3 rather than [u]divsi3 even remainder is
;; unused, so we use split to drop mod operation for lower register pressure.

(define_split
  [(set (match_operand:SI 0 "register_operand")
	(div:SI (match_operand:SI 1 "register_operand")
		(match_operand:SI 2 "register_operand")))
   (set (match_operand:SI 3 "register_operand")
	(mod:SI (match_dup 1) (match_dup 2)))]
  "find_regno_note (insn, REG_UNUSED, REGNO (operands[3])) != NULL
   && can_create_pseudo_p ()"
  [(set (match_dup 0)
	(div:SI (match_dup 1)
		(match_dup 2)))])

(define_split
  [(set (match_operand:SI 0 "register_operand")
	(udiv:SI (match_operand:SI 1 "register_operand")
		 (match_operand:SI 2 "register_operand")))
   (set (match_operand:SI 3 "register_operand")
	(umod:SI (match_dup 1) (match_dup 2)))]
  "find_regno_note (insn, REG_UNUSED, REGNO (operands[3])) != NULL
   && can_create_pseudo_p ()"
  [(set (match_dup 0)
	(udiv:SI (match_dup 1)
		 (match_dup 2)))])

(define_peephole2
  [(set (match_operand:DI 0 "register_operand")
	(mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand"))
		 (sign_extend:DI (match_operand:SI 2 "register_operand"))))]
  "NDS32_EXT_DSP_P ()
   && peep2_regno_dead_p (1, WORDS_BIG_ENDIAN ? REGNO (operands[0]) + 1 : REGNO (operands[0]))"
  [(const_int 1)]
{
  rtx highpart = nds32_di_high_part_subreg (operands[0]);
  emit_insn (gen_smulsi3_highpart (highpart, operands[1], operands[2]));
  DONE;
})

(define_split
  [(set (match_operand:DI 0 "nds32_general_register_operand" "")
	(match_operand:DI 1 "nds32_general_register_operand" ""))]
  "find_regno_note (insn, REG_UNUSED, REGNO (operands[0])) != NULL
   || find_regno_note (insn, REG_UNUSED, REGNO (operands[0]) + 1) != NULL"
  [(set (match_dup 0) (match_dup 1))]
{
  rtx dead_note = find_regno_note (curr_insn, REG_UNUSED, REGNO (operands[0]));
  HOST_WIDE_INT offset;
  if (dead_note == NULL_RTX)
    offset = 0;
  else
    offset = 4;
  operands[0] = simplify_gen_subreg (
		  SImode, operands[0],
		  DImode, offset);
  operands[1] = simplify_gen_subreg (
		  SImode, operands[1],
		  DImode, offset);
})
