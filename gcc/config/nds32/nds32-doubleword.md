;; DImode/DFmode patterns description of Andes NDS32 cpu for GNU compiler
;; Copyright (C) 2012-2018 Free Software Foundation, Inc.
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


;; -------------------------------------------------------------
;; Move DImode/DFmode instructions.
;; -------------------------------------------------------------


(define_expand "movdi"
  [(set (match_operand:DI 0 "general_operand" "")
	(match_operand:DI 1 "general_operand" ""))]
  ""
{
  /* Need to force register if mem <- !reg.  */
  if (MEM_P (operands[0]) && !REG_P (operands[1]))
    operands[1] = force_reg (DImode, operands[1]);
})

(define_expand "movdf"
  [(set (match_operand:DF 0 "general_operand" "")
	(match_operand:DF 1 "general_operand" ""))]
  ""
{
  /* Need to force register if mem <- !reg.  */
  if (MEM_P (operands[0]) && !REG_P (operands[1]))
    operands[1] = force_reg (DFmode, operands[1]);
})


(define_insn "move_<mode>"
  [(set (match_operand:DIDF 0 "nonimmediate_operand" "=r, r,  r, r, Da, m, f, Q, f, *r, *f")
	(match_operand:DIDF 1 "general_operand"      " r, i, Da, m,  r, r, Q, f, f, *f, *r"))]
  "register_operand(operands[0], <MODE>mode)
   || register_operand(operands[1], <MODE>mode)"
{
  switch (which_alternative)
    {
    case 0:
      return "movd44\t%0, %1";
    case 1:
      /* reg <- const_int, we ask gcc to split instruction.  */
      return "#";
    case 2:
      /* The memory format is (mem (reg)),
	 we can generate 'lmw.bi' instruction.  */
      return nds32_output_double (operands, true);
    case 3:
      /* We haven't 64-bit load instruction,
	 we split this pattern to two SImode pattern.  */
      return "#";
    case 4:
      /* The memory format is (mem (reg)),
	 we can generate 'smw.bi' instruction.  */
      return nds32_output_double (operands, false);
    case 5:
      /* We haven't 64-bit store instruction,
	 we split this pattern to two SImode pattern.  */
      return "#";
    case 6:
      return nds32_output_float_load (operands);
    case 7:
      return nds32_output_float_store (operands);
    case 8:
      return "fcpysd\t%0, %1, %1";
    case 9:
      return "fmfdr\t%0, %1";
    case 10:
      return "fmtdr\t%1, %0";
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "type"    "alu,alu,load,load,store,store,fload,fstore,fcpy,fmfdr,fmtdr")
   (set_attr_alternative "length"
     [
       ;; Alternative 0
       (if_then_else (match_test "!TARGET_16_BIT")
		     (const_int 4)
		     (const_int 2))
       ;; Alternative 1
       (const_int 16)
       ;; Alternative 2
       (const_int 4)
       ;; Alternative 3
       (const_int 8)
       ;; Alternative 4
       (const_int 4)
       ;; Alternative 5
       (const_int 8)
       ;; Alternative 6
       (const_int 4)
       ;; Alternative 7
       (const_int 4)
       ;; Alternative 8
       (const_int 4)
       ;; Alternative 9
       (const_int 4)
       ;; Alternative 10
       (const_int 4)
     ])
   (set_attr "feature" " v1, v1,  v1,  v1,   v1,   v1,    fpu,    fpu,    fpu,    fpu,    fpu")])

;; Split move_di pattern when the hard register is odd.
(define_split
  [(set (match_operand:DIDF 0 "register_operand" "")
	(match_operand:DIDF 1 "register_operand" ""))]
  "(NDS32_IS_GPR_REGNUM (REGNO (operands[0]))
    && ((REGNO (operands[0]) & 0x1) == 1))
   || (NDS32_IS_GPR_REGNUM (REGNO (operands[1]))
       && ((REGNO (operands[1]) & 0x1) == 1))"
  [(set (match_dup 2) (match_dup 3))
   (set (match_dup 4) (match_dup 5))]
  {
     operands[2] = gen_lowpart (SImode, operands[0]);
     operands[4] = gen_highpart (SImode, operands[0]);
     operands[3] = gen_lowpart (SImode, operands[1]);
     operands[5] = gen_highpart (SImode, operands[1]);
  }
)

(define_split
  [(set (match_operand:DIDF 0 "register_operand"     "")
	(match_operand:DIDF 1 "const_double_operand" ""))]
  "flag_pic || reload_completed"
  [(set (match_dup 2) (match_dup 3))
   (set (match_dup 4) (match_dup 5))]
{
  /* Construct lowpart rtx.  */
  operands[2] = gen_lowpart (SImode, operands[0]);
  operands[3] = gen_lowpart (SImode, operands[1]);

  /* Construct highpart rtx.  */
  /* Note that operands[1] can be VOIDmode constant,
     so we need to use gen_highpart_mode().
     Refer to gcc/emit-rtl.c for more information.  */
  operands[4] = gen_highpart (SImode, operands[0]);
  operands[5] = gen_highpart_mode (SImode,
				   GET_MODE (operands[0]), operands[1]);

  /* Actually we would like to create move behavior by ourself.
     So that movsi expander could have chance to split large constant.  */
  emit_move_insn (operands[2], operands[3]);

  unsigned HOST_WIDE_INT mask = GET_MODE_MASK (SImode);
  if ((UINTVAL (operands[3]) & mask) == (UINTVAL (operands[5]) & mask))
    emit_move_insn (operands[4], operands[2]);
  else
    emit_move_insn (operands[4], operands[5]);
  DONE;
})

;; There is 'movd44' instruction for DImode/DFmode movement under V3/V3M ISA.
;; We only need to split it under V2 ISA or none-16-bit code generation.
(define_split
  [(set (match_operand:DIDF 0 "register_operand" "")
	(match_operand:DIDF 1 "register_operand" ""))]
  "reload_completed
   && (TARGET_ISA_V2 || !TARGET_16_BIT)
   && NDS32_IS_GPR_REGNUM (REGNO (operands[0]))
   && NDS32_IS_GPR_REGNUM (REGNO (operands[1]))"
  [(set (match_dup 0) (match_dup 1))
   (set (match_dup 2) (match_dup 3))]
{
  operands[2] = gen_highpart (SImode, operands[0]);
  operands[3] = gen_highpart (SImode, operands[1]);
  operands[0] = gen_lowpart (SImode, operands[0]);
  operands[1] = gen_lowpart (SImode, operands[1]);

  /* Handle a partial overlap.  */
  if (rtx_equal_p (operands[0], operands[3]))
    {
      rtx tmp0 = operands[0];
      rtx tmp1 = operands[1];

      operands[0] = operands[2];
      operands[1] = operands[3];
      operands[2] = tmp0;
      operands[3] = tmp1;
    }
})

(define_split
  [(set (match_operand:DIDF 0 "nds32_general_register_operand" "")
	(match_operand:DIDF 1 "memory_operand" ""))]
  "reload_completed
   && nds32_split_double_word_load_store_p (operands, true)"
  [(set (match_dup 2) (match_dup 3))
   (set (match_dup 4) (match_dup 5))]
{
  nds32_spilt_doubleword (operands, true);
})

(define_split
  [(set (match_operand:DIDF 0  "memory_operand" "")
	(match_operand:DIDF 1  "nds32_general_register_operand" ""))]
  "reload_completed
   && nds32_split_double_word_load_store_p (operands, false)"
  [(set (match_dup 2) (match_dup 3))
   (set (match_dup 4) (match_dup 5))]
{
  nds32_spilt_doubleword (operands, false);
})

;; -------------------------------------------------------------
;; Boolean DImode instructions.
;; -------------------------------------------------------------

;; Nowadays, the generic code is supposed to split the DImode
;; boolean operations and have good code generation.
;; Unless we find out some bad cases, there is no need to
;; define DImode boolean operations by ourself.

;; -------------------------------------------------------------
