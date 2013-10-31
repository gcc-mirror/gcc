;; DImode/DFmode patterns description of Andes NDS32 cpu for GNU compiler
;; Copyright (C) 2012-2013 Free Software Foundation, Inc.
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
  [(set (match_operand:DIDF 0 "nonimmediate_operand" "=r, r, r, m")
	(match_operand:DIDF 1 "general_operand"      " r, i, m, r"))]
  ""
{
  rtx addr;
  rtx otherops[5];

  switch (which_alternative)
    {
    case 0:
      return "movd44\t%0, %1";

    case 1:
      /* reg <- const_int, we ask gcc to split instruction.  */
      return "#";

    case 2:
      /* Refer to nds32_legitimate_address_p() in nds32.c,
         we only allow "reg", "symbol_ref", "const", and "reg + const_int"
         as address rtx for DImode/DFmode memory access.  */
      addr = XEXP (operands[1], 0);

      otherops[0] = gen_rtx_REG (SImode, REGNO (operands[0]));
      otherops[1] = gen_rtx_REG (SImode, REGNO (operands[0]) + 1);
      otherops[2] = addr;

      if (REG_P (addr))
	{
	  /* (reg) <- (mem (reg)) */
	  output_asm_insn ("lmw.bi\t%0, [%2], %1, 0", otherops);
	}
      else if (GET_CODE (addr) == PLUS)
	{
	  /* (reg) <- (mem (plus (reg) (const_int))) */
	  rtx op0 = XEXP (addr, 0);
	  rtx op1 = XEXP (addr, 1);

	  if (REG_P (op0))
	    {
	      otherops[2] = op0;
	      otherops[3] = op1;
	      otherops[4] = gen_int_mode (INTVAL (op1) + 4, SImode);
	    }
	  else
	    {
	      otherops[2] = op1;
	      otherops[3] = op0;
	      otherops[4] = gen_int_mode (INTVAL (op0) + 4, SImode);
	    }

	  /* To avoid base overwrite when REGNO(%0) == REGNO(%2).  */
	  if (REGNO (otherops[0]) != REGNO (otherops[2]))
	    {
	      output_asm_insn ("lwi\t%0, [%2 + (%3)]", otherops);
	      output_asm_insn ("lwi\t%1, [%2 + (%4)]", otherops);
	    }
	  else
	    {
	      output_asm_insn ("lwi\t%1, [%2 + (%4)]", otherops);
	      output_asm_insn ("lwi\t%0,[ %2 + (%3)]", otherops);
	    }
	}
      else
	{
	  /* (reg) <- (mem (symbol_ref ...))
	     (reg) <- (mem (const ...)) */
	  output_asm_insn ("lwi.gp\t%0, [ + %2]", otherops);
	  output_asm_insn ("lwi.gp\t%1, [ + %2 + 4]", otherops);
	}

      /* We have already used output_asm_insn() by ourself,
         so return an empty string.  */
      return "";

    case 3:
      /* Refer to nds32_legitimate_address_p() in nds32.c,
         we only allow "reg", "symbol_ref", "const", and "reg + const_int"
         as address rtx for DImode/DFmode memory access.  */
      addr = XEXP (operands[0], 0);

      otherops[0] = gen_rtx_REG (SImode, REGNO (operands[1]));
      otherops[1] = gen_rtx_REG (SImode, REGNO (operands[1]) + 1);
      otherops[2] = addr;

      if (REG_P (addr))
	{
	  /* (mem (reg)) <- (reg) */
	  output_asm_insn ("smw.bi\t%0, [%2], %1, 0", otherops);
	}
      else if (GET_CODE (addr) == PLUS)
	{
	  /* (mem (plus (reg) (const_int))) <- (reg) */
	  rtx op0 = XEXP (addr, 0);
	  rtx op1 = XEXP (addr, 1);

	  if (REG_P (op0))
	    {
	      otherops[2] = op0;
	      otherops[3] = op1;
	      otherops[4] = gen_int_mode (INTVAL (op1) + 4, SImode);
	    }
	  else
	    {
	      otherops[2] = op1;
	      otherops[3] = op0;
	      otherops[4] = gen_int_mode (INTVAL (op0) + 4, SImode);
	    }

	  /* To avoid base overwrite when REGNO(%0) == REGNO(%2).  */
	  if (REGNO (otherops[0]) != REGNO (otherops[2]))
	    {
	      output_asm_insn ("swi\t%0, [%2 + (%3)]", otherops);
	      output_asm_insn ("swi\t%1, [%2 + (%4)]", otherops);
	    }
	  else
	    {
	      output_asm_insn ("swi\t%1, [%2 + (%4)]", otherops);
	      output_asm_insn ("swi\t%0, [%2 + (%3)]", otherops);
	    }
	}
      else
	{
	  /* (mem (symbol_ref ...)) <- (reg)
	     (mem (const ...))      <- (reg) */
	  output_asm_insn ("swi.gp\t%0, [ + %2]", otherops);
	  output_asm_insn ("swi.gp\t%1, [ + %2 + 4]", otherops);
	}

      /* We have already used output_asm_insn() by ourself,
         so return an empty string.  */
      return "";

    default:
      gcc_unreachable ();
    }
}
  [(set_attr "type"   "move,move,move,move")
   (set_attr "length" "   4,  16,   8,   8")])

(define_split
  [(set (match_operand:DIDF 0 "register_operand"     "")
	(match_operand:DIDF 1 "const_double_operand" ""))]
  "reload_completed"
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
  emit_move_insn (operands[4], operands[5]);
  DONE;
})

;; There is 'movd44' instruction for DImode/DFmode movement under V3/V3M ISA.
;; We only need to split it under V2 ISA or none-16-bit code generation.
(define_split
  [(set (match_operand:DIDF 0 "register_operand" "")
	(match_operand:DIDF 1 "register_operand" ""))]
  "reload_completed
   && (TARGET_ISA_V2 || !TARGET_16_BIT)"
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

;; -------------------------------------------------------------
;; Boolean DImode instructions.
;; -------------------------------------------------------------

;; Nowadays, the generic code is supposed to split the DImode
;; boolean operations and have good code generation.
;; Unless we find out some bad cases, there is no need to
;; define DImode boolean operations by ourself.

;; -------------------------------------------------------------
