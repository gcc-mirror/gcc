/* Auxiliary functions for output asm template or expand rtl
   pattern of Andes NDS32 cpu for GNU compiler
   Copyright (C) 2012-2017 Free Software Foundation, Inc.
   Contributed by Andes Technology Corporation.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* ------------------------------------------------------------------------ */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "memmodel.h"
#include "tm_p.h"
#include "optabs.h"		/* For GEN_FCN.  */
#include "recog.h"
#include "output.h"
#include "tm-constrs.h"

/* ------------------------------------------------------------------------ */

/* A helper function to return character based on byte size.  */
static char
nds32_byte_to_size (int byte)
{
  switch (byte)
    {
    case 4:
      return 'w';
    case 2:
      return 'h';
    case 1:
      return 'b';
    default:
      /* Normally it should not be here.  */
      gcc_unreachable ();
    }
}

/* A helper function to return memory format.  */
enum nds32_16bit_address_type
nds32_mem_format (rtx op)
{
  machine_mode mode_test;
  int val;
  int regno;

  if (!TARGET_16_BIT)
    return ADDRESS_NOT_16BIT_FORMAT;

  mode_test = GET_MODE (op);

  op = XEXP (op, 0);

  /* 45 format.  */
  if (GET_CODE (op) == REG && (mode_test == SImode))
    return ADDRESS_REG;

  /* 333 format for QI/HImode.  */
  if (GET_CODE (op) == REG && (REGNO (op) < R8_REGNUM))
    return ADDRESS_LO_REG_IMM3U;

  /* post_inc 333 format.  */
  if ((GET_CODE (op) == POST_INC) && (mode_test == SImode))
    {
      regno = REGNO(XEXP (op, 0));

      if (regno < 8)
	return ADDRESS_POST_INC_LO_REG_IMM3U;
    }

  /* post_inc 333 format.  */
  if ((GET_CODE (op) == POST_MODIFY)
      && (mode_test == SImode)
      && (REG_P (XEXP (XEXP (op, 1), 0)))
      && (CONST_INT_P (XEXP (XEXP (op, 1), 1))))
    {
      regno = REGNO (XEXP (XEXP (op, 1), 0));
      val = INTVAL (XEXP (XEXP (op, 1), 1));
      if (regno < 8 && val < 32)
	return ADDRESS_POST_INC_LO_REG_IMM3U;
    }

  if ((GET_CODE (op) == PLUS)
      && (GET_CODE (XEXP (op, 0)) == REG)
      && (GET_CODE (XEXP (op, 1)) == CONST_INT))
    {
      val = INTVAL (XEXP (op, 1));

      regno = REGNO(XEXP (op, 0));

      if (regno > 7
	  && regno != SP_REGNUM
	  && regno != FP_REGNUM)
	return ADDRESS_NOT_16BIT_FORMAT;

      switch (mode_test)
	{
	case QImode:
	  /* 333 format.  */
	  if (val >= 0 && val < 8 && regno < 8)
	    return ADDRESS_LO_REG_IMM3U;
	  break;

	case HImode:
	  /* 333 format.  */
	  if (val >= 0 && val < 16 && (val % 2 == 0) && regno < 8)
	    return ADDRESS_LO_REG_IMM3U;
	  break;

	case SImode:
	case SFmode:
	case DFmode:
	  /* fp imply 37 format.  */
	  if ((regno == FP_REGNUM) &&
	      (val >= 0 && val < 512 && (val % 4 == 0)))
	    return ADDRESS_FP_IMM7U;
	  /* sp imply 37 format.  */
	  else if ((regno == SP_REGNUM) &&
		   (val >= 0 && val < 512 && (val % 4 == 0)))
	    return ADDRESS_SP_IMM7U;
	  /* 333 format.  */
	  else if (val >= 0 && val < 32 && (val % 4 == 0) && regno < 8)
	    return ADDRESS_LO_REG_IMM3U;
	  break;

	default:
	  break;
	}
    }

  return ADDRESS_NOT_16BIT_FORMAT;
}

/* Output 16-bit store.  */
const char *
nds32_output_16bit_store (rtx *operands, int byte)
{
  char pattern[100];
  char size;
  rtx code = XEXP (operands[0], 0);

  size = nds32_byte_to_size (byte);

  switch (nds32_mem_format (operands[0]))
    {
    case ADDRESS_REG:
      operands[0] = code;
      output_asm_insn ("swi450\t%1, [%0]", operands);
      break;
    case ADDRESS_LO_REG_IMM3U:
      snprintf (pattern, sizeof (pattern), "s%ci333\t%%1, %%0", size);
      output_asm_insn (pattern, operands);
      break;
    case ADDRESS_POST_INC_LO_REG_IMM3U:
      snprintf (pattern, sizeof (pattern), "s%ci333.bi\t%%1, %%0", size);
      output_asm_insn (pattern, operands);
      break;
    case ADDRESS_FP_IMM7U:
      output_asm_insn ("swi37\t%1, %0", operands);
      break;
    case ADDRESS_SP_IMM7U:
      /* Get immediate value and set back to operands[1].  */
      operands[0] = XEXP (code, 1);
      output_asm_insn ("swi37.sp\t%1, [ + (%0)]", operands);
      break;
    default:
      break;
    }

  return "";
}

/* Output 16-bit load.  */
const char *
nds32_output_16bit_load (rtx *operands, int byte)
{
  char pattern[100];
  unsigned char size;
  rtx code = XEXP (operands[1], 0);

  size = nds32_byte_to_size (byte);

  switch (nds32_mem_format (operands[1]))
    {
    case ADDRESS_REG:
      operands[1] = code;
      output_asm_insn ("lwi450\t%0, [%1]", operands);
      break;
    case ADDRESS_LO_REG_IMM3U:
      snprintf (pattern, sizeof (pattern), "l%ci333\t%%0, %%1", size);
      output_asm_insn (pattern, operands);
      break;
    case ADDRESS_POST_INC_LO_REG_IMM3U:
      snprintf (pattern, sizeof (pattern), "l%ci333.bi\t%%0, %%1", size);
      output_asm_insn (pattern, operands);
      break;
    case ADDRESS_FP_IMM7U:
      output_asm_insn ("lwi37\t%0, %1", operands);
      break;
    case ADDRESS_SP_IMM7U:
      /* Get immediate value and set back to operands[0].  */
      operands[1] = XEXP (code, 1);
      output_asm_insn ("lwi37.sp\t%0, [ + (%1)]", operands);
      break;
    default:
      break;
    }

  return "";
}

/* Output 32-bit store.  */
const char *
nds32_output_32bit_store (rtx *operands, int byte)
{
  char pattern[100];
  unsigned char size;
  rtx code = XEXP (operands[0], 0);

  size = nds32_byte_to_size (byte);

  switch (GET_CODE (code))
    {
    case REG:
      /* (mem (reg X))
	 => access location by using register,
	 use "sbi / shi / swi" */
      snprintf (pattern, sizeof (pattern), "s%ci\t%%1, %%0", size);
      break;

    case SYMBOL_REF:
    case CONST:
      /* (mem (symbol_ref X))
	 (mem (const (...)))
	 => access global variables,
	 use "sbi.gp / shi.gp / swi.gp" */
      operands[0] = XEXP (operands[0], 0);
      snprintf (pattern, sizeof (pattern), "s%ci.gp\t%%1, [ + %%0]", size);
      break;

    case POST_INC:
      /* (mem (post_inc reg))
	 => access location by using register which will be post increment,
	 use "sbi.bi / shi.bi / swi.bi" */
      snprintf (pattern, sizeof (pattern),
		"s%ci.bi\t%%1, %%0, %d", size, byte);
      break;

    case POST_DEC:
      /* (mem (post_dec reg))
	 => access location by using register which will be post decrement,
	 use "sbi.bi / shi.bi / swi.bi" */
      snprintf (pattern, sizeof (pattern),
		"s%ci.bi\t%%1, %%0, -%d", size, byte);
      break;

    case POST_MODIFY:
      switch (GET_CODE (XEXP (XEXP (code, 1), 1)))
	{
	case REG:
	case SUBREG:
	  /* (mem (post_modify (reg) (plus (reg) (reg))))
	     => access location by using register which will be
	     post modified with reg,
	     use "sb.bi/ sh.bi / sw.bi" */
	  snprintf (pattern, sizeof (pattern), "s%c.bi\t%%1, %%0", size);
	  break;
	case CONST_INT:
	  /* (mem (post_modify (reg) (plus (reg) (const_int))))
	     => access location by using register which will be
	     post modified with const_int,
	     use "sbi.bi/ shi.bi / swi.bi" */
	  snprintf (pattern, sizeof (pattern), "s%ci.bi\t%%1, %%0", size);
	  break;
	default:
	  abort ();
	}
      break;

    case PLUS:
      switch (GET_CODE (XEXP (code, 1)))
	{
	case REG:
	case SUBREG:
	  /* (mem (plus reg reg)) or (mem (plus (mult reg const_int) reg))
	     => access location by adding two registers,
	     use "sb / sh / sw" */
	  snprintf (pattern, sizeof (pattern), "s%c\t%%1, %%0", size);
	  break;
	case CONST_INT:
	  /* (mem (plus reg const_int))
	     => access location by adding one register with const_int,
	     use "sbi / shi / swi" */
	  snprintf (pattern, sizeof (pattern), "s%ci\t%%1, %%0", size);
	  break;
	default:
	  abort ();
	}
      break;

    case LO_SUM:
      operands[2] = XEXP (code, 1);
      operands[0] = XEXP (code, 0);
      snprintf (pattern, sizeof (pattern),
		"s%ci\t%%1, [%%0 + lo12(%%2)]", size);
      break;

    default:
      abort ();
    }

  output_asm_insn (pattern, operands);
  return "";
}

/* Output 32-bit load.  */
const char *
nds32_output_32bit_load (rtx *operands, int byte)
{
  char pattern[100];
  unsigned char size;
  rtx code;

  code = XEXP (operands[1], 0);

  size = nds32_byte_to_size (byte);

  switch (GET_CODE (code))
    {
    case REG:
      /* (mem (reg X))
	 => access location by using register,
	 use "lbi / lhi / lwi" */
      snprintf (pattern, sizeof (pattern), "l%ci\t%%0, %%1", size);
      break;

    case SYMBOL_REF:
    case CONST:
      /* (mem (symbol_ref X))
	 (mem (const (...)))
	 => access global variables,
	 use "lbi.gp / lhi.gp / lwi.gp" */
      operands[1] = XEXP (operands[1], 0);
      snprintf (pattern, sizeof (pattern), "l%ci.gp\t%%0, [ + %%1]", size);
      break;

    case POST_INC:
      /* (mem (post_inc reg))
	 => access location by using register which will be post increment,
	 use "lbi.bi / lhi.bi / lwi.bi" */
      snprintf (pattern, sizeof (pattern),
		"l%ci.bi\t%%0, %%1, %d", size, byte);
      break;

    case POST_DEC:
      /* (mem (post_dec reg))
	 => access location by using register which will be post decrement,
	 use "lbi.bi / lhi.bi / lwi.bi" */
      snprintf (pattern, sizeof (pattern),
		"l%ci.bi\t%%0, %%1, -%d", size, byte);
      break;

    case POST_MODIFY:
      switch (GET_CODE (XEXP (XEXP (code, 1), 1)))
	{
	case REG:
	case SUBREG:
	  /* (mem (post_modify (reg) (plus (reg) (reg))))
	     => access location by using register which will be
	     post modified with reg,
	     use "lb.bi/ lh.bi / lw.bi" */
	  snprintf (pattern, sizeof (pattern), "l%c.bi\t%%0, %%1", size);
	  break;
	case CONST_INT:
	  /* (mem (post_modify (reg) (plus (reg) (const_int))))
	     => access location by using register which will be
	     post modified with const_int,
	     use "lbi.bi/ lhi.bi / lwi.bi" */
	  snprintf (pattern, sizeof (pattern), "l%ci.bi\t%%0, %%1", size);
	  break;
	default:
	  abort ();
	}
      break;

    case PLUS:
      switch (GET_CODE (XEXP (code, 1)))
	{
	case REG:
	case SUBREG:
	  /* (mem (plus reg reg)) or (mem (plus (mult reg const_int) reg))
	     use "lb / lh / lw" */
	  snprintf (pattern, sizeof (pattern), "l%c\t%%0, %%1", size);
	  break;
	case CONST_INT:
	  /* (mem (plus reg const_int))
	     => access location by adding one register with const_int,
	     use "lbi / lhi / lwi" */
	  snprintf (pattern, sizeof (pattern), "l%ci\t%%0, %%1", size);
	  break;
	default:
	  abort ();
	}
      break;

    case LO_SUM:
      operands[2] = XEXP (code, 1);
      operands[1] = XEXP (code, 0);
      snprintf (pattern, sizeof (pattern),
		"l%ci\t%%0, [%%1 + lo12(%%2)]", size);
      break;

    default:
      abort ();
    }

  output_asm_insn (pattern, operands);
  return "";
}

/* Output 32-bit load with signed extension.  */
const char *
nds32_output_32bit_load_s (rtx *operands, int byte)
{
  char pattern[100];
  unsigned char size;
  rtx code;

  code = XEXP (operands[1], 0);

  size = nds32_byte_to_size (byte);

  switch (GET_CODE (code))
    {
    case REG:
      /* (mem (reg X))
         => access location by using register,
         use "lbsi / lhsi" */
      snprintf (pattern, sizeof (pattern), "l%csi\t%%0, %%1", size);
      break;

    case SYMBOL_REF:
    case CONST:
      /* (mem (symbol_ref X))
         (mem (const (...)))
         => access global variables,
         use "lbsi.gp / lhsi.gp" */
      operands[1] = XEXP (operands[1], 0);
      snprintf (pattern, sizeof (pattern), "l%csi.gp\t%%0, [ + %%1]", size);
      break;

    case POST_INC:
      /* (mem (post_inc reg))
         => access location by using register which will be post increment,
         use "lbsi.bi / lhsi.bi" */
      snprintf (pattern, sizeof (pattern),
		"l%csi.bi\t%%0, %%1, %d", size, byte);
      break;

    case POST_DEC:
      /* (mem (post_dec reg))
         => access location by using register which will be post decrement,
         use "lbsi.bi / lhsi.bi" */
      snprintf (pattern, sizeof (pattern),
		"l%csi.bi\t%%0, %%1, -%d", size, byte);
      break;

    case POST_MODIFY:
      switch (GET_CODE (XEXP (XEXP (code, 1), 1)))
	{
	case REG:
	case SUBREG:
	  /* (mem (post_modify (reg) (plus (reg) (reg))))
	     => access location by using register which will be
	     post modified with reg,
	     use "lbs.bi/ lhs.bi" */
	  snprintf (pattern, sizeof (pattern), "l%cs.bi\t%%0, %%1", size);
	  break;
	case CONST_INT:
	  /* (mem (post_modify (reg) (plus (reg) (const_int))))
	     => access location by using register which will be
	     post modified with const_int,
	     use "lbsi.bi/ lhsi.bi" */
	  snprintf (pattern, sizeof (pattern), "l%csi.bi\t%%0, %%1", size);
	  break;
	default:
	  abort ();
	}
      break;

    case PLUS:
      switch (GET_CODE (XEXP (code, 1)))
	{
	case REG:
	case SUBREG:
	  /* (mem (plus reg reg)) or (mem (plus (mult reg const_int) reg))
	     use "lbs / lhs" */
	  snprintf (pattern, sizeof (pattern), "l%cs\t%%0, %%1", size);
	  break;
	case CONST_INT:
	  /* (mem (plus reg const_int))
	     => access location by adding one register with const_int,
	     use "lbsi / lhsi" */
	  snprintf (pattern, sizeof (pattern), "l%csi\t%%0, %%1", size);
	  break;
	default:
	  abort ();
	}
      break;

    case LO_SUM:
      operands[2] = XEXP (code, 1);
      operands[1] = XEXP (code, 0);
      snprintf (pattern, sizeof (pattern),
		"l%csi\t%%0, [%%1 + lo12(%%2)]", size);
      break;

    default:
      abort ();
    }

  output_asm_insn (pattern, operands);
  return "";
}

/* Function to output stack push operation.
   We need to deal with normal stack push multiple or stack v3push.  */
const char *
nds32_output_stack_push (rtx par_rtx)
{
  /* A string pattern for output_asm_insn().  */
  char pattern[100];
  /* The operands array which will be used in output_asm_insn().  */
  rtx operands[3];
  /* Pick up varargs first regno and last regno for further use.  */
  int rb_va_args = cfun->machine->va_args_first_regno;
  int re_va_args = cfun->machine->va_args_last_regno;
  int last_argument_regno = NDS32_FIRST_GPR_REGNUM
			    + NDS32_MAX_GPR_REGS_FOR_ARGS
			    - 1;
  /* Pick up callee-saved first regno and last regno for further use.  */
  int rb_callee_saved = cfun->machine->callee_saved_first_gpr_regno;
  int re_callee_saved = cfun->machine->callee_saved_last_gpr_regno;

  /* First we need to check if we are pushing argument registers not used
     for the named arguments.  If so, we have to create 'smw.adm' (push.s)
     instruction.  */
  if (reg_mentioned_p (gen_rtx_REG (SImode, last_argument_regno), par_rtx))
    {
      /* Set operands[0] and operands[1].  */
      operands[0] = gen_rtx_REG (SImode, rb_va_args);
      operands[1] = gen_rtx_REG (SImode, re_va_args);
      /* Create assembly code pattern: "Rb, Re, { }".  */
      snprintf (pattern, sizeof (pattern), "push.s\t%s", "%0, %1, { }");
      /* We use output_asm_insn() to output assembly code by ourself.  */
      output_asm_insn (pattern, operands);
      return "";
    }

  /* If we step here, we are going to do v3push or multiple push operation.  */

  /* The v3push/v3pop instruction should only be applied on
     none-isr and none-variadic function.  */
  if (TARGET_V3PUSH
      && !nds32_isr_function_p (current_function_decl)
      && (cfun->machine->va_args_size == 0))
    {
      /* For stack v3push:
           operands[0]: Re
           operands[1]: imm8u */

      /* This variable is to check if 'push25 Re,imm8u' is available.  */
      int sp_adjust;

      /* Set operands[0].  */
      operands[0] = gen_rtx_REG (SImode, re_callee_saved);

      /* Check if we can generate 'push25 Re,imm8u',
         otherwise, generate 'push25 Re,0'.  */
      sp_adjust = cfun->machine->local_size
		  + cfun->machine->out_args_size
		  + cfun->machine->callee_saved_area_gpr_padding_bytes;
      if (satisfies_constraint_Iu08 (GEN_INT (sp_adjust))
	  && NDS32_DOUBLE_WORD_ALIGN_P (sp_adjust))
	operands[1] = GEN_INT (sp_adjust);
      else
	operands[1] = GEN_INT (0);

      /* Create assembly code pattern.  */
      snprintf (pattern, sizeof (pattern), "push25\t%%0, %%1");
    }
  else
    {
      /* For normal stack push multiple:
         operands[0]: Rb
         operands[1]: Re
         operands[2]: En4 */

      /* This variable is used to check if we only need to generate En4 field.
         As long as Rb==Re=SP_REGNUM, we set this variable to 1.  */
      int push_en4_only_p = 0;

      /* Set operands[0] and operands[1].  */
      operands[0] = gen_rtx_REG (SImode, rb_callee_saved);
      operands[1] = gen_rtx_REG (SImode, re_callee_saved);

      /* 'smw.adm $sp,[$sp],$sp,0' means push nothing.  */
      if (!cfun->machine->fp_size
	  && !cfun->machine->gp_size
	  && !cfun->machine->lp_size
	  && REGNO (operands[0]) == SP_REGNUM
	  && REGNO (operands[1]) == SP_REGNUM)
	{
	  /* No need to generate instruction.  */
	  return "";
	}
      else
	{
	  /* If Rb==Re=SP_REGNUM, we only need to generate En4 field.  */
	  if (REGNO (operands[0]) == SP_REGNUM
	      && REGNO (operands[1]) == SP_REGNUM)
	    push_en4_only_p = 1;

	  /* Create assembly code pattern.
	     We need to handle the form: "Rb, Re, { $fp $gp $lp }".  */
	  snprintf (pattern, sizeof (pattern),
		    "push.s\t%s{%s%s%s }",
		    push_en4_only_p ? "" : "%0, %1, ",
		    cfun->machine->fp_size ? " $fp" : "",
		    cfun->machine->gp_size ? " $gp" : "",
		    cfun->machine->lp_size ? " $lp" : "");
	}
    }

  /* We use output_asm_insn() to output assembly code by ourself.  */
  output_asm_insn (pattern, operands);
  return "";
}

/* Function to output stack pop operation.
   We need to deal with normal stack pop multiple or stack v3pop.  */
const char *
nds32_output_stack_pop (rtx par_rtx ATTRIBUTE_UNUSED)
{
  /* A string pattern for output_asm_insn().  */
  char pattern[100];
  /* The operands array which will be used in output_asm_insn().  */
  rtx operands[3];
  /* Pick up callee-saved first regno and last regno for further use.  */
  int rb_callee_saved = cfun->machine->callee_saved_first_gpr_regno;
  int re_callee_saved = cfun->machine->callee_saved_last_gpr_regno;

  /* If we step here, we are going to do v3pop or multiple pop operation.  */

  /* The v3push/v3pop instruction should only be applied on
     none-isr and none-variadic function.  */
  if (TARGET_V3PUSH
      && !nds32_isr_function_p (current_function_decl)
      && (cfun->machine->va_args_size == 0))
    {
      /* For stack v3pop:
           operands[0]: Re
           operands[1]: imm8u */

      /* This variable is to check if 'pop25 Re,imm8u' is available.  */
      int sp_adjust;

      /* Set operands[0].  */
      operands[0] = gen_rtx_REG (SImode, re_callee_saved);

      /* Check if we can generate 'pop25 Re,imm8u',
         otherwise, generate 'pop25 Re,0'.
         We have to consider alloca issue as well.
         If the function does call alloca(), the stack pointer is not fixed.
         In that case, we cannot use 'pop25 Re,imm8u' directly.
         We have to caculate stack pointer from frame pointer
         and then use 'pop25 Re,0'.  */
      sp_adjust = cfun->machine->local_size
		  + cfun->machine->out_args_size
		  + cfun->machine->callee_saved_area_gpr_padding_bytes;
      if (satisfies_constraint_Iu08 (GEN_INT (sp_adjust))
	  && NDS32_DOUBLE_WORD_ALIGN_P (sp_adjust)
	  && !cfun->calls_alloca)
	operands[1] = GEN_INT (sp_adjust);
      else
	operands[1] = GEN_INT (0);

      /* Create assembly code pattern.  */
      snprintf (pattern, sizeof (pattern), "pop25\t%%0, %%1");
    }
  else
    {
      /* For normal stack pop multiple:
         operands[0]: Rb
         operands[1]: Re
         operands[2]: En4 */

      /* This variable is used to check if we only need to generate En4 field.
         As long as Rb==Re=SP_REGNUM, we set this variable to 1.  */
      int pop_en4_only_p = 0;

      /* Set operands[0] and operands[1].  */
      operands[0] = gen_rtx_REG (SImode, rb_callee_saved);
      operands[1] = gen_rtx_REG (SImode, re_callee_saved);

      /* 'lmw.bim $sp,[$sp],$sp,0' means pop nothing.  */
      if (!cfun->machine->fp_size
	  && !cfun->machine->gp_size
	  && !cfun->machine->lp_size
	  && REGNO (operands[0]) == SP_REGNUM
	  && REGNO (operands[1]) == SP_REGNUM)
	{
	  /* No need to generate instruction.  */
	  return "";
	}
      else
	{
	  /* If Rb==Re=SP_REGNUM, we only need to generate En4 field.  */
	  if (REGNO (operands[0]) == SP_REGNUM
	      && REGNO (operands[1]) == SP_REGNUM)
	    pop_en4_only_p = 1;

	  /* Create assembly code pattern.
	     We need to handle the form: "Rb, Re, { $fp $gp $lp }".  */
	  snprintf (pattern, sizeof (pattern),
		    "pop.s\t%s{%s%s%s }",
		    pop_en4_only_p ? "" : "%0, %1, ",
		    cfun->machine->fp_size ? " $fp" : "",
		    cfun->machine->gp_size ? " $gp" : "",
		    cfun->machine->lp_size ? " $lp" : "");
	}
    }

  /* We use output_asm_insn() to output assembly code by ourself.  */
  output_asm_insn (pattern, operands);
  return "";
}

/* Function to generate PC relative jump table.
   Refer to nds32.md for more details.

   The following is the sample for the case that diff value
   can be presented in '.short' size.

     addi    $r1, $r1, -(case_lower_bound)
     slti    $ta, $r1, (case_number)
     beqz    $ta, .L_skip_label

     la      $ta, .L35             ! get jump table address
     lh      $r1, [$ta + $r1 << 1] ! load symbol diff from jump table entry
     addi    $ta, $r1, $ta
     jr5     $ta

     ! jump table entry
   L35:
     .short  .L25-.L35
     .short  .L26-.L35
     .short  .L27-.L35
     .short  .L28-.L35
     .short  .L29-.L35
     .short  .L30-.L35
     .short  .L31-.L35
     .short  .L32-.L35
     .short  .L33-.L35
     .short  .L34-.L35 */
const char *
nds32_output_casesi_pc_relative (rtx *operands)
{
  machine_mode mode;
  rtx diff_vec;

  diff_vec = PATTERN (NEXT_INSN (as_a <rtx_insn *> (operands[1])));

  gcc_assert (GET_CODE (diff_vec) == ADDR_DIFF_VEC);

  /* Step C: "t <-- operands[1]".  */
  output_asm_insn ("la\t$ta, %l1", operands);

  /* Get the mode of each element in the difference vector.  */
  mode = GET_MODE (diff_vec);

  /* Step D: "z <-- (mem (plus (operands[0] << m) t))",
     where m is 0, 1, or 2 to load address-diff value from table.  */
  switch (mode)
    {
    case QImode:
      output_asm_insn ("lb\t%2, [$ta + %0 << 0]", operands);
      break;
    case HImode:
      output_asm_insn ("lh\t%2, [$ta + %0 << 1]", operands);
      break;
    case SImode:
      output_asm_insn ("lw\t%2, [$ta + %0 << 2]", operands);
      break;
    default:
      gcc_unreachable ();
    }

  /* Step E: "t <-- z + t".
     Add table label_ref with address-diff value to
     obtain target case address.  */
  output_asm_insn ("add\t$ta, %2, $ta", operands);

  /* Step F: jump to target with register t.  */
  if (TARGET_16_BIT)
    return "jr5\t$ta";
  else
    return "jr\t$ta";
}

/* Function to generate normal jump table.  */
const char *
nds32_output_casesi (rtx *operands)
{
  /* Step C: "t <-- operands[1]".  */
  output_asm_insn ("la\t$ta, %l1", operands);

  /* Step D: "z <-- (mem (plus (operands[0] << 2) t))".  */
  output_asm_insn ("lw\t%2, [$ta + %0 << 2]", operands);

  /* No need to perform Step E, which is only used for
     pc relative jump table.  */

  /* Step F: jump to target with register z.  */
  if (TARGET_16_BIT)
    return "jr5\t%2";
  else
    return "jr\t%2";
}

/* ------------------------------------------------------------------------ */
