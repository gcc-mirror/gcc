/* Subroutines for insn-output.c for AT&T we32000 Family.
   Copyright (C) 1991, 1992, 1997, 1998, 1999, 2000, 2001
   Free Software Foundation, Inc.
   Contributed by John Wehle (john@feith1.uucp)

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


#include "config.h"
#include "system.h"
#include "insn-config.h"
#include "rtl.h"
#include "function.h"
#include "real.h"
#include "recog.h"
#include "output.h"
#include "regs.h"
#include "tree.h"
#include "expr.h"
#include "hard-reg-set.h"
#include "tm_p.h"
#include "target.h"
#include "target-def.h"

static void we32k_output_function_prologue PARAMS ((FILE *, HOST_WIDE_INT));
static void we32k_output_function_epilogue PARAMS ((FILE *, HOST_WIDE_INT));

/* Initialize the GCC target structure.  */
#undef TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\t.half\t"
#undef TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP "\t.word\t"

#undef TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE we32k_output_function_prologue
#undef TARGET_ASM_FUNCTION_EPILOGUE
#define TARGET_ASM_FUNCTION_EPILOGUE we32k_output_function_epilogue

struct gcc_target targetm = TARGET_INITIALIZER;

/* Generate the assembly code for function entry.  FILE is a stdio
   stream to output the code to.  SIZE is an int: how many units of
   temporary storage to allocate.

   Refer to the array `regs_ever_live' to determine which registers to
   save; `regs_ever_live[I]' is nonzero if register number I is ever
   used in the function.  This function is responsible for knowing
   which registers should not be saved even if used.  */

static void
we32k_output_function_prologue (file, size)
     FILE *file;
     HOST_WIDE_INT size;
{
  register int nregs_to_save;
  register int regno;

  nregs_to_save = 0;
  for (regno = 8; regno > 2; regno--)
    if (regs_ever_live[regno] && ! call_used_regs[regno])
      nregs_to_save = (9 - regno);

  fprintf (file, "\tsave &%d\n", nregs_to_save);
  if (size)
    fprintf (file, "\taddw2 &%d,%%sp\n", (size + 3) & ~3);
}

/* This function generates the assembly code for function exit.
   Args are as for output_function_prologue ().

   The function epilogue should not depend on the current stack
   pointer!  It should use the frame pointer only.  This is mandatory
   because of alloca; we also take advantage of it to omit stack
   adjustments before returning.  */

static void
we32k_output_function_epilogue (file, size)
     FILE *file;
     HOST_WIDE_INT size ATTRIBUTE_UNUSED;
{
  register int nregs_to_restore;
  register int regno;

  nregs_to_restore = 0;
  for (regno = 8; regno > 2; regno--)
    if (regs_ever_live[regno] && ! call_used_regs[regno])
      nregs_to_restore = (9 - regno);

  fprintf (file, "\tret &%d\n", nregs_to_restore);
}

void
output_move_double (operands)
     rtx *operands;
{
  rtx lsw_operands[2];
  rtx lsw_sreg = NULL;
  rtx msw_dreg = NULL;

  if (GET_CODE (operands[0]) == REG) 
    {
      lsw_operands[0] = gen_rtx_REG (SImode, REGNO (operands[0]) + 1);
      msw_dreg = operands[0];
    }
  else if (GET_CODE (operands[0]) == MEM && offsettable_memref_p (operands[0]))
    lsw_operands[0] = adjust_address (operands[0], SImode, 4);
  else
    abort ();

  if (GET_CODE (operands[1]) == REG) 
    {
      lsw_operands[1] = gen_rtx_REG (SImode, REGNO (operands[1]) + 1);
      lsw_sreg = lsw_operands[1];
    }
  else if (GET_CODE (operands[1]) == MEM && offsettable_memref_p (operands[1])) 
    {
      lsw_operands[1] = adjust_address (operands[1], SImode, 4);
      lsw_sreg = operands[1];
      for ( ; ; ) 
	{
	  if (REG_P (lsw_sreg))
	    break;
	  if (CONSTANT_ADDRESS_P (lsw_sreg)) 
	    {
	      lsw_sreg = NULL;
	      break;
	    }
	  if (GET_CODE (lsw_sreg) == MEM) 
	    {
	      lsw_sreg = XEXP (lsw_sreg, 0);
	      continue;
	    }
	  if (GET_CODE (lsw_sreg) == PLUS)
	    {
	      if (CONSTANT_ADDRESS_P (XEXP (lsw_sreg, 1))) 
		{
		  lsw_sreg = XEXP (lsw_sreg, 0);
		  continue;
		}
	      else if (CONSTANT_ADDRESS_P (XEXP (lsw_sreg, 0))) 
		{
		  lsw_sreg = XEXP (lsw_sreg, 1);
		  continue;
		}
	    }
	  abort ();
	}
    }
  else if (GET_CODE (operands[1]) == CONST_DOUBLE)
    {
      lsw_operands[1] = GEN_INT (CONST_DOUBLE_HIGH (operands[1]));
      operands[1] = GEN_INT (CONST_DOUBLE_LOW (operands[1]));
    }
  else if (GET_CODE (operands[1]) == CONST_INT)
    {
      lsw_operands[1] = operands[1];
      operands[1] = const0_rtx;
    }
  else
    abort ();

  if (!msw_dreg || !lsw_sreg || REGNO (msw_dreg) != REGNO (lsw_sreg)) 
    {
      output_asm_insn ("movw %1, %0", operands);
      output_asm_insn ("movw %1, %0", lsw_operands);
    }
  else 
    {
      output_asm_insn ("movw %1, %0", lsw_operands);
      output_asm_insn ("movw %1, %0", operands);
    }
}

void
output_push_double (operands)
     rtx *operands;
{
  rtx lsw_operands[1];

  if (GET_CODE (operands[0]) == REG)
    lsw_operands[0] = gen_rtx_REG (SImode, REGNO (operands[0]) + 1);
  else if (GET_CODE (operands[0]) == MEM && offsettable_memref_p (operands[0]))
    lsw_operands[0] = adjust_address (operands[0], SImode, 4);
  else if (GET_CODE (operands[0]) == CONST_DOUBLE)
    {
      lsw_operands[0] = GEN_INT (CONST_DOUBLE_HIGH (operands[0]));
      operands[0] = GEN_INT (CONST_DOUBLE_LOW (operands[0]));
    }
  else if (GET_CODE (operands[0]) == CONST_INT)
    { 
      lsw_operands[0] = operands[0];
      operands[0] = const0_rtx;
    }
  else
    abort ();

  output_asm_insn ("pushw %0", operands);
  output_asm_insn ("pushw %0", lsw_operands);
}
