/* Subroutines for insn-output.c for AT&T we32000 Family.
   Contributed by John Wehle (john@feith1.uucp)
   Copyright (C) 1991-1992 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include <stdio.h>
#include "config.h"
#include "rtl.h"
#include "real.h"


void
output_move_double (operands)
     rtx *operands;
{
  rtx lsw_operands[2];
  rtx lsw_sreg = NULL;
  rtx msw_dreg = NULL;

  if (GET_CODE (operands[0]) == REG) 
    {
      lsw_operands[0] = gen_rtx (REG, SImode, REGNO (operands[0]) + 1);
      msw_dreg = operands[0];
    }
  else if (GET_CODE (operands[0]) == MEM && offsettable_memref_p (operands[0]))
    lsw_operands[0] = adj_offsettable_operand (operands[0], 4);
  else
    abort ();

  if (GET_CODE (operands[1]) == REG) 
    {
      lsw_operands[1] = gen_rtx (REG, SImode, REGNO (operands[1]) + 1);
      lsw_sreg = lsw_operands[1];
    }
  else if (GET_CODE (operands[1]) == MEM && offsettable_memref_p (operands[1])) 
    {
      lsw_operands[1] = adj_offsettable_operand (operands[1], 4);
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
      lsw_operands[1] = gen_rtx (CONST_INT, SImode,
				 CONST_DOUBLE_HIGH (operands[1]));
      operands[1] = gen_rtx (CONST_INT, SImode,
			     CONST_DOUBLE_LOW (operands[1]));
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
    lsw_operands[0] = gen_rtx (REG, SImode, REGNO (operands[0]) + 1);
  else if (GET_CODE (operands[0]) == MEM && offsettable_memref_p (operands[0]))
    lsw_operands[0] = adj_offsettable_operand (operands[0], 4);
  else if (GET_CODE (operands[0]) == CONST_DOUBLE)
    {
      lsw_operands[0] = gen_rtx (CONST_INT, SImode,
				 CONST_DOUBLE_HIGH (operands[0]));
      operands[0] = gen_rtx (CONST_INT, SImode,
			     CONST_DOUBLE_LOW (operands[0]));
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
