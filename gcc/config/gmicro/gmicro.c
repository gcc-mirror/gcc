/* Subroutines for insn-output.c for the Gmicro.
   Ported by Masanobu Yuhara, Fujitsu Laboratories LTD.
   (yuhara@flab.fujitsu.co.jp)

   Copyright (C) 1990, 1991 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Among other things, the copyright
notice and this notice must be preserved on all copies.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include <stdio.h>
#include "config.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-flags.h"
#include "output.h"
#include "insn-attr.h"

extern char *rtx_name[];

mypr (s, a1, a2, a3, a4, a5)
     char *s;
     int a1, a2, a3, a4, a5;
{
  fprintf (stderr, s, a1, a2, a3, a4, a5);
}

myprcode (i)
     int i;
{
  if (i < 0 || i > 90)
    fprintf (stderr, "code = %d\n", i);
  else
    fprintf (stderr, "code = %s\n", rtx_name[i]);
}

myabort (i)
     int i;
{
  fprintf (stderr, "myabort");
  myprcode (i);
}


/* This is how to output an ascii string.  */
/* See ASM_OUTPUT_ASCII in gmicro.h.  */
output_ascii (file, p, size)
     FILE *file;
     char *p;
     int size;
{
  int i;
  int in_quote = 0;
  register int c;

  fprintf (file, "\t.sdata ");

  for (i = 0; i < size; i++) 
    {
      c = p[i];
      if (c >= ' ' && c < 0x7f) 
	{
	  if (!in_quote) 
	    {
	      putc ('"', file);
	      in_quote = 1;
	    }
	  putc (c, file);
	}
      else 
	{
	  if (in_quote) 
	    {
	      putc ('"', file);
	      in_quote = 0;
	    }
	  fprintf (file, "<%d>", c);
	}
    }
  if (in_quote)
    putc ('"', file);
  putc ('\n', file);
}


/* call this when GET_CODE (index) is MULT. */
print_scaled_index (file, index)
     FILE *file;
     register rtx index;
{
  register rtx ireg;
  int scale;

  if (GET_CODE (XEXP (index, 0)) == REG) 
    {
      ireg = XEXP (index, 0);
      scale = INTVAL (XEXP (index, 1));
    }
  else 
    {
      ireg = XEXP (index, 1);
      scale = INTVAL (XEXP (index, 0));
    }
  if (scale == 1)
    fprintf (file, "%s", reg_names[REGNO (ireg)]);
  else
    fprintf (file, "%s*%d", reg_names[REGNO (ireg)], scale);
}
    

print_operand_address (file, addr)
     FILE *file;
     register rtx addr;
{
  register rtx xtmp0, xtmp1, breg, ixreg;
  int scale;
  int needcomma = 0;
  rtx offset;

  fprintf (file, "@");
 retry:
  switch (GET_CODE (addr)) 
    {
    case MEM:
      fprintf (file, "@");
      addr = XEXP (addr, 0);
      goto retry;

    case REG:
      fprintf (file, "%s", reg_names[REGNO (addr)]);
      break;

    case MULT:
      print_scaled_index (file, addr);
      break;

    case PRE_DEC:
      fprintf (file, "-%s", reg_names[REGNO (XEXP (addr, 0))]);
      break;

    case POST_INC:
      fprintf (file, "%s+", reg_names[REGNO (XEXP (addr, 0))]);
      break;

    case PLUS:
      xtmp0 = XEXP (addr, 0);
      xtmp1 = XEXP (addr, 1);
      ixreg = 0;	breg = 0;
      offset = 0;
      if (CONSTANT_ADDRESS_P (xtmp0)) 
	{
	  offset = xtmp0;
	  breg = xtmp1;
	}
      else if (CONSTANT_ADDRESS_P (xtmp1)) 
	{
	  offset = xtmp1;
	  breg = xtmp0;
	}
      else 
	{
	  goto NOT_DISP;
	}

      if (REG_CODE_BASE_P (breg))
	goto PRINT_MEM;

      if (GET_CODE (breg) == MULT) 
	{
	  if (REG_CODE_INDEX_P (XEXP (breg, 0))) 
	    {
	      ixreg = XEXP (breg, 0);
	      scale = INTVAL (XEXP (breg, 1));
	      breg = 0;
	    }
	  else 
	    {
	      ixreg = XEXP (breg, 1);
	      scale = INTVAL (XEXP (breg, 0));
	      breg = 0;
	    }
	  goto PRINT_MEM;
	}

      /* GET_CODE (breg) must be PLUS here. */
      xtmp0 = XEXP (breg, 0);
      xtmp1 = XEXP (breg, 1);
      if (REG_CODE_BASE_P (xtmp0)) 
	{
	  breg = xtmp0;
	  xtmp0 = xtmp1;
	}
      else 
	{
	  breg = xtmp1;
	  /* xtmp0 = xtmp0; */
	}

      if (GET_CODE (xtmp0) == MULT) 
	{
	  if (REG_CODE_INDEX_P (XEXP (xtmp0, 0))) 
	    {
	      ixreg = XEXP (xtmp0, 0);
	      scale = INTVAL (XEXP (xtmp0, 1));
	    }
	  else 
	    {
	      ixreg = XEXP (xtmp0, 1);
	      scale = INTVAL (XEXP (xtmp0, 0));
	    }
	}
      else 
	{
	  ixreg = xtmp0;
	  scale = 1;
	}
      goto PRINT_MEM;

    NOT_DISP:
      if (REG_CODE_BASE_P (xtmp0)) 
	{
	  breg = xtmp0;
	  xtmp0 = xtmp1;
	}
      else if (REG_CODE_BASE_P (xtmp1)) 
	{
	  breg = xtmp1;
	  /* xtmp0 = xtmp0; */
	}
      else
	goto NOT_BASE;
    
      if (REG_CODE_INDEX_P (xtmp0)) 
	{
	  ixreg = xtmp0;
	  scale = 1;
	  goto PRINT_MEM;
	}
      else if (CONSTANT_ADDRESS_P (xtmp0)) 
	{
	  offset = xtmp0;
	  goto PRINT_MEM;
	}
      else if (GET_CODE (xtmp0) == MULT) 
	{
	  if (REG_CODE_INDEX_P (XEXP (xtmp0, 0))) 
	    {
	      ixreg = XEXP (xtmp0, 0);
	      scale = INTVAL (XEXP (xtmp0, 1));
	    }
	  else 
	    {
	      ixreg = XEXP (xtmp0, 1);
	      scale = INTVAL (XEXP (xtmp0, 0));
	    }
	  goto PRINT_MEM;
	}

      /* GET_CODE (xtmp0) must be PLUS. */
      xtmp1 = XEXP (xtmp0, 1);
      xtmp0 = XEXP (xtmp0, 0);

      if (CONSTANT_ADDRESS_P (xtmp0)) 
	{
	  offset = xtmp0;
	  xtmp0 = xtmp1;
	}
      else 
	{
	  offset = xtmp1;
	  /* xtmp0 = xtmp0; */
	}

      if (REG_CODE_INDEX_P (xtmp0)) 
	{
	  ixreg = xtmp0;
	}
      else 
	{			/* GET_CODE (xtmp0) must be MULT. */
	  if (REG_CODE_INDEX_P (XEXP (xtmp0, 0))) 
	    {
	      ixreg = XEXP (xtmp0, 0);
	      scale = INTVAL (XEXP (xtmp0, 1));
	    }
	  else 
	    {
	      ixreg = XEXP (xtmp0, 1);
	      scale = INTVAL (XEXP (xtmp0, 0));
	    }
	}
      goto PRINT_MEM;

    NOT_BASE:
      if (GET_CODE (xtmp0) == PLUS) 
	{
	  ixreg = xtmp1;
	  /* xtmp0 = xtmp0; */
	}
      else 
	{
	  ixreg = xtmp0;
	  xtmp0 = xtmp1;
	}

      if (REG_CODE_INDEX_P (ixreg)) 
	{
	  scale = 1;
	}
      else if (REG_CODE_INDEX_P (XEXP (ixreg, 0))) 
	{
	  scale = INTVAL (XEXP (ixreg, 1));
	  ixreg = XEXP (ixreg, 0);
	}
      else 
	{			/* was else if with no condition. OK ??? */
	  scale = INTVAL (XEXP (ixreg, 0));
	  ixreg = XEXP (ixreg, 1);
	}

      if (REG_CODE_BASE_P (XEXP (xtmp0, 0))) 
	{
	  breg = XEXP (xtmp0, 0);
	  offset = XEXP (xtmp0, 1);
	}
      else 
	{
	  breg = XEXP (xtmp0, 1);
	  offset = XEXP (xtmp0, 0);
	}

    PRINT_MEM:
      if (breg == 0 && ixreg == 0) 
	{
	  output_address (offset);
	  break;
	}
      else if (ixreg == 0 && offset == 0) 
	{
	  fprintf (file, "%s", reg_names[REGNO (breg)]);
	  break;
	}
      else 
	{
	  fprintf (file, "(");
	  if (offset != 0) 
	    {
	      output_addr_const (file, offset);
	      needcomma = 1;
	    }
	  if (breg != 0) 
	    {
	      if (needcomma)
		fprintf (file, ",");
	      fprintf (file, "%s", reg_names[REGNO (breg)]);
	      needcomma = 1;
	    }
	  if (ixreg != 0) 
	    {
	      if (needcomma)
		fprintf (file, ",");
	      fprintf (file, "%s", reg_names[REGNO (ixreg)]);
	      if (scale != 1)
		fprintf (file,"*%d", scale);
	    }
	  fprintf (file, ")");

	  break;
	}

    default:
      output_addr_const (file, addr);
    }
}



/* Return a REG that occurs in ADDR with coefficient 1.
   ADDR can be effectively incremented by incrementing REG.  */

static rtx
find_addr_reg (addr)
     rtx addr;
{
  while (GET_CODE (addr) == PLUS)
    {
      if (GET_CODE (XEXP (addr, 0)) == REG)
	addr = XEXP (addr, 0);
      else if (GET_CODE (XEXP (addr, 1)) == REG)
	addr = XEXP (addr, 1);
      else if (GET_CODE (XEXP (addr, 0)) == PLUS)
	addr = XEXP (addr, 0);
      else if (GET_CODE (XEXP (addr, 1)) == PLUS)
	addr = XEXP (addr, 1);
    }
  if (GET_CODE (addr) == REG)
    return addr;
  return 0;
}


    /* Return the best assembler insn template
    for moving operands[1] into operands[0] as a fullword.  */

static char *
singlemove_string (operands)
     rtx *operands;
{
  if (FPU_REG_P (operands[0]) || FPU_REG_P (operands[1])) 
    {
      if (GREG_P (operands[0]) || GREG_P (operands[1])) 
	{
	  myabort (101);	/* Not Supported yet !! */
	}
      else 
	{
	  return "fmov.s %1,%0";
	}
    }
  return "mov.w %1,%0";
}


/* Output assembler code to perform a doubleword move insn
   with operands OPERANDS.  */

char *
output_move_double (operands)
     rtx *operands;
{
  enum 
    { REGOP, OFFSOP, MEMOP, PUSHOP, POPOP, CNSTOP, RNDOP }
  optype0, optype1;
  rtx latehalf[2];
  rtx addreg0 = 0, addreg1 = 0;

  /* First classify both operands.  */

  if (REG_P (operands[0]))
    optype0 = REGOP;
  else if (offsettable_memref_p (operands[0]))
    optype0 = OFFSOP;
  else if (GET_CODE (XEXP (operands[0], 0)) == POST_INC)
    optype0 = POPOP;
  else if (GET_CODE (XEXP (operands[0], 0)) == PRE_DEC)
    optype0 = PUSHOP;
  else if (GET_CODE (operands[0]) == MEM)
    optype0 = MEMOP;
  else
    optype0 = RNDOP;

  if (REG_P (operands[1]))
    optype1 = REGOP;
  else if (CONSTANT_P (operands[1]))
    optype1 = CNSTOP;
  else if (offsettable_memref_p (operands[1]))
    optype1 = OFFSOP;
  else if (GET_CODE (XEXP (operands[1], 0)) == POST_INC)
    optype1 = POPOP;
  else if (GET_CODE (XEXP (operands[1], 0)) == PRE_DEC)
    optype1 = PUSHOP;
  else if (GET_CODE (operands[1]) == MEM)
    optype1 = MEMOP;
  else
    optype1 = RNDOP;

  /* Check for the cases that the operand constraints are not
     supposed to allow to happen.  Abort if we get one,
     because generating code for these cases is painful.  */

  if (optype0 == RNDOP || optype1 == RNDOP)
    myabort (102);

  /* If one operand is decrementing and one is incrementing
     decrement the former register explicitly
     and change that operand into ordinary indexing.  */

  if (optype0 == PUSHOP && optype1 == POPOP)
    {
      operands[0] = XEXP (XEXP (operands[0], 0), 0);
      output_asm_insn ("sub.w %#8,%0", operands);
      operands[0] = gen_rtx (MEM, DImode, operands[0]);
      optype0 = OFFSOP;
    }
  if (optype0 == POPOP && optype1 == PUSHOP)
    {
      operands[1] = XEXP (XEXP (operands[1], 0), 0);
      output_asm_insn ("sub.w %#8,%1", operands);
      operands[1] = gen_rtx (MEM, DImode, operands[1]);
      optype1 = OFFSOP;
    }

  /* If an operand is an unoffsettable memory ref, find a register
     we can increment temporarily to make it refer to the second word.  */

  if (optype0 == MEMOP)
    addreg0 = find_addr_reg (operands[0]);

  if (optype1 == MEMOP)
    addreg1 = find_addr_reg (operands[1]);

  /* Ok, we can do one word at a time.
     Normally we do the low-numbered word first,
     but if either operand is autodecrementing then we
     do the high-numbered word first.
     
     In either case, set up in LATEHALF the operands to use
     for the high-numbered word and in some cases alter the
     operands in OPERANDS to be suitable for the low-numbered word.  */

  if (optype0 == REGOP)
    latehalf[0] = gen_rtx (REG, SImode, REGNO (operands[0]) + 1);
  else if (optype0 == OFFSOP)
    latehalf[0] = adj_offsettable_operand (operands[0], 4);
  else
    latehalf[0] = operands[0];

  if (optype1 == REGOP)
    latehalf[1] = gen_rtx (REG, SImode, REGNO (operands[1]) + 1);
  else if (optype1 == OFFSOP)
    latehalf[1] = adj_offsettable_operand (operands[1], 4);
  else if (optype1 == CNSTOP)
    {
      if (GET_CODE (operands[1]) == CONST_DOUBLE)
	split_double (operands[1], &operands[1], &latehalf[1]);
      else if (CONSTANT_P (operands[1]))
	latehalf[1] = const0_rtx;
    }
  else
    latehalf[1] = operands[1];

  /* If insn is effectively movd N(sp),-(sp) then we will do the
     high word first.  We should use the adjusted operand 1 (which is N+4(sp))
     for the low word as well, to compensate for the first decrement of sp.  */
  if (optype0 == PUSHOP
      && REGNO (XEXP (XEXP (operands[0], 0), 0)) == STACK_POINTER_REGNUM
      && reg_overlap_mentioned_p (stack_pointer_rtx, operands[1]))
    operands[1] = latehalf[1];

  /* If one or both operands autodecrementing,
     do the two words, high-numbered first.  */

  /* Likewise,  the first move would clobber the source of the second one,
     do them in the other order.  This happens only for registers;
     such overlap can't happen in memory unless the user explicitly
     sets it up, and that is an undefined circumstance.  */

  if (optype0 == PUSHOP || optype1 == PUSHOP
      || (optype0 == REGOP && optype1 == REGOP
	  && REGNO (operands[0]) == REGNO (latehalf[1])))
    {
      /* Make any unoffsettable addresses point at high-numbered word.  */
      if (addreg0)
	output_asm_insn ("add.w %#4,%0", &addreg0);
      if (addreg1)
	output_asm_insn ("add.w %#4,%0", &addreg1);

      /* Do that word.  */
      output_asm_insn (singlemove_string (latehalf), latehalf);

      /* Undo the adds we just did.  */
      if (addreg0)
	output_asm_insn ("sub.w %#4,%0", &addreg0);
      if (addreg1)
	output_asm_insn ("sub.w %#4,%0", &addreg1);

      /* Do low-numbered word.  */
      return singlemove_string (operands);
    }

  /* Normal case: do the two words, low-numbered first.  */

  output_asm_insn (singlemove_string (operands), operands);

  /* Make any unoffsettable addresses point at high-numbered word.  */
  if (addreg0)
    output_asm_insn ("add.w %#4,%0", &addreg0);
  if (addreg1)
    output_asm_insn ("add.w %#4,%0", &addreg1);

  /* Do that word.  */
  output_asm_insn (singlemove_string (latehalf), latehalf);

  /* Undo the adds we just did.  */
  if (addreg0)
    output_asm_insn ("sub.w %#4,%0", &addreg0);
  if (addreg1)
    output_asm_insn ("sub.w %#4,%0", &addreg1);

  return "";
}

/* Move const_double to floating point register (DF) */
char *
output_move_const_double (operands)
     rtx *operands;
{
  int code = standard_fpu_constant_p (operands[1]);

  if (FPU_REG_P (operands[0])) 
    {
      if (code != 0)
	{
	  static char buf[40];

	  sprintf (buf, "fmvr from%d,%%0.d", code);
	  return buf;
	}
      else 
	{
	  return "fmov %1,%0.d";
	}
    }
  else if (GREG_P (operands[0])) 
    {
      rtx xoperands[2];
      xoperands[0] = gen_rtx (REG, SImode, REGNO (operands[0]) + 1);
      xoperands[1] = gen_rtx (CONST_INT, VOIDmode,
			      CONST_DOUBLE_HIGH (operands[1]));
      output_asm_insn ("mov.w %1,%0", xoperands);
      operands[1] = gen_rtx (CONST_INT, VOIDmode,
			     CONST_DOUBLE_LOW (operands[1]));
      return "mov.w %1,%0";
    }
  else 
    {
      return output_move_double (operands); /* ?????? */
    }
}

char *
output_move_const_single (operands)
     rtx *operands;
{
  int code = standard_fpu_constant_p (operands[1]);
  static char buf[40];

  if (FPU_REG_P (operands[0])) 
    {
      if (code != 0)
	{
	  sprintf (buf, "fmvr from%d,%%0.s", code);
	  return buf;
	}
      return "fmov.s %f1,%0";
    }
  else 
    return "mov.w %f1,%0";
}


/* Return nonzero if X, a CONST_DOUBLE, has a value that we can get
   from the "fmvr" instruction of the Gmicro FPU.
   The value, anded with 0xff, gives the code to use in fmovecr
   to get the desired constant.  */

  u.i[0] = CONST_DOUBLE_LOW (x);
  u.i[1] = CONST_DOUBLE_HIGH (x);
  d = u.d;

  if (d == 0.0)			/* +0.0 */
    return 0x0;
  /* Note: there are various other constants available
     but it is a nuisance to put in their values here.  */
  if (d == 1.0)			/* +1.0 */
    return 0x1;

  /*
   * Stuff that looks different if it's single or double
   */
  if (GET_MODE (x) == SFmode)
    {
      if (d == S_PI)
	return 0x2;
      if (d == (S_PI / 2.0))
	return 0x3;
      if (d == S_E)
	return 0x4;
      if (d == S_LOGEof2)
	return 0x5;
      if (d == S_LOGEof10)
	return 0x6;
      if (d == S_LOG10of2)
	return 0x7;
      if (d == S_LOG10ofE)
	return 0x8;
      if (d == S_LOG2ofE)
	return 0x9;
    }
  else
    {
      if (d == D_PI)
	return 0x2;
      if (d == (D_PI / 2.0))
	return 0x3;
      if (d == D_E)
	return 0x4;
      if (d == D_LOGEof2)
	return 0x5;
      if (d == D_LOGEof10)
	return 0x6;
      if (d == D_LOG10of2)
	return 0x7;
      if (d == D_LOG10ofE)
	return 0x8;
      if (d == D_LOG2ofE)
	return 0x9;
    }

  return 0;
}

#undef S_PI
#undef D_PI
#undef S_E
#undef D_E
#undef S_LOGEof2
#undef D_LOGEof2
#undef S_LOGEof10
#undef D_LOGEof10
#undef S_LOG10of2
#undef D_LOG10of2
#undef S_LOG10ofE
#undef D_LOG10ofE
#undef S_LOG2ofE
#undef D_LOG2ofE

/* dest should be operand 0 */
/* imm should be operand 1 */

extern char *sub_imm_word ();

char *
add_imm_word (imm, dest, immp)
     int imm;
     rtx dest, *immp;
{
  int is_reg, short_ok;


  if (imm < 0) 
    {
      *immp = gen_rtx (CONST_INT, VOIDmode, -imm);
      return sub_imm_word (-imm, dest);
    }
    
  if (imm == 0)
    return "mov:l.w #0,%0";
    
  short_ok = short_format_ok (dest);

  if (short_ok && imm <= 8)
    return "add:q %1,%0.w";

  if (imm < 128)
    return "add:e %1,%0.w";

  is_reg = (GET_CODE (dest) == REG);

  if (is_reg)
    return "add:l %1,%0.w";
    
  if (short_ok)
    return "add:i %1,%0.w";
    
  return "add %1,%0.w";
}

char *
sub_imm_word (imm, dest, immp)
     int imm;
     rtx dest, *immp;
{
  int is_reg, short_ok;

  if (imm < 0 &&  imm != 0x80000000) 
    {
      *immp = gen_rtx (CONST_INT, VOIDmode, -imm);
      return add_imm_word (-imm, dest);
    }
    
  if (imm == 0)
    return "mov:z.w #0,%0";
    
  short_ok = short_format_ok (dest);

  if (short_ok && imm <= 8)
    return "sub:q %1,%0.w";

  if (imm < 128)
    return "sub:e %1,%0.w";

  is_reg = (GET_CODE (dest) == REG);

  if (is_reg)
    return "sub:l %1,%0.w";
    
  if (short_ok)
    return "sub:i %1,%0.w";
    
  return "sub %1,%0.w";
}

int
short_format_ok (x)
     rtx x;
{
  rtx x0, x1;

  if (GET_CODE (x) == REG)
    return 1;

  if (GET_CODE (x) == MEM 
      && GET_CODE (XEXP (x, 0)) == PLUS) 
    {
      x0 = XEXP (XEXP (x, 0), 0);
      x1 = XEXP (XEXP (x, 0), 1);
      return ((GET_CODE (x0) == REG
	       && CONSTANT_P (x1)
	       && ((unsigned) (INTVAL (x1) + 0x8000)  < 0x10000))
	      ||
	      (GET_CODE (x1) == REG
	       && CONSTANT_P (x0)
	       && ((unsigned) (INTVAL (x0) + 0x8000)  < 0x10000)));
    }

  return 0;
}

myoutput_sp_adjust (file, op, fsize)
     FILE *file;
     char *op;
     int fsize;
{
  if (fsize == 0)
    ;
  else if (fsize < 8)
    fprintf (file, "\t%s:q #%d,sp.w\n", op, fsize);
  else if (fsize < 128)
    fprintf (file, "\t%s:e #%d,sp.w\n", op, fsize);
  else
    fprintf (file, "\t%s:l #%d,sp.w\n", op, fsize);
}


char *
mov_imm_word (imm, dest)
     int imm;
     rtx dest;
{
  int is_reg, short_ok;

  if (imm == 0)
    return "mov:z.w #0,%0";
    
  short_ok = short_format_ok (dest);

  if (short_ok && imm > 0 && imm <= 8)
    return "mov:q %1,%0.w";

  if (-128 <= imm && imm < 128)
    return "mov:e %1,%0.w";

  is_reg = (GET_CODE (dest) == REG);

  if (is_reg)
    return "mov:l %1,%0.w";
    
  if (short_ok)
    return "mov:i %1,%0.w";
    
  return "mov %1,%0.w";
}

char *
cmp_imm_word (imm, dest)
     int imm;
     rtx dest;
{
  int is_reg, short_ok;

  if (imm == 0)
    return "cmp:z.w #0,%0";
    
  short_ok = short_format_ok (dest);

  if (short_ok && imm >0 && imm <= 8)
    return "cmp:q %1,%0.w";

  if (-128 <= imm && imm < 128)
    return "cmp:e %1,%0.w";

  is_reg = (GET_CODE (dest) == REG);

  if (is_reg)
    return "cmp:l %1,%0.w";
    
  if (short_ok)
    return "cmp:i %1,%0.w";
    
  return "cmp %1,%0.w";
}

char *
push_imm_word (imm)
     int imm;
{
  if (imm == 0)
    return "mov:z.w #0,%-";
    
  if (imm > 0 && imm <= 8)
    return "mov:q %1,%-.w";

  if (-128 <= imm && imm < 128)
    return "mov:e %1,%-.w";

  return "mov:g %1,%-.w";
    
  /* In some cases, g-format may be better than I format.??
     return "mov %1,%0.w";
     */
}

my_signed_comp (insn)
     rtx insn;
{
  rtx my_insn;

  my_insn = NEXT_INSN (insn);
  if (GET_CODE (my_insn) != JUMP_INSN) 
    {
      fprintf (stderr, "my_signed_comp: Not Jump_insn ");
      myabort (GET_CODE (my_insn));
    }
  my_insn = PATTERN (my_insn);
  if (GET_CODE (my_insn) != SET) 
    {
      fprintf (stderr, "my_signed_comp: Not Set ");
      myabort (GET_CODE (my_insn));
    }
  my_insn = SET_SRC (my_insn);
  if (GET_CODE (my_insn) != IF_THEN_ELSE) 
    {
      fprintf (stderr, "my_signed_comp: Not if_then_else ");
      myabort (GET_CODE (my_insn));
    }
  switch (GET_CODE (XEXP (my_insn, 0)))
    {
    case NE:
    case EQ:
    case GE:
    case GT:
    case LE:
    case LT:
      return 1;
    case GEU:
    case GTU:
    case LEU:
    case LTU:
      return 0;
    }
  fprintf (stderr, "my_signed_comp: Not cccc ");
  myabort (GET_CODE (XEXP (my_insn, 0)));
}
