/* Subroutines for insn-output.c for SPUR.  Adapted from routines for
   the Motorola 68000 family.
   Copyright (C) 1988, 1991 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

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

static rtx find_addr_reg ();

char *
output_compare (operands, opcode, exchange_opcode, 
		neg_opcode, neg_exchange_opcode)
     rtx *operands;
     char *opcode;
     char *exchange_opcode;
     char *neg_opcode;
     char *neg_exchange_opcode;
{
  static char buf[100];
  operands[2] = operands[0];
  if (GET_CODE (cc_prev_status.value1) == CONST_INT)
    {
      operands[1] = cc_prev_status.value1;
      operands[0] = cc_prev_status.value2;
      opcode = exchange_opcode, neg_opcode = neg_exchange_opcode;
    }
  else
    {
      operands[0] = cc_prev_status.value1;
      operands[1] = cc_prev_status.value2;
    }
  if (TARGET_LONG_JUMPS)
    sprintf (buf,
	     "cmp_br_delayed %s,%%0,%%1,1f\n\tnop\n\tjump %%l2\n\tnop\n1:",
	     neg_opcode);
  else 
    sprintf (buf, "cmp_br_delayed %s,%%0,%%1,%%l2\n\tnop", opcode);
  return buf;
}

/* Return the best assembler insn template
   for moving operands[1] into operands[0] as a fullword.  */

static char *
singlemove_string (operands)
     rtx *operands;
{
  if (GET_CODE (operands[0]) == MEM)
    return "st_32 %r1,%0";
  if (GET_CODE (operands[1]) == MEM)
    return "ld_32 %0,%1\n\tnop";
  if (GET_CODE (operands[1]) == REG)
    return "add_nt %0,%1,$0";
  return "add_nt %0,r0,%1";
}

/* Output assembler code to perform a doubleword move insn
   with operands OPERANDS.  */

char *
output_move_double (operands)
     rtx *operands;
{
  enum { REGOP, OFFSOP, MEMOP, PUSHOP, POPOP, CNSTOP, RNDOP } optype0, optype1;
  rtx latehalf[2];
  rtx addreg0 = 0, addreg1 = 0;

  /* First classify both operands.  */

  if (REG_P (operands[0]))
    optype0 = REGOP;
  else if (offsettable_memref_p (operands[0]))
    optype0 = OFFSOP;
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
  else if (GET_CODE (operands[1]) == MEM)
    optype1 = MEMOP;
  else
    optype1 = RNDOP;

  /* Check for the cases that the operand constraints are not
     supposed to allow to happen.  Abort if we get one,
     because generating code for these cases is painful.  */

  if (optype0 == RNDOP || optype1 == RNDOP)
    abort ();

  /* If an operand is an unoffsettable memory ref, find a register
     we can increment temporarily to make it refer to the second word.  */

  if (optype0 == MEMOP)
    addreg0 = find_addr_reg (XEXP (operands[0], 0));

  if (optype1 == MEMOP)
    addreg1 = find_addr_reg (XEXP (operands[1], 0));

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
	{
	  latehalf[1] = gen_rtx (CONST_INT, VOIDmode,
				 CONST_DOUBLE_HIGH (operands[1]));
	  operands[1] = gen_rtx (CONST_INT, VOIDmode,
				 CONST_DOUBLE_LOW (operands[1]));
	}
      else if (CONSTANT_P (operands[1]))
	latehalf[1] = const0_rtx;
    }
  else
    latehalf[1] = operands[1];

  /* If the first move would clobber the source of the second one,
     do them in the other order.  This happens only for registers;
     such overlap can't happen in memory unless the user explicitly
     sets it up, and that is an undefined circumstance.  */

  if (optype0 == REGOP && optype1 == REGOP
      && REGNO (operands[0]) == REGNO (latehalf[1]))
    {
      /* Make any unoffsettable addresses point at high-numbered word.  */
      if (addreg0)
	output_asm_insn ("add_nt %0,%0,$4", &addreg0);
      if (addreg1)
	output_asm_insn ("add_nt %0,%0,$4", &addreg1);

      /* Do that word.  */
      output_asm_insn (singlemove_string (latehalf), latehalf);

      /* Undo the adds we just did.  */
      if (addreg0)
	output_asm_insn ("add_nt %0,%0,$-4", &addreg0);
      if (addreg1)
	output_asm_insn ("add_nt %0,%0,$-4", &addreg0);

      /* Do low-numbered word.  */
      return singlemove_string (operands);
    }

  /* Normal case: do the two words, low-numbered first.  */

  output_asm_insn (singlemove_string (operands), operands);

  /* Make any unoffsettable addresses point at high-numbered word.  */
  if (addreg0)
    output_asm_insn ("add_nt %0,%0,$4", &addreg0);
  if (addreg1)
    output_asm_insn ("add_nt %0,%0,$4", &addreg1);

  /* Do that word.  */
  output_asm_insn (singlemove_string (latehalf), latehalf);

  /* Undo the adds we just did.  */
  if (addreg0)
    output_asm_insn ("add_nt %0,%0,$-4", &addreg0);
  if (addreg1)
    output_asm_insn ("add_nt %0,%0,$-4", &addreg1);

  return "";
}

static char *
output_fp_move_double (operands)
     rtx *operands;
{
  if (FP_REG_P (operands[0]))
    {
      if (FP_REG_P (operands[1]))
	return "fmov %0,%1";
      if (GET_CODE (operands[1]) == REG)
	{
	  rtx xoperands[2];
	  int offset = - get_frame_size () - 8;
	  xoperands[1] = gen_rtx (REG, SImode, REGNO (operands[1]) + 1);
	  xoperands[0] = gen_rtx (CONST_INT, VOIDmode, offset + 4);
	  output_asm_insn ("st_32 %1,r25,%0", xoperands);
	  xoperands[1] = operands[1];
	  xoperands[0] = gen_rtx (CONST_INT, VOIDmode, offset);
	  output_asm_insn ("st_32 %1,r25,%0", xoperands);
	  xoperands[1] = operands[0];
	  output_asm_insn ("ld_dbl %1,r25,%0\n\tnop", xoperands);
	  return "";
	}
      return "ld_dbl %0,%1\n\tnop";
    }
  else if (FP_REG_P (operands[1]))
    {
      if (GET_CODE (operands[0]) == REG)
	{
	  rtx xoperands[2];
	  int offset = - get_frame_size () - 8;
	  xoperands[0] = gen_rtx (CONST_INT, VOIDmode, offset);
	  xoperands[1] = operands[1];
	  output_asm_insn ("st_dbl %1,r25,%0", xoperands);
	  xoperands[1] = operands[0];
	  output_asm_insn ("ld_32 %1,r25,%0\n\tnop", xoperands);
	  xoperands[1] = gen_rtx (REG, SImode, REGNO (operands[0]) + 1);
	  xoperands[0] = gen_rtx (CONST_INT, VOIDmode, offset + 4);
	  output_asm_insn ("ld_32 %1,r25,%0\n\tnop", xoperands);
	  return "";
	}
      return "st_dbl %1,%0";
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
      else if (CONSTANT_P (XEXP (addr, 0)))
	addr = XEXP (addr, 1);
      else if (CONSTANT_P (XEXP (addr, 1)))
	addr = XEXP (addr, 0);
      else
	abort ();
    }
  if (GET_CODE (addr) == REG)
    return addr;
  abort ();
}

/* Generate code to add a large integer constant to register, reg, storing
 * the result in a register, target.  Offset must be 27-bit signed quantity */

static char *
output_add_large_offset (target, reg, offset)
     rtx target, reg;
     int offset;
{
  rtx operands[3];
  int high, n, i;
  operands[0] = target, operands[1] = reg;
    
  for (high = offset, n = 0; 
       (unsigned) (high + 0x2000) >= 0x4000; 
       high >>= 1, n += 1)
    ;
  operands[2] = gen_rtx (CONST_INT, VOIDmode, high);
  output_asm_insn ("add_nt r2,r0,%2", operands);
  i = n;
  while (i >= 3)
    output_asm_insn ("sll r2,r2,$3", operands), i -= 3;
  if (i == 2) 
    output_asm_insn ("sll r2,r2,$2", operands);
  else if (i == 1)
    output_asm_insn ("sll r2,r2,$1", operands);
  output_asm_insn ("add_nt %0,r2,%1", operands);
  if (offset - (high << n) != 0)
    {
      operands[2] = gen_rtx (CONST_INT, VOIDmode, offset - (high << n));
      output_asm_insn ("add_nt %0,%0,%2", operands);
    }
  return "";
}

/* Additional TESTFN for matching. Like immediate_operand, but matches big
 * constants */

int
big_immediate_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_INT);
}
