/* Subroutines for insn-output.c for Tahoe.
   Copyright (C) 1989, 1991, 1997 Free Software Foundation, Inc.

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
#include <stdio.h>
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-flags.h"
#include "output.h"
#include "insn-attr.h"

/*
 * File: output-tahoe.c
 *
 * Original port made at the University of Buffalo by Devon Bowen,
 * Dale Wiles and Kevin Zachmann.
 *
 * Changes for HCX by Piet van Oostrum,
 * University of Utrecht, The Netherlands (piet@cs.ruu.nl)
 *
 * Speed tweaks by Michael Tiemann (tiemann@lurch.stanford.edu).
 *
 * Mail bugs reports or fixes to:	gcc@cs.buffalo.edu
 */


/* On tahoe, you have to go to memory to convert a register
   from sub-word to word.  */

rtx tahoe_reg_conversion_loc;

int
extensible_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if ((GET_CODE (op) == REG
       || (GET_CODE (op) == SUBREG
	   && GET_CODE (SUBREG_REG (op)) == REG))
      && tahoe_reg_conversion_loc == 0)
    tahoe_reg_conversion_loc = assign_stack_local (SImode, GET_MODE_SIZE (SImode));
  return general_operand (op, mode);
}

/* most of the print_operand_address function was taken from the vax	*/
/* since the modes are basically the same. I had to add a special case,	*/
/* though, for symbol references with offsets.				*/

print_operand_address (file, addr)
     FILE *file;
     register rtx addr;
{
  register rtx reg1, reg2, breg, ireg;
  rtx offset;
  static char *reg_name[] = REGISTER_NAMES;

 retry:
  switch (GET_CODE (addr))
    {
    case MEM:
      fprintf (file, "*");
      addr = XEXP (addr, 0);
      goto retry;

    case REG:
      fprintf (file, "(%s)", reg_name [REGNO (addr)]);
      break;

    case PRE_DEC:
      fprintf (file, "-(%s)", reg_name [REGNO (XEXP (addr, 0))]);
      break;

    case POST_INC:
      fprintf (file, "(%s)+", reg_name [REGNO (XEXP (addr, 0))]);
      break;

    case PLUS:
      reg1 = 0;	reg2 = 0;
      ireg = 0;	breg = 0;
      offset = 0;

      if (CONSTANT_ADDRESS_P (XEXP (addr, 0))
	  && GET_CODE (XEXP (addr, 1)) == CONST_INT)
	output_addr_const (file, addr);

      if (CONSTANT_ADDRESS_P (XEXP (addr, 1))
	  && GET_CODE (XEXP (addr, 0)) == CONST_INT)
	output_addr_const (file, addr);

      if (CONSTANT_ADDRESS_P (XEXP (addr, 0))
	  || GET_CODE (XEXP (addr, 0)) == MEM)
	{
	  offset = XEXP (addr, 0);
	  addr = XEXP (addr, 1);
	}
      else if (CONSTANT_ADDRESS_P (XEXP (addr, 1))
	       || GET_CODE (XEXP (addr, 1)) == MEM)
	{
	  offset = XEXP (addr, 1);
	  addr = XEXP (addr, 0);
	}
      if (GET_CODE (addr) != PLUS)
	;
      else if (GET_CODE (XEXP (addr, 0)) == MULT)
	{
	  reg1 = XEXP (addr, 0);
	  addr = XEXP (addr, 1);
	}
      else if (GET_CODE (XEXP (addr, 1)) == MULT)
	{
	  reg1 = XEXP (addr, 1);
	  addr = XEXP (addr, 0);
	}
      else if (GET_CODE (XEXP (addr, 0)) == REG)
	{
	  reg1 = XEXP (addr, 0);
	  addr = XEXP (addr, 1);
	}
      else if (GET_CODE (XEXP (addr, 1)) == REG)
	{
	  reg1 = XEXP (addr, 1);
	  addr = XEXP (addr, 0);
	}
      if (GET_CODE (addr) == REG || GET_CODE (addr) == MULT)
	{
	  if (reg1 == 0)
	    reg1 = addr;
	  else
	    reg2 = addr;
	  addr = 0;
	}
      if (offset != 0)
	{
	  if (addr != 0) abort ();
	  addr = offset;
	}
      if (reg1 != 0 && GET_CODE (reg1) == MULT)
	{
	  breg = reg2;
	  ireg = reg1;
	}
      else if (reg2 != 0 && GET_CODE (reg2) == MULT)
	{
	  breg = reg1;
	  ireg = reg2;
	}
      else if (reg2 != 0 || GET_CODE (addr) == MEM)
	{
	  breg = reg2;
	  ireg = reg1;
	}
      else
	{
	  breg = reg1;
	  ireg = reg2;
	}
      if (addr != 0)
	output_address (offset);
      if (breg != 0)
	{
	  if (GET_CODE (breg) != REG)
	    abort ();
	  fprintf (file, "(%s)", reg_name[REGNO (breg)]);
	}
      if (ireg != 0)
	{
	  if (GET_CODE (ireg) == MULT)
	    ireg = XEXP (ireg, 0);
	  if (GET_CODE (ireg) != REG)
	    abort ();
	  fprintf (file, "[%s]", reg_name[REGNO (ireg)]);
	}
      break;

    default:
      output_addr_const (file, addr);
    }
}

/* Do a quick check and find out what the best way to do the */
/* mini-move is. Could be a push or a move.....		     */

static char *
singlemove_string (operands)
     rtx *operands;
{
  if (operands[1] == const0_rtx)
      return "clrl %0";
  if (push_operand (operands[0], SImode))
    return "pushl %1";
  return "movl %1,%0";
}

/* given the rtx for an address, return true if the given */
/* register number is used in the address somewhere.	  */

regisused(addr,regnum)
rtx addr;
int regnum;
{
	if (GET_CODE(addr) == REG)
		if (REGNO(addr) == regnum)
			return (1);
		else
			return (0);

	if (GET_CODE(addr) == MEM)
		return regisused(XEXP(addr,0),regnum);

	if ((GET_CODE(addr) == MULT) || (GET_CODE(addr) == PLUS))
		return ((regisused(XEXP(addr,0),regnum)) ||
					(regisused(XEXP(addr,1),regnum)));

	return 0;
}


/* Given some rtx, traverse it and return the register used in a */
/* index. If no index is found, return 0.			 */

rtx
index_reg(addr)
rtx addr;
{
	rtx temp;

	if (GET_CODE(addr) == MEM)
		return index_reg(XEXP(addr,0));

	if (GET_CODE(addr) == MULT)
		if (GET_CODE(XEXP(addr,0)) == REG)
			return XEXP(addr,0);
		else
			return XEXP(addr,1);

	if (GET_CODE(addr) == PLUS)
		if (temp = index_reg(XEXP(addr,0)))
			return temp;
		else
			return index_reg(XEXP(addr,1));

	return 0;
}


/* simulate the move double by generating two movl's. You have */
/* to be careful about mixing modes here.		       */

char *
output_move_double (operands)
     rtx *operands;
{
  enum { REGOP, OFFSOP, MEMOP, PUSHOP, POPOP, INDOP, CNSTOP, RNDOP }
    optype0, optype1;
  rtx latehalf[2];
  rtx shftreg0 = 0, shftreg1 = 0;
  rtx temp0 = 0, temp1 = 0;
  rtx addreg0 = 0, addreg1 = 0;
  int dohighfirst = 0;

  /* First classify both operands. */

  if (REG_P (operands[0]))
    optype0 = REGOP;
  else if ((GET_CODE(operands[0])==MEM) && (shftreg0=index_reg(operands[0])))
    optype0 = INDOP;
  else if (offsettable_memref_p (operands[0]))
    optype0 = OFFSOP;
  else if (GET_CODE (XEXP (operands[0], 0)) == PRE_DEC) {
    optype0 = PUSHOP;
    dohighfirst++;
  } else if (GET_CODE (operands[0]) == MEM)
    optype0 = MEMOP;
  else
    optype0 = RNDOP;

  if (REG_P (operands[1]))
    optype1 = REGOP;
  else if ((GET_CODE(operands[1])==MEM) && (shftreg1=index_reg(operands[1])))
    optype1 = INDOP;
  else if (offsettable_memref_p (operands[1]))
    optype1 = OFFSOP;
  else if (GET_CODE (XEXP (operands[1], 0)) == POST_INC)
    optype1 = POPOP; 
  else if (GET_CODE (operands[1]) == MEM)
    optype1 = MEMOP;
  else if (CONSTANT_P (operands[1]))
    optype1 = CNSTOP;
  else
    optype1 = RNDOP;

  /* set up for the high byte move for operand zero */

  switch (optype0) {

	/* if it's a register, just use the next highest in the */
	/* high address move.					*/

	case REGOP  : latehalf[0] = gen_rtx (REG,SImode,REGNO(operands[0])+1);
		      break;

	/* for an offsettable address, use the gcc function to  */
	/* modify the operand to get an offset of 4 higher for  */
	/* the second move.					*/

	case OFFSOP : latehalf[0] = adj_offsettable_operand (operands[0], 4);
		      break;

	/* if the operand is MEMOP type, it must be a pointer	*/
	/* to a pointer. So just remember to increase the mem	*/
	/* location and use the same operand.			*/

	case MEMOP  : latehalf[0] = operands[0];
		      addreg0 = XEXP(operands[0],0);
		      break;

	/* if we're dealing with a push instruction, just leave */
	/* the operand alone since it auto-increments.		*/

	case PUSHOP : latehalf[0] = operands[0];
		      break;

	/* YUCK! Indexed addressing!! If the address is considered   */
	/* offsettable, go use the offset in the high part. Otherwise */
	/* find what exactly is being added to the multiplication. If */
	/* it's a mem reference, increment that with the high part   */
	/* being unchanged to cause the shift. If it's a reg, do the */
	/* same. If you can't identify it, abort. Remember that the  */
	/* shift register was already set during identification.     */

	case INDOP  : if (offsettable_memref_p(operands[0])) {
			   latehalf[0] = adj_offsettable_operand(operands[0],4);
			   break;
		      }

		      latehalf[0] = operands[0];

		      temp0 = XEXP(XEXP(operands[0],0),0);
                      if (GET_CODE(temp0) == MULT) {
			   temp1 = temp0;
			   temp0 = XEXP(XEXP(operands[0],0),1);
		      } else {
			   temp1 = XEXP(XEXP(operands[0],0),1);
			   if (GET_CODE(temp1) != MULT)
				abort();
		      }

		      if (GET_CODE(temp0) == MEM)
			   addreg0 = temp0;
		      else if (GET_CODE(temp0) == REG)
			   addreg0 = temp0;
		      else
			   abort();

		      break;

	/* if we don't know the operand type, print a friendly  */
	/* little error message...   8-)			*/

	case RNDOP  :
	default     : abort();
  }

  /* do the same setup for operand one */

  switch (optype1) {

	case REGOP  : latehalf[1] = gen_rtx(REG,SImode,REGNO(operands[1])+1);
		      break;

	case OFFSOP : latehalf[1] = adj_offsettable_operand (operands[1], 4);
		      break;

	case MEMOP  : latehalf[1] = operands[1];
		      addreg1 = XEXP(operands[1],0);
		      break;

	case POPOP  : latehalf[1] = operands[1];
		      break;

	case INDOP  : if (offsettable_memref_p(operands[1])) {
			   latehalf[1] = adj_offsettable_operand(operands[1],4);
			   break;
		      }

		      latehalf[1] = operands[1];

		      temp0 = XEXP(XEXP(operands[1],0),0);
                      if (GET_CODE(temp0) == MULT) {
			   temp1 = temp0;
			   temp0 = XEXP(XEXP(operands[1],0),1);
		      } else {
			   temp1 = XEXP(XEXP(operands[1],0),1);
			   if (GET_CODE(temp1) != MULT)
				abort();
		      }

		      if (GET_CODE(temp0) == MEM)
			   addreg1 = temp0;
		      else if (GET_CODE(temp0) == REG)
			   addreg1 = temp0;
		      else
			   abort();

		      break;

	case CNSTOP :
	  if (GET_CODE (operands[1]) == CONST_DOUBLE)
	    split_double (operands[1], &operands[1], &latehalf[1]);
	  else if (CONSTANT_P (operands[1]))
	    latehalf[1] = const0_rtx;
	  else abort ();
	  break;

	case RNDOP  :
	default     : abort();
  }


  /* double the register used for shifting in both of the operands */
  /* but make sure the same register isn't doubled twice!	   */

  if (shftreg0 && shftreg1 && (rtx_equal_p(shftreg0,shftreg1)))
	output_asm_insn("addl2 %0,%0", &shftreg0);
  else {
	if (shftreg0)
		output_asm_insn("addl2 %0,%0", &shftreg0);
	if (shftreg1)
		output_asm_insn("addl2 %0,%0", &shftreg1);
  }

  /* if the destination is a register and that register is needed in  */
  /* the source addressing mode, swap the order of the moves since we */
  /* don't want this destroyed til last. If both regs are used, not   */
  /* much we can do, so abort. If these becomes a problem, maybe we   */
  /* can do it on the stack?					      */

  if (GET_CODE(operands[0])==REG && regisused(operands[1],REGNO(operands[0])))
	if (regisused(latehalf[1],REGNO(latehalf[0])))
		8;
	else
		dohighfirst++;

  /* if we're pushing, do the high address part first. */

  if (dohighfirst) {

	if (addreg0 && addreg1 && (rtx_equal_p(addreg0,addreg1)))
		output_asm_insn("addl2 $4,%0", &addreg0);
	else {
		if (addreg0)
			output_asm_insn("addl2 $4,%0", &addreg0);
		if (addreg1)
			output_asm_insn("addl2 $4,%0", &addreg1);
	}

	output_asm_insn(singlemove_string(latehalf), latehalf);

	if (addreg0 && addreg1 && (rtx_equal_p(addreg0,addreg1)))
		output_asm_insn("subl2 $4,%0", &addreg0);
	else {
		if (addreg0)
			output_asm_insn("subl2 $4,%0", &addreg0);
		if (addreg1)
			output_asm_insn("subl2 $4,%0", &addreg1);
	}

	return singlemove_string(operands);
  }

  output_asm_insn(singlemove_string(operands), operands);

  if (addreg0 && addreg1 && (rtx_equal_p(addreg0,addreg1)))
	output_asm_insn("addl2 $4,%0", &addreg0);
  else {
	if (addreg0)
		output_asm_insn("addl2 $4,%0", &addreg0);
	if (addreg1)
		output_asm_insn("addl2 $4,%0", &addreg1);
  }

  output_asm_insn(singlemove_string(latehalf), latehalf);

  if (addreg0 && addreg1 && (rtx_equal_p(addreg0,addreg1)))
	output_asm_insn("subl2 $4,%0", &addreg0);
  else {
	if (addreg0)
		output_asm_insn("subl2 $4,%0", &addreg0);
	if (addreg1)
		output_asm_insn("subl2 $4,%0", &addreg1);
  }

  if (shftreg0 && shftreg1 && (rtx_equal_p(shftreg0,shftreg1)))
	output_asm_insn("shar $1,%0,%0", &shftreg0);
  else {
	if (shftreg0)
		output_asm_insn("shar $1,%0,%0", &shftreg0);
	if (shftreg1)
		output_asm_insn("shar $1,%0,%0", &shftreg1);
  }

  return "";
}


/* This checks if a zero_extended cmp[bw] can be replaced by a sign_extended
   cmp[bw]. This can be done if the operand is a constant that fits in a
   byte/word or a memory operand. Besides that the next instruction must be an
   unsigned compare. Some of these tests are done by the machine description */

int
tahoe_cmp_check (insn, op, max)
rtx insn, op; int max;
{
    if (GET_CODE (op) == CONST_INT
	&& ( INTVAL (op) < 0 || INTVAL (op) > max ))
	return 0;
    {
	register rtx next = NEXT_INSN (insn);

	if ((GET_CODE (next) == JUMP_INSN
	   || GET_CODE (next) == INSN
	   || GET_CODE (next) == CALL_INSN))
	    {
		next = PATTERN (next);
		if (GET_CODE (next) == SET
		    && SET_DEST (next) == pc_rtx
		    && GET_CODE (SET_SRC (next)) == IF_THEN_ELSE)
		    switch (GET_CODE (XEXP (SET_SRC (next), 0)))
			{
			case EQ:
			case NE:
			case LTU:
			case GTU:
			case LEU:
			case GEU:
			    return 1;
			}
	    }
    }
    return 0;
}
