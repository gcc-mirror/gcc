/* Subroutines for gcc2 for pdp11.
   Copyright (C) 1994, 1995, 1996, 1997, 1998, 1999
   Free Software Foundation, Inc.
   Contributed by Michael K. Gschwind (mike@vlsivie.tuwien.ac.at).

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
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-flags.h"
#include "function.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "recog.h"
#include "tm_p.h"

/*
#define FPU_REG_P(X)	((X)>=8 && (X)<14)
#define CPU_REG_P(X)	((X)>=0 && (X)<8)
*/

/* this is the current value returned by the macro FIRST_PARM_OFFSET 
   defined in tm.h */
int current_first_parm_offset;

/* This is where the condition code register lives.  */
/* rtx cc0_reg_rtx; - no longer needed? */

static rtx find_addr_reg PARAMS ((rtx)); 
static const char *singlemove_string PARAMS ((rtx *)); 

/* Nonzero if OP is a valid second operand for an arithmetic insn.  */

int
arith_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode) || GET_CODE (op) == CONST_INT);
}

int
const_immediate_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == CONST_INT);
}

int 
immediate15_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
    return (GET_CODE (op) == CONST_INT && ((INTVAL (op) & 0x8000) == 0x0000));
}

int
expand_shift_operand (op, mode)
  rtx op;
  enum machine_mode mode ATTRIBUTE_UNUSED;
{
    return (GET_CODE (op) == CONST_INT 
	    && abs (INTVAL(op)) > 1 
	    && abs (INTVAL(op)) <= 4);
}

/*
   stream is a stdio stream to output the code to.
   size is an int: how many units of temporary storage to allocate.
   Refer to the array `regs_ever_live' to determine which registers
   to save; `regs_ever_live[I]' is nonzero if register number I
   is ever used in the function.  This macro is responsible for
   knowing which registers should not be saved even if used.  
*/

void 
output_function_prologue(stream, size)
  FILE *stream;
  int size;
{							       
    int fsize = ((size) + 1) & ~1;      				
    int regno;

    int via_ac = -1;
    
    fprintf (stream, "\n\t;	/* function prologue %s*/\n", current_function_name);		

    /* if we are outputting code for main, 
       the switch FPU to right mode if TARGET_FPU */
    if ( (strcmp ("main", current_function_name) == 0)
	 && TARGET_FPU)
    {
	fprintf(stream, "\t;/* switch cpu to double float, single integer */\n");
	fprintf(stream, "\tsetd\n");
	fprintf(stream, "\tseti\n\n");
    }
    
    if (frame_pointer_needed) 					
    {								
	fprintf(stream, "\tmov fp, -(sp)\n");			
	fprintf(stream, "\tmov sp, fp\n");				
    }								
    else 								
    {								
	/* DON'T SAVE FP */
    }								

    /* make frame */
    if (fsize)							
	fprintf (stream, "\tsub $%o, sp\n", fsize);			

    /* save CPU registers  */
    for (regno = 0; regno < 8; regno++)				
	if (regs_ever_live[regno] && ! call_used_regs[regno])	
	    if (! ((regno == FRAME_POINTER_REGNUM)			
		   && frame_pointer_needed))				
		fprintf (stream, "\tmov %s, -(sp)\n", reg_names[regno]);	
    /* fpu regs saving */
    
    /* via_ac specifies the ac to use for saving ac4, ac5 */
    via_ac = -1;
    
    for (regno = 8; regno < FIRST_PSEUDO_REGISTER ; regno++) 
    {
	/* ac0 - ac3 */						
	if (LOAD_FPU_REG_P(regno)
	    && regs_ever_live[regno] 
	    && ! call_used_regs[regno])
	{
	    fprintf (stream, "\tfstd %s, -(sp)\n", reg_names[regno]);
	    via_ac = regno;
	}
	
	/* maybe make ac4, ac5 call used regs?? */
	/* ac4 - ac5 */
	if (NO_LOAD_FPU_REG_P(regno)
	    && regs_ever_live[regno]
	    && ! call_used_regs[regno])
	{
	    if (via_ac == -1)
		abort();
	    
	    fprintf (stream, "\tfldd %s, %s\n", reg_names[regno], reg_names[via_ac]);
	    fprintf (stream, "\tfstd %s, -(sp)\n", reg_names[via_ac]);
	}
    }

    fprintf (stream, "\t;/* end of prologue */\n\n");		
}

/*
   The function epilogue should not depend on the current stack pointer!
   It should use the frame pointer only.  This is mandatory because
   of alloca; we also take advantage of it to omit stack adjustments
   before returning.  */

/* maybe we can make leaf functions faster by switching to the
   second register file - this way we don't have to save regs!
   leaf functions are ~ 50% of all functions (dynamically!) 

   set/clear bit 11 (dec. 2048) of status word for switching register files - 
   but how can we do this? the pdp11/45 manual says bit may only 
   be set (p.24), but not cleared!

   switching to kernel is probably more expensive, so we'll leave it 
   like this and not use the second set of registers... 

   maybe as option if you want to generate code for kernel mode? */


void 
output_function_epilogue(stream, size)
  FILE *stream;
  int size;
{								
    int fsize = ((size) + 1) & ~1;      				
    int i, j, k;

    int via_ac;
    
    fprintf (stream, "\n\t;	/*function epilogue */\n");		

    if (frame_pointer_needed)					
    {								
	/* hope this is safe - m68k does it also .... */		
	regs_ever_live[FRAME_POINTER_REGNUM] = 0;			
								
	for (i =7, j = 0 ; i >= 0 ; i--)				
	    if (regs_ever_live[i] && ! call_used_regs[i])		
		j++;
	
	/* remember # of pushed bytes for CPU regs */
	k = 2*j;
	
	for (i =7 ; i >= 0 ; i--)					
	    if (regs_ever_live[i] && ! call_used_regs[i])		
		fprintf(stream, "\tmov %o(fp), %s\n",-fsize-2*j--, reg_names[i]);

	/* get ACs */						
	via_ac = FIRST_PSEUDO_REGISTER -1;
	
	for (i = FIRST_PSEUDO_REGISTER; i > 7; i--)
	    if (regs_ever_live[i] && ! call_used_regs[i])
	    {
		via_ac = i;
		k += 8;
	    }
	
	for (i = FIRST_PSEUDO_REGISTER; i > 7; i--)
	{
	    if (LOAD_FPU_REG_P(i)
		&& regs_ever_live[i]
		&& ! call_used_regs[i])
	    {
		fprintf(stream, "\tfldd %o(fp), %s\n", -fsize-k, reg_names[i]);
		k -= 8;
	    }
	    
	    if (NO_LOAD_FPU_REG_P(i)
		&& regs_ever_live[i]
		&& ! call_used_regs[i])
	    {
		if (! LOAD_FPU_REG_P(via_ac))
		    abort();
		    
		fprintf(stream, "\tfldd %o(fp), %s\n", -fsize-k, reg_names[via_ac]);
		fprintf(stream, "\tfstd %s, %s\n", reg_names[via_ac], reg_names[i]);
		k -= 8;
	    }
	}
	
	fprintf(stream, "\tmov fp, sp\n");				
	fprintf (stream, "\tmov (sp)+, fp\n");     			
    }								
    else								
    {		   
	via_ac = FIRST_PSEUDO_REGISTER -1;
	
	/* get ACs */
	for (i = FIRST_PSEUDO_REGISTER; i > 7; i--)
	    if (regs_ever_live[i] && call_used_regs[i])
		via_ac = i;
	
	for (i = FIRST_PSEUDO_REGISTER; i > 7; i--)
	{
	    if (LOAD_FPU_REG_P(i)
		&& regs_ever_live[i]
		&& ! call_used_regs[i])
	      fprintf(stream, "\tfldd (sp)+, %s\n", reg_names[i]);
	    
	    if (NO_LOAD_FPU_REG_P(i)
		&& regs_ever_live[i]
		&& ! call_used_regs[i])
	    {
		if (! LOAD_FPU_REG_P(via_ac))
		    abort();
		    
		fprintf(stream, "\tfldd (sp)+, %s\n", reg_names[via_ac]);
		fprintf(stream, "\tfstd %s, %s\n", reg_names[via_ac], reg_names[i]);
	    }
	}

	for (i=7; i >= 0; i--)					
	    if (regs_ever_live[i] && !call_used_regs[i])		
		fprintf(stream, "\tmov (sp)+, %s\n", reg_names[i]);	
								
	if (fsize)						
	    fprintf((stream), "\tadd $%o, sp\n", fsize);      		
    }			
					
    fprintf (stream, "\trts pc\n");					
    fprintf (stream, "\t;/* end of epilogue*/\n\n\n");		
}
	
/* Return the best assembler insn template
   for moving operands[1] into operands[0] as a fullword.  */
static const char *
singlemove_string (operands)
     rtx *operands;
{
  if (operands[1] != const0_rtx)
    return "mov %1,%0";

  return "clr %0";
}


/* Output assembler code to perform a doubleword move insn
   with operands OPERANDS.  */

const char *
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
#if 0
	   || GET_CODE (operands[1]) == CONST_DOUBLE)
#endif
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
    abort ();

  /* If one operand is decrementing and one is incrementing
     decrement the former register explicitly
     and change that operand into ordinary indexing.  */

  if (optype0 == PUSHOP && optype1 == POPOP)
    {
      operands[0] = XEXP (XEXP (operands[0], 0), 0);
      output_asm_insn ("sub $4,%0", operands);
      operands[0] = gen_rtx_MEM (SImode, operands[0]);
      optype0 = OFFSOP;
    }
  if (optype0 == POPOP && optype1 == PUSHOP)
    {
      operands[1] = XEXP (XEXP (operands[1], 0), 0);
      output_asm_insn ("sub $4,%1", operands);
      operands[1] = gen_rtx_MEM (SImode, operands[1]);
      optype1 = OFFSOP;
    }

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
    latehalf[0] = gen_rtx_REG (HImode, REGNO (operands[0]) + 1);
  else if (optype0 == OFFSOP)
    latehalf[0] = adj_offsettable_operand (operands[0], 2);
  else
    latehalf[0] = operands[0];

  if (optype1 == REGOP)
    latehalf[1] = gen_rtx_REG (HImode, REGNO (operands[1]) + 1);
  else if (optype1 == OFFSOP)
    latehalf[1] = adj_offsettable_operand (operands[1], 2);
  else if (optype1 == CNSTOP)
    {
	if (CONSTANT_P (operands[1]))
	{
	    /* now the mess begins, high word is in lower word??? 

	       that's what ashc makes me think, but I don't remember :-( */
	    latehalf[1] = GEN_INT (INTVAL(operands[1]) >> 16);
	    operands[1] = GEN_INT (INTVAL(operands[1]) & 0xff);
	}
      else if (GET_CODE (operands[1]) == CONST_DOUBLE)
	{
	    /* immediate 32 bit values not allowed */
	    abort();
	}
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
	output_asm_insn ("add $2,%0", &addreg0);
      if (addreg1)
	output_asm_insn ("add $2,%0", &addreg1);

      /* Do that word.  */
      output_asm_insn (singlemove_string (latehalf), latehalf);

      /* Undo the adds we just did.  */
      if (addreg0)
	output_asm_insn ("sub $2,%0", &addreg0);
      if (addreg1)
	output_asm_insn ("sub $2,%0", &addreg1);

      /* Do low-numbered word.  */
      return singlemove_string (operands);
    }

  /* Normal case: do the two words, low-numbered first.  */

  output_asm_insn (singlemove_string (operands), operands);

  /* Make any unoffsettable addresses point at high-numbered word.  */
  if (addreg0)
    output_asm_insn ("add $2,%0", &addreg0);
  if (addreg1)
    output_asm_insn ("add $2,%0", &addreg1);

  /* Do that word.  */
  output_asm_insn (singlemove_string (latehalf), latehalf);

  /* Undo the adds we just did.  */
  if (addreg0)
    output_asm_insn ("sub $2,%0", &addreg0);
  if (addreg1)
    output_asm_insn ("sub $2,%0", &addreg1);

  return "";
}
/* Output assembler code to perform a quadword move insn
   with operands OPERANDS.  */

const char *
output_move_quad (operands)
     rtx *operands;
{
  enum { REGOP, OFFSOP, MEMOP, PUSHOP, POPOP, CNSTOP, RNDOP } optype0, optype1;
  rtx latehalf[2];
  rtx addreg0 = 0, addreg1 = 0;

  output_asm_insn(";; movdi/df: %1 -> %0", operands);
  
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
  else if (CONSTANT_P (operands[1])
	   || GET_CODE (operands[1]) == CONST_DOUBLE)
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
    abort ();
  
  /* check if we move a CPU reg to an FPU reg, or vice versa! */
  if (optype0 == REGOP && optype1 == REGOP)
      /* bogus - 64 bit cannot reside in CPU! */
      if (CPU_REG_P(REGNO(operands[0]))
	  || CPU_REG_P (REGNO(operands[1])))
	  abort();
  
  if (optype0 == REGOP || optype1 == REGOP)
  {
      /* check for use of clrd???? 
         if you ever allow ac4 and ac5 (now we require secondary load) 
	 you must check whether 
	 you want to load into them or store from them - 
	 then dump ac0 into $help$ movce ac4/5 to ac0, do the 
	 store from ac0, and restore ac0 - if you can find 
	 an unused ac[0-3], use that and you save a store and a load!*/

      if (FPU_REG_P(REGNO(operands[0])))
      {
	  if (GET_CODE(operands[1]) == CONST_DOUBLE)
	  {
	      union { double d; int i[2]; } u;
	      u.i[0] = CONST_DOUBLE_LOW (operands[1]); 
	      u.i[1] = CONST_DOUBLE_HIGH (operands[1]); 
	      
	      if (u.d == 0.0)
		  return "{clrd|clrf} %0";
	  }
	      
	  return "{ldd|movf} %1, %0";
      }
      
      if (FPU_REG_P(REGNO(operands[1])))
	  return "{std|movf} %1, %0";
  }
      
  /* If one operand is decrementing and one is incrementing
     decrement the former register explicitly
     and change that operand into ordinary indexing.  */

  if (optype0 == PUSHOP && optype1 == POPOP)
    {
      operands[0] = XEXP (XEXP (operands[0], 0), 0);
      output_asm_insn ("sub $8,%0", operands);
      operands[0] = gen_rtx_MEM (DImode, operands[0]);
      optype0 = OFFSOP;
    }
  if (optype0 == POPOP && optype1 == PUSHOP)
    {
      operands[1] = XEXP (XEXP (operands[1], 0), 0);
      output_asm_insn ("sub $8,%1", operands);
      operands[1] = gen_rtx_MEM (SImode, operands[1]);
      optype1 = OFFSOP;
    }

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
    latehalf[0] = gen_rtx_REG (SImode, REGNO (operands[0]) + 2);
  else if (optype0 == OFFSOP)
    latehalf[0] = adj_offsettable_operand (operands[0], 4);
  else
    latehalf[0] = operands[0];

  if (optype1 == REGOP)
    latehalf[1] = gen_rtx_REG (SImode, REGNO (operands[1]) + 2);
  else if (optype1 == OFFSOP)
    latehalf[1] = adj_offsettable_operand (operands[1], 4);
  else if (optype1 == CNSTOP)
    {
      if (GET_CODE (operands[1]) == CONST_DOUBLE)
	{
	    /* floats only. not yet supported!

	     -- compute it into PDP float format, - internally,
	     just use IEEE and ignore possible problems ;-)

	     we might get away with it !!!! */

	    abort();
	    
#ifndef HOST_WORDS_BIG_ENDIAN
	  latehalf[1] = GEN_INT (CONST_DOUBLE_LOW (operands[1]));
	  operands[1] = GEN_INT	(CONST_DOUBLE_HIGH (operands[1]));
#else /* HOST_WORDS_BIG_ENDIAN */
	  latehalf[1] = GEN_INT (CONST_DOUBLE_HIGH (operands[1]));
	  operands[1] = GEN_INT (CONST_DOUBLE_LOW (operands[1]));
#endif /* HOST_WORDS_BIG_ENDIAN */
	}
      else if (GET_CODE(operands[1]) == CONST_INT)
	{
	  latehalf[1] = GEN_INT (0);
	}
      else
	abort();
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
	output_asm_insn ("add $4,%0", &addreg0);
      if (addreg1)
	output_asm_insn ("add $4,%0", &addreg1);

      /* Do that word.  */
      output_asm_insn(output_move_double(latehalf), latehalf);

      /* Undo the adds we just did.  */
      if (addreg0)
	output_asm_insn ("sub $4,%0", &addreg0);
      if (addreg1)
	output_asm_insn ("sub $4,%0", &addreg1);

      /* Do low-numbered word.  */
      return output_move_double (operands);
    }

  /* Normal case: do the two words, low-numbered first.  */

  output_asm_insn (output_move_double (operands), operands);

  /* Make any unoffsettable addresses point at high-numbered word.  */
  if (addreg0)
    output_asm_insn ("add $4,%0", &addreg0);
  if (addreg1)
    output_asm_insn ("add $4,%0", &addreg1);

  /* Do that word.  */
  output_asm_insn (output_move_double (latehalf), latehalf);

  /* Undo the adds we just did.  */
  if (addreg0)
    output_asm_insn ("sub $4,%0", &addreg0);
  if (addreg1)
    output_asm_insn ("sub $4,%0", &addreg1);

  return "";
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
      if (GET_CODE (XEXP (addr, 1)) == REG)
	addr = XEXP (addr, 1);
      if (CONSTANT_P (XEXP (addr, 0)))
	addr = XEXP (addr, 1);
      if (CONSTANT_P (XEXP (addr, 1)))
	addr = XEXP (addr, 0);
    }
  if (GET_CODE (addr) == REG)
    return addr;
  return 0;
}

/* Output an ascii string.  */
void
output_ascii (file, p, size)
     FILE *file;
     const char *p;
     int size;
{
  int i;

  /* This used to output .byte "string", which doesn't work with the UNIX
     assembler and I think not with DEC ones either.  */
  fprintf (file, "\t.byte ");

  for (i = 0; i < size; i++)
    {
      register int c = p[i];
      if (c < 0)
	c += 256;
      fprintf (file, "%o", c);
      if (i < size - 1)
	putc (',', file);
    }
  putc ('\n', file);
}


/* --- stole from out-vax, needs changes */

void
print_operand_address (file, addr)
     FILE *file;
     register rtx addr;
{
  register rtx reg1, reg2, breg, ireg;
  rtx offset;

 retry:

  switch (GET_CODE (addr))
    {
    case MEM:
      if (TARGET_UNIX_ASM)
	fprintf (file, "*");
      else
	fprintf (file, "@");
      addr = XEXP (addr, 0);
      goto retry;

    case REG:
      fprintf (file, "(%s)", reg_names[REGNO (addr)]);
      break;

    case PRE_DEC:
      fprintf (file, "-(%s)", reg_names[REGNO (XEXP (addr, 0))]);
      break;

    case POST_INC:
      fprintf (file, "(%s)+", reg_names[REGNO (XEXP (addr, 0))]);
      break;

    case PLUS:
      reg1 = 0;	reg2 = 0;
      ireg = 0;	breg = 0;
      offset = 0;
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
	output_address (addr);
      if (breg != 0)
	{
	  if (GET_CODE (breg) != REG)
	    abort ();
	  fprintf (file, "(%s)", reg_names[REGNO (breg)]);
	}
      if (ireg != 0)
	{
	  if (GET_CODE (ireg) == MULT)
	    ireg = XEXP (ireg, 0);
	  if (GET_CODE (ireg) != REG)
	    abort ();
	  abort();
	  fprintf (file, "[%s]", reg_names[REGNO (ireg)]);
	}
      break;

    default:
      output_addr_const_pdp11 (file, addr);
    }
}

/* register move costs, indexed by regs */

static int move_costs[N_REG_CLASSES][N_REG_CLASSES] = 
{
             /* NO  MUL  GEN  LFPU  NLFPU FPU ALL */

/* NO */     {  0,   0,   0,    0,    0,    0,   0},
/* MUL */    {  0,   2,   2,   10,   22,   22,  22},
/* GEN */    {  0,   2,   2,   10,   22,   22,  22},
/* LFPU */   {  0,  10,  10,    2,    2,    2,  10},
/* NLFPU */  {  0,  22,  22,    2,    2,    2,  22},
/* FPU */    {  0,  22,  22,    2,    2,    2,  22},
/* ALL */    {  0,  22,  22,   10,   22,   22,  22}
}  ;


/* -- note that some moves are tremendously expensive, 
   because they require lots of tricks! do we have to 
   charge the costs incurred by secondary reload class 
   -- as we do here with 22 -- or not ? */

int 
register_move_cost(c1, c2)
  enum reg_class c1, c2;
{
    return move_costs[(int)c1][(int)c2];
}

const char *
output_jump(pos, neg, length)
  const char *pos, *neg;
  int length;
{
    static int x = 0;
    
    static char buf[1000];

#if 0
/* currently we don't need this, because the tstdf and cmpdf 
   copy the condition code immediately, and other float operations are not 
   yet recognized as changing the FCC - if so, then the length-cost of all
   jump insns increases by one, because we have to potentially copy the 
   FCC! */
    if (cc_status.flags & CC_IN_FPU)
	output_asm_insn("cfcc", NULL);
#endif
	
    switch (length)
    {
      case 1:
	
	strcpy(buf, pos);
	strcat(buf, " %l0");
	
	return buf;
	
      case 3:
	
	sprintf(buf, "%s JMP_%d\n\tjmp %%l0\nJMP_%d:", neg, x, x);
	
	x++;
	
	return buf;
	
      default:
	
	abort();
    }
    
}

void
notice_update_cc_on_set(exp, insn)
  rtx exp;
  rtx insn ATTRIBUTE_UNUSED;
{
    if (GET_CODE (SET_DEST (exp)) == CC0)
    { 
	cc_status.flags = 0;					
	cc_status.value1 = SET_DEST (exp);			
	cc_status.value2 = SET_SRC (exp);			

/*
	if (GET_MODE(SET_SRC(exp)) == DFmode)
	    cc_status.flags |= CC_IN_FPU;
*/	
    }							
    else if ((GET_CODE (SET_DEST (exp)) == REG		
	      || GET_CODE (SET_DEST (exp)) == MEM)		
	     && GET_CODE (SET_SRC (exp)) != PC		
	     && (GET_MODE (SET_DEST(exp)) == HImode		
		 || GET_MODE (SET_DEST(exp)) == QImode)	
		&& (GET_CODE (SET_SRC(exp)) == PLUS		
		    || GET_CODE (SET_SRC(exp)) == MINUS	
		    || GET_CODE (SET_SRC(exp)) == AND	
		    || GET_CODE (SET_SRC(exp)) == IOR	
		    || GET_CODE (SET_SRC(exp)) == XOR	
		    || GET_CODE (SET_SRC(exp)) == NOT	
		    || GET_CODE (SET_SRC(exp)) == NEG	
			|| GET_CODE (SET_SRC(exp)) == REG	
		    || GET_CODE (SET_SRC(exp)) == MEM))	
    { 
	cc_status.flags = 0;					
	cc_status.value1 = SET_SRC (exp);   			
	cc_status.value2 = SET_DEST (exp);			
	
	if (cc_status.value1 && GET_CODE (cc_status.value1) == REG	
	    && cc_status.value2					
	    && reg_overlap_mentioned_p (cc_status.value1, cc_status.value2))
    	    cc_status.value2 = 0;					
	if (cc_status.value1 && GET_CODE (cc_status.value1) == MEM	
	    && cc_status.value2					
	    && GET_CODE (cc_status.value2) == MEM)			
	    cc_status.value2 = 0; 					
    }							
    else if (GET_CODE (SET_SRC (exp)) == CALL)		
    { 
	CC_STATUS_INIT; 
    }
    else if (GET_CODE (SET_DEST (exp)) == REG)       		
	/* what's this ? */					
    { 
	if ((cc_status.value1					
	     && reg_overlap_mentioned_p (SET_DEST (exp), cc_status.value1)))
	    cc_status.value1 = 0;				
	if ((cc_status.value2					
	     && reg_overlap_mentioned_p (SET_DEST (exp), cc_status.value2)))
	    cc_status.value2 = 0;				
    }							
    else if (SET_DEST(exp) == pc_rtx)
    { 
	/* jump */
    }
    else /* if (GET_CODE (SET_DEST (exp)) == MEM)	*/	
    {  
	/* the last else is a bit paranoiac, but since nearly all instructions 
	   play with condition codes, it's reasonable! */

	CC_STATUS_INIT; /* paranoia*/ 
    }		        
}


int
simple_memory_operand(op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
    rtx addr;

    /* Eliminate non-memory operations */
    if (GET_CODE (op) != MEM)
	return FALSE;

#if 0
    /* dword operations really put out 2 instructions, so eliminate them.  */
    if (GET_MODE_SIZE (GET_MODE (op)) > (HAVE_64BIT_P () ? 8 : 4))
	return FALSE;
#endif

    /* Decode the address now.  */

  indirection:
    
    addr = XEXP (op, 0);

    switch (GET_CODE (addr))
    {
      case REG:
	/* (R0) - no extra cost */
	return 1;
	
      case PRE_DEC:
      case POST_INC:
	/* -(R0), (R0)+ - cheap! */
	return 0;
	
      case MEM:
	/* cheap - is encoded in addressing mode info! 

	   -- except for @(R0), which has to be @0(R0) !!! */

	if (GET_CODE (XEXP (addr, 0)) == REG)
	    return 0;
	
	op=addr;
	goto indirection;
	
      case CONST_INT:
      case LABEL_REF:	       
      case CONST:
      case SYMBOL_REF:
	/* @#address - extra cost */
	return 0;

      case PLUS:
	/* X(R0) - extra cost */
	return 0;

      default:
	break;
    }
    
    return FALSE;
}


/*
 * output a block move:
 *
 * operands[0]	... to
 * operands[1]  ... from
 * operands[2]  ... length
 * operands[3]  ... alignment
 * operands[4]  ... scratch register
 */

 
const char *
output_block_move(operands)
  rtx *operands;
{
    static int count = 0;
    char buf[200];
    
    if (GET_CODE(operands[2]) == CONST_INT
	&& ! optimize_size)
    {
	if (INTVAL(operands[2]) < 16
	    && INTVAL(operands[3]) == 1)
	{
	    register int i;
	    
	    for (i = 1; i <= INTVAL(operands[2]); i++)
		output_asm_insn("movb (%1)+, (%0)+", operands);

	    return "";
	}
	else if (INTVAL(operands[2]) < 32)
	{
	    register int i;
	    
	    for (i = 1; i <= INTVAL(operands[2])/2; i++)
		output_asm_insn("mov (%1)+, (%0)+", operands);
	    
	    /* may I assume that moved quantity is 
	       multiple of alignment ???

	       I HOPE SO !
	    */

	    return "";
	}
	

	/* can do other clever things, maybe... */
    }

    if (CONSTANT_P(operands[2]) )
    {
	/* just move count to scratch */
	output_asm_insn("mov %2, %4", operands);
    }
    else
    {
	/* just clobber the register */
	operands[4] = operands[2];
    }
    

    /* switch over alignment */
    switch (INTVAL(operands[3]))
    {
      case 1:
	
	/* 
	  x:
	  movb (%1)+, (%0)+
	  
	  if (TARGET_45)
	     sob %4,x
	  else
	     dec %4
	     bgt x

	*/

	sprintf(buf, "\nmovestrhi%d:", count);
	output_asm_insn(buf, NULL);
	
	output_asm_insn("movb (%1)+, (%0)+", operands);
	
	if (TARGET_45)
	{
	    sprintf(buf, "sob %%4, movestrhi%d", count);
	    output_asm_insn(buf, operands);
	}
	else
	{
	    output_asm_insn("dec %4", operands);
	    
	    sprintf(buf, "bgt movestrhi%d", count);
	    output_asm_insn(buf, NULL);
	}
	
	count ++;
	break;
	
      case 2:
	
	/* 
	   asr %4

	   x:

	   mov (%1)+, (%0)+

	   if (TARGET_45)
	     sob %4, x
	   else
	     dec %4
	     bgt x
	*/

      generate_compact_code:

	output_asm_insn("asr %4", operands);

	sprintf(buf, "\nmovestrhi%d:", count);
	output_asm_insn(buf, NULL);
	
	output_asm_insn("mov (%1)+, (%0)+", operands);
	
	if (TARGET_45)
	{
	    sprintf(buf, "sob %%4, movestrhi%d", count);
	    output_asm_insn(buf, operands);
	}
	else
	{
	    output_asm_insn("dec %4", operands);
	    
	    sprintf(buf, "bgt movestrhi%d", count);
	    output_asm_insn(buf, NULL);
	}
	
	count ++;
	break;

      case 4:
	
	/*

	   asr %4
	   asr %4

	   x:

	   mov (%1)+, (%0)+
	   mov (%1)+, (%0)+

	   if (TARGET_45)
	     sob %4, x
	   else
	     dec %4
	     bgt x
	*/

	if (optimize_size)
	    goto generate_compact_code;
	
	output_asm_insn("asr %4", operands);
	output_asm_insn("asr %4", operands);

	sprintf(buf, "\nmovestrhi%d:", count);
	output_asm_insn(buf, NULL);
	
	output_asm_insn("mov (%1)+, (%0)+", operands);
	output_asm_insn("mov (%1)+, (%0)+", operands);
	
	if (TARGET_45)
	{
	    sprintf(buf, "sob %%4, movestrhi%d", count);
	    output_asm_insn(buf, operands);
	}
	else
	{
	    output_asm_insn("dec %4", operands);
	    
	    sprintf(buf, "bgt movestrhi%d", count);
	    output_asm_insn(buf, NULL);
	}
	
	count ++;
	break;
       
      default:
	
	/*
	   
	   asr %4
	   asr %4
	   asr %4

	   x:

	   mov (%1)+, (%0)+
	   mov (%1)+, (%0)+
	   mov (%1)+, (%0)+
	   mov (%1)+, (%0)+
	   
	   if (TARGET_45)
	     sob %4, x
	   else
	     dec %4
	     bgt x
	*/


	if (optimize_size)
	    goto generate_compact_code;
	
	output_asm_insn("asr %4", operands);
	output_asm_insn("asr %4", operands);
	output_asm_insn("asr %4", operands);

	sprintf(buf, "\nmovestrhi%d:", count);
	output_asm_insn(buf, NULL);
	
	output_asm_insn("mov (%1)+, (%0)+", operands);
	output_asm_insn("mov (%1)+, (%0)+", operands);
	output_asm_insn("mov (%1)+, (%0)+", operands);
	output_asm_insn("mov (%1)+, (%0)+", operands);
	
	if (TARGET_45)
	{
	    sprintf(buf, "sob %%4, movestrhi%d", count);
	    output_asm_insn(buf, operands);
	}
	else
	{
	    output_asm_insn("dec %4", operands);
	    
	    sprintf(buf, "bgt movestrhi%d", count);
	    output_asm_insn(buf, NULL);
	}
	
	count ++;
	break;
	
	;
	
    }
    
    return "";
}

/* for future use */
int
comparison_operator_index(op)
  rtx op;
{
    switch (GET_CODE(op))
    {
      case NE:
	return 0;
	
      case EQ:
	return 1;
	
      case GE:
	return 2;
	
      case GT:
	return 3;
	
      case LE:
	return 4;
	
      case LT:
	return 5;
	
      case GEU:
	return 6;
	
      case GTU:
	return 7;

      case LEU:
	return 8;
	
      case LTU:
	return 9;
	
      default:
	return -1;
    }
}    
	
/* tests whether the rtx is a comparison operator */
int
comp_operator (op, mode)
  rtx op;
  enum machine_mode mode ATTRIBUTE_UNUSED;
{
    return comparison_operator_index(op) >= 0;
}

    
int
legitimate_address_p (mode, address)
  enum machine_mode mode;
  rtx address;
{
/* #define REG_OK_STRICT */
    GO_IF_LEGITIMATE_ADDRESS(mode, address, win);
    
    return 0;
    
  win:
    return 1;

/* #undef REG_OK_STRICT */
}

/* A copy of output_addr_const modified for pdp11 expression syntax.
   output_addr_const also gets called for %cDIGIT and %nDIGIT, which we don't
   use, and for debugging output, which we don't support with this port either.
   So this copy should get called whenever needed.
*/
void
output_addr_const_pdp11 (file, x)
     FILE *file;
     rtx x;
{
  char buf[256];

 restart:
  switch (GET_CODE (x))
    {
    case PC:
      if (flag_pic)
	putc ('.', file);
      else
	abort ();
      break;

    case SYMBOL_REF:
      assemble_name (file, XSTR (x, 0));
      break;

    case LABEL_REF:
      ASM_GENERATE_INTERNAL_LABEL (buf, "L", CODE_LABEL_NUMBER (XEXP (x, 0)));
      assemble_name (file, buf);
      break;

    case CODE_LABEL:
      ASM_GENERATE_INTERNAL_LABEL (buf, "L", CODE_LABEL_NUMBER (x));
      assemble_name (file, buf);
      break;

    case CONST_INT:
      /* Should we check for constants which are too big?  Maybe cutting
	 them off to 16 bits is OK?  */
      fprintf (file, "%ho", (unsigned short) INTVAL (x));
      break;

    case CONST:
      /* This used to output parentheses around the expression,
	 but that does not work on the 386 (either ATT or BSD assembler).  */
      output_addr_const_pdp11 (file, XEXP (x, 0));
      break;

    case CONST_DOUBLE:
      if (GET_MODE (x) == VOIDmode)
	{
	  /* We can use %o if the number is one word and positive.  */
	  if (CONST_DOUBLE_HIGH (x))
	    abort (); /* Should we just silently drop the high part?  */
	  else
	    fprintf (file, "%ho", (unsigned short) CONST_DOUBLE_LOW (x));
	}
      else
	/* We can't handle floating point constants;
	   PRINT_OPERAND must handle them.  */
	output_operand_lossage ("floating constant misused");
      break;

    case PLUS:
      /* Some assemblers need integer constants to appear last (eg masm).  */
      if (GET_CODE (XEXP (x, 0)) == CONST_INT)
	{
	  output_addr_const_pdp11 (file, XEXP (x, 1));
	  if (INTVAL (XEXP (x, 0)) >= 0)
	    fprintf (file, "+");
	  output_addr_const_pdp11 (file, XEXP (x, 0));
	}
      else
	{
	  output_addr_const_pdp11 (file, XEXP (x, 0));
	  if (INTVAL (XEXP (x, 1)) >= 0)
	    fprintf (file, "+");
	  output_addr_const_pdp11 (file, XEXP (x, 1));
	}
      break;

    case MINUS:
      /* Avoid outputting things like x-x or x+5-x,
	 since some assemblers can't handle that.  */
      x = simplify_subtraction (x);
      if (GET_CODE (x) != MINUS)
	goto restart;

      output_addr_const_pdp11 (file, XEXP (x, 0));
      fprintf (file, "-");
      if (GET_CODE (XEXP (x, 1)) == CONST_INT
	  && INTVAL (XEXP (x, 1)) < 0)
	{
	  fprintf (file, ASM_OPEN_PAREN);
	  output_addr_const_pdp11 (file, XEXP (x, 1));
	  fprintf (file, ASM_CLOSE_PAREN);
	}
      else
	output_addr_const_pdp11 (file, XEXP (x, 1));
      break;

    case ZERO_EXTEND:
    case SIGN_EXTEND:
      output_addr_const_pdp11 (file, XEXP (x, 0));
      break;

    default:
      output_operand_lossage ("invalid expression as operand");
    }
}
