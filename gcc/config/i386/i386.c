/* Subroutines for insn-output.c for Intel X86.
   Copyright (C) 1988, 1992, 1994 Free Software Foundation, Inc.

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

#include <stdio.h>
#include <setjmp.h>
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
#include "tree.h"
#include "flags.h"
#include "function.h"

#ifdef EXTRA_CONSTRAINT
/* If EXTRA_CONSTRAINT is defined, then the 'S'
   constraint in REG_CLASS_FROM_LETTER will no longer work, and various
   asm statements that need 'S' for class SIREG will break.  */
 error EXTRA_CONSTRAINT conflicts with S constraint letter
/* The previous line used to be #error, but some compilers barf
   even if the conditional was untrue.  */
#endif

#define AT_BP(mode) (gen_rtx (MEM, (mode), frame_pointer_rtx))

extern FILE *asm_out_file;
extern char *strcat ();

char *singlemove_string ();
char *output_move_const_single ();
char *output_fp_cc0_set ();

char *hi_reg_name[] = HI_REGISTER_NAMES;
char *qi_reg_name[] = QI_REGISTER_NAMES;
char *qi_high_reg_name[] = QI_HIGH_REGISTER_NAMES;

/* Array of the smallest class containing reg number REGNO, indexed by
   REGNO.  Used by REGNO_REG_CLASS in i386.h. */

enum reg_class regclass_map[FIRST_PSEUDO_REGISTER] =
{
  /* ax, dx, cx, bx */
  AREG, DREG, CREG, BREG,
  /* si, di, bp, sp */
  SIREG, DIREG, INDEX_REGS, GENERAL_REGS,
  /* FP registers */
  FP_TOP_REG, FP_SECOND_REG, FLOAT_REGS, FLOAT_REGS,
  FLOAT_REGS, FLOAT_REGS, FLOAT_REGS, FLOAT_REGS,       
  /* arg pointer */
  INDEX_REGS
};

/* Test and compare insns in i386.md store the information needed to
   generate branch and scc insns here.  */

struct rtx_def *i386_compare_op0 = NULL_RTX;
struct rtx_def *i386_compare_op1 = NULL_RTX;
struct rtx_def *(*i386_compare_gen)(), *(*i386_compare_gen_eq)();

/* Register allocation order */
char *i386_reg_alloc_order = (char *)0;
static char regs_allocated[FIRST_PSEUDO_REGISTER];


/* Sometimes certain combinations of command options do not make
   sense on a particular target machine.  You can define a macro
   `OVERRIDE_OPTIONS' to take account of this.  This macro, if
   defined, is executed once just after all the command options have
   been parsed.

   Don't use this macro to turn on various extra optimizations for
   `-O'.  That is what `OPTIMIZATION_OPTIONS' is for.  */

void
override_options ()
{
  int ch, i, regno;

#ifdef SUBTARGET_OVERRIDE_OPTIONS
  SUBTARGET_OVERRIDE_OPTIONS;
#endif

  /* Validate registers in register allocation order */
  if (i386_reg_alloc_order)
    {
      for (i = 0; (ch = i386_reg_alloc_order[i]) != '\0'; i++)
	{
	  switch (ch)
	    {
	    case 'a':	regno = 0;	break;
	    case 'd':	regno = 1;	break;
	    case 'c':	regno = 2;	break;
	    case 'b':	regno = 3;	break;
	    case 'S':	regno = 4;	break;
	    case 'D':	regno = 5;	break;
	    case 'B':	regno = 6;	break;

	    default:	fatal ("Register '%c' is unknown", ch);
	    }

	  if (regs_allocated[regno])
	    fatal ("Register '%c' was already specified in the allocation order", ch);

	  regs_allocated[regno] = 1;
	}
    }
}

/* A C statement (sans semicolon) to choose the order in which to
   allocate hard registers for pseudo-registers local to a basic
   block.

   Store the desired register order in the array `reg_alloc_order'.
   Element 0 should be the register to allocate first; element 1, the
   next register; and so on.

   The macro body should not assume anything about the contents of
   `reg_alloc_order' before execution of the macro.

   On most machines, it is not necessary to define this macro.  */

void
order_regs_for_local_alloc ()
{
  int i, ch, order, regno;

  /* User specified the register allocation order */
  if (i386_reg_alloc_order)
    {
      for (i = order = 0; (ch = i386_reg_alloc_order[i]) != '\0'; i++)
	{
	  switch (ch)
	    {
	    case 'a':	regno = 0;	break;
	    case 'd':	regno = 1;	break;
	    case 'c':	regno = 2;	break;
	    case 'b':	regno = 3;	break;
	    case 'S':	regno = 4;	break;
	    case 'D':	regno = 5;	break;
	    case 'B':	regno = 6;	break;
	    }

	  reg_alloc_order[order++] = regno;
	}

      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	{
	  if (!regs_allocated[i])
	    reg_alloc_order[order++] = i;
	}
    }

  /* If users did not specify a register allocation order, favor eax
     normally except if DImode variables are used, in which case
     favor edx before eax, which seems to cause less spill register
     not found messages.  */
  else
    {
      rtx insn;

      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	reg_alloc_order[i] = i;

      if (optimize)
	{
	  int use_dca = FALSE;

	  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
	    {
	      if (GET_CODE (insn) == INSN)
		{
		  rtx set = NULL_RTX;
		  rtx pattern = PATTERN (insn);

		  if (GET_CODE (pattern) == SET)
		    set = pattern;

		  else if ((GET_CODE (pattern) == PARALLEL
			    || GET_CODE (pattern) == SEQUENCE)
			   && GET_CODE (XVECEXP (pattern, 0, 0)) == SET)
		    set = XVECEXP (pattern, 0, 0);

		  if (set && GET_MODE (SET_SRC (set)) == DImode)
		    {
		      use_dca = TRUE;
		      break;
		    }
		}
	    }

	  if (use_dca)
	    {
	      reg_alloc_order[0] = 1;	/* edx */
	      reg_alloc_order[1] = 2;	/* ecx */
	      reg_alloc_order[2] = 0;	/* eax */
	    }
	}
    }
}


/* Output an insn whose source is a 386 integer register.  SRC is the
   rtx for the register, and TEMPLATE is the op-code template.  SRC may
   be either SImode or DImode.

   The template will be output with operands[0] as SRC, and operands[1]
   as a pointer to the top of the 386 stack.  So a call from floatsidf2
   would look like this:

      output_op_from_reg (operands[1], AS1 (fild%z0,%1));

   where %z0 corresponds to the caller's operands[1], and is used to
   emit the proper size suffix.

   ??? Extend this to handle HImode - a 387 can load and store HImode
   values directly. */

void
output_op_from_reg (src, template)
     rtx src;
     char *template;
{
  rtx xops[4];
  int size = GET_MODE_SIZE (GET_MODE (src));

  xops[0] = src;
  xops[1] = AT_SP (Pmode);
  xops[2] = GEN_INT (size);
  xops[3] = stack_pointer_rtx;

  if (size > UNITS_PER_WORD)
    {
      rtx high;
      if (size > 2 * UNITS_PER_WORD)
	{
	  high = gen_rtx (REG, SImode, REGNO (src) + 2);
	  output_asm_insn (AS1 (push%L0,%0), &high);
	}
      high = gen_rtx (REG, SImode, REGNO (src) + 1);
      output_asm_insn (AS1 (push%L0,%0), &high);
    }
  output_asm_insn (AS1 (push%L0,%0), &src);

  output_asm_insn (template, xops);

  output_asm_insn (AS2 (add%L3,%2,%3), xops);
}

/* Output an insn to pop an value from the 387 top-of-stack to 386
   register DEST. The 387 register stack is popped if DIES is true.  If
   the mode of DEST is an integer mode, a `fist' integer store is done,
   otherwise a `fst' float store is done. */

void
output_to_reg (dest, dies)
     rtx dest;
     int dies;
{
  rtx xops[4];
  int size = GET_MODE_SIZE (GET_MODE (dest));

  xops[0] = AT_SP (Pmode);
  xops[1] = stack_pointer_rtx;
  xops[2] = GEN_INT (size);
  xops[3] = dest;

  output_asm_insn (AS2 (sub%L1,%2,%1), xops);

  if (GET_MODE_CLASS (GET_MODE (dest)) == MODE_INT)
    {
      if (dies)
	output_asm_insn (AS1 (fistp%z3,%y0), xops);
      else
	output_asm_insn (AS1 (fist%z3,%y0), xops);
    }
  else if (GET_MODE_CLASS (GET_MODE (dest)) == MODE_FLOAT)
    {
      if (dies)
	output_asm_insn (AS1 (fstp%z3,%y0), xops);
      else
	{
	  if (GET_MODE (dest) == XFmode)
	    {
	      output_asm_insn (AS1 (fstp%z3,%y0), xops);
	      output_asm_insn (AS1 (fld%z3,%y0), xops);
	    }
	  else
	    output_asm_insn (AS1 (fst%z3,%y0), xops);
	}
    }
  else
    abort ();

  output_asm_insn (AS1 (pop%L0,%0), &dest);

  if (size > UNITS_PER_WORD)
    {
      dest = gen_rtx (REG, SImode, REGNO (dest) + 1);
      output_asm_insn (AS1 (pop%L0,%0), &dest);
      if (size > 2 * UNITS_PER_WORD)
	{
	  dest = gen_rtx (REG, SImode, REGNO (dest) + 1);
	  output_asm_insn (AS1 (pop%L0,%0), &dest);
	}
    }
}

char *
singlemove_string (operands)
     rtx *operands;
{
  rtx x;
  if (GET_CODE (operands[0]) == MEM
      && GET_CODE (x = XEXP (operands[0], 0)) == PRE_DEC)
    {
      if (XEXP (x, 0) != stack_pointer_rtx)
	abort ();
      return "push%L1 %1";
    }
  else if (GET_CODE (operands[1]) == CONST_DOUBLE)
    {
      return output_move_const_single (operands);
    }
  else if (GET_CODE (operands[0]) == REG || GET_CODE (operands[1]) == REG)
    return AS2 (mov%L0,%1,%0);
  else if (CONSTANT_P (operands[1]))
    return AS2 (mov%L0,%1,%0);
  else
    {
      output_asm_insn ("push%L1 %1", operands);
      return "pop%L0 %0";
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


/* Output an insn to add the constant N to the register X.  */

static void
asm_add (n, x)
     int n;
     rtx x;
{
  rtx xops[2];
  xops[0] = x;

  if (n == -1)
    output_asm_insn (AS1 (dec%L0,%0), xops);
  else if (n == 1)
    output_asm_insn (AS1 (inc%L0,%0), xops);
  else if (n < 0)
    {
      xops[1] = GEN_INT (-n);
      output_asm_insn (AS2 (sub%L0,%1,%0), xops);
    }
  else if (n > 0)
    {
      xops[1] = GEN_INT (n);
      output_asm_insn (AS2 (add%L0,%1,%0), xops);
    }
}


/* Output assembler code to perform a doubleword move insn
   with operands OPERANDS.  */

char *
output_move_double (operands)
     rtx *operands;
{
  enum {REGOP, OFFSOP, MEMOP, PUSHOP, POPOP, CNSTOP, RNDOP } optype0, optype1;
  rtx latehalf[2];
  rtx middlehalf[2];
  rtx xops[2];
  rtx addreg0 = 0, addreg1 = 0;
  int dest_overlapped_low = 0;
  int size = GET_MODE_SIZE (GET_MODE (operands[1]));

  middlehalf[0] = 0;
  middlehalf[1] = 0;

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
    abort ();

  /* If one operand is decrementing and one is incrementing
     decrement the former register explicitly
     and change that operand into ordinary indexing.  */

  if (optype0 == PUSHOP && optype1 == POPOP)
    {
      /* ??? Can this ever happen on i386? */
      operands[0] = XEXP (XEXP (operands[0], 0), 0);
      asm_add (-size, operands[0]);
      if (GET_MODE (operands[1]) == XFmode)
        operands[0] = gen_rtx (MEM, XFmode, operands[0]);
      else if (GET_MODE (operands[0]) == DFmode)
        operands[0] = gen_rtx (MEM, DFmode, operands[0]);
      else
        operands[0] = gen_rtx (MEM, DImode, operands[0]);
      optype0 = OFFSOP;
    }

  if (optype0 == POPOP && optype1 == PUSHOP)
    {
      /* ??? Can this ever happen on i386? */
      operands[1] = XEXP (XEXP (operands[1], 0), 0);
      asm_add (-size, operands[1]);
      if (GET_MODE (operands[1]) == XFmode)
        operands[1] = gen_rtx (MEM, XFmode, operands[1]);
      else if (GET_MODE (operands[1]) == DFmode)
        operands[1] = gen_rtx (MEM, DFmode, operands[1]);
      else
        operands[1] = gen_rtx (MEM, DImode, operands[1]);
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

  if (size == 12)
    {
      if (optype0 == REGOP)
	{
	  middlehalf[0] = gen_rtx (REG, SImode, REGNO (operands[0]) + 1);
	  latehalf[0] = gen_rtx (REG, SImode, REGNO (operands[0]) + 2);
	}
      else if (optype0 == OFFSOP)
	{
	  middlehalf[0] = adj_offsettable_operand (operands[0], 4);
	  latehalf[0] = adj_offsettable_operand (operands[0], 8);
	}
      else
	{
         middlehalf[0] = operands[0];
         latehalf[0] = operands[0];
	}
    
      if (optype1 == REGOP)
	{
          middlehalf[1] = gen_rtx (REG, SImode, REGNO (operands[1]) + 1);
          latehalf[1] = gen_rtx (REG, SImode, REGNO (operands[1]) + 2);
	}
      else if (optype1 == OFFSOP)
	{
          middlehalf[1] = adj_offsettable_operand (operands[1], 4);
          latehalf[1] = adj_offsettable_operand (operands[1], 8);
	}
      else if (optype1 == CNSTOP)
	{
	  if (GET_CODE (operands[1]) == CONST_DOUBLE)
	    {
	      REAL_VALUE_TYPE r; long l[3];

	      REAL_VALUE_FROM_CONST_DOUBLE (r, operands[1]);
	      REAL_VALUE_TO_TARGET_LONG_DOUBLE (r, l);
	      operands[1] = GEN_INT (l[0]);
	      middlehalf[1] = GEN_INT (l[1]);
	      latehalf[1] = GEN_INT (l[2]);
	    }
	  else if (CONSTANT_P (operands[1]))
	    /* No non-CONST_DOUBLE constant should ever appear here.  */
	    abort ();
        }
      else
	{
	  middlehalf[1] = operands[1];
	  latehalf[1] = operands[1];
	}
    }
  else /* size is not 12: */
    {
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
	    {
	      /* ??? jrv: Can this really happen?  A DImode constant
		 that isn't a CONST_DOUBLE? */
	      if (GET_CODE (operands[1]) == CONST_INT
		  && INTVAL (operands[1]) < 0)
	        latehalf[1] = constm1_rtx;
	      else
	        latehalf[1] = const0_rtx;
	    }
	}
      else
	latehalf[1] = operands[1];
    }

  /* If insn is effectively movd N (sp),-(sp) then we will do the
     high word first.  We should use the adjusted operand 1
     (which is N+4 (sp) or N+8 (sp))
     for the low word and middle word as well,
     to compensate for the first decrement of sp.  */
  if (optype0 == PUSHOP
      && REGNO (XEXP (XEXP (operands[0], 0), 0)) == STACK_POINTER_REGNUM
      && reg_overlap_mentioned_p (stack_pointer_rtx, operands[1]))
    middlehalf[1] = operands[1] = latehalf[1];

  /* For (set (reg:DI N) (mem:DI ... (reg:SI N) ...)),
     if the upper part of reg N does not appear in the MEM, arrange to
     emit the move late-half first.  Otherwise, compute the MEM address
     into the upper part of N and use that as a pointer to the memory
     operand.  */
  if (optype0 == REGOP
      && (optype1 == OFFSOP || optype1 == MEMOP))
    {
      if (reg_mentioned_p (operands[0], XEXP (operands[1], 0))
	  && reg_mentioned_p (latehalf[0], XEXP (operands[1], 0)))
	{
	  /* If both halves of dest are used in the src memory address,
	     compute the address into latehalf of dest.  */
compadr:
	  xops[0] = latehalf[0];
	  xops[1] = XEXP (operands[1], 0);
	  output_asm_insn (AS2 (lea%L0,%a1,%0), xops);
	  if( GET_MODE (operands[1]) == XFmode )
	    {
/*	    abort (); */
	      operands[1] = gen_rtx (MEM, XFmode, latehalf[0]);
	      middlehalf[1] = adj_offsettable_operand (operands[1], size-8);
	      latehalf[1] = adj_offsettable_operand (operands[1], size-4);
	    }
	  else
	    {
	      operands[1] = gen_rtx (MEM, DImode, latehalf[0]);
	      latehalf[1] = adj_offsettable_operand (operands[1], size-4);
	    }
	}
      else if (size == 12
		 && reg_mentioned_p (middlehalf[0], XEXP (operands[1], 0)))
	{
	  /* Check for two regs used by both source and dest. */
	  if (reg_mentioned_p (operands[0], XEXP (operands[1], 0))
		|| reg_mentioned_p (latehalf[0], XEXP (operands[1], 0)))
		goto compadr;

	  /* JRV says this can't happen: */
	  if (addreg0 || addreg1)
	      abort();

	  /* Only the middle reg conflicts; simply put it last. */
	  output_asm_insn (singlemove_string (operands), operands);
	  output_asm_insn (singlemove_string (latehalf), latehalf);
	  output_asm_insn (singlemove_string (middlehalf), middlehalf);
	  return "";
	}
      else if (reg_mentioned_p (operands[0], XEXP (operands[1], 0)))
	/* If the low half of dest is mentioned in the source memory
	   address, the arrange to emit the move late half first.  */
	dest_overlapped_low = 1;
    }

  /* If one or both operands autodecrementing,
     do the two words, high-numbered first.  */

  /* Likewise,  the first move would clobber the source of the second one,
     do them in the other order.  This happens only for registers;
     such overlap can't happen in memory unless the user explicitly
     sets it up, and that is an undefined circumstance.  */

/*
  if (optype0 == PUSHOP || optype1 == PUSHOP
      || (optype0 == REGOP && optype1 == REGOP
	  && REGNO (operands[0]) == REGNO (latehalf[1]))
      || dest_overlapped_low)
*/
  if (optype0 == PUSHOP || optype1 == PUSHOP
      || (optype0 == REGOP && optype1 == REGOP
	  && ((middlehalf[1] && REGNO (operands[0]) == REGNO (middlehalf[1]))
	      || REGNO (operands[0]) == REGNO (latehalf[1])))
      || dest_overlapped_low)
    {
      /* Make any unoffsettable addresses point at high-numbered word.  */
      if (addreg0)
	asm_add (size-4, addreg0);
      if (addreg1)
	asm_add (size-4, addreg1);

      /* Do that word.  */
      output_asm_insn (singlemove_string (latehalf), latehalf);

      /* Undo the adds we just did.  */
      if (addreg0)
         asm_add (-4, addreg0);
      if (addreg1)
	asm_add (-4, addreg1);

      if (size == 12)
        {
        output_asm_insn (singlemove_string (middlehalf), middlehalf);
        if (addreg0)
           asm_add (-4, addreg0);
        if (addreg1)
	   asm_add (-4, addreg1);
	}

      /* Do low-numbered word.  */
      return singlemove_string (operands);
    }

  /* Normal case: do the two words, low-numbered first.  */

  output_asm_insn (singlemove_string (operands), operands);

  /* Do the middle one of the three words for long double */
  if (size == 12)
    {
      if (addreg0)
        asm_add (4, addreg0);
      if (addreg1)
        asm_add (4, addreg1);

      output_asm_insn (singlemove_string (middlehalf), middlehalf);
    }

  /* Make any unoffsettable addresses point at high-numbered word.  */
  if (addreg0)
    asm_add (4, addreg0);
  if (addreg1)
    asm_add (4, addreg1);

  /* Do that word.  */
  output_asm_insn (singlemove_string (latehalf), latehalf);

  /* Undo the adds we just did.  */
  if (addreg0)
    asm_add (4-size, addreg0);
  if (addreg1)
    asm_add (4-size, addreg1);

  return "";
}


#define MAX_TMPS 2		/* max temporary registers used */

/* Output the appropriate code to move push memory on the stack */

char *
output_move_pushmem (operands, insn, length, tmp_start, n_operands)
     rtx operands[];
     rtx insn;
     int length;
     int tmp_start;
     int n_operands;
{

  struct {
    char *load;
    char *push;
    rtx   xops[2];
  } tmp_info[MAX_TMPS];

  rtx src = operands[1];
  int max_tmps = 0;
  int offset = 0;
  int stack_p = reg_overlap_mentioned_p (stack_pointer_rtx, src);
  int stack_offset = 0;
  int i, num_tmps;
  rtx xops[1];

  if (!offsettable_memref_p (src))
    fatal_insn ("Source is not offsettable", insn);

  if ((length & 3) != 0)
    fatal_insn ("Pushing non-word aligned size", insn);

  /* Figure out which temporary registers we have available */
  for (i = tmp_start; i < n_operands; i++)
    {
      if (GET_CODE (operands[i]) == REG)
	{
	  if (reg_overlap_mentioned_p (operands[i], src))
	    continue;

	  tmp_info[ max_tmps++ ].xops[1] = operands[i];
	  if (max_tmps == MAX_TMPS)
	    break;
	}
    }

  if (max_tmps == 0)
    for (offset = length - 4; offset >= 0; offset -= 4)
      {
	xops[0] = adj_offsettable_operand (src, offset + stack_offset);
	output_asm_insn (AS1(push%L0,%0), xops);
	if (stack_p)
	  stack_offset += 4;
      }

  else
    for (offset = length - 4; offset >= 0; )
      {
	for (num_tmps = 0; num_tmps < max_tmps && offset >= 0; num_tmps++)
	  {
	    tmp_info[num_tmps].load    = AS2(mov%L0,%0,%1);
	    tmp_info[num_tmps].push    = AS1(push%L0,%1);
	    tmp_info[num_tmps].xops[0] = adj_offsettable_operand (src, offset + stack_offset);
	    offset -= 4;
	  }

	for (i = 0; i < num_tmps; i++)
	  output_asm_insn (tmp_info[i].load, tmp_info[i].xops);

	for (i = 0; i < num_tmps; i++)
	  output_asm_insn (tmp_info[i].push, tmp_info[i].xops);

	if (stack_p)
	  stack_offset += 4*num_tmps;
      }

  return "";
}



/* Output the appropriate code to move data between two memory locations */

char *
output_move_memory (operands, insn, length, tmp_start, n_operands)
     rtx operands[];
     rtx insn;
     int length;
     int tmp_start;
     int n_operands;
{
  struct {
    char *load;
    char *store;
    rtx   xops[3];
  } tmp_info[MAX_TMPS];

  rtx dest = operands[0];
  rtx src  = operands[1];
  rtx qi_tmp = NULL_RTX;
  int max_tmps = 0;
  int offset = 0;
  int i, num_tmps;
  rtx xops[3];

  if (GET_CODE (dest) == MEM
      && GET_CODE (XEXP (dest, 0)) == PRE_INC
      && XEXP (XEXP (dest, 0), 0) == stack_pointer_rtx)
    return output_move_pushmem (operands, insn, length, tmp_start, n_operands);

  if (!offsettable_memref_p (src))
    fatal_insn ("Source is not offsettable", insn);

  if (!offsettable_memref_p (dest))
    fatal_insn ("Destination is not offsettable", insn);

  /* Figure out which temporary registers we have available */
  for (i = tmp_start; i < n_operands; i++)
    {
      if (GET_CODE (operands[i]) == REG)
	{
	  if ((length & 1) != 0 && !qi_tmp && QI_REG_P (operands[i]))
	    qi_tmp = operands[i];

	  if (reg_overlap_mentioned_p (operands[i], dest))
	    fatal_insn ("Temporary register overlaps the destination", insn);

	  if (reg_overlap_mentioned_p (operands[i], src))
	    fatal_insn ("Temporary register overlaps the source", insn);

	  tmp_info[ max_tmps++ ].xops[2] = operands[i];
	  if (max_tmps == MAX_TMPS)
	    break;
	}
    }

  if (max_tmps == 0)
    fatal_insn ("No scratch registers were found to do memory->memory moves", insn);

  if ((length & 1) != 0)
    {
      if (!qi_tmp)
	fatal_insn ("No byte register found when moving odd # of bytes.", insn);
    }

  while (length > 1)
    {
      for (num_tmps = 0; num_tmps < max_tmps; num_tmps++)
	{
	  if (length >= 4)
	    {
	      tmp_info[num_tmps].load    = AS2(mov%L0,%1,%2);
	      tmp_info[num_tmps].store   = AS2(mov%L0,%2,%0);
	      tmp_info[num_tmps].xops[0] = adj_offsettable_operand (dest, offset);
	      tmp_info[num_tmps].xops[1] = adj_offsettable_operand (src, offset);
	      offset += 4;
	      length -= 4;
	    }
	  else if (length >= 2)
	    {
	      tmp_info[num_tmps].load    = AS2(mov%W0,%1,%2);
	      tmp_info[num_tmps].store   = AS2(mov%W0,%2,%0);
	      tmp_info[num_tmps].xops[0] = adj_offsettable_operand (dest, offset);
	      tmp_info[num_tmps].xops[1] = adj_offsettable_operand (src, offset);
	      offset += 2;
	      length -= 2;
	    }
	  else
	    break;
	}

      for (i = 0; i < num_tmps; i++)
	output_asm_insn (tmp_info[i].load, tmp_info[i].xops);

      for (i = 0; i < num_tmps; i++)
	output_asm_insn (tmp_info[i].store, tmp_info[i].xops);
    }

  if (length == 1)
    {
      xops[0] = adj_offsettable_operand (dest, offset);
      xops[1] = adj_offsettable_operand (src, offset);
      xops[2] = qi_tmp;
      output_asm_insn (AS2(mov%B0,%1,%2), xops);
      output_asm_insn (AS2(mov%B0,%2,%0), xops);
    }

  return "";
}


int
standard_80387_constant_p (x)
     rtx x;
{
#if ! defined (REAL_IS_NOT_DOUBLE) || defined (REAL_ARITHMETIC)
  REAL_VALUE_TYPE d;
  jmp_buf handler;
  int is0, is1;

  if (setjmp (handler))
    return 0;

  set_float_handler (handler);
  REAL_VALUE_FROM_CONST_DOUBLE (d, x);
  is0 = REAL_VALUES_EQUAL (d, dconst0);
  is1 = REAL_VALUES_EQUAL (d, dconst1);
  set_float_handler (NULL_PTR);

  if (is0)
    return 1;

  if (is1)
    return 2;

  /* Note that on the 80387, other constants, such as pi,
     are much slower to load as standard constants
     than to load from doubles in memory!  */
#endif

  return 0;
}

char *
output_move_const_single (operands)
     rtx *operands;
{
  if (FP_REG_P (operands[0]))
    {
      int conval = standard_80387_constant_p (operands[1]);

      if (conval == 1)
	return "fldz";

      if (conval == 2)
	return "fld1";
    }
  if (GET_CODE (operands[1]) == CONST_DOUBLE)
    {
      REAL_VALUE_TYPE r; long l;

      if (GET_MODE (operands[1]) == XFmode)
	abort ();

      REAL_VALUE_FROM_CONST_DOUBLE (r, operands[1]);
      REAL_VALUE_TO_TARGET_SINGLE (r, l);
      operands[1] = GEN_INT (l);
    }
  return singlemove_string (operands);
}

/* Returns 1 if OP is either a symbol reference or a sum of a symbol
   reference and a constant.  */

int
symbolic_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  switch (GET_CODE (op))
    {
    case SYMBOL_REF:
    case LABEL_REF:
      return 1;
    case CONST:
      op = XEXP (op, 0);
      return ((GET_CODE (XEXP (op, 0)) == SYMBOL_REF
	       || GET_CODE (XEXP (op, 0)) == LABEL_REF)
	      && GET_CODE (XEXP (op, 1)) == CONST_INT);
    default:
      return 0;
    }
}

/* Test for a valid operand for a call instruction.
   Don't allow the arg pointer register or virtual regs
   since they may change into reg + const, which the patterns
   can't handle yet.  */

int
call_insn_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == MEM
      && ((CONSTANT_ADDRESS_P (XEXP (op, 0))
	   /* This makes a difference for PIC.  */
	   && general_operand (XEXP (op, 0), Pmode))
	  || (GET_CODE (XEXP (op, 0)) == REG
	      && XEXP (op, 0) != arg_pointer_rtx
	      && !(REGNO (XEXP (op, 0)) >= FIRST_PSEUDO_REGISTER
		   && REGNO (XEXP (op, 0)) <= LAST_VIRTUAL_REGISTER))))
    return 1;
  return 0;
}

/* Like call_insn_operand but allow (mem (symbol_ref ...))
   even if pic.  */

int
expander_call_insn_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == MEM
      && (CONSTANT_ADDRESS_P (XEXP (op, 0))
	  || (GET_CODE (XEXP (op, 0)) == REG
	      && XEXP (op, 0) != arg_pointer_rtx
	      && !(REGNO (XEXP (op, 0)) >= FIRST_PSEUDO_REGISTER
		   && REGNO (XEXP (op, 0)) <= LAST_VIRTUAL_REGISTER))))
    return 1;
  return 0;
}

/* Returns 1 if OP contains a symbol reference */

int
symbolic_reference_mentioned_p (op)
     rtx op;
{
  register char *fmt;
  register int i;

  if (GET_CODE (op) == SYMBOL_REF || GET_CODE (op) == LABEL_REF)
    return 1;

  fmt = GET_RTX_FORMAT (GET_CODE (op));
  for (i = GET_RTX_LENGTH (GET_CODE (op)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  register int j;

	  for (j = XVECLEN (op, i) - 1; j >= 0; j--)
	    if (symbolic_reference_mentioned_p (XVECEXP (op, i, j)))
	      return 1;
	}
      else if (fmt[i] == 'e' && symbolic_reference_mentioned_p (XEXP (op, i)))
	return 1;
    }

  return 0;
}

/* This function generates the assembly code for function entry.
   FILE is an stdio stream to output the code to.
   SIZE is an int: how many units of temporary storage to allocate. */

void
function_prologue (file, size)
     FILE *file;
     int size;
{
  register int regno;
  int limit;
  rtx xops[4];
  int pic_reg_used = flag_pic && (current_function_uses_pic_offset_table
				  || current_function_uses_const_pool);

  xops[0] = stack_pointer_rtx;
  xops[1] = frame_pointer_rtx;
  xops[2] = GEN_INT (size);
  if (frame_pointer_needed)
    {
      output_asm_insn ("push%L1 %1", xops);
      output_asm_insn (AS2 (mov%L0,%0,%1), xops);
    }

  if (size)
    output_asm_insn (AS2 (sub%L0,%2,%0), xops);

  /* Note If use enter it is NOT reversed args.
     This one is not reversed from intel!!
     I think enter is slower.  Also sdb doesn't like it.
     But if you want it the code is:
     {
     xops[3] = const0_rtx;
     output_asm_insn ("enter %2,%3", xops);
     }
     */
  limit = (frame_pointer_needed ? FRAME_POINTER_REGNUM : STACK_POINTER_REGNUM);
  for (regno = limit - 1; regno >= 0; regno--)
    if ((regs_ever_live[regno] && ! call_used_regs[regno])
	|| (regno == PIC_OFFSET_TABLE_REGNUM && pic_reg_used))
      {
	xops[0] = gen_rtx (REG, SImode, regno);
	output_asm_insn ("push%L0 %0", xops);
      }

  if (pic_reg_used)
    {
      xops[0] = pic_offset_table_rtx;
      xops[1] = (rtx) gen_label_rtx ();

      output_asm_insn (AS1 (call,%P1), xops);
      ASM_OUTPUT_INTERNAL_LABEL (file, "L", CODE_LABEL_NUMBER (xops[1]));
      output_asm_insn (AS1 (pop%L0,%0), xops);
      output_asm_insn ("addl $_GLOBAL_OFFSET_TABLE_+[.-%P1],%0", xops);
    }
}

/* Return 1 if it is appropriate to emit `ret' instructions in the
   body of a function.  Do this only if the epilogue is simple, needing a
   couple of insns.  Prior to reloading, we can't tell how many registers
   must be saved, so return 0 then.

   If NON_SAVING_SETJMP is defined and true, then it is not possible
   for the epilogue to be simple, so return 0.  This is a special case
   since NON_SAVING_SETJMP will not cause regs_ever_live to change until
   final, but jump_optimize may need to know sooner if a `return' is OK.  */

int
simple_386_epilogue ()
{
  int regno;
  int nregs = 0;
  int reglimit = (frame_pointer_needed
		  ? FRAME_POINTER_REGNUM : STACK_POINTER_REGNUM);
  int pic_reg_used = flag_pic && (current_function_uses_pic_offset_table
				  || current_function_uses_const_pool);

#ifdef NON_SAVING_SETJMP
  if (NON_SAVING_SETJMP && current_function_calls_setjmp)
    return 0;
#endif

  if (! reload_completed)
    return 0;

  for (regno = reglimit - 1; regno >= 0; regno--)
    if ((regs_ever_live[regno] && ! call_used_regs[regno])
	|| (regno == PIC_OFFSET_TABLE_REGNUM && pic_reg_used))
      nregs++;

  return nregs == 0 || ! frame_pointer_needed;
}


/* This function generates the assembly code for function exit.
   FILE is an stdio stream to output the code to.
   SIZE is an int: how many units of temporary storage to deallocate. */

void
function_epilogue (file, size)
     FILE *file;
     int size;
{
  register int regno;
  register int nregs, limit;
  int offset;
  rtx xops[3];
  int pic_reg_used = flag_pic && (current_function_uses_pic_offset_table
				  || current_function_uses_const_pool);

  /* Compute the number of registers to pop */

  limit = (frame_pointer_needed
	   ? FRAME_POINTER_REGNUM
	   : STACK_POINTER_REGNUM);

  nregs = 0;

  for (regno = limit - 1; regno >= 0; regno--)
    if ((regs_ever_live[regno] && ! call_used_regs[regno])
	|| (regno == PIC_OFFSET_TABLE_REGNUM && pic_reg_used))
      nregs++;

  /* sp is often  unreliable so we must go off the frame pointer,
   */

  /* In reality, we may not care if sp is unreliable, because we can
     restore the register relative to the frame pointer.  In theory,
     since each move is the same speed as a pop, and we don't need the
     leal, this is faster.  For now restore multiple registers the old
     way. */

  offset = -size - (nregs * UNITS_PER_WORD);

  xops[2] = stack_pointer_rtx;

  if (nregs > 1 || ! frame_pointer_needed)
    {
      if (frame_pointer_needed)
	{
	  xops[0] = adj_offsettable_operand (AT_BP (Pmode), offset);
	  output_asm_insn (AS2 (lea%L2,%0,%2), xops);
	}

      for (regno = 0; regno < limit; regno++)
	if ((regs_ever_live[regno] && ! call_used_regs[regno])
	    || (regno == PIC_OFFSET_TABLE_REGNUM && pic_reg_used))
	  {
	    xops[0] = gen_rtx (REG, SImode, regno);
	    output_asm_insn ("pop%L0 %0", xops);
	  }
    }
  else
    for (regno = 0; regno < limit; regno++)
      if ((regs_ever_live[regno] && ! call_used_regs[regno])
	  || (regno == PIC_OFFSET_TABLE_REGNUM && pic_reg_used))
	{
	  xops[0] = gen_rtx (REG, SImode, regno);
	  xops[1] = adj_offsettable_operand (AT_BP (Pmode), offset);
	  output_asm_insn (AS2 (mov%L0,%1,%0), xops);
	  offset += 4;
	}

  if (frame_pointer_needed)
    {
      /* On i486, mov & pop is faster than "leave". */

      if (!TARGET_386)
	{
	  xops[0] = frame_pointer_rtx;
	  output_asm_insn (AS2 (mov%L2,%0,%2), xops);
	  output_asm_insn ("pop%L0 %0", xops);
	}
      else
	output_asm_insn ("leave", xops);
    }
  else if (size)
    {
      /* If there is no frame pointer, we must still release the frame. */

      xops[0] = GEN_INT (size);
      output_asm_insn (AS2 (add%L2,%0,%2), xops);
    }

  if (current_function_pops_args && current_function_args_size)
    {
      xops[1] = GEN_INT (current_function_pops_args);

      /* i386 can only pop 32K bytes (maybe 64K?  Is it signed?).  If
	 asked to pop more, pop return address, do explicit add, and jump
	 indirectly to the caller. */

      if (current_function_pops_args >= 32768)
	{
	  /* ??? Which register to use here? */
	  xops[0] = gen_rtx (REG, SImode, 2);
	  output_asm_insn ("pop%L0 %0", xops);
	  output_asm_insn (AS2 (add%L2,%1,%2), xops);
	  output_asm_insn ("jmp %*%0", xops);
	}
      else
	  output_asm_insn ("ret %1", xops);
    }
  else
    output_asm_insn ("ret", xops);
}


/* GO_IF_LEGITIMATE_ADDRESS recognizes an RTL expression
   that is a valid memory address for an instruction.
   The MODE argument is the machine mode for the MEM expression
   that wants to use this address.

   On x86, legitimate addresses are:
	base				movl (base),reg
	displacement			movl disp,reg
	base + displacement		movl disp(base),reg
	index + base			movl (base,index),reg
	(index + base) + displacement	movl disp(base,index),reg
	index*scale			movl (,index,scale),reg
	index*scale + disp		movl disp(,index,scale),reg
	index*scale + base 		movl (base,index,scale),reg
	(index*scale + base) + disp	movl disp(base,index,scale),reg

	In each case, scale can be 1, 2, 4, 8.  */

/* This is exactly the same as print_operand_addr, except that
   it recognizes addresses instead of printing them.

   It only recognizes address in canonical form.  LEGITIMIZE_ADDRESS should
   convert common non-canonical forms to canonical form so that they will
   be recognized.  */

#define ADDR_INVALID(msg,insn)						\
do {									\
  if (TARGET_DEBUG_ADDR)						\
    {									\
      fprintf (stderr, msg);						\
      debug_rtx (insn);							\
    }									\
} while (0)

int
legitimate_address_p (mode, addr, strict)
     enum machine_mode mode;
     register rtx addr;
     int strict;
{
  rtx base  = NULL_RTX;
  rtx indx  = NULL_RTX;
  rtx scale = NULL_RTX;
  rtx disp  = NULL_RTX;

  if (TARGET_DEBUG_ADDR)
    {
      fprintf (stderr,
	       "\n==========\nGO_IF_LEGITIMATE_ADDRESS, mode = %s, strict = %d\n",
	       GET_MODE_NAME (mode), strict);

      debug_rtx (addr);
    }

  if (GET_CODE (addr) == REG || GET_CODE (addr) == SUBREG)
      base = addr;				/* base reg */

  else if (GET_CODE (addr) == PLUS)
    {
      rtx op0 = XEXP (addr, 0);
      rtx op1 = XEXP (addr, 1);
      enum rtx_code code0 = GET_CODE (op0);
      enum rtx_code code1 = GET_CODE (op1);

      if (code0 == REG || code0 == SUBREG)
	{
	  if (code1 == REG || code1 == SUBREG)
	    {
	      indx = op0;			/* index + base */
	      base = op1;
	    }

	  else
	    {
	      base = op0;			/* base + displacement */
	      disp = op1;
	    }
	}

      else if (code0 == MULT)
	{
	  indx  = XEXP (op0, 0);
	  scale = XEXP (op0, 1);

	  if (code1 == REG || code1 == SUBREG)
	    base = op1;				/* index*scale + base */

	  else
	    disp = op1;				/* index*scale + disp */
	}

      else if (code0 == PLUS && GET_CODE (XEXP (op0, 0)) == MULT)
	{
	  indx  = XEXP (XEXP (op0, 0), 0);	/* index*scale + base + disp */
	  scale = XEXP (XEXP (op0, 0), 1);
	  base  = XEXP (op0, 1);
	  disp  = op1;
	}

      else if (code0 == PLUS)
	{
	  indx = XEXP (op0, 0);			/* index + base + disp */
	  base = XEXP (op0, 1);
	  disp = op1;
	}

      else
	{
	  ADDR_INVALID ("PLUS subcode is not valid.\n", op0);
	  return FALSE;
	}
    }

  else if (GET_CODE (addr) == MULT)
    {
      indx  = XEXP (addr, 0);			/* index*scale */
      scale = XEXP (addr, 1);
    }

  else
    disp = addr;				/* displacement */

  /* Allow arg pointer and stack pointer as index if there is not scaling */
  if (base && indx && !scale
      && (indx == arg_pointer_rtx || indx == stack_pointer_rtx))
    {
      rtx tmp = base;
      base = indx;
      indx = tmp;
    }

  /* Validate base register */
  /* Don't allow SUBREG's here, it can lead to spill failures when the base
     is one word out of a two word structure, which is represented internally
     as a DImode int.  */
  if (base)
    {
      if (GET_CODE (base) != REG)
	{
	  ADDR_INVALID ("Base is not a register.\n", base);
	  return FALSE;
	}

      if ((strict && !REG_OK_FOR_BASE_STRICT_P (base))
	  || (!strict && !REG_OK_FOR_BASE_NONSTRICT_P (base)))
	{
	  ADDR_INVALID ("Base is not valid.\n", base);
	  return FALSE;
	}
    }

  /* Validate index register */
  /* Don't allow SUBREG's here, it can lead to spill failures when the index
     is one word out of a two word structure, which is represented internally
     as a DImode int.  */
  if (indx)
    {
      if (GET_CODE (indx) != REG)
	{
	  ADDR_INVALID ("Index is not a register.\n", indx);
	  return FALSE;
	}

      if ((strict && !REG_OK_FOR_INDEX_STRICT_P (indx))
	  || (!strict && !REG_OK_FOR_INDEX_NONSTRICT_P (indx)))
	{
	  ADDR_INVALID ("Index is not valid.\n", indx);
	  return FALSE;
	}
    }
  else if (scale)
    abort ();					/* scale w/o index illegal */

  /* Validate scale factor */
  if (scale)
    {
      HOST_WIDE_INT value;

      if (GET_CODE (scale) != CONST_INT)
	{
	  ADDR_INVALID ("Scale is not valid.\n", scale);
	  return FALSE;
	}

      value = INTVAL (scale);
      if (value != 1 && value != 2 && value != 4 && value != 8)
	{
	  ADDR_INVALID ("Scale is not a good multiplier.\n", scale);
	  return FALSE;
	}
    }

  /* Validate displacement */
  if (disp)
    {
      if (!CONSTANT_ADDRESS_P (disp))
	{
	  ADDR_INVALID ("Displacement is not valid.\n", disp);
	  return FALSE;
	}

      if (GET_CODE (disp) == CONST_DOUBLE)
	{
	  ADDR_INVALID ("Displacement is a const_double.\n", disp);
	  return FALSE;
	}

      if (flag_pic && SYMBOLIC_CONST (disp) && base != pic_offset_table_rtx
	  && (indx != pic_offset_table_rtx || scale != NULL_RTX))
	{
	  ADDR_INVALID ("Displacement is an invalid pic reference.\n", disp);
	  return FALSE;
	}

      if (HALF_PIC_P () && HALF_PIC_ADDRESS_P (disp)
	  && (base != NULL_RTX || indx != NULL_RTX))
	{
	  ADDR_INVALID ("Displacement is an invalid half-pic reference.\n", disp);
	  return FALSE;
	}
    }

  if (TARGET_DEBUG_ADDR)
    fprintf (stderr, "Address is valid.\n");

  /* Everything looks valid, return true */
  return TRUE;
}


/* Return a legitimate reference for ORIG (an address) using the
   register REG.  If REG is 0, a new pseudo is generated.

   There are three types of references that must be handled:

   1. Global data references must load the address from the GOT, via
      the PIC reg.  An insn is emitted to do this load, and the reg is
      returned.

   2. Static data references must compute the address as an offset
      from the GOT, whose base is in the PIC reg.  An insn is emitted to
      compute the address into a reg, and the reg is returned.  Static
      data objects have SYMBOL_REF_FLAG set to differentiate them from
      global data objects.

   3. Constant pool addresses must be handled special.  They are
      considered legitimate addresses, but only if not used with regs.
      When printed, the output routines know to print the reference with the
      PIC reg, even though the PIC reg doesn't appear in the RTL.

   GO_IF_LEGITIMATE_ADDRESS rejects symbolic references unless the PIC
   reg also appears in the address (except for constant pool references,
   noted above).

   "switch" statements also require special handling when generating
   PIC code.  See comments by the `casesi' insn in i386.md for details.  */

rtx
legitimize_pic_address (orig, reg)
     rtx orig;
     rtx reg;
{
  rtx addr = orig;
  rtx new = orig;

  if (GET_CODE (addr) == SYMBOL_REF || GET_CODE (addr) == LABEL_REF)
    {
      if (GET_CODE (addr) == SYMBOL_REF && CONSTANT_POOL_ADDRESS_P (addr))
	reg = new = orig;
      else
	{
	  if (reg == 0)
	    reg = gen_reg_rtx (Pmode);

	  if ((GET_CODE (addr) == SYMBOL_REF && SYMBOL_REF_FLAG (addr))
	      || GET_CODE (addr) == LABEL_REF)
	    new = gen_rtx (PLUS, Pmode, pic_offset_table_rtx, orig);
	  else
	    new = gen_rtx (MEM, Pmode,
			   gen_rtx (PLUS, Pmode,
				    pic_offset_table_rtx, orig));

	  emit_move_insn (reg, new);
	}
      current_function_uses_pic_offset_table = 1;
      return reg;
    }
  else if (GET_CODE (addr) == CONST || GET_CODE (addr) == PLUS)
    {
      rtx base;

      if (GET_CODE (addr) == CONST)
	{
	  addr = XEXP (addr, 0);
	  if (GET_CODE (addr) != PLUS)
	    abort ();
	}

      if (XEXP (addr, 0) == pic_offset_table_rtx)
	return orig;

      if (reg == 0)
	reg = gen_reg_rtx (Pmode);

      base = legitimize_pic_address (XEXP (addr, 0), reg);
      addr = legitimize_pic_address (XEXP (addr, 1),
				     base == reg ? NULL_RTX : reg);

      if (GET_CODE (addr) == CONST_INT)
	return plus_constant (base, INTVAL (addr));

      if (GET_CODE (addr) == PLUS && CONSTANT_P (XEXP (addr, 1)))
	{
	  base = gen_rtx (PLUS, Pmode, base, XEXP (addr, 0));
	  addr = XEXP (addr, 1);
	}
	return gen_rtx (PLUS, Pmode, base, addr);
    }
  return new;
}


/* Emit insns to move operands[1] into operands[0].  */

void
emit_pic_move (operands, mode)
     rtx *operands;
     enum machine_mode mode;
{
  rtx temp = reload_in_progress ? operands[0] : gen_reg_rtx (Pmode);

  if (GET_CODE (operands[0]) == MEM && SYMBOLIC_CONST (operands[1]))
    operands[1] = (rtx) force_reg (SImode, operands[1]);
  else
    operands[1] = legitimize_pic_address (operands[1], temp);
}


/* Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new, valid address.
   This macro is used in only one place: `memory_address' in explow.c.

   OLDX is the address as it was before break_out_memory_refs was called.
   In some cases it is useful to look at this to decide what needs to be done.

   MODE and WIN are passed so that this macro can use
   GO_IF_LEGITIMATE_ADDRESS.

   It is always safe for this macro to do nothing.  It exists to recognize
   opportunities to optimize the output.

   For the 80386, we handle X+REG by loading X into a register R and
   using R+REG.  R will go in a general reg and indexing will be used.
   However, if REG is a broken-out memory address or multiplication,
   nothing needs to be done because REG can certainly go in a general reg.

   When -fpic is used, special handling is needed for symbolic references.
   See comments by legitimize_pic_address in i386.c for details.  */

rtx
legitimize_address (x, oldx, mode)
     register rtx x;
     register rtx oldx;
     enum machine_mode mode;
{
  int changed = 0;
  unsigned log;

  if (TARGET_DEBUG_ADDR)
    {
      fprintf (stderr, "\n==========\nLEGITIMIZE_ADDRESS, mode = %s\n", GET_MODE_NAME (mode));
      debug_rtx (x);
    }

  if (flag_pic && SYMBOLIC_CONST (x))
    return legitimize_pic_address (x, 0);

  /* Canonicalize shifts by 0, 1, 2, 3 into multiply */
  if (GET_CODE (x) == ASHIFT
      && GET_CODE (XEXP (x, 1)) == CONST_INT
      && (log = (unsigned)exact_log2 (INTVAL (XEXP (x, 1)))) < 4)
    {
      changed = 1;
      x = gen_rtx (MULT, Pmode,
		   force_reg (Pmode, XEXP (x, 0)),
		   GEN_INT (1 << log));
    }

  if (GET_CODE (x) == PLUS)
    {
      /* Canonicalize shifts by 0, 1, 2, 3 into multiply */
      if (GET_CODE (XEXP (x, 0)) == ASHIFT
	  && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT
	  && (log = (unsigned)exact_log2 (INTVAL (XEXP (XEXP (x, 0), 1)))) < 4)
	{
	  changed = 1;
	  XEXP (x, 0) = gen_rtx (MULT, Pmode,
				 force_reg (Pmode, XEXP (XEXP (x, 0), 0)),
				 GEN_INT (1 << log));
	}

      if (GET_CODE (XEXP (x, 1)) == ASHIFT
	  && GET_CODE (XEXP (XEXP (x, 1), 1)) == CONST_INT
	  && (log = (unsigned)exact_log2 (INTVAL (XEXP (XEXP (x, 1), 1)))) < 4)
	{
	  changed = 1;
	  XEXP (x, 1) = gen_rtx (MULT, Pmode,
				 force_reg (Pmode, XEXP (XEXP (x, 1), 0)),
				 GEN_INT (1 << log));
	}

      /* Put multiply first if it isn't already */
      if (GET_CODE (XEXP (x, 1)) == MULT)
	{
	  rtx tmp = XEXP (x, 0);
	  XEXP (x, 0) = XEXP (x, 1);
	  XEXP (x, 1) = tmp;
	  changed = 1;
	}

      /* Canonicalize (plus (mult (reg) (const)) (plus (reg) (const)))
	 into (plus (plus (mult (reg) (const)) (reg)) (const)).  This can be
	 created by virtual register instantiation, register elimination, and
	 similar optimizations.  */
      if (GET_CODE (XEXP (x, 0)) == MULT && GET_CODE (XEXP (x, 1)) == PLUS)
	{
	  changed = 1;
	  x = gen_rtx (PLUS, Pmode,
		       gen_rtx (PLUS, Pmode, XEXP (x, 0), XEXP (XEXP (x, 1), 0)),
		       XEXP (XEXP (x, 1), 1));
	}

      /* Canonicalize (plus (plus (mult (reg) (const)) (plus (reg) (const))) const)
	 into (plus (plus (mult (reg) (const)) (reg)) (const)).  */
      else if (GET_CODE (x) == PLUS && GET_CODE (XEXP (x, 0)) == PLUS
	       && GET_CODE (XEXP (XEXP (x, 0), 0)) == MULT
	       && GET_CODE (XEXP (XEXP (x, 0), 1)) == PLUS
	       && CONSTANT_P (XEXP (x, 1)))
	{
	  rtx constant, other;

	  if (GET_CODE (XEXP (x, 1)) == CONST_INT)
	    {
	      constant = XEXP (x, 1);
	      other = XEXP (XEXP (XEXP (x, 0), 1), 1);
	    }
	  else if (GET_CODE (XEXP (XEXP (XEXP (x, 0), 1), 1)) == CONST_INT)
	    {
	      constant = XEXP (XEXP (XEXP (x, 0), 1), 1);
	      other = XEXP (x, 1);
	    }
	  else
	    constant = 0;

	  if (constant)
	    {
	      changed = 1;
	      x = gen_rtx (PLUS, Pmode,
			   gen_rtx (PLUS, Pmode, XEXP (XEXP (x, 0), 0),
				    XEXP (XEXP (XEXP (x, 0), 1), 0)),
			   plus_constant (other, INTVAL (constant)));
	    }
	}

      if (changed && legitimate_address_p (mode, x, FALSE))
	return x;

      if (GET_CODE (XEXP (x, 0)) == MULT)
	{
	  changed = 1;
	  XEXP (x, 0) = force_operand (XEXP (x, 0), 0);
	}

      if (GET_CODE (XEXP (x, 1)) == MULT)
	{
	  changed = 1;
	  XEXP (x, 1) = force_operand (XEXP (x, 1), 0);
	}

      if (changed
	  && GET_CODE (XEXP (x, 1)) == REG
	  && GET_CODE (XEXP (x, 0)) == REG)
	return x;

      if (flag_pic && SYMBOLIC_CONST (XEXP (x, 1)))
	{
	  changed = 1;
	  x = legitimize_pic_address (x, 0);
	}

      if (changed && legitimate_address_p (mode, x, FALSE))
	return x;

      if (GET_CODE (XEXP (x, 0)) == REG)
	{
	  register rtx temp = gen_reg_rtx (Pmode);
	  register rtx val  = force_operand (XEXP (x, 1), temp);
	  if (val != temp)
	    emit_move_insn (temp, val);

	  XEXP (x, 1) = temp;
	  return x;
	}

      else if (GET_CODE (XEXP (x, 1)) == REG)
	{
	  register rtx temp = gen_reg_rtx (Pmode);
	  register rtx val  = force_operand (XEXP (x, 0), temp);
	  if (val != temp)
	    emit_move_insn (temp, val);

	  XEXP (x, 0) = temp;
	  return x;
	}
    }

  return x;
}


/* Print an integer constant expression in assembler syntax.  Addition
   and subtraction are the only arithmetic that may appear in these
   expressions.  FILE is the stdio stream to write to, X is the rtx, and
   CODE is the operand print code from the output string.  */

static void
output_pic_addr_const (file, x, code)
     FILE *file;
     rtx x;
     int code;
{
  char buf[256];

  switch (GET_CODE (x))
    {
    case PC:
      if (flag_pic)
	putc ('.', file);
      else
	abort ();
      break;

    case SYMBOL_REF:
    case LABEL_REF:
      if (GET_CODE (x) == SYMBOL_REF)
	assemble_name (file, XSTR (x, 0));
      else
	{
	  ASM_GENERATE_INTERNAL_LABEL (buf, "L",
				       CODE_LABEL_NUMBER (XEXP (x, 0)));
	  assemble_name (asm_out_file, buf);
	}

      if (GET_CODE (x) == SYMBOL_REF && CONSTANT_POOL_ADDRESS_P (x))
	fprintf (file, "@GOTOFF(%%ebx)");
      else if (code == 'P')
	fprintf (file, "@PLT");
      else if (GET_CODE (x) == LABEL_REF)
	fprintf (file, "@GOTOFF");
      else if (! SYMBOL_REF_FLAG (x))
	fprintf (file, "@GOT");
      else
	fprintf (file, "@GOTOFF");

      break;

    case CODE_LABEL:
      ASM_GENERATE_INTERNAL_LABEL (buf, "L", CODE_LABEL_NUMBER (x));
      assemble_name (asm_out_file, buf);
      break;

    case CONST_INT:
      fprintf (file, "%d", INTVAL (x));
      break;

    case CONST:
      /* This used to output parentheses around the expression,
	 but that does not work on the 386 (either ATT or BSD assembler).  */
      output_pic_addr_const (file, XEXP (x, 0), code);
      break;

    case CONST_DOUBLE:
      if (GET_MODE (x) == VOIDmode)
	{
	  /* We can use %d if the number is <32 bits and positive.  */
	  if (CONST_DOUBLE_HIGH (x) || CONST_DOUBLE_LOW (x) < 0)
	    fprintf (file, "0x%x%08x",
		     CONST_DOUBLE_HIGH (x), CONST_DOUBLE_LOW (x));
	  else
	    fprintf (file, "%d", CONST_DOUBLE_LOW (x));
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
	  output_pic_addr_const (file, XEXP (x, 1), code);
	  if (INTVAL (XEXP (x, 0)) >= 0)
	    fprintf (file, "+");
	  output_pic_addr_const (file, XEXP (x, 0), code);
	}
      else
	{
	  output_pic_addr_const (file, XEXP (x, 0), code);
	  if (INTVAL (XEXP (x, 1)) >= 0)
	    fprintf (file, "+");
	  output_pic_addr_const (file, XEXP (x, 1), code);
	}
      break;

    case MINUS:
      output_pic_addr_const (file, XEXP (x, 0), code);
      fprintf (file, "-");
      output_pic_addr_const (file, XEXP (x, 1), code);
      break;

    default:
      output_operand_lossage ("invalid expression as operand");
    }
}

/* Meaning of CODE:
   f -- float insn (print a CONST_DOUBLE as a float rather than in hex).
   D,L,W,B,Q,S -- print the opcode suffix for specified size of operand.
   R -- print the prefix for register names.
   z -- print the opcode suffix for the size of the current operand.
   * -- print a star (in certain assembler syntax)
   w -- print the operand as if it's a "word" (HImode) even if it isn't.
   c -- don't print special prefixes before constant operands.
*/

void
print_operand (file, x, code)
     FILE *file;
     rtx x;
     int code;
{
  if (code)
    {
      switch (code)
	{
	case '*':
	  if (USE_STAR)
	    putc ('*', file);
	  return;

	case 'L':
	  PUT_OP_SIZE (code, 'l', file);
	  return;

	case 'W':
	  PUT_OP_SIZE (code, 'w', file);
	  return;

	case 'B':
	  PUT_OP_SIZE (code, 'b', file);
	  return;

	case 'Q':
	  PUT_OP_SIZE (code, 'l', file);
	  return;

	case 'S':
	  PUT_OP_SIZE (code, 's', file);
	  return;

	case 'T':
	  PUT_OP_SIZE (code, 't', file);
	  return;

	case 'z':
	  /* 387 opcodes don't get size suffixes if the operands are
	     registers. */

	  if (STACK_REG_P (x))
	    return;

	  /* this is the size of op from size of operand */
	  switch (GET_MODE_SIZE (GET_MODE (x)))
	    {
	    case 1:
	      PUT_OP_SIZE ('B', 'b', file);
	      return;

	    case 2:
	      PUT_OP_SIZE ('W', 'w', file);
	      return;

	    case 4:
	      if (GET_MODE (x) == SFmode)
		{
		  PUT_OP_SIZE ('S', 's', file);
		  return;
		}
	      else
		PUT_OP_SIZE ('L', 'l', file);
	      return;

	    case 12:
		  PUT_OP_SIZE ('T', 't', file);
		  return;

	    case 8:
	      if (GET_MODE_CLASS (GET_MODE (x)) == MODE_INT)
		{
#ifdef GAS_MNEMONICS
		  PUT_OP_SIZE ('Q', 'q', file);
		  return;
#else
		  PUT_OP_SIZE ('Q', 'l', file);	/* Fall through */
#endif
		}

	      PUT_OP_SIZE ('Q', 'l', file);
	      return;
	    }

	case 'b':
	case 'w':
	case 'k':
	case 'h':
	case 'y':
	case 'P':
	  break;

	default:
	  {
	    char str[50];

	    sprintf (str, "invalid operand code `%c'", code);
	    output_operand_lossage (str);
	  }
	}
    }
  if (GET_CODE (x) == REG)
    {
      PRINT_REG (x, code, file);
    }
  else if (GET_CODE (x) == MEM)
    {
      PRINT_PTR (x, file);
      if (CONSTANT_ADDRESS_P (XEXP (x, 0)))
	{
	  if (flag_pic)
	    output_pic_addr_const (file, XEXP (x, 0), code);
	  else
	    output_addr_const (file, XEXP (x, 0));
	}
      else
	output_address (XEXP (x, 0));
    }
  else if (GET_CODE (x) == CONST_DOUBLE && GET_MODE (x) == SFmode)
    {
      REAL_VALUE_TYPE r; long l;
      REAL_VALUE_FROM_CONST_DOUBLE (r, x);
      REAL_VALUE_TO_TARGET_SINGLE (r, l);
      PRINT_IMMED_PREFIX (file);
      fprintf (file, "0x%x", l);
    }
 /* These float cases don't actually occur as immediate operands. */
 else if (GET_CODE (x) == CONST_DOUBLE && GET_MODE (x) == DFmode)
    {
      REAL_VALUE_TYPE r; char dstr[30];
      REAL_VALUE_FROM_CONST_DOUBLE (r, x);
      REAL_VALUE_TO_DECIMAL (r, "%.22e", dstr);
      fprintf (file, "%s", dstr);
    }
  else if (GET_CODE (x) == CONST_DOUBLE && GET_MODE (x) == XFmode)
    {
      REAL_VALUE_TYPE r; char dstr[30];
      REAL_VALUE_FROM_CONST_DOUBLE (r, x);
      REAL_VALUE_TO_DECIMAL (r, "%.22e", dstr);
      fprintf (file, "%s", dstr);
    }
  else 
    {
      if (code != 'P')
	{
	  if (GET_CODE (x) == CONST_INT || GET_CODE (x) == CONST_DOUBLE)
	    PRINT_IMMED_PREFIX (file);
	  else if (GET_CODE (x) == CONST || GET_CODE (x) == SYMBOL_REF
		   || GET_CODE (x) == LABEL_REF)
	    PRINT_OFFSET_PREFIX (file);
	}
      if (flag_pic)
	output_pic_addr_const (file, x, code);
      else
	output_addr_const (file, x);
    }
}

/* Print a memory operand whose address is ADDR.  */

void
print_operand_address (file, addr)
     FILE *file;
     register rtx addr;
{
  register rtx reg1, reg2, breg, ireg;
  rtx offset;

  switch (GET_CODE (addr))
    {
    case REG:
      ADDR_BEG (file);
      fprintf (file, "%se", RP);
      fputs (hi_reg_name[REGNO (addr)], file);
      ADDR_END (file);
      break;

    case PLUS:
      reg1 = 0;
      reg2 = 0;
      ireg = 0;
      breg = 0;
      offset = 0;
      if (CONSTANT_ADDRESS_P (XEXP (addr, 0)))
	{
	  offset = XEXP (addr, 0);
	  addr = XEXP (addr, 1);
	}
      else if (CONSTANT_ADDRESS_P (XEXP (addr, 1)))
	{
	  offset = XEXP (addr, 1);
	  addr = XEXP (addr, 0);
	}
      if (GET_CODE (addr) != PLUS) ;
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
	  if (reg1 == 0) reg1 = addr;
	  else reg2 = addr;
	  addr = 0;
	}
      if (offset != 0)
	{
	  if (addr != 0) abort ();
	  addr = offset;
	}
      if ((reg1 && GET_CODE (reg1) == MULT)
	  || (reg2 != 0 && REGNO_OK_FOR_BASE_P (REGNO (reg2))))
	{
	  breg = reg2;
	  ireg = reg1;
	}
      else if (reg1 != 0 && REGNO_OK_FOR_BASE_P (REGNO (reg1)))
	{
	  breg = reg1;
	  ireg = reg2;
	}

      if (ireg != 0 || breg != 0)
	{
	  int scale = 1;

	  if (addr != 0)
	    {
	      if (flag_pic)
		output_pic_addr_const (file, addr, 0);

	      else if (GET_CODE (addr) == LABEL_REF)
		output_asm_label (addr);

	      else
		output_addr_const (file, addr);
	    }

  	  if (ireg != 0 && GET_CODE (ireg) == MULT)
	    {
	      scale = INTVAL (XEXP (ireg, 1));
	      ireg = XEXP (ireg, 0);
	    }

	  /* The stack pointer can only appear as a base register,
	     never an index register, so exchange the regs if it is wrong. */

	  if (scale == 1 && ireg && REGNO (ireg) == STACK_POINTER_REGNUM)
	    {
	      rtx tmp;

	      tmp = breg;
	      breg = ireg;
	      ireg = tmp;
	    }

	  /* output breg+ireg*scale */
	  PRINT_B_I_S (breg, ireg, scale, file);
	  break;
	}

    case MULT:
      {
	int scale;
	if (GET_CODE (XEXP (addr, 0)) == CONST_INT)
	  {
	    scale = INTVAL (XEXP (addr, 0));
	    ireg = XEXP (addr, 1);
	  }
	else
	  {
	    scale = INTVAL (XEXP (addr, 1));
	    ireg = XEXP (addr, 0);
	  }
	output_addr_const (file, const0_rtx);
	PRINT_B_I_S ((rtx) 0, ireg, scale, file);
      }
      break;

    default:
      if (GET_CODE (addr) == CONST_INT
	  && INTVAL (addr) < 0x8000
	  && INTVAL (addr) >= -0x8000)
	fprintf (file, "%d", INTVAL (addr));
      else
	{
	  if (flag_pic)
	    output_pic_addr_const (file, addr, 0);
	  else
	    output_addr_const (file, addr);
	}
    }
}

/* Set the cc_status for the results of an insn whose pattern is EXP.
   On the 80386, we assume that only test and compare insns, as well
   as SI, HI, & DI mode ADD, SUB, NEG, AND, IOR, XOR, ASHIFT,
   ASHIFTRT, and LSHIFTRT instructions set the condition codes usefully.
   Also, we assume that jumps, moves and sCOND don't affect the condition
   codes.  All else clobbers the condition codes, by assumption.

   We assume that ALL integer add, minus, etc. instructions effect the
   condition codes.  This MUST be consistent with i386.md.

   We don't record any float test or compare - the redundant test &
   compare check in final.c does not handle stack-like regs correctly. */

void
notice_update_cc (exp)
     rtx exp;
{
  if (GET_CODE (exp) == SET)
    {
      /* Jumps do not alter the cc's.  */
      if (SET_DEST (exp) == pc_rtx)
	return;
      /* Moving register or memory into a register:
	 it doesn't alter the cc's, but it might invalidate
	 the RTX's which we remember the cc's came from.
	 (Note that moving a constant 0 or 1 MAY set the cc's).  */
      if (REG_P (SET_DEST (exp))
	  && (REG_P (SET_SRC (exp)) || GET_CODE (SET_SRC (exp)) == MEM
	      || GET_RTX_CLASS (GET_CODE (SET_SRC (exp))) == '<'))
	{
	  if (cc_status.value1
	      && reg_overlap_mentioned_p (SET_DEST (exp), cc_status.value1))
	    cc_status.value1 = 0;
	  if (cc_status.value2
	      && reg_overlap_mentioned_p (SET_DEST (exp), cc_status.value2))
	    cc_status.value2 = 0;
	  return;
	}
      /* Moving register into memory doesn't alter the cc's.
	 It may invalidate the RTX's which we remember the cc's came from.  */
      if (GET_CODE (SET_DEST (exp)) == MEM
	  && (REG_P (SET_SRC (exp))
	      || GET_RTX_CLASS (GET_CODE (SET_SRC (exp))) == '<'))
	{
	  if (cc_status.value1 && GET_CODE (cc_status.value1) == MEM)
	    cc_status.value1 = 0;
	  if (cc_status.value2 && GET_CODE (cc_status.value2) == MEM)
	    cc_status.value2 = 0;
	  return;
	}
      /* Function calls clobber the cc's.  */
      else if (GET_CODE (SET_SRC (exp)) == CALL)
	{
	  CC_STATUS_INIT;
	  return;
	}
      /* Tests and compares set the cc's in predictable ways.  */
      else if (SET_DEST (exp) == cc0_rtx)
	{
	  CC_STATUS_INIT;
	  cc_status.value1 = SET_SRC (exp);
	  return;
	}
      /* Certain instructions effect the condition codes. */
      else if (GET_MODE (SET_SRC (exp)) == SImode
	       || GET_MODE (SET_SRC (exp)) == HImode
	       || GET_MODE (SET_SRC (exp)) == QImode)
	switch (GET_CODE (SET_SRC (exp)))
	  {
	  case ASHIFTRT: case LSHIFTRT:
	  case ASHIFT:
	    /* Shifts on the 386 don't set the condition codes if the
	       shift count is zero. */
	    if (GET_CODE (XEXP (SET_SRC (exp), 1)) != CONST_INT)
	      {
		CC_STATUS_INIT;
		break;
	      }
	    /* We assume that the CONST_INT is non-zero (this rtx would
	       have been deleted if it were zero. */

	  case PLUS: case MINUS: case NEG:
	  case AND: case IOR: case XOR:
	    cc_status.flags = CC_NO_OVERFLOW;
	    cc_status.value1 = SET_SRC (exp);
	    cc_status.value2 = SET_DEST (exp);
	    break;

	  default:
	    CC_STATUS_INIT;
	  }
      else
	{
	  CC_STATUS_INIT;
	}
    }
  else if (GET_CODE (exp) == PARALLEL
	   && GET_CODE (XVECEXP (exp, 0, 0)) == SET)
    {
      if (SET_DEST (XVECEXP (exp, 0, 0)) == pc_rtx)
	return;
      if (SET_DEST (XVECEXP (exp, 0, 0)) == cc0_rtx)
	{
	  CC_STATUS_INIT;
	  if (stack_regs_mentioned_p (SET_SRC (XVECEXP (exp, 0, 0))))
	    cc_status.flags |= CC_IN_80387;
	  else
	    cc_status.value1 = SET_SRC (XVECEXP (exp, 0, 0));
	  return;
	}
      CC_STATUS_INIT;
    }
  else
    {
      CC_STATUS_INIT;
    }
}

/* Split one or more DImode RTL references into pairs of SImode
   references.  The RTL can be REG, offsettable MEM, integer constant, or
   CONST_DOUBLE.  "operands" is a pointer to an array of DImode RTL to
   split and "num" is its length.  lo_half and hi_half are output arrays
   that parallel "operands". */

void
split_di (operands, num, lo_half, hi_half)
     rtx operands[];
     int num;
     rtx lo_half[], hi_half[];
{
  while (num--)
    {
      if (GET_CODE (operands[num]) == REG)
	{
	  lo_half[num] = gen_rtx (REG, SImode, REGNO (operands[num]));
	  hi_half[num] = gen_rtx (REG, SImode, REGNO (operands[num]) + 1);
	}
      else if (CONSTANT_P (operands[num]))
	{
	  split_double (operands[num], &lo_half[num], &hi_half[num]);
	}
      else if (offsettable_memref_p (operands[num]))
	{
	  lo_half[num] = operands[num];
	  hi_half[num] = adj_offsettable_operand (operands[num], 4);
	}
      else
	abort();
    }
}

/* Return 1 if this is a valid binary operation on a 387.
   OP is the expression matched, and MODE is its mode. */

int
binary_387_op (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  if (mode != VOIDmode && mode != GET_MODE (op))
    return 0;

  switch (GET_CODE (op))
    {
    case PLUS:
    case MINUS:
    case MULT:
    case DIV:
      return GET_MODE_CLASS (GET_MODE (op)) == MODE_FLOAT;

    default:
      return 0;
    }
}


/* Return 1 if this is a valid shift or rotate operation on a 386.
   OP is the expression matched, and MODE is its mode. */

int
shift_op (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  rtx operand = XEXP (op, 0);

  if (mode != VOIDmode && mode != GET_MODE (op))
    return 0;

  if (GET_MODE (operand) != GET_MODE (op)
      || GET_MODE_CLASS (GET_MODE (op)) != MODE_INT)
    return 0;

  return (GET_CODE (op) == ASHIFT
	  || GET_CODE (op) == ASHIFTRT
	  || GET_CODE (op) == LSHIFTRT
	  || GET_CODE (op) == ROTATE
	  || GET_CODE (op) == ROTATERT);
}

/* Return 1 if OP is COMPARE rtx with mode VOIDmode.
   MODE is not used.  */

int
VOIDmode_compare_op (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  return GET_CODE (op) == COMPARE && GET_MODE (op) == VOIDmode;
}

/* Output code to perform a 387 binary operation in INSN, one of PLUS,
   MINUS, MULT or DIV.  OPERANDS are the insn operands, where operands[3]
   is the expression of the binary operation.  The output may either be
   emitted here, or returned to the caller, like all output_* functions.

   There is no guarantee that the operands are the same mode, as they
   might be within FLOAT or FLOAT_EXTEND expressions. */

char *
output_387_binary_op (insn, operands)
     rtx insn;
     rtx *operands;
{
  rtx temp;
  char *base_op;
  static char buf[100];

  switch (GET_CODE (operands[3]))
    {
    case PLUS:
      if (GET_MODE_CLASS (GET_MODE (operands[1])) == MODE_INT
	  || GET_MODE_CLASS (GET_MODE (operands[2])) == MODE_INT)
	base_op = "fiadd";
      else
	base_op = "fadd";
      break;

    case MINUS:
      if (GET_MODE_CLASS (GET_MODE (operands[1])) == MODE_INT
	  || GET_MODE_CLASS (GET_MODE (operands[2])) == MODE_INT)
	base_op = "fisub";
      else
	base_op = "fsub";
      break;

    case MULT:
      if (GET_MODE_CLASS (GET_MODE (operands[1])) == MODE_INT
	  || GET_MODE_CLASS (GET_MODE (operands[2])) == MODE_INT)
	base_op = "fimul";
      else
	base_op = "fmul";
      break;

    case DIV:
      if (GET_MODE_CLASS (GET_MODE (operands[1])) == MODE_INT
	  || GET_MODE_CLASS (GET_MODE (operands[2])) == MODE_INT)
	base_op = "fidiv";
      else
	base_op = "fdiv";
      break;

    default:
      abort ();
    }

  strcpy (buf, base_op);

  switch (GET_CODE (operands[3]))
    {
    case MULT:
    case PLUS:
      if (REG_P (operands[2]) && REGNO (operands[0]) == REGNO (operands[2]))
	{
	  temp = operands[2];
	  operands[2] = operands[1];
	  operands[1] = temp;
	}

      if (GET_CODE (operands[2]) == MEM)
	return strcat (buf, AS1 (%z2,%2));

      if (NON_STACK_REG_P (operands[1]))
	{
	  output_op_from_reg (operands[1], strcat (buf, AS1 (%z0,%1)));
	  RET;
	}
      else if (NON_STACK_REG_P (operands[2]))
	{
	  output_op_from_reg (operands[2], strcat (buf, AS1 (%z0,%1)));
	  RET;
	}

      if (find_regno_note (insn, REG_DEAD, REGNO (operands[2])))
	return strcat (buf, AS2 (p,%2,%0));

      if (STACK_TOP_P (operands[0]))
	return strcat (buf, AS2C (%y2,%0));
      else
	return strcat (buf, AS2C (%2,%0));

    case MINUS:
    case DIV:
      if (GET_CODE (operands[1]) == MEM)
	return strcat (buf, AS1 (r%z1,%1));

      if (GET_CODE (operands[2]) == MEM)
	return strcat (buf, AS1 (%z2,%2));

      if (NON_STACK_REG_P (operands[1]))
	{
	  output_op_from_reg (operands[1], strcat (buf, AS1 (r%z0,%1)));
	  RET;
	}
      else if (NON_STACK_REG_P (operands[2]))
	{
	  output_op_from_reg (operands[2], strcat (buf, AS1 (%z0,%1)));
	  RET;
	}

      if (! STACK_REG_P (operands[1]) || ! STACK_REG_P (operands[2]))
	abort ();

      if (find_regno_note (insn, REG_DEAD, REGNO (operands[2])))
	return strcat (buf, AS2 (rp,%2,%0));

      if (find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
	return strcat (buf, AS2 (p,%1,%0));

      if (STACK_TOP_P (operands[0]))
	{
	  if (STACK_TOP_P (operands[1]))
	    return strcat (buf, AS2C (%y2,%0));
	  else
	    return strcat (buf, AS2 (r,%y1,%0));
	}
      else if (STACK_TOP_P (operands[1]))
	return strcat (buf, AS2C (%1,%0));
      else
	return strcat (buf, AS2 (r,%2,%0));

    default:
      abort ();
    }
}

/* Output code for INSN to convert a float to a signed int.  OPERANDS
   are the insn operands.  The output may be SFmode or DFmode and the
   input operand may be SImode or DImode.  As a special case, make sure
   that the 387 stack top dies if the output mode is DImode, because the
   hardware requires this.  */

char *
output_fix_trunc (insn, operands)
     rtx insn;
     rtx *operands;
{
  int stack_top_dies = find_regno_note (insn, REG_DEAD, FIRST_STACK_REG) != 0;
  rtx xops[2];

  if (! STACK_TOP_P (operands[1]) ||
      (GET_MODE (operands[0]) == DImode && ! stack_top_dies))
    abort ();

  xops[0] = GEN_INT (12);
  xops[1] = operands[4];

  output_asm_insn (AS1 (fnstc%W2,%2), operands);
  output_asm_insn (AS2 (mov%L2,%2,%4), operands);
  output_asm_insn (AS2 (mov%B1,%0,%h1), xops);
  output_asm_insn (AS2 (mov%L4,%4,%3), operands);
  output_asm_insn (AS1 (fldc%W3,%3), operands);

  if (NON_STACK_REG_P (operands[0]))
    output_to_reg (operands[0], stack_top_dies);
  else if (GET_CODE (operands[0]) == MEM)
    {
      if (stack_top_dies)
	output_asm_insn (AS1 (fistp%z0,%0), operands);
      else
	output_asm_insn (AS1 (fist%z0,%0), operands);
    }
  else
    abort ();

  return AS1 (fldc%W2,%2);
}

/* Output code for INSN to compare OPERANDS.  The two operands might
   not have the same mode: one might be within a FLOAT or FLOAT_EXTEND
   expression.  If the compare is in mode CCFPEQmode, use an opcode that
   will not fault if a qNaN is present. */

char *
output_float_compare (insn, operands)
     rtx insn;
     rtx *operands;
{
  int stack_top_dies;
  rtx body = XVECEXP (PATTERN (insn), 0, 0);
  int unordered_compare = GET_MODE (SET_SRC (body)) == CCFPEQmode;

  if (! STACK_TOP_P (operands[0]))
    abort ();

  stack_top_dies = find_regno_note (insn, REG_DEAD, FIRST_STACK_REG) != 0;

  if (STACK_REG_P (operands[1])
      && stack_top_dies
      && find_regno_note (insn, REG_DEAD, REGNO (operands[1]))
      && REGNO (operands[1]) != FIRST_STACK_REG)
    {
      /* If both the top of the 387 stack dies, and the other operand
	 is also a stack register that dies, then this must be a
	 `fcompp' float compare */

      if (unordered_compare)
	output_asm_insn ("fucompp", operands);
      else
	output_asm_insn ("fcompp", operands);
    }
  else
    {
      static char buf[100];

      /* Decide if this is the integer or float compare opcode, or the
	 unordered float compare. */

      if (unordered_compare)
	strcpy (buf, "fucom");
      else if (GET_MODE_CLASS (GET_MODE (operands[1])) == MODE_FLOAT)
	strcpy (buf, "fcom");
      else
	strcpy (buf, "ficom");

      /* Modify the opcode if the 387 stack is to be popped. */

      if (stack_top_dies)
	strcat (buf, "p");

      if (NON_STACK_REG_P (operands[1]))
	output_op_from_reg (operands[1], strcat (buf, AS1 (%z0,%1)));
      else
        output_asm_insn (strcat (buf, AS1 (%z1,%y1)), operands);
    }

  /* Now retrieve the condition code. */

  return output_fp_cc0_set (insn);
}

/* Output opcodes to transfer the results of FP compare or test INSN
   from the FPU to the CPU flags.  If TARGET_IEEE_FP, ensure that if the
   result of the compare or test is unordered, no comparison operator
   succeeds except NE.  Return an output template, if any.  */

char *
output_fp_cc0_set (insn)
     rtx insn;
{
  rtx xops[3];
  rtx unordered_label;
  rtx next;
  enum rtx_code code;

  xops[0] = gen_rtx (REG, HImode, 0);
  output_asm_insn (AS1 (fnsts%W0,%0), xops);

  if (! TARGET_IEEE_FP)
    return "sahf";

  next = next_cc0_user (insn);
  if (next == NULL_RTX)
    abort ();

  if (GET_CODE (next) == JUMP_INSN
      && GET_CODE (PATTERN (next)) == SET
      && SET_DEST (PATTERN (next)) == pc_rtx
      && GET_CODE (SET_SRC (PATTERN (next))) == IF_THEN_ELSE)
    {
      code = GET_CODE (XEXP (SET_SRC (PATTERN (next)), 0));
    }
  else if (GET_CODE (PATTERN (next)) == SET)
    {
      code = GET_CODE (SET_SRC (PATTERN (next)));
    }
  else
    abort ();

  xops[0] = gen_rtx (REG, QImode, 0);

  switch (code)
    {
    case GT:
      xops[1] = GEN_INT (0x45);
      output_asm_insn (AS2 (and%B0,%1,%h0), xops);
      /* je label */
      break;

    case LT:
      xops[1] = GEN_INT (0x45);
      xops[2] = GEN_INT (0x01);
      output_asm_insn (AS2 (and%B0,%1,%h0), xops);
      output_asm_insn (AS2 (cmp%B0,%2,%h0), xops);
      /* je label */
      break;

    case GE:
      xops[1] = GEN_INT (0x05);
      output_asm_insn (AS2 (and%B0,%1,%h0), xops);
      /* je label */
      break;

    case LE:
      xops[1] = GEN_INT (0x45);
      xops[2] = GEN_INT (0x40);
      output_asm_insn (AS2 (and%B0,%1,%h0), xops);
      output_asm_insn (AS1 (dec%B0,%h0), xops);
      output_asm_insn (AS2 (cmp%B0,%2,%h0), xops);
      /* jb label */
      break;

    case EQ:
      xops[1] = GEN_INT (0x45);
      xops[2] = GEN_INT (0x40);
      output_asm_insn (AS2 (and%B0,%1,%h0), xops);
      output_asm_insn (AS2 (cmp%B0,%2,%h0), xops);
      /* je label */
      break;

    case NE:
      xops[1] = GEN_INT (0x44);
      xops[2] = GEN_INT (0x40);
      output_asm_insn (AS2 (and%B0,%1,%h0), xops);
      output_asm_insn (AS2 (xor%B0,%2,%h0), xops);
      /* jne label */
      break;

    case GTU:
    case LTU:
    case GEU:
    case LEU:
    default:
      abort ();
    }
  RET;
}

#define MAX_386_STACK_LOCALS 2

static rtx i386_stack_locals[(int) MAX_MACHINE_MODE][MAX_386_STACK_LOCALS];

/* Define the structure for the machine field in struct function.  */
struct machine_function
{
  rtx i386_stack_locals[(int) MAX_MACHINE_MODE][MAX_386_STACK_LOCALS];
};

/* Functions to save and restore i386_stack_locals.
   These will be called, via pointer variables,
   from push_function_context and pop_function_context.  */

void
save_386_machine_status (p)
     struct function *p;
{
  p->machine = (struct machine_function *) xmalloc (sizeof i386_stack_locals);
  bcopy ((char *) i386_stack_locals, (char *) p->machine->i386_stack_locals,
	 sizeof i386_stack_locals);
}

void
restore_386_machine_status (p)
     struct function *p;
{
  bcopy ((char *) p->machine->i386_stack_locals, (char *) i386_stack_locals,
	 sizeof i386_stack_locals);
  free (p->machine);
}

/* Clear stack slot assignments remembered from previous functions.
   This is called from INIT_EXPANDERS once before RTL is emitted for each
   function.  */

void
clear_386_stack_locals ()
{
  enum machine_mode mode;
  int n;

  for (mode = VOIDmode; (int) mode < (int) MAX_MACHINE_MODE;
       mode = (enum machine_mode) ((int) mode + 1))
    for (n = 0; n < MAX_386_STACK_LOCALS; n++)
      i386_stack_locals[(int) mode][n] = NULL_RTX;

  /* Arrange to save and restore i386_stack_locals around nested functions.  */
  save_machine_status = save_386_machine_status;
  restore_machine_status = restore_386_machine_status;
}

/* Return a MEM corresponding to a stack slot with mode MODE.
   Allocate a new slot if necessary.

   The RTL for a function can have several slots available: N is
   which slot to use.  */

rtx
assign_386_stack_local (mode, n)
     enum machine_mode mode;
     int n;
{
  if (n < 0 || n >= MAX_386_STACK_LOCALS)
    abort ();

  if (i386_stack_locals[(int) mode][n] == NULL_RTX)
    i386_stack_locals[(int) mode][n]
      = assign_stack_local (mode, GET_MODE_SIZE (mode), 0);

  return i386_stack_locals[(int) mode][n];
}
