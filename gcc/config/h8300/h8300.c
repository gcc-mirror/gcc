/* Subroutines for insn-output.c for Hitachi H8/300.
   Copyright (C) 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000
   Free Software Foundation, Inc. 
   Contributed by Steve Chamberlain (sac@cygnus.com),
   Jim Wilson (wilson@cygnus.com), and Doug Evans (dje@cygnus.com).

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
#include "tree.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-flags.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "recog.h"
#include "expr.h"
#include "function.h"
#include "obstack.h"
#include "toplev.h"
#include "tm_p.h"

/* Forward declarations.  */
static int h8300_interrupt_function_p PARAMS ((tree));
static int h8300_monitor_function_p PARAMS ((tree));
static int h8300_os_task_function_p PARAMS ((tree));
static void dosize PARAMS ((FILE *, const char *, unsigned int));
static const char *cond_string PARAMS ((enum rtx_code));

/* CPU_TYPE, says what cpu we're compiling for.  */
int cpu_type;

/* True if the current function is an interrupt handler
   (either via #pragma or an attribute specification).  */
int interrupt_handler;

/* True if the current function is an OS Task
   (via an attribute specification).  */
int os_task;

/* True if the current function is a monitor
   (via an attribute specification).  */
int monitor;

/* True if a #pragma saveall has been seen for the current function.  */
int pragma_saveall;

static const char *const names_big[] =
{"r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7"};

static const char *const names_extended[] =
{"er0", "er1", "er2", "er3", "er4", "er5", "er6", "er7"};

static const char *const names_upper_extended[] =
{"e0", "e1", "e2", "e3", "e4", "e5", "e6", "e7"};

/* Points to one of the above.  */
/* ??? The above could be put in an array indexed by CPU_TYPE.  */
const char * const *h8_reg_names;

/* Various operations needed by the following, indexed by CPU_TYPE.  */

static const char *const h8_push_ops[2] = {"push", "push.l"};
static const char *const h8_pop_ops[2] = {"pop", "pop.l"};
static const char *const h8_mov_ops[2] = {"mov.w", "mov.l"};

const char *h8_push_op, *h8_pop_op, *h8_mov_op;

/* Initialize various cpu specific globals at start up.  */

void
h8300_init_once ()
{
  if (TARGET_H8300)
    {
      cpu_type = (int) CPU_H8300;
      h8_reg_names = names_big;
    }
  else
    {
      /* For this we treat the H8/300 and H8/S the same.  */
      cpu_type = (int) CPU_H8300H;
      h8_reg_names = names_extended;
    }
  h8_push_op = h8_push_ops[cpu_type];
  h8_pop_op = h8_pop_ops[cpu_type];
  h8_mov_op = h8_mov_ops[cpu_type];
}

const char *
byte_reg (x, b)
     rtx x;
     int b;
{
  static const char *const names_small[] =
  {"r0l", "r0h", "r1l", "r1h", "r2l", "r2h", "r3l", "r3h",
   "r4l", "r4h", "r5l", "r5h", "r6l", "r6h", "r7l", "r7h"};

  return names_small[REGNO (x) * 2 + b];
}

/* REGNO must be saved/restored across calls if this macro is true.  */

#define WORD_REG_USED(regno)					\
  (regno < 7							\
   /* No need to save registers if this function will not return.*/\
   && ! TREE_THIS_VOLATILE (current_function_decl)		\
   && (pragma_saveall						\
       /* Save any call saved register that was used.  */	\
       || (regs_ever_live[regno] && !call_used_regs[regno])	\
       /* Save the frame pointer if it was used.  */		\
       || (regno == FRAME_POINTER_REGNUM && regs_ever_live[regno])\
       /* Save any register used in an interrupt handler.  */	\
       || (interrupt_handler && regs_ever_live[regno])		\
       /* Save call clobbered registers in non-leaf interrupt	\
	  handlers.  */						\
       || (interrupt_handler					\
	   && call_used_regs[regno]				\
	   && !current_function_is_leaf)))

/* Output assembly language to FILE for the operation OP with operand size
   SIZE to adjust the stack pointer.  */

static void
dosize (file, op, size)
     FILE *file;
     const char *op;
     unsigned int size;
{
  /* On the h8300h and h8300s, for sizes <= 8 bytes it is as good or
     better to use adds/subs insns rather than add.l/sub.l
     with an immediate value.   */
  if (size > 4 && size <= 8 && (TARGET_H8300H || TARGET_H8300S))
    {
      /* Crank the size down to <= 4 */
      fprintf (file, "\t%ss\t#%d,sp\n", op, 4);
      size -= 4;
    }

  switch (size)
    {
    case 4:
      if (TARGET_H8300H || TARGET_H8300S)
	{
	  fprintf (file, "\t%ss\t#%d,sp\n", op, 4);
	  size = 0;
	  break;
	}
    case 3:
      fprintf (file, "\t%ss\t#%d,sp\n", op, 2);
      size -= 2;
      /* Fall through...  */
    case 2:
    case 1:
      fprintf (file, "\t%ss\t#%d,sp\n", op, size);
      size = 0;
      break;
    case 0:
      break;
    default:
      if (TARGET_H8300)
	{
	  if (current_function_needs_context
	      && strcmp (op, "sub") == 0)
	    {
	      /* Egad.  We don't have a temporary to hold the
		 size of the frame in the prologue!  Just inline
		 the bastard since this shouldn't happen often.  */
	      while (size >= 2)
		{
		  fprintf (file, "\tsubs\t#2,sp\n");
		  size -= 2;
		}

	      if (size)
		fprintf (file, "\tsubs\t#1,sp\n");

	      size = 0;
	    }
	  else
	    fprintf (file, "\tmov.w\t#%d,r3\n\t%s.w\tr3,sp\n", size, op);
	}
      else
	fprintf (file, "\t%s\t#%d,sp\n", op, size);
      size = 0;
      break;
    }
}

/* Output assembly language code for the function prologue.  */
static int push_order[FIRST_PSEUDO_REGISTER] =
{0, 1, 2, 3, 4, 5, 6, -1, -1, -1};
static int pop_order[FIRST_PSEUDO_REGISTER] =
{6, 5, 4, 3, 2, 1, 0, -1, -1, -1};

/* This is what the stack looks like after the prolog of 
   a function with a frame has been set up:

   <args>
   PC
   FP			<- fp
   <locals>
   <saved registers> 	<- sp

   This is what the stack looks like after the prolog of
   a function which doesn't have a frame:

   <args>
   PC
   <locals>
   <saved registers>   	<- sp
*/

void
function_prologue (file, size)
     FILE *file;
     int size;
{
  int fsize = (size + STACK_BOUNDARY / 8 - 1) & -STACK_BOUNDARY / 8;
  int idx;

  /* Note a function with the interrupt attribute and set interrupt_handler
     accordingly.  */
  if (h8300_interrupt_function_p (current_function_decl))
    interrupt_handler = 1;

  /* If the current function has the OS_Task attribute set, then
     we have a naked prologue.  */
  if (h8300_os_task_function_p (current_function_decl))
    {
      fprintf (file, ";OS_Task prologue\n");
      os_task = 1;
      return;
    }

  if (h8300_monitor_function_p (current_function_decl))
    {
      /* My understanding of monitor functions is they act just
	 like interrupt functions, except the prologue must
	 mask interrupts.  */
      fprintf (file, ";monitor prologue\n");
      interrupt_handler = 1;
      monitor = 1;
      if (TARGET_H8300)
	{
	  fprintf (file, "\tsubs\t#2,sp\n");
	  fprintf (file, "\tpush\tr0\n");
	  fprintf (file, "\tstc\tccr,r0l\n");
	  fprintf (file, "\torc\t#128,ccr\n");
	  fprintf (file, "\tmov.b\tr0l,@(4,sp)\n");
	}
      else
	{
	  fprintf (file, "\tpush\ter0\n");
	  fprintf (file, "\tstc\tccr,r0l\n");
	  fprintf (file, "\torc\t#128,ccr\n");
	  fprintf (file, "\tmov.b\tr0l,@(4,sp)\n");
	}
    }

  if (frame_pointer_needed)
    {
      /* Push fp */
      fprintf (file, "\t%s\t%s\n", h8_push_op,
	       h8_reg_names[FRAME_POINTER_REGNUM]);
      fprintf (file, "\t%s\t%s,%s\n", h8_mov_op,
	       h8_reg_names[STACK_POINTER_REGNUM],
	       h8_reg_names[FRAME_POINTER_REGNUM]);
    }

  /* leave room for locals */
  dosize (file, "sub", fsize);

  /* Push the rest of the registers */
  for (idx = 0; idx < FIRST_PSEUDO_REGISTER; idx++)
    {
      int regno = push_order[idx];

      if (regno >= 0
	  && WORD_REG_USED (regno)
	  && (!frame_pointer_needed || regno != FRAME_POINTER_REGNUM))
	{
	  if (TARGET_H8300S)
	    {
	      /* Try to push multiple registers.  */
	      if (regno == 0 || regno == 4)
		{
		  int second_regno = push_order[idx + 1];
		  int third_regno = push_order[idx + 2];
		  int fourth_regno = push_order[idx + 3];

		  if (fourth_regno >= 0
		      && WORD_REG_USED (fourth_regno)
		      && (!frame_pointer_needed
			  || fourth_regno != FRAME_POINTER_REGNUM)
		      && third_regno >= 0
		      && WORD_REG_USED (third_regno)
		      && (!frame_pointer_needed
			  || third_regno != FRAME_POINTER_REGNUM)
		      && second_regno >= 0
		      && WORD_REG_USED (second_regno)
		      && (!frame_pointer_needed
			  || second_regno != FRAME_POINTER_REGNUM))
		    {
		      fprintf (file, "\tstm.l %s-%s,@-sp\n", 
			       h8_reg_names[regno],
			       h8_reg_names[fourth_regno]);
		      idx += 3;
		      continue;
		    }
		}
	      if (regno == 0 || regno == 4)
		{
		  int second_regno = push_order[idx + 1];
		  int third_regno = push_order[idx + 2];

		  if (third_regno >= 0
		      && WORD_REG_USED (third_regno)
		      && (!frame_pointer_needed
			  || third_regno != FRAME_POINTER_REGNUM)
		      && second_regno >= 0
		      && WORD_REG_USED (second_regno)
		      && (!frame_pointer_needed
			  || second_regno != FRAME_POINTER_REGNUM))
		    {
		      fprintf (file, "\tstm.l %s-%s,@-sp\n", 
			       h8_reg_names[regno],
			       h8_reg_names[third_regno]);
		      idx += 2;
		      continue;
		    }
		}
	      if (regno == 0 || regno == 2 || regno == 4 || regno == 6)
		{
		  int second_regno = push_order[idx + 1];

		  if (second_regno >= 0
		      && WORD_REG_USED (second_regno)
		      && (!frame_pointer_needed
			  || second_regno != FRAME_POINTER_REGNUM))
		    {
		      fprintf (file, "\tstm.l %s-%s,@-sp\n", 
			       h8_reg_names[regno],
			       h8_reg_names[second_regno]);
		      idx += 1;
		      continue;
		    }
		}
	    }
	  fprintf (file, "\t%s\t%s\n", h8_push_op, h8_reg_names[regno]);
	}
    }
}

/* Output assembly language code for the function epilogue.  */

void
function_epilogue (file, size)
     FILE *file;
     int size;
{
  int fsize = (size + STACK_BOUNDARY / 8 - 1) & -STACK_BOUNDARY / 8;
  int idx;
  rtx insn = get_last_insn ();

  if (os_task)
    {
      /* OS_Task epilogues are nearly naked -- they just have an
	 rts instruction.  */
      fprintf (file, ";OS_task epilogue\n");
      fprintf (file, "\trts\n");
      goto out;
    }

  /* monitor epilogues are the same as interrupt function epilogues.
     Just make a note that we're in an monitor epilogue.  */
  if (monitor)
    fprintf(file, ";monitor epilogue\n");

  /* If the last insn was a BARRIER, we don't have to write any code.  */
  if (GET_CODE (insn) == NOTE)
    insn = prev_nonnote_insn (insn);
  if (insn && GET_CODE (insn) == BARRIER)
    goto out;

  /* Pop the saved registers. */
  for (idx = 0; idx < FIRST_PSEUDO_REGISTER; idx++)
    {
      int regno = pop_order[idx];

      if (regno >= 0
	  && WORD_REG_USED (regno)
	  && (!frame_pointer_needed || regno != FRAME_POINTER_REGNUM))
	{
	  if (TARGET_H8300S)
	    {
	      /* Try to pop multiple registers.  */
	      if (regno == 7 || regno == 3)
		{
		  int second_regno = pop_order[idx + 1];
		  int third_regno = pop_order[idx + 2];
		  int fourth_regno = pop_order[idx + 3];

		  if (fourth_regno >= 0
		      && WORD_REG_USED (fourth_regno)
		      && (!frame_pointer_needed
			  || fourth_regno != FRAME_POINTER_REGNUM)
		      && third_regno >= 0
		      && WORD_REG_USED (third_regno)
		      && (!frame_pointer_needed
			  || third_regno != FRAME_POINTER_REGNUM)
		      && second_regno >= 0
		      && WORD_REG_USED (second_regno)
		      && (!frame_pointer_needed
			  || second_regno != FRAME_POINTER_REGNUM))
		    {
		      fprintf (file, "\tldm.l @sp+,%s-%s\n", 
			       h8_reg_names[fourth_regno],
			       h8_reg_names[regno]);
		      idx += 3;
		      continue;
		    }
		}
	      if (regno == 6 || regno == 2)
		{
		  int second_regno = pop_order[idx + 1];
		  int third_regno = pop_order[idx + 2];

		  if (third_regno >= 0
		      && WORD_REG_USED (third_regno)
		      && (!frame_pointer_needed
			  || third_regno != FRAME_POINTER_REGNUM)
		      && second_regno >= 0
		      && WORD_REG_USED (second_regno)
		      && (!frame_pointer_needed
			  || second_regno != FRAME_POINTER_REGNUM))
		    {
		      fprintf (file, "\tldm.l @sp+,%s-%s\n", 
			       h8_reg_names[third_regno],
			       h8_reg_names[regno]);
		      idx += 2;
		      continue;
		    }
		}
	      if (regno == 7 || regno == 5 || regno == 3 || regno == 1)
		{
		  int second_regno = pop_order[idx + 1];

		  if (second_regno >= 0
		      && WORD_REG_USED (second_regno)
		      && (!frame_pointer_needed
			  || second_regno != FRAME_POINTER_REGNUM))
		    {
		      fprintf (file, "\tldm.l @sp+,%s-%s\n", 
			       h8_reg_names[second_regno],
			       h8_reg_names[regno]);
		      idx += 1;
		      continue;
		    }
		}
	    }
	  fprintf (file, "\t%s\t%s\n", h8_pop_op, h8_reg_names[regno]);
	}
    }

  /* deallocate locals */
  dosize (file, "add", fsize);

  /* pop frame pointer if we had one. */
  if (frame_pointer_needed)
    fprintf (file, "\t%s\t%s\n", h8_pop_op, h8_reg_names[FRAME_POINTER_REGNUM]);

  /* If this is a monitor function, there is one register still left on
     the stack.  */
  if (monitor)
    fprintf (file, "\t%s\t%s\n", h8_pop_op, h8_reg_names[0]);

  if (interrupt_handler)
    fprintf (file, "\trte\n");
  else
    fprintf (file, "\trts\n");

out:
  interrupt_handler = 0;
  os_task = 0;
  monitor = 0;
  pragma_saveall = 0;
}

/* Output assembly code for the start of the file.  */

void
asm_file_start (file)
     FILE *file;
{
  fprintf (file, ";\tGCC For the Hitachi H8/300\n");
  fprintf (file, ";\tBy Hitachi America Ltd and Cygnus Support\n");
  fprintf (file, ";\trelease F-1\n");
  if (optimize)
    fprintf (file, "; -O%d\n", optimize);
  if (TARGET_H8300H)
    fprintf (file, "\n\t.h8300h\n");
  else if (TARGET_H8300S)
    fprintf (file, "\n\t.h8300s\n");
  else
    fprintf (file, "\n\n");
  output_file_directive (file, main_input_filename);
}

/* Output assembly language code for the end of file.  */

void
asm_file_end (file)
     FILE *file;
{
  fprintf (file, "\t.end\n");
}

/* Return true if VALUE is a valid constant for constraint 'P'.
   IE: VALUE is a power of two <= 2**15.  */

int
small_power_of_two (value)
     int value;
{
  switch (value)
    {
    case 1:
    case 2:
    case 4:
    case 8:
    case 16:
    case 32:
    case 64:
    case 128:
    case 256:
    case 512:
    case 1024:
    case 2048:
    case 4096:
    case 8192:
    case 16384:
    case 32768:
      return 1;
    }
  return 0;
}

/* Return true if VALUE is a valid constant for constraint 'O', which
   means that the constant would be ok to use as a bit for a bclr
   instruction.  */

int
ok_for_bclr (value)
     int value;
{
  return small_power_of_two ((~value) & 0xff);
}

/* Return true is OP is a valid source operand for an integer move
   instruction.  */

int
general_operand_src (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == MEM && GET_CODE (XEXP (op, 0)) == POST_INC)
    return 1;
  return general_operand (op, mode);
}

/* Return true if OP is a valid destination operand for an integer move
   instruction.  */

int
general_operand_dst (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == MEM && GET_CODE (XEXP (op, 0)) == PRE_DEC)
    return 1;
  return general_operand (op, mode);
}

/* Return true if OP is a const valid for a bit clear instruction.  */

int
o_operand (operand, mode)
     rtx operand;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (operand) == CONST_INT
	  && CONST_OK_FOR_O (INTVAL (operand)));
}

/* Return true if OP is a const valid for a bit set or bit xor instruction.  */

int
p_operand (operand, mode)
     rtx operand;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (operand) == CONST_INT
	  && CONST_OK_FOR_P (INTVAL (operand)));
}

/* Return true if OP is a valid call operand.  */

int
call_insn_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE (op) == MEM)
    {
      rtx inside = XEXP (op, 0);
      if (register_operand (inside, Pmode))
	return 1;
      if (CONSTANT_ADDRESS_P (inside))
	return 1;
    }
  return 0;
}

int
adds_subs_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE (op) == CONST_INT)
    {
      if (INTVAL (op) <= 4 && INTVAL (op) >= 0)
	return 1;
      if (INTVAL (op) >= -4 && INTVAL (op) <= 0)
	return 1;
      if ((TARGET_H8300H || TARGET_H8300S)
	  && INTVAL (op) != 7
	  && (INTVAL (op) <= 8 && INTVAL (op) >= 0))
	return 1;
      if ((TARGET_H8300H || TARGET_H8300S)
	  && INTVAL (op) != -7
	  && (INTVAL (op) >= -8 && INTVAL (op) <= 0))
	return 1;
    }
  return 0;
}

/* Return nonzero if op is an adds/subs operand which only requires
   one insn to implement.  It is assumed that OP is already an adds/subs
   operand.  */
int
one_insn_adds_subs_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  int val = INTVAL (op);

  if (val == 1 || val == -1
      || val == 2 || val == -2
      || ((TARGET_H8300H || TARGET_H8300S)
	  && (val == 4 || val == -4)))
    return 1;
  return 0;
}

const char *
output_adds_subs (operands)
     rtx *operands;
{
  int val = INTVAL (operands[2]);

  /* First get the value into the range -4..4 inclusive.

     The only way it can be out of this range is when TARGET_H8300H
     or TARGET_H8300S is true, thus it is safe to use adds #4 and subs #4.  */
  if (val > 4)
    {
      output_asm_insn ("adds #4,%A0", operands);
      val -= 4;
    }

  if (val < -4)
    {
      output_asm_insn ("subs #4,%A0", operands);
      val += 4;
    }

  /* Handle case were val == 4 or val == -4 and we're compiling
     for TARGET_H8300H or TARGET_H8300S.  */
  if ((TARGET_H8300H || TARGET_H8300S)
      && val == 4)
    return "adds #4,%A0";

  if ((TARGET_H8300H || TARGET_H8300S)
      && val == -4)
    return "subs #4,%A0";

  if (val > 2)
    {
      output_asm_insn ("adds #2,%A0", operands);
      val -= 2;
    }

  if (val < -2)
    {
      output_asm_insn ("subs #2,%A0", operands);
      val += 2;
    }

  /* val should be one or two now.  */
  if (val == 2)
    return "adds #2,%A0";

  if (val == -2)
    return "subs #2,%A0";

  /* val should be one now.  */
  if (val == 1)
    return "adds #1,%A0";

  if (val == -1)
    return "subs #1,%A0";

  /* If not optimizing, we might be asked to add 0.  */
  if (val == 0)
    return "";

  /* In theory, this can't happen.  */
  abort ();
}

/* Return true if OP is a valid call operand, and OP represents
   an operand for a small call (4 bytes instead of 6 bytes).  */

int
small_call_insn_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE (op) == MEM)
    {
      rtx inside = XEXP (op, 0);

      /* Register indirect is a small call.  */
      if (register_operand (inside, Pmode))
	return 1;

      /* A call through the function vector is a small
	 call too.  */
      if (GET_CODE (inside) == SYMBOL_REF
	  && SYMBOL_REF_FLAG (inside))
	return 1;
    }
  /* Otherwise it's a large call.  */
  return 0;
}

/* Return true if OP is a valid jump operand.  */

int
jump_address_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == REG)
    return mode == Pmode;

  if (GET_CODE (op) == MEM)
    {
      rtx inside = XEXP (op, 0);
      if (register_operand (inside, Pmode))
	return 1;
      if (CONSTANT_ADDRESS_P (inside))
	return 1;
    }
  return 0;
}

/* Recognize valid operands for bitfield instructions.  */

extern int rtx_equal_function_value_matters;

int
bit_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  /* We can except any general operand, expept that MEM operands must
     be limited to those that use addresses valid for the 'U' constraint.  */
  if (!general_operand (op, mode))
    return 0;

  /* Accept any mem during RTL generation.  Otherwise, the code that does
     insv and extzv will think that we can not handle memory.  However,
     to avoid reload problems, we only accept 'U' MEM operands after RTL
     generation.  This means that any named pattern which uses this predicate
     must force its operands to match 'U' before emitting RTL.  */

  if (GET_CODE (op) == REG)
    return 1;
  if (GET_CODE (op) == SUBREG)
    return 1;
  if (!rtx_equal_function_value_matters)
    {
      /* We're building rtl */
      return GET_CODE (op) == MEM;
    }
  else
    {
      return (GET_CODE (op) == MEM
	      && EXTRA_CONSTRAINT (op, 'U'));
    }
}

int
bit_memory_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == MEM
	  && EXTRA_CONSTRAINT (op, 'U'));
}

/* Recognize valid operators for bit test.  */

int
eq_operator (x, mode)
     rtx x;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (x) == EQ || GET_CODE (x) == NE);
}

/* Handle machine specific pragmas for compatibility with existing
   compilers for the H8/300.

   pragma saveall generates prolog/epilog code which saves and
   restores all the registers on function entry.

   pragma interrupt saves and restores all registers, and exits with
   an rte instruction rather than an rts.  A pointer to a function
   with this attribute may be safely used in an interrupt vector.  */

int
handle_pragma (p_getc, p_ungetc, pname)
     int (* ATTRIBUTE_UNUSED p_getc) PARAMS ((void));
     void (* ATTRIBUTE_UNUSED p_ungetc) PARAMS ((int));
     const char *pname;
{
  int retval = 0;

  if (strcmp (pname, "interrupt") == 0)
    interrupt_handler = retval = 1;
  else if (strcmp (pname, "saveall") == 0)
    pragma_saveall = retval = 1;

  return retval;
}

/* If the next arg with MODE and TYPE is to be passed in a register, return
   the rtx to represent where it is passed.  CUM represents the state after
   the last argument.  NAMED is not used.  */

static const char *const hand_list[] =
{
  "__main",
  "__cmpsi2",
  "__divhi3",
  "__modhi3",
  "__udivhi3",
  "__umodhi3",
  "__divsi3",
  "__modsi3",
  "__udivsi3",
  "__umodsi3",
  "__mulhi3",
  "__mulsi3",
  "__reg_memcpy",
  "__reg_memset",
  "__ucmpsi2",
  0,
};

/* Return an RTX to represent where a value with mode MODE will be returned
   from a function.  If the result is 0, the argument is pushed.  */

rtx
function_arg (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int named;
{
  rtx result = 0;
  const char *fname;
  int regpass = 0;

  /* Never pass unnamed arguments in registers.  */
  if (!named)
    return 0;

  /* Pass 3 regs worth of data in regs when user asked on the command line.  */
  if (TARGET_QUICKCALL)
    regpass = 3;

  /* If calling hand written assembler, use 4 regs of args.  */

  if (cum->libcall)
    {
      const char * const *p;

      fname = XSTR (cum->libcall, 0);

      /* See if this libcall is one of the hand coded ones.  */

      for (p = hand_list; *p && strcmp (*p, fname) != 0; p++)
	;

      if (*p)
	regpass = 4;
    }

  if (regpass)
    {
      int size;

      if (mode == BLKmode)
	size = int_size_in_bytes (type);
      else
	size = GET_MODE_SIZE (mode);

      if (size + cum->nbytes > regpass * UNITS_PER_WORD)
	{
	  result = 0;
	}
      else
	{
	  switch (cum->nbytes / UNITS_PER_WORD)
	    {
	    case 0:
	      result = gen_rtx_REG (mode, 0);
	      break;
	    case 1:
	      result = gen_rtx_REG (mode, 1);
	      break;
	    case 2:
	      result = gen_rtx_REG (mode, 2);
	      break;
	    case 3:
	      result = gen_rtx_REG (mode, 3);
	      break;
	    default:
	      result = 0;
	    }
	}
    }

  return result;
}

/* Return the cost of the rtx R with code CODE.  */

int
const_costs (r, c)
     rtx r;
     enum rtx_code c;
{
  switch (c)
    {
    case CONST_INT:
      switch (INTVAL (r))
	{
	case 0:
	case 1:
	case 2:
	case -1:
	case -2:
	  return 0;
	case 4:
	case -4:
	  if (TARGET_H8300H || TARGET_H8300S)
	    return 0;
	  else
	    return 1;
	default:
	  return 1;
	}

    case CONST:
    case LABEL_REF:
    case SYMBOL_REF:
      return 3;

    case CONST_DOUBLE:
      return 20;

    default:
      return 4;
    }
}

/* Documentation for the machine specific operand escapes:

   'A' print rn in h8/300 mode, erN in H8/300H mode
   'C' print (operand - 2).
   'E' like s but negative.
   'F' like t but negative.
   'G' constant just the negative
   'M' turn a 'M' constant into its negative mod 2.
   'P' if operand is incing/decing sp, print .w, otherwise .b.
   'R' print operand as a byte:8 address if appropriate, else fall back to
       'X' handling.
   'S' print operand as a long word
   'T' print operand as a word
   'U' if operand is incing/decing sp, print l, otherwise nothing.
   'V' find the set bit, and print its number.
   'W' find the clear bit, and print its number.
   'X' print operand as a byte
   'Y' print either l or h depending on whether last 'Z' operand < 8 or >= 8.
       If this operand isn't a register, fall back to 'R' handling.
   'Z' print int & 7.
   'b' print the bit opcode
   'c' print the ibit opcode
   'd' bcc if EQ, bcs if NE
   'e' first word of 32 bit value - if reg, then least reg. if mem
       then least. if const then most sig word
   'f' second word of 32 bit value - if reg, then biggest reg. if mem
       then +2. if const then least sig word
   'g' bcs if EQ, bcc if NE
   'j' print operand as condition code.
   'k' print operand as reverse condition code.
   's' print as low byte of 16 bit value
   't' print as high byte of 16 bit value
   'w' print as low byte of 32 bit value
   'x' print as 2nd byte of 32 bit value
   'y' print as 3rd byte of 32 bit value
   'z' print as msb of 32 bit value
*/

/* Return assembly language string which identifies a comparison type.  */

static const char *
cond_string (code)
     enum rtx_code code;
{
  switch (code)
    {
    case NE:
      return "ne";
    case EQ:
      return "eq";
    case GE:
      return "ge";
    case GT:
      return "gt";
    case LE:
      return "le";
    case LT:
      return "lt";
    case GEU:
      return "hs";
    case GTU:
      return "hi";
    case LEU:
      return "ls";
    case LTU:
      return "lo";
    default:
      abort ();
    }
}

/* Print operand X using operand code CODE to assembly language output file
   FILE.  */

void
print_operand (file, x, code)
     FILE *file;
     rtx x;
     int code;
{
  /* This is used for communication between the 'P' and 'U' codes.  */
  static const char *last_p;

  /* This is used for communication between codes V,W,Z and Y.  */
  static int bitint;

  switch (code)
    {
    case 'A':
      if (GET_CODE (x) == REG)
	fprintf (file, "%s", h8_reg_names[REGNO (x)]);
      else
	goto def;
      break;
    case 'C':
      fprintf (file, "#%d", INTVAL (x) - 2);
      break;
    case 'E':
      switch (GET_CODE (x))
	{
	case REG:
	  fprintf (file, "%sl", names_big[REGNO (x)]);
	  break;
	case CONST_INT:
	  fprintf (file, "#%d", (-INTVAL (x)) & 0xff);
	  break;
	default:
	  abort ();
	}
      break;
    case 'F':
      switch (GET_CODE (x))
	{
	case REG:
	  fprintf (file, "%sh", names_big[REGNO (x)]);
	  break;
	case CONST_INT:
	  fprintf (file, "#%d", ((-INTVAL (x)) & 0xff00) >> 8);
	  break;
	default:
	  abort ();
	}
      break;
    case 'G':
      if (GET_CODE (x) != CONST_INT)
	abort ();
      fprintf (file, "#%d", 0xff & (-INTVAL (x)));
      break;
    case 'M':
      /* For 3/-3 and 4/-4, the other 2 is handled separately.  */
      switch (INTVAL (x))
	{
	case 2:
	case 4:
	case -2:
	case -4:
	  fprintf (file, "#2");
	  break;
	case 1:
	case 3:
	case -1:
	case -3:
	  fprintf (file, "#1");
	  break;
	default:
	  abort ();
	}
      break;
    case 'P':
      if (REGNO (XEXP (XEXP (x, 0), 0)) == STACK_POINTER_REGNUM)
	{
	  last_p = "";
	  fprintf (file, ".w");
	}
      else
	{
	  last_p = "l";
	  fprintf (file, ".b");
	}
      break;
    case 'S':
      if (GET_CODE (x) == REG)
	fprintf (file, "%s", names_extended[REGNO (x)]);
      else
	goto def;
      break;
    case 'T':
      if (GET_CODE (x) == REG)
	fprintf (file, "%s", names_big[REGNO (x)]);
      else
	goto def;
      break;
    case 'U':
      fprintf (file, "%s%s", names_big[REGNO (x)], last_p);
      break;
    case 'V':
      bitint = exact_log2 (INTVAL (x));
      if (bitint == -1)
	abort ();
      fprintf (file, "#%d", bitint & 7);
      break;
    case 'W':
      bitint = exact_log2 ((~INTVAL (x)) & 0xff);
      if (bitint == -1)
	abort ();
      fprintf (file, "#%d", bitint & 7);
      break;
    case 'R':
    case 'X':
      if (GET_CODE (x) == REG)
	fprintf (file, "%s", byte_reg (x, 0));
      else
	goto def;
      break;
    case 'Y':
      if (bitint == -1)
	abort ();
      if (GET_CODE (x) == REG)
	fprintf (file, "%s%c", names_big[REGNO (x)], bitint > 7 ? 'h' : 'l');
      else
	print_operand (file, x, 'R');
      bitint = -1;
      break;
    case 'Z':
      bitint = INTVAL (x);
      fprintf (file, "#%d", bitint & 7);
      break;
    case 'b':
      switch (GET_CODE (x))
	{
	case IOR:
	  fprintf (file, "bor");
	  break;
	case XOR:
	  fprintf (file, "bxor");
	  break;
	case AND:
	  fprintf (file, "band");
	  break;
	default:
	  break;
	}
      break;
    case 'c':
      switch (GET_CODE (x))
	{
	case IOR:
	  fprintf (file, "bior");
	  break;
	case XOR:
	  fprintf (file, "bixor");
	  break;
	case AND:
	  fprintf (file, "biand");
	  break;
	default:
	  break;
	}
      break;
    case 'd':
      switch (GET_CODE (x))
	{
	case EQ:
	  fprintf (file, "bcc");
	  break;
	case NE:
	  fprintf (file, "bcs");
	  break;
	default:
	  abort ();
	}
      break;
    case 'e':
      switch (GET_CODE (x))
	{
	case REG:
	  if (TARGET_H8300)
	    fprintf (file, "%s", names_big[REGNO (x)]);
	  else
	    fprintf (file, "%s", names_upper_extended[REGNO (x)]);
	  break;
	case MEM:
	  x = adj_offsettable_operand (x, 0);
	  print_operand (file, x, 0);
	  break;
	case CONST_INT:
	  fprintf (file, "#%d", ((INTVAL (x) >> 16) & 0xffff));
	  break;
	case CONST_DOUBLE:
	  {
	    long val;
	    REAL_VALUE_TYPE rv;
	    REAL_VALUE_FROM_CONST_DOUBLE (rv, x);
	    REAL_VALUE_TO_TARGET_SINGLE (rv, val);
	    fprintf (file, "#%ld", ((val >> 16) & 0xffff));
	    break;
	  }
	default:
	  abort ();
	  break;
	}
      break;
    case 'f':
      switch (GET_CODE (x))
	{
	case REG:
	  if (TARGET_H8300)
	    fprintf (file, "%s", names_big[REGNO (x) + 1]);
	  else
	    fprintf (file, "%s", names_big[REGNO (x)]);
	  break;
	case MEM:
	  x = adj_offsettable_operand (x, 2);
	  print_operand (file, x, 0);
	  break;
	case CONST_INT:
	  fprintf (file, "#%d", INTVAL (x) & 0xffff);
	  break;
	case CONST_DOUBLE:
	  {
	    long val;
	    REAL_VALUE_TYPE rv;
	    REAL_VALUE_FROM_CONST_DOUBLE (rv, x);
	    REAL_VALUE_TO_TARGET_SINGLE (rv, val);
	    fprintf (file, "#%ld", (val & 0xffff));
	    break;
	  }
	default:
	  abort ();
	}
      break;
    case 'g':
      switch (GET_CODE (x))
	{
	case NE:
	  fprintf (file, "bcc");
	  break;
	case EQ:
	  fprintf (file, "bcs");
	  break;
	default:
	  abort ();
	}
      break;
    case 'j':
      asm_fprintf (file, cond_string (GET_CODE (x)));
      break;
    case 'k':
      asm_fprintf (file, cond_string (reverse_condition (GET_CODE (x))));
      break;
    case 's':
      if (GET_CODE (x) == CONST_INT)
	fprintf (file, "#%d", (INTVAL (x)) & 0xff);
      else
	fprintf (file, "%s", byte_reg (x, 0));
      break;
    case 't':
      if (GET_CODE (x) == CONST_INT)
	fprintf (file, "#%d", (INTVAL (x) >> 8) & 0xff);
      else
	fprintf (file, "%s", byte_reg (x, 1));
      break;
    case 'u':
      if (GET_CODE (x) != CONST_INT)
	abort ();
      fprintf (file, "%d", INTVAL (x));
      break;
    case 'w':
      if (GET_CODE (x) == CONST_INT)
	fprintf (file, "#%d", INTVAL (x) & 0xff);
      else
	fprintf (file, "%s",
		 byte_reg (x, TARGET_H8300 ? 2 : 0));
      break;
    case 'x':
      if (GET_CODE (x) == CONST_INT)
	fprintf (file, "#%d", (INTVAL (x) >> 8) & 0xff);
      else
	fprintf (file, "%s",
		 byte_reg (x, TARGET_H8300 ? 3 : 1));
      break;
    case 'y':
      if (GET_CODE (x) == CONST_INT)
	fprintf (file, "#%d", (INTVAL (x) >> 16) & 0xff);
      else
	fprintf (file, "%s", byte_reg (x, 0));
      break;
    case 'z':
      if (GET_CODE (x) == CONST_INT)
	fprintf (file, "#%d", (INTVAL (x) >> 24) & 0xff);
      else
	fprintf (file, "%s", byte_reg (x, 1));
      break;

    default:
    def:
      switch (GET_CODE (x))
	{
	case REG:
	  switch (GET_MODE (x))
	    {
	    case QImode:
#if 0 /* Is it asm ("mov.b %0,r2l", ...) */
	      fprintf (file, "%s", byte_reg (x, 0));
#else /* ... or is it asm ("mov.b %0l,r2l", ...) */
	      fprintf (file, "%s", names_big[REGNO (x)]);
#endif
	      break;
	    case HImode:
	      fprintf (file, "%s", names_big[REGNO (x)]);
	      break;
	    case SImode:
	    case SFmode:
	      fprintf (file, "%s", names_extended[REGNO (x)]);
	      break;
	    default:
	      abort ();
	    }
	  break;

	case MEM:
	  fprintf (file, "@");
	  output_address (XEXP (x, 0));

	  /* If this is an 'R' operand (reference into the 8-bit
	     area), then specify a symbolic address as "foo:8",
	     otherwise if operand is still in eight bit section, use
	     "foo:16".  */
 	  if (GET_CODE (XEXP (x, 0)) == SYMBOL_REF
	      && SYMBOL_REF_FLAG (XEXP (x, 0)))
	    fprintf (file, (code == 'R' ? ":8" : ":16"));
	  else if (GET_CODE (XEXP (x, 0)) == SYMBOL_REF
	      && TINY_DATA_NAME_P (XSTR (XEXP (x, 0), 0)))
	    fprintf (file, ":16");
	  break;

	case CONST_INT:
	case SYMBOL_REF:
	case CONST:
	case LABEL_REF:
	  fprintf (file, "#");
	  print_operand_address (file, x);
	  break;
	case CONST_DOUBLE:
	  {
	    long val;
	    REAL_VALUE_TYPE rv;
	    REAL_VALUE_FROM_CONST_DOUBLE (rv, x);
	    REAL_VALUE_TO_TARGET_SINGLE (rv, val);
	    fprintf (file, "#%ld", val);
	    break;
	  }
	default:
	  break;
	}
    }
}

/* Output assembly language output for the address ADDR to FILE.  */

void
print_operand_address (file, addr)
     FILE *file;
     rtx addr;
{
  switch (GET_CODE (addr))
    {
    case REG:
      fprintf (file, "%s", h8_reg_names[REGNO (addr)]);
      break;

    case PRE_DEC:
      fprintf (file, "-%s", h8_reg_names[REGNO (XEXP (addr, 0))]);
      break;

    case POST_INC:
      fprintf (file, "%s+", h8_reg_names[REGNO (XEXP (addr, 0))]);
      break;

    case PLUS:
      fprintf (file, "(");
      if (GET_CODE (XEXP (addr, 0)) == REG)
	{
	  /* reg,foo */
	  print_operand_address (file, XEXP (addr, 1));
	  fprintf (file, ",");
	  print_operand_address (file, XEXP (addr, 0));
	}
      else
	{
	  /* foo+k */
	  print_operand_address (file, XEXP (addr, 0));
	  fprintf (file, "+");
	  print_operand_address (file, XEXP (addr, 1));
	}
      fprintf (file, ")");
      break;

    case CONST_INT:
      {
	/* Since the h8/300 only has 16 bit pointers, negative values are also
	   those >= 32768.  This happens for example with pointer minus a
	   constant.  We don't want to turn (char *p - 2) into
	   (char *p + 65534) because loop unrolling can build upon this
	   (IE: char *p + 131068).  */
	int n = INTVAL (addr);
	if (TARGET_H8300)
	  n = (int) (short) n;
	if (n < 0)
	  /* ??? Why the special case for -ve values? */
	  fprintf (file, "-%d", -n);
	else
	  fprintf (file, "%d", n);
	break;
      }

    default:
      output_addr_const (file, addr);
      break;
    }
}

/* Output all insn addresses and their sizes into the assembly language
   output file.  This is helpful for debugging whether the length attributes
   in the md file are correct.  This is not meant to be a user selectable
   option.  */

void
final_prescan_insn (insn, operand, num_operands)
     rtx insn, *operand ATTRIBUTE_UNUSED;
     int num_operands ATTRIBUTE_UNUSED;
{
  /* This holds the last insn address.  */
  static int last_insn_address = 0;

  int uid = INSN_UID (insn);

  if (TARGET_RTL_DUMP)
    {
      fprintf (asm_out_file, "\n****************");
      print_rtl (asm_out_file, PATTERN (insn));
      fprintf (asm_out_file, "\n");
    }

  if (TARGET_ADDRESSES)
    {
      fprintf (asm_out_file, "; 0x%x %d\n", insn_addresses[uid],
	       insn_addresses[uid] - last_insn_address);
      last_insn_address = insn_addresses[uid];
    }
}

/* Prepare for an SI sized move.  */

int
do_movsi (operands)
     rtx operands[];
{
  rtx src = operands[1];
  rtx dst = operands[0];
  if (!reload_in_progress && !reload_completed)
    {
      if (!register_operand (dst, GET_MODE (dst)))
	{
	  rtx tmp = gen_reg_rtx (GET_MODE (dst));
	  emit_move_insn (tmp, src);
	  operands[1] = tmp;
	}
    }
  return 0;
}

/* Function for INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET).
   Define the offset between two registers, one to be eliminated, and the other
   its replacement, at the start of a routine.  */

int
initial_offset (from, to)
     int from, to;
{
  int offset = 0;

  if (from == ARG_POINTER_REGNUM && to == FRAME_POINTER_REGNUM)
    offset = UNITS_PER_WORD + frame_pointer_needed * UNITS_PER_WORD;
  else
    {
      int regno;

      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
	if (WORD_REG_USED (regno))
	  offset += UNITS_PER_WORD;

      /* See the comments for get_frame_size.  We need to round it up to
	 STACK_BOUNDARY.  */

      offset += ((get_frame_size () + STACK_BOUNDARY / BITS_PER_UNIT - 1)
		 & ~(STACK_BOUNDARY / BITS_PER_UNIT - 1));

      if (from == ARG_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
	offset += UNITS_PER_WORD;	/* Skip saved PC */
    }
  return offset;
}

/* Update the condition code from the insn.  */

void
notice_update_cc (body, insn)
     rtx body;
     rtx insn;
{
  switch (get_attr_cc (insn))
    {
    case CC_NONE:
      /* Insn does not affect CC at all.  */
      break;

    case CC_NONE_0HIT:
      /* Insn does not change CC, but the 0'th operand has been changed.  */
      if (cc_status.value1 != 0
	  && reg_overlap_mentioned_p (recog_data.operand[0], cc_status.value1))
	cc_status.value1 = 0;
      break;

    case CC_SET_ZN:
      /* Insn sets the Z,N flags of CC to recog_data.operand[0].
	 The V flag is unusable.  The C flag may or may not be known but
	 that's ok because alter_cond will change tests to use EQ/NE.  */
      CC_STATUS_INIT;
      cc_status.flags |= CC_OVERFLOW_UNUSABLE | CC_NO_CARRY;
      cc_status.value1 = recog_data.operand[0];
      break;

    case CC_SET_ZNV:
      /* Insn sets the Z,N,V flags of CC to recog_data.operand[0].
	 The C flag may or may not be known but that's ok because
	 alter_cond will change tests to use EQ/NE.  */
      CC_STATUS_INIT;
      cc_status.flags |= CC_NO_CARRY;
      cc_status.value1 = recog_data.operand[0];
      break;

    case CC_COMPARE:
      /* The insn is a compare instruction.  */
      CC_STATUS_INIT;
      cc_status.value1 = SET_SRC (body);
      break;

    case CC_CLOBBER:
      /* Insn doesn't leave CC in a usable state.  */
      CC_STATUS_INIT;
      break;
    }
}

/* Recognize valid operators for bit instructions */

int
bit_operator (x, mode)
     rtx x;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  enum rtx_code code = GET_CODE (x);

  return (code == XOR
	  || code == AND
	  || code == IOR);
}

/* Shifts.

   We devote a fair bit of code to getting efficient shifts since we can only
   shift one bit at a time on the H8/300 and H8/300H and only one or two
   bits at a time on the H8/S.

   The basic shift methods:

     * loop shifts -- emit a loop using one (or two on H8/S) bit shifts;
     this is the default.  SHIFT_LOOP

     * inlined shifts -- emit straight line code for the shift; this is
     used when a straight line shift is about the same size or smaller
     than a loop.  We allow the inline version to be slightly longer in
     some cases as it saves a register.  SHIFT_INLINE

     * rotate + and -- rotate the value the opposite direction, then
     mask off the values we don't need.  This is used when only a few
     of the bits in the original value will survive in the shifted value.
     Again, this is used when it's about the same size or smaller than
     a loop.  We allow this version to be slightly longer as it is usually
     much faster than a loop.  SHIFT_ROT_AND

     * swap (+ shifts) -- often it's possible to swap bytes/words to
     simulate a shift by 8/16.  Once swapped a few inline shifts can be
     added if the shift count is slightly more than 8 or 16.  This is used
     when it's about the same size or smaller than a loop.  We allow this
     version to be slightly longer as it is usually much faster than a loop.
     SHIFT_SPECIAL

     * There other oddballs.  Not worth explaining.  SHIFT_SPECIAL


   Here are some thoughts on what the absolutely positively best code is.
   "Best" here means some rational trade-off between code size and speed,
   where speed is more preferred but not at the expense of generating 20 insns.

   A trailing '*' after the shift count indicates the "best" mode isn't
   implemented.
   
   H8/300 QImode shifts
   1-4    - do them inline
   5-6    - ASHIFT | LSHIFTRT: rotate, mask off other bits
            ASHIFTRT: loop
   7      - ASHIFT | LSHIFTRT: rotate, mask off other bits
            ASHIFTRT: shll, subx (propagate carry bit to all bits)

   H8/300 HImode shifts
   1-4    - do them inline
   5-6    - loop
   7      - shift 2nd half other way into carry.
	    copy 1st half into 2nd half
	    rotate 2nd half other way with carry
	    rotate 1st half other way (no carry)
	    mask off bits in 1st half (ASHIFT | LSHIFTRT).
	    sign extend 1st half (ASHIFTRT)
   8      - move byte, zero (ASHIFT | LSHIFTRT) or sign extend other (ASHIFTRT)
   9-12   - do shift by 8, inline remaining shifts
   13-14* - ASHIFT | LSHIFTRT: rotate 3/2, mask, move byte, set other byte to 0
          - ASHIFTRT: loop
   15     - ASHIFT | LSHIFTRT: rotate 1, mask, move byte, set other byte to 0
          - ASHIFTRT: shll, subx, set other byte

   H8/300 SImode shifts
   1-2    - do them inline
   3-6    - loop
   7*     - shift other way once, move bytes into place,
            move carry into place (possibly with sign extension)
   8      - move bytes into place, zero or sign extend other
   9-14   - loop
   15*    - shift other way once, move word into place, move carry into place
   16     - move word, zero or sign extend other
   17-23  - loop
   24*    - move bytes into place, zero or sign extend other
   25-27  - loop
   28-30* - ASHIFT | LSHIFTRT: rotate top byte, mask, move byte into place,
                               zero others
            ASHIFTRT: loop
   31     - ASHIFT | LSHIFTRT: rotate top byte, mask, move byte into place,
                               zero others
            ASHIFTRT: shll top byte, subx, copy to other bytes

   H8/300H QImode shifts (same as H8/300 QImode shifts)
   1-4    - do them inline
   5-6    - ASHIFT | LSHIFTRT: rotate, mask off other bits
            ASHIFTRT: loop
   7      - ASHIFT | LSHIFTRT: rotate, mask off other bits
            ASHIFTRT: shll, subx (propagate carry bit to all bits)


   H8/300H HImode shifts
   1-4    - do them inline
   5-6    - loop
   7      - shift 2nd half other way into carry.
	    copy 1st half into 2nd half
	    rotate entire word other way using carry
	    mask off remaining bits  (ASHIFT | LSHIFTRT)
	    sign extend remaining bits (ASHIFTRT)
   8      - move byte, zero (ASHIFT | LSHIFTRT) or sign extend other (ASHIFTRT)
   9-12   - do shift by 8, inline remaining shifts
   13-14  - ASHIFT | LSHIFTRT: rotate 3/2, mask, move byte, set other byte to 0
          - ASHIFTRT: loop
   15     - ASHIFT | LSHIFTRT: rotate 1, mask, move byte, set other byte to 0
          - ASHIFTRT: shll, subx, set other byte

   H8/300H SImode shifts
   (These are complicated by the fact that we don't have byte level access to
   the top word.)
   A word is: bytes 3,2,1,0 (msb -> lsb), word 1,0 (msw -> lsw)
   1-4    - do them inline
   5-14   - loop
   15*    - shift other way once, move word into place, move carry into place
            (with sign extension for ASHIFTRT)
   16     - move word into place, zero or sign extend other
   17-20  - do 16bit shift, then inline remaining shifts
   20-23  - loop
   24*    - ASHIFT: move byte 0(msb) to byte 1, zero byte 0,
                    move word 0 to word 1, zero word 0
            LSHIFTRT: move word 1 to word 0, move byte 1 to byte 0,
                      zero word 1, zero byte 1
            ASHIFTRT: move word 1 to word 0, move byte 1 to byte 0,
                      sign extend byte 0, sign extend word 0
   25-27* - either loop, or
            do 24 bit shift, inline rest
   28-30  - ASHIFT: rotate 4/3/2, mask
            LSHIFTRT: rotate 4/3/2, mask
            ASHIFTRT: loop
   31     - shll, subx byte 0, sign extend byte 0, sign extend word 0

   H8/S QImode shifts
   1-6    - do them inline
   7      - ASHIFT | LSHIFTRT: rotate, mask off other bits
            ASHIFTRT: shll, subx (propagate carry bit to all bits)

   H8/S HImode shifts
   1-7	  - do them inline
   8      - move byte, zero (ASHIFT | LSHIFTRT) or sign extend other (ASHIFTRT)
   9-12   - do shift by 8, inline remaining shifts
   13-14  - ASHIFT | LSHIFTRT: rotate 3/2, mask, move byte, set other byte to 0
          - ASHIFTRT: loop
   15     - ASHIFT | LSHIFTRT: rotate 1, mask, move byte, set other byte to 0
          - ASHIFTRT: shll, subx, set other byte

   H8/S SImode shifts
   (These are complicated by the fact that we don't have byte level access to
   the top word.)
   A word is: bytes 3,2,1,0 (msb -> lsb), word 1,0 (msw -> lsw)
   1-10   - do them inline
   11-14  - loop
   15*    - shift other way once, move word into place, move carry into place
            (with sign extension for ASHIFTRT)
   16     - move word into place, zero or sign extend other
   17-20  - do 16bit shift, then inline remaining shifts
   20-23  - loop
   24*    - ASHIFT: move byte 0(msb) to byte 1, zero byte 0,
                    move word 0 to word 1, zero word 0
            LSHIFTRT: move word 1 to word 0, move byte 1 to byte 0,
                      zero word 1, zero byte 1
            ASHIFTRT: move word 1 to word 0, move byte 1 to byte 0,
                      sign extend byte 0, sign extend word 0
   25-27* - either loop, or
            do 24 bit shift, inline rest
   28-30  - ASHIFT: rotate 4/3/2, mask
            LSHIFTRT: rotate 4/3/2, mask
            ASHIFTRT: loop
   31     - shll, subx byte 0, sign extend byte 0, sign extend word 0

   Panic!!!  */

int
nshift_operator (x, mode)
     rtx x;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  switch (GET_CODE (x))
    {
    case ASHIFTRT:
    case LSHIFTRT:
    case ASHIFT:
      return 1;

    default:
      return 0;
    }
}

/* Called from the .md file to emit code to do shifts.
   Returns a boolean indicating success
   (currently this is always TRUE).  */

int
expand_a_shift (mode, code, operands)
     enum machine_mode mode;
     int code;
     rtx operands[];
{
  emit_move_insn (operands[0], operands[1]);

  /* need a loop to get all the bits we want  - we generate the
     code at emit time, but need to allocate a scratch reg now  */

  emit_insn (gen_rtx_PARALLEL
	     (VOIDmode,
	      gen_rtvec (2,
			 gen_rtx_SET (VOIDmode, operands[0],
				      gen_rtx (code, mode, operands[0],
					       operands[2])),
			 gen_rtx_CLOBBER (VOIDmode,
					  gen_rtx_SCRATCH (QImode)))));

  return 1;
}

/* Shift algorithm determination.

   There are various ways of doing a shift:
   SHIFT_INLINE: If the amount is small enough, just generate as many one-bit
                 shifts as we need.
   SHIFT_ROT_AND: If the amount is large but close to either end, rotate the
                  necessary bits into position and then set the rest to zero.
   SHIFT_SPECIAL: Hand crafted assembler.
   SHIFT_LOOP:    If the above methods fail, just loop.  */

enum shift_alg
{
  SHIFT_INLINE,
  SHIFT_ROT_AND,
  SHIFT_SPECIAL,
  SHIFT_LOOP,
  SHIFT_MAX
};

/* Symbols of the various shifts which can be used as indices.  */

enum shift_type
  {
    SHIFT_ASHIFT, SHIFT_LSHIFTRT, SHIFT_ASHIFTRT
  };

/* Symbols of the various modes which can be used as indices.  */

enum shift_mode
  {
    QIshift, HIshift, SIshift
  };

/* For single bit shift insns, record assembler and what bits of the
   condition code are valid afterwards (represented as various CC_FOO
   bits, 0 means CC isn't left in a usable state).  */

struct shift_insn
{
  const char *assembler;
  int cc_valid;
};

/* Assembler instruction shift table.

   These tables are used to look up the basic shifts.
   They are indexed by cpu, shift_type, and mode.
*/

static const struct shift_insn shift_one[2][3][3] =
{
/* H8/300 */
  {
/* SHIFT_ASHIFT */
    {
      { "shll\t%X0", CC_NO_CARRY },
      { "add.w\t%T0,%T0", CC_OVERFLOW_UNUSABLE | CC_NO_CARRY },
      { "add.w\t%f0,%f0\n\taddx\t%y0,%y0\n\taddx\t%z0,%z0", 0 }
    },
/* SHIFT_LSHIFTRT */
    {
      { "shlr\t%X0", CC_NO_CARRY },
      { "shlr\t%t0\n\trotxr\t%s0", 0 },
      { "shlr\t%z0\n\trotxr\t%y0\n\trotxr\t%x0\n\trotxr\t%w0", 0 }
    },
/* SHIFT_ASHIFTRT */
    {
      { "shar\t%X0", CC_OVERFLOW_UNUSABLE | CC_NO_CARRY },
      { "shar\t%t0\n\trotxr\t%s0", 0 },
      { "shar\t%z0\n\trotxr\t%y0\n\trotxr\t%x0\n\trotxr\t%w0", 0 }
    }
  },
/* H8/300H */
  {
/* SHIFT_ASHIFT */
    {
      { "shll.b\t%X0", CC_NO_CARRY },
      { "shll.w\t%T0", CC_NO_CARRY },
      { "shll.l\t%S0", CC_NO_CARRY }
    },
/* SHIFT_LSHIFTRT */
    {
      { "shlr.b\t%X0", CC_NO_CARRY },
      { "shlr.w\t%T0", CC_NO_CARRY },
      { "shlr.l\t%S0", CC_NO_CARRY }
    },
/* SHIFT_ASHIFTRT */
    {
      { "shar.b\t%X0", CC_OVERFLOW_UNUSABLE | CC_NO_CARRY },
      { "shar.w\t%T0", CC_OVERFLOW_UNUSABLE | CC_NO_CARRY },
      { "shar.l\t%S0", CC_OVERFLOW_UNUSABLE | CC_NO_CARRY }
    }
  }
};

static const struct shift_insn shift_two[3][3] =
{
/* SHIFT_ASHIFT */
    {
      { "shll.b\t#2,%X0", CC_NO_CARRY },
      { "shll.w\t#2,%T0", CC_NO_CARRY },
      { "shll.l\t#2,%S0", CC_NO_CARRY }
    },
/* SHIFT_LSHIFTRT */
    {
      { "shlr.b\t#2,%X0", CC_NO_CARRY },
      { "shlr.w\t#2,%T0", CC_NO_CARRY },
      { "shlr.l\t#2,%S0", CC_NO_CARRY }
    },
/* SHIFT_ASHIFTRT */
    {
      { "shar.b\t#2,%X0", CC_OVERFLOW_UNUSABLE | CC_NO_CARRY },
      { "shar.w\t#2,%T0", CC_OVERFLOW_UNUSABLE | CC_NO_CARRY },
      { "shar.l\t#2,%S0", CC_OVERFLOW_UNUSABLE | CC_NO_CARRY }
    }
};

/* Rotates are organized by which shift they'll be used in implementing.
   There's no need to record whether the cc is valid afterwards because
   it is the AND insn that will decide this.  */

static const char *const rotate_one[2][3][3] =
{
/* H8/300 */
  {
/* SHIFT_ASHIFT */
    {
      "rotr\t%X0",
      "shlr\t%t0\n\trotxr\t%s0\n\tbst\t#7,%t0",
      0
    },
/* SHIFT_LSHIFTRT */
    {
      "rotl\t%X0",
      "shll\t%s0\n\trotxl\t%t0\n\tbst\t#0,%s0",
      0
    },
/* SHIFT_ASHIFTRT */
    {
      "rotl\t%X0",
      "shll\t%s0\n\trotxl\t%t0\n\tbst\t#0,%s0",
      0
    }
  },
/* H8/300H */
  {
/* SHIFT_ASHIFT */
    {
      "rotr.b\t%X0",
      "rotr.w\t%T0",
      "rotr.l\t%S0"
    },
/* SHIFT_LSHIFTRT */
    {
      "rotl.b\t%X0",
      "rotl.w\t%T0",
      "rotl.l\t%S0"
    },
/* SHIFT_ASHIFTRT */
    {
      "rotl.b\t%X0",
      "rotl.w\t%T0",
      "rotl.l\t%S0"
    }
  }
};

static const char *const rotate_two[3][3] =
{
/* SHIFT_ASHIFT */
    {
      "rotr.b\t#2,%X0",
      "rotr.w\t#2,%T0",
      "rotr.l\t#2,%S0"
    },
/* SHIFT_LSHIFTRT */
    {
      "rotl.b\t#2,%X0",
      "rotl.w\t#2,%T0",
      "rotl.l\t#2,%S0"
    },
/* SHIFT_ASHIFTRT */
    {
      "rotl.b\t#2,%X0",
      "rotl.w\t#2,%T0",
      "rotl.l\t#2,%S0"
    }
};

static enum shift_alg get_shift_alg PARAMS ((enum attr_cpu, enum shift_type,
					     enum machine_mode, int,
					     const char **, const char **,
					     int *));

/* Given CPU, MODE, SHIFT_TYPE, and shift count COUNT, determine the best
   algorithm for doing the shift.  The assembler code is stored in ASSEMBLER.
   We don't achieve maximum efficiency in all cases, but the hooks are here
   to do so.

   For now we just use lots of switch statements.  Since we don't even come
   close to supporting all the cases, this is simplest.  If this function ever
   gets too big, perhaps resort to a more table based lookup.  Of course,
   at this point you may just wish to do it all in rtl.

   WARNING: The constraints on insns shiftbyn_QI/HI/SI assume shifts of
   1,2,3,4 will be inlined (1,2 for SI).  */

static enum shift_alg
get_shift_alg (cpu, shift_type, mode, count, assembler_p,
	       assembler2_p, cc_valid_p)
     enum attr_cpu cpu;
     enum shift_type shift_type;
     enum machine_mode mode;
     int count;
     const char **assembler_p;
     const char **assembler2_p;
     int *cc_valid_p;
{
  /* The default is to loop.  */
  enum shift_alg alg = SHIFT_LOOP;
  enum shift_mode shift_mode;

  /* We don't handle negative shifts or shifts greater than the word size,
     they should have been handled already.  */

  if (count < 0 || count > GET_MODE_BITSIZE (mode))
    abort ();

  switch (mode)
    {
    case QImode:
      shift_mode = QIshift;
      break;
    case HImode:
      shift_mode = HIshift;
      break;
    case SImode:
      shift_mode = SIshift;
      break;
    default:
      abort ();
    }

  /* Assume either SHIFT_LOOP or SHIFT_INLINE.
     It is up to the caller to know that looping clobbers cc.  */
  *assembler_p = shift_one[cpu][shift_type][shift_mode].assembler;
  if (TARGET_H8300S)
    *assembler2_p = shift_two[shift_type][shift_mode].assembler;
  else
    *assembler2_p = NULL;
  *cc_valid_p = shift_one[cpu][shift_type][shift_mode].cc_valid;

  /* Now look for cases we want to optimize.  */

  switch (shift_mode)
    {
    case QIshift:
      if (count <= 4)
	return SHIFT_INLINE;
      else
	{
	  /* Shift by 5/6 are only 3 insns on the H8/S, so it's just as
	     fast as SHIFT_ROT_AND, plus CC is valid.  */
	  if (TARGET_H8300S && count <= 6)
	    return SHIFT_INLINE;

	  /* For ASHIFTRT by 7 bits, the sign bit is simply replicated
	     through the entire value.  */
	  if (shift_type == SHIFT_ASHIFTRT && count == 7)
	    {
	      *assembler_p = "shll\t%X0\n\tsubx\t%X0,%X0";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    }

	  /* Other ASHIFTRTs are too much of a pain.  */
	  if (shift_type == SHIFT_ASHIFTRT)
	    return SHIFT_LOOP;

	  /* Other shifts by 5, 6, or 7 bits use SHIFT_ROT_AND.  */
	  *assembler_p = rotate_one[cpu][shift_type][shift_mode];
	  if (TARGET_H8300S)
	    *assembler2_p = rotate_two[shift_type][shift_mode];
	  *cc_valid_p = 0;
	  return SHIFT_ROT_AND;
	}

    case HIshift:
      if (count <= 4)
	return SHIFT_INLINE;
      else if (TARGET_H8300S && count <= 7)
	return SHIFT_INLINE;
      else if (count == 7)
	{
	  if (shift_type == SHIFT_ASHIFT && TARGET_H8300)
	    {
	      *assembler_p = "shar.b\t%t0\n\tmov.b\t%s0,%t0\n\trotxr.b\t%t0\n\trotr.b\t%s0\n\tand.b\t#0x80,%s0";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    }

	  if (shift_type == SHIFT_ASHIFT && TARGET_H8300H)
	    {
	      *assembler_p = "shar.b\t%t0\n\tmov.b\t%s0,%t0\n\trotxr.w\t%T0\n\tand.b\t#0x80,%s0";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    }

	  if (shift_type == SHIFT_LSHIFTRT && TARGET_H8300)
	    {
	      *assembler_p = "shal.b\t%s0\n\tmov.b\t%t0,%s0\n\trotxl.b\t%s0\n\trotl.b\t%t0\n\tand.b\t#0x01,%t0";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    }

	  if (shift_type == SHIFT_LSHIFTRT && TARGET_H8300H)
	    {
	      *assembler_p = "shal.b\t%s0\n\tmov.b\t%t0,%s0\n\trotxl.w\t%T0\n\tand.b\t#0x01,%t0";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    }

	  if (shift_type == SHIFT_ASHIFTRT)
	    {
	      *assembler_p = "shal.b\t%s0\n\tmov.b\t%t0,%s0\n\trotxl.b\t%s0\n\tsubx\t%t0,%t0";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    }
	}
      else if (count == 8)
	{
	  switch (shift_type)
	    {
	    case SHIFT_ASHIFT:
	      *assembler_p = "mov.b\t%s0,%t0\n\tsub.b\t%s0,%s0";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    case SHIFT_LSHIFTRT:
	      *assembler_p = "mov.b\t%t0,%s0\n\tsub.b\t%t0,%t0";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    case SHIFT_ASHIFTRT:
	      if (TARGET_H8300)
		*assembler_p = "mov.b\t%t0,%s0\n\tshll\t%t0\n\tsubx\t%t0,%t0\t";
	      else
		*assembler_p = "mov.b\t%t0,%s0\n\texts.w\t%T0";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    }
	}
      else if (count == 9)
	{
	  switch (shift_type)
	    {
	    case SHIFT_ASHIFT:
	      *assembler_p = "mov.b\t%s0,%t0\n\tsub.b\t%s0,%s0\n\tshal.b\t%t0";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    case SHIFT_LSHIFTRT:
	      *assembler_p = "mov.b\t%t0,%s0\n\tsub.b\t%t0,%t0\n\tshlr.b\t%s0";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    case SHIFT_ASHIFTRT:
	      if (TARGET_H8300)
		*assembler_p = "mov.b\t%t0,%s0\n\tbld\t#7,%s0\n\tsubx\t%t0,%t0\n\tshar.b\t%s0";
	      else
		*assembler_p = "mov.b\t%t0,%s0\n\texts.w\t%T0\n\tshar.b\t%s0";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    }
	}
      else if (count == 10)
	{
	  switch (shift_type)
	    {
	    case SHIFT_ASHIFT:
	      if (TARGET_H8300S)
		*assembler_p = "mov.b\t%s0,%t0\n\tsub.b\t%s0,%s0\n\tshal.b\t#2,%t0\n\t";
	      else
		*assembler_p = "mov.b\t%s0,%t0\n\tsub.b\t%s0,%s0\n\tshal.b\t%t0\n\tshal.b\t%t0";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    case SHIFT_LSHIFTRT:
	      if (TARGET_H8300S)
		*assembler_p = "mov.b\t%t0,%s0\n\tsub.b\t%t0,%t0\n\tshlr.b\t#2,%s0";
	      else
		*assembler_p = "mov.b\t%t0,%s0\n\tsub.b\t%t0,%t0\n\tshlr.b\t%s0\n\tshlr.b\t%s0";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    case SHIFT_ASHIFTRT:
	      if (TARGET_H8300)
		*assembler_p = "mov.b\t%t0,%s0\n\tbld\t#7,%s0\n\tsubx\t%t0,%t0\n\tshar.b\t%s0\n\tshar.b\t%s0";
	      else if (TARGET_H8300H)
		*assembler_p = "mov.b\t%t0,%s0\n\texts.w\t%T0\n\tshar.b\t%s0\n\tshar.b\t%s0";
	      else if (TARGET_H8300S)
		*assembler_p = "mov.b\t%t0,%s0\n\texts.w\t%T0\n\tshar.b\t#2,%s0";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    }
	}
      else if (count == 11)
	{
	  switch (shift_type)
	    {
	    case SHIFT_ASHIFT:
	      if (TARGET_H8300S)
		*assembler_p = "mov.b\t%s0,%t0\n\tsub.b\t%s0,%s0\n\tshal.b\t#2,%t0\n\tshal.b\t%t0";
	      else
		*assembler_p = "mov.b\t%s0,%t0\n\tsub.b\t%s0,%s0\n\tshal.b\t%t0\n\tshal.b\t%t0\n\tshal.b\t%t0";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    case SHIFT_LSHIFTRT:
	      if (TARGET_H8300S)
		*assembler_p = "mov.b\t%t0,%s0\n\tsub.b\t%t0,%t0\n\tshlr.b\t#2,%s0\n\tshlr.b\t%s0";
	      else
		*assembler_p = "mov.b\t%t0,%s0\n\tsub.b\t%t0,%t0\n\tshlr.b\t%s0\n\tshlr.b\t%s0\n\tshlr.b\t%s0";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    case SHIFT_ASHIFTRT:
	      if (TARGET_H8300)
		*assembler_p = "mov.b\t%t0,%s0\n\tbld\t#7,%s0\n\tsubx\t%t0,%t0\n\tshar.b\t%s0\n\tshar.b\t%s0\n\tshar.b\t%s0";
	      else if (TARGET_H8300H)
		*assembler_p = "mov.b\t%t0,%s0\n\texts.w\t%T0\n\tshar.b\t%s0\n\tshar.b\t%s0\n\tshar.b\t%s0";
	      else if (TARGET_H8300S)
		*assembler_p = "mov.b\t%t0,%s0\n\texts.w\t%T0\n\tshar.b\t#2,%s0\n\tshar.b\t%s0";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    }
	}
      else if (count == 12)
	{
	  switch (shift_type)
	    {
	    case SHIFT_ASHIFT:
	      if (TARGET_H8300S)
		*assembler_p = "mov.b\t%s0,%t0\n\tsub.b\t%s0,%s0\n\tshal.b\t#2,%t0\n\tshal.b\t#2,%t0";
	      else
		*assembler_p = "mov.b\t%s0,%t0\n\tsub.b\t%s0,%s0\n\tshal.b\t%t0\n\tshal.b\t%t0\n\tshal.b\t%t0\n\tshal.b\t%t0";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    case SHIFT_LSHIFTRT:
	      if (TARGET_H8300S)
		*assembler_p = "mov.b\t%t0,%s0\n\tsub.b\t%t0,%t0\n\tshlr.b\t#2,%s0\n\tshlr.b\t#2,%s0";
	      else
		*assembler_p = "mov.b\t%t0,%s0\n\tsub.b\t%t0,%t0\n\tshlr.b\t%s0\n\tshlr.b\t%s0\n\tshlr.b\t%s0\n\tshlr.b\t%s0";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    case SHIFT_ASHIFTRT:
	      if (TARGET_H8300)
		*assembler_p = "mov.b\t%t0,%s0\n\tbld\t#7,%s0\n\tsubx\t%t0,%t0\n\tshar.b\t%s0\n\tshar.b\t%s0\n\tshar.b\t%s0\n\tshar.b\t%s0";
	      else if (TARGET_H8300H)
		*assembler_p = "mov.b\t%t0,%s0\n\textw.w\t%T0\n\tshar.b\t%s0\n\tshar.b\t%s0\n\tshar.b\t%s0\n\tshar.b\t%s0";
	      else if (TARGET_H8300S)
		*assembler_p = "mov.b\t%t0,%s0\n\textw.w\t%T0\n\tshar.b\t#2,%s0\n\tshar.b\t#2,%s0";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    }
	}
      else if ((!TARGET_H8300 && (count == 13 || count == 14))
	       || count == 15)
	{
	  if (count == 15 && shift_type == SHIFT_ASHIFTRT)
	    {
	      *assembler_p = "shll\t%t0,%t0\n\tsubx\t%t0,%t0\n\tmov.b\t%t0,%s0";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    }
	  else if (shift_type != SHIFT_ASHIFTRT)
	    {
	      *assembler_p = rotate_one[cpu][shift_type][shift_mode];
	      if (TARGET_H8300S)
	        *assembler2_p = rotate_two[shift_type][shift_mode];
	      else
		*assembler2_p = NULL;
	      *cc_valid_p = 0;
	      return SHIFT_ROT_AND;
	    }
	}
      break;

    case SIshift:
      if (count <= (TARGET_H8300 ? 2 : 4))
	return SHIFT_INLINE;
      else if (TARGET_H8300S && count <= 10)
	return SHIFT_INLINE;
      else if (count == 8 && TARGET_H8300)
	{
	  switch (shift_type)
	    {
	    case SHIFT_ASHIFT:
	      *assembler_p = "mov.b\t%y0,%z0\n\tmov.b\t%x0,%y0\n\tmov.b\t%w0,%x0\n\tsub.b\t%w0,%w0";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    case SHIFT_LSHIFTRT:
	      *assembler_p = "mov.b\t%x0,%w0\n\tmov.b\t%y0,%x0\n\tmov.b\t%z0,%y0\n\tsub.b\t%z0,%z0";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    case SHIFT_ASHIFTRT:
	      *assembler_p = "mov.b\t%x0,%w0\n\tmov.b\t%y0,%x0\n\tmov.b\t%z0,%y0\n\tshll\t%z0\n\tsubx\t%z0,%z0";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    }
	}
      else if (count == 8 && !TARGET_H8300)
	{
	  switch (shift_type)
	    {
	    case SHIFT_ASHIFT:
	      *assembler_p = "mov.w\t%e0,%f4\n\tmov.b\t%s4,%t4\n\tmov.b\t%t0,%s4\n\tmov.b\t%s0,%t0\n\tsub.b\t%s0,%s0\n\tmov.w\t%f4,%e0";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    case SHIFT_LSHIFTRT:
	      *assembler_p = "mov.w\t%e0,%f4\n\tmov.b\t%t0,%s0\n\tmov.b\t%s4,%t0\n\tmov.b\t%t4,%s4\n\textu.w\t%f4\n\tmov.w\t%f4,%e0";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    case SHIFT_ASHIFTRT:
	      *assembler_p = "mov.w\t%e0,%f4\n\tmov.b\t%t0,%s0\n\tmov.b\t%s4,%t0\n\tmov.b\t%t4,%s4\n\texts.w\t%f4\n\tmov.w\t%f4,%e0";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    }
	}
      else if (count == 16)
	{
	  switch (shift_type)
	    {
	    case SHIFT_ASHIFT:
	      *assembler_p = "mov.w\t%f0,%e0\n\tsub.w\t%f0,%f0";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    case SHIFT_LSHIFTRT:
	      *assembler_p = "mov.w\t%e0,%f0\n\tsub.w\t%e0,%e0";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    case SHIFT_ASHIFTRT:
	      if (TARGET_H8300)
		*assembler_p = "mov.w\t%e0,%f0\n\tshll\t%z0\n\tsubx\t%z0,%z0\n\tmov.b\t%z0,%y0";
	      else
		*assembler_p = "mov.w\t%e0,%f0\n\texts.l\t%S0";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    }
	}
      else if (count == 17 && !TARGET_H8300)
	{
	  switch (shift_type)
	    {
	    case SHIFT_ASHIFT:
	      *assembler_p = "mov.w\t%f0,%e0\n\tsub.w\t%f0,%f0\n\tshll.l\t%S0";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    case SHIFT_LSHIFTRT:
	      *assembler_p = "mov.w\t%e0,%f0\n\tsub.w\t%e0,%e0\n\tshlr.l\t%S0";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    case SHIFT_ASHIFTRT:
	      *assembler_p = "mov.w\t%e0,%f0\n\texts.l\t%S0\n\tshar.l\t%S0";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    }
	}
      else if (count == 18 && !TARGET_H8300)
	{
	  switch (shift_type)
	    {
	    case SHIFT_ASHIFT:
	      if (TARGET_H8300S)
		*assembler_p = "mov.w\t%f0,%e0\n\tsub.w\t%f0,%f0\n\tshll.l\t#2,%S0";
	      else
		*assembler_p = "mov.w\t%f0,%e0\n\tsub.w\t%f0,%f0\n\tshll.l\t%S0\n\tshll.l\t%S0";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    case SHIFT_LSHIFTRT:
	      if (TARGET_H8300S)
		*assembler_p = "mov.w\t%e0,%f0\n\tsub.w\t%e0,%e0\n\tshlr.l\t#2,%S0";
	      else
		*assembler_p = "mov.w\t%e0,%f0\n\tsub.w\t%e0,%e0\n\tshlr.l\t%S0\n\tshlr.l\t%S0";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    case SHIFT_ASHIFTRT:
	      if (TARGET_H8300S)
		*assembler_p = "mov.w\t%e0,%f0\n\texts.l\t%S0\n\tshar.l\t#2,%S0";
	      else
		*assembler_p = "mov.w\t%e0,%f0\n\texts.l\t%S0\n\tshar.l\t%S0\n\tshar.l\t%S0";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    }
	}
      else if (count == 19 && !TARGET_H8300)
	{
	  switch (shift_type)
	    {
	    case SHIFT_ASHIFT:
	      if (TARGET_H8300S)
		*assembler_p = "mov.w\t%f0,%e0\n\tsub.w\t%f0,%f0\n\tshll.l\t#2,%S0\n\tshll.l\t%S0";
	      else
		*assembler_p = "mov.w\t%f0,%e0\n\tsub.w\t%f0,%f0\n\tshll.l\t%S0\n\tshll.l\t%S0\n\tshll.l\t%S0";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    case SHIFT_LSHIFTRT:
	      if (TARGET_H8300S)
		*assembler_p = "mov.w\t%e0,%f0\n\tsub.w\t%e0,%e0\n\tshlr.l\t#2,%S0\n\tshlr.l\t%S0";
	      else
		*assembler_p = "mov.w\t%e0,%f0\n\tsub.w\t%e0,%e0\n\tshlr.l\t%S0\n\tshlr.l\t%S0\n\tshlr.l\t%S0";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    case SHIFT_ASHIFTRT:
	      if (TARGET_H8300S)
		*assembler_p = "mov.w\t%e0,%f0\n\texts.l\t%S0\n\tshar.l\t#2,%S0\n\tshar.l\t%S0";
	      else
		*assembler_p = "mov.w\t%e0,%f0\n\texts.l\t%S0\n\tshar.l\t%S0\n\tshar.l\t%S0\n\tshar.l\t%S0";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    }
	}
      else if (count == 20 && TARGET_H8300S)
	{
	  switch (shift_type)
	    {
	    case SHIFT_ASHIFT:
	      *assembler_p = "mov.w\t%f0,%e0\n\tsub.w\t%f0,%f0\n\tshll.l\t#2,%S0\n\tshll.l\t#2,%S0";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    case SHIFT_LSHIFTRT:
	      *assembler_p = "mov.w\t%e0,%f0\n\tsub.w\t%e0,%e0\n\tshlr.l\t#2,%S0\n\tshlr.l\t#2,%S0";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    case SHIFT_ASHIFTRT:
	      *assembler_p = "mov.w\t%e0,%f0\n\texts.l\t%S0\n\tshar.l\t#2,%S0\n\tshar.l\t#2,%S0";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    }
	}
      else if (count == 24 && !TARGET_H8300)
	{
	  switch (shift_type)
	    {
	    case SHIFT_ASHIFT:
	      *assembler_p = "mov.b\t%s0,%t0\n\tsub.b\t%s0,%s0\n\tmov.w\t%f0,%e0\n\tsub.w\t%f0,%f0";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    case SHIFT_LSHIFTRT:
	      *assembler_p = "mov.w\t%e0,%f0\n\tmov.b\t%t0,%s0\n\textu.w\t%f0\n\textu.l\t%S0";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    case SHIFT_ASHIFTRT:
	      *assembler_p = "mov.w\t%e0,%f0\n\tmov.b\t%t0,%s0\n\texts.w\t%f0\n\texts.l\t%S0";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    }
	}
      else if (count >= 28 && count <= 30 && !TARGET_H8300)
	{
	  if (shift_type == SHIFT_ASHIFTRT)
	    {
	      return SHIFT_LOOP;
	    }
	  else
	    {
	      *assembler_p = rotate_one[cpu][shift_type][shift_mode];
	      if (TARGET_H8300S)
	        *assembler2_p = rotate_two[shift_type][shift_mode];
	      else
		*assembler2_p = NULL;
	      *cc_valid_p = 0;
	      return SHIFT_ROT_AND;
	    }
	}
      else if (count == 31)
	{
	  if (shift_type == SHIFT_ASHIFTRT)
	    {
	      if (TARGET_H8300)
		*assembler_p = "shll\t%z0\n\tsubx %w0,%w0\n\tmov.b\t%w0,%x0\n\tmov.w\t%f0,%e0";
	      else
		*assembler_p = "shll\t%e0\n\tsubx\t%w0,%w0\n\tmov.b\t%w0,%x0\n\tmov.w\t%f0,%e0";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    }
	  else
	    {
	      if (TARGET_H8300)
		{
		  if (shift_type == SHIFT_ASHIFT)
		    *assembler_p = "sub.w\t%e0,%e0\n\tshlr\t%w0\n\tmov.w\t%e0,%f0\n\trotxr\t%z0";
		  else
		    *assembler_p = "sub.w\t%f0,%f0\n\tshll\t%z0\n\tmov.w\t%f0,%e0\n\trotxl\t%w0";
		  *cc_valid_p = 0;
		  return SHIFT_SPECIAL;
		}
	      else
		{
		  *assembler_p = rotate_one[cpu][shift_type][shift_mode];
	      if (TARGET_H8300S)
	        *assembler2_p = rotate_two[shift_type][shift_mode];
	      else
		*assembler2_p = NULL;
		  *cc_valid_p = 0;
		  return SHIFT_ROT_AND;
		}
	    }
	}
      break;

    default:
      abort ();
    }

  return alg;
}

/* Emit the assembler code for doing shifts.  */

const char *
emit_a_shift (insn, operands)
     rtx insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  static int loopend_lab;
  const char *assembler;
  const char *assembler2;
  int cc_valid;
  rtx shift = operands[3];
  enum machine_mode mode = GET_MODE (shift);
  enum rtx_code code = GET_CODE (shift);
  enum shift_type shift_type;
  enum shift_mode shift_mode;

  loopend_lab++;

  switch (mode)
    {
    case QImode:
      shift_mode = QIshift;
      break;
    case HImode:
      shift_mode = HIshift;
      break;
    case SImode:
      shift_mode = SIshift;
      break;
    default:
      abort ();
    }

  switch (code)
    {
    case ASHIFTRT:
      shift_type = SHIFT_ASHIFTRT;
      break;
    case LSHIFTRT:
      shift_type = SHIFT_LSHIFTRT;
      break;
    case ASHIFT:
      shift_type = SHIFT_ASHIFT;
      break;
    default:
      abort ();
    }

  if (GET_CODE (operands[2]) != CONST_INT)
    {
      /* Indexing by reg, so have to loop and test at top */
      output_asm_insn ("mov.b	%X2,%X4", operands);
      fprintf (asm_out_file, "\tble	.Lle%d\n", loopend_lab);

      /* Get the assembler code to do one shift.  */
      get_shift_alg (cpu_type, shift_type, mode, 1, &assembler,
		     &assembler2, &cc_valid);
    }
  else
    {
      int n = INTVAL (operands[2]);
      enum shift_alg alg;

      /* If the count is negative, make it 0.  */
      if (n < 0)
	n = 0;
      /* If the count is too big, truncate it.
         ANSI says shifts of GET_MODE_BITSIZE are undefined - we choose to
	 do the intuitive thing.  */
      else if (n > GET_MODE_BITSIZE (mode))
	n = GET_MODE_BITSIZE (mode);

      alg = get_shift_alg (cpu_type, shift_type, mode, n, &assembler,
			   &assembler2, &cc_valid);

      switch (alg)
	{
	case SHIFT_INLINE:
	  /* Emit two bit shifts first.  */
	  while (n > 1 && assembler2 != NULL)
	    {
	      output_asm_insn (assembler2, operands);
	      n -= 2;
	    }

	  /* Now emit one bit shifts for any residual.  */
	  while (n > 0)
	    {
	      output_asm_insn (assembler, operands);
	      n -= 1;
	    }

	  /* Keep track of CC.  */
	  if (cc_valid)
	    {
	      cc_status.value1 = operands[0];
	      cc_status.flags |= cc_valid;
	    }
	  return "";

	case SHIFT_ROT_AND:
	  {
	    int m = GET_MODE_BITSIZE (mode) - n;
	    int mask = (shift_type == SHIFT_ASHIFT
			? ((1 << (GET_MODE_BITSIZE (mode) - n)) - 1) << n
			: (1 << (GET_MODE_BITSIZE (mode) - n)) - 1);
	    char insn_buf[200];
	    /* Not all possibilities of rotate are supported.  They shouldn't
	       be generated, but let's watch for 'em.  */
	    if (assembler == 0)
	      abort ();

	    /* Emit two bit rotates first.  */
	    while (m > 1 && assembler2 != NULL)
	      {
		output_asm_insn (assembler2, operands);
		m -= 2;
	      }

	    /* Now single bit rotates for any residual.  */
	    while (m > 0)
	      {
		output_asm_insn (assembler, operands);
		m -= 1;
	      }

	    /* Now mask off the high bits.  */
	    if (TARGET_H8300)
	      {
		switch (mode)
		  {
		  case QImode:
		    sprintf (insn_buf, "and #%d,%%X0", mask);
		    cc_status.value1 = operands[0];
		    cc_status.flags |= CC_NO_CARRY;
		    break;
		  case HImode:
		    sprintf (insn_buf, "and #%d,%%s0\n\tand #%d,%%t0",
			     mask & 255, mask >> 8);
		    break;
		  case SImode:
		    abort ();
		  default:
		    break;
		  }
	      }
	    else
	      {
		sprintf (insn_buf, "and.%c #%d,%%%c0",
			 "bwl"[shift_mode], mask,
			 mode == QImode ? 'X' : mode == HImode ? 'T' : 'S');
		cc_status.value1 = operands[0];
		cc_status.flags |= CC_NO_CARRY;
	      }
	    output_asm_insn (insn_buf, operands);
	    return "";
	  }
	case SHIFT_SPECIAL:
	  output_asm_insn (assembler, operands);
	  return "";
	}

      /* A loop to shift by a "large" constant value.
	 If we have shift-by-2 insns, use them.  */
      if (assembler2 != NULL)
	{
	  fprintf (asm_out_file, "\tmov.b	#%d,%sl\n", n / 2,
		   names_big[REGNO (operands[4])]);
	  fprintf (asm_out_file, ".Llt%d:\n", loopend_lab);
	  output_asm_insn (assembler2, operands);
	  output_asm_insn ("add	#0xff,%X4", operands);
	  fprintf (asm_out_file, "\tbne	.Llt%d\n", loopend_lab);
	  if (n % 2)
	    output_asm_insn (assembler, operands);
	  return "";
	}
      else
	{
	  fprintf (asm_out_file, "\tmov.b	#%d,%sl\n", n,
		   names_big[REGNO (operands[4])]);
	  fprintf (asm_out_file, ".Llt%d:\n", loopend_lab);
	  output_asm_insn (assembler, operands);
	  output_asm_insn ("add	#0xff,%X4", operands);
	  fprintf (asm_out_file, "\tbne	.Llt%d\n", loopend_lab);
	  return "";
	}
    }

  fprintf (asm_out_file, ".Llt%d:\n", loopend_lab);
  output_asm_insn (assembler, operands);
  output_asm_insn ("add	#0xff,%X4", operands);
  fprintf (asm_out_file, "\tbne	.Llt%d\n", loopend_lab);
  fprintf (asm_out_file, ".Lle%d:\n", loopend_lab);

  return "";
}

/* Fix the operands of a gen_xxx so that it could become a bit
  operating insn.  */

int
fix_bit_operand (operands, what, type)
     rtx *operands;
     int what;
     enum rtx_code type;
{
  /* The bit_operand predicate accepts any memory during RTL generation, but
     only 'U' memory afterwards, so if this is a MEM operand, we must force
     it to be valid for 'U' by reloading the address.  */

  if (GET_CODE (operands[2]) == CONST_INT)
    {
      if (CONST_OK_FOR_LETTER_P (INTVAL (operands[2]), what))
	{
	  /* Ok to have a memory dest.  */
	  if (GET_CODE (operands[0]) == MEM && !EXTRA_CONSTRAINT (operands[0], 'U'))
	    {
	      rtx mem = gen_rtx_MEM (GET_MODE (operands[0]),
				     copy_to_mode_reg (Pmode,
						       XEXP (operands[0], 0)));
	      RTX_UNCHANGING_P (mem) = RTX_UNCHANGING_P (operands[0]);
	      MEM_COPY_ATTRIBUTES (mem, operands[0]);
	      operands[0] = mem;
	    }

	  if (GET_CODE (operands[1]) == MEM && !EXTRA_CONSTRAINT (operands[1], 'U'))
	    {
	      rtx mem = gen_rtx_MEM (GET_MODE (operands[1]),
				     copy_to_mode_reg (Pmode,
						       XEXP (operands[1], 0)));
	      RTX_UNCHANGING_P (mem) = RTX_UNCHANGING_P (operands[1]);
	      MEM_COPY_ATTRIBUTES (mem, operands[0]);
	      operands[1] = mem;
	    }
	  return 0;
	}
    }

  /* Dest and src op must be register.  */

  operands[1] = force_reg (QImode, operands[1]);
  {
    rtx res = gen_reg_rtx (QImode);
    emit_insn (gen_rtx_SET (VOIDmode, res,
			    gen_rtx (type, QImode, operands[1], operands[2])));
    emit_insn (gen_rtx_SET (VOIDmode, operands[0], res));
  }
  return 1;
}

/* Return nonzero if FUNC is an interrupt function as specified
   by the "interrupt" attribute.  */

static int
h8300_interrupt_function_p (func)
     tree func;
{
  tree a;

  if (TREE_CODE (func) != FUNCTION_DECL)
    return 0;

  a = lookup_attribute ("interrupt_handler", DECL_MACHINE_ATTRIBUTES (func));
  return a != NULL_TREE;
}

/* Return nonzero if FUNC is an OS_Task function as specified
   by the "OS_Task" attribute.  */

static int
h8300_os_task_function_p (func)
     tree func;
{
  tree a;

  if (TREE_CODE (func) != FUNCTION_DECL)
    return 0;

  a = lookup_attribute ("OS_Task", DECL_MACHINE_ATTRIBUTES (func));
  return a != NULL_TREE;
}

/* Return nonzero if FUNC is a monitor function as specified
   by the "monitor" attribute.  */

static int
h8300_monitor_function_p (func)
     tree func;
{
  tree a;

  if (TREE_CODE (func) != FUNCTION_DECL)
    return 0;

  a = lookup_attribute ("monitor", DECL_MACHINE_ATTRIBUTES (func));
  return a != NULL_TREE;
}

/* Return nonzero if FUNC is a function that should be called
   through the function vector.  */

int
h8300_funcvec_function_p (func)
     tree func;
{
  tree a;

  if (TREE_CODE (func) != FUNCTION_DECL)
    return 0;

  a = lookup_attribute ("function_vector", DECL_MACHINE_ATTRIBUTES (func));
  return a != NULL_TREE;
}

/* Return nonzero if DECL is a variable that's in the eight bit
   data area.  */

int
h8300_eightbit_data_p (decl)
     tree decl;
{
  tree a;

  if (TREE_CODE (decl) != VAR_DECL)
    return 0;

  a = lookup_attribute ("eightbit_data", DECL_MACHINE_ATTRIBUTES (decl));
  return a != NULL_TREE;
}

/* Return nonzero if DECL is a variable that's in the tiny
   data area.  */

int
h8300_tiny_data_p (decl)
     tree decl;
{
  tree a;

  if (TREE_CODE (decl) != VAR_DECL)
    return 0;

  a = lookup_attribute ("tiny_data", DECL_MACHINE_ATTRIBUTES (decl));
  return a != NULL_TREE;
}

/* Return nonzero if ATTR is a valid attribute for DECL.
   ATTRIBUTES are any existing attributes and ARGS are the arguments
   supplied with ATTR.

   Supported attributes:

   interrupt_handler: output a prologue and epilogue suitable for an
   interrupt handler.

   function_vector: This function should be called through the
   function vector.

   eightbit_data: This variable lives in the 8-bit data area and can
   be referenced with 8-bit absolute memory addresses.

   tiny_data: This variable lives in the tiny data area and can be
   referenced with 16-bit absolute memory references.  */

int
h8300_valid_machine_decl_attribute (decl, attributes, attr, args)
     tree decl;
     tree attributes ATTRIBUTE_UNUSED;
     tree attr;
     tree args;
{
  if (args != NULL_TREE)
    return 0;

  if (is_attribute_p ("interrupt_handler", attr)
      || is_attribute_p ("OS_Task", attr)
      || is_attribute_p ("monitor", attr)
      || is_attribute_p ("function_vector", attr))
    return TREE_CODE (decl) == FUNCTION_DECL;

  if (is_attribute_p ("eightbit_data", attr)
      && (TREE_STATIC (decl) || DECL_EXTERNAL (decl)))
    {
      if (DECL_INITIAL (decl) == NULL_TREE)
	{
	  warning ("Only initialized variables can be placed into the 8-bit area.");
	  return 0;
	}
      DECL_SECTION_NAME (decl) = build_string (7, ".eight");
      return 1;
    }

  if (is_attribute_p ("tiny_data", attr)
      && (TREE_STATIC (decl) || DECL_EXTERNAL (decl)))
    {
      if (DECL_INITIAL (decl) == NULL_TREE)
	{
	  warning ("Only initialized variables can be placed into the 8-bit area.");
	  return 0;
	}
      DECL_SECTION_NAME (decl) = build_string (6, ".tiny");
      return 1;
    }
      
  return 0;
}

extern struct obstack *saveable_obstack;

void
h8300_encode_label (decl)
     tree decl;
{
  const char *str = XSTR (XEXP (DECL_RTL (decl), 0), 0);
  int len = strlen (str);
  char *newstr;

  newstr = obstack_alloc (saveable_obstack, len + 2);

  strcpy (newstr + 1, str);
  *newstr = '&';
  XSTR (XEXP (DECL_RTL (decl), 0), 0) = newstr;
}

const char *
output_simode_bld (bild, log2, operands)
     int bild;
     int log2;
     rtx operands[];
{
  /* Clear the destination register.  */
  if (TARGET_H8300H || TARGET_H8300S)
    output_asm_insn ("sub.l\t%S0,%S0", operands);
  else
    output_asm_insn ("sub.w\t%e0,%e0\n\tsub.w\t%f0,%f0", operands);

  /* Get the bit number we want to load.  */
  if (log2)
    operands[2] = GEN_INT (exact_log2 (INTVAL (operands[2])));

  /* Now output the bit load or bit inverse load, and store it in
     the destination.  */
  if (bild)
    output_asm_insn ("bild\t%Z2,%Y1\n\tbst\t#0,%w0", operands);
  else
    output_asm_insn ("bld\t%Z2,%Y1\n\tbst\t#0,%w0", operands);

  /* All done.  */
  return "";
}

/* Given INSN and its current length LENGTH, return the adjustment
   (in bytes) to correctly compute INSN's length.

   We use this to get the lengths of various memory references correct.  */

int
h8300_adjust_insn_length (insn, length)
     rtx insn;
     int length ATTRIBUTE_UNUSED;
{
  rtx pat;

  /* We must filter these ou before calling get_attr_adjust_length.  */
  if (GET_CODE (PATTERN (insn)) == USE
      || GET_CODE (PATTERN (insn)) == CLOBBER
      || GET_CODE (PATTERN (insn)) == SEQUENCE
      || GET_CODE (PATTERN (insn)) == ADDR_VEC
      || GET_CODE (PATTERN (insn)) == ADDR_DIFF_VEC)
    return 0;

  if (get_attr_adjust_length (insn) == ADJUST_LENGTH_NO)
    return 0;

  pat = PATTERN (insn);

  /* Adjust length for reg->mem and mem->reg copies.  */
  if (GET_CODE (pat) == SET
      && (GET_CODE (SET_SRC (pat)) == MEM
	  || GET_CODE (SET_DEST (pat)) == MEM))
    {
      /* This insn might need a length adjustment.  */
      rtx addr;

      if (GET_CODE (SET_SRC (pat)) == MEM)
	addr = XEXP (SET_SRC (pat), 0);
      else
	addr = XEXP (SET_DEST (pat), 0);

      /* On the H8/300, only one adjustment is necessary; if the
	 address mode is register indirect, then this insn is two
	 bytes shorter than indicated in the machine description.  */
      if (TARGET_H8300 && GET_CODE (addr) == REG)
	return -2;

      /* On the H8/300H and H8/S, register indirect is 6 bytes shorter than
	 indicated in the machine description.  */
      if ((TARGET_H8300H || TARGET_H8300S)
          && GET_CODE (addr) == REG)
	return -6;

      /* On the H8/300H and H8/300S, reg + d, for small displacements is 4
	 bytes shorter than indicated in the machine description.  */
      if ((TARGET_H8300H || TARGET_H8300S)
	  && GET_CODE (addr) == PLUS
	  && GET_CODE (XEXP (addr, 0)) == REG
	  && GET_CODE (XEXP (addr, 1)) == CONST_INT
	  && INTVAL (XEXP (addr, 1)) > -32768
	  && INTVAL (XEXP (addr, 1)) < 32767)
	return -4;

      /* On the H8/300H and H8/300S, abs:16 is two bytes shorter than the
	 more general abs:24.  */
      if ((TARGET_H8300H || TARGET_H8300S)
	  && GET_CODE (addr) == SYMBOL_REF
	  && TINY_DATA_NAME_P (XSTR (addr, 0)))
	return -2;
    }

  /* Loading some constants needs adjustment.  */
  if (GET_CODE (pat) == SET
      && GET_CODE (SET_SRC (pat)) == CONST_INT
      && GET_MODE (SET_DEST (pat)) == SImode
      && INTVAL (SET_SRC (pat)) != 0)
    {
      if (TARGET_H8300
	  && ((INTVAL (SET_SRC (pat)) & 0xffff) == 0
	      || ((INTVAL (SET_SRC (pat)) >> 16) & 0xffff) == 0))
	return -2;

      if (TARGET_H8300H || TARGET_H8300S)
	{
	  int val = INTVAL (SET_SRC (pat));

	  if (val == (val & 0xff)
	      || val == (val & 0xff00))
	    return -6;

	  if (val == -4 || val == -2 || val == -1)
	    return -6;
	}
    }

  /* Shifts need various adjustments.  */
  if (GET_CODE (pat) == PARALLEL
      && GET_CODE (XVECEXP (pat, 0, 0)) == SET
      && (GET_CODE (SET_SRC (XVECEXP (pat, 0, 0))) == ASHIFTRT
          || GET_CODE (SET_SRC (XVECEXP (pat, 0, 0))) == LSHIFTRT
          || GET_CODE (SET_SRC (XVECEXP (pat, 0, 0))) == ASHIFT))
    {
      rtx src = SET_SRC (XVECEXP (pat, 0, 0));
      enum machine_mode mode = GET_MODE (src);
      int shift;

      if (GET_CODE (XEXP (src, 1)) != CONST_INT)
	return 0;

      shift = INTVAL (XEXP (src, 1));
      /* According to ANSI, negative shift is undefined.  It is
         considered to be zero in this case (see function
         emit_a_shift above). */
      if (shift < 0)
	shift = 0;

      /* QImode shifts by small constants take one insn
	 per shift.  So the adjustment is 20 (md length) -
	 # shifts * 2.  */
      if (mode == QImode && shift <= 4)
	return -(20 - shift * 2);

      /* Similarly for HImode and SImode shifts by
	 small constants on the H8/300H and H8/300S.  */
      if ((TARGET_H8300H || TARGET_H8300S)
	  && (mode == HImode || mode == SImode) && shift <= 4)
	return -(20 - shift * 2);

      /* HImode shifts by small constants for the H8/300.  */
      if (mode == HImode && shift <= 4)
	return -(20 - (shift * (GET_CODE (src) == ASHIFT ? 2 : 4)));

      /* SImode shifts by small constants for the H8/300.  */
      if (mode == SImode && shift <= 2)
	return -(20 - (shift * (GET_CODE (src) == ASHIFT ? 6 : 8)));

      /* XXX ??? Could check for more shift/rotate cases here.  */
    }
    
  return 0;
}
