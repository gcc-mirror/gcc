/* Output routines for GCC for Hitachi Super-H
   Copyright (C) 1993 Free Software Foundation, Inc.

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


/* Contributed by Steve Chamberlain (sac@cygnus.com) */

#include <stdio.h>
#include "assert.h"
#include "config.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-flags.h"
#include "tree.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "obstack.h"
#include "expr.h"


static int add_constant ();
static void dump_constants ();

int current_function_anonymous_args;
extern int current_function_pretend_args_size;

/* Global variables for machine-dependent things. */

/* Saved operands from the last compare to use when we generate an scc
  or bcc insn. */

rtx sh_compare_op0;
rtx sh_compare_op1;

/* Provides the class number of the smallest class containing
   reg number */

int regno_reg_class[FIRST_PSEUDO_REGISTER] =
{
  R0_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,
  GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,
  GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,
  GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,
  GENERAL_REGS, PR_REGS, T_REGS, NO_REGS, MAC_REGS,
  MAC_REGS,
};

/* Provide reg_class from a letter such as appears in the machine
   description. */

enum reg_class reg_class_from_letter[] =
{
  /* a */ NO_REGS, /* b */ NO_REGS, /* c */ NO_REGS, /* d */ NO_REGS,
  /* e */ NO_REGS, /* f */ NO_REGS, /* g */ NO_REGS, /* h */ NO_REGS,
  /* i */ NO_REGS, /* j */ NO_REGS, /* k */ NO_REGS, /* l */ PR_REGS,
  /* m */ NO_REGS, /* n */ NO_REGS, /* o */ NO_REGS, /* p */ NO_REGS,
  /* q */ NO_REGS, /* r */ NO_REGS, /* s */ NO_REGS, /* t */ T_REGS,
  /* u */ NO_REGS, /* v */ NO_REGS, /* w */ NO_REGS, /* x */ MAC_REGS,
  /* y */ NO_REGS, /* z */ R0_REGS
};


/* Local label counter, used for constants in the pool and inside
   pattern branches.  */

static int lf;

/* Used to work out sizes of instructions */
static int first_pc;
static int pc;

static int dumpnext;

/* Functions for generating procedure prologue and epilogue code */

/* Adjust the stack and return the number of bytes taken to do it */

static int
output_stack_adjust (file, direction, size)
     FILE *file;
     int direction;
     int size;
{
  int code_size;

  if (size > 127)
    {
      fprintf (file, "\tmov.l	LK%d,r13\n",
	       add_constant (GEN_INT (size * direction), SImode));

      fprintf (file, "\tadd	r13,r15\n");
      code_size += 4;
    }
  else if (size)
    {
      fprintf (file, "\tadd	#%d,r15\n", direction * size);
      code_size += 2;
    }
  return code_size;
}

/* Generate code to push the regs specified in the mask, and return
   the number of bytes the insns take. */

static int
push_regs (f, mask)
     FILE *f;
     int mask;
{
  int i;
  int size = 0;

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      if (mask & (1 << i))
	{
	  fprintf (f, "\tmov.l	r%d,@-r15\n", i);
	  size += 2;
	}
    }
  return size;
}


/* Working out the right code to use for an epilogue can get quite
   hairy, since there are only certain insns which can go in the delay
   slot, and there may or may not be a delay insn provided already.

   We generate a canonical list of the instructions to use to perform
   the exit, massage that and output from that list */


/* The structure of a canonical element. */

typedef struct
{
  enum epi_type
    {
      STACK_ADJUST,		/* add i to stack pointer 	*/
      POP,			/* pop into register i 		*/
      RTS,			/* rts instruction 		*/
      DELAY,			/* delay slot instruction 	*/
      NOP,			/* a nop 			*/
      DELETED,
    } type;
  int i;
}

epilogue_insn;

static epilogue_insn epilogue_vec[20];
static int epilogue_vec_len;

static void
set_epilogue_insn (type, l)
     enum epi_type type;
     int l;
{
  epilogue_vec[epilogue_vec_len].type = type;
  epilogue_vec[epilogue_vec_len].i = l;
  epilogue_vec_len++;
}

/* Delete an insn from the epilogue list. */

static void
delete_epilogue_insn (n)
     int n;
{
  int j;

  for (j = n; j < epilogue_vec_len; j++)
    epilogue_vec[j] = epilogue_vec[j + 1];

  epilogue_vec_len--;
}

/* Run through the epilogue list and optimize it. */

static void
optimize_epilogue_vec ()
{
  int i;

  /* Turn two adds in a row into one add and kill empty adds */
  for (i = 0; i < epilogue_vec_len - 1; i++)
    {
      if (epilogue_vec[i].type == STACK_ADJUST
	  && epilogue_vec[i + 1].type == STACK_ADJUST)
	{
	  epilogue_vec[i].i += epilogue_vec[i + 1].i;
	  delete_epilogue_insn (i + 1);
	}
      if (epilogue_vec[i].type == STACK_ADJUST
	  && epilogue_vec[i].i == 0)
	delete_epilogue_insn (i);
    }

  /* If the instruction after the RTS is a nop, see if it can be
     changed */

  for (i = 1; i < epilogue_vec_len - 1; i++)
    {
      if (epilogue_vec[i].type == RTS
	  && epilogue_vec[i + 1].type == NOP)
	{
	  epilogue_vec[i + 1] = epilogue_vec[i - 1];
	  delete_epilogue_insn (i - 1);
	}
    }

  /* Delete all the instructions after the rts's delay slot */
  for (i = 0; i < epilogue_vec_len; i++)
    {
      if (epilogue_vec[i].type == RTS)
	{
	  int j;

	  for (j = i + 2; j < epilogue_vec_len; j++)
	    epilogue_vec[j].type = DELETED;
	  return;
	}
    }
}

/* Dump out the insns in epilogue vector. */

static void
output_epilogue_vec ()
{
  int i;

  for (i = 0; i < epilogue_vec_len; i++)
    {
      switch (epilogue_vec[i].type)
	{
	case STACK_ADJUST:
	  fprintf (asm_out_file, "\tadd	#%d,r15\n", epilogue_vec[i].i);
	  break;

	case NOP:
	  fprintf (asm_out_file, "\tor	r0,r0\n");
	  break;

	case DELAY:
	  final_scan_insn (XEXP (current_function_epilogue_delay_list, 0),
			   asm_out_file, 1, 0, 1);
	  break;

	case DELETED:
	  fprintf (asm_out_file, "\t!delete_epilogue_insnd\n");
	  break;

	case RTS:
	  fprintf (asm_out_file, "\trts\n");
	  break;

	case POP:
	  fprintf (asm_out_file, "\tmov.l	@r15+,r%d\n",
		   epilogue_vec[i].i);
	  break;
	}
    }
  epilogue_vec_len = 0;
}

/* Number of bytes pushed for anonymous args */

static int extra_push;

/* Work out the registers which need to be saved, both as a mask and a
   count */

int
calc_live_regs (count)
     int *count;
{
  int reg;
  int live_regs_mask = 0;
  *count = 0;

  for (reg = 0; reg < FIRST_PSEUDO_REGISTER; reg++)
    {
      if (regs_ever_live[reg] && !call_used_regs[reg])
	{
	  (*count)++;
	  live_regs_mask |= (1 << reg);
	}
    }
  return live_regs_mask;
}

/* Generate a procedure prologue.  */

void
output_prologue (f, frame_size)
     FILE *f;
     int frame_size;
{
  int live_regs_mask;
  int d;

  pc = 0;

  /* This only happens when an arg has been split, part in
     registers, part in memory.  Allocate the stack space so there is
     somewhere to put the value */

  output_stack_adjust (f, -1, current_function_pretend_args_size);

  live_regs_mask = calc_live_regs (&d);

  extra_push = 0;

  if (current_function_anonymous_args)
    {
      /* Push arg regs as if they'd been provided by caller in stack */
      int i;
      for (i = 0; i < NPARM_REGS; i++)
	{
	  int rn = NPARM_REGS + FIRST_PARM_REG - i - 1;
	  if (i > NPARM_REGS - current_function_args_info)
	    break;
	  fprintf (f, "\tmov.l	r%d,@-r15\n", rn);
	  extra_push += 4;
	  pc += 2;
	}
    }

  if (frame_pointer_needed)
    {
      /* Don't need to push the fp with the rest of the registers. */
      live_regs_mask &= ~(1 << FRAME_POINTER_REGNUM);
      pc += push_regs (f, live_regs_mask);
      if (regs_ever_live[PR_REG])
	{

	  fprintf (f, "\tsts.l	pr,@-r15\n");
	  pc += 2;
	}

      fprintf (f, "\tmov.l	r14,@-r15\n");
      fprintf (f, "\tmov	r15,r14\n");
      pc += 4;
      pc += output_stack_adjust (f, -1, frame_size);
    }
  else
    {
      pc += push_regs (f, live_regs_mask);

      if (regs_ever_live[PR_REG])
	{

	  fprintf (f, "\tsts.l	pr,@-r15\n");
	  pc += 2;
	}
      pc += output_stack_adjust (f, -1, frame_size);
    }
}


/* Generate a procedure epilogue. */

void
output_epilogue (f, frame_size)
     FILE *f;
     int frame_size;
{
  int live_regs_mask = 0;
  int d;
  int i;

  live_regs_mask = calc_live_regs (&d);

  /* Reclaim the room for the automatics. */

  output_stack_adjust (f, 1, frame_size);

  /* Make the frame pointer. */

  if (frame_pointer_needed)
    {
      fprintf (f, "\tmov	r14,r15\n");
      fprintf (f, "\tmov.l	@r15+,r14\n");
      live_regs_mask &= ~(1 << FRAME_POINTER_REGNUM);
    }

  /* Get the PR register if it was clobbered in the function. */

  if (regs_ever_live[PR_REG])
    fprintf (f, "\tlds.l	@r15+,pr\n");

  /* Pop all the registers */
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      int j = (FIRST_PSEUDO_REGISTER - 1) - i;
      if (live_regs_mask & (1 << j))
	{
	  set_epilogue_insn (POP, j);
	}
    }

  /* Need to adjust the stack by some amount of bytes since we've pushed
     some of the args which normally come in registers */

  set_epilogue_insn (STACK_ADJUST, extra_push);

  /* Need to adjust the stack by some amount of bytes if there
     an arg has been split part register and part stack */

  set_epilogue_insn (STACK_ADJUST, current_function_pretend_args_size);

  set_epilogue_insn (RTS, 0);

  /* Got here without dumping a register pop into the delay slot */
  if (current_function_epilogue_delay_list)
    {
      set_epilogue_insn (DELAY, 0);
    }
  set_epilogue_insn (NOP, 0);

  optimize_epilogue_vec ();

  output_epilogue_vec ();

  dump_constants ();
  current_function_anonymous_args = 0;
}

/* Print the operand address in x to the stream */

void
print_operand_address (stream, x)
     FILE *stream;
     rtx x;
{
  switch (GET_CODE (x))
    {
    case REG:
      fprintf (stream, "@%s", reg_names[REGNO (x)]);
      break;

    case PLUS:
      {
	rtx base = XEXP (x, 0);
	rtx index = XEXP (x, 1);

	if (GET_CODE (base) != REG)
	  {
	    /* Ensure that BASE is a register (one of them must be). */
	    rtx temp = base;
	    base = index;
	    index = temp;
	  }

	switch (GET_CODE (index))
	  {
	  case CONST_INT:
	    fprintf (stream, "@(%d,%s)",
		     INTVAL (index),
		     reg_names[REGNO (base)]);
	    break;

	  case REG:
	    fprintf (stream, "@(%s,%s)",
		     reg_names[REGNO (base)],
		     reg_names[REGNO (index)]);
	    break;

	  default:
	    abort ();
	  }
      }

      break;
    case PRE_DEC:
      fprintf (stream, "@-%s", reg_names[REGNO (XEXP (x, 0))]);
      break;

    case POST_INC:
      fprintf (stream, "@%s+", reg_names[REGNO (XEXP (x, 0))]);
      break;

    default:
      output_addr_const (stream, x);
      break;
    }
}

/* Print operand x (an rtx) in assembler syntax to file stream
   according to modifier code.

 '*'  print a local label
 '^'  increment the local label number
 '!'  dump the constant table
 '#'  output a nop if there is nothing to put in the delay slot
 'R'  print the next register or memory location along, ie the lsw in
      a double word value
 'I'  put something into the constant pool and print its label */

void
print_operand (stream, x, code)
     FILE *stream;
     rtx x;
     int code;
{
  switch (code)
    {
    case '*':
      fprintf (stream, "LF%d", lf);
      break;
    case '!':
      dump_constants();
      break;
    case '^':
      lf++;
      break;

    case '#':
      /* Output a nop if there's nothing in the delay slot */
      if (dbr_sequence_length () == 0)
	{
	  fprintf (stream, "\n\tor	r0,r0\t!wasted slot");
	}
      break;

    case 'I':
      fprintf (asm_out_file, "LK%d", add_constant (x, SImode));
      break;

    case 'R':
      /* Next location along in memory or register*/
      switch (GET_CODE (x))
	{
	case REG:
	  fputs (reg_names[REGNO (x) + 1], (stream));
	  break;
	case MEM:
	  print_operand_address (stream,
			       XEXP (adj_offsettable_operand (x, 4), 0), 0);
	  break;
	}
      break;

    default:
      switch (GET_CODE (x))
	{
	case REG:
	  fputs (reg_names[REGNO (x)], (stream));
	  break;
	case MEM:
	  output_address (XEXP (x, 0));
	  break;
	default:
	  fputc ('#', stream);
	  output_addr_const (stream, x);
	  break;

	}
      break;
    }
}



/* Define the offset between two registers, one to be eliminated, and 
   the other its replacement, at the start of a routine.  */

int
initial_elimination_offset (from, to)
{
  int regs_saved;
  int d = calc_live_regs (&regs_saved);
  int total_saved_regs_space = (regs_saved + regs_ever_live[PR_REG]) * 4;
  int total_auto_space = get_frame_size ();


  if (from == ARG_POINTER_REGNUM && to == FRAME_POINTER_REGNUM)
    {
      return total_saved_regs_space;
    }

  if (from == ARG_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    {
      return total_saved_regs_space + total_auto_space;
    }

  if (from == FRAME_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    {
      return total_auto_space;
    }
}

delay_slots_for_epilogue ()
{
  /* We need to find something to fill the epilogue if there won't be
     any instructions to make the stack or pop registers which can be
     moved into the slot */

  int d;
  calc_live_regs (&d);
  return !(get_frame_size () + d);
}


/* Prepare operands for a move define_expand; specifically, one of the
   operands must be in a register */

void
prepare_move_operands (operands, mode)
     rtx operands[];
     enum machine_mode mode;
{
  /* One of the operands has to be a register */
  if ((!register_operand (operands[0], mode)
       && !register_operand (operands[1], mode))
      || GET_CODE(operands[1]) == PLUS)
    {
      /* copy the source to a register */
      operands[1] = copy_to_mode_reg (mode, operands[1]);
    }
}


/* Prepare the operands for an scc instruction; make sure that the
   compare has been done.  */
rtx
prepare_scc_operands (code)
{
  if (GET_CODE(sh_compare_op0) != REG 
      || REGNO(sh_compare_op0) != T_REG)
    {
      /* First need a compare insn */
      emit_insn (gen_rtx (SET, SImode, 
			  gen_rtx (REG, SImode, T_REG),
			  gen_rtx (code, SImode, sh_compare_op0,
				   sh_compare_op1)));
    }
  return gen_rtx(REG, SImode, T_REG);
}

/* Functions to output assembly */

/* Return a sequence of instructions to perform DI move, taking into
   account overlapping source and dest registers */

char *
output_movedouble (operands, mode)
     rtx operands[];
     enum machine_mode mode;
{
  if (register_operand (operands[0], mode)
      && register_operand (operands[1], mode))
    {
      if (REGNO (operands[1]) == MACH_REG)
	return "sts	mach,%0\n\tsts	macl,%R0";
      return "mov	%1,%0\n\tmov	%R1,%R0";
    }

  if (GET_CODE (operands[1]) == CONST_INT)
    {
      if (INTVAL (operands[1]) < 0)
	return "mov	#-1,%0\n\tmov	%1,%R0";
      else
	return "mov	#0,%0\n\tmov	%1,%R0";
    }

  if (GET_CODE (operands[1]) == MEM)
    {
      int idxreg = -1;
      rtx inside = XEXP (operands[1], 0);

      if (GET_CODE (inside) == REG)
	idxreg = REGNO (inside);
      else if (GET_CODE (inside) == PLUS)
	{
	  rtx lhs = XEXP (inside, 0);
	  rtx rhs = XEXP (inside, 1);
	  if (GET_CODE (lhs) == REG)
	    idxreg = REGNO (lhs);
	  else if (GET_CODE (rhs) == REG)
	    idxreg = REGNO (rhs);
	  else
	    abort ();
	}
      else
	abort ();

      if (REGNO (operands[0]) != idxreg)
	{
	  /* The dest register is mentioned in the addressing mode,
	     so print them the other way around */
	  return "mov.l	%1,%0\n\tmov.l	%R1,%R0 ! one way";
	}
      return "mov.l	%R1,%R0\n\tmov.l	%1,%0 ! other way";
    }

  return "mov.l	%R1,%R0\n\tmov.l	%1,%0";
}

/* Emit assembly to shift reg by k bits */

char *
output_shift (string, reg, k)
     char *string;
     rtx reg;
     rtx k;
{
  int s = INTVAL (k);
  while (s)
    {
      char *out;
      int d;

      if (s >= 16)
	{
	  d = 16;
	  out = "16";
	}
      else if (s >= 8)
	{
	  d = 8;
	  out = "8";
	}
      else if (s >= 2)
	{
	  d = 2;
	  out = "2";
	}
      else
	{
	  d = 1;
	  out = "";
	}
      fprintf (asm_out_file, "\t%s%s\tr%d\n", string, out, REGNO (reg));
      s -= d;
    }
  return "";
}

/* Return the text of the branch instruction which matches its length
   attribute.  */

char *
output_branch (logic, insn)
     int logic;
     rtx *insn;
{
  extern rtx recog_operand[];
  int label = lf++;
  
  switch (get_attr_length (insn))
    {
    case 2:
      /* Simple branch in range -200..+200 bytes */
      return logic ? "bt	%l0" : "bf	%l0";

    case 6:
      /* Branch in range -4000..+4000 bytes */
      fprintf (asm_out_file, "\tb%c\tLF%d\n", logic ? 'f' : 't', label);
      output_asm_insn ("bra	%l0	! 12 bit cond ", recog_operand);
      fprintf (asm_out_file, "\tor	r0,r0\n");
      fprintf (asm_out_file, "LF%d:\n", label);
      lf++;
      return "";

    case 8:
      /* Branches a long way away */
	
      fprintf (asm_out_file, "\tb%c\tLF%d\n", logic ? 'f' : 't', label);
      output_asm_insn ("mov.l	%I0,r13", recog_operand);
      fprintf (asm_out_file, "\tjmp	@r13	! 32 cond \n");
      fprintf (asm_out_file, "\tor	r0,r0\n");
      fprintf (asm_out_file, "LF%d:\n", label);
      return "";
    }
  return "bad";

}

/* Predicates used by the templates */

/* Nonzero if OP is a valid source operand for an arithmetic insn.  */

int
arith_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (register_operand (op, mode))
    return 1;

  if (GET_CODE (op) == CONST_INT)
    {
      if (CONST_OK_FOR_I (INTVAL (op)))
	return 1;
    }
  return 0;
}


/* Nonzero if OP is a valid source operand for a logical operation */

int
logical_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (register_operand (op, mode))
    return 1;

  if (GET_CODE (op) == CONST_INT)
    {
      if (CONST_OK_FOR_L (INTVAL (op)))
	return 1;
    }
  return 0;
}

/* Nonzero if p is a valid shift operand for lshr and ashl */

int
ok_shift_value (p)
     rtx p;
{
  if (GET_CODE (p) == CONST_INT)
    {
      switch (INTVAL (p))
	{
	case 1:
	case 2:
	case 8:
	case 16:
	  return 1;
	default:
	  if (TARGET_FASTCODE)
	    return 1;
	}
    }
  return 0;
}

/* Nonzero if the arg is an immediate which has to be loaded from
   memory */

int
hard_immediate_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (immediate_operand (op, mode))
    {
      if (GET_CODE (op) == CONST_INT
	  && INTVAL (op) >= -128 && INTVAL (op) < 127)
	return 0;
      return 1;
    }
  return 0;
}

/* The SH cannot load a large constant into a register, constants have to
   come from a pc relative load.  The reference of a pc relative load
   instruction must be less than 1k infront of the instruction.  This
   means that we often have to dump a constant inside a function, and
   generate code to branch around it.

   It is important to minimize this, since the branches will slow things
   down and make things bigger.

  Worst case code looks like:

      mov.l L1,rn
      bra   L2
      nop
      align
L1:   .long value
L2:
      ..

      mov.l L3,rn
      bra   L4
      nop
      align
L3:   .long value
L4:
      ..

   During shorten_branches we notice the instructions which can have a
   constant table in them, if we see two that are close enough
   together, we move the constants from the first table to the second
   table and continue.  This process can happen again and again, and
   in the best case, moves the constant table outside of the function.

   In the above example, we can tell that L3 is within 1k of L1, so
   the first move can be shrunk from the 3 insn+constant sequence into
   just 1 insn, and the constant moved to L3 to make:

   mov.l	L1,rn
   ..
   mov.l	L3,rn
   bra		L4
   nop
   align
L3:.long value
L4:.long value

   Then the second move becomes the target for the shortening process.

   We keep a simple list of all the constants accumulated in the
   current pool so there are no duplicates in a single table, but
   they are not factored into the size estimates.

*/

typedef struct
{
  rtx value;
  int number;
  enum machine_mode mode;
} pool_node;

/* The maximum number of constants that can fit into one pool, since
   the pc relative range is 0...1020 bytes and constants are at least 4
   bytes long */

#define MAX_POOL_SIZE (1020/4)
static pool_node pool_vector[MAX_POOL_SIZE];
static int pool_size;


/* Add a constant to the pool and return its label number.  */

static int
add_constant (x, mode)
     rtx x;
     enum machine_mode mode;
{
  int i;

  /* Start the countdown on the first constant */

  if (!pool_size)
    {
      first_pc = pc;
    }

  /* First see if we've already got it */

  for (i = 0; i < pool_size; i++)
    {

      if (x->code == pool_vector[i].value->code
	  && mode == pool_vector[i].mode)
	{
	  if (x->code == CODE_LABEL)
	    {
	      if (XINT (x, 3) != XINT (pool_vector[i].value, 3))
		continue;
	    }
	}

      if (rtx_equal_p (x, pool_vector[i].value))
	return pool_vector[i].number;
    }
  

  pool_vector[pool_size].value = x;
  pool_vector[pool_size].mode = mode;
  pool_vector[pool_size].number = lf;
  pool_size++;

  return lf++;
}

/* Nonzero if the insn could take a constant table.  */

static int
has_constant_table (insn)
     rtx insn;
{
  rtx body;

  if (GET_CODE (insn) == NOTE
      || GET_CODE (insn) == BARRIER
      || GET_CODE (insn) == CODE_LABEL)
    return 0;

  body = PATTERN (insn);
  if (GET_CODE (body) == SEQUENCE)
    return 0;
  if (GET_CODE (body) == ADDR_VEC)
    return 0;
  if (GET_CODE (body) == USE)
    return 0;
  if (GET_CODE (body) == CLOBBER)
    return 0;
  if (get_attr_constneed (insn) == CONSTNEED_YES)
    return 1;

  if (GET_CODE (body) == UNSPEC_VOLATILE)
    {
      return INTVAL (XVECEXP (body, 0, 0)) == 1;
    }
  return 0;
}

/*  Adjust the length of an instruction.

    We'll look at the previous instruction which holds a constant
    table and see if we can move the table to here instead. */

int target_insn_uid;
int target_insn_smallest_size;

int target_pc;
int target_insn_range;
int current_pc;
int table_size;

void
adjust_insn_length (insn, insn_lengths)
     rtx insn;
     short *insn_lengths;
{
  int uid = INSN_UID (insn);

  current_pc += insn_lengths[uid];

  if (has_constant_table (insn)) 
    {
      if (current_pc >= target_insn_range)
	{
	  /* This instruction is further away from the referencing
	     instruction than it can reach, so we'll stop accumulating
	     from that one and start fresh. */
	  target_pc = current_pc;
	  target_insn_range = current_pc + 1000;
	}
      else
	{
	  /* This instruction is within the reach of the target,
	     remove the constant table from the target by adjusting
	     downwards, and increase the size of this one to
	     compensate.  */


	  /* Add the stuff from this insn to what will go in the
	     growing table. */

	  table_size += get_attr_constantsize (insn);

	  /* The target shinks to its smallest natural size */
	  insn_lengths[target_insn_uid] = target_insn_smallest_size;

	  /* The current insn grows to be its larger size plust the
	     table size. */

	  insn_lengths[uid] = get_attr_largestsize (insn) + table_size;

	}
      /* Current insn becomes the target.  */
      target_insn_uid = uid;
      target_insn_smallest_size = get_attr_smallestsize (insn);

    }

}


/* Dump out the pending constant pool.  */

static void
dump_constants ()
{
  int i;
  for (i = 0; i < pool_size; i++)
    {
      pool_node *p = pool_vector + i;
      fprintf (asm_out_file, "\n\t! constants - waited %d\n", pc - first_pc);
      fprintf (asm_out_file, "\t.align\t2\n");
      fprintf (asm_out_file, "LK%d:", p->number);
      switch (GET_MODE_CLASS (p->mode))
	{
	case MODE_INT:
	case MODE_PARTIAL_INT:
	  assemble_integer (p->value, GET_MODE_SIZE (p->mode), 1);
	  break;
	case MODE_FLOAT:
	  {
	    union real_extract u;
	    bcopy (&CONST_DOUBLE_LOW (p->value), &u, sizeof u);
	    assemble_real (u.d, p->mode);
	  }
	}

      fprintf (asm_out_file, "\n");
    }
  pool_size = 0;
  current_pc = 0;
  target_insn_range = 0;
}


/* Emit the text to load a value from a constant table.  */

char *
output_movepcrel (insn, operands, mode)
     rtx insn;
     rtx operands[];
     enum machine_mode mode;
{
  int len = GET_MODE_SIZE (mode);
  int rn = REGNO (operands[0]);

  fprintf (asm_out_file, "\tmov.l	LK%d,r%d\n",
	   add_constant (operands[1], mode), rn);

  if (GET_MODE_SIZE(mode) > 4) 
    {
      fprintf (asm_out_file,
	       "\tmov.l	LK%d+4,r%d\n",
	       add_constant (operands[1], mode),
	       rn + 1);

    } 
  /* If this instruction is as small as it can be, there can be no 
     constant table attached to it.  */
  if (get_attr_length (insn) !=  get_attr_smallestsize (insn))
    {
      /* This needs a constant table */
      fprintf (asm_out_file, "\t!constant table start\n");
      fprintf (asm_out_file, "\tbra	LF%d\n", lf);
      fprintf (asm_out_file, "\tor	r0,r0 ! wasted slot\n");
      dump_constants ();
      fprintf (asm_out_file, "LF%d:\n", lf++);
      fprintf (asm_out_file, "\t!constant table end\n");
    }
  return "";
}


/* Dump out interesting debug info */

void
final_prescan_insn (insn, opvec, noperands)
     rtx insn;
     rtx *opvec;
     int noperands;
{
  register rtx body = PATTERN (insn);

  if (target_flags & ISIZE_BIT)
    {
      extern int *insn_addresses;

      fprintf (asm_out_file, "\n!%04x*\n",
	       insn_addresses[INSN_UID (insn)] + 0x10);

      fprintf (asm_out_file, "\n!%04x %d %04x len=%d\n",
	       pc, pool_size, first_pc, get_attr_length (insn));

      if (TARGET_DUMP_RTL)
	print_rtl (asm_out_file, body);

      pc += get_attr_length (insn);
    }
}

