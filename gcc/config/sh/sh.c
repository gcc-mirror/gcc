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
int dump_constants ();

int current_function_anonymous_args;
extern int current_function_pretend_args_size;
extern char *version_string;
extern int flag_traditional;


enum attr_cpu sh_cpu;		/* target cpu */

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

static int lf = 100;

/* Used to work out sizes of instructions */
static int first_pc;
static int pc;
#define MAYBE_DUMP_LEVEL 900
#define MUST_DUMP_LEVEL 1000
static int dumpnext;


void
push (rn)
{
  emit_insn (gen_push (gen_rtx (REG, SImode, rn)));
}

void
pop (rn)
{
  emit_insn (gen_pop (gen_rtx (REG, SImode, rn)));
}


/* Adjust the stack and return the number of bytes taken to do it */

static void
output_stack_adjust (direction, size)
     int direction;
     int size;
{
  if (size)
    {
      rtx val = GEN_INT (size);
      rtx insn;

      if (size > 120)
	{
	  rtx nval = gen_rtx (REG, SImode, 13);
	  emit_insn (gen_movsi (nval, val));
	  val = nval;
	}

      if (direction > 0)
	insn = gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx, val);
      else
	insn = gen_subsi3 (stack_pointer_rtx, stack_pointer_rtx, val);

      emit_insn (insn);
    }
}



/* Generate code to push the regs specified in the mask, and return
   the number of bytes the insns take. */

static void
push_regs (mask)
     int mask;
{
  int i;
  int size = 0;

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      if (mask & (1 << i))
	{
	  push (i);
	}
    }
}


/*
  Print an instruction which would have gone into a delay slot
  after an instructiuon, but couldn't because the instruction expanded
  into a sequence where putting the slot insn at the end wouldn't work.
  */

void
print_slot (insn)
     rtx insn;
{
  final_scan_insn (XVECEXP (insn, 0, 1), asm_out_file, optimize, 0, 1);

  INSN_DELETED_P (XVECEXP (insn, 0, 1)) = 1;
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




static int
need_slot (insn)
     rtx insn;
{
  return (insn && !INSN_ANNULLED_BRANCH_P (XVECEXP (insn, 0, 0)));
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
	    fprintf (stream, "@(r0,%s)",
		     reg_names[MAX (REGNO (base), REGNO (index))]);

	    break;

	  default:
	    debug_rtx (x);

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

   '.'  print a .s if insn needs delay slot
   '*'  print a local label
   '^'  increment the local label number
   '!'  dump the constant table
   '#'  output a nop if there is nothing to put in the delay slot
   'R'  print the next register or memory location along, ie the lsw in
   a double word value
   'O'  print a constant without the #
   'M'  print a constant as its negative
   'I'  put something into the constant pool and print its label */

void
print_operand (stream, x, code)
     FILE *stream;
     rtx x;
     int code;
{
  switch (code)
    {


    case '.':
      if (need_slot (final_sequence))
	fprintf (stream, ".s");
      break;
    case '*':
      fprintf (stream, "LF%d", lf);
      break;
    case '!':
      dump_constants (0);
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
    case 'O':
      fprintf (asm_out_file, "%d", INTVAL (x));
      break;

    case 'I':
      fprintf (asm_out_file, "LK%d", add_constant (x, SImode));
      break;

    case 'M':
      fprintf (asm_out_file, "#%d", -INTVAL (x));
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
  int total_saved_regs_space = (regs_saved) * 4;
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


/* Prepare operands for a move define_expand; specifically, one of the
   operands must be in a register.  Take this chance to remove
   addressing modes which can't be coped with very well. */

int
prepare_move_operands (operands, mode)
     rtx operands[];
     enum machine_mode mode;
{
  /* One of the operands has to be a register */
  if ((!register_operand (operands[0], mode)
       && !register_operand (operands[1], mode))
      || GET_CODE (operands[1]) == PLUS)
    {
      /* copy the source to a register */
      operands[1] = copy_to_mode_reg (mode, operands[1]);
    }

  /* If we've got a negative index, break it down */

  if (GET_CODE (operands[0]) == MEM && !reload_in_progress)
    {

      rtx inside = XEXP (operands[0], 0);
      if (GET_CODE (inside) == PLUS)
	{
	  rtx inside1 = XEXP (inside, 1);
	  if (GET_CODE (inside1) == CONST_INT
	      && INTVAL (inside1) < 0)
	    {
	      /* Catch this now and break it into bits, it will only cause
		 problems later */

	      rtx sub = copy_to_mode_reg (SImode, inside);
	      XEXP (operands[0], 0) = sub;
	    }
	}
    }
  return 0;
}


/* Prepare the operands for an scc instruction; make sure that the
   compare has been done.  */
rtx
prepare_scc_operands (code)
{
  if (GET_CODE (sh_compare_op0) != REG
      || REGNO (sh_compare_op0) != T_REG)
    {
      /* First need a compare insn */
      emit_insn (gen_rtx (SET, SImode,
			  gen_rtx (REG, SImode, T_REG),
			  gen_rtx (code, SImode, sh_compare_op0,
				   sh_compare_op1)));
    }
  return gen_rtx (REG, SImode, T_REG);
}


/* Functions to output assembly */

/* Return a sequence of instructions to perform DI or DF move.

   Since the SH cannot move a DI or DF in one instruction, we have
   to take care when we see overlapping source and dest registers.

 */
char *
output_movedouble (operands, mode)
     rtx operands[];
     enum machine_mode mode;
{
  rtx dst = operands[0];
  rtx src = operands[1];
  int lowfirst;

  if (register_operand (dst, mode)
      && register_operand (src, mode))
    {
      if (REGNO (src) == MACH_REG)
	return "sts	mach,%0\n\tsts	macl,%R0";

      /*
	when mov.d r1,r2 do r2->r3 then r1->r2
	when mov.d r1,r0 do r1->r0 then r2->r1
	*/

      if (REGNO (src) + 1 == REGNO (dst))
	return "mov	%1,%0\n\tmov	%R1,%R0 ! cr";
      else
	return "mov	%R1,%R0\n\tmov	%1,%0 ";

    }
  else if (GET_CODE (src) == CONST_INT)
    {
      if (INTVAL (src) < 0)
	return "mov	#-1,%0\n\tmov	%1,%R0";
      else
	return "mov	#0,%0\n\tmov	%1,%R0";
    }

  else if (GET_CODE (src) == MEM)
    {
      int ptrreg1 = -1;
      int ptrreg2 = -1;
      int dreg = REGNO (dst);
      rtx inside = XEXP (src, 0);

      if (GET_CODE (inside) == REG)
	{
	  ptrreg1 = REGNO (inside);
	}
      else if (GET_CODE (inside) == PLUS)
	{
	  rtx lhs = XEXP (inside, 0);
	  rtx rhs = XEXP (inside, 1);
	  if (GET_CODE (lhs) == REG)
	    ptrreg1 = REGNO (lhs);
	  if (GET_CODE (rhs) == REG)
	    ptrreg2 = REGNO (rhs);
	}
      else
	abort ();


      if ((ptrreg1 >= 0 && ptrreg2 >= 0)
	  && (dreg == ptrreg1
	      || dreg == ptrreg2
	      || dreg + 1 == ptrreg1
	      || dreg + 1 == ptrreg2))
	{
	  /* This move clobbers both index registers,
	     calculate the sum in one register.  */
	  fprintf (asm_out_file, "	add	%s,%s ! special fix\n",
		   reg_names[ptrreg2], reg_names[ptrreg1]);

	  if (dreg == ptrreg1)
	    {
	      /* Copy into dreg+1 first.  */
	      fprintf (asm_out_file, "	mov.l	@(4,%s),%s\n",
		       reg_names[ptrreg1],
		       reg_names[dreg + 1]);

	      fprintf (asm_out_file, "	mov.l	@(%s),%s\n",
		       reg_names[ptrreg1],
		       reg_names[dreg]);
	    }
	  else
	    {
	      /* Copy into dreg first. */
	      fprintf (asm_out_file, "	mov.l	@(%s),%s\n",
		       reg_names[ptrreg1],
		       reg_names[dreg]);

	      fprintf (asm_out_file, "	mov.l	@(4,%s),%s\n",
		       reg_names[ptrreg1],
		       reg_names[dreg + 1]);

	    }
	  warning ("generated complex amode");
	  return "";
	}

      /* Work out the safe way to copy */
      if (dreg == ptrreg1)
	{
	  /* Copy into the second half first */
	  return "mov.l	%R1,%R0\n\tmov.l	%1,%0 ! cr";
	}
    }

  return "mov.l	%1,%0\n\tmov.l	%R1,%R0";
}

/* Emit assembly to shift reg by k bits */

char *
output_shift (string, reg, k, code)
     char *string;
     rtx reg;
     rtx k;
     int code;

{
  int s = INTVAL (k);

  if (code == ASHIFT && s == 31)
    {
      /* Shift left by 31 moving into the t bit, clearing and rotating the other way */

      fprintf (asm_out_file, "\trotr	r%d\n", REGNO (reg));
      fprintf (asm_out_file, "\tmov	#0,r%d\n", REGNO (reg));
      fprintf (asm_out_file, "\trotcr	r%d\n", REGNO (reg));
      s = 0;
    }

  if (code == LSHIFTRT && s == 31)
    {
      fprintf (asm_out_file, "\trotl	r%d\n", REGNO (reg));
      fprintf (asm_out_file, "\tmov	#0,r%d\n", REGNO (reg));
      fprintf (asm_out_file, "\trotcl	r%d\n", REGNO (reg));
      s = 0;
    }

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
   attribute.

   This gets tricky if we have an insn in the delay slot of a branch
   and the branch needs more than 1 insn to complete.*/



char *
output_branch (logic, insn)
     int logic;
     rtx insn;
{
  extern rtx recog_operand[];
  int label = lf++;
  int rn = -1;
  int need_save;

  switch (get_attr_length (insn))
    {
    case 2:
      /* Simple branch in range -200..+200 bytes */
      return logic ? "bt%.	%l0" : "bf%.	%l0";

    case 6:
      /* Branch in range -4000..+4000 bytes */
      {
	rtx oldop = recog_operand[0];


	if (need_slot (final_sequence))
	  {
	    fprintf (asm_out_file, "\tb%c.s\tLF%d\n", logic ? 'f' : 't',
		     label);

	    print_slot (final_sequence);
	  }

	else
	  {
	    fprintf (asm_out_file, "\tb%c\tLF%d\n", logic ? 'f' : 't',
		     label);
	  }
	recog_operand[0] = oldop;

	output_asm_insn ("bra	%l0	! 12 bit cond ", recog_operand);
	fprintf (asm_out_file, "\tor	r0,r0\n");
	label = dump_constants (label);
	fprintf (asm_out_file, "LF%d:\n", label);
      }

      return "";

    case 8:
      /* Branches a long way away */
      {

	rtx oldop = recog_operand[0];

	if (need_slot (final_sequence))
	  {
	    fprintf (asm_out_file, "\tb%c.s\tLF%d\n", logic ? 'f' : 't', label);
	    print_slot (final_sequence);

	  }
	else
	  {
	    fprintf (asm_out_file, "\tb%c\tLF%d\n", logic ? 'f' : 't', label);
	  }

	recog_operand[0] = oldop;

	/* We use r13 as a scratch */
	need_save = 0;
	rn = 13;

	if (need_save)
	  fprintf (asm_out_file, "\tpush  r%d\n", rn);
	fprintf (asm_out_file, "\tmov.l	LK%d,r%d\n", add_constant (oldop, SImode), rn);
	fprintf (asm_out_file, "\tjmp	@r%d	! 32 cond \n", rn);
	if (need_save)
	  fprintf (asm_out_file, "\tpop  r%d\n", rn);
	else
	  fprintf (asm_out_file, "\tor	r0,r0\n");
	fprintf (asm_out_file, "LF%d:\n", label);
	return "";
      }
    }
  return "bad";
}


/* Predicates used by the templates */

/* Non zero if op is an immediate ok for a byte index */

int 
byte_index_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_INT
	  && INTVAL (op) >= 0 && INTVAL (op) <= 15);
}

/* Non zero if OP is a pop operand */

int
pop_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) != MEM)
    return 0;

  if (GET_MODE (op) != mode)
    return 0;

  op = XEXP (op, 0);

  if (GET_CODE (op) != POST_INC)
    return 0;

  return XEXP (op, 0) == stack_pointer_rtx;
}

/* Non zero if OP is an immediate which can be made from two insns. */

int
painful_immediate_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == CONST_INT)
    {
      int i = INTVAL (op);

      if (i > 127 && i < 255)
	return 1;		/* two adds */
    }
  return 0;
}


/* Non zero if OP can be source of a simple move operation. */

int
general_movsrc_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == REG
      || GET_CODE (op) == SUBREG
      || (GET_CODE (op) == CONST_INT &&
	  CONST_OK_FOR_I (INTVAL (op)))
      || GET_CODE (op) == MEM)
    return general_operand (op, mode);
  return 0;
}



/* Nonzero if OP is a normal arithmetic register. */

int
arith_reg_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (register_operand (op, mode))
    {
      if (GET_CODE (op) == REG)
	return REGNO (op) != T_REG;
      return 1;
    }
  return 0;
}


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
	    return INTVAL (p) >= 0;
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
int pool_bytes;

int last_uid;
int last_pc;

void
adjust_insn_length (insn, insn_lengths)
     rtx insn;
     short *insn_lengths;
{
  int uid = INSN_UID (insn);
  rtx body = PATTERN (insn);

  current_pc += insn_lengths[uid];


  if (GET_CODE (body) == SEQUENCE)
    {
      int i;

      for (i = 0; i < XVECLEN (body, 0); i++)
	{
	  adjust_insn_length (XVECEXP (body, 0, i), insn_lengths);
	}
    }
  else
    {
      if (has_constant_table (insn))
	{
	  if (current_pc >= target_insn_range)
	    {
	      /* This instruction is further away from the referencing
	       instruction than it can reach, so we'll stop accumulating
	       from that one and start fresh. */
	      target_pc = current_pc;
	      target_insn_range = current_pc + MAYBE_DUMP_LEVEL;
	    }
	  else
	    {
	      /* This instruction is within the reach of the target,
	       remove the constant table from the target by adjusting
	       downwards, and increase the size of this one to
	       compensate.  */


	      /* Add the stuff from this insn to what will go in the
	       growing table. */

	      pool_bytes += get_attr_constantsize (insn);

	      /* The target shinks to its smallest natural size */
	      insn_lengths[target_insn_uid] = target_insn_smallest_size;

	      /* The current insn grows to be its larger size plust the
	       table size. */

	      insn_lengths[uid] = get_attr_largestsize (insn) + pool_bytes;

	    }
	  /* Current insn becomes the target.  */
	  target_insn_uid = uid;
	  target_insn_smallest_size = get_attr_smallestsize (insn);

	}
    }
}



/* Dump out the pending constant pool.
   If label provided then insert an branch in the middle of the table
   */

int
dump_constants (label)
{
  int i;
  int rlabel = label;
  int size = 0;

  if (pool_size)
    {
      fprintf (asm_out_file, "\n\t! constants - waited %d\n", pc - first_pc);
      fprintf (asm_out_file, "\t.align\t2\n");

      for (i = 0; i < pool_size; i++)
	{
	  pool_node *p = pool_vector + i;

	  fprintf (asm_out_file, "LK%d:", p->number);
	  size += GET_MODE_SIZE (p->mode);

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

	  /* After 200 bytes of table, stick in another branch */
	  if (label && size > 200)
	    {
	      rlabel = lf++;
	      fprintf (asm_out_file, "LF%d:\tbra	LF%d\n", label, rlabel);
	      fprintf (asm_out_file, "\tor	r0,r0\n");
	      label = 0;
	    }

	}
    }

  pool_size = 0;
  current_pc = 0;
  pc = 0;
  pool_bytes = 0;

  target_insn_range = 0;
  return rlabel;

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

  if (GET_MODE_SIZE (mode) > 4)
    {
      fprintf (asm_out_file,
	       "\tmov.l	LK%d+4,r%d\n",
	       add_constant (operands[1], mode),
	       rn + 1);

    }

  /* This may have been the last move in the function, so nothing
     took its constant table, we may be able to move it past the end
     of the function (after the rts) if we are careful */

  if (target_insn_uid == INSN_UID (insn)
      && current_pc < target_insn_range)
    return "";


  /* If this instruction is as small as it can be, there can be no
     constant table attached to it.  */
  if (get_attr_length (insn) != get_attr_smallestsize (insn))
    {
      /* This needs a constant table */
      fprintf (asm_out_file, "\t!constant table start\n");
      fprintf (asm_out_file, "\tbra	LF%d\n", lf);
      fprintf (asm_out_file, "\tor	r0,r0 ! wasted slot\n");
      dump_constants (0);
      fprintf (asm_out_file, "LF%d:\n", lf++);
      fprintf (asm_out_file, "\t!constant table end\n");
    }
  return "";
}


/* Dump out interesting debug info */

rtx
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


    }

  pc += get_attr_length (insn);
  if (pool_size && pc - first_pc > MUST_DUMP_LEVEL)
    {
      /* For some reason we have not dumped out a constant table, and
	 we have emitted a lot of code.  This can happen if the think
	 which wants the table is a long conditional branch (which has no
	 room for a constant table), and there has not been a move
	 constant anywhere. */
      int label = lf++;
      fprintf (asm_out_file, "\t!forced constant table\n");
      fprintf (asm_out_file, "\tbra	LF%d\n", label);
      fprintf (asm_out_file, "\tor	r0,r0 ! wasted slot\n");
      label = dump_constants (label);
      fprintf (asm_out_file, "LF%d:\n", label);
      fprintf (asm_out_file, "\t!constant table end\n");
    }
}



/* Block move stuff stolen from m88k*/

/* Emit code to perform a block move.  Choose the best method.

   OPERANDS[0] is the destination.
   OPERANDS[1] is the source.
   OPERANDS[2] is the size.
   OPERANDS[3] is the alignment safe to use.  */

/* Emit code to perform a block move with an offset sequence of ld/st
   instructions (..., ld 0, st 1, ld 1, st 0, ...).  SIZE and ALIGN are
   known constants.  DEST and SRC are registers.  OFFSET is the known
   starting point for the output pattern.  */

static enum machine_mode mode_from_align[] =
{VOIDmode, QImode, HImode, VOIDmode, SImode,
 VOIDmode, VOIDmode, VOIDmode, DImode};
static void

block_move_sequence (dest, dest_mem, src, src_mem, size, align, offset)
     rtx dest, dest_mem;
     rtx src, src_mem;
     int size;
     int align;
     int offset;
{
  rtx temp[2];
  enum machine_mode mode[2];
  int amount[2];
  int active[2];
  int phase = 0;
  int next;
  int offset_ld = offset;
  int offset_st = offset;

  active[0] = active[1] = FALSE;

  /* Establish parameters for the first load and for the second load if
     it is known to be the same mode as the first.  */
  amount[0] = amount[1] = align;


  mode[0] = mode_from_align[align];

  temp[0] = gen_reg_rtx (mode[0]);
  if (size >= 2 * align)
    {
      mode[1] = mode[0];
      temp[1] = gen_reg_rtx (mode[1]);
    }

  do
    {
      rtx srcp, dstp;
      next = phase;
      phase = !phase;

      if (size > 0)
	{
	  /* Change modes as the sequence tails off.  */
	  if (size < amount[next])
	    {
	      amount[next] = (size >= 4 ? 4 : (size >= 2 ? 2 : 1));
	      mode[next] = mode_from_align[amount[next]];
	      temp[next] = gen_reg_rtx (mode[next]);
	    }
	  size -= amount[next];
	  srcp = gen_rtx (MEM,
			  MEM_IN_STRUCT_P (src_mem) ? mode[next] : BLKmode,
			  gen_rtx (PLUS, Pmode, src,
				   gen_rtx (CONST_INT, SImode, offset_ld)));
	  RTX_UNCHANGING_P (srcp) = RTX_UNCHANGING_P (src_mem);
	  MEM_VOLATILE_P (srcp) = MEM_VOLATILE_P (src_mem);
	  MEM_IN_STRUCT_P (srcp) = 1;
	  emit_insn (gen_rtx (SET, VOIDmode, temp[next], srcp));
	  offset_ld += amount[next];
	  active[next] = TRUE;
	}

      if (active[phase])
	{
	  active[phase] = FALSE;
	  dstp = gen_rtx (MEM,
			  MEM_IN_STRUCT_P (dest_mem) ? mode[phase] : BLKmode,
			  gen_rtx (PLUS, Pmode, dest,
				   gen_rtx (CONST_INT, SImode, offset_st)));
	  RTX_UNCHANGING_P (dstp) = RTX_UNCHANGING_P (dest_mem);
	  MEM_VOLATILE_P (dstp) = MEM_VOLATILE_P (dest_mem);
	  MEM_IN_STRUCT_P (dstp) = 1;
	  emit_insn (gen_rtx (SET, VOIDmode, dstp, temp[phase]));
	  offset_st += amount[phase];
	}
    }
  while (active[next]);
}

void
expand_block_move (dest_mem, src_mem, operands)
     rtx dest_mem;
     rtx src_mem;
     rtx *operands;
{
  int align = INTVAL (operands[3]);
  int constp = (GET_CODE (operands[2]) == CONST_INT);
  int bytes = (constp ? INTVAL (operands[2]) : 0);

#if 0
  if (constp && bytes <= 0)
    return;

  if (align > 4)
    align = 4;

  if (constp && bytes <= 3 * align)
    block_move_sequence (operands[0], dest_mem, operands[1], src_mem,
			 bytes, align, 0);

#if 0
  else if (constp && bytes <= best_from_align[target][align])
    block_move_no_loop (operands[0], dest_mem, operands[1], src_mem,
			bytes, align);

  else if (constp && align == 4 && TARGET_88100)
    block_move_loop (operands[0], dest_mem, operands[1], src_mem,
		     bytes, align);
#endif
  else
#endif
    {
      emit_library_call (gen_rtx (SYMBOL_REF, Pmode, "memcpy"), 0,
			 VOIDmode, 3,
			 operands[0], Pmode,
			 operands[1], Pmode,
			 operands[2], SImode);
    }
}


override_options ()
{
  sh_cpu = CPU_SH0;
  if (TARGET_SH1)
    sh_cpu = CPU_SH1;
  if (TARGET_SH2)
    sh_cpu = CPU_SH2;
  if (TARGET_SH3)
    sh_cpu = CPU_SH3;
}


/* Stuff taken from m88k.c */

/* Output to FILE the start of the assembler file.  */

struct option
{
  char *string;
  int *variable;
  int on_value;
};

static int
output_option (file, sep, type, name, indent, pos, max)
     FILE *file;
     char *sep;
     char *type;
     char *name;
     char *indent;
     int pos;
     int max;
{
  if (strlen (sep) + strlen (type) + strlen (name) + pos > max)
    {
      fprintf (file, indent);
      return fprintf (file, "%s%s", type, name);
    }
  return pos + fprintf (file, "%s%s%s", sep, type, name);
}

static struct
  {
    char *name;
    int value;
  }

m_options[] = TARGET_SWITCHES;

static void
output_options (file, f_options, f_len, W_options, W_len,
		pos, max, sep, indent, term)
     FILE *file;
     struct option *f_options;
     struct option *W_options;
     int f_len, W_len;
     int pos;
     int max;
     char *sep;
     char *indent;
     char *term;
{
  register int j;


  if (optimize)
    pos = output_option (file, sep, "-O", "", indent, pos, max);
  if (write_symbols != NO_DEBUG)
    pos = output_option (file, sep, "-g", "", indent, pos, max);
  if (flag_traditional)
    pos = output_option (file, sep, "-traditional", "", indent, pos, max);
  if (profile_flag)
    pos = output_option (file, sep, "-p", "", indent, pos, max);
  if (profile_block_flag)
    pos = output_option (file, sep, "-a", "", indent, pos, max);

  for (j = 0; j < f_len; j++)
    if (*f_options[j].variable == f_options[j].on_value)
      pos = output_option (file, sep, "-f", f_options[j].string,
			   indent, pos, max);

  for (j = 0; j < W_len; j++)
    if (*W_options[j].variable == W_options[j].on_value)
      pos = output_option (file, sep, "-W", W_options[j].string,
			   indent, pos, max);

  for (j = 0; j < sizeof m_options / sizeof m_options[0]; j++)
    if (m_options[j].name[0] != '\0'
	&& m_options[j].value > 0
	&& ((m_options[j].value & target_flags)
	    == m_options[j].value))
      pos = output_option (file, sep, "-m", m_options[j].name,
			   indent, pos, max);


  fprintf (file, term);
}

void
output_file_start (file, f_options, f_len, W_options, W_len)
     FILE *file;
     struct option *f_options;
     struct option *W_options;
     int f_len, W_len;
{
  register int pos;

  output_file_directive (file, main_input_filename);

  /* Switch to the data section so that the coffsem symbol and the
     gcc2_compiled. symbol aren't in the text section.  */
  data_section ();


  pos = fprintf (file, "\n! Hitachi SH cc1 (%s) arguments:", version_string);
  output_options (file, f_options, f_len, W_options, W_len,
		  pos, 75, " ", "\n! ", "\n\n");
}


/* Code to generate prologue and epilogue sequences */

void
sh_expand_prologue ()
{
  int live_regs_mask;
  int d;

  live_regs_mask = calc_live_regs (&d);

  output_stack_adjust (-1, current_function_pretend_args_size);

  if (current_function_anonymous_args)
    {
      /* Push arg regs as if they'd been provided by caller in stack */
      int i;
      for (i = 0; i < NPARM_REGS; i++)
	{
	  int rn = NPARM_REGS + FIRST_PARM_REG - i - 1;
	  if (i > NPARM_REGS - current_function_args_info)
	    break;
	  push (rn);

	  extra_push += 4;
	}
    }

  if (frame_pointer_needed)
    {
      push_regs (live_regs_mask);
      emit_insn (gen_movsi (frame_pointer_rtx, stack_pointer_rtx));
    }
  else
    {
      push_regs (live_regs_mask);
    }

  output_stack_adjust (-1, get_frame_size ());
}

void
sh_expand_epilogue ()
{
  int live_regs_mask;
  int d;
  int i;

  live_regs_mask = calc_live_regs (&d);

  if (frame_pointer_needed)
    {
      emit_insn (gen_movsi (stack_pointer_rtx, frame_pointer_rtx));
    }
  else
    {
      output_stack_adjust (1, get_frame_size ());
    }


  /* Pop all the registers */
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      int j = (FIRST_PSEUDO_REGISTER - 1) - i;
      if (live_regs_mask & (1 << j))
	{
	  pop (j);
	}
    }
  output_stack_adjust (1, extra_push +
		       current_function_pretend_args_size);

  extra_push = 0;

  current_function_anonymous_args = 0;
}


/* Return the cost of a shift */

int
shiftcosts (RTX)
     rtx RTX;
{
  /* If shift by a non constant, then this will be expensive. */
  if (GET_CODE (XEXP (RTX, 1)) != CONST_INT)
    return 20;

  /* otherwise, it will be very cheap if by one of the constants
     we can cope with. */
  if (CONST_OK_FOR_K (INTVAL (XEXP (RTX, 1))))
    return 1;

  /* otherwise it will be several insns. */
  return 4;
}

/* Return the cost of a multiply */
int
multcosts (RTX)
     rtx RTX;
{
  /* If we we're aiming at small code, then just count the number of
     insns in a multiply call sequence, otherwise, count all the insnsn
     inside the call. */
  if (TARGET_SMALLCODE)
    return 3;
  return 30;
}
