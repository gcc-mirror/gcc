/* Output routines for GCC for ARM/RISCiX.
   Copyright (C) 1991 Free Software Foundation, Inc.
   Contributed by Pieter `Tiggr' Schoenmakers (rcpieter@win.tue.nl)
   	      and Martin Simmons (@harleqn.co.uk).

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
#include "assert.h"
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
#include "flags.h"

/* The maximum number of insns skipped which will be conditionalised if
   possible.  */
#define MAX_INSNS_SKIPPED  5

/* Some function declarations.  */
extern FILE *asm_out_file;
extern char *output_multi_immediate ();
extern char *arm_output_asm_insn ();
extern void arm_increase_location ();

/* In case of a PRE_INC, POST_INC, PRE_DEC, POST_DEC memory reference, we
   must report the mode of the memory reference from PRINT_OPERAND to
   PRINT_OPERAND_ADDRESS.  */
int output_memory_reference_mode;

/* Nonzero if the prologue must setup `fp'.  */
int current_function_anonymous_args;

/* Location counter of .text segment.  */
int arm_text_location = 0;

/* A hash table is used to store text segment labels and their associated
   offset from the start of the text segment.  */
struct label_offset
{
  char *name;
  int offset;
  struct label_offset *cdr;
};

#define LABEL_HASH_SIZE  257

static struct label_offset *offset_table[LABEL_HASH_SIZE];

/* For an explanation of these variables, see final_prescan_insn below.  */
int arm_ccfsm_state;
int arm_current_cc;
rtx arm_target_insn;
int arm_target_label;
char *arm_condition_codes[];

/* Return the number of mov instructions needed to get the constant VALUE into
   a register.  */

int
arm_const_nmoves (value)
     register int value;
{
  register int i;

  if (value == 0)
    return (1);
  for (i = 0; value; i++, value &= ~0xff)
    while ((value & 3) == 0)
      value = (value >> 2) | ((value & 3) << 30);
  return (i);
} /* arm_const_nmoves */


/* Return TRUE if int I is a valid immediate ARM constant.  */

int
const_ok_for_arm (i)
     int i;
{
  unsigned int mask = ~0xFF;

  do
    {
      if ((i & mask) == 0)
	return(TRUE);
      mask = (mask << 2) | (mask >> (32 - 2));
    } while (mask != ~0xFF);

  return (FALSE);
} /* const_ok_for_arm */

/* Return TRUE if rtx X is a valid immediate FPU constant. */

int
const_double_rtx_ok_for_fpu (x)
     rtx x;
{
  double d;
  union real_extract u;
  u.i[0] = CONST_DOUBLE_LOW(x);
  u.i[1] = CONST_DOUBLE_HIGH(x);
  d = u.d;

  return (d == 0.0 || d == 1.0 || d == 2.0 || d == 3.0
	  || d == 4.0 || d == 5.0 || d == 0.5 || d == 10.0);
} /* const_double_rtx_ok_for_fpu */

/* Predicates for `match_operand' and `match_operator'.  */

/* Return TRUE for valid operands for the rhs of an ARM instruction.  */

int
arm_rhs_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT && const_ok_for_arm (INTVAL (op))));
} /* arm_rhs_operand */

/* Return TRUE for valid operands for the rhs of an FPU instruction.  */

int
fpu_rhs_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (register_operand (op, mode))
    return(TRUE);
  else if (GET_CODE (op) == CONST_DOUBLE)
    return (const_double_rtx_ok_for_fpu (op));
  else return (FALSE);
} /* fpu_rhs_operand */

/* Return nonzero if OP is a constant power of two.  */

int
power_of_two_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == CONST_INT)
    {
      int value = INTVAL(op);
      return (value != 0  &&  (value & (value-1)) == 0);
    }
  return (FALSE);
} /* power_of_two_operand */

/* Return TRUE for a valid operand of a DImode operation.
   Either: REG, CONST_DOUBLE or MEM(offsettable).
   Note that this disallows MEM(REG+REG).  */

int
di_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (register_operand (op, mode))
    return (TRUE);

  switch (GET_CODE (op))
    {
    case CONST_DOUBLE:
    case CONST_INT:
      return (TRUE);
    case MEM:
      return (memory_address_p (DImode, XEXP (op, 0))
	      && offsettable_address_p (FALSE, DImode, XEXP (op, 0)));
    default:
      return (FALSE);
    }
} /* di_operand */

/* Return TRUE for valid index operands. */

int
index_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand(op, mode)
	  || (immediate_operand (op, mode) && abs (INTVAL (op)) < 4096));
} /* index_operand */

/* Return TRUE for arithmetic operators which can be combined with a multiply
   (shift).  */

int
shiftable_operator (x, mode)
     rtx x;
     enum machine_mode mode;
{
  if (GET_MODE (x) != mode)
    return FALSE;
  else
    {
      enum rtx_code code = GET_CODE (x);

      return (code == PLUS || code == MINUS
	      || code == IOR || code == XOR || code == AND);
    }
} /* shiftable_operator */

/* Return TRUE for shift operators. */

int
shift_operator (x, mode)
     rtx x;
     enum machine_mode mode;
{
  if (GET_MODE (x) != mode)
    return FALSE;
  else
    {
      enum rtx_code code = GET_CODE (x);

      return (code == ASHIFT || code == LSHIFT
	      || code == ASHIFTRT || code == LSHIFTRT);
    }
} /* shift_operator */

/* Routines to output assembly language.  */

/* Output the operands of a LDM/STM instruction to STREAM.
   MASK is the ARM register set mask of which only bits 0-15 are important.
   INSTR is the possibly suffixed base register.  HAT unequals zero if a hat
   must follow the register list.  */

void
print_multi_reg (stream, instr, mask, hat)
     FILE *stream;
     char *instr;
     int mask, hat;
{
  int i;
  int not_first = FALSE;

  fprintf (stream, "\t%s, {", instr);
  for (i = 0; i < 16; i++)
    if (mask & (1 << i))
      {
	if (not_first)
	  fprintf (stream, ", ");
	fprintf (stream, "%s", reg_names[i]);
	not_first = TRUE;
      }
  fprintf (stream, "}%s\n", hat ? "^" : "");
} /* print_multi_reg */

/* Output a 'call' insn. */

char *
output_call (operands)
	rtx operands[];
{
  operands[0] = XEXP (operands[0], 0);

  /* Handle calls to lr using ip (which may be clobbered in subr anyway). */

  if (REGNO (operands[0]) == 14)
    {
      operands[0] = gen_rtx (REG, SImode, 12);
      arm_output_asm_insn ("mov\t%0, lr", operands);
    }
  arm_output_asm_insn ("mov\tlr, pc", operands);
  arm_output_asm_insn ("mov\tpc, %0", operands);
  return ("");
} /* output_call */

/* Output a move from arm registers to an fpu registers.
   OPERANDS[0] is an fpu register.
   OPERANDS[1] is the first registers of an arm register pair.  */

char *
output_mov_double_fpu_from_arm (operands)
     rtx operands[];
{
  int arm_reg0 = REGNO (operands[1]);
  rtx ops[2];

  if (arm_reg0 == 12)
    abort();
  ops[0] = gen_rtx (REG, SImode, arm_reg0);
  ops[1] = gen_rtx (REG, SImode, 1 + arm_reg0);
  arm_output_asm_insn ("stmfd\tsp!, {%0, %1}", ops);
  arm_output_asm_insn ("ldfd\t%0, [sp], #8", operands);
  return ("");
} /* output_mov_double_fpu_from_arm */

/* Output a move from an fpu register to arm registers.
   OPERANDS[0] is the first registers of an arm register pair.
   OPERANDS[1] is an fpu register.  */

char *
output_mov_double_arm_from_fpu (operands)
     rtx operands[];
{
  int arm_reg0 = REGNO (operands[0]);
  rtx ops[2];

  if (arm_reg0 == 12)
    abort();
  ops[0] = gen_rtx (REG, SImode, arm_reg0);
  ops[1] = gen_rtx (REG, SImode, 1 + arm_reg0);
  arm_output_asm_insn ("stfd\t%1, [sp, #-8]!", operands);
  arm_output_asm_insn ("ldmfd\tsp!, {%0, %1}", ops);
  return("");
} /* output_mov_double_arm_from_fpu */

/* Output a move between double words.
   It must be REG<-REG, REG<-CONST_DOUBLE, REG<-CONST_INT, REG<-MEM
   or MEM<-REG and all MEMs must be offsettable addresses.  */

char *
output_move_double (operands)
     rtx operands[];
{
  enum rtx_code code0 = GET_CODE (operands[0]);
  enum rtx_code code1 = GET_CODE (operands[1]);
  rtx otherops[2];

  if (code0 == REG)
    {
      int reg0 = REGNO (operands[0]);

      otherops[0] = gen_rtx (REG, SImode, 1 + reg0);
      if (code1 == REG)
	{
	  int reg1 = REGNO (operands[1]);
	  if (reg1 == 12)
	    abort();
	  otherops[1] = gen_rtx (REG, SImode, 1 + reg1);

	  /* Ensure the second source is not overwritten */
	  if (reg0 == 1 + reg1)
	    {
	      arm_output_asm_insn("mov\t%0, %1", otherops);
	      arm_output_asm_insn("mov\t%0, %1", operands);
	    }
	  else
	    {
	      arm_output_asm_insn("mov\t%0, %1", operands);
	      arm_output_asm_insn("mov\t%0, %1", otherops);
	    }
	}
      else if (code1 == CONST_DOUBLE)
	{
	  otherops[1] = gen_rtx (CONST_INT, VOIDmode,
				 CONST_DOUBLE_HIGH (operands[1]));
	  operands[1] = gen_rtx (CONST_INT, VOIDmode,
				 CONST_DOUBLE_LOW (operands[1]));
	  arm_output_asm_insn ("mov\t%0, %1", operands);
	  arm_output_asm_insn ("mov\t%0, %1", otherops);
	}
      else if (code1 == CONST_INT)
	{
	  otherops[1] = const0_rtx;
	  arm_output_asm_insn ("mov\t%0, %1", operands);
	  arm_output_asm_insn ("mov\t%0, %1", otherops);
	}
      else if (code1 == MEM)
	{
	  if (GET_CODE (XEXP (operands[1], 0)) == REG)
	    {
	      /* Handle the simple case where address is [r, #0] more
		 efficient.  */
	      operands[1] = XEXP (operands[1], 0);
	      arm_output_asm_insn ("ldmia\t%1, %M0", operands);
	    }
	  else
	    {
	      otherops[1] = adj_offsettable_operand (operands[1], 4);
	      /* Take care of overlapping base/data reg.  */
	      if (reg_mentioned_p (operands[0], operands[1]))
		{
		  arm_output_asm_insn ("ldr\t%0, %1", otherops);
		  arm_output_asm_insn ("ldr\t%0, %1", operands);
		}
	      else
		{
		  arm_output_asm_insn ("ldr\t%0, %1", operands);
		  arm_output_asm_insn ("ldr\t%0, %1", otherops);
		}
	    }
	}
      else abort();  /* Constraints should prevent this */
    }
  else if (code0 == MEM && code1 == REG)
    {
      if (REGNO (operands[1]) == 12)
	abort();

      if (GET_CODE (XEXP (operands[0], 0)) == REG)
	{
	  operands[0] = XEXP (operands[0], 0);
	  arm_output_asm_insn ("stmia\t%0, %M1", operands);
	}
      else
	{
	  otherops[0] = adj_offsettable_operand (operands[0], 4);
	  otherops[1] = gen_rtx (REG, SImode, 1 + REGNO (operands[1]));
	  arm_output_asm_insn ("str\t%1, %0", operands);
	  arm_output_asm_insn ("str\t%1, %0", otherops);
	}
    }
  else abort();  /* Constraints should prevent this */

  return("");
} /* output_move_double */


/* Output an arbitrary MOV reg, #n.
   OPERANDS[0] is a register.  OPERANDS[1] is a const_int.  */

char *
output_mov_immediate (operands)
     rtx operands[2];
{
  int n = INTVAL (operands[1]);
  int n_ones = 0;
  int i;

  /* Try to use one MOV */

  if (const_ok_for_arm (n))
    return (arm_output_asm_insn ("mov\t%0, %1", operands));

  /* Try to use one MVN */

  if (const_ok_for_arm(~n))
    {
      operands[1] = gen_rtx (CONST_INT, VOIDmode, ~n);
      return (arm_output_asm_insn ("mvn\t%0, %1", operands));
    }

  /* If all else fails, make it out of ORRs or BICs as appropriate. */

  for (i=0; i < 32; i++)
    if (n & 1 << i)
      n_ones++;

  if (n_ones > 16)  /* Shorter to use MVN with BIC in this case. */
    output_multi_immediate(operands, "mvn\t%0, %1", "bic\t%0, %0, %1", 1, ~n);
  else
    output_multi_immediate(operands, "mov\t%0, %1", "orr\t%0, %0, %1", 1, n);
  return("");
} /* output_mov_immediate */


/* Output an ADD r, s, #n where n may be too big for one instruction.  If
   adding zero to one register, output nothing.  */

char *
output_add_immediate (operands)
     rtx operands[3];
{
  int n = INTVAL (operands[2]);

  if (n != 0 || REGNO (operands[0]) != REGNO (operands[1]))
    {
      if (n < 0)
	output_multi_immediate (operands,
				"sub\t%0, %1, %2", "sub\t%0, %0, %2", 2, -n);
      else
	output_multi_immediate (operands,
				"add\t%0, %1, %2", "add\t%0, %0, %2", 2, n);
    }
  return("");
} /* output_add_immediate */


/* Output a multiple immediate operation.
   OPERANDS is the vector of operands referred to in the output patterns.
   INSTR1 is the output pattern to use for the first constant.
   INSTR2 is the output pattern to use for subsequent constants.
   IMMED_OP is the index of the constant slot in OPERANDS.
   N is the constant value.  */

char *
output_multi_immediate (operands, instr1, instr2, immed_op, n)
     rtx operands[];
     char *instr1, *instr2;
     int immed_op, n;
{
  if (n == 0)
    {
      operands[immed_op] = const0_rtx;
      arm_output_asm_insn (instr1, operands); /* Quick and easy output */
    }
  else
    {
      int i;
      char *instr = instr1;

      /* Note that n is never zero here (which would give no output) */

      for (i = 0; i < 32; i += 2)
	{
	  if (n & (3 << i))
	    {
	      operands[immed_op] = gen_rtx (CONST_INT, VOIDmode,
					    n & (255 << i));
	      arm_output_asm_insn (instr, operands);
	      instr = instr2;
	      i += 6;
	    }
	}
    }
  return ("");
} /* output_multi_immediate */


/* Return the appropriate ARM instruction for the operation code.
   The returned result should not be overwritten.  OP is the rtx of the
   operation.  SHIFT_FIRST_ARG is TRUE if the first argument of the operator
   was shifted.  */

char *
arithmetic_instr (op, shift_first_arg)
     rtx op;
{
  switch (GET_CODE(op))
    {
    case PLUS:
      return ("add");
    case MINUS:
      if (shift_first_arg)
	return ("rsb");
      else
	return ("sub");
    case IOR:
      return ("orr");
    case XOR:
      return ("eor");
    case AND:
      return ("and");
    default:
      abort();
    }
  return ("");			/* stupid cc */
} /* arithmetic_instr */


/* Ensure valid constant shifts and return the appropriate shift mnemonic
   for the operation code.  The returned result should not be overwritten.
   OP is the rtx code of the shift.
   SHIFT_PTR points to the shift size operand.  */

char *
shift_instr (op, shift_ptr)
     enum rtx_code op;
     rtx *shift_ptr;
{
  int min_shift = 0;
  int max_shift = 31;
  char *mnem;

  switch (op)
    {
    case ASHIFT:
      mnem = "asl";
      break;
    case LSHIFT:
      mnem = "lsl";
      break;
    case ASHIFTRT:
      mnem = "asr";
      max_shift = 32;
      break;
    case LSHIFTRT:
      mnem = "lsr";
      max_shift = 32;
      break;
    default:
      abort();
    }

  if (GET_CODE (*shift_ptr) == CONST_INT)
    {
      int shift = INTVAL (*shift_ptr);

      if (shift < min_shift)
	*shift_ptr = gen_rtx (CONST_INT, VOIDmode, 0);
      else if (shift > max_shift)
	*shift_ptr = gen_rtx (CONST_INT, VOIDmode, max_shift);
    }
  return (mnem);
} /* shift_instr */


/* Obtain the shift from the POWER of two. */

int
int_log2 (power)
     unsigned int power;
{
  int shift = 0;

  while (((1 << shift) & power) == 0)
    {
      if (shift > 31)
	abort();
      shift++;
    }
  return (shift);
} /* int_log2 */


/* Output an arithmetic instruction which may set the condition code.
   OPERANDS[0] is the destination register.
   OPERANDS[1] is the arithmetic operator expression.
   OPERANDS[2] is the left hand argument.
   OPERANDS[3] is the right hand argument.
   CONST_FIRST_ARG is TRUE if the first argument of the operator was constant.
   SET_COND is TRUE when the condition code should be set.  */

char *
output_arithmetic (operands, const_first_arg, set_cond)
     rtx operands[4];
     int const_first_arg;
     int set_cond;
{
  char mnemonic[80];
  char *instr = arithmetic_instr (operands[1], const_first_arg);

  sprintf (mnemonic, "%s%s\t%%0, %%2, %%3", instr, set_cond ? "s" : "");
  return (arm_output_asm_insn (mnemonic, operands));
} /* output_arithmetic */


/* Output an arithmetic instruction with a shift.
   OPERANDS[0] is the destination register.
   OPERANDS[1] is the arithmetic operator expression.
   OPERANDS[2] is the unshifted register.
   OPERANDS[3] is the shift operator expression.
   OPERANDS[4] is the shifted register.
   OPERANDS[5] is the shift constant or register.
   SHIFT_FIRST_ARG is TRUE if the first argument of the operator was shifted.
   SET_COND is TRUE when the condition code should be set.  */

char *
output_arithmetic_with_shift (operands, shift_first_arg, set_cond)
     rtx operands[6];
     int shift_first_arg;
     int set_cond;
{
  char mnemonic[80];
  char *instr = arithmetic_instr (operands[1], shift_first_arg);
  char *condbit = set_cond ? "s" : "";
  char *shift = shift_instr (GET_CODE (operands[3]), &operands[5]);

  sprintf (mnemonic, "%s%s\t%%0, %%2, %%4, %s %%5", instr, condbit, shift);
  return (arm_output_asm_insn (mnemonic, operands));
} /* output_arithmetic_with_shift */


/* Output an arithmetic instruction with a power of two multiplication.
   OPERANDS[0] is the destination register.
   OPERANDS[1] is the arithmetic operator expression.
   OPERANDS[2] is the unmultiplied register.
   OPERANDS[3] is the multiplied register.
   OPERANDS[4] is the constant multiple (power of two).
   SHIFT_FIRST_ARG is TRUE if the first arg of the operator was multiplied.  */

char *
output_arithmetic_with_immediate_multiply (operands, shift_first_arg)
     rtx operands[5];
     int shift_first_arg;
{
  char mnemonic[80];
  char *instr = arithmetic_instr (operands[1], shift_first_arg);
  int shift = int_log2 (INTVAL (operands[4]));

  sprintf (mnemonic, "%s\t%%0, %%2, %%3, asl#%d", instr, shift);
  return (arm_output_asm_insn (mnemonic, operands));
} /* output_arithmetic_with_immediate_multiply */


/* Output a move with a shift.
   OP is the shift rtx code.
   OPERANDS[0] = destination register.
   OPERANDS[1] = source register.
   OPERANDS[2] = shift constant or register.  */

char *
output_shifted_move (op, operands)
     enum rtx_code op;
     rtx operands[2];
{
  char mnemonic[80];

  if (GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) == 0)
    sprintf (mnemonic, "mov\t%%0, %%1");
  else
    sprintf (mnemonic, "mov\t%%0, %%1, %s %%2",
	     shift_instr (op, &operands[2]));
  return (arm_output_asm_insn (mnemonic, operands));
} /* output_shifted_move */


/* Output a .ascii pseudo-op, keeping track of lengths.  This is because
   /bin/as is horribly restrictive.  */

void
output_ascii_pseudo_op (stream, p, len)
     FILE *stream;
     char *p;
     int len;
{
  int i;
  int len_so_far = 1000;
  int chars_so_far = 0;

  for (i = 0; i < len; i++)
    {
      register int c = p[i];

      if (len_so_far > 50)
	{
	  if (chars_so_far)
	    fputs ("\"\n", stream);
	  fputs ("\t.ascii\t\"", stream);
	  len_so_far = 0;
	  arm_increase_location (chars_so_far);
	  chars_so_far = 0;
	}

      if (c == '\"' || c == '\\')
	{
	  putc('\\', stream);
	  len_so_far++;
	}
      if (c >= ' ' && c < 0177)
	{
	  putc (c, stream);
	  len_so_far++;
	}
      else
	{
	  fprintf (stream, "\\%03o", c);
	  len_so_far +=4;
	}
      chars_so_far++;
    }
  fputs ("\"\n", stream);
  arm_increase_location (chars_so_far);
} /* output_ascii_pseudo_op */

void
output_prologue (f, frame_size)
     FILE *f;
     int frame_size;
{

  int reg, live_regs_mask = 0, code_size = 0;
  rtx operands[3];

  /* Nonzero if the `fp' (argument pointer) register is needed.  */
  int fp_needed = 0;

  /* Nonzero if we must stuff some register arguments onto the stack as if
     they were passed there.  */
  int store_arg_regs = 0;

  fprintf (f, "\t@ args = %d, pretend = %d, frame = %d\n",
	   current_function_args_size, current_function_pretend_args_size, frame_size);
  fprintf (f, "\t@ frame_pointer_needed = %d, current_function_anonymous_args = %d\n",
	   frame_pointer_needed, current_function_anonymous_args);

  if (current_function_pretend_args_size || current_function_args_size
      || frame_pointer_needed || current_function_anonymous_args || TARGET_APCS)
    fp_needed = 1;

  if (current_function_anonymous_args && current_function_pretend_args_size)
    store_arg_regs = 1;

  for (reg = 4; reg < 10; reg++)
    if (regs_ever_live[reg])
      live_regs_mask |= (1 << reg);

  if (fp_needed)
    {
      live_regs_mask |= 0xD800;
      /* The following statement is probably redundant now
	 because the frame pointer is recorded in regs_ever_live.  */
      if (frame_pointer_needed)
	live_regs_mask |= (1 << FRAME_POINTER_REGNUM);
      fputs ("\tmov\tip, sp\n", f);
      code_size += 4;
    }
  else if (regs_ever_live[14])
    live_regs_mask |= 0x4000;

  /* If CURRENT_FUNCTION_PRETEND_ARGS_SIZE, adjust the stack pointer to make
     room.  If also STORE_ARG_REGS store the argument registers involved in
     the created slot (this is for stdarg and varargs).  */
  if (current_function_pretend_args_size)
    {
      if (store_arg_regs)
	{
	  int arg_size, mask = 0;

	  assert (current_function_pretend_args_size <= 16);
	  for (reg = 3, arg_size = current_function_pretend_args_size;
	       arg_size > 0; reg--, arg_size -= 4)
	    mask |= (1 << reg);
	  print_multi_reg (f, "stmfd\tsp!", mask, FALSE);
	}
      else
	{
	  operands[0] = operands[1] = stack_pointer_rtx;
	  operands[2] = gen_rtx (CONST_INT, VOIDmode,
				 -current_function_pretend_args_size);
	  output_add_immediate (operands);
	}
    }

  if (live_regs_mask)
    {
      print_multi_reg (f, "stmfd\tsp!", live_regs_mask, FALSE);
      code_size += 4;
    }

  for (reg = 23; reg > 19; reg--)
    if (regs_ever_live[reg])
      {
	fprintf (f, "\tstfe\t%s, [sp, #-12]!\n", reg_names[reg]);
	code_size += 4;
      }

  if (fp_needed)
    {
      /* Make `fp' point to saved value of `pc'. */

      operands[0] = arg_pointer_rtx;
      operands[1] = gen_rtx (REG, SImode, 12);
      operands[2] = gen_rtx (CONST_INT, VOIDmode,
			     - (4 + current_function_pretend_args_size));
      output_add_immediate (operands);
    }

  if (frame_pointer_needed)
    {
      fprintf (f, "\tmov\trfp, sp\n");
      code_size += 4;
    }

  if (frame_size)
    {
      operands[0] = operands[1] = stack_pointer_rtx;
      operands[2] = gen_rtx (CONST_INT, VOIDmode, -frame_size);
      output_add_immediate (operands);
    }

  arm_increase_location (code_size);
} /* output_prologue */


void
output_epilogue (f, frame_size)
     FILE *f;
     int frame_size;
{
  int reg, live_regs_mask = 0, code_size = 0, fp_needed = 0;
  rtx operands[3];

  if (current_function_pretend_args_size || current_function_args_size
      || frame_pointer_needed || current_function_anonymous_args || TARGET_APCS)
    fp_needed = 1;

  for (reg = 4; reg < 10; reg++)
    if (regs_ever_live[reg])
      live_regs_mask |= (1 << reg);

  if (fp_needed)
    {
      live_regs_mask |= 0xA800;
      if (frame_pointer_needed)
        live_regs_mask |= (1 << FRAME_POINTER_REGNUM);
    }
  else if (regs_ever_live[14])
    live_regs_mask |= 0x4000;

  for (reg = 20; reg < 24; reg++)
    if (regs_ever_live[reg])
      {
	fprintf (f, "\tldfe\t%s, [%s], #12\n", reg_names[reg],
		 frame_pointer_needed ? "rfp" : "sp");
	code_size += 4;
      }

  if (fp_needed)
    {
      print_multi_reg (f, "ldmea\tfp", live_regs_mask, TRUE);
      code_size += 4;
    }
  else
    {
      /* Restore stack pointer if necessary.  */
      if (frame_size)
	{
	  operands[0] = operands[1] = stack_pointer_rtx;
	  operands[2] = gen_rtx (CONST_INT, VOIDmode, frame_size);
	  output_add_immediate (operands);
	}

      if (current_function_pretend_args_size == 0 && regs_ever_live[14])
	{
	  print_multi_reg (f, "ldmfd\tsp!",
			   (live_regs_mask & ~0x4000) | 0x8000, TRUE);
	  code_size += 4;
	}
      else
	{
	  if (live_regs_mask)
	    {
	      print_multi_reg (f, "ldmfd\tsp!", live_regs_mask, FALSE);
	      code_size += 4;
	    }
	  if (current_function_pretend_args_size)
	    {
	      operands[0] = operands[1] = stack_pointer_rtx;
	      operands[2] = gen_rtx (CONST_INT, VOIDmode,
				     current_function_pretend_args_size);
	      output_add_immediate (operands);
	    }
	  fputs ("\tmovs\tpc, lr\n", f);
	  code_size += 4;
	}
    }
  arm_increase_location (code_size);
  current_function_anonymous_args = 0;
} /* output_epilogue */

/* Increase the `arm_text_location' by AMOUNT if we're in the text
   segment.  */

void
arm_increase_location (amount)
     int amount;
{
  if (in_text_section ())
    arm_text_location += amount;
} /* arm_increase_location */


/* Like output_asm_insn (), but also increases the arm_text_location (if in
   the .text segment, of course, even though this will always be true).
   Returns the empty string.  */

char *
arm_output_asm_insn (template, operands)
     char *template;
     rtx *operands;
{
  extern FILE *asm_out_file;

  output_asm_insn (template, operands);
  if (in_text_section ())
    arm_text_location += 4;
  fflush (asm_out_file);
  return ("");
} /* arm_output_asm_insn */


/* Output a label definition.  If this label is within the .text segment, it
   is stored in OFFSET_TABLE, to be used when building `llc' instructions.
   Maybe GCC remembers names not starting with a `*' for a long time, but this
   is a minority anyway, so we just make a copy.  Do not store the leading `*'
   if the name starts with one.  */

void
arm_asm_output_label (stream, name)
     FILE *stream;
     char *name;
{
  char *real_name, *s;
  struct label_offset *cur;
  int hash = 0;

  assemble_name (stream, name);
  fputs (":\n", stream);
  if (! in_text_section ())
    return;

  if (name[0] == '*')
    {
      real_name = xmalloc (1 + strlen (&name[1]));
      strcpy (real_name, &name[1]);
    }
  else
    {
      real_name = xmalloc (2 + strlen (name));
      strcpy (real_name, "_");
      strcat (real_name, name);
    }
  for (s = real_name; *s; s++)
    hash += *s;
  hash = hash % LABEL_HASH_SIZE;
  cur = (struct label_offset *) xmalloc (sizeof (struct label_offset));
  cur->name = real_name;
  cur->offset = arm_text_location;
  cur->cdr = offset_table[hash];
  offset_table[hash] = cur;
} /* arm_asm_output_label */


/* Output the instructions needed to perform what Martin's /bin/as called
   llc: load an SImode thing from the function's constant pool.

   XXX This could be enhanced in that we do not really need a pointer in the
   constant pool pointing to the real thing.  If we can address this pointer,
   we can also address what it is pointing at, in fact, anything in the text
   segment which has been defined already within this .s file.  */

char *
arm_output_llc (operands)
     rtx *operands;
{
  char *s, *name = XSTR (XEXP (operands[1], 0), 0);
  struct label_offset *he;
  int hash = 0, conditional = (arm_ccfsm_state == 3 || arm_ccfsm_state == 4);

  if (*name != '*')
    abort ();

  for (s = &name[1]; *s; s++)
    hash += *s;
  hash = hash % LABEL_HASH_SIZE;
  he = offset_table[hash];
  while (he && strcmp (he->name, &name[1]))
    he = he->cdr;

  if (!he)
    abort ();

  if (arm_text_location + 8 - he->offset < 4095)
    {
      fprintf (asm_out_file, "\tldr%s\t%s, [pc, #%s - . - 8]\n",
	       conditional ? arm_condition_codes[arm_current_cc] : "",
	       reg_names[REGNO (operands[0])], &name[1]);
      arm_increase_location (4);
      return ("");
    }
  else
    {
      int offset = - (arm_text_location + 8 - he->offset);
      char *reg_name = reg_names[REGNO (operands[0])];

      /* ??? This is a hack, assuming the constant pool never is more than
	 (1 + 255) * 4096 == 1Meg away from the PC.  */

      if (offset > 1000000)
	abort ();

      fprintf (asm_out_file, "\tsub%s\t%s, pc, #(8 + . - %s) & ~4095\n",
	       conditional ? arm_condition_codes[arm_current_cc] : "",
	       reg_name, &name[1]);
      fprintf (asm_out_file, "\tldr%s\t%s, [%s, #- ((4 + . - %s) & 4095)]\n",
	       conditional ? arm_condition_codes[arm_current_cc] : "",
	       reg_name, reg_name, &name[1]);
      arm_increase_location (8);
    }
  return ("");
} /* arm_output_llc */


/* Output code resembling an .lcomm directive.  /bin/as doesn't have this
   directive hence this hack, which works by reserving some `.space' in the
   bss segment directly.

   XXX This is a severe hack, which is guaranteed NOT to work since it doesn't
   define STATIC COMMON space but merely STATIC BSS space.  */

void
output_lcomm_directive (stream, name, size, rounded)
     FILE *stream;
     char *name;
     int size, rounded;
{
  fputs ("\n\t.bss\t@ .lcomm\n", stream);
  assemble_name (stream, name);
  fprintf (stream, ":\t.space\t%d\n", rounded);
  if (in_text_section ())
    fputs ("\n\t.text\n", stream);
  else
    fputs ("\n\t.data\n", stream);
} /* output_lcomm_directive */

/* A finite state machine takes care of noticing whether or not instructions
   can be conditionally executed, and thus decrease execution time and code
   size by deleting branch instructions.  The fsm is controlled by
   final_prescan_insn, and controls the actions of ASM_OUTPUT_OPCODE.  */

/* The state of the fsm controlling condition codes are:
   0: normal, do nothing special
   1: make ASM_OUTPUT_OPCODE not output this instruction
   2: make ASM_OUTPUT_OPCODE not output this instruction
   3: make instructions conditional
   4: make instructions conditional

   State transitions (state->state by whom under condition):
   0 -> 1 final_prescan_insn if the `target' is a label
   0 -> 2 final_prescan_insn if the `target' is an unconditional branch
   1 -> 3 ASM_OUTPUT_OPCODE after not having output the conditional branch
   2 -> 4 ASM_OUTPUT_OPCODE after not having output the conditional branch
   3 -> 0 ASM_OUTPUT_INTERNAL_LABEL if the `target' label is reached
          (the target label has CODE_LABEL_NUMBER equal to arm_target_label).
   4 -> 0 final_prescan_insn if the `target' unconditional branch is reached
          (the target insn is arm_target_insn).

   XXX In case the `target' is an unconditional branch, this conditionalising
   of the instructions always reduces code size, but not always execution
   time.  But then, I want to reduce the code size to somewhere near what
   /bin/cc produces.  */

/* The condition codes of the ARM, and the inverse function.  */
char *arm_condition_codes[] =
{
  "eq", "ne", "cs", "cc", "mi", "pl", "vs", "vc",
  "hi", "ls", "ge", "lt", "gt", "le", "al", "nv"
};

#define ARM_INVERSE_CONDITION_CODE(X)  ((X) ^ 1)

/* Returns the index of the ARM condition code string in
   `arm_condition_codes'.  COMPARISON should be an rtx like
   `(eq (...) (...))'.  */

int
get_arm_condition_code (comparison)
     rtx comparison;
{
  switch (GET_CODE (comparison))
    {
    case NE: return (1);
    case EQ: return (0);
    case GE: return (10);
    case GT: return (12);
    case LE: return (13);
    case LT: return (11);
    case GEU: return (2);
    case GTU: return (8);
    case LEU: return (9);
    case LTU: return (3);
    default: abort ();
    }
  /*NOTREACHED*/
  return (42);
} /* get_arm_condition_code */


void
final_prescan_insn (insn, opvec, noperands)
     rtx insn;
     rtx *opvec;
     int noperands;
{
  /* BODY will hold the body of INSN.  */
  register rtx body = PATTERN (insn);

  /* This will be 1 if trying to repeat the trick, and things need to be
     reversed if it appears to fail.  */
  int reverse = 0;

  /* START_INSN will hold the insn from where we start looking.  This is the
     first insn after the following code_label if REVERSE is true.  */
  rtx start_insn = insn;

  /* If in state 4, check if the target branch is reached, in order to
     change back to state 0.  */
  if (arm_ccfsm_state == 4)
    {
      if (insn == arm_target_insn)
	arm_ccfsm_state = 0;
      return;
    }

  /* If in state 3, it is possible to repeat the trick, if this insn is an
     unconditional branch to a label, and immediately following this branch
     is the previous target label which is only used once, and the label this
     branch jumps to is not too far off.  */
  if (arm_ccfsm_state == 3)
    {
      if (simplejump_p (insn))
	{
	  start_insn = next_nonnote_insn (start_insn);
	  if (GET_CODE (start_insn) == BARRIER)
	    {
	      /* XXX Isn't this always a barrier?  */
	      start_insn = next_nonnote_insn (start_insn);
	    }
	  if (GET_CODE (start_insn) == CODE_LABEL
	      && CODE_LABEL_NUMBER (start_insn) == arm_target_label
	      && LABEL_NUSES (start_insn) == 1)
	    reverse = TRUE;
	  else
	    return;
	}
      else
	return;
    }

  if (arm_ccfsm_state != 0 && !reverse)
    abort ();
  if (GET_CODE (insn) != JUMP_INSN)
    return;

  if (reverse
      || (GET_CODE (body) == SET && GET_CODE (SET_DEST (body)) == PC
	  && GET_CODE (SET_SRC (body)) == IF_THEN_ELSE))
    {
      int insns_skipped = 0, fail = FALSE, succeed = FALSE;
      /* Flag which part of the IF_THEN_ELSE is the LABEL_REF.  */
      int then_not_else = TRUE;
      rtx this_insn = start_insn, label;

      /* Register the insn jumped to.  */
      if (reverse)
	label = XEXP (SET_SRC (body), 0);
      else if (GET_CODE (XEXP (SET_SRC (body), 1)) == LABEL_REF)
	label = XEXP (XEXP (SET_SRC (body), 1), 0);
      else if (GET_CODE (XEXP (SET_SRC (body), 2)) == LABEL_REF)
	{
	  label = XEXP (XEXP (SET_SRC (body), 2), 0);
	  then_not_else = FALSE;
	}
      else
	abort ();

      /* See how many insns this branch skips, and what kind of insns.  If all
	 insns are okay, and the label or unconditional branch to the same
	 label is not too far away, succeed.  */
      for (insns_skipped = 0;
	   !fail && !succeed && insns_skipped < MAX_INSNS_SKIPPED;
	   insns_skipped++)
	{
	  rtx scanbody;

	  this_insn = next_nonnote_insn (this_insn);
	  if (!this_insn)
	    break;

	  scanbody = PATTERN (this_insn);

	  switch (GET_CODE (this_insn))
	    {
	    case CODE_LABEL:
	      /* Succeed if it is the target label, otherwise fail since
		 control falls in from somewhere else.  */
	      if (this_insn == label)
		{
		  arm_ccfsm_state = 1;
		  succeed = TRUE;
		}
	      else
		fail = TRUE;
	      break;

	    case BARRIER:	/* XXX Is this case necessary?  */
	      /* Succeed if the following insn is the target label.
		 Otherwise fail.  */
	      this_insn = next_nonnote_insn (this_insn);
	      if (this_insn == label)
		{
		  arm_ccfsm_state = 1;
		  succeed = TRUE;
		}
	      else
		fail = TRUE;
	      break;

	    case JUMP_INSN:
      	      /* If this is an unconditional branch to the same label, succeed.
		 If it is to another label, do nothing.  If it is conditional,
		 fail.  */
	      /* XXX Probably, the test for the SET and the PC are unnecessary. */

	      if (GET_CODE (scanbody) == SET && GET_CODE (SET_DEST (scanbody)) == PC)
		{
		  if (GET_CODE (SET_SRC (scanbody)) == LABEL_REF
		      && XEXP (SET_SRC (scanbody), 0) == label && !reverse)
		    {
		      arm_ccfsm_state = 2;
		      succeed = TRUE;
		    }
		  else if (GET_CODE (SET_SRC (scanbody)) == IF_THEN_ELSE)
		    fail = TRUE;
		}
	      break;

	    case INSN:
	      /* Instructions affecting the condition codes make it fail.  */
	      if (sets_cc0_p (scanbody))
		fail = TRUE;
	      break;

	    default:
	      break;
	    }
	}
      if (succeed)
	{
	  if (arm_ccfsm_state == 1 || reverse)
	    arm_target_label = CODE_LABEL_NUMBER (label);
	  else if (arm_ccfsm_state == 2)
	    arm_target_insn = this_insn;
	  else
	    abort ();

	  /* If REVERSE is true, ARM_CURRENT_CC needs to be inverted from what
	     it was.  */
	  if (!reverse)
	    arm_current_cc = get_arm_condition_code (XEXP (SET_SRC (body), 0));
	  if (reverse || then_not_else)
	    arm_current_cc = ARM_INVERSE_CONDITION_CODE (arm_current_cc);
	}
    }
} /* final_prescan_insn */

/* EOF */
