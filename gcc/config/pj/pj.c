/* Output routines for GCC for picoJava II
   Copyright (C) 2000 Free Software Foundation, Inc.

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

/* Contributed by Steve Chamberlain (sac@pobox.com), of Transmeta.  */

/* The picoJava architecture doesn't have general registers, it has an
   operand stack.  Any of the first 256 words on the operand stack between
   the locations indicated by the vars register and the optop register
   are accessible with one instruction, almost as if they were registers.
   The opstack isn't aliased into memory, so deferecencing address of
   something on the opstack is impossible.

   Small scalar incoming arguments to a function arrive on the operand
   stack, large scalars and aggregates arrive in the `aggregate'
   stack.  The aggregate stack lives in normal memory.


   just before a call       after the call insn and frame setup.

   vars->   ....
   
      	   arg-5            vars->arg-5
   	   arg-4                  arg-4
   	   arg-3                  arg-3
   	   arg-2                  arg-2
   	   arg-1                  arg-1
   	   arg-0                  arg-0
   	   target-addr            old-vars
   	   #arg words             old-pc
   optop->                        saved globals
                                  local-0
                                  local-1
           		          ....
       		            optop->

   This port generates code for a machine with 32 general purpose
   registers, and on output changes the references to the fake registers
   into offsets from the vars register.  Because the opstack grows
   downwards and all indexes are negated, some care has to be taken here
   to deal with endian problems; for example after a call on a little endian
   machine, an incoming DImode argument of value 0x1122334455667788 in
   `register 0', would live on the opstack like this:

     vars - 0   0x11223344
     vars - 4   0x55667788
     vars - 8   old-vars
     vars - 12  old-pc

   The picoJava instructon to read and put that onto the opstack as a
   DImode value is `lload 0', yet the least significant word lives at
   vars - 4, for which the instruction is `iload 1'.  The incoming
   argument code remembers which arguments arrive swapped in the
   CUMULATIVE_ARGS structure.  The information is used to fill in
   pj_si_vars_offset_vec and pj_di_vars_offset_vec during the prologue
   printing.

   Outgoing arguments are collected in fake `outgoing' registers, or
   in the aggregate stack.  The emitted code to write into an outgoing
   register does nothing, which leaves the expression to be written on
   the top of the opstack.  GCC always evaluates arguments in the right
   order, so nothing more needs to be done.  */


#include <setjmp.h>
#include "config.h"
#include "system.h"
#include "rtl.h"
#include "tree.h"
#include "tm_p.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-flags.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "except.h"
#include "function.h"
#include "recog.h"
#include "expr.h"
#include "toplev.h"
#include "basic-block.h"
#include "ggc.h"

/* Compare insns in pj.md store the information needed to generate
   branch instructions here.  */
rtx pj_cmp_op0;
rtx pj_cmp_op1;
enum machine_mode pj_cmp_mode;

static void pj_output_rval PROTO ((rtx, enum machine_mode, rtx));
static void pj_output_store_into_lval PROTO ((enum machine_mode mode, rtx op));

/* These vectors turn a register number into an offset from the vars
   pointer register.  */
short pj_si_vars_offset_vec[FIRST_PSEUDO_REGISTER];
short pj_di_vars_offset_vec[FIRST_PSEUDO_REGISTER];
short pj_debugreg_renumber_vec[FIRST_PSEUDO_REGISTER];

/* Number of fake registers in the frame, used by prologue and epilogue
   code.  */
static int nfakes;

/* Whether anything has been printed to the current assembly output
   line. */
int pj_stuff_on_line;

/* printf to the asm_out_file, with special format control characters
   for decoding operands.  

 %*              - start of opcode
 %d,%x,%c,%s     - as printf
 %X              - address constant.
 %<alpha><digit> - operand <digit> passed to pj_print_operand with code <alpha>.  */

static void pj_printf
VPROTO ((const char *template, ...))
{
#ifndef ANSI_PROTOTYPES
  const char *template;
#endif
  register int c;

  va_list argptr;
  int ops_read = 0;
  rtx operands[10];
  VA_START (argptr, template);
#ifndef ANSI_PROTOTYPES
  template = va_arg (argptr, const char *);
#endif

  while ((c = *template++))
    {
      int was_stuff_on_line = pj_stuff_on_line;
      pj_stuff_on_line = 1;
      switch (c)
	{
	case '\n':
	  putc (c, asm_out_file);
	  pj_stuff_on_line = 0;
	  break;
	default:
	  putc (c, asm_out_file);
	  break;
	case '%':
	  {
	    switch (*template)
	      {
	      case '%':
		putc ('%', asm_out_file);
		template++;
		pj_stuff_on_line = 1;
		break;
	      case '*':
		/* Marks start of opcode, tab out.  */
		if (was_stuff_on_line)
		  fprintf (asm_out_file, "; ");
		template++;
		break;
	      case 'd':
		template++;
		fprintf (asm_out_file, "%d", va_arg (argptr, int));
		break;
	      case 'x':
		template++;
		fprintf (asm_out_file, "%x", va_arg (argptr, int));
		break;
	      case 'c':
		template++;
		fprintf (asm_out_file, "%c", va_arg (argptr, int));
		break;
	      case 's':
		template++;
		fputs (va_arg (argptr, const char *), asm_out_file);
		break;
	      case 'X':
		template++;
		output_addr_const (asm_out_file, va_arg (argptr, rtx));
		break;
	      default:
		{
		  int code = 0;
		  rtx send;

		  if (ISALPHA (*template))
		    code = *template++;
		  if (ISDIGIT (*template))
		    {
		      int num = atoi (template);
		      template++;
		      while (ops_read <= num)
			operands[ops_read++] = va_arg (argptr, rtx);
		      send = operands[num];
		    }
		  else
		    send = va_arg (argptr, rtx);

		  /* A null means leave the word on the stack, so there's
		     no need to do anything for that.  */

		  if (send)
		    pj_print_operand (asm_out_file, send, code);
		}
	      }
	  }
	}
    }
  va_end (argptr);
}

/* Output code to efficiently push a single word integer constant onto
   the opstack.  */

static void
pj_output_push_int (val)
     int val;
{
  int low = ((val & 0x8000) ? ~0xffff : 0) | (val & 0xffff);

  if (low == -1)
    pj_printf ("%*iconst_m1");
  else if (low >= 0 && low <= 5)
    pj_printf ("%*iconst_%d", low);
  else if (low >= -128 && low < 128)
    pj_printf ("%*bipush %d", low);
  else
    pj_printf ("%*sipush %d", low);

  if ((low & 0xffff0000) != (val & 0xffff0000))
    pj_printf ("%*sethi 0x%x", (val >> 16) & 0xffff);
}

/* Output code to add a constant to the value on the top of the
   opstack.  */

static void
pj_output_print_add_k (int size)
{
  if (size >= 0)
    {
      pj_output_push_int (size);
      pj_printf ("%*iadd");
    }
  else
    {
      pj_output_push_int (-size);
      pj_printf ("%*isub");
    }
}

/* Output code to load the value pointed to by the top of stack onto
   the stack.  */

static void
pj_output_load (mode, uns)
     enum machine_mode mode;
     int uns;
{
  int i;
  switch (GET_MODE_SIZE (mode))
    {
    case 1:
      pj_printf (uns ? "%*load_ubyte" : "%*load_byte");
      break;
    case 2:
      pj_printf (uns ? "%*load_char" : "%*load_short");
      break;
    case 8:
      if (TARGET_TM_EXTENSIONS)
	{
	  pj_printf ("%*tm_load_long");
	  break;
	}
      /* Fall through.  */
    default:
      for (i = GET_MODE_SIZE (mode); i > 4; i -= 4)
	{
	  pj_printf ("%*dup");
	  pj_output_print_add_k (i - 4);
	  pj_printf ("%*load_word");
	  pj_printf ("%*swap");
	}
      pj_printf ("%*load_word");
    }
}

/*  Output code to increment the provided lval operand.  */

static void
pj_output_inc (op, size)
     rtx op;
     int size;
{
  if (STACK_REG_RTX_P (op))
    pj_printf ("%*iinc %d,%d", pj_si_vars_offset_vec[REGNO (op)], size);
  else
    {
      pj_output_rval (op, SImode, 0);
      pj_output_push_int (size);
      pj_printf ("%*iadd");
      pj_output_store_into_lval (SImode, op);
    }
}

/* Output the text for a conversion operator.  */

static void
pj_output_cnv_op (e, op)
     enum insn_code e;
     rtx op;
{
  pj_printf ((const char *) insn_data[(int) e].output, 0, XEXP (op, 0));
}

/* Turn a machine_mode into an opcode modifier chararacter.  */

static char
mode_to_char (mode)
     enum machine_mode mode;
{
  switch (mode)
    {
    case QImode:
    case HImode:
    case SImode:
      return 'i';
      break;
    case DImode:
      return 'l';
      break;
    case DFmode:
      return 'd';
      break;
    case SFmode:
      return 'f';
      break;
    default:
      abort ();
    }
}

/* Output an index off the var register. If we're moving an 8 byte
   value then reduce the index, since the picoJava instruction loading
   the value uses the index of the highest part of the register as
   it's name.  */

static void
pj_output_varidx (mode, do_store, idx)
     enum machine_mode mode;
     int do_store;
     int idx;
{
  pj_printf ("%*%c%s%c%d",
	     mode_to_char (mode),
	     do_store ? "store" : "load",
	     (GET_MODE_SIZE (mode) == 4 || GET_MODE_SIZE (mode) == 8)
	     && idx <= 3 ? '_' : ' ', idx);
}

/* Output an rvalue expression.  */

static void
pj_output_rval (op, mode, outer_op)
     rtx op;
     enum machine_mode mode;
     rtx outer_op;
{
  enum rtx_code code = GET_CODE (op);

  optab tab;

  if (code == DIV && GET_MODE_CLASS (mode) == MODE_INT)
    tab = sdiv_optab;
  else
    tab = code_to_optab[code];

  if (code == PLUS)
    {
      pj_output_rval (XEXP (op, 0), mode, op);
      pj_output_rval (XEXP (op, 1), mode, op);
      pj_printf ("%*%cadd", mode_to_char (mode));
    }
  else if (tab && tab->handlers[mode].insn_code != CODE_FOR_nothing)
    {
      const char *template =
	(const char *) insn_data[tab->handlers[mode].insn_code].output;
      if (code == NEG)
	pj_printf (template, 0, XEXP (op, 0));
      else
	pj_printf (template, 0, XEXP (op, 0), XEXP (op, 1));
    }
  else
    switch (GET_CODE (op))
      {
      case PC:
	fprintf (asm_out_file, " pc ");
	break;

      case CONST:
	pj_output_rval (XEXP (op, 0), mode, op);
	break;

      case MEM:
	pj_output_rval (XEXP (op, 0), Pmode, op);
	pj_output_load (mode, 0);
	break;

      case SYMBOL_REF:
	pj_printf ("%*ipush %X", op);
	break;

      case REG:
	switch (mode)
	  {
	  case SImode:
	  case SFmode:
	  case HImode:
	  case QImode:
	    if (pj_si_vars_offset_vec[REGNO (op)] >= 0)
	      pj_output_varidx (mode, 0, pj_si_vars_offset_vec[REGNO (op)]);
	    else
	      pj_printf ("%*read_%s", reg_names[REGNO (op)]);
	    break;
	  case DImode:
	  case DFmode:
	    if (pj_di_vars_offset_vec[REGNO (op)] >= 0)
	      pj_output_varidx (mode, 0, pj_di_vars_offset_vec[REGNO (op)]);
	    else
	      switch (REGNO (op))
		{
		case G1_REG:
		  pj_printf ("%*read_global2");
		  pj_printf ("%*read_global1");
		  break;

		  /* A 64 bit read of global0 gives global0 and
		     optop.  */
		case G0_REG:
		  pj_printf ("%*read_optop");
		  pj_printf ("%*read_global0");
		  break;

		default:
		  abort ();
		}
	    break;
	  default:
	    abort ();
	  }
	break;

      case CONST_DOUBLE:
	pj_printf (pj_standard_float_constant (op));
	break;

      case CONST_INT:
	if (mode == SImode || mode == HImode || mode == QImode)
	  pj_output_push_int (INTVAL (op));
	else if (mode == DImode)
	  {
	    int v = INTVAL (op);
	    if (v == 1)
	      pj_printf ("%*lconst_1", 0);
	    else if (v == 0)
	      pj_printf ("%*lconst_0", 0);
	    else
	      {
		rtx hi = GEN_INT (v < 0 ? -1 : 0);
		rtx lo = op;
		pj_output_rval (TARGET_LITTLE_ENDIAN ? hi : lo, SImode, op);
		pj_output_rval (TARGET_LITTLE_ENDIAN ? lo : hi, SImode, op);
	      }
	  }
	else
	  abort ();
	break;

      case FLOAT_TRUNCATE:
	pj_printf ("%S0%*d2f", XEXP (op, 0));
	break;
      case LABEL_REF:
	pj_printf ("%*ipush %X", XEXP (op, 0));
	break;

      case SUBREG:
	pj_output_rval (alter_subreg (op), mode, outer_op);
	break;

      case POST_INC:
	pj_output_rval (XEXP (op, 0), mode, op);
	pj_output_inc (XEXP (op, 0), GET_MODE_SIZE (GET_MODE (outer_op)));
	break;

      case POST_DEC:
	pj_output_rval (XEXP (op, 0), mode, op);
	pj_output_inc (XEXP (op, 0), -GET_MODE_SIZE (GET_MODE (outer_op)));
	break;

      case PRE_INC:
	pj_output_inc (XEXP (op, 0), GET_MODE_SIZE (GET_MODE (outer_op)));
	pj_output_rval (XEXP (op, 0), mode, op);
	break;

      case PRE_DEC:
	if (OPTOP_REG_RTX_P (XEXP (op, 0)))
	  pj_output_rval (XEXP (op, 0), mode, op);
	else if (STACK_REG_RTX_P (XEXP (op, 0)))
	  {
	    pj_output_inc (XEXP (op, 0),
			   -GET_MODE_SIZE (GET_MODE (outer_op)));
	    pj_output_rval (XEXP (op, 0), mode, op);
	  }
	else
	  {
	    pj_printf ("%S0", XEXP (op, 0));
	    pj_output_print_add_k (-GET_MODE_SIZE (GET_MODE (outer_op)));
	    pj_printf ("%*dup%R0", XEXP (op, 0));
	  }
	break;

      case FIX:
	pj_output_cnv_op (fixtrunctab[GET_MODE (XEXP (op, 0))][mode][0], op);
	break;

      case FLOAT:
	if (mode == DFmode && GET_CODE (XEXP (op, 0)) == CONST_INT)
	  pj_output_cnv_op (floattab[mode][SImode][0], op);
	else
	  pj_output_cnv_op (floattab[mode][GET_MODE (XEXP (op, 0))][0], op);
	break;

      case FLOAT_EXTEND:
      case SIGN_EXTEND:
	/* Sign extending from a memop to register is automatic.  */
	if (mode == SImode && GET_CODE (XEXP (op, 0)) == MEM)
	  pj_output_rval (XEXP (op, 0), GET_MODE (XEXP (op, 0)), op);
	else
	  pj_output_cnv_op (extendtab[mode][GET_MODE (XEXP (op, 0))][0], op);
	break;

      case ZERO_EXTEND:
	pj_output_cnv_op (extendtab[mode][GET_MODE (XEXP (op, 0))][1], op);
	break;

      default:
	abort ();
	break;
      }
}

/* Store the top of stack into the lval operand OP.  */

void
pj_output_store_into_lval (mode, op)
     enum machine_mode mode;
     rtx op;
{
  if (GET_CODE (op) == REG)
    {
      int rn = REGNO (op);

      /* Outgoing values are left on the stack and not written
         anywhere.  */
      if (!OUTGOING_REG_RTX_P (op))
	{
	  switch (GET_MODE (op))
	    {
	    case SImode:
	    case QImode:
	    case HImode:
	    case SFmode:
	      if (pj_si_vars_offset_vec[rn] >= 0)
		pj_output_varidx (mode, 1, pj_si_vars_offset_vec[rn]);
	      else
		pj_printf ("%*write_%s", reg_names[rn]);
	      break;
	    case DImode:
	    case DFmode:
	      if (pj_di_vars_offset_vec[rn] >= 0)
		pj_output_varidx (mode, 1, pj_di_vars_offset_vec[rn]);
	      else
		switch (rn)
		  {
		  case G1_REG:
		    pj_printf ("%*write_global1");
		    pj_printf ("%*write_global2");
		    break;
		  default:
		    abort ();
		  }
	      break;
	    default:
	      abort ();
	    }
	}
    }
  else
    {
      pj_output_rval (XEXP (op, 0), Pmode, op);

      switch (GET_MODE_SIZE (mode))
	{
	case 1:
	  pj_printf ("%*store_byte", 0);
	  break;
	case 2:
	  pj_printf ("%*store_short", 0);
	  break;
	case 8:
	  if (TARGET_TM_EXTENSIONS)
	    {
	      pj_printf ("%*tm_store_long");
	      break;
	    }
	  /* Fall through.  */
	default:
	  {
	    int i;
	    for (i = GET_MODE_SIZE (mode); i > 4; i -= 4)
	      {
		pj_printf ("%*dup_x1", 0);
		pj_printf ("%*store_word", 0);
		pj_printf ("%*iconst_4", 0);
		pj_printf ("%*iadd", 0);
	      }
	  }
	  pj_printf ("%*store_word", 0);
	  break;
	}
    }
}

/* Print a condition, unsigned and signed have the same text because
   the unsigned operands have been run through icmp first.  */

static void
pj_print_cond (code)
     enum rtx_code code;
{
  switch (code)
    {
    case EQ:
      fputs ("eq", asm_out_file);
      break;
    case NE:
      fputs ("ne", asm_out_file);
      break;
    case GT:
    case GTU:
      fputs ("gt", asm_out_file);
      break;
    case GE:
    case GEU:
      fputs ("ge", asm_out_file);
      break;
    case LT:
    case LTU:
      fputs ("lt", asm_out_file);
      break;
    case LE:
    case LEU:
      fputs ("le", asm_out_file);
      break;
    default:
      abort ();
    }
}
/* Print operand X (an rtx) in assembler syntax to file STREAM
   according to modifier CODE.

   C  emit the first part of a Check_call pseudop. 
   D  emit operand, if no mode, assume DImode.
   E  emit the second part of a check_call pseudop. 
   I  print the XEXP (X, 0) Inside of the operand.
   J  print Just the integer or register part of an operand, for iinc.
   P  emit source is SI padded to DI with 0, used for unsigned mod and divide.
   R  emit the operand as an lval Result.
   S  emit Source operand, if no mode, assume SImode.
   X  nan choice suffix for floating point comparision.
   Y  condition name from op.
   Z  Y, reversed.
   *  marks start of opcode.  */

void
pj_print_operand (stream, x, code)
     FILE *stream;
     rtx x;
     int code;
{
  static int last_call_known;
  switch (code)
    {
    case 'C':
      if (GET_CODE (x) == SYMBOL_REF)
	{
	  last_call_known = 1;
	  pj_printf ("%*.check_call %0", x);
	}
      else
	last_call_known = 0;
      break;

    case 'D':
      pj_output_rval (x,
		      GET_MODE (x) == VOIDmode ? DImode : GET_MODE (x),
		      NULL_RTX);
      break;

    case 'E':
      if (last_call_known)
	pj_printf (",%d", INTVAL (x));
      break;

    case 'I':
      pj_output_rval (XEXP (x, 0), GET_MODE (XEXP (x, 0)), NULL_RTX);
      break;

    case 'J':
      if (GET_CODE (x) == CONST_INT)
	pj_printf ("%d", INTVAL (x));
      else if (GET_CODE (x) == REG)
	pj_printf ("%d", pj_si_vars_offset_vec[REGNO (x)]);
      else
	abort ();
      break;

    case 'P':
      if (TARGET_LITTLE_ENDIAN)
	pj_printf ("%*iconst_0", 0);
      pj_output_rval (x,
		      GET_MODE (x) == VOIDmode ? SImode : GET_MODE (x),
		      NULL_RTX);
      if (!TARGET_LITTLE_ENDIAN)
	pj_printf ("%*iconst_0", 0);
      break;

    case 'R':
      pj_output_store_into_lval (GET_MODE (x), x);
      break;

    case 'S':
      pj_output_rval (x,
		      GET_MODE (x) == VOIDmode ? SImode : GET_MODE (x),
		      NULL_RTX);
      break;

    case 'X':
      fputc (GET_CODE (x) == LT || GET_CODE (x) == LE ? 'g' : 'l', stream);
      break;

    case 'Y':
      pj_print_cond (GET_CODE (x));
      break;

    case 'Z':
      pj_print_cond (reverse_condition (GET_CODE (x)));
      break;

    case '*':
      pj_printf ("%*");
      break;

    default:
      output_addr_const (stream, x);
      break;
    }
}

/* Return in an rtx the number of words pushed onto the optop to be
   used as the word count in a call insn.  (NEXT_ARG_REG is NULL when
   called from expand_builtin_apply).  */

rtx
pj_workout_arg_words (stack_size, next_arg_reg)
     rtx stack_size ATTRIBUTE_UNUSED;
     rtx next_arg_reg;
{
  return GEN_INT ((next_arg_reg ? REGNO (next_arg_reg) - O0_REG : 0) + 2);
}

/* Handle the INCOMING_FUNCTION_ARG macro.
   Determine where to put an argument to a function.
   Value is zero to push the argument on the stack,
   or a hard register in which to store the argument.

   CUM is a variable of type CUMULATIVE_ARGS which gives info about
    the preceding args and about the function being called.
   MODE is the argument's machine mode.
   TYPE is the data type of the argument (as a tree).
    This is null for libcalls where that information may
    not be available.
   NAMED is nonzero if this argument is a named parameter
    (otherwise it is an extra parameter matching an ellipsis). */

rtx
pj_function_incoming_arg (cum, mode, passed_type, named_arg)
     CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree passed_type ATTRIBUTE_UNUSED;
     int named_arg ATTRIBUTE_UNUSED;
{
  int arg_words = PJ_ARG_WORDS (mode);

  /* If the whole argument will fit into registers, return the first
     register needed.  Also fill in the arg_adjust information so that
     we can work out the right offset to use when looking at the
     insides of a DI or DF value.  */

  if (cum->total_words + arg_words <= ARGS_IN_REGS)
    {
      int i;
      if (mode == DImode || mode == DFmode)
	{
	  cum->arg_adjust[cum->total_words + 0] = +1;
	  cum->arg_adjust[cum->total_words + 1] = -1;
	}
      else
	for (i = 0; i < arg_words; i++)
	  cum->arg_adjust[cum->total_words + i] = 0;

      return gen_rtx (REG, mode, I0_REG + cum->total_words);
    }
  return NULL_RTX;
}

/* Output code to add two SImode values.  Deals carefully with the the common
   case of moving the optop.  */

char *
pj_output_addsi3 (operands)
     rtx *operands;
{
  if (OPTOP_REG_RTX_P (operands[0]) && OPTOP_REG_RTX_P (operands[1])
      && GET_CODE (operands[2]) == CONST_INT
      && INTVAL (operands[2]) >= -32 && INTVAL (operands[2]) <= 32)
    {
      static struct
      {
	const char *two;
	const char *one;
      }
      name[2] =
      {
	{ "pop2", "pop"},
	{ "lconst_0", "iconst_0"}
      };
      int size = INTVAL (operands[2]);
      int d = 0;

      if (size < 0)
	{
	  d = 1;
	  size = -size;
	}

      for (; size >= 8; size -= 8)
	output_asm_insn (name[d].two, 0);


      if (size > 0)
	output_asm_insn (name[d].one, 0);

      return "";
    }

  if (STACK_REG_RTX_P (operands[0])
      && rtx_equal_p (operands[0], operands[1])
      && GET_CODE (operands[2]) == CONST_INT
      && INTVAL (operands[2]) >= -128 && INTVAL (operands[2]) <= 127)
    {
      return "iinc %J0,%J2";
    }

  return "%S1%S2%*iadd%R0";
}

/* Generate rtl for the prologue of the current function.  */

void
pj_expand_prologue ()
{
  int i;
  int off = 0;
  int arg_words = current_function->args_info.named_words;

  memset (pj_si_vars_offset_vec, -1, sizeof (pj_si_vars_offset_vec));
  memset (pj_di_vars_offset_vec, -1, sizeof (pj_di_vars_offset_vec));

  /* Work out the register numbers of the named arguments.  */
  for (i = 0; i < current_function->args_info.named_words; i++)
    {
      pj_debugreg_renumber_vec[I0_REG + i]
	= off + R0_REG + current_function->args_info.arg_adjust[i];
      pj_si_vars_offset_vec[I0_REG + i]
	= off + current_function->args_info.arg_adjust[i];
      pj_di_vars_offset_vec[I0_REG + i] = off;
      off++;
    }

  if (current_function_varargs || current_function_stdarg)
    {
      /* If the function is varadic we need to call the vhelper
         function.  vhelper pops off the unnamed argument words from
         the opstack and puts them onto the the aggregate stack.  The
         unnamed words are replacedwith two extra arguments, a pointer
         to the aggreagate stack for the first vararg and the original
         global0 value.  */

      emit_insn (gen_varargs (GEN_INT (arg_words * 4)));
      pj_si_vars_offset_vec[VA_REG] = off++;
      off++;
      arg_words += 2;
    }

  /* Skip over the return pc and old vars in the frame.  */
  off += 2;

  /* Work out the register numbers and offsets from the var pointer
     for the normal registers.  */
  nfakes = 0;

  for (i = LAST_I_REG; i >= R0_REG; i--)
    if (regs_ever_live[i] && pj_si_vars_offset_vec[i] == -1)
      {
	nfakes++;
	pj_si_vars_offset_vec[i] = off;
	pj_di_vars_offset_vec[i] = off - 1;
	pj_debugreg_renumber_vec[i] = off + R0_REG;
	off++;
      }

  if (TARGET_TEST)
    {
      fprintf (asm_out_file, "\n\t! args %d, size %d, fakes %d\n", 
	       arg_words,
	       get_frame_size () / 4,
	       nfakes);

      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	if (pj_si_vars_offset_vec[i] >= 0)
	  fprintf (asm_out_file, "\t!vars - %d %d: %s\n",
		   pj_si_vars_offset_vec[i],
		   pj_di_vars_offset_vec[i], 
		   reg_names[i]);
    }

  /* Make room on the opstack for the fake registers.  */
  if (TARGET_TM_EXTENSIONS)
    RTX_FRAME_RELATED_P (emit_insn (gen_tm_frame (GEN_INT (arg_words),
						  GEN_INT (nfakes)))) = 1;
  else
    RTX_FRAME_RELATED_P (emit_insn
			 (gen_addsi3
			  (gen_rtx_REG (SImode, OPTOP_REG),
			   gen_rtx_REG (SImode, OPTOP_REG),
			   GEN_INT (-nfakes * 4)))) = 1;


  if (frame_pointer_needed)
      emit_move_insn (frame_pointer_rtx, stack_pointer_rtx);

  if (get_frame_size ())
    RTX_FRAME_RELATED_P (emit_insn (gen_addsi3 (stack_pointer_rtx,
						stack_pointer_rtx,
						GEN_INT
						(-get_frame_size ())))) = 1;

  emit_insn (gen_rtx_USE (VOIDmode, gen_rtx_REG (SImode, OPTOP_REG)));
}

/* Generate rtl for the epilogue of the current function.  */

void
pj_expand_epilogue ()
{
  if (frame_pointer_needed)
    emit_move_insn (stack_pointer_rtx, frame_pointer_rtx);
  else if (get_frame_size ())
    emit_insn (gen_addsi3 (stack_pointer_rtx,
			   stack_pointer_rtx, GEN_INT (get_frame_size ())));
  if (nfakes)
    emit_insn (gen_addsi3 (gen_rtx_REG (SImode, OPTOP_REG),
			   gen_rtx_REG (SImode, OPTOP_REG),
			   GEN_INT (nfakes * 4)));


  /* If this is a varargs function, then global0 is stashed away on
     the top of the optop stack as the last secret argument by the
     __vhelper.  Pop off the va pointer provided too.  */

  if (current_function_varargs || current_function_stdarg)
    emit_insn (gen_varargs_finish
	       (GEN_INT (current_function->args_info.named_words + 1)));

  emit_insn (gen_rtx_USE (VOIDmode, gen_rtx_REG (SImode, OPTOP_REG)));
}

/* Return the opcode name for an instruction to load a standard
   floating point constant, or NULL.  */

const char *
pj_standard_float_constant (op)
     rtx op;
{
  REAL_VALUE_TYPE r;
  enum machine_mode mode = GET_MODE (op);

  if (GET_CODE (op) != CONST_DOUBLE || (mode != DFmode && mode != SFmode))
    return NULL;

  REAL_VALUE_FROM_CONST_DOUBLE (r, op);

  if (REAL_VALUES_EQUAL (r, dconst0) && !REAL_VALUE_MINUS_ZERO (r))
    return mode == DFmode ? "%*dconst_0" : "%*fconst_0";

  if (REAL_VALUES_EQUAL (r, dconst1))
    return mode == DFmode ? "%*dconst_1" : "%*fconst_1";

  if (REAL_VALUES_EQUAL (r, dconst2))
    return mode == DFmode ? 0 : "%*fconst_2";

  return NULL;
}

/* Read the value at the current address, and decrement by the size.
   The function is interesting because we're reading from high memory to low memory
   and have to adjust the addresses of reads of 8 byte values
   accordingly.  */

rtx
pj_expand_builtin_va_arg (valist, type)
     tree valist;
     tree type;
{
  tree addr_tree, t;
  HOST_WIDE_INT align;
  HOST_WIDE_INT rounded_size;
  rtx addr;

  /* Compute the rounded size of the type.  */
  align = PARM_BOUNDARY / BITS_PER_UNIT;
  rounded_size = (((int_size_in_bytes (type) + align - 1) / align) * align);

  /* Get AP.  */
  addr_tree = valist;
  addr = expand_expr (addr_tree, NULL_RTX, Pmode, EXPAND_NORMAL);
  addr = copy_to_reg (addr);

  /* Aggregates and large scalars are passed by reference.  */
  if (AGGREGATE_TYPE_P (type) || rounded_size > 8)
    {
      addr = gen_rtx_MEM (Pmode, addr);
      rounded_size = 4;
    }

  /* adjust address to cope with double word sizes */
  if (rounded_size > 4)
    addr = gen_rtx_PLUS (Pmode, addr, GEN_INT (-4));

  /* Compute new value for AP; AP = AP - SIZE */
  t = build (MODIFY_EXPR, TREE_TYPE (valist), valist,
	     build (MINUS_EXPR, TREE_TYPE (valist), valist,
		    build_int_2 (rounded_size, 0)));

  TREE_SIDE_EFFECTS (t) = 1;

  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  return addr;
}

/* Return nonzero if the operand is valid as a source operand; it's
   general and it's not an outgoing argument register.  */

int
pj_source_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return !OUTGOING_REG_RTX_P (op) && general_operand (op, mode);
}

/* Return nonzero if the operator is a signed compare.  */

int
pj_signed_comparison_operator (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (mode != GET_MODE (op))
    return 0;

  switch (GET_CODE (op))
    {
    case EQ:
    case NE:
    case LE:
    case LT:
    case GE:
    case GT:
      return 1;
    default:
      return 0;
    }
}

/* Return nonzero if the operator is an unsigned compare.  */

int
pj_unsigned_comparison_operator (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (mode != GET_MODE (op))
    return 0;

  switch (GET_CODE (op))
    {
    case GTU:
    case GEU:
    case LTU:
    case LEU:
      return 1;
    default:
      return 0;
    }
}

/* Helper function for pj_machine_dependent_reorg.  Find the one
   instance of register OP in the source part of PAT.  If there are no
   copies return NULL, if there are more than one, return NOT_UNIQUE.  */

#define NOT_UNIQUE (&const0_rtx)

static rtx *
unique_src_operand (pat, reg)
     rtx *pat;
     rtx reg;
{
  register rtx *result = 0;
  register const char *fmt;
  register int i;
  register int j;

  if (GET_CODE (*pat) == SET)
    {
      if (GET_CODE (XEXP (*pat, 0)) == MEM)
	result = unique_src_operand (&XEXP (SET_DEST (*pat), 0), reg);
      pat = &SET_SRC (*pat);
    }

  if (GET_CODE (*pat) == REG && REGNO (*pat) == REGNO (reg))
    return pat;

  fmt = GET_RTX_FORMAT (GET_CODE (*pat));
  for (i = GET_RTX_LENGTH (GET_CODE (*pat)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  rtx *new_result = unique_src_operand (&XEXP (*pat, i), reg);

	  if (new_result)
	    {
	      if (result)
		return NOT_UNIQUE;
	      result = new_result;
	    }
	}
      else if (fmt[i] == 'E')
	{
	  for (j = XVECLEN (*pat, i) - 1; j >= 0; j--)
	    {
	      rtx *new_result =
		unique_src_operand (&XVECEXP (*pat, i, j), reg);

	      if (new_result)
		{
		  if (result)
		    return NOT_UNIQUE;
		  result = new_result;
		}
	    }
	}
    }
  return result;
}

/* Clean up the instructions to remove unneeded loads and stores.

   For example, rewrite

   iload a; iload b; iadd; istore z
   iload z; iload c; iadd; istore z

   as

   iload a; iload b; iadd ; iload c; iadd; istore z

   This function moves a cursor over each instruction, inspecting the
   LOG_LINKS.  Each of the cursor's LOG_LINK incoming instructions are
   inspected, any which have a simple register destination which is
   also used as a source in the cursor instruction, and aren't used
   again between the the incoming instruction and the cursor, and
   which become dead or set after the cursor get their sources
   substituted into the position of the source register in the cursor
   instruction.  */

void
pj_machine_dependent_reorg (insns)
     rtx insns;
{
  rtx cursor;

  if (!optimize || !TARGET_REORG)
    return;

  for (cursor = insns; cursor; cursor = NEXT_INSN (cursor))
    {
      rtx links;
      rtx cursor_pat;

      /* We only care about INSNs, JUMP_INSNs. Ignore any special USE insns.  */

      if ((GET_CODE (cursor) != INSN && GET_CODE (cursor) != JUMP_INSN)
	  || GET_CODE (cursor_pat = PATTERN (cursor)) == USE
	  || GET_CODE (cursor_pat) == CLOBBER
	  || GET_CODE (cursor_pat) == ADDR_VEC
	  || GET_CODE (cursor_pat) == ADDR_DIFF_VEC)
	continue;

      for (links = LOG_LINKS (cursor); links; links = XEXP (links, 1))
	{
	  rtx prev = XEXP (links, 0);
	  rtx prev_pat;
	  rtx prev_dest;
	  rtx prev_src;
	  rtx *dst_place;

	  if (GET_CODE (prev) == INSN
	      && GET_CODE (prev_pat = PATTERN (prev)) == SET
	      && GET_CODE (prev_dest = SET_DEST (prev_pat)) == REG
	      && dead_or_set_p (cursor, prev_dest)
	      && !reg_used_between_p (prev_dest, prev, cursor)
	      && no_labels_between_p (prev, cursor)
	      && no_jumps_between_p (prev, cursor)
	      && !modified_between_p ((prev_src = SET_SRC (prev_pat)), prev,
				      cursor)
	      && (dst_place = unique_src_operand (&cursor_pat, prev_dest))
	      && dst_place != NOT_UNIQUE
	      && REGNO (prev_dest) != OPTOP_REG
	      && GET_MODE (prev_dest) != XFmode
	      && GET_MODE (*dst_place) == GET_MODE (SET_DEST (prev_pat)))
	    {
	      *dst_place = SET_SRC (prev_pat);
	      PUT_CODE (prev, NOTE);
	      NOTE_LINE_NUMBER (prev) = NOTE_INSN_DELETED;
	    }
	}
    }
}
