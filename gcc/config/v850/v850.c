/* Subroutines for insn-output.c for NEC V850 series
   Copyright (C) 1996, 1997 Free Software Foundation, Inc.
   Contributed by Jeff Law (law@cygnus.com).

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

#include <stdio.h>
#include <ctype.h>
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
#include "recog.h"
#include "expr.h"
#include "tree.h"
#include "obstack.h"

/* True if the current function has anonymous arguments.  */
int current_function_anonymous_args;

/* Information about the various small memory areas.  */
struct small_memory_info small_memory[ (int)SMALL_MEMORY_max ] =
{
  /* name	value		max		physical max */
  { "tda",	(char *)0,	0,		256 },
  { "sda",	(char *)0,	0,		65536 },
  { "zda",	(char *)0,	0,		32768 },
};

/* True if we don't need to check any more if the current
   function is an interrupt handler */
static int v850_interrupt_cache_p = FALSE;

/* Whether current function is an interrupt handler.  */
static int v850_interrupt_p = FALSE;


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
  int i;
  extern int atoi ();

  /* Parse -m{s,t,z}da=nnn switches */
  for (i = 0; i < (int)SMALL_MEMORY_max; i++)
    {
      if (small_memory[i].value)
	{
	  if (!isdigit (*small_memory[i].value))
	    error ("%s=%s is not numeric.",
		   small_memory[i].name,
		   small_memory[i].value);
	  else
	    {
	      small_memory[i].max = atoi (small_memory[i].value);
	      if (small_memory[i].max > small_memory[i].physical_max)
		error ("%s=%s is too large.",
		   small_memory[i].name,
		   small_memory[i].value);
	    }
	}
    }
}


/* Output assembly code for the start of the file.  */

void
asm_file_start (file)
     FILE *file;
{
  output_file_directive (file, main_input_filename);
}


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
  int size, align;

  if (TARGET_GHS && !named)
    return NULL_RTX;

  if (mode == BLKmode)
    size = int_size_in_bytes (type);
  else
    size = GET_MODE_SIZE (mode);

  if (type)
    align = TYPE_ALIGN (type) / BITS_PER_UNIT;
  else
    align = size;

  cum->nbytes = (cum->nbytes + align - 1) &~(align - 1);

  if (cum->nbytes > 4 * UNITS_PER_WORD)
    return 0;

  if (type == NULL_TREE
      && cum->nbytes + size > 4 * UNITS_PER_WORD)
    return 0;

  switch (cum->nbytes / UNITS_PER_WORD)
    {
    case 0:
      result = gen_rtx (REG, mode, 6);
      break;
    case 1:
      result = gen_rtx (REG, mode, 7);
      break;
    case 2:
      result = gen_rtx (REG, mode, 8);
      break;
    case 3:
      result = gen_rtx (REG, mode, 9);
      break;
    default:
      result = 0;
    }

  return result;
}


/* Return the number of words which must be put into registers
   for values which are part in registers and part in memory.  */

int
function_arg_partial_nregs (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int named;
{
  int size, align;

  if (TARGET_GHS && !named)
    return 0;

  if (mode == BLKmode)
    size = int_size_in_bytes (type);
  else
    size = GET_MODE_SIZE (mode);

  if (type)
    align = TYPE_ALIGN (type) / BITS_PER_UNIT;
  else
    align = size;

  cum->nbytes = (cum->nbytes + align - 1) &~(align - 1);

  if (cum->nbytes > 4 * UNITS_PER_WORD)
    return 0;

  if (cum->nbytes + size <= 4 * UNITS_PER_WORD)
    return 0;

  if (type == NULL_TREE
      && cum->nbytes + size > 4 * UNITS_PER_WORD)
    return 0;

  return (4 * UNITS_PER_WORD - cum->nbytes) / UNITS_PER_WORD;
}


/* Return the high and low words of a CONST_DOUBLE */

static void
const_double_split (x, p_high, p_low)
     rtx x;
     HOST_WIDE_INT *p_high;
     HOST_WIDE_INT *p_low;
{
  if (GET_CODE (x) == CONST_DOUBLE)
    {
      long t[2];
      REAL_VALUE_TYPE rv;

      switch (GET_MODE (x))
	{
	case DFmode:
	  REAL_VALUE_FROM_CONST_DOUBLE (rv, x);
	  REAL_VALUE_TO_TARGET_DOUBLE (rv, t);
	  *p_high = t[1];	/* since v850 is little endian */
	  *p_low = t[0];	/* high is second word */
	  return;

	case SFmode:
	  REAL_VALUE_FROM_CONST_DOUBLE (rv, x);
	  REAL_VALUE_TO_TARGET_SINGLE (rv, *p_high);
	  *p_low = 0;
	  return;

	case VOIDmode:
	case DImode:
	  *p_high = CONST_DOUBLE_HIGH (x);
	  *p_low  = CONST_DOUBLE_LOW (x);
	  return;
	}
    }

  fatal_insn ("const_double_split got a bad insn:", x);
}


/* Return the cost of the rtx R with code CODE.  */

static int
const_costs_int (value, zero_cost)
     HOST_WIDE_INT value;
     int zero_cost;
{
  if (CONST_OK_FOR_I (value))
      return zero_cost;
  else if (CONST_OK_FOR_J (value))
    return 1;
  else if (CONST_OK_FOR_K (value))
    return 2;
  else
    return 4;
}

int
const_costs (r, c)
     rtx r;
     enum rtx_code c;
{
  HOST_WIDE_INT high, low;

  switch (c)
    {
    case CONST_INT:
      return const_costs_int (INTVAL (r), 0);

    case CONST_DOUBLE:
      const_double_split (r, &high, &low);
      if (GET_MODE (r) == SFmode)
	return const_costs_int (high, 1);
      else
	return const_costs_int (high, 1) + const_costs_int (low, 1);

    case SYMBOL_REF:
    case LABEL_REF:
    case CONST:
      return 2;

    case HIGH:
      return 1;

    default:
      return 4;
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
  HOST_WIDE_INT high, low;

  switch (code)
    {
    case 'b':
    case 'B':
    case 'c':
    case 'C':
      switch ((code == 'B' || code == 'C')
	      ? reverse_condition (GET_CODE (x)) : GET_CODE (x))
	{
	  case NE:
	    if (code == 'c' || code == 'C')
	      fprintf (file, "nz");
	    else
	      fprintf (file, "ne");
	    break;
	  case EQ:
	    if (code == 'c' || code == 'C')
	      fprintf (file, "z");
	    else
	      fprintf (file, "e");
	    break;
	  case GE:
	    fprintf (file, "ge");
	    break;
	  case GT:
	    fprintf (file, "gt");
	    break;
	  case LE:
	    fprintf (file, "le");
	    break;
	  case LT:
	    fprintf (file, "lt");
	    break;
	  case GEU:
	    fprintf (file, "nl");
	    break;
	  case GTU:
	    fprintf (file, "h");
	    break;
	  case LEU:
	    fprintf (file, "nh");
	    break;
	  case LTU:
	    fprintf (file, "l");
	    break;
	  default:
	    abort ();
	}
      break;
    case 'F':			/* high word of CONST_DOUBLE */
      if (GET_CODE (x) == CONST_INT)
	fprintf (file, "%d", (INTVAL (x) >= 0) ? 0 : -1);
      else if (GET_CODE (x) == CONST_DOUBLE)
	{
	  const_double_split (x, &high, &low);
	  fprintf (file, "%ld", (long) high);
	}
      else
	abort ();
      break;
    case 'G':			/* low word of CONST_DOUBLE */
      if (GET_CODE (x) == CONST_INT)
	fprintf (file, "%ld", (long) INTVAL (x));
      else if (GET_CODE (x) == CONST_DOUBLE)
	{
	  const_double_split (x, &high, &low);
	  fprintf (file, "%ld", (long) low);
	}
      else
	abort ();
      break;
    case 'L':
      fprintf (file, "%d\n", INTVAL (x) & 0xffff);
      break;
    case 'M':
      fprintf (file, "%d", exact_log2 (INTVAL (x)));
      break;
    case 'O':
      if (special_symbolref_operand (x, VOIDmode))
        {
          char* name;

	  if (GET_CODE (x) == SYMBOL_REF)
	    name = XSTR (x, 0);
	  else if (GET_CODE (x) == CONST)
	    name = XSTR (XEXP (XEXP (x, 0), 0), 0);
	  else
	    abort ();

          if (ZDA_NAME_P (name))
            fprintf (file, "zdaoff");
          else if (SDA_NAME_P (name))
            fprintf (file, "sdaoff");
          else if (TDA_NAME_P (name))
            fprintf (file, "tdaoff");
          else
            abort();
        }
      else
        abort();
      break;
    case 'P':
      if (special_symbolref_operand (x, VOIDmode))
        output_addr_const (file, x);
      else
        abort();
      break;
    case 'Q':
      if (special_symbolref_operand (x, VOIDmode))
        {
          char* name;

	  if (GET_CODE (x) == SYMBOL_REF)
	    name = XSTR (x, 0);
	  else if (GET_CODE (x) == CONST)
	    name = XSTR (XEXP (XEXP (x, 0), 0), 0);
	  else
	    abort ();

          if (ZDA_NAME_P (name))
            fprintf (file, "r0");
          else if (SDA_NAME_P (name))
            fprintf (file, "gp");
          else if (TDA_NAME_P (name))
            fprintf (file, "ep");
          else
            abort();
        }
      else
        abort();
      break;
    case 'R':		/* 2nd word of a double.  */
      switch (GET_CODE (x))
	{
	  case REG:
	    fprintf (file, reg_names[REGNO (x) + 1]);
	    break;
	  case MEM:
	    print_operand_address (file,
				   XEXP (adj_offsettable_operand (x, 4), 0));
	    break;
	}
      break;
    case 'S':
      {
        /* if it's a referance to a TDA variable, use sst/sld vs. st/ld */
        if (GET_CODE (x) == MEM && ep_memory_operand (x, GET_MODE (x), FALSE))
          fputs ("s", file);

        break;
      }
    case 'T':
      {
	/* Like an 'S' operand above, but for unsigned loads only.  */
        if (GET_CODE (x) == MEM && ep_memory_operand (x, GET_MODE (x), TRUE))
          fputs ("s", file);

        break;
      }
    case 'W':			/* print the instruction suffix */
      switch (GET_MODE (x))
	{
	default:
	  abort ();

	case QImode: fputs (".b", file); break;
	case HImode: fputs (".h", file); break;
	case SImode: fputs (".w", file); break;
	case SFmode: fputs (".w", file); break;
	}
      break;
    case '.':			/* register r0 */
      fputs (reg_names[0], file);
      break;
    case 'z':			/* reg or zero */
      if (x == const0_rtx)
	fputs (reg_names[0], file);
      else if (GET_CODE (x) == REG)
	fputs (reg_names[REGNO (x)], file);
      else
	abort ();
      break;
    default:
      switch (GET_CODE (x))
	{
	case MEM:
	  if (GET_CODE (XEXP (x, 0)) == CONST_INT)
	    output_address (gen_rtx (PLUS, SImode,
				     gen_rtx (REG, SImode, 0),
				     XEXP (x, 0)));
	  else
	    output_address (XEXP (x, 0));
	  break;

	case REG:
	  fputs (reg_names[REGNO (x)], file);
	  break;
	case SUBREG:
	  fputs (reg_names[REGNO (SUBREG_REG (x)) + SUBREG_WORD (x)], file);
	  break;
	case CONST_INT:
	case SYMBOL_REF:
	case CONST:
	case LABEL_REF:
	case CODE_LABEL:
	  print_operand_address (file, x);
	  break;
	default:
	  abort ();
	}
      break;

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
      fprintf (file, "0[");
      print_operand (file, addr, 0);
      fprintf (file, "]");
      break;
    case LO_SUM:
      if (GET_CODE (XEXP (addr, 0)) == REG)
	{
	  /* reg,foo */
	  fprintf (file, "lo(");
	  print_operand (file, XEXP (addr, 1), 0);
	  fprintf (file, ")[");
	  print_operand (file, XEXP (addr, 0), 0);
	  fprintf (file, "]");
	}
      break;
    case PLUS:
      if (GET_CODE (XEXP (addr, 0)) == REG
	  || GET_CODE (XEXP (addr, 0)) == SUBREG)
	{
	  /* reg,foo */
	  print_operand (file, XEXP (addr, 1), 0);
	  fprintf (file, "[");
	  print_operand (file, XEXP (addr, 0), 0);
	  fprintf (file, "]");
	}
      else
	{
	  print_operand (file, XEXP (addr, 0), 0);
	  fprintf (file, "+");
	  print_operand (file, XEXP (addr, 1), 0);
	}
      break;
    case SYMBOL_REF:
      if (ENCODED_NAME_P (XSTR (addr, 0)))
        {
          char* name = XSTR (addr, 0);
          char* off_name;
          char* reg_name;

          if (ZDA_NAME_P (name))
            {
              off_name = "zdaoff";
              reg_name = "r0";
            }
          else if (SDA_NAME_P (name))
            {
              off_name = "sdaoff";
              reg_name = "gp";
            }
          else if (TDA_NAME_P (name))
            {
              off_name = "tdaoff";
              reg_name = "ep";
            }
          else
            abort();

          fprintf (file, "%s(", off_name);
          output_addr_const (file, addr);
          fprintf (file, ")[%s]", reg_name);
        }
      else
        output_addr_const (file, addr);
      break;
    case CONST:
      if (special_symbolref_operand (addr, VOIDmode))
        {
          char* name = XSTR (XEXP (XEXP (addr, 0), 0), 0);
          char* off_name;
          char* reg_name;

          if (ZDA_NAME_P (name))
            {
              off_name = "zdaoff";
              reg_name = "r0";
            }
          else if (SDA_NAME_P (name))
            {
              off_name = "sdaoff";
              reg_name = "gp";
            }
          else if (TDA_NAME_P (name))
            {
              off_name = "tdaoff";
              reg_name = "ep";
            }
          else
            abort();

          fprintf (file, "%s(", off_name);
          output_addr_const (file, addr);
          fprintf (file, ")[%s]", reg_name);
        }
      else
        output_addr_const (file, addr);
      break;
    default:
      output_addr_const (file, addr);
      break;
    }
}


/* Return appropriate code to load up a 1, 2, or 4 integer/floating
   point value.  */

char *
output_move_single (operands)
     rtx *operands;
{
  rtx dst = operands[0];
  rtx src = operands[1];

  if (REG_P (dst))
    {
      if (REG_P (src))
	return "mov %1,%0";

      else if (GET_CODE (src) == CONST_INT)
	{
	  HOST_WIDE_INT value = INTVAL (src);

	  if (CONST_OK_FOR_J (value))		/* signed 5 bit immediate */
	    return "mov %1,%0";

	  else if (CONST_OK_FOR_K (value))	/* signed 16 bit immediate */
	    return "movea lo(%1),%.,%0";

	  else if (CONST_OK_FOR_L (value))	/* upper 16 bits were set */
	    return "movhi hi(%1),%.,%0";

	  else					/* random constant */
	    return "movhi hi(%1),%.,%0\n\tmovea lo(%1),%0,%0";
	}

      else if (GET_CODE (src) == CONST_DOUBLE && GET_MODE (src) == SFmode)
	{
	  HOST_WIDE_INT high, low;

	  const_double_split (src, &high, &low);
	  if (CONST_OK_FOR_J (high))		/* signed 5 bit immediate */
	    return "mov %F1,%0";

	  else if (CONST_OK_FOR_K (high))	/* signed 16 bit immediate */
	    return "movea lo(%F1),%.,%0";

	  else if (CONST_OK_FOR_L (high))	/* upper 16 bits were set */
	    return "movhi hi(%F1),%.,%0";

	  else					/* random constant */
	    return "movhi hi(%F1),%.,%0\n\tmovea lo(%F1),%0,%0";
	}

      else if (GET_CODE (src) == MEM)
	return "%S1ld%W1 %1,%0";

      else if (special_symbolref_operand (src, VOIDmode))
	return "movea %O1(%P1),%Q1,%0";

      else if (GET_CODE (src) == LABEL_REF
	       || GET_CODE (src) == SYMBOL_REF
	       || GET_CODE (src) == CONST)
	{
	  return "movhi hi(%1),%.,%0\n\tmovea lo(%1),%0,%0";
	}

      else if (GET_CODE (src) == HIGH)
	return "movhi hi(%1),%.,%0";

      else if (GET_CODE (src) == LO_SUM)
	{
	  operands[2] = XEXP (src, 0);
	  operands[3] = XEXP (src, 1);
	  return "movea lo(%3),%2,%0";
	}
    }

  else if (GET_CODE (dst) == MEM)
    {
      if (REG_P (src))
	return "%S0st%W0 %1,%0";

      else if (GET_CODE (src) == CONST_INT && INTVAL (src) == 0)
	return "%S0st%W0 %.,%0";

      else if (GET_CODE (src) == CONST_DOUBLE
	       && CONST0_RTX (GET_MODE (dst)) == src)
	return "%S0st%W0 %.,%0";
    }

  fatal_insn ("output_move_single:", gen_rtx (SET, VOIDmode, dst, src));
  return "";
}


/* Return appropriate code to load up an 8 byte integer or floating point value */

char *
output_move_double (operands)
    rtx *operands;
{
  enum machine_mode mode = GET_MODE (operands[0]);
  rtx dst = operands[0];
  rtx src = operands[1];

  if (register_operand (dst, mode)
      && register_operand (src, mode))
    {
      if (REGNO (src) + 1 == REGNO (dst))
	return "mov %R1,%R0\n\tmov %1,%0";
      else
	return "mov %1,%0\n\tmov %R1,%R0";
    }

  /* Storing 0 */
  if (GET_CODE (dst) == MEM
      && ((GET_CODE (src) == CONST_INT && INTVAL (src) == 0)
	  || (GET_CODE (src) == CONST_DOUBLE && CONST_DOUBLE_OK_FOR_G (src))))
    return "st.w %.,%0\n\tst.w %.,%R0";

  if (GET_CODE (src) == CONST_INT || GET_CODE (src) == CONST_DOUBLE)
    {
      HOST_WIDE_INT high_low[2];
      int i;
      rtx xop[10];

      if (GET_CODE (src) == CONST_DOUBLE)
	const_double_split (src, &high_low[1], &high_low[0]);
      else
	{
	  high_low[0] = INTVAL (src);
	  high_low[1] = (INTVAL (src) >= 0) ? 0 : -1;
	}

      for (i = 0; i < 2; i++)
	{
	  xop[0] = gen_rtx (REG, SImode, REGNO (dst)+i);
	  xop[1] = GEN_INT (high_low[i]);
	  output_asm_insn (output_move_single (xop), xop);
	}

      return "";
    }

  if (GET_CODE (src) == MEM)
    {
      int ptrreg = -1;
      int dreg = REGNO (dst);
      rtx inside = XEXP (src, 0);

      if (GET_CODE (inside) == REG)
 	ptrreg = REGNO (inside);
      else if (GET_CODE (inside) == SUBREG)
	ptrreg = REGNO (SUBREG_REG (inside)) + SUBREG_WORD (inside);
      else if (GET_CODE (inside) == PLUS)
	ptrreg = REGNO (XEXP (inside, 0));
      else if (GET_CODE (inside) == LO_SUM)
	ptrreg = REGNO (XEXP (inside, 0));

      if (dreg == ptrreg)
	return "ld.w %R1,%R0\n\tld.w %1,%0";
    }

  if (GET_CODE (src) == MEM)
    return "ld.w %1,%0\n\tld.w %R1,%R0";
  
  if (GET_CODE (dst) == MEM)
    return "st.w %1,%0\n\tst.w %R1,%R0";

  return "mov %1,%0\n\tmov %R1,%R0";
}


/* Return maximum offset supported for a short EP memory reference of mode
   MODE and signedness UNSIGNEDP.  */

int
ep_memory_offset (mode, unsignedp)
     enum machine_mode mode;
     int unsignedp;
{
  int max_offset = 0;

  switch (mode)
    {
    case QImode:
      max_offset = (1 << 7);
      break;

    case HImode:
      max_offset = (1 << 8);
      break;

    case SImode:
    case SFmode:
      max_offset = (1 << 8);
      break;
    }

  return max_offset;
}

/* Return true if OP is a valid short EP memory reference */

int
ep_memory_operand (op, mode, unsigned_load)
     rtx op;
     enum machine_mode mode;
     int unsigned_load;
{
  rtx addr, op0, op1;
  int max_offset;
  int mask;

  if (GET_CODE (op) != MEM)
    return FALSE;

  max_offset = ep_memory_offset (mode, unsigned_load);

  mask = GET_MODE_SIZE (mode) - 1;

  addr = XEXP (op, 0);
  if (GET_CODE (addr) == CONST)
    addr = XEXP (addr, 0);

  switch (GET_CODE (addr))
    {
    default:
      break;

    case SYMBOL_REF:
      return TDA_NAME_P (XSTR (addr, 0));

    case REG:
      return REGNO (addr) == EP_REGNUM;

    case PLUS:
      op0 = XEXP (addr, 0);
      op1 = XEXP (addr, 1);
      if (GET_CODE (op1) == CONST_INT
	  && INTVAL (op1) < max_offset
	  && (INTVAL (op1) & mask) == 0)
	{
	  if (GET_CODE (op0) == REG && REGNO (op0) == EP_REGNUM)
	    return TRUE;

	  if (GET_CODE (op0) == SYMBOL_REF && TDA_NAME_P (XSTR (op0, 0)))
	    return TRUE;
	}
      break;
    }

  return FALSE;
}

/* Return true if OP is either a register or 0 */

int
reg_or_0_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == CONST_INT)
    return INTVAL (op) == 0;

  else if (GET_CODE (op) == CONST_DOUBLE)
    return CONST_DOUBLE_OK_FOR_G (op);

  else
    return register_operand (op, mode);
}

/* Return true if OP is either a register or a signed five bit integer */

int
reg_or_int5_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == CONST_INT)
    return CONST_OK_FOR_J (INTVAL (op));

  else
    return register_operand (op, mode);
}

/* Return true if OP is a valid call operand.  */

int
call_address_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  /* Only registers are valid call operands if TARGET_LONG_CALLS.  */
  if (TARGET_LONG_CALLS)
    return GET_CODE (op) == REG;
  return (GET_CODE (op) == SYMBOL_REF || GET_CODE (op) == REG);
}

int
special_symbolref_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == SYMBOL_REF)
    return ENCODED_NAME_P (XSTR (op, 0));

  else if (GET_CODE (op) == CONST)
    return (GET_CODE (XEXP (op, 0)) == PLUS
	    && GET_CODE (XEXP (XEXP (op, 0), 0)) == SYMBOL_REF
	    && ENCODED_NAME_P (XSTR (XEXP (XEXP (op, 0), 0), 0))
	    && GET_CODE (XEXP (XEXP (op, 0), 1)) == CONST_INT
	    && CONST_OK_FOR_K (INTVAL (XEXP (XEXP (op, 0), 1))));

  return FALSE;
}

int
movsi_source_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  /* Some constants, as well as symbolic operands
     must be done with HIGH & LO_SUM patterns.  */
  if (CONSTANT_P (op)
      && GET_CODE (op) != HIGH
      && !(GET_CODE (op) == CONST_INT
           && (CONST_OK_FOR_J (INTVAL (op))
               || CONST_OK_FOR_K (INTVAL (op))
               || CONST_OK_FOR_L (INTVAL (op)))))
    return special_symbolref_operand (op, mode);
  else
    return general_operand (op, mode);
}

int
power_of_two_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) != CONST_INT)
    return 0;

  if (exact_log2 (INTVAL (op)) == -1)
    return 0;
  return 1;
}

int
not_power_of_two_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  unsigned int mask;

  if (mode == QImode)
    mask = 0xff;
  else if (mode == HImode)
    mask = 0xffff;
  else if (mode == SImode)
    mask = 0xffffffff; 
  else
    return 0;

  if (GET_CODE (op) != CONST_INT)
    return 0;

  if (exact_log2 (~INTVAL (op) & mask) == -1)
    return 0;
  return 1;
}


/* Substitute memory references involving a pointer, to use the ep pointer,
   taking care to save and preserve the ep.  */

static void
substitute_ep_register (first_insn, last_insn, uses, regno, p_r1, p_ep)
     rtx first_insn;
     rtx last_insn;
     int uses;
     int regno;
     rtx *p_r1;
     rtx *p_ep;
{
  rtx reg = gen_rtx (REG, Pmode, regno);
  rtx insn;
  int i;

  if (!*p_r1)
    {
      regs_ever_live[1] = 1;
      *p_r1 = gen_rtx (REG, Pmode, 1);
      *p_ep = gen_rtx (REG, Pmode, 30);
    }

  if (TARGET_DEBUG)
    fprintf (stderr, "Saved %d bytes (%d uses of register %s) in function %s, starting as insn %d, ending at %d\n",
	     2 * (uses - 3), uses, reg_names[regno],
	     IDENTIFIER_POINTER (DECL_NAME (current_function_decl)),
	     INSN_UID (first_insn), INSN_UID (last_insn));

  if (GET_CODE (first_insn) == NOTE)
    first_insn = next_nonnote_insn (first_insn);

  last_insn = next_nonnote_insn (last_insn);
  for (insn = first_insn; insn && insn != last_insn; insn = NEXT_INSN (insn))
    {
      if (GET_CODE (insn) == INSN)
	{
	  rtx pattern = single_set (insn);

	  /* Replace the memory references.  */
	  if (pattern)
	    {
	      rtx *p_mem;
	      /* Memory operands are signed by default.  */
	      int unsignedp = FALSE;

	      if (GET_CODE (SET_DEST (pattern)) == MEM
		  && GET_CODE (SET_SRC (pattern)) == MEM)
		p_mem = (rtx *)0;

	      else if (GET_CODE (SET_DEST (pattern)) == MEM)
		p_mem = &SET_DEST (pattern);

	      else if (GET_CODE (SET_SRC (pattern)) == MEM)
		p_mem = &SET_SRC (pattern);

	      else
		p_mem = (rtx *)0;

	      if (p_mem)
		{
		  rtx addr = XEXP (*p_mem, 0);

		  if (GET_CODE (addr) == REG && REGNO (addr) == regno)
		    *p_mem = change_address (*p_mem, VOIDmode, *p_ep);

		  else if (GET_CODE (addr) == PLUS
			   && GET_CODE (XEXP (addr, 0)) == REG
			   && REGNO (XEXP (addr, 0)) == regno
			   && GET_CODE (XEXP (addr, 1)) == CONST_INT
			   && (((unsigned)INTVAL (XEXP (addr, 1)))
			       < ep_memory_offset (GET_MODE (*p_mem),
						   unsignedp)))
		    *p_mem = change_address (*p_mem, VOIDmode,
					     gen_rtx (PLUS, Pmode,
						      *p_ep, XEXP (addr, 1)));
		}
	    }
	}
    }

  /* Optimize back to back cases of ep <- r1 & r1 <- ep.  */
  insn = prev_nonnote_insn (first_insn);
  if (insn && GET_CODE (insn) == INSN
      && GET_CODE (PATTERN (insn)) == SET
      && SET_DEST (PATTERN (insn)) == *p_ep
      && SET_SRC (PATTERN (insn)) == *p_r1)
    delete_insn (insn);
  else
    emit_insn_before (gen_rtx (SET, Pmode, *p_r1, *p_ep), first_insn);

  emit_insn_before (gen_rtx (SET, Pmode, *p_ep, reg), first_insn);
  emit_insn_before (gen_rtx (SET, Pmode, *p_ep, *p_r1), last_insn);
}


/* In rare cases, correct code generation requires extra machine
   dependent processing between the second jump optimization pass and
   delayed branch scheduling.  On those machines, define this macro
   as a C statement to act on the code starting at INSN.

   On the 850, we use it to implement the -mep mode to copy heavily used
   pointers to ep to use the implicit addressing */

void v850_reorg (start_insn)
     rtx start_insn;
{
  struct {
    int uses;
    rtx first_insn;
    rtx last_insn;
  } regs[FIRST_PSEUDO_REGISTER];

  int i;
  int use_ep = FALSE;
  rtx r1 = NULL_RTX;
  rtx ep = NULL_RTX;
  rtx insn;
  rtx pattern;

  /* If not ep mode, just return now */
  if (!TARGET_EP)
    return;

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      regs[i].uses = 0;
      regs[i].first_insn = NULL_RTX;
      regs[i].last_insn = NULL_RTX;
    }

  for (insn = start_insn; insn != NULL_RTX; insn = NEXT_INSN (insn))
    {
      switch (GET_CODE (insn))
	{
	  /* End of basic block */
	default:
	  if (!use_ep)
	    {
	      int max_uses = -1;
	      int max_regno = -1;

	      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
		{
		  if (max_uses < regs[i].uses)
		    {
		      max_uses = regs[i].uses;
		      max_regno = i;
		    }
		}

	      if (max_uses > 3)
		substitute_ep_register (regs[max_regno].first_insn,
					regs[max_regno].last_insn,
					max_uses, max_regno, &r1, &ep);
	    }

	  use_ep = FALSE;
	  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	    {
	      regs[i].uses = 0;
	      regs[i].first_insn = NULL_RTX;
	      regs[i].last_insn = NULL_RTX;
	    }
	  break;

	case NOTE:
	  break;

	case INSN:
	  pattern = single_set (insn);

	  /* See if there are any memory references we can shorten */
	  if (pattern)
	    {
	      rtx src = SET_SRC (pattern);
	      rtx dest = SET_DEST (pattern);
	      rtx mem;
	      /* Memory operands are signed by default.  */
	      int unsignedp = FALSE;

	      if (GET_CODE (dest) == MEM && GET_CODE (src) == MEM)
		mem = NULL_RTX;

	      else if (GET_CODE (dest) == MEM)
		mem = dest;

	      else if (GET_CODE (src) == MEM)
		mem = src;

	      else
		mem = NULL_RTX;

	      if (mem && ep_memory_operand (mem, GET_MODE (mem), unsignedp))
		use_ep = TRUE;

	      else if (!use_ep && mem
		       && GET_MODE_SIZE (GET_MODE (mem)) <= UNITS_PER_WORD)
		{
		  rtx addr = XEXP (mem, 0);
		  int regno = -1;
		  int short_p;

		  if (GET_CODE (addr) == REG)
		    {
		      short_p = TRUE;
		      regno = REGNO (addr);
		    }

		  else if (GET_CODE (addr) == PLUS
			   && GET_CODE (XEXP (addr, 0)) == REG
			   && GET_CODE (XEXP (addr, 1)) == CONST_INT
			   && (((unsigned)INTVAL (XEXP (addr, 1)))
			       < ep_memory_offset (GET_MODE (mem), unsignedp)))
		    {
		      short_p = TRUE;
		      regno = REGNO (XEXP (addr, 0));
		    }

		  else
		    short_p = FALSE;

		  if (short_p)
		    {
		      regs[regno].uses++;
		      regs[regno].last_insn = insn;
		      if (!regs[regno].first_insn)
			regs[regno].first_insn = insn;
		    }
		}

	      /* Loading up a register in the basic block zaps any savings
		 for the register */
	      if (GET_CODE (dest) == REG || GET_CODE (dest) == SUBREG)
		{
		  enum machine_mode mode = GET_MODE (dest);
		  int word = 0;
		  int regno;
		  int endregno;

		  while (GET_CODE (dest) == SUBREG)
		    {
		      word = SUBREG_WORD (dest);
		      dest = SUBREG_REG (dest);
		    }

		  regno = REGNO (dest) + word;
		  endregno = regno + HARD_REGNO_NREGS (regno, mode);

		  if (!use_ep)
		    {
		      /* See if we can use the pointer before this
			 modification.  */
		      int max_uses = -1;
		      int max_regno = -1;

		      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
			{
			  if (max_uses < regs[i].uses)
			    {
			      max_uses = regs[i].uses;
			      max_regno = i;
			    }
			}

		      if (max_uses > 3
			  && max_regno >= regno
			  && max_regno < endregno)
			{
			  substitute_ep_register (regs[max_regno].first_insn,
						  regs[max_regno].last_insn,
						  max_uses, max_regno, &r1, &ep);

			  /* Since we made a substitution, zap all remembered
			     registers.  */
			  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
			    {
			      regs[i].uses = 0;
			      regs[i].first_insn = NULL_RTX;
			      regs[i].last_insn = NULL_RTX;
			    }
			}
		    }

		  for (i = regno; i < endregno; i++)
		    {
		      regs[i].uses = 0;
		      regs[i].first_insn = NULL_RTX;
		      regs[i].last_insn = NULL_RTX;
		    }
		}
	    }
	}
    }
}


/* # of registers saved by the interrupt handler.  */
#define INTERRUPT_FIXED_NUM 4

/* # of bytes for registers saved by the interrupt handler.  */
#define INTERRUPT_FIXED_SAVE_SIZE (4 * INTERRUPT_FIXED_NUM)

/* # of registers saved in register parameter area.  */
#define INTERRUPT_REGPARM_NUM 4
/* # of words saved for other registers.  */
#define INTERRUPT_ALL_SAVE_NUM \
  (30 - INTERRUPT_FIXED_NUM + INTERRUPT_REGPARM_NUM)

#define INTERRUPT_ALL_SAVE_SIZE (4 * INTERRUPT_ALL_SAVE_NUM)

int
compute_register_save_size (p_reg_saved)
     long *p_reg_saved;
{
  int size = 0;
  int i;
  int interrupt_handler = v850_interrupt_function_p (current_function_decl);
  int call_p = regs_ever_live[31];
  long reg_saved = 0;

  /* Count the return pointer if we need to save it.  */
  if (profile_flag && !call_p)
    regs_ever_live[31] = call_p = 1;
 
  /* Count space for the register saves.  */
  if (interrupt_handler)
    {
      for (i = 0; i <= 31; i++)
	switch (i)
	  {
	  default:
	    if (regs_ever_live[i] || call_p)
	      {
		size += 4;
		reg_saved |= 1L << i;
	      }
	    break;

	    /* We don't save/restore r0 or the stack pointer */
	  case 0:
	  case STACK_POINTER_REGNUM:
	    break;

	    /* For registers with fixed use, we save them, set them to the
	       appropriate value, and then restore them.
	       These registers are handled specially, so don't list them
	       on the list of registers to save in the prologue.  */
	  case 1:		/* temp used to hold ep */
	  case 4:		/* gp */
	  case 10:		/* temp used to call interrupt save/restore */
	  case EP_REGNUM:	/* ep */
	    size += 4;
	    break;
	  }
    }

  else
    for (i = 0; i <= 31; i++)
      if (regs_ever_live[i] && ((! call_used_regs[i]) || i == 31))
	{
	  size += 4;
	  reg_saved |= 1L << i;
	}

  if (p_reg_saved)
    *p_reg_saved = reg_saved;

  return size;
}

int
compute_frame_size (size, p_reg_saved)
     int size;
     long *p_reg_saved;
{
  extern int current_function_outgoing_args_size;

  return (size
	  + compute_register_save_size (p_reg_saved)
	  + current_function_outgoing_args_size);
}


void
expand_prologue ()
{
  unsigned int i;
  int offset;
  unsigned int size = get_frame_size ();
  unsigned int actual_fsize;
  unsigned int init_stack_alloc = 0;
  rtx save_regs[32];
  rtx save_all;
  int num_save;
  int default_stack;
  int code;
  int interrupt_handler = v850_interrupt_function_p (current_function_decl);
  long reg_saved = 0;

  actual_fsize = compute_frame_size (size, &reg_saved);

  /* Save/setup global registers for interrupt functions right now */
  if (interrupt_handler)
    {
      emit_insn (gen_save_interrupt ());
      actual_fsize -= INTERRUPT_FIXED_SAVE_SIZE;
      if (((1L << 31) & reg_saved) != 0)
	actual_fsize -= INTERRUPT_ALL_SAVE_SIZE;
    }

  /* Save arg registers to the stack if necessary.  */
  else if (current_function_anonymous_args)
    {
      if (TARGET_PROLOG_FUNCTION)
	emit_insn (gen_save_r6_r9 ());
      else
	{
	  offset = 0;
	  for (i = 6; i < 10; i++)
	    {
	      emit_move_insn (gen_rtx (MEM, SImode,
				       plus_constant (stack_pointer_rtx,
						      offset)),
			      gen_rtx (REG, SImode, i));
	      offset += 4;
	    }
	}
    }

  /* Identify all of the saved registers */
  num_save = 0;
  default_stack = 0;
  for (i = 1; i < 31; i++)
    {
      if (((1L << i) & reg_saved) != 0)
	save_regs[num_save++] = gen_rtx (REG, Pmode, i);
    }

  /* If the return pointer is saved, the helper functions also allocate
     16 bytes of stack for arguments to be saved in.  */
  if (((1L << 31) & reg_saved) != 0)
    {
      save_regs[num_save++] = gen_rtx (REG, Pmode, 31);
      default_stack = 16;
    }

  /* See if we have an insn that allocates stack space and saves the particular
     registers we want to.  */
  save_all = NULL_RTX;
  if (TARGET_PROLOG_FUNCTION && num_save > 0 && actual_fsize >= default_stack)
    {
      int alloc_stack = (4 * num_save) + default_stack;
      int unalloc_stack = actual_fsize - alloc_stack;
      int save_func_len = 4;
      int save_normal_len;

      if (unalloc_stack)
	save_func_len += CONST_OK_FOR_J (unalloc_stack) ? 2 : 4;

      /* see if we would have used ep to save the stack */
      if (TARGET_EP && num_save > 3 && (unsigned)actual_fsize < 255)
	save_normal_len = (3 * 2) + (2 * num_save);
      else
	save_normal_len = 4 * num_save;

      save_normal_len += CONST_OK_FOR_J (actual_fsize) ? 2 : 4;

      /* Don't bother checking if we don't actually save any space.
	 This happens for instance if one register is saved and additional
	 stack space is allocated.  */
      if (save_func_len < save_normal_len)
	{
	  save_all = gen_rtx (PARALLEL, VOIDmode, rtvec_alloc (num_save + (TARGET_V850 ? 2 : 1)));
	  XVECEXP (save_all, 0, 0) = gen_rtx (SET, VOIDmode,
					      stack_pointer_rtx,
					      gen_rtx (PLUS, Pmode,
						       stack_pointer_rtx,
						       GEN_INT (-alloc_stack)));

	  if (TARGET_V850)
	    {
	      XVECEXP (save_all, 0, num_save+1)
		= gen_rtx (CLOBBER, VOIDmode, gen_rtx (REG, Pmode, 10));
	    }

	  offset = - default_stack;
	  for (i = 0; i < num_save; i++)
	    {
	      XVECEXP (save_all, 0, i+1)
		= gen_rtx (SET, VOIDmode,
			   gen_rtx (MEM, Pmode,
				    plus_constant (stack_pointer_rtx, offset)),
				    save_regs[i]);
	      offset -= 4;
	    }

	  code = recog (save_all, NULL_RTX, NULL_PTR);
	  if (code >= 0)
	    {
	      rtx insn = emit_insn (save_all);
	      INSN_CODE (insn) = code;
	      actual_fsize -= alloc_stack;

	      if (TARGET_DEBUG)
		fprintf (stderr, "Saved %d bytes via prologue function (%d vs. %d) for function %s\n",
			 save_normal_len - save_func_len,
			 save_normal_len, save_func_len,
			 IDENTIFIER_POINTER (DECL_NAME (current_function_decl)));
	    }
	  else
	    save_all = NULL_RTX;
	}
    }

  /* If no prolog save function is available, store the registers the old fashioned
     way (one by one). */
  if (!save_all)
    {
      /* Special case interrupt functions that save all registers for a call.  */
      if (interrupt_handler && ((1L << 31) & reg_saved) != 0)
	emit_insn (gen_save_all_interrupt ());

      else
	{
	  /* If the stack is too big, allocate it in chunks so we can do the
	     register saves.  We use the register save size so we use the ep
	     register.  */
	  if (actual_fsize && !CONST_OK_FOR_K (-actual_fsize))
	    init_stack_alloc = compute_register_save_size (NULL);
	  else
	    init_stack_alloc = actual_fsize;
	      
	  /* Save registers at the beginning of the stack frame */
	  offset = init_stack_alloc - 4;
	  
	  if (init_stack_alloc)
	    emit_insn (gen_addsi3 (stack_pointer_rtx,
				   stack_pointer_rtx,
				   GEN_INT (-init_stack_alloc)));
	  
	  /* Save the return pointer first.  */
	  if (num_save > 0 && REGNO (save_regs[num_save-1]) == 31)
	    {
	      emit_move_insn (gen_rtx (MEM, SImode,
				       plus_constant (stack_pointer_rtx,
						      offset)),
			      save_regs[--num_save]);
	      offset -= 4;
	    }
	  
	  for (i = 0; i < num_save; i++)
	    {
	      emit_move_insn (gen_rtx (MEM, SImode,
				       plus_constant (stack_pointer_rtx,
						      offset)),
			      save_regs[i]);
	      offset -= 4;
	    }
	}
    }

  /* Allocate the rest of the stack that was not allocated above (either it is
     > 32K or we just called a function to save the registers and needed more
     stack.  */
  if (actual_fsize > init_stack_alloc)
    {
      int diff = actual_fsize - init_stack_alloc;
      if (CONST_OK_FOR_K (diff))
	emit_insn (gen_addsi3 (stack_pointer_rtx,
			       stack_pointer_rtx,
			       GEN_INT (-diff)));
      else
	{
	  rtx reg = gen_rtx (REG, Pmode, 12);
	  emit_move_insn (reg, GEN_INT (-diff));
	  emit_insn (gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx, reg));
	}
    }

  /* If we need a frame pointer, set it up now.  */
  if (frame_pointer_needed)
    emit_move_insn (hard_frame_pointer_rtx, stack_pointer_rtx);
}


void
expand_epilogue ()
{
  unsigned int i;
  int offset;
  unsigned int size = get_frame_size ();
  long reg_saved = 0;
  unsigned int actual_fsize = compute_frame_size (size, &reg_saved);
  unsigned int init_stack_free = 0;
  rtx restore_regs[32];
  rtx restore_all;
  int num_restore;
  int default_stack;
  int code;
  int interrupt_handler = v850_interrupt_function_p (current_function_decl);

  /* Eliminate the initial stack stored by interrupt functions.  */
  if (interrupt_handler)
    {
      actual_fsize -= INTERRUPT_FIXED_SAVE_SIZE;
      if (((1L << 31) & reg_saved) != 0)
	actual_fsize -= INTERRUPT_ALL_SAVE_SIZE;
    }

  /* Cut off any dynamic stack created.  */
  if (frame_pointer_needed)
    emit_move_insn (stack_pointer_rtx, hard_frame_pointer_rtx);

  /* Identify all of the saved registers */
  num_restore = 0;
  default_stack = 0;
  for (i = 1; i < 31; i++)
    {
      if (((1L << i) & reg_saved) != 0)
	restore_regs[num_restore++] = gen_rtx (REG, Pmode, i);
    }

  /* If the return pointer is saved, the helper functions also allocate
     16 bytes of stack for arguments to be saved in.  */
  if (((1L << 31) & reg_saved) != 0)
    {
      restore_regs[num_restore++] = gen_rtx (REG, Pmode, 31);
      default_stack = 16;
    }

  /* See if we have an insn that restores the particular registers we
     want to.  */
  restore_all = NULL_RTX;
  if (TARGET_PROLOG_FUNCTION && num_restore > 0 && actual_fsize >= default_stack
      && !interrupt_handler)
    {
      int alloc_stack = (4 * num_restore) + default_stack;
      int unalloc_stack = actual_fsize - alloc_stack;
      int restore_func_len = 4;
      int restore_normal_len;

      if (unalloc_stack)
	restore_func_len += CONST_OK_FOR_J (unalloc_stack) ? 2 : 4;

      /* see if we would have used ep to restore the registers */
      if (TARGET_EP && num_restore > 3 && (unsigned)actual_fsize < 255)
	restore_normal_len = (3 * 2) + (2 * num_restore);
      else
	restore_normal_len = 4 * num_restore;

      restore_normal_len += (CONST_OK_FOR_J (actual_fsize) ? 2 : 4) + 2;

      /* Don't bother checking if we don't actually save any space.  */
      if (restore_func_len < restore_normal_len)
	{
	  restore_all = gen_rtx (PARALLEL, VOIDmode,
				 rtvec_alloc (num_restore + 2));
	  XVECEXP (restore_all, 0, 0) = gen_rtx (RETURN, VOIDmode);
	  XVECEXP (restore_all, 0, 1)
	    = gen_rtx (SET, VOIDmode, stack_pointer_rtx,
		       gen_rtx (PLUS, Pmode,
				stack_pointer_rtx,
				GEN_INT (alloc_stack)));

	  offset = alloc_stack - 4;
	  for (i = 0; i < num_restore; i++)
	    {
	      XVECEXP (restore_all, 0, i+2)
		= gen_rtx (SET, VOIDmode,
			   restore_regs[i],
			   gen_rtx (MEM, Pmode,
				    plus_constant (stack_pointer_rtx, offset)));
	      offset -= 4;
	    }

	  code = recog (restore_all, NULL_RTX, NULL_PTR);
	  if (code >= 0)
	    {
	      rtx insn;

	      actual_fsize -= alloc_stack;
	      if (actual_fsize)
		{
		  if (CONST_OK_FOR_K (actual_fsize))
		    emit_insn (gen_addsi3 (stack_pointer_rtx,
					   stack_pointer_rtx,
					   GEN_INT (actual_fsize)));
		  else
		    {
		      rtx reg = gen_rtx (REG, Pmode, 12);
		      emit_move_insn (reg, GEN_INT (actual_fsize));
		      emit_insn (gen_addsi3 (stack_pointer_rtx,
					     stack_pointer_rtx,
					     reg));
		    }
		}

	      insn = emit_jump_insn (restore_all);
	      INSN_CODE (insn) = code;

	      if (TARGET_DEBUG)
		fprintf (stderr, "Saved %d bytes via epilogue function (%d vs. %d) in function %s\n",
			 restore_normal_len - restore_func_len,
			 restore_normal_len, restore_func_len,
			 IDENTIFIER_POINTER (DECL_NAME (current_function_decl)));
	    }
	  else
	    restore_all = NULL_RTX;
	}
    }

  /* If no epilog save function is available, restore the registers the
     old fashioned way (one by one). */
  if (!restore_all)
    {
      /* If the stack is large, we need to cut it down in 2 pieces.  */
      if (actual_fsize && !CONST_OK_FOR_K (-actual_fsize))
	init_stack_free = 4 * num_restore;
      else
	init_stack_free = actual_fsize;

      /* Deallocate the rest of the stack if it is > 32K or if extra stack
	 was allocated for an interrupt handler that makes a call.  */
      if (actual_fsize > init_stack_free || (interrupt_handler && actual_fsize))
	{
	  int diff = actual_fsize - ((interrupt_handler) ? 0 : init_stack_free);
	  if (CONST_OK_FOR_K (diff))
	    emit_insn (gen_addsi3 (stack_pointer_rtx,
				   stack_pointer_rtx,
				   GEN_INT (diff)));
	  else
	    {
	      rtx reg = gen_rtx (REG, Pmode, 12);
	      emit_move_insn (reg, GEN_INT (diff));
	      emit_insn (gen_addsi3 (stack_pointer_rtx,
				     stack_pointer_rtx,
				     reg));
	    }
	}

      /* Special case interrupt functions that save all registers
	 for a call.  */
      if (interrupt_handler && ((1L << 31) & reg_saved) != 0)
	emit_insn (gen_restore_all_interrupt ());
      else
	{
	  /* Restore registers from the beginning of the stack frame */
	  offset = init_stack_free - 4;

	  /* Restore the return pointer first.  */
	  if (num_restore > 0 && REGNO (restore_regs[num_restore-1]) == 31)
	    {
	      emit_move_insn (restore_regs[--num_restore],
			      gen_rtx (MEM, SImode,
				       plus_constant (stack_pointer_rtx,
						      offset)));
	      offset -= 4;
	    }

	  for (i = 0; i < num_restore; i++)
	    {
	      emit_move_insn (restore_regs[i],
			      gen_rtx (MEM, SImode,
				       plus_constant (stack_pointer_rtx,
						      offset)));

	      offset -= 4;
	    }

	  /* Cut back the remainder of the stack.  */
	  if (init_stack_free)
	    emit_insn (gen_addsi3 (stack_pointer_rtx,
				   stack_pointer_rtx,
				   GEN_INT (init_stack_free)));
	}

      /* And return or use reti for interrupt handlers.  */
      if (interrupt_handler)
	emit_jump_insn (gen_restore_interrupt ());
      else if (actual_fsize)
	emit_jump_insn (gen_return_internal ());
      else
	emit_jump_insn (gen_return ());
    }

  current_function_anonymous_args = 0;
  v850_interrupt_cache_p = FALSE;
  v850_interrupt_p = FALSE;
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
	  && reg_overlap_mentioned_p (recog_operand[0], cc_status.value1))
	cc_status.value1 = 0;
      break;

    case CC_SET_ZN:
      /* Insn sets the Z,N flags of CC to recog_operand[0].
	 V,C is in an unusable state.  */
      CC_STATUS_INIT;
      cc_status.flags |= CC_OVERFLOW_UNUSABLE | CC_NO_CARRY;
      cc_status.value1 = recog_operand[0];
      break;

    case CC_SET_ZNV:
      /* Insn sets the Z,N,V flags of CC to recog_operand[0].
	 C is in an unusable state.  */
      CC_STATUS_INIT;
      cc_status.flags |= CC_NO_CARRY;
      cc_status.value1 = recog_operand[0];
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


/* Return nonzero if ATTR is a valid attribute for DECL.
   ATTRIBUTES are any existing attributes and ARGS are the arguments
   supplied with ATTR.

   Supported attributes:

   interrupt_handler or interrupt: output a prologue and epilogue suitable
   for an interrupt handler.  */

int
v850_valid_machine_decl_attribute (decl, attributes, attr, args)
     tree decl;
     tree attributes;
     tree attr;
     tree args;
{
  if (args != NULL_TREE)
    return 0;

  if (is_attribute_p ("interrupt_handler", attr)
      || is_attribute_p ("interrupt", attr))
    return TREE_CODE (decl) == FUNCTION_DECL;

  return 0;
}


/* Return nonzero if FUNC is an interrupt function as specified
   by the "interrupt" attribute.  */

int
v850_interrupt_function_p (func)
     tree func;
{
  tree a;
  int ret = 0;

  if (v850_interrupt_cache_p)
    return v850_interrupt_p;

  if (TREE_CODE (func) != FUNCTION_DECL)
    return 0;

  a = lookup_attribute ("interrupt_handler", DECL_MACHINE_ATTRIBUTES (func));
  if (a != NULL_TREE)
    ret = 1;

  else
    {
      a = lookup_attribute ("interrupt", DECL_MACHINE_ATTRIBUTES (func));
      ret = a != NULL_TREE;
    }

  /* Its not safe to trust global variables until after function inlining has
     been done.  */
  if (reload_completed | reload_in_progress)
    v850_interrupt_p = ret;

  return ret;
}


extern struct obstack *saveable_obstack;

v850_encode_data_area (decl)
     tree decl;
{
  char *str = XSTR (XEXP (DECL_RTL (decl), 0), 0);
  int len = strlen (str);
  char *newstr;

  /* In the Cygnus sources we actually do something; this is just
     here to make merges easier.  */
  return;
}

/* Return true if the given RTX is a register which can be restored
   by a function epilogue.  */
int
register_is_ok_for_epilogue (op, mode)
     rtx op;
     enum machine_mode mode;
{
  /* The save/restore routines can only cope with registers 2, and 20 - 31 */
  return (GET_CODE (op) == REG)
	  && (((REGNO (op) >= 20) && REGNO (op) <= 31)
	      || REGNO (op) == 2);
}

/* Return non-zero if the given RTX is suitable for collapsing into
   jump to a function epilogue.  */
int
pattern_is_ok_for_epilogue (op, mode)
     rtx op;
     enum machine_mode mode;
{
  int count = XVECLEN (op, 0);
  int i;
  
  /* If there are no registers to restore then the function epilogue
     is not suitable.  */
  if (count <= 2)
    return 0;

  /* The pattern matching has already established that we are performing a
     function epilogue and that we are popping at least one register.  We must
     now check the remaining entries in the vector to make sure that they are
     also register pops.  There is no good reason why there should ever be
     anything else in this vector, but being paranoid always helps...

     The test below performs the C equivalent of this machine description
     pattern match:

        (set (match_operand:SI n "register_is_ok_for_epilogue" "r")
	  (mem:SI (plus:SI (reg:SI 3) (match_operand:SI n "immediate_operand" "i"))))
     */

  for (i = 3; i < count; i++)
    {
      rtx vector_element = XVECEXP (op, 0, i);
      rtx dest;
      rtx src;
      rtx plus;
      
      if (GET_CODE (vector_element) != SET)
	return 0;
      
      dest = SET_DEST (vector_element);
      src = SET_SRC (vector_element);

      if (GET_CODE (dest) != REG
	  || GET_MODE (dest) != SImode
	  || ! register_is_ok_for_epilogue (dest, SImode)
	  || GET_CODE (src) != MEM
	  || GET_MODE (src) != SImode)
	return 0;

      plus = XEXP (src, 0);

      if (GET_CODE (plus) != PLUS
	  || GET_CODE (XEXP (plus, 0)) != REG
	  || GET_MODE (XEXP (plus, 0)) != SImode
	  || REGNO (XEXP (plus, 0)) != STACK_POINTER_REGNUM
	  || GET_CODE (XEXP (plus, 1)) != CONST_INT)
	return 0;
    }

  return 1;
}

/* Construct a JR instruction to a routine that will perform the equivalent of
   the RTL passed in as an argument.  This RTL is a function epilogue that
   pops registers off the stack and possibly releases some extra stack space
   as well.  The code has already verified that the RTL matches these
   requirements.  */
char *
construct_restore_jr (op)
     rtx op;
{
  int count = XVECLEN (op, 0);
  int stack_bytes;
  unsigned long int mask;
  unsigned long int first;
  unsigned long int last;
  int i;
  static char buff [100]; /* XXX */
  
  if (count <= 2)
    {
      error ("Bogus JR construction: %d\n", count);
      return NULL;
    }

  /* Work out how many bytes to pop off the stack before retrieving
     registers.  */
  if (GET_CODE (XVECEXP (op, 0, 1)) != SET)
    abort ();
  if (GET_CODE (SET_SRC (XVECEXP (op, 0, 1))) != PLUS)
    abort ();
  if (GET_CODE (XEXP (SET_SRC (XVECEXP (op, 0, 1)), 1)) != CONST_INT)
    abort ();
    
  stack_bytes = INTVAL (XEXP (SET_SRC (XVECEXP (op, 0, 1)), 1));

  /* Each pop will remove 4 bytes from the stack... */
  stack_bytes -= (count - 2) * 4;

  /* Make sure that the amount we are popping either 0 or 16 bytes.  */
  if (stack_bytes != 0 && stack_bytes != 16)
    {
      error ("Bad amount of stack space removal: %d", stack_bytes);
      return NULL;
    }

  /* Now compute the bit mask of registers to push.  */
  mask = 0;
  for (i = 2; i < count; i++)
    {
      rtx vector_element = XVECEXP (op, 0, i);
      
      if (GET_CODE (vector_element) != SET)
	abort ();
      if (GET_CODE (SET_DEST (vector_element)) != REG)
	abort ();
      if (! register_is_ok_for_epilogue (SET_DEST (vector_element), SImode))
	abort ();
      
      mask |= 1 << REGNO (SET_DEST (vector_element));
    }

  /* Scan for the first register to pop.  */
  for (first = 0; first < 32; first++)
    {
      if (mask & (1 << first))
	break;
    }

  if (first >= 32)
    abort ();

  /* Discover the last register to pop.  */
  if (mask & (1 << 31))
    {
      if (stack_bytes != 16)
	abort ();
      
      last = 31;
    }
  else
    {
      if (stack_bytes != 0)
	abort ();
      if ((mask & (1 << 29)) == 0)
	abort ();
      
      last = 29;
    }

  /* Paranoia */
  for (i = (first == 2 ? 20 : first + 1); i < 29; i++)
    if ((mask & (1 << i)) == 0)
      abort ();

  if (first == last)
    sprintf (buff, "jr __return_%s", reg_names [first]);
  else
    sprintf (buff, "jr __return_%s_%s", reg_names [first], reg_names [last]);

  return buff;
}


/* Return non-zero if the given RTX is suitable for collapsing into
   a jump to a function prologue.  */
int
pattern_is_ok_for_prologue (op, mode)
     rtx op;
     enum machine_mode mode;
{
  int count = XVECLEN (op, 0);
  int i; 
  rtx vector_element;
 
  /* If there are no registers to save then the function prologue
     is not suitable.  */
  if (count <= 2)
    return 0;

  /* The pattern matching has already established that we are adjusting the
     stack and pushing at least one register.  We must now check that the
     remaining entries in the vector to make sure that they are also register
     pushes, except for the last entry which should be a CLOBBER of r10.

     The test below performs the C equivalent of this machine description
     pattern match:

     (set (mem:SI (plus:SI (reg:SI 3)
      (match_operand:SI 2 "immediate_operand" "i")))
      (match_operand:SI 3 "register_is_ok_for_epilogue" "r"))

     */

  for (i = 2; i < count - 1; i++)
    {
      rtx dest;
      rtx src;
      rtx plus;
      
      vector_element = XVECEXP (op, 0, i);
      
      if (GET_CODE (vector_element) != SET)
	return 0;
      
      dest = SET_DEST (vector_element);
      src = SET_SRC (vector_element);

      if (GET_CODE (dest) != MEM
	  || GET_MODE (dest) != SImode
	  || GET_CODE (src) != REG
	  || GET_MODE (src) != SImode
	  || ! register_is_ok_for_epilogue (src, SImode))
	return 0;

      plus = XEXP (dest, 0);

      if ( GET_CODE (plus) != PLUS
	  || GET_CODE (XEXP (plus, 0)) != REG
	  || GET_MODE (XEXP (plus, 0)) != SImode
	  || REGNO (XEXP (plus, 0)) != STACK_POINTER_REGNUM
	  || GET_CODE (XEXP (plus, 1)) != CONST_INT)
	return 0;

      /* If the register is being pushed somewhere other than the stack
	 space just aquired by the first operand then abandon this quest.
	 Note: the test is <= becuase both values are negative.	 */
      if (INTVAL (XEXP (plus, 1))
	  <= INTVAL (XEXP (SET_SRC (XVECEXP (op, 0, 0)), 1)))
	{
	  return 0;
	}
    }

  /* Make sure that the last entry in the vector is a clobber.  */
  vector_element = XVECEXP (op, 0, i);
  
  if (GET_CODE (vector_element) != CLOBBER
      || GET_CODE (XEXP (vector_element, 0)) != REG
      || REGNO (XEXP (vector_element, 0)) != 10)
    return 0;
  
  return 1;
}

/* Construct a JARL instruction to a routine that will perform the equivalent
   of the RTL passed as a parameter.  This RTL is a function prologue that
   saves some of the registers r20 - r31 onto the stack, and possibly acquires
   some stack space as well.  The code has already verified that the RTL
   matches these requirements.  */
char *
construct_save_jarl (op)
     rtx op;
{
  int count = XVECLEN (op, 0);
  int stack_bytes;
  unsigned long int mask;
  unsigned long int first;
  unsigned long int last;
  int i;
  static char buff [100]; /* XXX */
  
  if (count <= 2)
    {
      error ("Bogus JARL construction: %d\n", count);
      return NULL;
    }

  /* Paranoia.  */
  if (GET_CODE (XVECEXP (op, 0, 0)) != SET)
    abort ();
  if (GET_CODE (SET_SRC (XVECEXP (op, 0, 0))) != PLUS)
    abort ();
  if (GET_CODE (XEXP (SET_SRC (XVECEXP (op, 0, 0)), 0)) != REG)
    abort ();
  if (GET_CODE (XEXP (SET_SRC (XVECEXP (op, 0, 0)), 1)) != CONST_INT)
    abort ();
    
  /* Work out how many bytes to push onto the stack after storing the
     registers.  */
  stack_bytes = INTVAL (XEXP (SET_SRC (XVECEXP (op, 0, 0)), 1));

  /* Each push will put 4 bytes from the stack... */
  stack_bytes += (count - 2) * 4;

  /* Make sure that the amount we are popping either 0 or 16 bytes.  */
  if (stack_bytes != 0 && stack_bytes != -16)
    {
      error ("Bad amount of stack space removal: %d", stack_bytes);
      return NULL;
    }

  /* Now compute the bit mask of registers to push.  */
  mask = 0;
  for (i = 1; i < count - 1; i++)
    {
      rtx vector_element = XVECEXP (op, 0, i);
      
      if (GET_CODE (vector_element) != SET)
	abort ();
      if (GET_CODE (SET_SRC (vector_element)) != REG)
	abort ();
      if (! register_is_ok_for_epilogue (SET_SRC (vector_element), SImode))
	abort ();
      
      mask |= 1 << REGNO (SET_SRC (vector_element));
    }

  /* Scan for the first register to push.  */  
  for (first = 0; first < 32; first++)
    {
      if (mask & (1 << first))
	break;
    }

  if (first >= 32)
    abort ();

  /* Discover the last register to push.  */
  if (mask & (1 << 31))
    {
      if (stack_bytes != -16)
	abort();
      
      last = 31;
    }
  else
    {
      if (stack_bytes != 0)
	abort ();
      if ((mask & (1 << 29)) == 0)
	abort ();
      
      last = 29;
    }

  /* Paranoia */
  for (i = (first == 2 ? 20 : first + 1); i < 29; i++)
    if ((mask & (1 << i)) == 0)
      abort ();

  if (first == last)
    sprintf (buff, "jarl __save_%s, r10", reg_names [first]);
  else
    sprintf (buff, "jarl __save_%s_%s, r10", reg_names [first],
	     reg_names [last]);

  return buff;
}

