/* Subroutines used for code generation on ROMP.
   Copyright (C) 1990, 91, 92, 93, 97-99, 2000 Free Software Foundation, Inc.
   Contributed by Richard Kenner (kenner@nyu.edu)

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
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "recog.h"
#include "expr.h"
#include "obstack.h"
#include "tree.h"
#include "function.h"
#include "tm_p.h"

#define min(A,B)	((A) < (B) ? (A) : (B))
#define max(A,B)	((A) > (B) ? (A) : (B))

static int unsigned_comparisons_p PARAMS ((rtx));
static void output_loadsave_fpregs PARAMS ((FILE *, enum rtx_code, rtx));
static void output_fpops PARAMS ((FILE *));
static void init_fpops PARAMS ((void));
static int memory_offset_in_range_p PARAMS ((rtx, enum machine_mode, int, int));
static unsigned int hash_rtx PARAMS ((rtx));

/* Return 1 if the insn using CC0 set by INSN does not contain
   any unsigned tests applied to the condition codes.

   Based on `next_insn_tests_no_inequality' in recog.c.  */

int
next_insn_tests_no_unsigned (insn)
     rtx insn;
{
  register rtx next = next_cc0_user (insn);

  if (next == 0)
    {
      if (find_reg_note (insn, REG_UNUSED, cc0_rtx))
	return 1;
      else
	abort ();
    }

  return ((GET_CODE (next) == JUMP_INSN
	   || GET_CODE (next) == INSN
	   || GET_CODE (next) == CALL_INSN)
	  && ! unsigned_comparisons_p (PATTERN (next)));
}

static int
unsigned_comparisons_p (x)
     rtx x;
{
  register const char *fmt;
  register int len, i;
  register enum rtx_code code = GET_CODE (x);

  switch (code)
    {
    case REG:
    case PC:
    case CC0:
    case CONST_INT:
    case CONST_DOUBLE:
    case CONST:
    case LABEL_REF:
    case SYMBOL_REF:
      return 0;

    case LTU:
    case GTU:
    case LEU:
    case GEU:
      return (XEXP (x, 0) == cc0_rtx || XEXP (x, 1) == cc0_rtx);
    default:
      break;
    }

  len = GET_RTX_LENGTH (code);
  fmt = GET_RTX_FORMAT (code);

  for (i = 0; i < len; i++)
    {
      if (fmt[i] == 'e')
	{
	  if (unsigned_comparisons_p (XEXP (x, i)))
	    return 1;
	}
      else if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    if (unsigned_comparisons_p (XVECEXP (x, i, j)))
	      return 1;
	}
    }
	    
  return 0;
}

/* Update the condition code from the insn.  Look mostly at the first
   byte of the machine-specific insn description information.

   cc_state.value[12] refer to two possible values that might correspond
   to the CC.  We only store register values.  */

void
update_cc (body, insn)
    rtx body ATTRIBUTE_UNUSED;
    rtx insn;
{
  switch (get_attr_cc (insn))
    {
    case CC_NONE:
      /* Insn does not affect the CC at all.  */
      break;

    case CC_CHANGE0:
      /* Insn doesn't affect the CC but does modify operand[0], known to be
	 a register.  */
      if (cc_status.value1 != 0
	  && reg_overlap_mentioned_p (recog_data.operand[0], cc_status.value1))
	cc_status.value1 = 0;

      if (cc_status.value2 != 0
	  && reg_overlap_mentioned_p (recog_data.operand[0], cc_status.value2))
	cc_status.value2 = 0;

      break;

    case CC_COPY1TO0:
      /* Insn copies operand[1] to operand[0], both registers, but doesn't
         affect the CC.  */
      if (cc_status.value1 != 0
	  && reg_overlap_mentioned_p (recog_data.operand[0], cc_status.value1))
	cc_status.value1 = 0;

      if (cc_status.value2 != 0
	  && reg_overlap_mentioned_p (recog_data.operand[0], cc_status.value2))
	cc_status.value2 = 0;

      if (cc_status.value1 != 0
	  && rtx_equal_p (cc_status.value1, recog_data.operand[1]))
	cc_status.value2 = recog_data.operand[0];

      if (cc_status.value2 != 0
	  && rtx_equal_p (cc_status.value2, recog_data.operand[1]))
	cc_status.value1 = recog_data.operand[0];

      break;

    case CC_CLOBBER:
      /* Insn clobbers CC. */
      CC_STATUS_INIT;
      break;

    case CC_SETS:
      /* Insn sets CC to recog_data.operand[0], but overflow is impossible.  */
      CC_STATUS_INIT;
      cc_status.flags |= CC_NO_OVERFLOW;
      cc_status.value1 = recog_data.operand[0];
      break;

   case CC_COMPARE:
      /* Insn is a compare which sets the CC fully.  Update CC_STATUS for this
	 compare and mark whether the test will be signed or unsigned.  */
      {
	register rtx p = PATTERN (insn);

	CC_STATUS_INIT;

	if (GET_CODE (p) == PARALLEL)
	  p = XVECEXP (p, 0, 0);
	cc_status.value1 = SET_SRC (p);

	if (GET_CODE (SET_SRC (p)) == REG)
	  cc_status.flags |= CC_NO_OVERFLOW;
	if (! next_insn_tests_no_unsigned (insn))
	  cc_status.flags |= CC_UNSIGNED;
      }
      break;

    case CC_TBIT:
      /* Insn sets T bit if result is non-zero.  Next insn must be branch. */
      CC_STATUS_INIT;
      cc_status.flags = CC_IN_TB | CC_NOT_NEGATIVE;
      break;

    default:
      abort ();
   }
}

/* Return 1 if a previous compare needs to be re-issued.  This will happen
   if two compares tested the same objects, but one was signed and the
   other unsigned.  OP is the comparison operation being performed.  */

int
restore_compare_p (op)
     rtx op;
{
  enum rtx_code code = GET_CODE (op);

  return (((code == GEU || code == LEU || code == GTU || code == LTU)
	   && ! (cc_status.flags & CC_UNSIGNED))
	  || ((code == GE || code == LE || code == GT || code == LT)
	      && (cc_status.flags & CC_UNSIGNED)));
}

/*  Generate the (long) string corresponding to an inline multiply insn.
    Note that `r10' does not refer to the register r10, but rather to the
    SCR used as the MQ.  */
const char *
output_in_line_mul ()
{
  static char insns[200];
  int i;

  strcpy (insns, "s %0,%0\n");
  strcat (insns, "\tmts r10,%1\n");
  for (i = 0; i < 16; i++)
    strcat (insns, "\tm %0,%2\n");
  strcat (insns, "\tmfs r10,%0");

  return insns;
}

/* Returns 1 if OP is a memory reference with an offset from a register within
   the range specified.  The offset must also be a multiple of the size of the
   mode.  */

static int
memory_offset_in_range_p (op, mode, low, high)
     register rtx op;
     enum machine_mode mode;
     int low, high;
{
  int offset = 0;

  if (! memory_operand (op, mode))
    return 0;

  while (GET_CODE (op) == SUBREG)
    {
      offset += SUBREG_WORD (op) * UNITS_PER_WORD;
#if BYTES_BIG_ENDIAN
      offset -= (min (UNITS_PER_WORD, GET_MODE_SIZE (GET_MODE (op)))
		 - min (UNITS_PER_WORD,
			GET_MODE_SIZE (GET_MODE (SUBREG_REG (op)))));
#endif
      op = SUBREG_REG (op);
    }

  /* We must now have either (mem (reg (x)), (mem (plus (reg (x)) (c))),
     or a constant pool address.  */
  if (GET_CODE (op) != MEM)
    abort ();

  /* Now use the actual mode and get the address.  */
  mode = GET_MODE (op);
  op = XEXP (op, 0);
  if (GET_CODE (op) == SYMBOL_REF && CONSTANT_POOL_ADDRESS_P (op))
    offset = get_pool_offset (op) + 12;
  else if (GET_CODE (op) == PLUS)
    {
      if (GET_CODE (XEXP (op, 1)) != CONST_INT
	  || ! register_operand (XEXP (op, 0), Pmode))
	return 0;

      offset += INTVAL (XEXP (op, 1));
    }

  else if (! register_operand (op, Pmode))
    return 0;

  return (offset >= low && offset <= high
	  && (offset % GET_MODE_SIZE (mode) == 0));
}

/* Return 1 if OP is a valid operand for a memory reference insn that can
   only reference indirect through a register.   */

int
zero_memory_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return memory_offset_in_range_p (op, mode, 0, 0);
}

/* Return 1 if OP is a valid operand for a `short' memory reference insn. */

int
short_memory_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (mode == VOIDmode)
    mode = GET_MODE (op);

  return memory_offset_in_range_p (op, mode, 0,
				   15 * min (UNITS_PER_WORD,
					     GET_MODE_SIZE (mode)));
}

/* Returns 1 if OP is a memory reference involving a symbolic constant
   that is not in the constant pool. */

int
symbolic_memory_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (! memory_operand (op, mode))
    return 0;

  while (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  if (GET_CODE (op) != MEM)
    abort ();

  op = XEXP (op, 0);
  if (constant_pool_address_operand (op, VOIDmode))
    return 0;
  else
    return romp_symbolic_operand (op, Pmode)
      || (GET_CODE (op) == PLUS && register_operand (XEXP (op, 0), Pmode)
	  && romp_symbolic_operand (XEXP (op, 1), Pmode));
}


/* Returns 1 if OP is a constant pool reference to the current function.  */

int
current_function_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE (op) != MEM || GET_CODE (XEXP (op, 0)) != SYMBOL_REF
      ||  ! CONSTANT_POOL_ADDRESS_P (XEXP (op, 0)))
    return 0;

  op = get_pool_constant (XEXP (op, 0));
  return (GET_CODE (op) == SYMBOL_REF
	  && ! strcmp (current_function_name, XSTR (op, 0)));
}

/* Return non-zero if this function is known to have a null epilogue.  */

int
null_epilogue ()
{
  return (reload_completed
	  && first_reg_to_save () == 16
	  && ! romp_pushes_stack ());
}

/* Returns 1 if OP is the address of a location in the constant pool.  */

int
constant_pool_address_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return ((GET_CODE (op) == SYMBOL_REF && CONSTANT_POOL_ADDRESS_P (op))
	  || (GET_CODE (op) == CONST && GET_CODE (XEXP (op, 0)) == PLUS
	      && GET_CODE (XEXP (XEXP (op, 0), 1)) == CONST_INT
	      && GET_CODE (XEXP (XEXP (op, 0), 0)) == SYMBOL_REF
	      && CONSTANT_POOL_ADDRESS_P (XEXP (XEXP (op, 0), 0))));
}

/* Returns 1 if OP is either a symbol reference or a sum of a symbol
   reference and a constant.  */

int
romp_symbolic_operand (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  switch (GET_CODE (op))
    {
    case SYMBOL_REF:
    case LABEL_REF:
      return ! op->integrated;

    case CONST:
      op = XEXP (op, 0);
      return (GET_CODE (XEXP (op, 0)) == SYMBOL_REF
	      || GET_CODE (XEXP (op, 0)) == LABEL_REF)
	     && GET_CODE (XEXP (op, 1)) == CONST_INT;

    default:
      return 0;
    }
}

/* Returns 1 if OP is a valid constant for the ROMP.  */

int
constant_operand (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  switch (GET_CODE (op))
    {
    case LABEL_REF:
    case SYMBOL_REF:
    case PLUS:
    case CONST:
      return romp_symbolic_operand (op,mode);

    case CONST_INT:
      return (unsigned int) (INTVAL (op) + 0x8000) < 0x10000
	     || (INTVAL (op) & 0xffff) == 0 || (INTVAL (op) & 0xffff0000) == 0;

    default:
      return 0;
    }
}

/* Returns 1 if OP is either a constant integer valid for the ROMP or a
   register.  If a register, it must be in the proper mode unless MODE is
   VOIDmode.  */

int
reg_or_cint_operand (op, mode)
      register rtx op;
      enum machine_mode mode;
{
  if (GET_CODE (op) == CONST_INT)
    return constant_operand (op, mode);

  return register_operand (op, mode);
}

/* Return 1 is the operand is either a register or ANY constant integer.  */

int
reg_or_any_cint_operand (op, mode)
    register rtx op;
    enum machine_mode mode;
{
     return GET_CODE (op) == CONST_INT || register_operand (op, mode);
}

/* Return 1 if the operand is either a register or a valid D-type operand. */

int
reg_or_D_operand (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  if (GET_CODE (op) == CONST_INT)
    return (unsigned) (INTVAL (op) + 0x8000) < 0x10000;

  return register_operand (op, mode);
}

/* Return 1 if the operand is either a register or an item that can be
   used as the operand of an SI add insn.  */

int
reg_or_add_operand (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  return reg_or_D_operand (op, mode) || romp_symbolic_operand (op, mode)
	 || (GET_CODE (op) == CONST_INT && (INTVAL (op) & 0xffff) == 0);
}

/* Return 1 if the operand is either a register or an item that can be
   used as the operand of a ROMP logical AND insn.  */

int
reg_or_and_operand (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  if (reg_or_cint_operand (op, mode))
    return 1;

  if (GET_CODE (op) != CONST_INT)
    return 0;

  return (INTVAL (op) & 0xffff) == 0xffff
	 || (INTVAL (op) & 0xffff0000) == 0xffff0000;
}

/* Return 1 if the operand is a register or memory operand.  */

int
reg_or_mem_operand (op, mode)
     register rtx op;
     register enum machine_mode mode;
{
  return register_operand (op, mode) || memory_operand (op, mode);
}

/* Return 1 if the operand is either a register or a memory operand that is
   not symbolic.  */

int
reg_or_nonsymb_mem_operand (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  if (register_operand (op, mode))
    return 1;

  if (memory_operand (op, mode) && ! symbolic_memory_operand (op, mode))
    return 1;

  return 0;
}

/* Return 1 if this operand is valid for the ROMP.  This is any operand except
   certain constant integers.  */

int
romp_operand (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  if (GET_CODE (op) == CONST_INT)
    return constant_operand (op, mode);

  return general_operand (op, mode);
}

/* Return 1 if the operand is (reg:mode 0).  */

int
reg_0_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return ((mode == VOIDmode || mode == GET_MODE (op))
	  && GET_CODE (op) == REG && REGNO (op) == 0);
}

/* Return 1 if the operand is (reg:mode 15).  */

int
reg_15_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return ((mode == VOIDmode || mode == GET_MODE (op))
	  && GET_CODE (op) == REG && REGNO (op) == 15);
}

/* Return 1 if this is a binary floating-point operation.  */

int
float_binary (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  if (mode != VOIDmode && mode != GET_MODE (op))
    return 0;

  if (GET_MODE (op) != SFmode && GET_MODE (op) != DFmode)
    return 0;

  switch (GET_CODE (op))
    {
    case PLUS:
    case MINUS:
    case MULT:
    case DIV:
      return GET_MODE (XEXP (op, 0)) == GET_MODE (op)
	     && GET_MODE (XEXP (op, 1)) == GET_MODE (op);

    default:
      return 0;
    }
}

/* Return 1 if this is a unary floating-point operation.  */

int
float_unary (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  if (mode != VOIDmode && mode != GET_MODE (op))
    return 0;

  if (GET_MODE (op) != SFmode && GET_MODE (op) != DFmode)
    return 0;

  return (GET_CODE (op) == NEG || GET_CODE (op) == ABS)
	 && GET_MODE (XEXP (op, 0)) == GET_MODE (op);
}

/* Return 1 if this is a valid floating-point conversion that can be done
   as part of an operation by the RT floating-point routines.  */

int
float_conversion (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  if (mode != VOIDmode && mode != GET_MODE (op))
    return 0;

  switch (GET_CODE (op))
    {
    case FLOAT_TRUNCATE:
      return GET_MODE (op) == SFmode && GET_MODE (XEXP (op, 0)) == DFmode;

    case FLOAT_EXTEND:
      return GET_MODE (op) == DFmode && GET_MODE (XEXP (op, 0)) == SFmode;

    case FLOAT:
      return ((GET_MODE (XEXP (op, 0)) == SImode
	       || GET_CODE (XEXP (op, 0)) == CONST_INT)
	      && (GET_MODE (op) == SFmode || GET_MODE (op) == DFmode));

    case FIX:
      return ((GET_MODE (op) == SImode
	       || GET_CODE (XEXP (op, 0)) == CONST_INT)
	      && (GET_MODE (XEXP (op, 0)) == SFmode
		  || GET_MODE (XEXP (op, 0)) == DFmode));

    default:
      return 0;
    }
}

/* Print an operand.  Recognize special options, documented below.  */

void
print_operand (file, x, code)
    FILE *file;
    rtx x;
    int code;
{
  int i;

  switch (code)
    {
    case 'B':
      /* Byte number (const/8) */
      if (GET_CODE (x) != CONST_INT)
	output_operand_lossage ("invalid %%B value");

      fprintf (file, "%d", INTVAL (x) / 8);
      break;

    case 'L':
      /* Low order 16 bits of constant.  */
      if (GET_CODE (x) != CONST_INT)
	output_operand_lossage ("invalid %%L value");

      fprintf (file, "%d", INTVAL (x) & 0xffff);
      break;

    case 's':
      /* Null or "16" depending on whether the constant is greater than 16. */
      if (GET_CODE (x) != CONST_INT)
	output_operand_lossage ("invalid %%s value");

      if (INTVAL (x) >= 16)
	fprintf (file, "16");

      break;

    case 'S':
      /* For shifts: 's' will have given the half.  Just give the amount
	 within 16.  */
      if (GET_CODE (x) != CONST_INT)
	output_operand_lossage ("invalid %%S value");

      fprintf (file, "%d", INTVAL (x) & 15);
      break;

    case 'b':
      /* The number of a single bit set or cleared, mod 16.  Note that the ROMP
	 numbers bits with the high-order bit 31.  */
      if (GET_CODE (x) != CONST_INT)
	output_operand_lossage ("invalid %%b value");

      if ((i = exact_log2 (INTVAL (x))) >= 0)
	fprintf (file, "%d", (31 - i) % 16);
      else if ((i = exact_log2 (~ INTVAL (x))) >= 0)
	fprintf (file, "%d", (31 - i) % 16);
      else
	output_operand_lossage ("invalid %%b value");

      break;

    case 'h':
      /* "l" or "u" depending on which half of the constant is zero.  */
      if (GET_CODE (x) != CONST_INT)
	output_operand_lossage ("invalid %%h value");

      if ((INTVAL (x) & 0xffff0000) == 0)
	fprintf (file, "l");
      else if ((INTVAL (x) & 0xffff) == 0)
	fprintf (file, "u");
      else
	output_operand_lossage ("invalid %%h value");

      break;

    case 'H':
      /* Upper or lower half, depending on which half is zero.  */
      if (GET_CODE (x) != CONST_INT)
	output_operand_lossage ("invalid %%H value");

      if ((INTVAL (x) & 0xffff0000) == 0)
	fprintf (file, "%d", INTVAL (x) & 0xffff);
      else if ((INTVAL (x) & 0xffff) == 0)
	fprintf (file, "%d", (INTVAL (x) >> 16) & 0xffff);
      else
	output_operand_lossage ("invalid %%H value");

      break;

    case 'z':
      /* Write two characters:
		'lo'	if the high order part is all ones
		'lz'	if the high order part is all zeros
		'uo'	if the low order part is all ones
		'uz'	if the low order part is all zeros 
       */
      if (GET_CODE (x) != CONST_INT)
	output_operand_lossage ("invalid %%z value");

      if ((INTVAL (x) & 0xffff0000) == 0)
	fprintf (file, "lz");
      else if ((INTVAL (x) & 0xffff0000) == 0xffff0000)
	fprintf (file, "lo");
      else if ((INTVAL (x) & 0xffff) == 0)
	fprintf (file, "uz");
      else if ((INTVAL (x) & 0xffff) == 0xffff)
	fprintf (file, "uo");
      else
	output_operand_lossage ("invalid %%z value");

      break;

    case 'Z':
      /* Upper or lower half, depending on which is non-zero or not
	 all ones.  Must be consistent with 'z' above.  */
      if (GET_CODE (x) != CONST_INT)
	output_operand_lossage ("invalid %%Z value");

      if ((INTVAL (x) & 0xffff0000) == 0
	  || (INTVAL (x) & 0xffff0000) == 0xffff0000)
	fprintf (file, "%d", INTVAL (x) & 0xffff);
      else if ((INTVAL (x) & 0xffff) == 0 || (INTVAL (x) & 0xffff) == 0xffff)
	fprintf (file, "%d", (INTVAL (x) >> 16) & 0xffff);
      else
	output_operand_lossage ("invalid %%Z value");

      break;

    case 'k':
      /* Same as 'z', except the trailing 'o' or 'z' is not written.  */
      if (GET_CODE (x) != CONST_INT)
	output_operand_lossage ("invalid %%k value");

      if ((INTVAL (x) & 0xffff0000) == 0
	  || (INTVAL (x) & 0xffff0000) == 0xffff0000)
	fprintf (file, "l");
      else if ((INTVAL (x) & 0xffff) == 0
	       || (INTVAL (x) & 0xffff) == 0xffff)
	fprintf (file, "u");
      else
	output_operand_lossage ("invalid %%k value");

      break;

    case 't':
      /* Similar to 's', except that we write 'h' or 'u'.  */
      if (GET_CODE (x) != CONST_INT)
	output_operand_lossage ("invalid %%k value");

      if (INTVAL (x) < 16)
	fprintf (file, "u");
      else
	fprintf (file, "l");
      break;

    case 'M':
      /* For memory operations, write 's' if the operand is a short
	 memory operand.  */
      if (short_memory_operand (x, VOIDmode))
	fprintf (file, "s");
      break;

    case 'N':
      /* Like 'M', but check for zero memory offset.  */
      if (zero_memory_operand (x, VOIDmode))
	fprintf (file, "s");
      break;

    case 'O':
      /* Write low-order part of DImode or DFmode.  Supported for MEM
	 and REG only.  */
      if (GET_CODE (x) == REG)
	fprintf (file, "%s", reg_names[REGNO (x) + 1]);
      else if (GET_CODE (x) == MEM)
	print_operand (file, gen_rtx_MEM (GET_MODE (x),
					  plus_constant (XEXP (x, 0), 4)), 0);
      else
	abort ();
      break;

    case 'C':
      /* Offset in constant pool for constant pool address.  */
      if (! constant_pool_address_operand (x, VOIDmode))
	abort ();
      if (GET_CODE (x) == SYMBOL_REF)
	fprintf (file, "%d", get_pool_offset (x) + 12);
      else 
	/* Must be (const (plus (symbol_ref) (const_int))) */
	fprintf (file, "%d",
		 (get_pool_offset (XEXP (XEXP (x, 0), 0)) + 12
		  + INTVAL (XEXP (XEXP (x, 0), 1))));
      break;

    case 'j':
      /* Branch opcode.  Check for condition in test bit for eq/ne.  */
      switch (GET_CODE (x))
	{
	case EQ:
	  if (cc_status.flags & CC_IN_TB)
	    fprintf (file, "ntb");
	  else
	    fprintf (file, "eq");
	  break;

	case NE:
	  if (cc_status.flags & CC_IN_TB)
	    fprintf (file, "tb");
	  else
	    fprintf (file, "ne");
	  break;

	case GT:
	case GTU:
	  fprintf (file, "h");
	  break;

	case LT:
	case LTU:
	  fprintf (file, "l");
	  break;

	case GE:
	case GEU:
	  fprintf (file, "he");
	  break;

	case LE:
	case LEU:
	  fprintf (file, "le");
	  break;

	default:
	  output_operand_lossage ("invalid %%j value");
	}
      break;

    case 'J':
      /* Reversed branch opcode.  */
      switch (GET_CODE (x))
	{
	case EQ:
	  if (cc_status.flags & CC_IN_TB)
	    fprintf (file, "tb");
	  else
	    fprintf (file, "ne");
	  break;

	case NE:
	  if (cc_status.flags & CC_IN_TB)
	    fprintf (file, "ntb");
	  else
	    fprintf (file, "eq");
	  break;

	case GT:
	case GTU:
	  fprintf (file, "le");
	  break;

	case LT:
	case LTU:
	  fprintf (file, "he");
	  break;

	case GE:
	case GEU:
	  fprintf (file, "l");
	  break;

	case LE:
	case LEU:
	  fprintf (file, "h");
	  break;

	default:
	  output_operand_lossage ("invalid %%j value");
	}
      break;

    case '.':
      /* Output nothing.  Used as delimiter in, e.g., "mc%B1%.3 " */
      break;

    case '#':
      /* Output 'x' if this insn has a delay slot, else nothing.  */
      if (dbr_sequence_length ())
	fprintf (file, "x");
      break;

    case 0:
      if (GET_CODE (x) == REG)
	fprintf (file, "%s", reg_names[REGNO (x)]);
      else if (GET_CODE (x) == MEM)
	{
	  if (GET_CODE (XEXP (x, 0)) == SYMBOL_REF
	      && current_function_operand (x, Pmode))
	    fprintf (file, "r14");
	  else
	    output_address (XEXP (x, 0));
	}
      else
	output_addr_const (file, x);
      break;

    default:
      output_operand_lossage ("invalid %%xn code");
    }
}

/* This page contains routines that are used to determine what the function
   prologue and epilogue code will do and write them out.  */

/*  Return the first register that is required to be saved. 16 if none.  */

int
first_reg_to_save()
{
  int first_reg;

  /* Find lowest numbered live register.  */
  for (first_reg = 6; first_reg <= 15; first_reg++)
    if (regs_ever_live[first_reg])
      break;

  /* If we think that we do not have to save r14, see if it will be used
     to be sure.  */
  if (first_reg > 14 && romp_using_r14 ())
    first_reg = 14;

  return first_reg;
}

/* Compute the size of the save area in the stack, including the space for
   the first four incoming arguments.  */

int
romp_sa_size ()
{
  int size;
  int i;

  /* We have the 4 words corresponding to the arguments passed in registers,
     4 reserved words, space for static chain, general register save area,
     and floating-point save area.  */
  size = 4 + 4 + 1 + (16 - first_reg_to_save ());

  /* The documentation says we have to leave 18 words in the save area if
     any floating-point registers at all are saved, not the three words
     per register you might otherwise expect.  */
  for (i = 2 + (TARGET_FP_REGS != 0); i <= 7; i++)
    if (regs_ever_live[i + 17])
      {
	size += 18;
	break;
      }

  return size * 4;
}

/* Return non-zero if this function makes calls or has fp operations
   (which are really calls).  */

int
romp_makes_calls ()
{
  rtx insn;

  for (insn = get_insns (); insn; insn = next_insn (insn))
    {
      if (GET_CODE (insn) == CALL_INSN)
	return 1;
      else if (GET_CODE (insn) == INSN)
	{
	  rtx body = PATTERN (insn);

	  if (GET_CODE (body) != USE && GET_CODE (body) != CLOBBER
	      && GET_CODE (body) != ADDR_VEC
	      && GET_CODE (body) != ADDR_DIFF_VEC
	      && get_attr_type (insn) == TYPE_FP)
	    return 1;
	}
    }

  return 0;
}

/* Return non-zero if this function will use r14 as a pointer to its
   constant pool.  */

int
romp_using_r14 ()
{
  /* If we are debugging, profiling, have a non-empty constant pool, or
     call a function, we need r14.  */
  return (write_symbols != NO_DEBUG || profile_flag || get_pool_size () != 0
	  || romp_makes_calls ());
}

/* Return non-zero if this function needs to push space on the stack.  */

int
romp_pushes_stack ()
{
  /* We need to push the stack if a frame pointer is needed (because the
     stack might be dynamically adjusted), if we are debugging, if the
     total required size is more than 100 bytes, or if we make calls.  */

  return (frame_pointer_needed || write_symbols != NO_DEBUG
	  || (romp_sa_size () + get_frame_size ()) > 100
	  || romp_makes_calls ());
}

/* Write function prologue.

   We compute the size of the fixed area required as follows:

   We always allocate 4 words for incoming arguments, 4 word reserved, 1
   word for static link, as many words as required for general register
   save area, plus 2 words for each FP reg 2-7 that must be saved.  */

void
output_prolog (file, size)
     FILE *file;
     int size;
{
  int first_reg;
  int reg_save_offset;
  int fp_save = size + current_function_outgoing_args_size;

  init_fpops ();

  /* Add in fixed size plus output argument area.  */
  size += romp_sa_size () + current_function_outgoing_args_size;

  /* Compute first register to save and perform the save operation if anything
     needs to be saved.  */
  first_reg = first_reg_to_save();
  reg_save_offset = - (4 + 4 + 1 + (16 - first_reg)) * 4;
  if (first_reg == 15)
    fprintf (file, "\tst r15,%d(r1)\n", reg_save_offset);
  else if (first_reg < 16)
    fprintf (file, "\tstm r%d,%d(r1)\n", first_reg, reg_save_offset);

  /* Set up pointer to data area if it is needed.  */
  if (romp_using_r14 ())
    fprintf (file, "\tcas r14,r0,r0\n");

  /* Set up frame pointer if needed.  */
  if (frame_pointer_needed)
    fprintf (file, "\tcal r13,-%d(r1)\n", romp_sa_size () + 64);

  /* Push stack if neeeded.  There are a couple of ways of doing this.  */
  if (romp_pushes_stack ())
    {
      if (size >= 32768)
	{
	  if (size >= 65536)
	    {
	      fprintf (file, "\tcau r0,%d(r0)\n", size >> 16);
	      fprintf (file, "\toil r0,r0,%d\n", size & 0xffff);
	    }
	  else
	    fprintf (file, "\tcal16 r0,%d(r0)\n", size);
	  fprintf (file, "\ts r1,r0\n");
	}
      else
	fprintf (file, "\tcal r1,-%d(r1)\n", size);
    }

  /* Save floating-point registers.  */
  output_loadsave_fpregs (file, USE,
			  plus_constant (stack_pointer_rtx, fp_save));
}

/* Output the offset information used by debuggers.
   This is the exactly the total_size value of output_epilog
   which is added to the frame pointer. However the value in the debug
   table is encoded in a space-saving way as follows:

   The first byte contains two fields: a 2-bit size field and the first
   6 bits of an offset value. The 2-bit size field is in the high-order
   position and specifies how many subsequent bytes follow after
   this one. An offset value is at most 4-bytes long.

   The last 6 bits of the first byte initialize the offset value. In many
   cases where procedures have small local storage, this is enough and, in
   this case, the high-order size field is zero so the byte can (almost) be
   used as is (see below). Thus, the byte value of 0x0d is encodes a offset
   size of 13 words, or 52 bytes.

   For procedures with a local space larger than 60 bytes, the 6 bits
   are the high-order 6 bits.  The remaining bytes follow as necessary,
   in Big Endian order.  Thus, the short value of 16907 (= 16384+523)
   encodes an offset of 2092 bytes (523 words).

   The total offset value is in words (not bytes), so the final value has to
   be multiplied by 4 before it can be used in address computations by a
   debugger.   */

void
output_encoded_offset (file, reg_offset)
     FILE *file;
     unsigned reg_offset;
{
  /* Convert the offset value to 4-byte words rather than bytes. */
  reg_offset = (reg_offset + 3) / 4;

  /* Now output 1-4 bytes in encoded form. */
  if (reg_offset < (1 << 6))
    /* Fits into one byte */
    fprintf (file, "\t.byte %d\n", reg_offset);
  else if (reg_offset < (1 << (6 + 8)))
    /* Fits into two bytes */
    fprintf (file, "\t.short %d\n", (1 << (6 + 8)) + reg_offset);
  else if (reg_offset < (1 << (6 + 8 + 8)))
    {
      /* Fits in three bytes */
      fprintf (file, "\t.byte %d\n", (2 << 6) + (reg_offset >> ( 6+ 8)));
      fprintf (file, "\t.short %d\n", reg_offset % (1 << (6 + 8)));
    }
  else
    {
      /* Use 4 bytes.  */
      fprintf (file, "\t.short %d", (3 << (6 + 8)) + (reg_offset >> (6 + 8)));
      fprintf (file, "\t.short %d\n", reg_offset % (1 << (6 + 8)));
    }
}

/* Write function epilogue.  */

void
output_epilog (file, size)
     FILE *file;
     int size;
{
  int first_reg = first_reg_to_save();
  int pushes_stack = romp_pushes_stack ();
  int reg_save_offset = - ((16 - first_reg) + 1 + 4 + 4) * 4;
  int total_size = (size + romp_sa_size ()
		    + current_function_outgoing_args_size);
  int fp_save = size + current_function_outgoing_args_size;
  int long_frame = total_size >= 32768;
  rtx insn = get_last_insn ();
  int write_code = 1;

  int nargs = 0;		/* words of arguments */
  tree argptr;

  /* Compute the number of words of arguments.  Since this is just for
     the traceback table, we ignore arguments that don't have a size or
     don't have a fixed size.  */

  for (argptr = DECL_ARGUMENTS (current_function_decl);
       argptr; argptr = TREE_CHAIN (argptr))
    {
      int this_size = int_size_in_bytes (TREE_TYPE (argptr));

      if (this_size > 0)
	nargs += (this_size + UNITS_PER_WORD - 1) / UNITS_PER_WORD;
    }
  
  /* If the last insn was a BARRIER, we don't have to write anything except
     the trace table.  */
  if (GET_CODE (insn) == NOTE)
    insn = prev_nonnote_insn (insn);
  if (insn && GET_CODE (insn) == BARRIER)
    write_code = 0;

  /* Restore floating-point registers.  */
  if (write_code)
    output_loadsave_fpregs (file, CLOBBER,
			    plus_constant (gen_rtx_REG (Pmode, 1), fp_save));

  /* If we push the stack and do not have size > 32K, adjust the register
     save location to the current position of sp.  Otherwise, if long frame,
     restore sp from fp.  */
  if (pushes_stack && ! long_frame)
    reg_save_offset += total_size;
  else if (long_frame && write_code)
    fprintf (file, "\tcal r1,%d(r13)\n", romp_sa_size () + 64);

  /* Restore registers.  */
  if (first_reg == 15 && write_code)
    fprintf (file, "\tl r15,%d(r1)\n", reg_save_offset);
  else if (first_reg < 16 && write_code)
    fprintf (file, "\tlm r%d,%d(r1)\n", first_reg, reg_save_offset);
  if (first_reg == 16) first_reg = 0;

  /* Handle popping stack, if needed and write debug table entry.  */
  if (pushes_stack)
    {
      if (write_code)
	{
	  if (long_frame)
	    fprintf (file, "\tbr r15\n");
	  else
	    fprintf (file, "\tbrx r15\n\tcal r1,%d(r1)\n", total_size);
	}

      /* Table header (0xdf), usual-type stack frame (0x07),
	 table header (0xdf), and first register saved.

	 The final 0x08 means that there is a byte following this one
	 describing the number of parameter words and the register used as
	 stack pointer.

	 If GCC passed floating-point parameters in floating-point registers,
	 it would be necessary to change the final byte from 0x08 to 0x0c.
	 Also an additional entry byte would be need to be emitted to specify
	 the first floating-point register.

	 (See also Section 11 (Trace Tables) in ``IBM/4.3 Linkage Convention,''
	 pages IBM/4.3-PSD:5-7 of Volume III of the IBM Academic Operating
	 System Manual dated July 1987.)  */

      fprintf (file, "\t.long 0x%x\n", 0xdf07df08 + first_reg * 0x10);

      if (nargs > 15) nargs = 15;

      /* The number of parameter words and the register used as the stack
	 pointer (encoded here as r1).

	 Note: The MetWare Hich C Compiler R2.1y actually gets this wrong;
	 it erroneously lists r13 but uses r1 as the stack too. But a bug in
	 dbx 1.5 nullifies this mistake---most of the time.
         (Dbx retrieves the value of r13 saved on the stack which is often
	 the value of r1 before the call.)  */

      fprintf (file, "\t.byte 0x%x1\n", nargs);
      output_encoded_offset (file, total_size);
    }
  else
    {
      if (write_code)
	fprintf (file, "\tbr r15\n");

      /* Table header (0xdf), no stack frame (0x02),
	 table header (0xdf) and no parameters saved (0x00).

	 If GCC passed floating-point parameters in floating-point registers,
	 it might be necessary to change the final byte from 0x00 to 0x04.
	 Also a byte would be needed to specify the first floating-point
	 register.  */
      fprintf (file, "\t.long 0xdf02df00\n");
    }

  /* Output any pending floating-point operations.  */
  output_fpops (file);
}

/* For the ROMP we need to make new SYMBOL_REFs for the actual name of a
   called routine.  To keep them unique we maintain a hash table of all
   that have been created so far.  */

struct symref_hashent {
  rtx symref;			/* Created SYMBOL_REF rtx.  */
  struct symref_hashent *next;	/* Next with same hash code.  */
};

#define SYMHASHSIZE 151
#define HASHBITS 65535

/* Define the hash table itself.  */

static struct symref_hashent *symref_hash_table[SYMHASHSIZE];

/* Given a name (allocable in temporary storage), return a SYMBOL_REF
   for the name.  The rtx is allocated from the current rtl_obstack, while
   the name string is allocated from the permanent obstack.  */
rtx
get_symref (name)
     register const char *name;
{
  extern struct obstack permanent_obstack;
  register const char *sp = name;
  unsigned int hash = 0;
  struct symref_hashent *p, **last_p;

  /* Compute the hash code for the string.  */
  while (*sp)
    hash = (hash << 4) + *sp++;

  /* Search for a matching entry in the hash table, keeping track of the
     insertion location as we do so.  */
  hash = (hash & HASHBITS) % SYMHASHSIZE;
  for (last_p = &symref_hash_table[hash], p = *last_p;
       p; last_p = &p->next, p = *last_p)
    if (strcmp (name, XSTR (p->symref, 0)) == 0)
      break;

  /* If couldn't find matching SYMBOL_REF, make a new one.  */
  if (p == 0)
    {
      /* Ensure SYMBOL_REF will stay around.  */
      end_temporary_allocation ();
      p = *last_p = (struct symref_hashent *)
			permalloc (sizeof (struct symref_hashent));
      p->symref = gen_rtx_SYMBOL_REF (Pmode,
				      obstack_copy0 (&permanent_obstack,
						     name, strlen (name)));
      p->next = 0;
      resume_temporary_allocation ();
    }

  return p->symref;
}

/* Validate the precision of a floating-point operation.

   We merge conversions from integers and between floating-point modes into
   the insn.  However, this must not effect the desired precision of the
   insn.  The RT floating-point system uses the widest of the operand modes.
   If this should be a double-precision insn, ensure that one operand
   passed to the floating-point processor has double mode.

   Note that since we don't check anything if the mode is single precision,
   it, strictly speaking, isn't necessary to call this for those insns.
   However, we do so in case something else needs to be checked in the
   future.

   This routine returns 1 if the operation is OK.  */

int
check_precision (opmode, op1, op2)
     enum machine_mode opmode;
     rtx op1, op2;
{
  if (opmode == SFmode)
    return 1;

  /* If operand is not a conversion from an integer mode or an extension from
     single-precision, it must be a double-precision value.  */
  if (GET_CODE (op1) != FLOAT && GET_CODE (op1) != FLOAT_EXTEND)
    return 1;

  if (op2 && GET_CODE (op2) != FLOAT && GET_CODE (op2) != FLOAT_EXTEND)
    return 1;

  return 0;
}

/* Floating-point on the RT is done by creating an operation block in the data
   area that describes the operation.  If two floating-point operations are the
   same in a single function, they can use the same block.

   These routines are responsible for managing these blocks.  */

/* Structure to describe a floating-point operation.  */

struct fp_op {
  struct fp_op *next_same_hash;		/* Next op with same hash code. */
  struct fp_op *next_in_mem;		/* Next op in memory. */
  int mem_offset;			/* Offset from data area.  */
  short size;				/* Size of block in bytes.  */
  short noperands;			/* Number of operands in block.  */
  rtx ops[3];				/* RTL for operands. */
  enum rtx_code opcode;			/* Operation being performed.  */
};

/* Size of hash table.  */
#define FP_HASH_SIZE 101

/* Hash table of floating-point operation blocks.  */
static struct fp_op *fp_hash_table[FP_HASH_SIZE];

/* First floating-point block in data area.  */
static struct fp_op *first_fpop;

/* Last block in data area so far.  */
static struct fp_op *last_fpop_in_mem;

/* Subroutine number in file, to get unique "LF" labels.  */
static int subr_number = 0;

/* Current word offset in data area (includes header and any constant pool). */
int data_offset;

/* Compute hash code for an RTX used in floating-point.  */

static unsigned int
hash_rtx (x)
     register rtx x;
{
  register unsigned int hash = (((int) GET_CODE (x) << 10)
				+ ((int) GET_MODE (x) << 20));
  register int i;
  register const char *fmt = GET_RTX_FORMAT (GET_CODE (x));

  for (i = 0; i < GET_RTX_LENGTH (GET_CODE (x)); i++)
    if (fmt[i] == 'e')
      hash += hash_rtx (XEXP (x, i));
    else if (fmt[i] == 'u')
      hash += (unsigned HOST_WIDE_INT) XEXP (x, i);
    else if (fmt[i] == 'i')
      hash += XINT (x, i);
    else if (fmt[i] == 's')
      hash += (unsigned HOST_WIDE_INT) XSTR (x, i);

  return hash;
}

/* Given an operation code and up to three operands, return a character string
   corresponding to the code to emit to branch to a floating-point operation
   block.  INSN is provided to see if the delay slot has been filled or not.

   A new floating-point operation block is created if this operation has not
   been seen before.  */

const char *
output_fpop (code, op0, op1, op2, insn)
     enum rtx_code code;
     rtx op0, op1, op2;
     rtx insn ATTRIBUTE_UNUSED;
{
  static char outbuf[40];
  unsigned int hash, hash0, hash1, hash2;
  int size, i;
  register struct fp_op *fpop, *last_fpop;
  int dyadic = (op2 != 0);
  enum machine_mode opmode;
  int noperands;
  rtx tem;
  unsigned int tem_hash;
  int fr0_avail = 0;

  /* Compute hash code for each operand.  If the operation is commutative,
     put the one with the smaller hash code first.  This will make us see
     more operations as identical.  */
  hash0 = op0 ? hash_rtx (op0) : 0;
  hash1 = op1 ? hash_rtx (op1) : 0;
  hash2 = op2 ? hash_rtx (op2) : 0;

  if (hash0 > hash1 && code == EQ)
    {
      tem = op0; op0 = op1; op1 = tem;
      tem_hash = hash0; hash0 = hash1; hash1 = tem_hash;
    }
  else if (hash1 > hash2 && (code == PLUS || code == MULT))
    {
      tem = op1; op1 = op2; op2 = tem;
      tem_hash = hash1; hash1 = hash2; hash2 = tem_hash;
    }

  /* If operation is commutative and the first and third operands are equal,
     swap the second and third operands.  Note that we must consider two
     operands equal if they are the same register even if different modes.  */
  if (op2 && (code == PLUS || code == MULT)
      && (rtx_equal_p (op0, op2)
	  || (GET_CODE (op0) == REG && GET_CODE (op2) == REG
	      && REGNO (op0) == REGNO (op2))))
    {
      tem = op1; op1 = op2; op2 = tem;
      tem_hash = hash1; hash1 = hash2; hash2 = tem_hash;
    }

  /* If the first and second operands are the same, merge them.  Don't do this
     for SFmode or SImode in general registers because this triggers a bug in
     the RT fp code.  */
  if (op1 && rtx_equal_p (op0, op1)
      && code != EQ && code != GE && code != SET
      && ((GET_MODE (op1) != SFmode && GET_MODE (op1) != SImode)
	  || GET_CODE (op0) != REG || FP_REGNO_P (REGNO (op0))))
    {
      op1 = op2;
      op2 = 0;
    }

  noperands = 1 + (op1 != 0) + (op2 != 0);

  /* Compute hash code for entire expression and see if operation block
     already exists.  */
  hash = ((int) code << 13) + (hash0 << 2) + (hash1 << 1) + hash2;

  hash %= FP_HASH_SIZE;
  for (fpop = fp_hash_table[hash], last_fpop = 0;
       fpop;
       last_fpop = fpop, fpop = fpop->next_same_hash)
    if (fpop->opcode == code && noperands == fpop->noperands
	&& (op0 == 0 || rtx_equal_p (op0, fpop->ops[0]))
	&& (op1 == 0 || rtx_equal_p (op1, fpop->ops[1]))
	&& (op2 == 0 || rtx_equal_p (op2, fpop->ops[2])))
      goto win;

  /* We have never seen this operation before.  */
  fpop = (struct fp_op *) oballoc (sizeof (struct fp_op));
  fpop->mem_offset = data_offset;
  fpop->opcode = code;
  fpop->noperands = noperands;
  fpop->ops[0] = op0;
  fpop->ops[1] = op1;
  fpop->ops[2] = op2;

  /* Compute the size using the rules in Appendix A of the RT Linkage
     Convention (4.3/RT-PSD:5) manual.  These rules are a bit ambiguous,
     but if we guess wrong, it will effect only efficiency, not correctness. */

  /* Size = 24 + 32 for each non-fp (or fr7) */
  size = 24;
  if (op0 && (GET_CODE (op0) != REG
	      || ! FP_REGNO_P (REGNO (op0)) || REGNO (op0) == 23))
    size += 32;

  if (op1 && (GET_CODE (op1) != REG
	      || ! FP_REGNO_P (REGNO (op1)) || REGNO (op1) == 23))
    size += 32;

  if (op2 && (GET_CODE (op2) != REG
	      || ! FP_REGNO_P (REGNO (op2)) || REGNO (op2) == 23))
    size += 32;

  /* Size + 12 for each conversion.  First get operation mode.  */
  if ((op0 && GET_MODE (op0) == DFmode)
      || (op1 && GET_MODE (op1) == DFmode)
      || (op2 && GET_MODE (op2) == DFmode))
    opmode = DFmode;
  else
    opmode = SFmode;

  if (op0 && GET_MODE (op0) != opmode)
    size += 12;
  if (op1 && GET_MODE (op1) != opmode)
    size += 12;
  if (op2 && GET_MODE (op2) != opmode)
    size += 12;

  /* 12 more if first and third operand types not the same. */
  if (op2 && GET_MODE (op0) != GET_MODE (op2))
    size += 12;

  /* CMP and CMPT need additional.  Also, compute size of save/restore here. */
  if (code == EQ)
    size += 32;
  else if (code == GE)
    size += 64;
  else if (code == USE || code == CLOBBER)
    {
      /* 34 + 24 for each additional register plus 8 if fr7 saved.  (We
         call it 36 because we need to keep the block length a multiple
	 of four.  */
      size = 36 - 24;
      for (i = 0; i <= 7; i++)
	if (INTVAL (op0) & (1 << (7-i)))
	  size += 24 + 8 * (i == 7);
    }

  /* We provide no general-purpose scratch registers.  */
  size +=16;

  /* No floating-point scratch registers are provided.  Compute extra
     length due to this.  This logic is that shown in the referenced
     appendix.  */

  i = 0;
  if (op0 && GET_CODE (op0) == REG && FP_REGNO_P (REGNO (op0)))
    i++;
  if (op1 && GET_CODE (op1) == REG && FP_REGNO_P (REGNO (op1)))
    i++;
  if (op2 && GET_CODE (op2) == REG && FP_REGNO_P (REGNO (op2)))
    i++;

  if ((op0 == 0 || GET_CODE (op0) != REG || REGNO(op0) != 17)
      && (op1 == 0 || GET_CODE (op1) != REG || REGNO(op1) != 17)
      && (op2 == 0 || GET_CODE (op2) != REG || REGNO(op2) != 17))
    fr0_avail = 1;

  if (dyadic)
    {
      if (i == 0)
	size += fr0_avail ? 64 : 112;
      else if (fpop->noperands == 2 && i == 1)
	size += fr0_avail ? 0 : 64;
      else if (fpop->noperands == 3)
	{
	  if (GET_CODE (op0) == REG && FP_REGNO_P (REGNO (op0))
	      && GET_CODE (op2) == REG && FP_REGNO_P (REGNO (op2)))
	    {
	      if (REGNO (op0) == REGNO (op2))
#if 1
		/* This triggers a bug on the RT. */
		abort ();
#else
		size += fr0_avail ? 0 : 64;
#endif
	    }
	  else
	    {
	      i = 0;
	      if (GET_CODE (op0) == REG && FP_REGNO_P (REGNO (op0)))
		i++;
	      if (GET_CODE (op2) == REG && FP_REGNO_P (REGNO (op2)))
		i++;
	      if (i == 0)
		size += fr0_avail ? 64 : 112;
	      else if (i == 1)
		size += fr0_avail ? 0 : 64;
	    }
	}
    }
  else if (code != USE && code != CLOBBER
	   && (GET_CODE (op0) != REG || ! FP_REGNO_P (REGNO (op0))))
    size += 64;
    
  if (! TARGET_FULL_FP_BLOCKS)
    {
      /* If we are not to pad the blocks, just compute its actual length.  */
      size = 12;	/* Header + opcode */
      if (code == USE || code == CLOBBER)
        size += 2;
      else
        {
	  if (op0) size += 2;
	  if (op1) size += 2;
	  if (op2) size += 2;
	}

      /* If in the middle of a word, round.  */
      if (size % UNITS_PER_WORD)
	size += 2;
	
      /* Handle any immediates.  */
      if (code != USE && code != CLOBBER && op0 && GET_CODE (op0) != REG)
        size += 4;
      if (op1 && GET_CODE (op1) != REG)
        size += 4;
      if (op2 && GET_CODE (op2) != REG)
        size += 4;

      if (code != USE && code != CLOBBER && 
	  op0 && GET_CODE (op0) == CONST_DOUBLE && GET_MODE (op0) == DFmode)
        size += 4;
      if (op1 && GET_CODE (op1) == CONST_DOUBLE && GET_MODE (op1) == DFmode)
        size += 4;
      if (op2 && GET_CODE (op2) == CONST_DOUBLE && GET_MODE (op2) == DFmode)
        size += 4;
    }

  /* Done with size computation!  Chain this in. */
  fpop->size = size;
  data_offset += size / UNITS_PER_WORD;
  fpop->next_in_mem = 0;
  fpop->next_same_hash = 0;

  if (last_fpop_in_mem)
    last_fpop_in_mem->next_in_mem = fpop;
  else
    first_fpop = fpop;
  last_fpop_in_mem = fpop;

  if (last_fpop)
    last_fpop->next_same_hash = fpop;
  else
    fp_hash_table[hash] = fpop;

win:
  /* FPOP describes the operation to be performed.  Return a string to branch
     to it.  */
  if (fpop->mem_offset < 32768 / UNITS_PER_WORD)
    sprintf (outbuf, "cal r15,%d(r14)\n\tbalr%s r15,r15",
	     fpop->mem_offset * UNITS_PER_WORD,
	     dbr_sequence_length () ? "x" : "");
  else
    sprintf (outbuf, "get r15,$L%dF%d\n\tbalr%s r15,r15",
	     subr_number, fpop->mem_offset * UNITS_PER_WORD,
	     dbr_sequence_length () ? "x" : "");
  return outbuf;
}

/* If necessary, output a floating-point operation to save or restore all
   floating-point registers.

   file is the file to write the operation to, CODE is USE for save, CLOBBER
   for restore, and ADDR is the address of the same area, as RTL.  */

static void
output_loadsave_fpregs (file, code, addr)
     FILE *file;
     enum rtx_code code;
     rtx addr;
{
  register int i;
  register int mask = 0;

  for (i = 2 + (TARGET_FP_REGS != 0); i <= 7; i++)
    if (regs_ever_live[i + 17])
      mask |= 1 << (7 - i);

  if (mask)
    fprintf (file, "\t%s\n",
	     output_fpop (code, GEN_INT (mask), gen_rtx_MEM (Pmode, addr),
				0, const0_rtx));

}

/* Output any floating-point operations at the end of the routine.  */

static void
output_fpops (file)
     FILE *file;
{
  register struct fp_op *fpop;
  register int size_so_far;
  register int i;
  rtx immed[3];

  if (first_fpop == 0)
    return;

  data_section ();

  ASM_OUTPUT_ALIGN (file, 2);

  for (fpop = first_fpop; fpop; fpop = fpop->next_in_mem)
    {
      if (fpop->mem_offset < 32768 / UNITS_PER_WORD)
	fprintf (file, "# data area offset = %d\n",
		 fpop->mem_offset * UNITS_PER_WORD);
      else
	fprintf (file, "L%dF%d:\n",
		 subr_number, fpop->mem_offset * UNITS_PER_WORD);

      fprintf (file, "\tcas r0,r15,r0\n");
      fprintf (file, "\t.long FPGLUE\n");
      switch (fpop->opcode)
	{
	case USE:
	  fprintf (file, "\t.byte 0x1d\t# STOREM\n");
	  break;
	case CLOBBER:
	  fprintf (file, "\t.byte 0x0f\t# LOADM\n");
	  break;
	case ABS:
	  fprintf (file, "\t.byte 0x00\t# ABS\n");
	  break;
	case PLUS:
	  fprintf (file, "\t.byte 0x02\t# ADD\n");
	  break;
	case EQ:
	  fprintf (file, "\t.byte 0x07\t# CMP\n");
	  break;
	case GE:
	  fprintf (file, "\t.byte 0x08\t# CMPT\n");
	  break;
	case DIV:
	  fprintf (file, "\t.byte 0x0c\t# DIV\n");
	  break;
	case SET:
	  fprintf (file, "\t.byte 0x14\t# MOVE\n");
	  break;
	case MULT:
	  fprintf (file, "\t.byte 0x15\t# MUL\n");
	  break;
	case NEG:
	  fprintf (file, "\t.byte 0x16\t# NEG\n");
	  break;
	case SQRT:
	  fprintf (file, "\t.byte 0x1c\t# SQRT\n");
	  break;
	case MINUS:
	  fprintf (file, "\t.byte 0x1e\t# SUB\n");
	  break;
	default:
	  abort ();
	}

      fprintf (file, "\t.byte %d\n", fpop->noperands);
      fprintf (file, "\t.short 0x8001\n");
      
      if ((fpop->ops[0] == 0
	   || GET_CODE (fpop->ops[0]) != REG || REGNO(fpop->ops[0]) != 17)
	  && (fpop->ops[1] == 0 || GET_CODE (fpop->ops[1]) != REG
	      || REGNO(fpop->ops[1]) != 17)
	  && (fpop->ops[2] == 0 || GET_CODE (fpop->ops[2]) != REG
	      || REGNO(fpop->ops[2]) != 17))
	fprintf (file, "\t.byte %d, 0x80\n", fpop->size);
      else
	fprintf (file, "\t.byte %d, 0\n", fpop->size);
      size_so_far = 12;
      for (i = 0; i < fpop->noperands; i++)
	{
	  register int type;
	  register int opbyte;
	  register const char *desc0;
	  char desc1[50];

	  immed[i] = 0;
	  switch (GET_MODE (fpop->ops[i]))
	    {
	    case SImode:
	    case VOIDmode:
	      desc0 = "int";
	      type = 0;
	      break;
	    case SFmode:
	      desc0 = "float";
	      type = 2;
	      break;
	    case DFmode:
	      desc0 = "double";
	      type = 3;
	      break;
	    default:
	      abort ();
	    }

	  switch (GET_CODE (fpop->ops[i]))
	    {
	    case REG:
	      strcpy(desc1, reg_names[REGNO (fpop->ops[i])]);
	      if (FP_REGNO_P (REGNO (fpop->ops[i])))
		{
		  type += 0x10;
		  opbyte = REGNO (fpop->ops[i]) - 17;
		}
	      else
		{
		  type += 0x00;
		  opbyte = REGNO (fpop->ops[i]);
		  if (type == 3)
		    opbyte = (opbyte << 4) + opbyte + 1;
		}
	      break;

	    case MEM:
	      type += 0x30;
	      if (GET_CODE (XEXP (fpop->ops[i], 0)) == PLUS)
		{
		  immed[i] = XEXP (XEXP (fpop->ops[i], 0), 1);
		  opbyte = REGNO (XEXP (XEXP (fpop->ops[i], 0), 0));
		  if (GET_CODE (immed[i]) == CONST_INT)
		    sprintf (desc1, "%d(%s)", INTVAL (immed[i]),
			     reg_names[opbyte]);
		  else
		    sprintf (desc1, "<memory> (%s)", reg_names[opbyte]);
		}
	      else if (GET_CODE (XEXP (fpop->ops[i], 0)) == REG)
		{
		  opbyte = REGNO (XEXP (fpop->ops[i], 0));
		  immed[i] = const0_rtx;
 		  sprintf (desc1, "(%s)", reg_names[opbyte]);
		}
	      else
		{
		  immed[i] = XEXP (fpop->ops[i], 0);
		  opbyte = 0;
		  sprintf(desc1, "<memory>");
		}
	      break;

	    case CONST_INT:
	    case CONST_DOUBLE:
	    case CONST:
	    case SYMBOL_REF:
	    case LABEL_REF:
	      type += 0x20;
	      opbyte = 0;
	      immed[i] = fpop->ops[i];
	      desc1[0] = '$';
	      desc1[1] = '\0';
	      break;

	    default:
	      abort ();
	    }

	  /* Save/restore is special.  */
	  if (i == 0 && (fpop->opcode == USE || fpop->opcode == CLOBBER))
	    type = 0xff, opbyte = INTVAL (fpop->ops[0]), immed[i] = 0;

	  fprintf (file, "\t.byte 0x%x,0x%x # (%s) %s\n",
		   type, opbyte, desc0, desc1);

	  size_so_far += 2;
	}

      /* If in the middle of a word, round.  */
      if (size_so_far % UNITS_PER_WORD)
	{
	  fprintf (file, "\t.space 2\n");
	  size_so_far += 2;
	}

      for (i = 0; i < fpop->noperands; i++)
	if (immed[i])
	  switch (GET_MODE (immed[i]))
	    {
	    case SImode:
	    case VOIDmode:
	      size_so_far += 4;
	      fprintf (file, "\t.long ");
	      output_addr_const (file, immed[i]);
	      fprintf (file, "\n");
	      break;

	    case DFmode:
	      size_so_far += 4;
	    case SFmode:
	      size_so_far += 4;
	      if (GET_CODE (immed[i]) == CONST_DOUBLE)
		{
		  union real_extract u;

		  bcopy ((char *) &CONST_DOUBLE_LOW (immed[i]),
			 (char *) &u, sizeof u);
		  if (GET_MODE (immed[i]) == DFmode)
		    ASM_OUTPUT_DOUBLE (file, u.d);
		  else
		    ASM_OUTPUT_FLOAT (file, u.d);
		}
	      else
		abort ();
	      break;

	    default:
	      abort ();
	    }
	
      if (size_so_far != fpop->size)
        {
          if (TARGET_FULL_FP_BLOCKS)
	    fprintf (file, "\t.space %d\n", fpop->size - size_so_far);
	  else
	    abort ();
	}
    }

  /* Update for next subroutine.  */
  subr_number++;
  text_section ();
}

 /* Initialize floating-point operation table.  */

static void
init_fpops()
{
  register int i;

  first_fpop = last_fpop_in_mem = 0;
  for (i = 0; i < FP_HASH_SIZE; i++)
    fp_hash_table[i] = 0;
}

/* Return the offset value of an automatic variable (N_LSYM) having
   the given offset. Basically, we correct by going from a frame pointer to
   stack pointer value.
*/

int
romp_debugger_auto_correction(offset)
     int offset;
{
  int fp_to_sp;

  /* We really want to go from STACK_POINTER_REGNUM to
     FRAME_POINTER_REGNUM, but this isn't defined. So go the other
     direction and negate. */
  INITIAL_ELIMINATION_OFFSET (FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM,
			      fp_to_sp);

  /* The offset value points somewhere between the frame pointer and
     the stack pointer. What is up from the frame pointer is down from the
     stack pointer. Therefore the negation in the offset value too. */

  return -(offset+fp_to_sp+4);
}

/* Return the offset value of an argument having
   the given offset. Basically, we correct by going from a arg pointer to
   stack pointer value. */

int
romp_debugger_arg_correction (offset)
     int offset;
{
  int fp_to_argp;

  INITIAL_ELIMINATION_OFFSET (ARG_POINTER_REGNUM, FRAME_POINTER_REGNUM,
			      fp_to_argp);

  /* Actually, something different happens if offset is from a floating-point
     register argument, but we don't handle it here.  */

  return (offset - fp_to_argp);
}
