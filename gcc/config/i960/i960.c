/* Subroutines used for code generation on intel 80960.
   Copyright (C) 1992, 1995, 1996, 1997, 1998, 1999, 2000, 2001
   Free Software Foundation, Inc.
   Contributed by Steven McGeady, Intel Corp.
   Additional Work by Glenn Colon-Bonet, Jonathan Shapiro, Andy Wilson
   Converted to GCC 2.0 by Jim Wilson and Michael Tiemann, Cygnus Support.

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
#include <math.h>
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "tree.h"
#include "expr.h"
#include "except.h"
#include "function.h"
#include "recog.h"
#include "toplev.h"
#include "cpplib.h"
#include "c-pragma.h"
#include "c-lex.h"
#include "tm_p.h"
#include "target.h"
#include "target-def.h"

static void i960_output_function_prologue PARAMS ((FILE *, HOST_WIDE_INT));
static void i960_output_function_epilogue PARAMS ((FILE *, HOST_WIDE_INT));

/* Save the operands last given to a compare for use when we
   generate a scc or bcc insn.  */

rtx i960_compare_op0, i960_compare_op1;

/* Used to implement #pragma align/noalign.  Initialized by OVERRIDE_OPTIONS
   macro in i960.h.  */

int i960_maxbitalignment;
int i960_last_maxbitalignment;

/* Used to implement switching between MEM and ALU insn types, for better
   C series performance.  */

enum insn_types i960_last_insn_type;

/* The leaf-procedure return register.  Set only if this is a leaf routine.  */

static int i960_leaf_ret_reg;

/* True if replacing tail calls with jumps is OK.  */

static int tail_call_ok;

/* A string containing a list of insns to emit in the epilogue so as to
   restore all registers saved by the prologue.  Created by the prologue
   code as it saves registers away.  */

char epilogue_string[1000];

/* A unique number (per function) for return labels.  */

static int ret_label = 0;

/* This is true if FNDECL is either a varargs or a stdarg function.
   This is used to help identify functions that use an argument block.  */

#define VARARGS_STDARG_FUNCTION(FNDECL)	\
((TYPE_ARG_TYPES (TREE_TYPE (FNDECL)) != 0						      \
  && (TREE_VALUE (tree_last (TYPE_ARG_TYPES (TREE_TYPE (FNDECL)))) != void_type_node))    \
 || current_function_varargs)

/* Initialize the GCC target structure.  */
#undef TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP "\t.word\t"

#undef TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE i960_output_function_prologue
#undef TARGET_ASM_FUNCTION_EPILOGUE
#define TARGET_ASM_FUNCTION_EPILOGUE i960_output_function_epilogue

struct gcc_target targetm = TARGET_INITIALIZER;

/* Initialize variables before compiling any files.  */

void
i960_initialize ()
{
  if (TARGET_IC_COMPAT2_0)
    {
      i960_maxbitalignment = 8;
      i960_last_maxbitalignment = 128;
    }
  else
    {
      i960_maxbitalignment = 128;
      i960_last_maxbitalignment = 8;
    }
}

/* Return true if OP can be used as the source of an fp move insn.  */

int
fpmove_src_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_DOUBLE || general_operand (op, mode));
}

#if 0
/* Return true if OP is a register or zero.  */

int
reg_or_zero_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return register_operand (op, mode) || op == const0_rtx;
}
#endif

/* Return truth value of whether OP can be used as an operands in a three
   address arithmetic insn (such as add %o1,7,%l2) of mode MODE.  */

int
arith_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode) || literal (op, mode));
}

/* Return truth value of whether OP can be used as an operands in a three
   address logic insn, possibly complementing OP, of mode MODE.  */

int
logic_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT
	      && INTVAL(op) >= -32 && INTVAL(op) < 32));
}

/* Return true if OP is a register or a valid floating point literal.  */

int
fp_arith_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode) || fp_literal (op, mode));
}

/* Return true if OP is a register or a valid signed integer literal.  */

int
signed_arith_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode) || signed_literal (op, mode));
}

/* Return truth value of whether OP is an integer which fits the
   range constraining immediate operands in three-address insns.  */

int
literal (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return ((GET_CODE (op) == CONST_INT) && INTVAL(op) >= 0 && INTVAL(op) < 32);
}

/* Return true if OP is a float constant of 1.  */

int
fp_literal_one (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (TARGET_NUMERICS && mode == GET_MODE (op) && op == CONST1_RTX (mode));
}

/* Return true if OP is a float constant of 0.  */

int
fp_literal_zero (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (TARGET_NUMERICS && mode == GET_MODE (op) && op == CONST0_RTX (mode));
}

/* Return true if OP is a valid floating point literal.  */

int
fp_literal(op, mode)
     rtx op;
     enum machine_mode mode;
{
  return fp_literal_zero (op, mode) || fp_literal_one (op, mode);
}

/* Return true if OP is a valid signed immediate constant.  */

int
signed_literal(op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return ((GET_CODE (op) == CONST_INT) && INTVAL(op) > -32 && INTVAL(op) < 32);
}

/* Return truth value of statement that OP is a symbolic memory
   operand of mode MODE.  */

int
symbolic_memory_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  if (GET_CODE (op) != MEM)
    return 0;
  op = XEXP (op, 0);
  return (GET_CODE (op) == SYMBOL_REF || GET_CODE (op) == CONST
	  || GET_CODE (op) == HIGH || GET_CODE (op) == LABEL_REF);
}

/* Return truth value of whether OP is EQ or NE.  */

int
eq_or_neq (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == EQ || GET_CODE (op) == NE);
}

/* OP is an integer register or a constant.  */

int
arith32_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (register_operand (op, mode))
    return 1;
  return (CONSTANT_P (op));
}

/* Return true if OP is an integer constant which is a power of 2.  */

int
power2_operand (op,mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE (op) != CONST_INT)
    return 0;

  return exact_log2 (INTVAL (op)) >= 0;
}

/* Return true if OP is an integer constant which is the complement of a
   power of 2.  */

int
cmplpower2_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE (op) != CONST_INT)
    return 0;

  return exact_log2 (~ INTVAL (op)) >= 0;
}

/* If VAL has only one bit set, return the index of that bit.  Otherwise
   return -1.  */

int
bitpos (val)
     unsigned int val;
{
  register int i;

  for (i = 0; val != 0; i++, val >>= 1)
    {
      if (val & 1)
	{
	  if (val != 1)
	    return -1;
	  return i;
	}
    }
  return -1;
}

/* Return non-zero if OP is a mask, i.e. all one bits are consecutive.
   The return value indicates how many consecutive non-zero bits exist
   if this is a mask.  This is the same as the next function, except that
   it does not indicate what the start and stop bit positions are.  */

int
is_mask (val)
     unsigned int val;
{
  register int start, end = 0, i;

  start = -1;
  for (i = 0; val != 0; val >>= 1, i++)
    {
      if (val & 1)
	{
	  if (start < 0)
	    start = i;

	  end = i;
	  continue;
	}
      /* Still looking for the first bit.  */
      if (start < 0)
	continue;

      /* We've seen the start of a bit sequence, and now a zero.  There
	 must be more one bits, otherwise we would have exited the loop.
	 Therefore, it is not a mask.  */
      if (val)
	return 0;
    }

  /* The bit string has ones from START to END bit positions only.  */
  return end - start + 1;
}

/* If VAL is a mask, then return nonzero, with S set to the starting bit
   position and E set to the ending bit position of the mask.  The return
   value indicates how many consecutive bits exist in the mask.  This is
   the same as the previous function, except that it also indicates the
   start and end bit positions of the mask.  */

int
bitstr (val, s, e)
     unsigned int val;
     int *s, *e;
{
  register int start, end, i;

  start = -1;
  end = -1;
  for (i = 0; val != 0; val >>= 1, i++)
    {
      if (val & 1)
	{
	  if (start < 0)
	    start = i;

	  end = i;
	  continue;
	}

      /* Still looking for the first bit.  */
      if (start < 0)
	continue;

      /* We've seen the start of a bit sequence, and now a zero.  There
	 must be more one bits, otherwise we would have exited the loop.
	 Therefor, it is not a mask.  */
      if (val)
	{
	  start = -1;
	  end = -1;
	  break;
	}
    }

  /* The bit string has ones from START to END bit positions only.  */
  *s = start;
  *e = end;
  return ((start < 0) ? 0 : end - start + 1);
}

/* Return the machine mode to use for a comparison.  */

enum machine_mode
select_cc_mode (op, x)
     RTX_CODE op;
     rtx x ATTRIBUTE_UNUSED;
{
  if (op == GTU || op == LTU || op == GEU || op == LEU)
    return CC_UNSmode;
  return CCmode;
}

/* X and Y are two things to compare using CODE.  Emit the compare insn and
   return the rtx for register 36 in the proper mode.  */

rtx
gen_compare_reg (code, x, y)
     enum rtx_code code;
     rtx x, y;
{
  rtx cc_reg;
  enum machine_mode ccmode = SELECT_CC_MODE (code, x, y);
  enum machine_mode mode
    = GET_MODE (x) == VOIDmode ? GET_MODE (y) : GET_MODE (x);

  if (mode == SImode)
    {
      if (! arith_operand (x, mode))
	x = force_reg (SImode, x);
      if (! arith_operand (y, mode))
	y = force_reg (SImode, y);
    }

  cc_reg = gen_rtx_REG (ccmode, 36);
  emit_insn (gen_rtx_SET (VOIDmode, cc_reg,
			  gen_rtx_COMPARE (ccmode, x, y)));

  return cc_reg;
}

/* For the i960, REG is cost 1, REG+immed CONST is cost 2, REG+REG is cost 2,
   REG+nonimmed CONST is cost 4.  REG+SYMBOL_REF, SYMBOL_REF, and similar
   are 4.  Indexed addresses are cost 6.  */

/* ??? Try using just RTX_COST, i.e. not defining ADDRESS_COST.  */

int
i960_address_cost (x)
     rtx x;
{
#if 0
  /* Handled before calling here.  */
  if (GET_CODE (x) == REG)
    return 1;
#endif
  /* This is a MEMA operand -- it's free.  */
  if (GET_CODE (x) == CONST_INT
      && INTVAL (x) >= 0
      && INTVAL (x) < 4096)
    return 0;

  if (GET_CODE (x) == PLUS)
    {
      rtx base = XEXP (x, 0);
      rtx offset = XEXP (x, 1);

      if (GET_CODE (base) == SUBREG)
	base = SUBREG_REG (base);
      if (GET_CODE (offset) == SUBREG)
	offset = SUBREG_REG (offset);

      if (GET_CODE (base) == REG)
	{
	  if (GET_CODE (offset) == REG)
	    return 2;
	  if (GET_CODE (offset) == CONST_INT)
	    {
	      if ((unsigned)INTVAL (offset) < 2047)
		return 2;
	      return 4;
	    }
	  if (CONSTANT_P (offset))
	    return 4;
	}
      if (GET_CODE (base) == PLUS || GET_CODE (base) == MULT)
	return 6;

      /* This is an invalid address.  The return value doesn't matter, but
	 for convenience we make this more expensive than anything else.  */
      return 12;
    }
  if (GET_CODE (x) == MULT)
    return 6;

  /* Symbol_refs and other unrecognized addresses are cost 4.  */
  return 4;
}

/* Emit insns to move operands[1] into operands[0].

   Return 1 if we have written out everything that needs to be done to
   do the move.  Otherwise, return 0 and the caller will emit the move
   normally.  */

int
emit_move_sequence (operands, mode)
     rtx *operands;
     enum machine_mode mode;
{
  /* We can only store registers to memory.  */
  
  if (GET_CODE (operands[0]) == MEM && GET_CODE (operands[1]) != REG
      && (operands[1] != const0_rtx || current_function_args_size
	  || current_function_varargs || current_function_stdarg
	  || rtx_equal_function_value_matters))
    /* Here we use the same test as movsi+1 pattern -- see i960.md.  */
    operands[1] = force_reg (mode, operands[1]);

  /* Storing multi-word values in unaligned hard registers to memory may
     require a scratch since we have to store them a register at a time and
     adding 4 to the memory address may not yield a valid insn.  */
  /* ??? We don't always need the scratch, but that would complicate things.
     Maybe later.  */
  /* ??? We must also handle stores to pseudos here, because the pseudo may be
     replaced with a MEM later.  This would be cleaner if we didn't have
     a separate pattern for unaligned DImode/TImode stores.  */
  if (GET_MODE_SIZE (mode) > UNITS_PER_WORD
      && (GET_CODE (operands[0]) == MEM
	  || (GET_CODE (operands[0]) == REG
	      && REGNO (operands[0]) >= FIRST_PSEUDO_REGISTER))
      && GET_CODE (operands[1]) == REG
      && REGNO (operands[1]) < FIRST_PSEUDO_REGISTER
      && ! HARD_REGNO_MODE_OK (REGNO (operands[1]), mode))
    {
      emit_insn (gen_rtx_PARALLEL
		 (VOIDmode,
		  gen_rtvec (2,
			     gen_rtx_SET (VOIDmode, operands[0], operands[1]),
			     gen_rtx_CLOBBER (VOIDmode,
					      gen_rtx_SCRATCH (Pmode)))));
      return 1;
    }

  return 0;
}

/* Output assembler to move a double word value.  */

const char *
i960_output_move_double (dst, src)
     rtx dst, src;
{
  rtx operands[5];

  if (GET_CODE (dst) == REG
      && GET_CODE (src) == REG)
    {
      if ((REGNO (src) & 1)
	  || (REGNO (dst) & 1))
	{
	  /* We normally copy the low-numbered register first.  However, if
	     the second source register is the same as the first destination
	     register, we must copy in the opposite order.  */
	  if (REGNO (src) + 1 == REGNO (dst))
	    return "mov	%D1,%D0\n\tmov	%1,%0";
	  else
	    return "mov	%1,%0\n\tmov	%D1,%D0";
	}
      else
	return "movl	%1,%0";
    }
  else if (GET_CODE (dst) == REG
	   && GET_CODE (src) == CONST_INT
	   && CONST_OK_FOR_LETTER_P (INTVAL (src), 'I'))
    {
      if (REGNO (dst) & 1)
	return "mov	%1,%0\n\tmov	0,%D0";
      else
	return "movl	%1,%0";
    }
  else if (GET_CODE (dst) == REG
	   && GET_CODE (src) == MEM)
    {
      if (REGNO (dst) & 1)
	{
	  /* One can optimize a few cases here, but you have to be
	     careful of clobbering registers used in the address and
	     edge conditions.  */
	  operands[0] = dst;
	  operands[1] = src;
	  operands[2] = gen_rtx_REG (Pmode, REGNO (dst) + 1);
	  operands[3] = gen_rtx_MEM (word_mode, operands[2]);
	  operands[4] = adjust_address (operands[3], word_mode,
					UNITS_PER_WORD);
	  output_asm_insn
	    ("lda	%1,%2\n\tld	%3,%0\n\tld	%4,%D0", operands);
	  return "";
	}
      else
	return "ldl	%1,%0";
    }
  else if (GET_CODE (dst) == MEM
	   && GET_CODE (src) == REG)
    {
      if (REGNO (src) & 1)
	{
	  operands[0] = dst;
	  operands[1] = adjust_address (dst, word_mode, UNITS_PER_WORD);
	  if (! memory_address_p (word_mode, XEXP (operands[1], 0)))
	    abort ();
	  operands[2] = src;
	  output_asm_insn ("st	%2,%0\n\tst	%D2,%1", operands);
	  return "";
	}
      return "stl	%1,%0";
    }
  else
    abort ();
}

/* Output assembler to move a double word zero.  */

const char *
i960_output_move_double_zero (dst)
     rtx dst;
{
  rtx operands[2];

  operands[0] = dst;
    {
      operands[1] = adjust_address (dst, word_mode, 4);
      output_asm_insn ("st	g14,%0\n\tst	g14,%1", operands);
    }
  return "";
}

/* Output assembler to move a quad word value.  */

const char *
i960_output_move_quad (dst, src)
     rtx dst, src;
{
  rtx operands[7];

  if (GET_CODE (dst) == REG
      && GET_CODE (src) == REG)
    {
      if ((REGNO (src) & 3)
	  || (REGNO (dst) & 3))
	{
	  /* We normally copy starting with the low numbered register.
	     However, if there is an overlap such that the first dest reg
	     is <= the last source reg but not < the first source reg, we
	     must copy in the opposite order.  */
	  if (REGNO (dst) <= REGNO (src) + 3
	      && REGNO (dst) >= REGNO (src))
	    return "mov	%F1,%F0\n\tmov	%E1,%E0\n\tmov	%D1,%D0\n\tmov	%1,%0";
	  else
	    return "mov	%1,%0\n\tmov	%D1,%D0\n\tmov	%E1,%E0\n\tmov	%F1,%F0";
	}
      else
	return "movq	%1,%0";
    }
  else if (GET_CODE (dst) == REG
	   && GET_CODE (src) == CONST_INT
	   && CONST_OK_FOR_LETTER_P (INTVAL (src), 'I'))
    {
      if (REGNO (dst) & 3)
	return "mov	%1,%0\n\tmov	0,%D0\n\tmov	0,%E0\n\tmov	0,%F0";
      else
	return "movq	%1,%0";
    }
  else if (GET_CODE (dst) == REG
	   && GET_CODE (src) == MEM)
    {
      if (REGNO (dst) & 3)
	{
	  /* One can optimize a few cases here, but you have to be
	     careful of clobbering registers used in the address and
	     edge conditions.  */
	  operands[0] = dst;
	  operands[1] = src;
	  operands[2] = gen_rtx_REG (Pmode, REGNO (dst) + 3);
	  operands[3] = gen_rtx_MEM (word_mode, operands[2]);
	  operands[4]
	    = adjust_address (operands[3], word_mode, UNITS_PER_WORD);
	  operands[5]
	    = adjust_address (operands[4], word_mode, UNITS_PER_WORD);
	  operands[6]
	    = adjust_address (operands[5], word_mode, UNITS_PER_WORD);
	  output_asm_insn ("lda	%1,%2\n\tld	%3,%0\n\tld	%4,%D0\n\tld	%5,%E0\n\tld	%6,%F0", operands);
	  return "";
	}
      else
	return "ldq	%1,%0";
    }
  else if (GET_CODE (dst) == MEM
	   && GET_CODE (src) == REG)
    {
      if (REGNO (src) & 3)
	{
	  operands[0] = dst;
	  operands[1] = adjust_address (dst, word_mode, UNITS_PER_WORD);
	  operands[2] = adjust_address (dst, word_mode, 2 * UNITS_PER_WORD);
	  operands[3] = adjust_address (dst, word_mode, 3 * UNITS_PER_WORD);
	  if (! memory_address_p (word_mode, XEXP (operands[3], 0)))
	    abort ();
	  operands[4] = src;
	  output_asm_insn ("st	%4,%0\n\tst	%D4,%1\n\tst	%E4,%2\n\tst	%F4,%3", operands);
	  return "";
	}
      return "stq	%1,%0";
    }
  else
    abort ();
}

/* Output assembler to move a quad word zero.  */

const char *
i960_output_move_quad_zero (dst)
     rtx dst;
{
  rtx operands[4];

  operands[0] = dst;
    {
      operands[1] = adjust_address (dst, word_mode, 4);
      operands[2] = adjust_address (dst, word_mode, 8);
      operands[3] = adjust_address (dst, word_mode, 12);
      output_asm_insn ("st	g14,%0\n\tst	g14,%1\n\tst	g14,%2\n\tst	g14,%3", operands);
    }
  return "";
}


/* Emit insns to load a constant to non-floating point registers.
   Uses several strategies to try to use as few insns as possible.  */

const char *
i960_output_ldconst (dst, src)
     register rtx dst, src;
{
  register int rsrc1;
  register unsigned rsrc2;
  enum machine_mode mode = GET_MODE (dst);
  rtx operands[4];

  operands[0] = operands[2] = dst;
  operands[1] = operands[3] = src;

  /* Anything that isn't a compile time constant, such as a SYMBOL_REF,
     must be a ldconst insn.  */

  if (GET_CODE (src) != CONST_INT && GET_CODE (src) != CONST_DOUBLE)
    {
      output_asm_insn ("ldconst	%1,%0", operands);
      return "";
    }
  else if (mode == XFmode)
    {
      REAL_VALUE_TYPE d;
      long value_long[3];
      int i;

      if (fp_literal_zero (src, XFmode))
	return "movt	0,%0";

      REAL_VALUE_FROM_CONST_DOUBLE (d, src);
      REAL_VALUE_TO_TARGET_LONG_DOUBLE (d, value_long);

      output_asm_insn ("# ldconst	%1,%0",operands);

      for (i = 0; i < 3; i++)
	{
	  operands[0] = gen_rtx_REG (SImode, REGNO (dst) + i);
	  operands[1] = GEN_INT (value_long[i]);
	  output_asm_insn (i960_output_ldconst (operands[0], operands[1]),
			   operands);
	}

      return ""; 
   }
  else if (mode == DFmode)
    {
      rtx first, second;

      if (fp_literal_zero (src, DFmode))
	return "movl	0,%0";

      split_double (src, &first, &second);

      output_asm_insn ("# ldconst	%1,%0",operands);

      operands[0] = gen_rtx_REG (SImode, REGNO (dst));
      operands[1] = first;
      output_asm_insn (i960_output_ldconst (operands[0], operands[1]),
		      operands);
      operands[0] = gen_rtx_REG (SImode, REGNO (dst) + 1);
      operands[1] = second;
      output_asm_insn (i960_output_ldconst (operands[0], operands[1]),
		      operands);
      return "";
    }
  else if (mode == SFmode)
    {
      REAL_VALUE_TYPE d;
      long value;

      REAL_VALUE_FROM_CONST_DOUBLE (d, src);
      REAL_VALUE_TO_TARGET_SINGLE (d, value);

      output_asm_insn ("# ldconst	%1,%0",operands);
      operands[0] = gen_rtx_REG (SImode, REGNO (dst));
      operands[1] = GEN_INT (value);
      output_asm_insn (i960_output_ldconst (operands[0], operands[1]),
		      operands);
      return "";
    }
  else if (mode == TImode)
    {
      /* ??? This is currently not handled at all.  */
      abort ();

      /* Note: lowest order word goes in lowest numbered reg.  */
      rsrc1 = INTVAL (src);
      if (rsrc1 >= 0 && rsrc1 < 32)
	return "movq	%1,%0";
      else
	output_asm_insn ("movq\t0,%0\t# ldconstq %1,%0",operands);
      /* Go pick up the low-order word.  */
    }
  else if (mode == DImode)
    {
      rtx upperhalf, lowerhalf, xoperands[2];

      if (GET_CODE (src) == CONST_DOUBLE || GET_CODE (src) == CONST_INT)
 	split_double (src, &lowerhalf, &upperhalf);

      else
	abort ();

      /* Note: lowest order word goes in lowest numbered reg.  */
      /* Numbers from 0 to 31 can be handled with a single insn.  */
      rsrc1 = INTVAL (lowerhalf);
      if (upperhalf == const0_rtx && rsrc1 >= 0 && rsrc1 < 32)
	return "movl	%1,%0";

      /* Output the upper half with a recursive call.  */
      xoperands[0] = gen_rtx_REG (SImode, REGNO (dst) + 1);
      xoperands[1] = upperhalf;
      output_asm_insn (i960_output_ldconst (xoperands[0], xoperands[1]),
		       xoperands);
      /* The lower word is emitted as normally.  */
    }
  else
    {
      rsrc1 = INTVAL (src);
      if (mode == QImode)
	{
	  if (rsrc1 > 0xff)
	    rsrc1 &= 0xff;
	}
      else if (mode == HImode)
	{
	  if (rsrc1 > 0xffff)
	    rsrc1 &= 0xffff;
	}
    }

  if (rsrc1 >= 0)
    {
      /* ldconst	0..31,X		-> 	mov	0..31,X  */
      if (rsrc1 < 32)
	{
	  if (i960_last_insn_type == I_TYPE_REG && TARGET_C_SERIES)
	    return "lda	%1,%0";
	  return "mov	%1,%0";
	}

      /* ldconst	32..63,X	->	add	31,nn,X  */
      if (rsrc1 < 63)
	{
	  if (i960_last_insn_type == I_TYPE_REG && TARGET_C_SERIES)
	    return "lda	%1,%0";
	  operands[1] = GEN_INT (rsrc1 - 31);
	  output_asm_insn ("addo\t31,%1,%0\t# ldconst %3,%0", operands);
	  return "";
	}
    }
  else if (rsrc1 < 0)
    {
      /* ldconst	-1..-31		->	sub	0,0..31,X  */
      if (rsrc1 >= -31)
	{
	  /* return 'sub -(%1),0,%0' */
	  operands[1] = GEN_INT (- rsrc1);
	  output_asm_insn ("subo\t%1,0,%0\t# ldconst %3,%0", operands);
	  return "";
	}
      
      /* ldconst	-32		->	not	31,X  */
      if (rsrc1 == -32)
	{
	  operands[1] = GEN_INT (~rsrc1);
	  output_asm_insn ("not\t%1,%0	# ldconst %3,%0", operands);
	  return "";
	}
    }

  /* If const is a single bit.  */
  if (bitpos (rsrc1) >= 0)
    {
      operands[1] = GEN_INT (bitpos (rsrc1));
      output_asm_insn ("setbit\t%1,0,%0\t# ldconst %3,%0", operands);
      return "";
    }

  /* If const is a bit string of less than 6 bits (1..31 shifted).  */
  if (is_mask (rsrc1))
    {
      int s, e;

      if (bitstr (rsrc1, &s, &e) < 6)
	{
	  rsrc2 = ((unsigned int) rsrc1) >> s;
	  operands[1] = GEN_INT (rsrc2);
	  operands[2] = GEN_INT (s);
	  output_asm_insn ("shlo\t%2,%1,%0\t# ldconst %3,%0", operands);
	  return "";
	}
    }

  /* Unimplemented cases:
     const is in range 0..31 but rotated around end of word:
     ror	31,3,g0	-> ldconst 0xe0000003,g0
   
     and any 2 instruction cases that might be worthwhile  */
  
  output_asm_insn ("ldconst	%1,%0", operands);
  return "";
}

/* Determine if there is an opportunity for a bypass optimization.
   Bypass succeeds on the 960K* if the destination of the previous
   instruction is the second operand of the current instruction.
   Bypass always succeeds on the C*.
 
   Return 1 if the pattern should interchange the operands.

   CMPBR_FLAG is true if this is for a compare-and-branch insn.
   OP1 and OP2 are the two source operands of a 3 operand insn.  */

int
i960_bypass (insn, op1, op2, cmpbr_flag)
     register rtx insn, op1, op2;
     int cmpbr_flag;
{
  register rtx prev_insn, prev_dest;

  if (TARGET_C_SERIES)
    return 0;

  /* Can't do this if op1 isn't a register.  */
  if (! REG_P (op1))
    return 0;

  /* Can't do this for a compare-and-branch if both ops aren't regs.  */
  if (cmpbr_flag && ! REG_P (op2))
    return 0;

  prev_insn = prev_real_insn (insn);

  if (prev_insn && GET_CODE (prev_insn) == INSN
      && GET_CODE (PATTERN (prev_insn)) == SET)
    {
      prev_dest = SET_DEST (PATTERN (prev_insn));
      if ((GET_CODE (prev_dest) == REG && REGNO (prev_dest) == REGNO (op1))
	  || (GET_CODE (prev_dest) == SUBREG
	      && GET_CODE (SUBREG_REG (prev_dest)) == REG
	      && REGNO (SUBREG_REG (prev_dest)) == REGNO (op1)))
	return 1;
    }
  return 0;
}

/* Output the code which declares the function name.  This also handles
   leaf routines, which have special requirements, and initializes some
   global variables.  */

void
i960_function_name_declare (file, name, fndecl)
     FILE *file;
     const char *name;
     tree fndecl;
{
  register int i, j;
  int leaf_proc_ok;
  rtx insn;

  /* Increment global return label.  */

  ret_label++;

  /* Compute whether tail calls and leaf routine optimizations can be performed
     for this function.  */

  if (TARGET_TAILCALL)
    tail_call_ok = 1;
  else
    tail_call_ok = 0;

  if (TARGET_LEAFPROC)
    leaf_proc_ok = 1;
  else
    leaf_proc_ok = 0;

  /* Even if nobody uses extra parms, can't have leafproc or tail calls if
     argblock, because argblock uses g14 implicitly.  */

  if (current_function_args_size != 0 || VARARGS_STDARG_FUNCTION (fndecl))
    {
      tail_call_ok = 0;
      leaf_proc_ok = 0;
    }
      
  /* See if caller passes in an address to return value.  */

  if (aggregate_value_p (DECL_RESULT (fndecl)))
    {
      tail_call_ok = 0;
      leaf_proc_ok = 0;
    }

  /* Can not use tail calls or make this a leaf routine if there is a non
     zero frame size.  */

  if (get_frame_size () != 0)
    leaf_proc_ok = 0;

  /* I don't understand this condition, and do not think that it is correct.
     Apparently this is just checking whether the frame pointer is used, and
     we can't trust regs_ever_live[fp] since it is (almost?) always set.  */

  if (tail_call_ok)
    for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
      if (GET_CODE (insn) == INSN
	  && reg_mentioned_p (frame_pointer_rtx, insn))
	{
	  tail_call_ok = 0;
	  break;
	}

  /* Check for CALL insns.  Can not be a leaf routine if there are any.  */

  if (leaf_proc_ok)
    for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
      if (GET_CODE (insn) == CALL_INSN)
	{
	  leaf_proc_ok = 0;
	  break;
	}

  /* Can not be a leaf routine if any non-call clobbered registers are
     used in this function.  */

  if (leaf_proc_ok)
    for (i = 0, j = 0; i < FIRST_PSEUDO_REGISTER; i++)
      if (regs_ever_live[i]
	  && ((! call_used_regs[i]) || (i > 7 && i < 12)))
	{
	  /* Global registers.  */
	  if (i < 16 && i > 7 && i != 13)
	    leaf_proc_ok = 0;
	  /* Local registers.  */
	  else if (i < 32)
	    leaf_proc_ok = 0;
	}

  /* Now choose a leaf return register, if we can find one, and if it is
     OK for this to be a leaf routine.  */

  i960_leaf_ret_reg = -1;

  if (optimize && leaf_proc_ok)
    {
      for (i960_leaf_ret_reg = -1, i = 0; i < 8; i++)
	if (regs_ever_live[i] == 0)
	  {
	    i960_leaf_ret_reg = i;
	    regs_ever_live[i] = 1;
	    break;
	  }
    }

  /* Do this after choosing the leaf return register, so it will be listed
     if one was chosen.  */

  fprintf (file, "\t#  Function '%s'\n", (name[0] == '*' ? &name[1] : name));
  fprintf (file, "\t#  Registers used: ");

  for (i = 0, j = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      if (regs_ever_live[i])
	{
	  fprintf (file, "%s%s ", reg_names[i], call_used_regs[i] ? "" : "*");

	  if (i > 15 && j == 0)
	    {
	      fprintf (file,"\n\t#\t\t   ");
	      j++;
            }
        }
    }

  fprintf (file, "\n");

  if (i960_leaf_ret_reg >= 0)
    {
      /* Make it a leaf procedure.  */

      if (TREE_PUBLIC (fndecl))
	fprintf (file,"\t.globl\t%s.lf\n", (name[0] == '*' ? &name[1] : name));

      fprintf (file, "\t.leafproc\t");
      assemble_name (file, name);
      fprintf (file, ",%s.lf\n", (name[0] == '*' ? &name[1] : name));
      ASM_OUTPUT_LABEL (file, name);
      fprintf (file, "\tlda    Li960R%d,g14\n", ret_label);
      fprintf (file, "%s.lf:\n", (name[0] == '*' ? &name[1] : name));
      fprintf (file, "\tmov    g14,g%d\n", i960_leaf_ret_reg);

      if (TARGET_C_SERIES)
	{
	  fprintf (file, "\tlda    0,g14\n");
	  i960_last_insn_type = I_TYPE_MEM;
	}
      else
	{
	  fprintf (file, "\tmov    0,g14\n");
	  i960_last_insn_type = I_TYPE_REG;
	}
    }
  else
    {
      ASM_OUTPUT_LABEL (file, name);
      i960_last_insn_type = I_TYPE_CTRL; 
    }
}

/* Compute and return the frame size.  */

int
compute_frame_size (size)
     int size;
{
  int actual_fsize;
  int outgoing_args_size = current_function_outgoing_args_size;

  /* The STARTING_FRAME_OFFSET is totally hidden to us as far
     as size is concerned.  */
  actual_fsize = (size + 15) & -16;
  actual_fsize += (outgoing_args_size + 15) & -16;

  return actual_fsize;
}

/* Here register group is range of registers which can be moved by
   one i960 instruction.  */

struct reg_group
{
  char start_reg;
  char length;
};

static int i960_form_reg_groups PARAMS ((int, int, int *, int, struct reg_group *));
static int i960_reg_group_compare PARAMS ((const void *, const void *));
static int i960_split_reg_group PARAMS ((struct reg_group *, int, int));
static void i960_arg_size_and_align PARAMS ((enum machine_mode, tree, int *, int *));

/* The following functions forms the biggest as possible register
   groups with registers in STATE.  REGS contain states of the
   registers in range [start, finish_reg).  The function returns the
   number of groups formed.  */
static int
i960_form_reg_groups (start_reg, finish_reg, regs, state, reg_groups)
     int start_reg;
     int finish_reg;
     int *regs;
     int state;
     struct reg_group *reg_groups;
{
  int i;
  int nw = 0;

  for (i = start_reg; i < finish_reg; )
    {
      if (regs [i] != state)
	{
	  i++;
	  continue;
	}
      else if (i % 2 != 0 || regs [i + 1] != state)
	reg_groups [nw].length = 1;
      else if (i % 4 != 0 || regs [i + 2] != state)
	reg_groups [nw].length = 2;
      else if (regs [i + 3] != state)
	reg_groups [nw].length = 3;
      else
	reg_groups [nw].length = 4;
      reg_groups [nw].start_reg = i;
      i += reg_groups [nw].length;
      nw++;
    }
  return nw;
}

/* We sort register winodws in descending order by length.  */
static int
i960_reg_group_compare (group1, group2)
     const void *group1;
     const void *group2;
{
  const struct reg_group *w1 = group1;
  const struct reg_group *w2 = group2;

  if (w1->length > w2->length)
    return -1;
  else if (w1->length < w2->length)
    return 1;
  else
    return 0;
}

/* Split the first register group in REG_GROUPS on subgroups one of
   which will contain SUBGROUP_LENGTH registers.  The function
   returns new number of winodws.  */
static int
i960_split_reg_group (reg_groups, nw, subgroup_length)
     struct reg_group *reg_groups;
     int nw;
     int subgroup_length;
{
  if (subgroup_length < reg_groups->length - subgroup_length)
    /* This guarantees correct alignments of the two subgroups for
       i960 (see spliting for the group length 2, 3, 4).  More
       generalized algorithm would require splitting the group more
       two subgroups.  */
    subgroup_length = reg_groups->length - subgroup_length;
  /* More generalized algorithm would require to try merging
     subgroups here.  But in case i960 it always results in failure
     because of register group alignment.  */
  reg_groups[nw].length = reg_groups->length - subgroup_length;
  reg_groups[nw].start_reg = reg_groups->start_reg + subgroup_length;
  nw++;
  reg_groups->length = subgroup_length;
  qsort (reg_groups, nw, sizeof (struct reg_group), i960_reg_group_compare);
  return nw;
}

/* Output code for the function prologue.  */

static void
i960_output_function_prologue (file, size)
     FILE *file;
     HOST_WIDE_INT size;
{
  register int i, j, nr;
  int n_saved_regs = 0;
  int n_remaining_saved_regs;
  HOST_WIDE_INT lvar_size;
  HOST_WIDE_INT actual_fsize, offset;
  int gnw, lnw;
  struct reg_group *g, *l;
  char tmpstr[1000];
  /* -1 if reg must be saved on proc entry, 0 if available, 1 if saved
     somewhere.  */
  int regs[FIRST_PSEUDO_REGISTER];
  /* All global registers (which must be saved) divided by groups.  */
  struct reg_group global_reg_groups [16];
  /* All local registers (which are available) divided by groups.  */
  struct reg_group local_reg_groups [16];


  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if (regs_ever_live[i]
	&& ((! call_used_regs[i]) || (i > 7 && i < 12))
	/* No need to save the static chain pointer.  */
	&& ! (i == STATIC_CHAIN_REGNUM && current_function_needs_context))
      {
	regs[i] = -1;
        /* Count global registers that need saving.  */
	if (i < 16)
	  n_saved_regs++;
      }
    else
      regs[i] = 0;

  n_remaining_saved_regs = n_saved_regs;

  epilogue_string[0] = '\0';

  if (current_function_profile)
    {
      /* When profiling, we may use registers 20 to 27 to save arguments, so
	 they can't be used here for saving globals.  J is the number of
	 argument registers the mcount call will save.  */
      for (j = 7; j >= 0 && ! regs_ever_live[j]; j--)
	;

      for (i = 20; i <= j + 20; i++)
	regs[i] = -1;
    }

  gnw = i960_form_reg_groups (0, 16, regs, -1, global_reg_groups);
  lnw = i960_form_reg_groups (19, 32, regs, 0, local_reg_groups);
  qsort (global_reg_groups, gnw, sizeof (struct reg_group),
	 i960_reg_group_compare);
  qsort (local_reg_groups, lnw, sizeof (struct reg_group),
	 i960_reg_group_compare);
  for (g = global_reg_groups, l = local_reg_groups; lnw != 0 && gnw != 0;)
    {
      if (g->length == l->length)
	{
	  fprintf (file, "\tmov%s	%s,%s\n",
		   ((g->length == 4) ? "q" :
		    (g->length == 3) ? "t" :
		    (g->length == 2) ? "l" : ""),
		   reg_names[(unsigned char) g->start_reg],
		   reg_names[(unsigned char) l->start_reg]);
	  sprintf (tmpstr, "\tmov%s	%s,%s\n",
		   ((g->length == 4) ? "q" :
		    (g->length == 3) ? "t" :
		    (g->length == 2) ? "l" : ""),
		   reg_names[(unsigned char) l->start_reg],
		   reg_names[(unsigned char) g->start_reg]);
	  strcat (epilogue_string, tmpstr);
	  n_remaining_saved_regs -= g->length;
	  for (i = 0; i < g->length; i++)
	    {
	      regs [i + g->start_reg] = 1;
	      regs [i + l->start_reg] = -1;
	      regs_ever_live [i + l->start_reg] = 1;
	    }
	  g++;
	  l++;
	  gnw--;
	  lnw--;
	}
      else if (g->length > l->length)
	gnw = i960_split_reg_group (g, gnw, l->length);
      else
	lnw = i960_split_reg_group (l, lnw, g->length);
    }

  actual_fsize = compute_frame_size (size) + 4 * n_remaining_saved_regs;
#if 0
  /* ??? The 1.2.1 compiler does this also.  This is meant to round the frame
     size up to the nearest multiple of 16.  I don't know whether this is
     necessary, or even desirable.

     The frame pointer must be aligned, but the call instruction takes care of
     that.  If we leave the stack pointer unaligned, we may save a little on
     dynamic stack allocation.  And we don't lose, at least according to the
     i960CA manual.  */
  actual_fsize = (actual_fsize + 15) & ~0xF;
#endif

  /* Check stack limit if necessary.  */
  if (current_function_limit_stack)
    {
      rtx min_stack = stack_limit_rtx;
      if (actual_fsize != 0)
	min_stack = plus_constant (stack_limit_rtx, -actual_fsize);

      /* Now, emulate a little bit of reload.  We want to turn 'min_stack'
	 into an arith_operand.  Use register 20 as the temporary.  */
      if (legitimate_address_p (Pmode, min_stack, 1) 
	  && !arith_operand (min_stack, Pmode))
	{
	  rtx tmp = gen_rtx_MEM (Pmode, min_stack);
	  fputs ("\tlda\t", file);
	  i960_print_operand (file, tmp, 0);
	  fputs (",r4\n", file);
	  min_stack = gen_rtx_REG (Pmode, 20);
	}
      if (arith_operand (min_stack, Pmode))
	{
	  fputs ("\tcmpo\tsp,", file);
	  i960_print_operand (file, min_stack, 0);
	  fputs ("\n\tfaultge.f\n", file);
	}
      else
	warning ("stack limit expression is not supported");
    }

  /* Allocate space for register save and locals.  */
  if (actual_fsize > 0)
    {
      if (actual_fsize < 32)
	fprintf (file, "\taddo	%d,sp,sp\n", actual_fsize);
      else
	fprintf (file, "\tlda\t%d(sp),sp\n", actual_fsize);
    }

  /* Take hardware register save area created by the call instruction
     into account, but store them before the argument block area.  */
  lvar_size = actual_fsize - compute_frame_size (0) - n_remaining_saved_regs * 4;
  offset = STARTING_FRAME_OFFSET + lvar_size;
  /* Save registers on stack if needed.  */
  /* ??? Is it worth to use the same algorithm as one for saving
     global registers in local registers? */
  for (i = 0, j = n_remaining_saved_regs; j > 0 && i < 16; i++)
    {
      if (regs[i] != -1)
	continue;

      nr = 1;

      if (i <= 14 && i % 2 == 0 && regs[i+1] == -1 && offset % 2 == 0)
	nr = 2;

      if (nr == 2 && i <= 12 && i % 4 == 0 && regs[i+2] == -1
	  && offset % 4 == 0)
	nr = 3;

      if (nr == 3 && regs[i+3] == -1)
	nr = 4;

      fprintf (file,"\tst%s	%s,%d(fp)\n",
	       ((nr == 4) ? "q" :
		(nr == 3) ? "t" :
		(nr == 2) ? "l" : ""),
	       reg_names[i], offset);
      sprintf (tmpstr,"\tld%s	%d(fp),%s\n",
	       ((nr == 4) ? "q" :
		(nr == 3) ? "t" :
		(nr == 2) ? "l" : ""),
	       offset, reg_names[i]);
      strcat (epilogue_string, tmpstr);
      i += nr-1;
      j -= nr;
      offset += nr * 4;
    }

  if (actual_fsize == 0)
    return;

  fprintf (file, "\t#Prologue stats:\n");
  fprintf (file, "\t#  Total Frame Size: %d bytes\n", actual_fsize);

  if (lvar_size)
    fprintf (file, "\t#  Local Variable Size: %d bytes\n", lvar_size);
  if (n_saved_regs)
    fprintf (file, "\t#  Register Save Size: %d regs, %d bytes\n",
	     n_saved_regs, n_saved_regs * 4);
  fprintf (file, "\t#End Prologue#\n");
}

/* Output code for the function profiler.  */

void
output_function_profiler (file, labelno)
     FILE *file;
     int labelno;
{
  /* The last used parameter register.  */
  int last_parm_reg;
  int i, j, increment;
  int varargs_stdarg_function
    = VARARGS_STDARG_FUNCTION (current_function_decl);

  /* Figure out the last used parameter register.  The proper thing to do
     is to walk incoming args of the function.  A function might have live
     parameter registers even if it has no incoming args.  Note that we
     don't have to save parameter registers g8 to g11 because they are
     call preserved.  */

  /* See also output_function_prologue, which tries to use local registers
     for preserved call-saved global registers.  */

  for (last_parm_reg = 7;
       last_parm_reg >= 0 && ! regs_ever_live[last_parm_reg];
       last_parm_reg--)
    ;

  /* Save parameter registers in regs r4 (20) to r11 (27).  */

  for (i = 0, j = 4; i <= last_parm_reg; i += increment, j += increment)
    {
      if (i % 4 == 0 && (last_parm_reg - i) >= 3)
	increment = 4;
      else if (i % 4 == 0 && (last_parm_reg - i) >= 2)
	increment = 3;
      else if (i % 2 == 0 && (last_parm_reg - i) >= 1)
	increment = 2;
      else
	increment = 1;

      fprintf (file, "\tmov%s	g%d,r%d\n",
	       (increment == 4 ? "q" : increment == 3 ? "t"
		: increment == 2 ? "l": ""), i, j);
      }

  /* If this function uses the arg pointer, then save it in r3 and then
     set it to zero.  */

  if (current_function_args_size != 0 || varargs_stdarg_function)
    fprintf (file, "\tmov	g14,r3\n\tmov	0,g14\n");

  /* Load location address into g0 and call mcount.  */

  fprintf (file, "\tlda\tLP%d,g0\n\tcallx\tmcount\n", labelno);

  /* If this function uses the arg pointer, restore it.  */

  if (current_function_args_size != 0 || varargs_stdarg_function)
    fprintf (file, "\tmov	r3,g14\n");

  /* Restore parameter registers.  */

  for (i = 0, j = 4; i <= last_parm_reg; i += increment, j += increment)
    {
      if (i % 4 == 0 && (last_parm_reg - i) >= 3)
	increment = 4;
      else if (i % 4 == 0 && (last_parm_reg - i) >= 2)
	increment = 3;
      else if (i % 2 == 0 && (last_parm_reg - i) >= 1)
	increment = 2;
      else
	increment = 1;

      fprintf (file, "\tmov%s	r%d,g%d\n",
	       (increment == 4 ? "q" : increment == 3 ? "t"
		: increment == 2 ? "l": ""), j, i);
    }
}

/* Output code for the function epilogue.  */

static void
i960_output_function_epilogue (file, size)
     FILE *file;
     HOST_WIDE_INT size ATTRIBUTE_UNUSED;
{
  if (i960_leaf_ret_reg >= 0)
    {
      fprintf (file, "Li960R%d:	ret\n", ret_label);
      return;
    }

  if (*epilogue_string == 0)
    {
      register rtx tmp;
	
      /* Emit a return insn, but only if control can fall through to here.  */

      tmp = get_last_insn ();
      while (tmp)
	{
	  if (GET_CODE (tmp) == BARRIER)
	    return;
	  if (GET_CODE (tmp) == CODE_LABEL)
	    break;
	  if (GET_CODE (tmp) == JUMP_INSN)
	    {
	      if (GET_CODE (PATTERN (tmp)) == RETURN)
		return;
	      break;
	    }
	  if (GET_CODE (tmp) == NOTE)
	    {
	      tmp = PREV_INSN (tmp);
	      continue;
	    }
	  break;
	}
      fprintf (file, "Li960R%d:	ret\n", ret_label);
      return;
    }

  fprintf (file, "Li960R%d:\n", ret_label);

  fprintf (file, "\t#EPILOGUE#\n");

  /* Output the string created by the prologue which will restore all
     registers saved by the prologue.  */

  if (epilogue_string[0] != '\0')
    fprintf (file, "%s", epilogue_string);

  /* Must clear g14 on return if this function set it.
     Only varargs/stdarg functions modify g14.  */

  if (VARARGS_STDARG_FUNCTION (current_function_decl))
    fprintf (file, "\tmov	0,g14\n");

  fprintf (file, "\tret\n");
  fprintf (file, "\t#End Epilogue#\n");
}

/* Output code for a call insn.  */

const char *
i960_output_call_insn (target, argsize_rtx, arg_pointer, insn)
     register rtx target, argsize_rtx, arg_pointer, insn;
{
  int argsize = INTVAL (argsize_rtx);
  rtx nexti = next_real_insn (insn);
  rtx operands[2];
  int varargs_stdarg_function
    = VARARGS_STDARG_FUNCTION (current_function_decl);

  operands[0] = target;
  operands[1] = arg_pointer;

  if (current_function_args_size != 0 || varargs_stdarg_function)
    output_asm_insn ("mov	g14,r3", operands);

  if (argsize > 48)
    output_asm_insn ("lda	%a1,g14", operands);
  else if (current_function_args_size != 0 || varargs_stdarg_function)
    output_asm_insn ("mov	0,g14", operands);

  /* The code used to assume that calls to SYMBOL_REFs could not be more
     than 24 bits away (b vs bx, callj vs callx).  This is not true.  This
     feature is now implemented by relaxing in the GNU linker.  It can convert
     bx to b if in range, and callx to calls/call/balx/bal as appropriate.  */

  /* Nexti could be zero if the called routine is volatile.  */
  if (optimize && (*epilogue_string == 0) && argsize == 0 && tail_call_ok 
      && (nexti == 0 || GET_CODE (PATTERN (nexti)) == RETURN))
    {
      /* Delete following return insn.  */
      if (nexti && no_labels_between_p (insn, nexti))
	delete_insn (nexti);
      output_asm_insn ("bx	%0", operands);
      return "# notreached";
    }

  output_asm_insn ("callx	%0", operands);

  /* If the caller sets g14 to the address of the argblock, then the caller
     must clear it after the return.  */

  if (current_function_args_size != 0 || varargs_stdarg_function)
    output_asm_insn ("mov	r3,g14", operands);
  else if (argsize > 48)
    output_asm_insn ("mov	0,g14", operands);

  return "";
}

/* Output code for a return insn.  */

const char *
i960_output_ret_insn (insn)
     register rtx insn;
{
  static char lbuf[20];
  
  if (*epilogue_string != 0)
    {
      if (! TARGET_CODE_ALIGN && next_real_insn (insn) == 0)
	return "";

      sprintf (lbuf, "b	Li960R%d", ret_label);
      return lbuf;
    }

  /* Must clear g14 on return if this function set it.
     Only varargs/stdarg functions modify g14.  */

  if (VARARGS_STDARG_FUNCTION (current_function_decl))
    output_asm_insn ("mov	0,g14", 0);

  if (i960_leaf_ret_reg >= 0)
    {
      sprintf (lbuf, "bx	(%s)", reg_names[i960_leaf_ret_reg]);
      return lbuf;
    }
  return "ret";
}

/* Print the operand represented by rtx X formatted by code CODE.  */

void
i960_print_operand (file, x, code)
     FILE *file;
     rtx x;
     int code;
{
  enum rtx_code rtxcode = x ? GET_CODE (x) : NIL;

  if (rtxcode == REG)
    {
      switch (code)
	{
	case 'D':
	  /* Second reg of a double or quad.  */
	  fprintf (file, "%s", reg_names[REGNO (x)+1]);
	  break;

	case 'E':
	  /* Third reg of a quad.  */
	  fprintf (file, "%s", reg_names[REGNO (x)+2]);
	  break;

	case 'F':
	  /* Fourth reg of a quad.  */
	  fprintf (file, "%s", reg_names[REGNO (x)+3]);
	  break;

	case 0:
	  fprintf (file, "%s", reg_names[REGNO (x)]);
	  break;

	default:
	  abort ();
	}
      return;
    }
  else if (rtxcode == MEM)
    {
      output_address (XEXP (x, 0));
      return;
    }
  else if (rtxcode == CONST_INT)
    {
      HOST_WIDE_INT val = INTVAL (x);
      if (code == 'C')
	val = ~val;
      if (val > 9999 || val < -999)
	fprintf (file, "0x%x", val);
      else
	fprintf (file, "%d", val);
      return;
    }
  else if (rtxcode == CONST_DOUBLE)
    {
      REAL_VALUE_TYPE d;
      char dstr[30];

      if (x == CONST0_RTX (GET_MODE (x)))
	{
	  fprintf (file, "0f0.0");
	  return;
	}
      else if (x == CONST1_RTX (GET_MODE (x)))
	{
	  fprintf (file, "0f1.0");
	  return;
	}

      REAL_VALUE_FROM_CONST_DOUBLE (d, x);
      REAL_VALUE_TO_DECIMAL (d, "%#g", dstr);
      fprintf (file, "0f%s", dstr);
      return;
    }

  switch(code)
    {
    case 'B':
      /* Branch or jump, depending on assembler.  */
      if (TARGET_ASM_COMPAT)
	fputs ("j", file);
      else
	fputs ("b", file);
      break;

    case 'S':
      /* Sign of condition.  */
      if ((rtxcode == EQ) || (rtxcode == NE) || (rtxcode == GTU)
	  || (rtxcode == LTU) || (rtxcode == GEU) || (rtxcode == LEU))
	fputs ("o", file);
      else if ((rtxcode == GT) || (rtxcode == LT)
	  || (rtxcode == GE) || (rtxcode == LE))
	fputs ("i", file);
      else
	abort();
      break;

    case 'I':
      /* Inverted condition.  */
      rtxcode = reverse_condition (rtxcode);
      goto normal;

    case 'X':
      /* Inverted condition w/ reversed operands.  */
      rtxcode = reverse_condition (rtxcode);
      /* Fallthrough.  */

    case 'R':
      /* Reversed operand condition.  */
      rtxcode = swap_condition (rtxcode);
      /* Fallthrough.  */

    case 'C':
      /* Normal condition.  */
    normal:
      if (rtxcode == EQ)  { fputs ("e", file); return; }
      else if (rtxcode == NE)  { fputs ("ne", file); return; }
      else if (rtxcode == GT)  { fputs ("g", file); return; }
      else if (rtxcode == GTU) { fputs ("g", file); return; }
      else if (rtxcode == LT)  { fputs ("l", file); return; }
      else if (rtxcode == LTU) { fputs ("l", file); return; }
      else if (rtxcode == GE)  { fputs ("ge", file); return; }
      else if (rtxcode == GEU) { fputs ("ge", file); return; }
      else if (rtxcode == LE)  { fputs ("le", file); return; }
      else if (rtxcode == LEU) { fputs ("le", file); return; }
      else abort ();
      break;

    case '+':
      /* For conditional branches, substitute ".t" or ".f".  */
      if (TARGET_BRANCH_PREDICT)
	{
	  x = find_reg_note (current_output_insn, REG_BR_PROB, 0);
	  if (x)
	    {
	      int pred_val = INTVAL (XEXP (x, 0));
	      fputs ((pred_val < REG_BR_PROB_BASE / 2 ? ".f" : ".t"), file);
	    }
	}
      break;

    case 0:
      output_addr_const (file, x);
      break;

    default:
      abort ();
    }

  return;
}

/* Print a memory address as an operand to reference that memory location.

   This is exactly the same as legitimate_address_p, except that it the prints
   addresses instead of recognizing them.  */

void
i960_print_operand_addr (file, addr)
     FILE *file;
     register rtx addr;
{
  rtx breg, ireg;
  rtx scale, offset;

  ireg = 0;
  breg = 0;
  offset = 0;
  scale = const1_rtx;

  if (GET_CODE (addr) == REG)
    breg = addr;
  else if (CONSTANT_P (addr))
    offset = addr;
  else if (GET_CODE (addr) == PLUS)
    {
      rtx op0, op1;

      op0 = XEXP (addr, 0);
      op1 = XEXP (addr, 1);

      if (GET_CODE (op0) == REG)
	{
	  breg = op0;
	  if (GET_CODE (op1) == REG)
	    ireg = op1;
	  else if (CONSTANT_P (op1))
	    offset = op1;
	  else
	    abort ();
	}
      else if (GET_CODE (op0) == PLUS)
	{
	  if (GET_CODE (XEXP (op0, 0)) == MULT)
	    {
	      ireg = XEXP (XEXP (op0, 0), 0);
	      scale = XEXP (XEXP (op0, 0), 1);
	      if (GET_CODE (XEXP (op0, 1)) == REG)
		{
		  breg = XEXP (op0, 1);
		  offset = op1;
		}
	      else
		abort ();
	    }
	  else if (GET_CODE (XEXP (op0, 0)) == REG)
	    {
	      breg = XEXP (op0, 0);
	      if (GET_CODE (XEXP (op0, 1)) == REG)
		{
		  ireg = XEXP (op0, 1);
		  offset = op1;
		}
	      else
		abort ();
	    }
	  else
	    abort ();
	}
      else if (GET_CODE (op0) == MULT)
	{
	  ireg = XEXP (op0, 0);
	  scale = XEXP (op0, 1);
	  if (GET_CODE (op1) == REG)
	    breg = op1;
	  else if (CONSTANT_P (op1))
	    offset = op1;
	  else
	    abort ();
	}
      else
	abort ();
    }
  else if (GET_CODE (addr) == MULT)
    {
      ireg = XEXP (addr, 0);
      scale = XEXP (addr, 1);
    }
  else
    abort ();

  if (offset)
    output_addr_const (file, offset);
  if (breg)
    fprintf (file, "(%s)", reg_names[REGNO (breg)]);
  if (ireg)
    fprintf (file, "[%s*%d]", reg_names[REGNO (ireg)], INTVAL (scale));
}

/* GO_IF_LEGITIMATE_ADDRESS recognizes an RTL expression
   that is a valid memory address for an instruction.
   The MODE argument is the machine mode for the MEM expression
   that wants to use this address.

	On 80960, legitimate addresses are:
		base				ld	(g0),r0
		disp	(12 or 32 bit)		ld	foo,r0
		base + index			ld	(g0)[g1*1],r0
		base + displ			ld	0xf00(g0),r0
		base + index*scale + displ	ld	0xf00(g0)[g1*4],r0
		index*scale + base		ld	(g0)[g1*4],r0
		index*scale + displ		ld	0xf00[g1*4],r0
		index*scale			ld	[g1*4],r0
		index + base + displ		ld	0xf00(g0)[g1*1],r0

	In each case, scale can be 1, 2, 4, 8, or 16.  */

/* This is exactly the same as i960_print_operand_addr, except that
   it recognizes addresses instead of printing them.

   It only recognizes address in canonical form.  LEGITIMIZE_ADDRESS should
   convert common non-canonical forms to canonical form so that they will
   be recognized.  */

/* These two macros allow us to accept either a REG or a SUBREG anyplace
   where a register is valid.  */

#define RTX_OK_FOR_BASE_P(X, STRICT)					\
  ((GET_CODE (X) == REG							\
    && (STRICT ? REG_OK_FOR_BASE_P_STRICT (X) : REG_OK_FOR_BASE_P (X)))	\
   || (GET_CODE (X) == SUBREG						\
       && GET_CODE (SUBREG_REG (X)) == REG				\
       && (STRICT ? REG_OK_FOR_BASE_P_STRICT (SUBREG_REG (X))		\
	   : REG_OK_FOR_BASE_P (SUBREG_REG (X)))))

#define RTX_OK_FOR_INDEX_P(X, STRICT)					\
  ((GET_CODE (X) == REG							\
    && (STRICT ? REG_OK_FOR_INDEX_P_STRICT (X) : REG_OK_FOR_INDEX_P (X)))\
   || (GET_CODE (X) == SUBREG						\
       && GET_CODE (SUBREG_REG (X)) == REG				\
       && (STRICT ? REG_OK_FOR_INDEX_P_STRICT (SUBREG_REG (X))		\
	   : REG_OK_FOR_INDEX_P (SUBREG_REG (X)))))

int
legitimate_address_p (mode, addr, strict)
     enum machine_mode mode ATTRIBUTE_UNUSED;
     register rtx addr;
     int strict;
{
  if (RTX_OK_FOR_BASE_P (addr, strict))
    return 1;
  else if (CONSTANT_P (addr))
    return 1;
  else if (GET_CODE (addr) == PLUS)
    {
      rtx op0, op1;

      if (! TARGET_COMPLEX_ADDR && ! reload_completed)
	return 0;

      op0 = XEXP (addr, 0);
      op1 = XEXP (addr, 1);

      if (RTX_OK_FOR_BASE_P (op0, strict))
	{
	  if (RTX_OK_FOR_INDEX_P (op1, strict))
	    return 1;
	  else if (CONSTANT_P (op1))
	    return 1;
	  else
	    return 0;
	}
      else if (GET_CODE (op0) == PLUS)
	{
	  if (GET_CODE (XEXP (op0, 0)) == MULT)
	    {
	      if (! (RTX_OK_FOR_INDEX_P (XEXP (XEXP (op0, 0), 0), strict)
		     && SCALE_TERM_P (XEXP (XEXP (op0, 0), 1))))
		return 0;

	      if (RTX_OK_FOR_BASE_P (XEXP (op0, 1), strict)
		  && CONSTANT_P (op1))
		return 1;
	      else
		return 0;
	    }
	  else if (RTX_OK_FOR_BASE_P (XEXP (op0, 0), strict))
	    {
	      if (RTX_OK_FOR_INDEX_P (XEXP (op0, 1), strict)
		  && CONSTANT_P (op1))
		return 1;
	      else
		return 0;
	    }
	  else
	    return 0;
	}
      else if (GET_CODE (op0) == MULT)
	{
	  if (! (RTX_OK_FOR_INDEX_P (XEXP (op0, 0), strict)
		 && SCALE_TERM_P (XEXP (op0, 1))))
	    return 0;

	  if (RTX_OK_FOR_BASE_P (op1, strict))
	    return 1;
	  else if (CONSTANT_P (op1))
	    return 1;
	  else
	    return 0;
	}
      else
	return 0;
    }
  else if (GET_CODE (addr) == MULT)
    {
      if (! TARGET_COMPLEX_ADDR && ! reload_completed)
	return 0;

      return (RTX_OK_FOR_INDEX_P (XEXP (addr, 0), strict)
	      && SCALE_TERM_P (XEXP (addr, 1)));
    }
  else
    return 0;
}

/* Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new, valid address.
   This macro is used in only one place: `memory_address' in explow.c.

   This converts some non-canonical addresses to canonical form so they
   can be recognized.  */

rtx
legitimize_address (x, oldx, mode)
     register rtx x;
     register rtx oldx ATTRIBUTE_UNUSED;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{ 
  if (GET_CODE (x) == SYMBOL_REF)
    {
      abort ();
      x = copy_to_reg (x);
    }

  if (! TARGET_COMPLEX_ADDR && ! reload_completed)
    return x;

  /* Canonicalize (plus (mult (reg) (const)) (plus (reg) (const)))
     into (plus (plus (mult (reg) (const)) (reg)) (const)).  This can be
     created by virtual register instantiation, register elimination, and
     similar optimizations.  */
  if (GET_CODE (x) == PLUS && GET_CODE (XEXP (x, 0)) == MULT
      && GET_CODE (XEXP (x, 1)) == PLUS)
    x = gen_rtx_PLUS (Pmode,
		      gen_rtx_PLUS (Pmode, XEXP (x, 0), XEXP (XEXP (x, 1), 0)),
		      XEXP (XEXP (x, 1), 1));

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
	constant = 0, other = 0;

      if (constant)
	x = gen_rtx_PLUS (Pmode,
			  gen_rtx_PLUS (Pmode, XEXP (XEXP (x, 0), 0),
					XEXP (XEXP (XEXP (x, 0), 1), 0)),
			  plus_constant (other, INTVAL (constant)));
    }

  return x;
}

#if 0
/* Return the most stringent alignment that we are willing to consider
   objects of size SIZE and known alignment ALIGN as having.  */
   
int
i960_alignment (size, align)
     int size;
     int align;
{
  int i;

  if (! TARGET_STRICT_ALIGN)
    if (TARGET_IC_COMPAT2_0 || align >= 4)
      {
	i = i960_object_bytes_bitalign (size) / BITS_PER_UNIT;
	if (i > align)
	  align = i;
      }

  return align;
}
#endif


int
hard_regno_mode_ok (regno, mode)
     int regno;
     enum machine_mode mode;
{
  if (regno < 32)
    {
      switch (mode)
	{
	case CCmode: case CC_UNSmode: case CC_CHKmode:
	  return 0;

	case DImode: case DFmode:
	  return (regno & 1) == 0;

	case TImode: case XFmode:
	  return (regno & 3) == 0;

	default:
	  return 1;
	}
    }
  else if (regno >= 32 && regno < 36)
    {
      switch (mode)
	{
	case SFmode: case DFmode: case XFmode:
	case SCmode: case DCmode:
	  return 1;

	default:
	  return 0;
	}
    }
  else if (regno == 36)
    {
      switch (mode)
	{
	case CCmode: case CC_UNSmode: case CC_CHKmode:
	  return 1;

	default:
	  return 0;
	}
    }
  else if (regno == 37)
    return 0;

  abort ();
}


/* Return the minimum alignment of an expression rtx X in bytes.  This takes
   advantage of machine specific facts, such as knowing that the frame pointer
   is always 16 byte aligned.  */

int
i960_expr_alignment (x, size)
     rtx x;
     int size;
{
  int align = 1;

  if (x == 0)
    return 1;

  switch (GET_CODE(x))
    {
    case CONST_INT:
      align = INTVAL(x);

      if ((align & 0xf) == 0)
	align = 16;
      else if ((align & 0x7) == 0)
	align = 8;
      else if ((align & 0x3) == 0)
	align = 4;
      else if ((align & 0x1) == 0)
	align = 2;
      else
	align = 1;
      break;

    case PLUS:
      align = MIN (i960_expr_alignment (XEXP (x, 0), size),
		   i960_expr_alignment (XEXP (x, 1), size));
      break;

    case SYMBOL_REF:
      /* If this is a valid program, objects are guaranteed to be
	 correctly aligned for whatever size the reference actually is.  */
      align = i960_object_bytes_bitalign (size) / BITS_PER_UNIT;
      break;

    case REG:
      if (REGNO (x) == FRAME_POINTER_REGNUM)
	align = 16;
      break;

    case ASHIFT:
      align = i960_expr_alignment (XEXP (x, 0), size);

      if (GET_CODE (XEXP (x, 1)) == CONST_INT)
	{
	  align = align << INTVAL (XEXP (x, 1));
	  align = MIN (align, 16);
	}
      break;

    case MULT:
      align = (i960_expr_alignment (XEXP (x, 0), size) *
	       i960_expr_alignment (XEXP (x, 1), size));

      align = MIN (align, 16);
      break;
    default:
      break;
    }

  return align;
}

/* Return true if it is possible to reference both BASE and OFFSET, which
   have alignment at least as great as 4 byte, as if they had alignment valid
   for an object of size SIZE.  */

int
i960_improve_align (base, offset, size)
     rtx base;
     rtx offset;
     int size;
{
  int i, j;

  /* We have at least a word reference to the object, so we know it has to
     be aligned at least to 4 bytes.  */

  i = MIN (i960_expr_alignment (base, 4),
	   i960_expr_alignment (offset, 4));

  i = MAX (i, 4);

  /* We know the size of the request.  If strict align is not enabled, we
     can guess that the alignment is OK for the requested size.  */

  if (! TARGET_STRICT_ALIGN)
    if ((j = (i960_object_bytes_bitalign (size) / BITS_PER_UNIT)) > i)
      i = j;

  return (i >= size);
}

/* Return true if it is possible to access BASE and OFFSET, which have 4 byte
   (SImode) alignment as if they had 16 byte (TImode) alignment.  */

int
i960_si_ti (base, offset)
     rtx base;
     rtx offset;
{
  return i960_improve_align (base, offset, 16);
}

/* Return true if it is possible to access BASE and OFFSET, which have 4 byte
   (SImode) alignment as if they had 8 byte (DImode) alignment.  */

int
i960_si_di (base, offset)
     rtx base;
     rtx offset;
{
  return i960_improve_align (base, offset, 8);
}

/* Return raw values of size and alignment (in words) for the data
   type being accessed.  These values will be rounded by the caller.  */

static void 
i960_arg_size_and_align (mode, type, size_out, align_out)
     enum machine_mode mode;
     tree type;
     int *size_out;
     int *align_out;
{
  int size, align;

  /* Use formal alignment requirements of type being passed, except make
     it at least a word.  If we don't have a type, this is a library call,
     and the parm has to be of scalar type.  In this case, consider its
     formal alignment requirement to be its size in words.  */

  if (mode == BLKmode)
    size = (int_size_in_bytes (type) + UNITS_PER_WORD - 1) / UNITS_PER_WORD;
  else if (mode == VOIDmode)
    {
      /* End of parm list.  */
      if (type == 0 || TYPE_MODE (type) != VOIDmode)
	abort ();
      size = 1;
    }
  else
    size = (GET_MODE_SIZE (mode) + UNITS_PER_WORD - 1) / UNITS_PER_WORD;

  if (type == 0)
    {
      /* ??? This is a hack to properly correct the alignment of XFmode
	 values without affecting anything else.  */
      if (size == 3)
	align = 4;
      else
	align = size;
    }
  else if (TYPE_ALIGN (type) >= BITS_PER_WORD)
    align = TYPE_ALIGN (type) / BITS_PER_WORD;
  else
    align = 1;

  *size_out  = size;
  *align_out = align;
}

/* On the 80960 the first 12 args are in registers and the rest are pushed.
   Any arg that is bigger than 4 words is placed on the stack and all
   subsequent arguments are placed on the stack.

   Additionally, parameters with an alignment requirement stronger than
   a word must be aligned appropriately.  Note that this means that a
   64 bit object with a 32 bit alignment is not 64 bit aligned and may be
   passed in an odd/even register pair.  */

/* Update CUM to advance past an argument described by MODE and TYPE.  */

void
i960_function_arg_advance (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int named ATTRIBUTE_UNUSED;
{
  int size, align;

  i960_arg_size_and_align (mode, type, &size, &align);

  if (size > 4 || cum->ca_nstackparms != 0
      || (size + ROUND_PARM (cum->ca_nregparms, align)) > NPARM_REGS
      || MUST_PASS_IN_STACK (mode, type))
    {
      /* Indicate that all the registers are in use, even if all are not,
	 so va_start will compute the right value.  */
      cum->ca_nregparms = NPARM_REGS;
      cum->ca_nstackparms = ROUND_PARM (cum->ca_nstackparms, align) + size;
    }
  else
    cum->ca_nregparms = ROUND_PARM (cum->ca_nregparms, align) + size;
}

/* Return the register that the argument described by MODE and TYPE is
   passed in, or else return 0 if it is passed on the stack.  */

rtx
i960_function_arg (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int named ATTRIBUTE_UNUSED;
{
  rtx ret;
  int size, align;

  if (mode == VOIDmode)
    return 0;

  i960_arg_size_and_align (mode, type, &size, &align);

  if (size > 4 || cum->ca_nstackparms != 0
      || (size + ROUND_PARM (cum->ca_nregparms, align)) > NPARM_REGS
      || MUST_PASS_IN_STACK (mode, type))
    {
      cum->ca_nstackparms = ROUND_PARM (cum->ca_nstackparms, align);
      ret = 0;
    }
  else
    {
      cum->ca_nregparms = ROUND_PARM (cum->ca_nregparms, align);
      ret = gen_rtx_REG (mode, cum->ca_nregparms);
    }

  return ret;
}

/* Return the number of bits that an object of size N bytes is aligned to.  */

int
i960_object_bytes_bitalign (n)
     int n;
{
  if (n > 8)      n = 128;
  else if (n > 4) n = 64;
  else if (n > 2) n = 32;
  else if (n > 1) n = 16;
  else            n = 8;

  return n;
}

/* Compute the alignment for an aggregate type TSIZE.
   Alignment is MAX (greatest member alignment,
                     MIN (pragma align, structure size alignment)).  */

int
i960_round_align (align, tsize)
     int align;
     tree tsize;
{
  int new_align;

  if (! tsize || TREE_CODE (tsize) != INTEGER_CST)
    return align;

  new_align = i960_object_bytes_bitalign (TREE_INT_CST_LOW (tsize)
					  / BITS_PER_UNIT);
  /* Handle #pragma align.  */
  if (new_align > i960_maxbitalignment)
    new_align = i960_maxbitalignment;

  if (align < new_align)
    align = new_align;

  return align;
}

/* Do any needed setup for a varargs function.  For the i960, we must
   create a register parameter block if one doesn't exist, and then copy
   all register parameters to memory.  */

void
i960_setup_incoming_varargs (cum, mode, type, pretend_size, no_rtl)
     CUMULATIVE_ARGS *cum;
     enum machine_mode mode ATTRIBUTE_UNUSED;
     tree type ATTRIBUTE_UNUSED;
     int *pretend_size ATTRIBUTE_UNUSED;
     int no_rtl;
{
  /* Note: for a varargs fn with only a va_alist argument, this is 0.  */
  int first_reg = cum->ca_nregparms;

  /* Copy only unnamed register arguments to memory.  If there are
     any stack parms, there are no unnamed arguments in registers, and
     an argument block was already allocated by the caller.
     Remember that any arg bigger than 4 words is passed on the stack as
     are all subsequent args.

     If there are no stack arguments but there are exactly NPARM_REGS
     registers, either there were no extra arguments or the caller
     allocated an argument block.  */

  if (cum->ca_nstackparms == 0 && first_reg < NPARM_REGS && !no_rtl)
    {
      rtx label = gen_label_rtx ();
      rtx regblock;

      /* If arg_pointer_rtx == 0, no arguments were passed on the stack
	 and we need to allocate a chunk to save the registers (if any
	 arguments were passed on the stack the caller would allocate the
	 48 bytes as well).  We must allocate all 48 bytes (12*4) because
	 va_start assumes it.  */
      emit_insn (gen_cmpsi (arg_pointer_rtx, const0_rtx));
      emit_jump_insn (gen_bne (label));
      emit_insn (gen_rtx_SET (VOIDmode, arg_pointer_rtx,
			      stack_pointer_rtx));
      emit_insn (gen_rtx_SET (VOIDmode, stack_pointer_rtx,
			      memory_address (SImode,
					      plus_constant (stack_pointer_rtx,
							     48))));
      emit_label (label);

      /* ??? Note that we unnecessarily store one extra register for stdarg
	 fns.  We could optimize this, but it's kept as for now.  */
      regblock = gen_rtx_MEM (BLKmode,
			      plus_constant (arg_pointer_rtx, first_reg * 4));
      set_mem_alias_set (regblock, get_varargs_alias_set ());
      set_mem_align (regblock, BITS_PER_WORD);
      move_block_from_reg (first_reg, regblock,
			   NPARM_REGS - first_reg,
			   (NPARM_REGS - first_reg) * UNITS_PER_WORD);
    }
}

/* Define the `__builtin_va_list' type for the ABI.  */

tree
i960_build_va_list ()
{
  return build_array_type (unsigned_type_node,
			   build_index_type (size_one_node));
}

/* Implement `va_start' for varargs and stdarg.  */

void
i960_va_start (stdarg_p, valist, nextarg)
     int stdarg_p ATTRIBUTE_UNUSED;
     tree valist;
     rtx nextarg ATTRIBUTE_UNUSED;
{
  tree s, t, base, num;

  /* The array type always decays to a pointer before we get here, so we
     can't use ARRAY_REF.  */
  base = build1 (INDIRECT_REF, unsigned_type_node, valist);
  num = build1 (INDIRECT_REF, unsigned_type_node,
		build (PLUS_EXPR, unsigned_type_node, valist,
		       TYPE_SIZE_UNIT (TREE_TYPE (valist))));

  s = make_tree (unsigned_type_node, arg_pointer_rtx);
  t = build (MODIFY_EXPR, unsigned_type_node, base, s);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  s = build_int_2 ((current_function_args_info.ca_nregparms
		    + current_function_args_info.ca_nstackparms) * 4, 0);
  t = build (MODIFY_EXPR, unsigned_type_node, num, s);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
}

/* Implement `va_arg'.  */

rtx
i960_va_arg (valist, type)
     tree valist, type;
{
  HOST_WIDE_INT siz, ali;
  tree base, num, pad, next, this, t1, t2, int48;
  rtx addr_rtx;

  /* The array type always decays to a pointer before we get here, so we
     can't use ARRAY_REF.  */
  base = build1 (INDIRECT_REF, unsigned_type_node, valist);
  num = build1 (INDIRECT_REF, unsigned_type_node,
		build (PLUS_EXPR, unsigned_type_node, valist,
		       TYPE_SIZE_UNIT (TREE_TYPE (valist))));

  /* Round up sizeof(type) to a word.  */
  siz = (int_size_in_bytes (type) + UNITS_PER_WORD - 1) & -UNITS_PER_WORD;

  /* Round up alignment to a word.  */
  ali = TYPE_ALIGN (type);
  if (ali < BITS_PER_WORD)
    ali = BITS_PER_WORD;
  ali /= BITS_PER_UNIT;

  /* Align NUM appropriate for the argument.  */
  pad = fold (build (PLUS_EXPR, unsigned_type_node, num, 
		      build_int_2 (ali - 1, 0)));
  pad = fold (build (BIT_AND_EXPR, unsigned_type_node, pad,
		      build_int_2 (-ali, -1)));
  pad = save_expr (pad);

  /* Increment VPAD past this argument.  */
  next = fold (build (PLUS_EXPR, unsigned_type_node, pad,
		      build_int_2 (siz, 0)));
  next = save_expr (next);

  /* Find the offset for the current argument.  Mind peculiar overflow
     from registers to stack.  */
  int48 = build_int_2 (48, 0);
  if (siz > 16)
    t2 = integer_one_node;
  else
    t2 = fold (build (GT_EXPR, integer_type_node, next, int48));
  t1 = fold (build (LE_EXPR, integer_type_node, num, int48));
  t1 = fold (build (TRUTH_AND_EXPR, integer_type_node, t1, t2));
  this = fold (build (COND_EXPR, unsigned_type_node, t1, int48, pad));

  /* Find the address for the current argument.  */
  t1 = fold (build (PLUS_EXPR, unsigned_type_node, base, this));
  t1 = build1 (NOP_EXPR, ptr_type_node, t1);
  addr_rtx = expand_expr (t1, NULL_RTX, Pmode, EXPAND_NORMAL);

  /* Increment NUM.  */
  t1 = build (MODIFY_EXPR, unsigned_type_node, num, next);
  TREE_SIDE_EFFECTS (t1) = 1;
  expand_expr (t1, const0_rtx, VOIDmode, EXPAND_NORMAL);
  
  return addr_rtx;
}

/* Calculate the final size of the reg parm stack space for the current
   function, based on how many bytes would be allocated on the stack.  */

int
i960_final_reg_parm_stack_space (const_size, var_size)
     int const_size;
     tree var_size;
{
  if (var_size || const_size > 48)
    return 48;
  else
    return 0;
}

/* Calculate the size of the reg parm stack space.  This is a bit complicated
   on the i960.  */

int
i960_reg_parm_stack_space (fndecl)
     tree fndecl;
{
  /* In this case, we are called from emit_library_call, and we don't need
     to pretend we have more space for parameters than what's apparent.  */
  if (fndecl == 0)
    return 0;

  /* In this case, we are called from locate_and_pad_parms when we're
     not IN_REGS, so we have an arg block.  */
  if (fndecl != current_function_decl)
    return 48;

  /* Otherwise, we have an arg block if the current function has more than
     48 bytes of parameters.  */
  if (current_function_args_size != 0 || VARARGS_STDARG_FUNCTION (fndecl))
    return 48;
  else
    return 0;
}

/* Return the register class of a scratch register needed to copy IN into
   or out of a register in CLASS in MODE.  If it can be done directly,
   NO_REGS is returned.  */

enum reg_class
secondary_reload_class (class, mode, in)
     enum reg_class class;
     enum machine_mode mode;
     rtx in;
{
  int regno = -1;

  if (GET_CODE (in) == REG || GET_CODE (in) == SUBREG)
    regno = true_regnum (in);

  /* We can place anything into LOCAL_OR_GLOBAL_REGS and can put
     LOCAL_OR_GLOBAL_REGS into anything.  */
  if (class == LOCAL_OR_GLOBAL_REGS || class == LOCAL_REGS
      || class == GLOBAL_REGS || (regno >= 0 && regno < 32))
    return NO_REGS;

  /* We can place any hard register, 0.0, and 1.0 into FP_REGS.  */
  if (class == FP_REGS
      && ((regno >= 0 && regno < FIRST_PSEUDO_REGISTER)
	  || in == CONST0_RTX (mode) || in == CONST1_RTX (mode)))
    return NO_REGS;

  return LOCAL_OR_GLOBAL_REGS;
}

/* Look at the opcode P, and set i96_last_insn_type to indicate which
   function unit it executed on.  */

/* ??? This would make more sense as an attribute.  */

void
i960_scan_opcode (p)
     const char *p;
{
  switch (*p)
    {
    case 'a':
    case 'd':
    case 'e':
    case 'm':
    case 'n':
    case 'o':
    case 'r':
      /* Ret is not actually of type REG, but it won't matter, because no
	 insn will ever follow it.  */
    case 'u':
    case 'x':
      i960_last_insn_type = I_TYPE_REG;
      break;

    case 'b':
      if (p[1] == 'x' || p[3] == 'x')
        i960_last_insn_type = I_TYPE_MEM;
      i960_last_insn_type = I_TYPE_CTRL;
      break;

    case 'f':
    case 't':
      i960_last_insn_type = I_TYPE_CTRL;
      break;

    case 'c':
      if (p[1] == 'a')
	{
	  if (p[4] == 'x')
	    i960_last_insn_type = I_TYPE_MEM;
	  else
	    i960_last_insn_type = I_TYPE_CTRL;
	}
      else if (p[1] == 'm')
	{
	  if (p[3] == 'd')
	    i960_last_insn_type = I_TYPE_REG;
	  else if (p[4] == 'b' || p[4] == 'j')
	    i960_last_insn_type = I_TYPE_CTRL;
	  else
	    i960_last_insn_type = I_TYPE_REG;
	}
      else
        i960_last_insn_type = I_TYPE_REG;
      break;

    case 'l':
      i960_last_insn_type = I_TYPE_MEM;
      break;

    case 's':
      if (p[1] == 't')
        i960_last_insn_type = I_TYPE_MEM;
      else
        i960_last_insn_type = I_TYPE_REG;
      break;
    }
}
