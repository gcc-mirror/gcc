/* Subroutines used for code generation on the DEC Alpha.
   Copyright (C) 1992, 93, 94, 95, 96, 97, 1998 Free Software Foundation, Inc.
   Contributed by Richard Kenner (kenner@vlsi1.ultra.nyu.edu)

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
#include <stdio.h>
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
#include "reload.h"
#include "expr.h"
#include "obstack.h"
#include "tree.h"

/* Specify which cpu to schedule for. */
enum processor_type alpha_cpu;

/* Specify how accurate floating-point traps need to be.  */

enum alpha_trap_precision alpha_tp;

/* Specify the floating-point rounding mode.  */

enum alpha_fp_rounding_mode alpha_fprm;

/* Specify which things cause traps.  */

enum alpha_fp_trap_mode alpha_fptm;

/* Strings decoded into the above options.  */
char *alpha_cpu_string;		/* -mcpu=ev[4|5] */
char *alpha_tp_string;		/* -mtrap-precision=[p|s|i] */
char *alpha_fprm_string;	/* -mfp-rounding-mode=[n|m|c|d] */
char *alpha_fptm_string;	/* -mfp-trap-mode=[n|u|su|sui] */

/* Save information from a "cmpxx" operation until the branch or scc is
   emitted.  */

rtx alpha_compare_op0, alpha_compare_op1;
int alpha_compare_fp_p;

/* Save the name of the current function as used by the assembler.  This
   is used by the epilogue.  */

char *alpha_function_name;

/* Non-zero if inside of a function, because the Alpha asm can't
   handle .files inside of functions.  */

static int inside_function = FALSE;

/* Non-zero if an instruction that may cause a trap is pending.  */

static int trap_pending = 0;

/* Nonzero if the current function needs gp.  */

int alpha_function_needs_gp;

extern char *version_string;
extern int rtx_equal_function_value_matters;

/* Declarations of static functions.  */
static void alpha_set_memflags_1  PROTO((rtx, int, int, int));
static rtx alpha_emit_set_const_1 PROTO((rtx, enum machine_mode,
					 HOST_WIDE_INT, int));
static void add_long_const	PROTO((FILE *, HOST_WIDE_INT, int, int, int));

/* Compute the size of the save area in the stack.  */
static void alpha_sa_mask	PROTO((unsigned long *imaskP,
				       unsigned long *fmaskP));

/* Get the number of args of a function in one of two ways.  */
#ifdef OPEN_VMS
#define NUM_ARGS current_function_args_info.num_args
#else
#define NUM_ARGS current_function_args_info
#endif

/* Parse target option strings. */

void
override_options ()
{
  alpha_cpu
    = TARGET_CPU_DEFAULT & MASK_CPU_EV6 ? PROCESSOR_EV6
      : (TARGET_CPU_DEFAULT & MASK_CPU_EV5 ? PROCESSOR_EV5 : PROCESSOR_EV4);

  if (alpha_cpu_string)
    {
      if (! strcmp (alpha_cpu_string, "ev4")
	  || ! strcmp (alpha_cpu_string, "21064"))
	{
	  alpha_cpu = PROCESSOR_EV4;
	  target_flags &= ~ (MASK_BWX | MASK_CIX | MASK_MAX);
	}
      else if (! strcmp (alpha_cpu_string, "ev5")
	       || ! strcmp (alpha_cpu_string, "21164"))
	{
	  alpha_cpu = PROCESSOR_EV5;
	  target_flags &= ~ (MASK_BWX | MASK_CIX | MASK_MAX);
	}
      else if (! strcmp (alpha_cpu_string, "ev56")
	       || ! strcmp (alpha_cpu_string, "21164a"))
	{
	  alpha_cpu = PROCESSOR_EV5;
	  target_flags |= MASK_BWX;
	  target_flags &= ~ (MASK_CIX | MASK_MAX);
	}
      else if (! strcmp (alpha_cpu_string, "pca56")
	       || ! strcmp (alpha_cpu_string, "21164PC"))
	{
	  alpha_cpu = PROCESSOR_EV5;
	  target_flags |= MASK_BWX | MASK_MAX;
	  target_flags &= ~ MASK_CIX;
	}
      else if (! strcmp (alpha_cpu_string, "ev6")
	       || ! strcmp (alpha_cpu_string, "21264"))
	{
	  alpha_cpu = PROCESSOR_EV6;
	  target_flags |= MASK_BWX | MASK_CIX | MASK_MAX;
	}
      else
	error ("bad value `%s' for -mcpu switch", alpha_cpu_string);
    }

  alpha_tp = ALPHA_TP_PROG;
  alpha_fprm = ALPHA_FPRM_NORM;
  alpha_fptm = ALPHA_FPTM_N;

  if (TARGET_IEEE)
    {
      alpha_tp = ALPHA_TP_INSN;
      alpha_fptm = ALPHA_FPTM_SU;
    }

  if (TARGET_IEEE_WITH_INEXACT)
    {
      alpha_tp = ALPHA_TP_INSN;
      alpha_fptm = ALPHA_FPTM_SUI;
    }

  if (alpha_tp_string)
    {
      if (! strcmp (alpha_tp_string, "p"))
	alpha_tp = ALPHA_TP_PROG;
      else if (! strcmp (alpha_tp_string, "f"))
	alpha_tp = ALPHA_TP_FUNC;
      else if (! strcmp (alpha_tp_string, "i"))
	alpha_tp = ALPHA_TP_INSN;
      else
	error ("bad value `%s' for -mtrap-precision switch", alpha_tp_string);
    }

  if (alpha_fprm_string)
    {
      if (! strcmp (alpha_fprm_string, "n"))
	alpha_fprm = ALPHA_FPRM_NORM;
      else if (! strcmp (alpha_fprm_string, "m"))
	alpha_fprm = ALPHA_FPRM_MINF;
      else if (! strcmp (alpha_fprm_string, "c"))
	alpha_fprm = ALPHA_FPRM_CHOP;
      else if (! strcmp (alpha_fprm_string,"d"))
	alpha_fprm = ALPHA_FPRM_DYN;
      else
	error ("bad value `%s' for -mfp-rounding-mode switch",
	       alpha_fprm_string);
    }

  if (alpha_fptm_string)
    {
      if (strcmp (alpha_fptm_string, "n") == 0)
	alpha_fptm = ALPHA_FPTM_N;
      else if (strcmp (alpha_fptm_string, "u") == 0)
	alpha_fptm = ALPHA_FPTM_U;
      else if (strcmp (alpha_fptm_string, "su") == 0)
	alpha_fptm = ALPHA_FPTM_SU;
      else if (strcmp (alpha_fptm_string, "sui") == 0)
	alpha_fptm = ALPHA_FPTM_SUI;
      else
	error ("bad value `%s' for -mfp-trap-mode switch", alpha_fptm_string);
    }

  /* Do some sanity checks on the above option. */

  if ((alpha_fptm == ALPHA_FPTM_SU || alpha_fptm == ALPHA_FPTM_SUI)
      && alpha_tp != ALPHA_TP_INSN)
    {
      warning ("fp software completion requires -mtrap-precision=i");
      alpha_tp = ALPHA_TP_INSN;
    }

  if (TARGET_FLOAT_VAX)
    {
      if (alpha_fprm == ALPHA_FPRM_MINF || alpha_fprm == ALPHA_FPRM_DYN)
	{
	  warning ("rounding mode not supported for VAX floats");
	  alpha_fprm = ALPHA_FPRM_NORM;
	}
      if (alpha_fptm == ALPHA_FPTM_SUI)
	{
	  warning ("trap mode not supported for VAX floats");
	  alpha_fptm = ALPHA_FPTM_SU;
	}
    }
}

/* Returns 1 if VALUE is a mask that contains full bytes of zero or ones.  */

int
zap_mask (value)
     HOST_WIDE_INT value;
{
  int i;

  for (i = 0; i < HOST_BITS_PER_WIDE_INT / HOST_BITS_PER_CHAR;
       i++, value >>= 8)
    if ((value & 0xff) != 0 && (value & 0xff) != 0xff)
      return 0;

  return 1;
}

/* Returns 1 if OP is either the constant zero or a register.  If a
   register, it must be in the proper mode unless MODE is VOIDmode.  */

int
reg_or_0_operand (op, mode)
      register rtx op;
      enum machine_mode mode;
{
  return op == const0_rtx || register_operand (op, mode);
}

/* Return 1 if OP is a constant in the range of 0-63 (for a shift) or
   any register.  */

int
reg_or_6bit_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return ((GET_CODE (op) == CONST_INT
	   && (unsigned HOST_WIDE_INT) INTVAL (op) < 64)
	  || register_operand (op, mode));
}


/* Return 1 if OP is an 8-bit constant or any register.  */

int
reg_or_8bit_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return ((GET_CODE (op) == CONST_INT
	   && (unsigned HOST_WIDE_INT) INTVAL (op) < 0x100)
	  || register_operand (op, mode));
}

/* Return 1 if OP is an 8-bit constant.  */

int
cint8_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_INT
	  && (unsigned HOST_WIDE_INT) INTVAL (op) < 0x100);
}

/* Return 1 if the operand is a valid second operand to an add insn.  */

int
add_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == CONST_INT)
    return (CONST_OK_FOR_LETTER_P (INTVAL (op), 'K')
	    || CONST_OK_FOR_LETTER_P (INTVAL (op), 'L')
	    || CONST_OK_FOR_LETTER_P (INTVAL (op), 'O'));

  return register_operand (op, mode);
}

/* Return 1 if the operand is a valid second operand to a sign-extending
   add insn.  */

int
sext_add_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == CONST_INT)
    return ((unsigned HOST_WIDE_INT) INTVAL (op) < 255
	    || (unsigned HOST_WIDE_INT) (- INTVAL (op)) < 255);

  return register_operand (op, mode);
}

/* Return 1 if OP is the constant 4 or 8.  */

int
const48_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_INT
	  && (INTVAL (op) == 4 || INTVAL (op) == 8));
}

/* Return 1 if OP is a valid first operand to an AND insn.  */

int
and_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == CONST_DOUBLE && GET_MODE (op) == VOIDmode)
    return (zap_mask (CONST_DOUBLE_LOW (op))
	    && zap_mask (CONST_DOUBLE_HIGH (op)));

  if (GET_CODE (op) == CONST_INT)
    return ((unsigned HOST_WIDE_INT) INTVAL (op) < 0x100
	    || (unsigned HOST_WIDE_INT) ~ INTVAL (op) < 0x100
	    || zap_mask (INTVAL (op)));

  return register_operand (op, mode);
}

/* Return 1 if OP is a valid first operand to an IOR or XOR insn.  */

int
or_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == CONST_INT)
    return ((unsigned HOST_WIDE_INT) INTVAL (op) < 0x100
	    || (unsigned HOST_WIDE_INT) ~ INTVAL (op) < 0x100);

  return register_operand (op, mode);
}

/* Return 1 if OP is a constant that is the width, in bits, of an integral
   mode smaller than DImode.  */

int
mode_width_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_INT
	  && (INTVAL (op) == 8 || INTVAL (op) == 16 || INTVAL (op) == 32));
}

/* Return 1 if OP is a constant that is the width of an integral machine mode
   smaller than an integer.  */

int
mode_mask_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
#if HOST_BITS_PER_WIDE_INT == 32
  if (GET_CODE (op) == CONST_DOUBLE)
    return CONST_DOUBLE_HIGH (op) == 0 && CONST_DOUBLE_LOW (op) == -1;
#endif

  return (GET_CODE (op) == CONST_INT
	  && (INTVAL (op) == 0xff
	      || INTVAL (op) == 0xffff
#if HOST_BITS_PER_WIDE_INT == 64
	      || INTVAL (op) == 0xffffffff
#endif
	      ));
}

/* Return 1 if OP is a multiple of 8 less than 64.  */

int
mul8_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_INT
	  && (unsigned HOST_WIDE_INT) INTVAL (op) < 64
	  && (INTVAL (op) & 7) == 0);
}

/* Return 1 if OP is the constant zero in floating-point.  */

int
fp0_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return (GET_MODE (op) == mode
	  && GET_MODE_CLASS (mode) == MODE_FLOAT && op == CONST0_RTX (mode));
}

/* Return 1 if OP is the floating-point constant zero or a register.  */

int
reg_or_fp0_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return fp0_operand (op, mode) || register_operand (op, mode);
}

/* Return 1 if OP is a register or a constant integer.  */


int
reg_or_cint_operand (op, mode)
    register rtx op;
    enum machine_mode mode;
{
     return GET_CODE (op) == CONST_INT || register_operand (op, mode);
}

/* Return 1 if OP is something that can be reloaded into a register;
   if it is a MEM, it need not be valid.  */

int
some_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (mode != VOIDmode && GET_MODE (op) != VOIDmode && mode != GET_MODE (op))
    return 0;

  switch (GET_CODE (op))
    {
    case REG:  case MEM:  case CONST_DOUBLE:
    case CONST_INT:  case LABEL_REF:  case SYMBOL_REF:  case CONST:
      return 1;

    case SUBREG:
      return some_operand (SUBREG_REG (op), VOIDmode);
    }

  return 0;
}

/* Return 1 if OP is a valid operand for the source of a move insn.  */

int
input_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (mode != VOIDmode && GET_MODE (op) != VOIDmode && mode != GET_MODE (op))
    return 0;

  if (GET_MODE_CLASS (mode) == MODE_FLOAT && GET_MODE (op) != mode)
    return 0;

  switch (GET_CODE (op))
    {
    case LABEL_REF:
    case SYMBOL_REF:
    case CONST:
        /* This handles both the Windows/NT and OSF cases.  */
      return mode == ptr_mode || mode == DImode;

    case REG:
      return 1;

    case SUBREG:
      if (register_operand (op, mode))
	return 1;
      /* ... fall through ... */
    case MEM:
      return ((TARGET_BWX || (mode != HImode && mode != QImode))
	      && general_operand (op, mode));

    case CONST_DOUBLE:
      return GET_MODE_CLASS (mode) == MODE_FLOAT && op == CONST0_RTX (mode);

    case CONST_INT:
      return mode == QImode || mode == HImode || add_operand (op, mode);
    }

  return 0;
}

/* Return 1 if OP is a SYMBOL_REF for a function known to be in this
   file.  */

int
current_file_function_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == SYMBOL_REF
	  && ! profile_flag && ! profile_block_flag
	  && (SYMBOL_REF_FLAG (op)
	      || op == XEXP (DECL_RTL (current_function_decl), 0)));
}

/* Return 1 if OP is a valid operand for the MEM of a CALL insn.  */

int
call_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (mode != Pmode)
    return 0;

  return (GET_CODE (op) == SYMBOL_REF
	  || (GET_CODE (op) == REG && (TARGET_OPEN_VMS || REGNO (op) == 27)));
}

/* Return 1 if OP is a valid Alpha comparison operator.  Here we know which
   comparisons are valid in which insn.  */

int
alpha_comparison_operator (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  enum rtx_code code = GET_CODE (op);

  if (mode != GET_MODE (op) || GET_RTX_CLASS (code) != '<')
    return 0;

  return (code == EQ || code == LE || code == LT
	  || (mode == DImode && (code == LEU || code == LTU)));
}

/* Return 1 if OP is a valid Alpha swapped comparison operator.  */

int
alpha_swapped_comparison_operator (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  enum rtx_code code = GET_CODE (op);

  if (mode != GET_MODE (op) || GET_RTX_CLASS (code) != '<')
    return 0;

  code = swap_condition (code);
  return (code == EQ || code == LE || code == LT
	  || (mode == DImode && (code == LEU || code == LTU)));
}

/* Return 1 if OP is a signed comparison operation.  */

int
signed_comparison_operator (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  switch (GET_CODE (op))
    {
    case EQ:  case NE:  case LE:  case LT:  case GE:   case GT:
      return 1;
    }

  return 0;
}

/* Return 1 if this is a divide or modulus operator.  */

int
divmod_operator (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  switch (GET_CODE (op))
    {
    case DIV:  case MOD:  case UDIV:  case UMOD:
      return 1;
    }

  return 0;
}

/* Return 1 if this memory address is a known aligned register plus
   a constant.  It must be a valid address.  This means that we can do
   this as an aligned reference plus some offset.

   Take into account what reload will do.

   We could say that out-of-range stack slots are alignable, but that would
   complicate get_aligned_mem and it isn't worth the trouble since few
   functions have large stack space.  */

int
aligned_memory_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == SUBREG)
    {
      if (GET_MODE (op) != mode)
	return 0;
      op = SUBREG_REG (op);
      mode = GET_MODE (op);
    }

  if (reload_in_progress && GET_CODE (op) == REG
      && REGNO (op) >= FIRST_PSEUDO_REGISTER)
    op = reg_equiv_mem[REGNO (op)];

  if (GET_CODE (op) != MEM || GET_MODE (op) != mode
      || ! memory_address_p (mode, XEXP (op, 0)))
    return 0;

  op = XEXP (op, 0);

  if (GET_CODE (op) == PLUS)
    op = XEXP (op, 0);

  return (GET_CODE (op) == REG
	  && REGNO_POINTER_ALIGN (REGNO (op)) >= 4);
}

/* Similar, but return 1 if OP is a MEM which is not alignable.  */

int
unaligned_memory_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == SUBREG)
    {
      if (GET_MODE (op) != mode)
	return 0;
      op = SUBREG_REG (op);
      mode = GET_MODE (op);
    }

  if (reload_in_progress && GET_CODE (op) == REG
      && REGNO (op) >= FIRST_PSEUDO_REGISTER)
    op = reg_equiv_mem[REGNO (op)];

  if (GET_CODE (op) != MEM || GET_MODE (op) != mode)
    return 0;

  op = XEXP (op, 0);

  if (! memory_address_p (mode, op))
    return 1;

  if (GET_CODE (op) == PLUS)
    op = XEXP (op, 0);

  return (GET_CODE (op) != REG
	  || REGNO_POINTER_ALIGN (REGNO (op)) < 4);
}

/* Return 1 if OP is either a register or an unaligned memory location.  */

int
reg_or_unaligned_mem_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return register_operand (op, mode) || unaligned_memory_operand (op, mode);
}

/* Return 1 if OP is any memory location.  During reload a pseudo matches.  */

int
any_memory_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == MEM
	  || (GET_CODE (op) == SUBREG && GET_CODE (SUBREG_REG (op)) == REG)
	  || (reload_in_progress && GET_CODE (op) == REG
	      && REGNO (op) >= FIRST_PSEUDO_REGISTER)
	  || (reload_in_progress && GET_CODE (op) == SUBREG
	      && GET_CODE (SUBREG_REG (op)) == REG
	      && REGNO (SUBREG_REG (op)) >= FIRST_PSEUDO_REGISTER));
}

/* REF is an alignable memory location.  Place an aligned SImode
   reference into *PALIGNED_MEM and the number of bits to shift into
   *PBITNUM.  */

void
get_aligned_mem (ref, paligned_mem, pbitnum)
     rtx ref;
     rtx *paligned_mem, *pbitnum;
{
  rtx base;
  HOST_WIDE_INT offset = 0;

  if (GET_CODE (ref) == SUBREG)
    {
      offset = SUBREG_WORD (ref) * UNITS_PER_WORD;
      if (BYTES_BIG_ENDIAN)
	offset -= (MIN (UNITS_PER_WORD, GET_MODE_SIZE (GET_MODE (ref)))
		   - MIN (UNITS_PER_WORD,
			  GET_MODE_SIZE (GET_MODE (SUBREG_REG (ref)))));
      ref = SUBREG_REG (ref);
    }

  if (GET_CODE (ref) == REG)
    ref = reg_equiv_mem[REGNO (ref)];

  if (reload_in_progress)
    base = find_replacement (&XEXP (ref, 0));
  else
    base = XEXP (ref, 0);

  if (GET_CODE (base) == PLUS)
    offset += INTVAL (XEXP (base, 1)), base = XEXP (base, 0);

  *paligned_mem = gen_rtx (MEM, SImode,
			   plus_constant (base, offset & ~3));
  MEM_IN_STRUCT_P (*paligned_mem) = MEM_IN_STRUCT_P (ref);
  MEM_VOLATILE_P (*paligned_mem) = MEM_VOLATILE_P (ref);
  RTX_UNCHANGING_P (*paligned_mem) = RTX_UNCHANGING_P (ref);

  *pbitnum = GEN_INT ((offset & 3) * 8);
}

/* Similar, but just get the address.  Handle the two reload cases.  
   Add EXTRA_OFFSET to the address we return.  */

rtx
get_unaligned_address (ref, extra_offset)
     rtx ref;
     int extra_offset;
{
  rtx base;
  HOST_WIDE_INT offset = 0;

  if (GET_CODE (ref) == SUBREG)
    {
      offset = SUBREG_WORD (ref) * UNITS_PER_WORD;
      if (BYTES_BIG_ENDIAN)
	offset -= (MIN (UNITS_PER_WORD, GET_MODE_SIZE (GET_MODE (ref)))
		   - MIN (UNITS_PER_WORD,
			  GET_MODE_SIZE (GET_MODE (SUBREG_REG (ref)))));
      ref = SUBREG_REG (ref);
    }

  if (GET_CODE (ref) == REG)
    ref = reg_equiv_mem[REGNO (ref)];

  if (reload_in_progress)
    base = find_replacement (&XEXP (ref, 0));
  else
    base = XEXP (ref, 0);

  if (GET_CODE (base) == PLUS)
    offset += INTVAL (XEXP (base, 1)), base = XEXP (base, 0);

  return plus_constant (base, offset + extra_offset);
}

/* Subfunction of the following function.  Update the flags of any MEM
   found in part of X.  */

static void
alpha_set_memflags_1 (x, in_struct_p, volatile_p, unchanging_p)
     rtx x;
     int in_struct_p, volatile_p, unchanging_p;
{
  int i;

  switch (GET_CODE (x))
    {
    case SEQUENCE:
    case PARALLEL:
      for (i = XVECLEN (x, 0) - 1; i >= 0; i--)
	alpha_set_memflags_1 (XVECEXP (x, 0, i), in_struct_p, volatile_p,
			      unchanging_p);
      break;

    case INSN:
      alpha_set_memflags_1 (PATTERN (x), in_struct_p, volatile_p,
			    unchanging_p);
      break;

    case SET:
      alpha_set_memflags_1 (SET_DEST (x), in_struct_p, volatile_p,
			    unchanging_p);
      alpha_set_memflags_1 (SET_SRC (x), in_struct_p, volatile_p,
			    unchanging_p);
      break;

    case MEM:
      MEM_IN_STRUCT_P (x) = in_struct_p;
      MEM_VOLATILE_P (x) = volatile_p;
      RTX_UNCHANGING_P (x) = unchanging_p;
      break;
    }
}

/* Given INSN, which is either an INSN or a SEQUENCE generated to
   perform a memory operation, look for any MEMs in either a SET_DEST or
   a SET_SRC and copy the in-struct, unchanging, and volatile flags from
   REF into each of the MEMs found.  If REF is not a MEM, don't do
   anything.  */

void
alpha_set_memflags (insn, ref)
     rtx insn;
     rtx ref;
{
  /* Note that it is always safe to get these flags, though they won't
     be what we think if REF is not a MEM.  */
  int in_struct_p = MEM_IN_STRUCT_P (ref);
  int volatile_p = MEM_VOLATILE_P (ref);
  int unchanging_p = RTX_UNCHANGING_P (ref);

  if (GET_CODE (ref) != MEM
      || (! in_struct_p && ! volatile_p && ! unchanging_p))
    return;

  alpha_set_memflags_1 (insn, in_struct_p, volatile_p, unchanging_p);
}

/* Try to output insns to set TARGET equal to the constant C if it can be
   done in less than N insns.  Do all computations in MODE.  Returns the place
   where the output has been placed if it can be done and the insns have been
   emitted.  If it would take more than N insns, zero is returned and no
   insns and emitted.  */

rtx
alpha_emit_set_const (target, mode, c, n)
     rtx target;
     enum machine_mode mode;
     HOST_WIDE_INT c;
     int n;
{
  rtx pat;
  int i;

  /* Try 1 insn, then 2, then up to N. */
  for (i = 1; i <= n; i++)
    if ((pat = alpha_emit_set_const_1 (target, mode, c, i)) != 0)
      return pat;

  return 0;
}

/* Internal routine for the above to check for N or below insns.  */

static rtx
alpha_emit_set_const_1 (target, mode, c, n)
     rtx target;
     enum machine_mode mode;
     HOST_WIDE_INT c;
     int n;
{
  HOST_WIDE_INT new = c;
  int i, bits;
  /* Use a pseudo if highly optimizing and still generating RTL.  */
  rtx subtarget
    = (flag_expensive_optimizations && rtx_equal_function_value_matters
       ? 0 : target);
  rtx temp;

#if HOST_BITS_PER_WIDE_INT == 64
  /* We are only called for SImode and DImode.  If this is SImode, ensure that
     we are sign extended to a full word.  This does not make any sense when
     cross-compiling on a narrow machine.  */

  if (mode == SImode)
    c = (c & 0xffffffff) - 2 * (c & 0x80000000);
#endif

  /* If this is a sign-extended 32-bit constant, we can do this in at most
     three insns, so do it if we have enough insns left.  We always have
     a sign-extended 32-bit constant when compiling on a narrow machine. 
     Note that we cannot handle the constant 0x80000000.  */

  if ((HOST_BITS_PER_WIDE_INT != 64
       || c >> 31 == -1 || c >> 31 == 0)
      && c != 0x80000000U)
    {
      HOST_WIDE_INT low = (c & 0xffff) - 2 * (c & 0x8000);
      HOST_WIDE_INT tmp1 = c - low;
      HOST_WIDE_INT high
	= ((tmp1 >> 16) & 0xffff) - 2 * ((tmp1 >> 16) & 0x8000);
      HOST_WIDE_INT extra = 0;

      /* If HIGH will be interpreted as negative but the constant is
	 positive, we must adjust it to do two ldha insns.  */

      if ((high & 0x8000) != 0 && c >= 0)
	{
	  extra = 0x4000;
	  tmp1 -= 0x40000000;
	  high = ((tmp1 >> 16) & 0xffff) - 2 * ((tmp1 >> 16) & 0x8000);
	}

      if (c == low || (low == 0 && extra == 0))
	return copy_to_suggested_reg (GEN_INT (c), target, mode);
      else if (n >= 2 + (extra != 0))
	{
	  temp = copy_to_suggested_reg (GEN_INT (low), subtarget, mode);

	  if (extra != 0)
	    temp = expand_binop (mode, add_optab, temp, GEN_INT (extra << 16),
				 subtarget, 0, OPTAB_WIDEN);

	  return expand_binop (mode, add_optab, temp, GEN_INT (high << 16),
			       target, 0, OPTAB_WIDEN);
	}
    }

  /* If we couldn't do it that way, try some other methods.  But if we have
     no instructions left, don't bother.  Likewise, if this is SImode and
     we can't make pseudos, we can't do anything since the expand_binop
     and expand_unop calls will widen and try to make pseudos.  */

  if (n == 1
      || (mode == SImode && ! rtx_equal_function_value_matters))
    return 0;

#if HOST_BITS_PER_WIDE_INT == 64
  /* First, see if can load a value into the target that is the same as the
     constant except that all bytes that are 0 are changed to be 0xff.  If we
     can, then we can do a ZAPNOT to obtain the desired constant.  */

  for (i = 0; i < 64; i += 8)
    if ((new & ((HOST_WIDE_INT) 0xff << i)) == 0)
      new |= (HOST_WIDE_INT) 0xff << i;

  /* We are only called for SImode and DImode.  If this is SImode, ensure that
     we are sign extended to a full word.  */

  if (mode == SImode)
    new = (new & 0xffffffff) - 2 * (new & 0x80000000);

  if (new != c
      && (temp = alpha_emit_set_const (subtarget, mode, new, n - 1)) != 0)
    return expand_binop (mode, and_optab, temp, GEN_INT (c | ~ new),
			 target, 0, OPTAB_WIDEN);
#endif

  /* Next, see if we can load a related constant and then shift and possibly
     negate it to get the constant we want.  Try this once each increasing
     numbers of insns.  */

  for (i = 1; i < n; i++)
    {
      /* First try complementing.  */
      if ((temp = alpha_emit_set_const (subtarget, mode, ~ c, i)) != 0)
	return expand_unop (mode, one_cmpl_optab, temp, target, 0);

      /* Next try to form a constant and do a left shift.  We can do this
	 if some low-order bits are zero; the exact_log2 call below tells
	 us that information.  The bits we are shifting out could be any
	 value, but here we'll just try the 0- and sign-extended forms of
	 the constant.  To try to increase the chance of having the same
	 constant in more than one insn, start at the highest number of
	 bits to shift, but try all possibilities in case a ZAPNOT will
	 be useful.  */

      if ((bits = exact_log2 (c & - c)) > 0)
	for (; bits > 0; bits--)
	  if ((temp = (alpha_emit_set_const
		       (subtarget, mode,
			(unsigned HOST_WIDE_INT) c >> bits, i))) != 0
	      || ((temp = (alpha_emit_set_const
			  (subtarget, mode,
			   ((unsigned HOST_WIDE_INT) c) >> bits, i)))
		  != 0))
	    return expand_binop (mode, ashl_optab, temp, GEN_INT (bits),
				 target, 0, OPTAB_WIDEN);

      /* Now try high-order zero bits.  Here we try the shifted-in bits as
	 all zero and all ones.  Be careful to avoid shifting outside the
	 mode and to avoid shifting outside the host wide int size.  */

      if ((bits = (MIN (HOST_BITS_PER_WIDE_INT, GET_MODE_SIZE (mode) * 8)
		   - floor_log2 (c) - 1)) > 0)
	for (; bits > 0; bits--)
	  if ((temp = alpha_emit_set_const (subtarget, mode,
					    c << bits, i)) != 0
	      || ((temp = (alpha_emit_set_const
			   (subtarget, mode,
			    ((c << bits) | (((HOST_WIDE_INT) 1 << bits) - 1)),
			    i)))
		  != 0))
	    return expand_binop (mode, lshr_optab, temp, GEN_INT (bits),
				 target, 1, OPTAB_WIDEN);

      /* Now try high-order 1 bits.  We get that with a sign-extension.
	 But one bit isn't enough here.  Be careful to avoid shifting outside
	 the mode and to avoid shifting outside the host wide int size. */
      
      if ((bits = (MIN (HOST_BITS_PER_WIDE_INT, GET_MODE_SIZE (mode) * 8)
		   - floor_log2 (~ c) - 2)) > 0)
	for (; bits > 0; bits--)
	  if ((temp = alpha_emit_set_const (subtarget, mode,
					    c << bits, i)) != 0
	      || ((temp = (alpha_emit_set_const
			   (subtarget, mode,
			    ((c << bits) | (((HOST_WIDE_INT) 1 << bits) - 1)),
			    i)))
		  != 0))
	    return expand_binop (mode, ashr_optab, temp, GEN_INT (bits),
				 target, 0, OPTAB_WIDEN);
    }

  return 0;
}

#if HOST_BITS_PER_WIDE_INT == 64
/* Having failed to find a 3 insn sequence in alpha_emit_set_const,
   fall back to a straight forward decomposition.  We do this to avoid
   exponential run times encountered when looking for longer sequences
   with alpha_emit_set_const.  */

rtx
alpha_emit_set_long_const (target, c)
     rtx target;
     HOST_WIDE_INT c;
{
  /* Use a pseudo if highly optimizing and still generating RTL.  */
  rtx subtarget
    = (flag_expensive_optimizations && rtx_equal_function_value_matters
       ? 0 : target);
  HOST_WIDE_INT d1, d2, d3, d4;
  rtx r1, r2;

  /* Decompose the entire word */
  d1 = ((c & 0xffff) ^ 0x8000) - 0x8000;
  c -= d1;
  d2 = ((c & 0xffffffff) ^ 0x80000000) - 0x80000000;
  c = (c - d2) >> 32;
  d3 = ((c & 0xffff) ^ 0x8000) - 0x8000;
  c -= d3;
  d4 = ((c & 0xffffffff) ^ 0x80000000) - 0x80000000;

  if (c - d4 != 0)
    abort();

  /* Construct the high word */
  if (d3 == 0)
    r1 = copy_to_suggested_reg (GEN_INT (d4), subtarget, DImode);
  else if (d4 == 0)
    r1 = copy_to_suggested_reg (GEN_INT (d3), subtarget, DImode);
  else
    r1 = expand_binop (DImode, add_optab, GEN_INT (d3), GEN_INT (d4),
		       subtarget, 0, OPTAB_WIDEN);

  /* Shift it into place */
  r2 = expand_binop (DImode, ashl_optab, r1, GEN_INT (32), 
		     subtarget, 0, OPTAB_WIDEN);

  if (subtarget == 0 && d1 == d3 && d2 == d4)
    r1 = expand_binop (DImode, add_optab, r1, r2, subtarget, 0, OPTAB_WIDEN);
  else
    {
      r1 = r2;

      /* Add in the low word */
      if (d2 != 0)
	r1 = expand_binop (DImode, add_optab, r1, GEN_INT (d2),
		           subtarget, 0, OPTAB_WIDEN);
      if (d1 != 0)
	r1 = expand_binop (DImode, add_optab, r1, GEN_INT (d1),
		           subtarget, 0, OPTAB_WIDEN);
    }

  if (subtarget == 0)
    r1 = copy_to_suggested_reg(r1, target, DImode);

  return r1;
}
#endif /* HOST_BITS_PER_WIDE_INT == 64 */

/* Rewrite a comparison against zero CMP of the form
   (CODE (cc0) (const_int 0)) so it can be written validly in
   a conditional move (if_then_else CMP ...).
   If both of the operands that set cc0 are non-zero we must emit
   an insn to perform the compare (it can't be done within
   the conditional move). */
rtx
alpha_emit_conditional_move (cmp, mode)
     rtx cmp;
     enum machine_mode mode;
{
  enum rtx_code code = GET_CODE (cmp);
  enum rtx_code cmov_code = NE;
  rtx op0 = alpha_compare_op0;
  rtx op1 = alpha_compare_op1;
  enum machine_mode cmp_mode
    = (GET_MODE (op0) == VOIDmode ? DImode : GET_MODE (op0));
  enum machine_mode cmp_op_mode = alpha_compare_fp_p ? DFmode : DImode;
  rtx tem;

  if (alpha_compare_fp_p != FLOAT_MODE_P (mode))
    return 0;

  /* We may be able to use a conditional move directly.
     This avoids emitting spurious compares. */
  if (signed_comparison_operator (cmp, cmp_op_mode)
      && (op0 == CONST0_RTX (cmp_mode) || op1 == CONST0_RTX (cmp_mode)))
    return gen_rtx (code, VOIDmode, op0, op1);

  /* We can't put the comparison insides a conditional move;
     emit a compare instruction and put that inside the
     conditional move.  Make sure we emit only comparisons we have;
     swap or reverse as necessary.  */

  switch (code)
    {
    case EQ:  case LE:  case LT:  case LEU:  case LTU:
      /* We have these compares: */
      break;

    case NE:
      /* This must be reversed. */
      code = reverse_condition (code);
      cmov_code = EQ;
      break;

    case GE:  case GT:  case GEU:  case GTU:
      /* These must be swapped.  Make sure the new first operand is in
	 a register.  */
      code = swap_condition (code);
      tem = op0, op0 = op1, op1 = tem;
      op0 = force_reg (cmp_mode, op0);
      break;

    default:
      abort ();
    }

  tem = gen_reg_rtx (cmp_op_mode);
  emit_move_insn (tem, gen_rtx (code, cmp_op_mode, op0, op1));
  return gen_rtx (cmov_code, VOIDmode, tem, CONST0_RTX (cmp_op_mode));
}

/* Adjust the cost of a scheduling dependency.  Return the new cost of
   a dependency LINK or INSN on DEP_INSN.  COST is the current cost.  */

int
alpha_adjust_cost (insn, link, dep_insn, cost)
     rtx insn;
     rtx link;
     rtx dep_insn;
     int cost;
{
  rtx set;

  /* If the dependence is an anti-dependence, there is no cost.  For an
     output dependence, there is sometimes a cost, but it doesn't seem
     worth handling those few cases.  */

  if (REG_NOTE_KIND (link) != 0)
    return 0;

  /* EV5 costs are as given in alpha.md; exceptions are given here. */
  if (alpha_cpu == PROCESSOR_EV5)
    {
      /* And the lord DEC saith:  "A special bypass provides an effective
	 latency of 0 cycles for an ICMP or ILOG insn producing the test
	 operand of an IBR or CMOV insn." */
      if (recog_memoized (dep_insn) >= 0
	  && (get_attr_type (dep_insn) == TYPE_ICMP
	      || get_attr_type (dep_insn) == TYPE_ILOG)
	  && recog_memoized (insn) >= 0
	  && (get_attr_type (insn) == TYPE_IBR
	      || (get_attr_type (insn) == TYPE_CMOV
		  && !((set = single_set (dep_insn)) != 0
		       && GET_CODE (PATTERN (insn)) == SET
		       && GET_CODE (SET_SRC (PATTERN (insn))) == IF_THEN_ELSE
		       && (rtx_equal_p (SET_DEST (set),
					XEXP (SET_SRC (PATTERN (insn)), 1))
			   || rtx_equal_p (SET_DEST (set),
					   XEXP (SET_SRC (PATTERN (insn)), 2)))))))
	return 1;
      return cost;
    } 

  /* If INSN is a store insn and DEP_INSN is setting the data being stored,
     we can sometimes lower the cost.  */

  if (recog_memoized (insn) >= 0 && get_attr_type (insn) == TYPE_ST
      && (set = single_set (dep_insn)) != 0
      && GET_CODE (PATTERN (insn)) == SET
      && rtx_equal_p (SET_DEST (set), SET_SRC (PATTERN (insn))))
    switch (get_attr_type (dep_insn))
      {
      case TYPE_LD:
	/* No savings here.  */
	return cost;

      case TYPE_IMULL:
      case TYPE_IMULQ:
	/* In these cases, we save one cycle.  */
	return cost - 2;

      default:
	/* In all other cases, we save two cycles.  */
	return MAX (0, cost - 4);
      }

  /* Another case that needs adjustment is an arithmetic or logical
     operation.  It's cost is usually one cycle, but we default it to
     two in the MD file.  The only case that it is actually two is
     for the address in loads and stores.  */

  if (recog_memoized (dep_insn) >= 0
      && (get_attr_type (dep_insn) == TYPE_IADD
	  || get_attr_type (dep_insn) == TYPE_ILOG))
    switch (get_attr_type (insn))
      {
      case TYPE_LD:
      case TYPE_ST:
	return cost;

      default:
	return 2;
      }

  /* The final case is when a compare feeds into an integer branch.  The cost
     is only one cycle in that case.  */

  if (recog_memoized (dep_insn) >= 0
      && get_attr_type (dep_insn) == TYPE_ICMP
      && recog_memoized (insn) >= 0
      && get_attr_type (insn) == TYPE_IBR)
    return 2;

  /* Otherwise, return the default cost. */

  return cost;
}

/* Print an operand.  Recognize special options, documented below.  */

void
print_operand (file, x, code)
    FILE *file;
    rtx x;
    char code;
{
  int i;

  switch (code)
    {
    case '&':
      /* Generates fp-rounding mode suffix: nothing for normal, 'c' for
	 chopped, 'm' for minus-infinity, and 'd' for dynamic rounding
	 mode.  alpha_fprm controls which suffix is generated.  */
      switch (alpha_fprm)
	{
	case ALPHA_FPRM_NORM:
	  break;
	case ALPHA_FPRM_MINF: 
	  fputc ('m', file);
	  break;
	case ALPHA_FPRM_CHOP:
	  fputc ('c', file);
	  break;
	case ALPHA_FPRM_DYN:
	  fputc ('d', file);
	  break;
	}
      break;

    case '\'':
      /* Generates trap-mode suffix for instructions that accept the su
	 suffix only (cmpt et al).  */
      if (alpha_tp == ALPHA_TP_INSN)
	fputs ("su", file);
      break;

    case ')':
      /* Generates trap-mode suffix for instructions that accept the u, su,
	 and sui suffix.  This is the bulk of the IEEE floating point
	 instructions (addt et al).  */
      switch (alpha_fptm)
	{
	case ALPHA_FPTM_N:
	  break;
	case ALPHA_FPTM_U:
	  fputc ('u', file);
	  break;
	case ALPHA_FPTM_SU:
	  fputs ("su", file);
	  break;
	case ALPHA_FPTM_SUI:
	  fputs ("sui", file);
	  break;
	}
      break;

    case '+':
      /* Generates trap-mode suffix for instructions that accept the sui
	 suffix (cvtqt and cvtqs).  */
      switch (alpha_fptm)
	{
	case ALPHA_FPTM_N: case ALPHA_FPTM_U:
	case ALPHA_FPTM_SU:	/* cvtqt/cvtqs can't cause underflow */
	  break;
	case ALPHA_FPTM_SUI:
	  fputs ("sui", file);
	  break;
	}
      break;

    case ',':
      /* Generates single precision instruction suffix.  */
      fprintf (file, "%c", (TARGET_FLOAT_VAX ? 'f' : 's'));
      break;

    case '-':
      /* Generates double precision instruction suffix.  */
      fprintf (file, "%c", (TARGET_FLOAT_VAX ? 'g' : 't'));
      break;

    case 'r':
      /* If this operand is the constant zero, write it as "$31".  */
      if (GET_CODE (x) == REG)
	fprintf (file, "%s", reg_names[REGNO (x)]);
      else if (x == CONST0_RTX (GET_MODE (x)))
	fprintf (file, "$31");
      else
	output_operand_lossage ("invalid %%r value");

      break;

    case 'R':
      /* Similar, but for floating-point.  */
      if (GET_CODE (x) == REG)
	fprintf (file, "%s", reg_names[REGNO (x)]);
      else if (x == CONST0_RTX (GET_MODE (x)))
	fprintf (file, "$f31");
      else
	output_operand_lossage ("invalid %%R value");

      break;

    case 'N':
      /* Write the 1's complement of a constant.  */
      if (GET_CODE (x) != CONST_INT)
	output_operand_lossage ("invalid %%N value");

      fprintf (file, HOST_WIDE_INT_PRINT_DEC, ~ INTVAL (x));
      break;

    case 'P':
      /* Write 1 << C, for a constant C.  */
      if (GET_CODE (x) != CONST_INT)
	output_operand_lossage ("invalid %%P value");

      fprintf (file, HOST_WIDE_INT_PRINT_DEC, (HOST_WIDE_INT) 1 << INTVAL (x));
      break;

    case 'h':
      /* Write the high-order 16 bits of a constant, sign-extended.  */
      if (GET_CODE (x) != CONST_INT)
	output_operand_lossage ("invalid %%h value");

      fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (x) >> 16);
      break;

    case 'L':
      /* Write the low-order 16 bits of a constant, sign-extended.  */
      if (GET_CODE (x) != CONST_INT)
	output_operand_lossage ("invalid %%L value");

      fprintf (file, HOST_WIDE_INT_PRINT_DEC,
	       (INTVAL (x) & 0xffff) - 2 * (INTVAL (x) & 0x8000));
      break;

    case 'm':
      /* Write mask for ZAP insn.  */
      if (GET_CODE (x) == CONST_DOUBLE)
	{
	  HOST_WIDE_INT mask = 0;
	  HOST_WIDE_INT value;

	  value = CONST_DOUBLE_LOW (x);
	  for (i = 0; i < HOST_BITS_PER_WIDE_INT / HOST_BITS_PER_CHAR;
	       i++, value >>= 8)
	    if (value & 0xff)
	      mask |= (1 << i);

	  value = CONST_DOUBLE_HIGH (x);
	  for (i = 0; i < HOST_BITS_PER_WIDE_INT / HOST_BITS_PER_CHAR;
	       i++, value >>= 8)
	    if (value & 0xff)
	      mask |= (1 << (i + sizeof (int)));

	  fprintf (file, HOST_WIDE_INT_PRINT_DEC, mask & 0xff);
	}

      else if (GET_CODE (x) == CONST_INT)
	{
	  HOST_WIDE_INT mask = 0, value = INTVAL (x);

	  for (i = 0; i < 8; i++, value >>= 8)
	    if (value & 0xff)
	      mask |= (1 << i);

	  fprintf (file, HOST_WIDE_INT_PRINT_DEC, mask);
	}
      else
	output_operand_lossage ("invalid %%m value");
      break;

    case 'M':
      /* 'b', 'w', or 'l' as the value of the constant.  */
      if (GET_CODE (x) != CONST_INT
	  || (INTVAL (x) != 8 && INTVAL (x) != 16 && INTVAL (x) != 32))
	output_operand_lossage ("invalid %%M value");

      fprintf (file, "%s",
	       INTVAL (x) == 8 ? "b" : INTVAL (x) == 16 ? "w" : "l");
      break;

    case 'U':
      /* Similar, except do it from the mask.  */
      if (GET_CODE (x) == CONST_INT && INTVAL (x) == 0xff)
	fprintf (file, "b");
      else if (GET_CODE (x) == CONST_INT && INTVAL (x) == 0xffff)
	fprintf (file, "w");
#if HOST_BITS_PER_WIDE_INT == 32
      else if (GET_CODE (x) == CONST_DOUBLE
	       && CONST_DOUBLE_HIGH (x) == 0
	       && CONST_DOUBLE_LOW (x) == -1)
	fprintf (file, "l");
#else
      else if (GET_CODE (x) == CONST_INT && INTVAL (x) == 0xffffffff)
	fprintf (file, "l");
#endif
      else
	output_operand_lossage ("invalid %%U value");
      break;

    case 's':
      /* Write the constant value divided by 8.  */
      if (GET_CODE (x) != CONST_INT
	  && (unsigned HOST_WIDE_INT) INTVAL (x) >= 64
	  && (INTVAL (x) & 7) != 8)
	output_operand_lossage ("invalid %%s value");

      fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (x) / 8);
      break;

    case 'S':
      /* Same, except compute (64 - c) / 8 */

      if (GET_CODE (x) != CONST_INT
	  && (unsigned HOST_WIDE_INT) INTVAL (x) >= 64
	  && (INTVAL (x) & 7) != 8)
	output_operand_lossage ("invalid %%s value");

      fprintf (file, HOST_WIDE_INT_PRINT_DEC, (64 - INTVAL (x)) / 8);
      break;

    case 'C': case 'D': case 'c': case 'd':
      /* Write out comparison name.  */
      {
	enum rtx_code c = GET_CODE (x);

        if (GET_RTX_CLASS (c) != '<')
	  output_operand_lossage ("invalid %%C value");

	if (code == 'D')
	  c = reverse_condition (c);
	else if (code == 'c')
	  c = swap_condition (c);
	else if (code == 'd')
	  c = swap_condition (reverse_condition (c));

        if (c == LEU)
	  fprintf (file, "ule");
        else if (c == LTU)
	  fprintf (file, "ult");
        else
	  fprintf (file, "%s", GET_RTX_NAME (c));
      }
      break;

    case 'E':
      /* Write the divide or modulus operator.  */
      switch (GET_CODE (x))
	{
	case DIV:
	  fprintf (file, "div%s", GET_MODE (x) == SImode ? "l" : "q");
	  break;
	case UDIV:
	  fprintf (file, "div%su", GET_MODE (x) == SImode ? "l" : "q");
	  break;
	case MOD:
	  fprintf (file, "rem%s", GET_MODE (x) == SImode ? "l" : "q");
	  break;
	case UMOD:
	  fprintf (file, "rem%su", GET_MODE (x) == SImode ? "l" : "q");
	  break;
	default:
	  output_operand_lossage ("invalid %%E value");
	  break;
	}
      break;

    case 'A':
      /* Write "_u" for unaligned access.  */
      if (GET_CODE (x) == MEM && GET_CODE (XEXP (x, 0)) == AND)
	fprintf (file, "_u");
      break;

    case 0:
      if (GET_CODE (x) == REG)
	fprintf (file, "%s", reg_names[REGNO (x)]);
      else if (GET_CODE (x) == MEM)
	output_address (XEXP (x, 0));
      else
	output_addr_const (file, x);
      break;

    default:
      output_operand_lossage ("invalid %%xn code");
    }
}

/* Do what is necessary for `va_start'.  The argument is ignored;
   We look at the current function to determine if stdarg or varargs
   is used and fill in an initial va_list.  A pointer to this constructor
   is returned.  */

struct rtx_def *
alpha_builtin_saveregs (arglist)
     tree arglist;
{
  rtx block, addr, dest, argsize;
  tree fntype = TREE_TYPE (current_function_decl);
  int stdarg = (TYPE_ARG_TYPES (fntype) != 0
		&& (TREE_VALUE (tree_last (TYPE_ARG_TYPES (fntype)))
		    != void_type_node));

  /* Compute the current position into the args, taking into account
     both registers and memory.  Both of these are already included in
     NUM_ARGS.  */

  argsize = GEN_INT (NUM_ARGS * UNITS_PER_WORD);

  /* For Unix, SETUP_INCOMING_VARARGS moves the starting address base up by 48,
     storing fp arg registers in the first 48 bytes, and the integer arg
     registers in the next 48 bytes.  This is only done, however, if any
     integer registers need to be stored.

     If no integer registers need be stored, then we must subtract 48 in
     order to account for the integer arg registers which are counted in
     argsize above, but which are not actually stored on the stack.  */

  if (TARGET_OPEN_VMS)
    addr = plus_constant (virtual_incoming_args_rtx,
			  NUM_ARGS <= 5 + stdarg
			  ? UNITS_PER_WORD : - 6 * UNITS_PER_WORD);
  else
    addr = (NUM_ARGS <= 5 + stdarg
	    ? plus_constant (virtual_incoming_args_rtx,
			     6 * UNITS_PER_WORD)
	    : plus_constant (virtual_incoming_args_rtx,
			     - (6 * UNITS_PER_WORD)));

  /* For VMS, we include the argsize, while on Unix, it's handled as
     a separate field.  */
  if (TARGET_OPEN_VMS)
    addr = plus_constant (addr, INTVAL (argsize));

  addr = force_operand (addr, NULL_RTX);

#ifdef POINTERS_EXTEND_UNSIGNED
  addr = convert_memory_address (ptr_mode, addr);
#endif

  if (TARGET_OPEN_VMS)
    return addr;
  else
    {
      /* Allocate the va_list constructor */
      block = assign_stack_local (BLKmode, 2 * UNITS_PER_WORD, BITS_PER_WORD);
      RTX_UNCHANGING_P (block) = 1;
      RTX_UNCHANGING_P (XEXP (block, 0)) = 1;

      /* Store the address of the first integer register in the __base
	 member.  */

      dest = change_address (block, ptr_mode, XEXP (block, 0));
      emit_move_insn (dest, addr);

      if (flag_check_memory_usage)
	emit_library_call (chkr_set_right_libfunc, 1, VOIDmode, 3,
			   dest, ptr_mode,
			   GEN_INT (GET_MODE_SIZE (ptr_mode)),
			   TYPE_MODE (sizetype),
			   GEN_INT (MEMORY_USE_RW), 
			   TYPE_MODE (integer_type_node));
  
      /* Store the argsize as the __va_offset member.  */
      dest = change_address (block, TYPE_MODE (integer_type_node),
			     plus_constant (XEXP (block, 0),
					    POINTER_SIZE/BITS_PER_UNIT));
      emit_move_insn (dest, argsize);

      if (flag_check_memory_usage)
	emit_library_call (chkr_set_right_libfunc, 1, VOIDmode, 3,
			   dest, ptr_mode,
			   GEN_INT (GET_MODE_SIZE
				    (TYPE_MODE (integer_type_node))),
			   TYPE_MODE (sizetype),
			   GEN_INT (MEMORY_USE_RW),
			   TYPE_MODE (integer_type_node));

      /* Return the address of the va_list constructor, but don't put it in a
	 register.  Doing so would fail when not optimizing and produce worse
	 code when optimizing.  */
      return XEXP (block, 0);
    }
}

#if OPEN_VMS
#define REG_PV 27
#define REG_RA 26
#else
#define REG_RA 26
#endif

/* Find the current function's return address.

   ??? It would be better to arrange things such that if we would ordinarily
   have been a leaf function and we didn't spill the hard reg that we
   wouldn't have to save the register in the prolog.  But it's not clear
   how to get the right information at the right time.  */

static rtx alpha_return_addr_rtx;

rtx
alpha_return_addr ()
{
  rtx ret;

  if ((ret = alpha_return_addr_rtx) == NULL)
    {
      alpha_return_addr_rtx = ret = gen_reg_rtx (Pmode);

      emit_insn_after (gen_rtx (SET, VOIDmode, ret,
			        gen_rtx (REG, Pmode, REG_RA)),
		       get_insns ());
    }

  return ret;
}

/* This page contains routines that are used to determine what the function
   prologue and epilogue code will do and write them out.  */

/* Compute the size of the save area in the stack.  */

#if OPEN_VMS

/* These variables are used for communication between the following functions.
   They indicate various things about the current function being compiled
   that are used to tell what kind of prologue, epilogue and procedure
   descriptior to generate. */

/* Nonzero if we need a stack procedure.  */
static int is_stack_procedure;

/* Register number (either FP or SP) that is used to unwind the frame.  */
static int unwind_regno;

/* Register number used to save FP.  We need not have one for RA since
   we don't modify it for register procedures.  This is only defined
   for register frame procedures.  */
static int save_fp_regno;

/* Register number used to reference objects off our PV.  */
static int base_regno;

/*  Compute register masks for saved registers.  */

static void
alpha_sa_mask (imaskP, fmaskP)
    unsigned long *imaskP;
    unsigned long *fmaskP;
{
  unsigned long imask = 0;
  unsigned long fmask = 0;
  int i;

  if (is_stack_procedure)
    imask |= (1L << HARD_FRAME_POINTER_REGNUM);

  /* One for every register we have to save.  */

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if (! fixed_regs[i] && ! call_used_regs[i] && regs_ever_live[i])
      {
	if (i < 32)
	  imask |= (1L << i);
	else
	  fmask |= (1L << (i - 32));
      }

  *imaskP = imask;
  *fmaskP = fmask;

  return;
}

int
alpha_sa_size ()
{
  int sa_size = 0;
  HOST_WIDE_INT stack_needed;
  int i;

  /* One for every register we have to save.  */

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if (! fixed_regs[i] && ! call_used_regs[i] && regs_ever_live[i])
      sa_size++;

  /* Start by assuming we can use a register procedure if we don't make any
     calls (REG_RA not used) or need to save any registers and a stack
     procedure if we do.  */
  is_stack_procedure = regs_ever_live[REG_RA] || sa_size != 0;

  /* Decide whether to refer to objects off our PV via FP or PV.
     If we need need FP for something else or if we receive a nonlocal
     goto (which expects PV to contain the value), we must use PV.
     Otherwise, start by assuming we can use FP.  */
  base_regno = (frame_pointer_needed || current_function_has_nonlocal_label
		|| is_stack_procedure
		|| current_function_outgoing_args_size
		? REG_PV : HARD_FRAME_POINTER_REGNUM);

  /* If we want to copy PV into FP, we need to find some register in which to
     save FP.  */

  save_fp_regno = -1;

  if (base_regno == HARD_FRAME_POINTER_REGNUM)
    for (i = 0; i < 32; i++)
      if (! fixed_regs[i] && call_used_regs[i] && ! regs_ever_live[i])
	save_fp_regno = i;

  if (save_fp_regno == -1)
    base_regno = REG_PV, is_stack_procedure = 1;

  /* Stack unwinding should be done via FP unless we use it for PV.  */
  unwind_regno
    = base_regno == REG_PV ? HARD_FRAME_POINTER_REGNUM : STACK_POINTER_REGNUM;

  /* If this is a stack procedure, allow space for saving FP and RA.  */
  if (is_stack_procedure)
    sa_size += 2;

  return sa_size * 8;
}

int
alpha_pv_save_size ()
{
  alpha_sa_size ();
  return is_stack_procedure ? 8 : 0;
}

int
alpha_using_fp ()
{
  alpha_sa_size ();
  return unwind_regno == HARD_FRAME_POINTER_REGNUM;
}

#else /* ! OPEN_VMS */

int
alpha_sa_size ()
{
  int size = 0;
  int i;

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if (! fixed_regs[i] && ! call_used_regs[i] && regs_ever_live[i])
      size++;

  /* If some registers were saved but not reg 26, reg 26 must also
     be saved, so leave space for it.  */
  if (size != 0 && ! regs_ever_live[26])
    size++;

  /* Our size must be even (multiple of 16 bytes).  */
  if (size & 1)
    size ++;

  return size * 8;
}

#endif /* ! OPEN_VMS */

/* Return 1 if this function can directly return via $26.  */

int
direct_return ()
{
  return (! TARGET_OPEN_VMS && reload_completed && alpha_sa_size () == 0
	  && get_frame_size () == 0
	  && current_function_outgoing_args_size == 0
	  && current_function_pretend_args_size == 0);
}

/* Write a version stamp.  Don't write anything if we are running as a
   cross-compiler.  Otherwise, use the versions in /usr/include/stamp.h.  */

#if !defined(CROSS_COMPILE) && !defined(_WIN32) && !defined(__linux__) && !defined(VMS)
#include <stamp.h>
#endif

void
alpha_write_verstamp (file)
     FILE *file;
{
#ifdef MS_STAMP
  fprintf (file, "\t.verstamp %d %d\n", MS_STAMP, LS_STAMP);
#endif
}

/* Write code to add constant C to register number IN_REG (possibly 31)
   and put the result into OUT_REG.  Use TEMP_REG as a scratch register;
   usually this will be OUT_REG, but should not be if OUT_REG is 
   STACK_POINTER_REGNUM, since it must be updated in a single instruction.
   Write the code to FILE.  */

static void
add_long_const (file, c, in_reg, out_reg, temp_reg)
     FILE *file;
     HOST_WIDE_INT c;
     int in_reg, out_reg, temp_reg;
{
  HOST_WIDE_INT low = (c & 0xffff) - 2 * (c & 0x8000);
  HOST_WIDE_INT tmp1 = c - low;
  HOST_WIDE_INT high = ((tmp1 >> 16) & 0xffff) - 2 * ((tmp1 >> 16) & 0x8000);
  HOST_WIDE_INT extra = 0;

  /* We don't have code to write out constants larger than 32 bits.  */
#if HOST_BITS_PER_LONG_INT == 64
  if ((unsigned HOST_WIDE_INT) c >> 32 != 0)
    abort ();
#endif

  /* If HIGH will be interpreted as negative, we must adjust it to do two
     ldha insns.  Note that we will never be building a negative constant
     here.  */

  if (high & 0x8000)
    {
      extra = 0x4000;
      tmp1 -= 0x40000000;
      high = ((tmp1 >> 16) & 0xffff) - 2 * ((tmp1 >> 16) & 0x8000);
    }

  if (low != 0)
    {
      int result_reg = (extra == 0 && high == 0) ? out_reg : temp_reg;

      if (low >= 0 && low < 255)
	fprintf (file, "\taddq $%d,%d,$%d\n", in_reg, low, result_reg);
      else
	fprintf (file, "\tlda $%d,%d($%d)\n", result_reg, low, in_reg);

      in_reg = result_reg;
    }

  if (extra)
    {
      int result_reg = (high == 0) ? out_reg : temp_reg;

      fprintf (file, "\tldah $%d,%d($%d)\n", result_reg, extra, in_reg);
      in_reg = result_reg;
    }

  if (high)
    fprintf (file, "\tldah $%d,%d($%d)\n", out_reg, high, in_reg);
}

/* Write function prologue.  */

#if OPEN_VMS

/* On vms we have two kinds of functions:

   - stack frame (PROC_STACK)
	these are 'normal' functions with local vars and which are
	calling other functions
   - register frame (PROC_REGISTER)
	keeps all data in registers, needs no stack

   We must pass this to the assembler so it can generate the
   proper pdsc (procedure descriptor)
   This is done with the '.pdesc' command.

   size is the stack size needed for local variables.  */

void
output_prolog (file, size)
     FILE *file;
     HOST_WIDE_INT size;
{
  unsigned long imask = 0;
  unsigned long fmask = 0;
  /* Stack space needed for pushing registers clobbered by us.  */
  HOST_WIDE_INT sa_size;
  /* Complete stack size needed.  */
  HOST_WIDE_INT frame_size;
  /* Offset from base reg to register save area.  */
  int rsa_offset = 8;
  /* Offset during register save.  */
  int reg_offset;
  /* Label for the procedure entry.  */
  char *entry_label = (char *) alloca (strlen (alpha_function_name) + 5);
  int i;

  sa_size = alpha_sa_size ();
  frame_size
    = ALPHA_ROUND (sa_size 
		   + (is_stack_procedure ? 8 : 0)
		   + size + current_function_pretend_args_size);

  /* Issue function start and label.  */
  fprintf (file, "\t.ent ");
  assemble_name (file, alpha_function_name);
  fprintf (file, "\n");
  sprintf (entry_label, "%s..en", alpha_function_name);
  ASM_OUTPUT_LABEL (file, entry_label);
  inside_function = TRUE;

  fprintf (file, "\t.base $%d\n", base_regno);

  /* Calculate register masks for clobbered registers.  */

  if (is_stack_procedure)
    alpha_sa_mask (&imask, &fmask);

  /* Adjust the stack by the frame size.  If the frame size is > 4096
     bytes, we need to be sure we probe somewhere in the first and last
     4096 bytes (we can probably get away without the latter test) and
     every 8192 bytes in between.  If the frame size is > 32768, we
     do this in a loop.  Otherwise, we generate the explicit probe
     instructions. 

     Note that we are only allowed to adjust sp once in the prologue.  */

  if (frame_size < 32768)
    {
      if (frame_size > 4096)
	{
	  int probed = 4096;

	  fprintf (file, "\tstq $31,-%d($30)\n", probed);

	  while (probed + 8192 < frame_size)
	    fprintf (file, "\tstq $31,-%d($30)\n", probed += 8192);

	  /* We only have to do this probe if we aren't saving registers.  */
	  if (sa_size == 0 && probed + 4096 < frame_size)
	    fprintf (file, "\tstq $31,-%d($30)\n", frame_size);
	}

      if (frame_size != 0)
	  fprintf (file, "\tlda $30,-%d($30)\n", frame_size);
    }
  else
    {
      /* Here we generate code to set R4 to SP + 4096 and set R23 to the
	 number of 8192 byte blocks to probe.  We then probe each block
	 in the loop and then set SP to the proper location.  If the
	 amount remaining is > 4096, we have to do one more probe if we
	 are not saving any registers.  */

      HOST_WIDE_INT blocks = (frame_size + 4096) / 8192;
      HOST_WIDE_INT leftover = frame_size + 4096 - blocks * 8192;

      add_long_const (file, blocks, 31, 23, 23);

      fprintf (file, "\tlda $22,4096($30)\n");

      assemble_name (file, alpha_function_name);
      fprintf (file, "..sc:\n");

      fprintf (file, "\tstq $31,-8192($22)\n");
      fprintf (file, "\tsubq $23,1,$23\n");
      fprintf (file, "\tlda $22,-8192($22)\n");

      fprintf (file, "\tbne $23,");
      assemble_name (file, alpha_function_name);
      fprintf (file, "..sc\n");

      if (leftover > 4096 && sa_size == 0)
	fprintf (file, "\tstq $31,-%d($22)\n", leftover);

      fprintf (file, "\tlda $30,-%d($22)\n", leftover);
    }

  if (is_stack_procedure)
    {
      int reg_offset = rsa_offset;

      /* Store R26 (RA) first.  */
      fprintf (file, "\tstq $26,%d($30)\n", reg_offset);
      reg_offset += 8;

      /* Store integer regs. according to mask.  */
      for (i = 0; i < 32; i++)
        if (imask & (1L<<i))
	  {
	    fprintf (file, "\tstq $%d,%d($30)\n", i, reg_offset);
	    reg_offset += 8;
	  }

      /* Print the register mask and do floating-point saves.  */

      if (imask)
	fprintf (file, "\t.mask 0x%x,0\n", imask);

      for (i = 0; i < 32; i++)
	{
	  if (fmask & (1L << i))
	    {
	      fprintf (file, "\tstt $f%d,%d($30)\n", i, reg_offset);
	      reg_offset += 8;
	    }
	}

      /* Print the floating-point mask, if we've saved any fp register.  */
      if (fmask)
	fprintf (file, "\t.fmask 0x%x,0\n", fmask);

      fprintf (file, "\tstq $27,0($30)\n");
    }
  else 
    {
      fprintf (file, "\t.fp_save $%d\n", save_fp_regno);
      fprintf (file, "\tbis $%d,$%d,$%d\n", HARD_FRAME_POINTER_REGNUM,
	       HARD_FRAME_POINTER_REGNUM, save_fp_regno);
    }

  if (base_regno != REG_PV)
    fprintf (file, "\tbis $%d,$%d,$%d\n", REG_PV, REG_PV, base_regno);

  if (unwind_regno == HARD_FRAME_POINTER_REGNUM)
    fprintf (file, "\tbis $%d,$%d,$%d\n", STACK_POINTER_REGNUM,
	     STACK_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM);

  /* Describe our frame.  */
  fprintf (file, "\t.frame $%d,", unwind_regno);

  /* If the frame size is larger than an integer, print it as zero to
     avoid an assembler error.  We won't be properly describing such a
     frame, but that's the best we can do.  */
  fprintf (file, HOST_WIDE_INT_PRINT_DEC,
#if HOST_BITS_PER_WIDE_INT == 64
	   frame_size >= (1l << 31) ? 0:
#endif
	   frame_size
	   );
  fprintf (file, ",$26,%d\n", rsa_offset);

  /* If we have to allocate space for outgoing args, do it now.  */
  if (current_function_outgoing_args_size != 0)
    fprintf (file, "\tlda $%d,%d($%d)\n", STACK_POINTER_REGNUM,
	     - ALPHA_ROUND (current_function_outgoing_args_size),
	     HARD_FRAME_POINTER_REGNUM);

  fprintf (file, "\t.prologue\n");

  link_section ();
  fprintf (file, "\t.align 3\n");
  ASM_OUTPUT_LABEL (file, alpha_function_name);
  fprintf (file, "\t.pdesc ");
  assemble_name (file, alpha_function_name);
  fprintf (file, "..en,%s\n", is_stack_procedure ? "stack" : "reg");
  alpha_need_linkage (alpha_function_name, 1);
  text_section ();

  return;
}

/* Write function epilogue.  */

void
output_epilog (file, size)
     FILE *file;
     int size;
{
  unsigned long imask = 0;
  unsigned long fmask = 0;
  /* Stack space needed for pushing registers clobbered by us.  */
  HOST_WIDE_INT sa_size = alpha_sa_size ();
  /* Complete stack size needed.  */
  HOST_WIDE_INT frame_size
    = ALPHA_ROUND (sa_size
		   + (is_stack_procedure ? 8 : 0)
		   + size + current_function_pretend_args_size);
  int i;
  rtx insn = get_last_insn ();

  /* If the last insn was a BARRIER, we don't have to write anything except
     the .end pseudo-op.  */

  if (GET_CODE (insn) == NOTE)
    insn = prev_nonnote_insn (insn);

  if (insn == 0 || GET_CODE (insn) != BARRIER)
    {
      /* Restore clobbered registers, load FP last.  */

      if (is_stack_procedure)
	{
	  int rsa_offset = 8;
	  int reg_offset;
	  int fp_offset;

	  if (unwind_regno == HARD_FRAME_POINTER_REGNUM)
	    fprintf (file, "\tbis $%d,$%d,$%d\n", HARD_FRAME_POINTER_REGNUM,
		     HARD_FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM);

	  alpha_sa_mask (&imask, &fmask);

	  /* Start reloading registers after RA.  */
	  reg_offset = rsa_offset + 8;

	  for (i = 0; i < 32; i++)
	    if (imask & (1L<<i))
	      {
		if (i == HARD_FRAME_POINTER_REGNUM)
		  fp_offset = reg_offset;
		else
		  fprintf (file, "\tldq $%d,%d($30)\n",
				  i, reg_offset);
		reg_offset += 8;
	      }

	  for (i = 0; i < 32; i++)
	    if (fmask & (1L << i))
	      {
		fprintf (file, "\tldt $f%d,%d($30)\n", i, reg_offset);
		reg_offset += 8;
	      }

	  /* Restore R26 (RA).  */
	  fprintf (file, "\tldq $26,%d($30)\n", rsa_offset);

	  /* Restore R29 (FP).  */
	  fprintf (file, "\tldq $29,%d($30)\n", fp_offset);
	}
      else
	fprintf (file, "\tbis $%d,$%d,$%d\n", save_fp_regno, save_fp_regno,
		 HARD_FRAME_POINTER_REGNUM);

      if (frame_size != 0)
	{
	  if (frame_size < 32768)
	    fprintf (file, "\tlda $30,%d($30)\n", frame_size);
	  else
	    {
	      long high = frame_size >> 16;
	      long low = frame_size & 0xffff;
	      if (low & 0x8000)
		{
		  high++;
		  low = -32768 + (low & 0x7fff);
		}
	      fprintf (file, "\tldah $2,%ld($31)\n", high);
	      fprintf (file, "\tlda $2,%ld($2)\n", low);
	      fprintf (file, "\taddq $30,$2,$30\n");
	    }
	}

      /* Finally return to the caller.  */
      fprintf (file, "\tret $31,($26),1\n");
    }

  /* End the function.  */
  fprintf (file, "\t.end ");
  assemble_name (file,  alpha_function_name);
  fprintf (file, "\n");
  inside_function = FALSE;

  /* Show that we know this function if it is called again.  */
  SYMBOL_REF_FLAG (XEXP (DECL_RTL (current_function_decl), 0)) = 1;

  alpha_return_addr_rtx = 0;
}

int
vms_valid_decl_attribute_p (decl, attributes, identifier, args)
     tree decl;
     tree attributes;
     tree identifier;
     tree args;
{
  if (is_attribute_p ("overlaid", identifier))
    return (args == NULL_TREE);
}

#else /* !OPEN_VMS */

void
output_prolog (file, size)
     FILE *file;
     HOST_WIDE_INT size;
{
  HOST_WIDE_INT out_args_size
    = ALPHA_ROUND (current_function_outgoing_args_size);
  HOST_WIDE_INT sa_size = alpha_sa_size ();
  HOST_WIDE_INT frame_size
    = (out_args_size + sa_size
       + ALPHA_ROUND (size + current_function_pretend_args_size));
  HOST_WIDE_INT reg_offset = out_args_size;
  HOST_WIDE_INT start_reg_offset = reg_offset;
  HOST_WIDE_INT actual_start_reg_offset = start_reg_offset;
  int int_reg_save_area_size = 0;
  rtx insn;
  unsigned reg_mask = 0;
  int i;

  /* Ecoff can handle multiple .file directives, so put out file and lineno.
     We have to do that before the .ent directive as we cannot switch
     files within procedures with native ecoff because line numbers are
     linked to procedure descriptors.
     Outputting the lineno helps debugging of one line functions as they
     would otherwise get no line number at all. Please note that we would
     like to put out last_linenum from final.c, but it is not accessible.  */

  if (write_symbols == SDB_DEBUG)
    {
      ASM_OUTPUT_SOURCE_FILENAME (file,
				  DECL_SOURCE_FILE (current_function_decl));
      if (debug_info_level != DINFO_LEVEL_TERSE)
        ASM_OUTPUT_SOURCE_LINE (file,
				DECL_SOURCE_LINE (current_function_decl));
    }

  /* The assembly language programmer's guide states that the second argument
     to the .ent directive, the lex_level, is ignored by the assembler,
     so we might as well omit it.  */
     
  if (!flag_inhibit_size_directive)
    {
      fprintf (file, "\t.ent ");
      assemble_name (file, alpha_function_name);
      fprintf (file, "\n");
    }
  ASM_OUTPUT_LABEL (file, alpha_function_name);
  inside_function = TRUE;

  if (TARGET_IEEE_CONFORMANT && !flag_inhibit_size_directive)
    /* Set flags in procedure descriptor to request IEEE-conformant
       math-library routines.  The value we set it to is PDSC_EXC_IEEE
       (/usr/include/pdsc.h). */
    fprintf (file, "\t.eflag 48\n");

  /* Set up offsets to alpha virtual arg/local debugging pointer.  */

  alpha_auto_offset = -frame_size + current_function_pretend_args_size;
  alpha_arg_offset = -frame_size + 48;

  /* If we need a GP (we have a LDSYM insn or a CALL_INSN), load it first. 
     Even if we are a static function, we still need to do this in case
     our address is taken and passed to something like qsort.

     We never need a GP for Windows/NT.  */

  alpha_function_needs_gp = 0;

#ifdef TARGET_PROFILING_NEEDS_GP
  if (profile_flag)
    alpha_function_needs_gp = 1;
#endif

  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    if ((GET_CODE (insn) == CALL_INSN)
	|| (GET_RTX_CLASS (GET_CODE (insn)) == 'i'
	    && GET_CODE (PATTERN (insn)) != USE
	    && GET_CODE (PATTERN (insn)) != CLOBBER
	    && (get_attr_type (insn) == TYPE_LDSYM
		|| get_attr_type (insn) == TYPE_ISUBR)))
      {
	alpha_function_needs_gp = 1;
	break;
      }

  if (TARGET_WINDOWS_NT == 0)
    {
      if (alpha_function_needs_gp)
	fprintf (file, "\tldgp $29,0($27)\n");

      /* Put a label after the GP load so we can enter the function at it.  */
      assemble_name (file, alpha_function_name);
      fprintf (file, "..ng:\n");
    }

  /* Adjust the stack by the frame size.  If the frame size is > 4096
     bytes, we need to be sure we probe somewhere in the first and last
     4096 bytes (we can probably get away without the latter test) and
     every 8192 bytes in between.  If the frame size is > 32768, we
     do this in a loop.  Otherwise, we generate the explicit probe
     instructions. 

     Note that we are only allowed to adjust sp once in the prologue.  */

  if (frame_size < 32768)
    {
      if (frame_size > 4096)
	{
	  int probed = 4096;

	  fprintf (file, "\tstq $31,-%d($30)\n", probed);

	  while (probed + 8192 < frame_size)
	    fprintf (file, "\tstq $31,-%d($30)\n", probed += 8192);

	  /* We only have to do this probe if we aren't saving registers.  */
	  if (sa_size == 0 && probed + 4096 < frame_size)
	    fprintf (file, "\tstq $31,-%d($30)\n", frame_size);
	}

      if (frame_size != 0)
	fprintf (file, "\tlda $30,-%d($30)\n", frame_size);
    }
  else
    {
      /* Here we generate code to set R4 to SP + 4096 and set R5 to the
	 number of 8192 byte blocks to probe.  We then probe each block
	 in the loop and then set SP to the proper location.  If the
	 amount remaining is > 4096, we have to do one more probe if we
	 are not saving any registers.  */

      HOST_WIDE_INT blocks = (frame_size + 4096) / 8192;
      HOST_WIDE_INT leftover = frame_size + 4096 - blocks * 8192;

      add_long_const (file, blocks, 31, 5, 5);

      fprintf (file, "\tlda $4,4096($30)\n");

      assemble_name (file, alpha_function_name);
      fprintf (file, "..sc:\n");

      fprintf (file, "\tstq $31,-8192($4)\n");
      fprintf (file, "\tsubq $5,1,$5\n");
      fprintf (file, "\tlda $4,-8192($4)\n");

      fprintf (file, "\tbne $5,");
      assemble_name (file, alpha_function_name);
      fprintf (file, "..sc\n");

      if (leftover > 4096 && sa_size == 0)
	fprintf (file, "\tstq $31,-%d($4)\n", leftover);

      fprintf (file, "\tlda $30,-%d($4)\n", leftover);
    }

  /* Describe our frame.  */
  if (!flag_inhibit_size_directive)
    {
      fprintf (file, "\t.frame $%d,",
	       (frame_pointer_needed
	        ? HARD_FRAME_POINTER_REGNUM : STACK_POINTER_REGNUM));

      /* If the frame size is larger than an integer, print it as zero to
	 avoid an assembler error.  We won't be properly describing such a
	 frame, but that's the best we can do.  */
      fprintf (file, HOST_WIDE_INT_PRINT_DEC,
#if HOST_BITS_PER_WIDE_INT == 64
	       frame_size >= (1l << 31) ? 0 :
#endif
	       frame_size
	       );
      fprintf (file, ",$26,%d\n", current_function_pretend_args_size);
    }
    
  /* Save register 26 if any other register needs to be saved.  */
  if (sa_size != 0)
    {
      reg_mask |= 1 << 26;
      fprintf (file, "\tstq $26,%d($30)\n", reg_offset);
      reg_offset += 8;
      int_reg_save_area_size += 8;
    }

  /* Now save any other used integer registers required to be saved.  */
  for (i = 0; i < 32; i++)
    if (! fixed_regs[i] && ! call_used_regs[i] && regs_ever_live[i] && i != 26)
      {
	reg_mask |= 1 << i;
	fprintf (file, "\tstq $%d,%d($30)\n", i, reg_offset);
	reg_offset += 8;
	int_reg_save_area_size += 8;
      }

  /* Print the register mask and do floating-point saves.  */
  if (reg_mask && !flag_inhibit_size_directive)
    {
      fprintf (file, "\t.mask 0x%x,", reg_mask);
      fprintf (file, HOST_WIDE_INT_PRINT_DEC,
#if HOST_BITS_PER_WIDE_INT == 64
	       frame_size >= (1l << 31) ? 0 :
#endif
	       actual_start_reg_offset - frame_size);
      fprintf (file, "\n");
    }

  start_reg_offset = reg_offset;
  reg_mask = 0;

  for (i = 0; i < 32; i++)
    if (! fixed_regs[i + 32] && ! call_used_regs[i + 32]
	&& regs_ever_live[i + 32])
      {
	reg_mask |= 1 << i;
	fprintf (file, "\tstt $f%d,%d($30)\n", i, reg_offset);
	reg_offset += 8;
      }

  /* Print the floating-point mask, if we've saved any fp register.  */
  if (reg_mask && !flag_inhibit_size_directive)
    fprintf (file, "\t.fmask 0x%x,%d\n", reg_mask,
	     actual_start_reg_offset - frame_size + int_reg_save_area_size);

  /* If we need a frame pointer, set it from the stack pointer.  Note that
     this must always be the last instruction in the prologue.  */
  if (frame_pointer_needed)
    fprintf (file, "\tbis $30,$30,$15\n");

  /* End the prologue and say if we used gp.  */
  if (!flag_inhibit_size_directive)
    fprintf (file, "\t.prologue %d\n", alpha_function_needs_gp);
}

/* Write function epilogue.  */

void
output_epilog (file, size)
     FILE *file;
     int size;
{
  rtx insn = get_last_insn ();
  HOST_WIDE_INT out_args_size
    = ALPHA_ROUND (current_function_outgoing_args_size);
  HOST_WIDE_INT sa_size = alpha_sa_size ();
  HOST_WIDE_INT frame_size
    = (out_args_size + sa_size
       + ALPHA_ROUND (size + current_function_pretend_args_size));
  HOST_WIDE_INT reg_offset = out_args_size;
  HOST_WIDE_INT frame_size_from_reg_save = frame_size - reg_offset;
  int restore_fp
    = frame_pointer_needed && regs_ever_live[HARD_FRAME_POINTER_REGNUM];
  int i;

  /* If the last insn was a BARRIER, we don't have to write anything except
     the .end pseudo-op.  */
  if (GET_CODE (insn) == NOTE)
    insn = prev_nonnote_insn (insn);
  if (insn == 0 || GET_CODE (insn) != BARRIER)
    {
      int fp_offset = 0;

      final_prescan_insn (NULL_RTX, NULL_PTR, 0);

      /* If we have a frame pointer, restore SP from it.  */
      if (frame_pointer_needed)
	fprintf (file, "\tbis $15,$15,$30\n");

      /* Restore all the registers, starting with the return address
	 register.  */
      if (sa_size != 0)
	{
	  fprintf (file, "\tldq $26,%d($30)\n", reg_offset);
	  reg_offset += 8;
	}

      /* Now restore any other used integer registers that that we saved,
	 except for FP if it is being used as FP, since it must be
	 restored last.  */

      for (i = 0; i < 32; i++)
	if (! fixed_regs[i] && ! call_used_regs[i] && regs_ever_live[i]
	    && i != 26)
	  {
	    if (i == HARD_FRAME_POINTER_REGNUM && frame_pointer_needed)
	      fp_offset = reg_offset;
	    else
	      fprintf (file, "\tldq $%d,%d($30)\n", i, reg_offset);
	    reg_offset += 8;
	  }

      for (i = 0; i < 32; i++)
	if (! fixed_regs[i + 32] && ! call_used_regs[i + 32]
	    && regs_ever_live[i + 32])
	  {
	    fprintf (file, "\tldt $f%d,%d($30)\n", i, reg_offset);
	    reg_offset += 8;
	  }

      /* If the stack size is large and we have a frame pointer, compute the
	 size of the stack into a register because the old FP restore, stack
	 pointer adjust, and return are required to be consecutive
	 instructions.   */
      if (frame_size > 32767 && restore_fp)
	add_long_const (file, frame_size, 31, 1, 1);

      /* If we needed a frame pointer and we have to restore it, do it
	 now.  This must be done in one instruction immediately
	 before the SP update.  */
      if (restore_fp && fp_offset)
	fprintf (file, "\tldq $15,%d($30)\n", fp_offset);

      /* Now update the stack pointer, if needed.  Only one instruction must
	 modify the stack pointer.  It must be the last instruction in the
	 sequence and must be an ADDQ or LDA instruction.  If the frame
	 pointer was loaded above, we may only put one instruction here.  */

      if (frame_size > 32768 && restore_fp)
	fprintf  (file, "\taddq $1,$30,$30\n");
      else
	add_long_const (file, frame_size, 30, 30, 1);

      /* Finally return to the caller.  */
      fprintf (file, "\tret $31,($26),1\n");
    }

  /* End the function.  */
  if (!flag_inhibit_size_directive)
    {
      fprintf (file, "\t.end ");
      assemble_name (file, alpha_function_name);
      fprintf (file, "\n");
    }
  inside_function = FALSE;

  /* Show that we know this function if it is called again.  */
  SYMBOL_REF_FLAG (XEXP (DECL_RTL (current_function_decl), 0)) = 1;

  alpha_return_addr_rtx = 0;
}
#endif /* !OPEN_VMS */

/* Debugging support.  */

#include "gstab.h"

/* Count the number of sdb related labels are generated (to find block
   start and end boundaries).  */

int sdb_label_count = 0;

/* Next label # for each statement.  */

static int sym_lineno = 0;

/* Count the number of .file directives, so that .loc is up to date.  */

static int num_source_filenames = 0;

/* Name of the file containing the current function.  */

static char *current_function_file = "";

/* Offsets to alpha virtual arg/local debugging pointers.  */

long alpha_arg_offset;
long alpha_auto_offset;

/* Emit a new filename to a stream.  */

void
alpha_output_filename (stream, name)
     FILE *stream;
     char *name;
{
  static int first_time = TRUE;
  char ltext_label_name[100];

  if (first_time)
    {
      first_time = FALSE;
      ++num_source_filenames;
      current_function_file = name;
      fprintf (stream, "\t.file\t%d ", num_source_filenames);
      output_quoted_string (stream, name);
      fprintf (stream, "\n");
      if (!TARGET_GAS && write_symbols == DBX_DEBUG)
	fprintf (stream, "\t#@stabs\n");
    }

  else if (write_symbols == DBX_DEBUG)
    {
      ASM_GENERATE_INTERNAL_LABEL (ltext_label_name, "Ltext", 0);
      fprintf (stream, "%s ", ASM_STABS_OP);
      output_quoted_string (stream, name);
      fprintf (stream, ",%d,0,0,%s\n", N_SOL, &ltext_label_name[1]);
    }

  else if (name != current_function_file
	   && strcmp (name, current_function_file) != 0)
    {
      if (inside_function && ! TARGET_GAS)
	fprintf (stream, "\t#.file\t%d ", num_source_filenames);
      else
	{
	  ++num_source_filenames;
	  current_function_file = name;
	  fprintf (stream, "\t.file\t%d ", num_source_filenames);
	}

      output_quoted_string (stream, name);
      fprintf (stream, "\n");
    }
}

/* Emit a linenumber to a stream.  */

void
alpha_output_lineno (stream, line)
     FILE *stream;
     int line;
{
  if (write_symbols == DBX_DEBUG)
    {
      /* mips-tfile doesn't understand .stabd directives.  */
      ++sym_lineno;
      fprintf (stream, "$LM%d:\n\t%s %d,0,%d,$LM%d\n",
	       sym_lineno, ASM_STABN_OP, N_SLINE, line, sym_lineno);
    }
  else
    fprintf (stream, "\n\t.loc\t%d %d\n", num_source_filenames, line);
}

/* Structure to show the current status of registers and memory.  */

struct shadow_summary
{
  struct {
    unsigned long i     : 32;	/* Mask of int regs */
    unsigned long fp    : 32;	/* Mask of fp regs */
    unsigned long mem   :  1;	/* mem == imem | fpmem */
  } used, defd;
};

/* Summary the effects of expression X on the machine.  Update SUM, a pointer
   to the summary structure.  SET is nonzero if the insn is setting the
   object, otherwise zero.  */

static void
summarize_insn (x, sum, set)
     rtx x;
     struct shadow_summary *sum;
     int set;
{
  char *format_ptr;
  int i, j;

  if (x == 0)
    return;

  switch (GET_CODE (x))
    {
      /* ??? Note that this case would be incorrect if the Alpha had a
	 ZERO_EXTRACT in SET_DEST.  */
    case SET:
      summarize_insn (SET_SRC (x), sum, 0);
      summarize_insn (SET_DEST (x), sum, 1);
      break;

    case CLOBBER:
      summarize_insn (XEXP (x, 0), sum, 1);
      break;

    case USE:
      summarize_insn (XEXP (x, 0), sum, 0);
      break;

    case PARALLEL:
      for (i = XVECLEN (x, 0) - 1; i >= 0; i--)
	summarize_insn (XVECEXP (x, 0, i), sum, 0);
      break;

    case REG:
      {
	int regno = REGNO (x);
	unsigned long mask = 1UL << (regno % 32);

	if (regno == 31 || regno == 63)
	  break;

	if (set)
	  {
	    if (regno < 32)
	      sum->defd.i |= mask;
	    else
	      sum->defd.fp |= mask;
	  }
	else
	  {
	    if (regno < 32)
	      sum->used.i  |= mask;
	    else
	      sum->used.fp |= mask;
	  }
	}
      break;

    case MEM:
      if (set)
	sum->defd.mem = 1;
      else
	sum->used.mem = 1;

      /* Find the regs used in memory address computation: */
      summarize_insn (XEXP (x, 0), sum, 0);
      break;

    case SUBREG:
      summarize_insn (SUBREG_REG (x), sum, 0);
      break;

    case CONST_INT:   case CONST_DOUBLE:
    case SYMBOL_REF:  case LABEL_REF:     case CONST:
      break;

      /* Handle common unary and binary ops for efficiency.  */
    case COMPARE:  case PLUS:    case MINUS:   case MULT:      case DIV:
    case MOD:      case UDIV:    case UMOD:    case AND:       case IOR:
    case XOR:      case ASHIFT:  case ROTATE:  case ASHIFTRT:  case LSHIFTRT:
    case ROTATERT: case SMIN:    case SMAX:    case UMIN:      case UMAX:
    case NE:       case EQ:      case GE:      case GT:        case LE:
    case LT:       case GEU:     case GTU:     case LEU:       case LTU:
      summarize_insn (XEXP (x, 0), sum, 0);
      summarize_insn (XEXP (x, 1), sum, 0);
      break;

    case NEG:  case NOT:  case SIGN_EXTEND:  case ZERO_EXTEND:
    case TRUNCATE:  case FLOAT_EXTEND:  case FLOAT_TRUNCATE:  case FLOAT:
    case FIX:  case UNSIGNED_FLOAT:  case UNSIGNED_FIX:  case ABS:
    case SQRT:  case FFS: 
      summarize_insn (XEXP (x, 0), sum, 0);
      break;

    default:
      format_ptr = GET_RTX_FORMAT (GET_CODE (x));
      for (i = GET_RTX_LENGTH (GET_CODE (x)) - 1; i >= 0; i--)
	switch (format_ptr[i])
	  {
	  case 'e':
	    summarize_insn (XEXP (x, i), sum, 0);
	    break;

	  case 'E':
	    for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	      summarize_insn (XVECEXP (x, i, j), sum, 0);
	    break;

	  default:
	    abort ();
	  }
    }
}

/* This function is executed just prior to the output of assembler code for
   INSN to modify the extracted operands so they will be output differently.

   OPVEC is the vector containing the operands extracted from INSN, and
   NOPERANDS is the number of elements of the vector which contain meaningful
   data for this insn.  The contents of this vector are what will be used to
   convert the insn template into assembler code, so you can change the
   assembler output by changing the contents of the vector.

   We use this function to ensure a sufficient number of `trapb' instructions
   are in the code when the user requests code with a trap precision of
   functions or instructions.

   In naive mode, when the user requests a trap-precision of "instruction", a
   trapb is needed after every instruction that may generate a trap (and after
   jsr/bsr instructions, because called functions may import a trap from the
   caller).  This ensures that the code is resumption safe but it is also slow.

   When optimizations are turned on, we delay issuing a trapb as long as
   possible.  In this context, a trap shadow is the sequence of instructions
   that starts with a (potentially) trap generating instruction and extends to
   the next trapb or call_pal instruction (but GCC never generates call_pal by
   itself).  We can delay (and therefore sometimes omit) a trapb subject to the
   following conditions:

   (a) On entry to the trap shadow, if any Alpha register or memory location
   contains a value that is used as an operand value by some instruction in
   the trap shadow (live on entry), then no instruction in the trap shadow
   may modify the register or memory location.

   (b) Within the trap shadow, the computation of the base register for a
   memory load or store instruction may not involve using the result
   of an instruction that might generate an UNPREDICTABLE result.

   (c) Within the trap shadow, no register may be used more than once as a
   destination register.  (This is to make life easier for the trap-handler.)

   (d) The trap shadow may not include any branch instructions.

     */

void
final_prescan_insn (insn, opvec, noperands)
     rtx insn;
     rtx *opvec;
     int noperands;
{
  static struct shadow_summary shadow = {0, 0, 0, 0, 0};

#define CLOSE_SHADOW				\
  do						\
    {						\
      fputs ("\ttrapb\n", asm_out_file);	\
      trap_pending = 0;				\
      bzero ((char *) &shadow,  sizeof shadow);	\
    }						\
  while (0)

  if (alpha_tp == ALPHA_TP_PROG)
    return;

  if (trap_pending)
    switch (alpha_tp)
      {
      case ALPHA_TP_FUNC:
	/* Generate one trapb before epilogue (indicated by INSN==0) */
	if (insn == 0)
	  CLOSE_SHADOW;
	break;

      case ALPHA_TP_INSN:
	if (optimize && insn != 0)
	  {
	    struct shadow_summary sum = {0, 0, 0};

	    switch (GET_CODE(insn))
	      {
	      case INSN:
		summarize_insn (PATTERN (insn), &sum, 0);

		if ((sum.defd.i & shadow.defd.i)
		    || (sum.defd.fp & shadow.defd.fp))
		  {
		    /* (c) would be violated */
		    CLOSE_SHADOW;
		    break;
		  }

		/* Combine shadow with summary of current insn: */
		shadow.used.i     |= sum.used.i;
		shadow.used.fp    |= sum.used.fp;
		shadow.used.mem   |= sum.used.mem;
		shadow.defd.i     |= sum.defd.i;
		shadow.defd.fp    |= sum.defd.fp;
		shadow.defd.mem   |= sum.defd.mem;

		if ((sum.defd.i & shadow.used.i)
		    || (sum.defd.fp & shadow.used.fp)
		    || (sum.defd.mem & shadow.used.mem))
		  {
		    /* (a) would be violated (also takes care of (b)).  */
		    if (get_attr_trap (insn) == TRAP_YES
			&& ((sum.defd.i & sum.used.i)
			    || (sum.defd.fp & sum.used.fp)))
		      abort ();

		    CLOSE_SHADOW;
		    break;
		  }
		break;

	      case JUMP_INSN:
	      case CALL_INSN:
	      case CODE_LABEL:
		CLOSE_SHADOW;
		break;

	      default:
		abort ();
	      }
	  }
	else
	  CLOSE_SHADOW;
	break;
      }

  if (insn != 0 && get_attr_trap (insn) == TRAP_YES)
    {
      if (optimize && !trap_pending && GET_CODE (insn) == INSN)
	summarize_insn (PATTERN (insn), &shadow, 0);
      trap_pending = 1;
    }
}

/* Check a floating-point value for validity for a particular machine mode.  */

static char *float_strings[] =
{
  /* These are for FLOAT_VAX.  */
   "1.70141173319264430e+38", /* 2^127 (2^24 - 1) / 2^24 */
  "-1.70141173319264430e+38",
   "2.93873587705571877e-39", /* 2^-128 */
  "-2.93873587705571877e-39",
  /* These are for the default broken IEEE mode, which traps
     on infinity or denormal numbers.  */
   "3.402823466385288598117e+38", /* 2^128 (1 - 2^-24) */
  "-3.402823466385288598117e+38",
   "1.1754943508222875079687e-38", /* 2^-126 */
  "-1.1754943508222875079687e-38",
};

static REAL_VALUE_TYPE float_values[8];
static int inited_float_values = 0;

int
check_float_value (mode, d, overflow)
     enum machine_mode mode;
     REAL_VALUE_TYPE *d;
     int overflow;
{

  if (TARGET_IEEE || TARGET_IEEE_CONFORMANT || TARGET_IEEE_WITH_INEXACT)
    return 0;

  if (inited_float_values == 0)
    {
      int i;
      for (i = 0; i < 8; i++)
	float_values[i] = REAL_VALUE_ATOF (float_strings[i], DFmode);

      inited_float_values = 1;
    }

  if (mode == SFmode)
    {
      REAL_VALUE_TYPE r;
      REAL_VALUE_TYPE *fvptr;

      if (TARGET_FLOAT_VAX)
	fvptr = &float_values[0];
      else
	fvptr = &float_values[4];

      bcopy ((char *) d, (char *) &r, sizeof (REAL_VALUE_TYPE));
      if (REAL_VALUES_LESS (fvptr[0], r))
	{
	  bcopy ((char *) &fvptr[0], (char *) d,
		 sizeof (REAL_VALUE_TYPE));
	  return 1;
	}
      else if (REAL_VALUES_LESS (r, fvptr[1]))
	{
	  bcopy ((char *) &fvptr[1], (char *) d,
		 sizeof (REAL_VALUE_TYPE));
	  return 1;
	}
      else if (REAL_VALUES_LESS (dconst0, r)
		&& REAL_VALUES_LESS (r, fvptr[2]))
	{
	  bcopy ((char *) &dconst0, (char *) d, sizeof (REAL_VALUE_TYPE));
	  return 1;
	}
      else if (REAL_VALUES_LESS (r, dconst0)
		&& REAL_VALUES_LESS (fvptr[3], r))
	{
	  bcopy ((char *) &dconst0, (char *) d, sizeof (REAL_VALUE_TYPE));
	  return 1;
	}
    }

  return 0;
}

#if OPEN_VMS

/* Return the VMS argument type corresponding to MODE.  */

enum avms_arg_type
alpha_arg_type (mode)
     enum machine_mode mode;
{
  switch (mode)
    {
    case SFmode:
      return TARGET_FLOAT_VAX ? FF : FS;
    case DFmode:
      return TARGET_FLOAT_VAX ? FD : FT;
    default:
      return I64;
    }
}

/* Return an rtx for an integer representing the VMS Argument Information
   register value.  */

struct rtx_def *
alpha_arg_info_reg_val (cum)
     CUMULATIVE_ARGS cum;
{
  unsigned HOST_WIDE_INT regval = cum.num_args;
  int i;

  for (i = 0; i < 6; i++)
    regval |= ((int) cum.atypes[i]) << (i * 3 + 8);

  return GEN_INT (regval);
}

/* Structure to collect function names for final output
   in link section.  */

enum links_kind {KIND_UNUSED, KIND_LOCAL, KIND_EXTERN};


struct alpha_links {
  struct alpha_links *next;
  char *name;
  enum links_kind kind;
};

static struct alpha_links *alpha_links_base = 0;

/* Make (or fake) .linkage entry for function call.

   IS_LOCAL is 0 if name is used in call, 1 if name is used in definition.  */

void
alpha_need_linkage (name, is_local)
    char *name;
    int is_local;
{
  rtx x;
  struct alpha_links *lptr, *nptr;

  if (name[0] == '*')
    name++;

  /* Is this name already defined ?  */

  for (lptr = alpha_links_base; lptr; lptr = lptr->next)
    if (strcmp (lptr->name, name) == 0)
      {
	if (is_local)
	  {
	    /* Defined here but external assumed.  */
	    if (lptr->kind == KIND_EXTERN)
	      lptr->kind = KIND_LOCAL;
	  }
	else
	  {
	    /* Used here but unused assumed.  */
	    if (lptr->kind == KIND_UNUSED)
	      lptr->kind = KIND_LOCAL;
	  }
	return;
      }

  nptr = (struct alpha_links *) xmalloc (sizeof (struct alpha_links));
  nptr->next = alpha_links_base;
  nptr->name = xstrdup (name);

  /* Assume external if no definition.  */
  nptr->kind = (is_local ? KIND_UNUSED : KIND_EXTERN);

  /* Ensure we have an IDENTIFIER so assemble_name can mark is used.  */
  get_identifier (name);

  alpha_links_base = nptr;

  return;
}


void
alpha_write_linkage (stream)
    FILE *stream;
{
  struct alpha_links *lptr, *nptr;

  readonly_section ();

  fprintf (stream, "\t.align 3\n");

  for (lptr = alpha_links_base; lptr; lptr = nptr)
    {
      nptr = lptr->next;

      if (lptr->kind == KIND_UNUSED
	  || ! TREE_SYMBOL_REFERENCED (get_identifier (lptr->name)))
	continue;

      fprintf (stream, "%s..lk:\n", lptr->name);
      if (lptr->kind == KIND_LOCAL)   
	{
	  /*  Local and used, build linkage pair.  */
	  fprintf (stream, "\t.quad %s..en\n", lptr->name);
	  fprintf (stream, "\t.quad %s\n", lptr->name);
	}
      else
	/* External and used, request linkage pair.  */
	fprintf (stream, "\t.linkage %s\n", lptr->name);
    }
}

#else

void
alpha_need_linkage (name, is_local)
     char *name;
     int is_local;
{
}

#endif /* OPEN_VMS */

