/* Subroutines used for code generation on the DEC Alpha.
   Copyright (C) 1992, 93-99, 2000 Free Software Foundation, Inc. 
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
#include "reload.h"
#include "tree.h"
#include "expr.h"
#include "obstack.h"
#include "except.h"
#include "function.h"
#include "toplev.h"
#include "ggc.h"
#include "tm_p.h"

/* External data.  */
extern char *version_string;
extern int rtx_equal_function_value_matters;

/* Specify which cpu to schedule for. */

enum processor_type alpha_cpu;
static const char * const alpha_cpu_name[] = 
{
  "ev4", "ev5", "ev6"
};

/* Specify how accurate floating-point traps need to be.  */

enum alpha_trap_precision alpha_tp;

/* Specify the floating-point rounding mode.  */

enum alpha_fp_rounding_mode alpha_fprm;

/* Specify which things cause traps.  */

enum alpha_fp_trap_mode alpha_fptm;

/* Strings decoded into the above options.  */

const char *alpha_cpu_string;	/* -mcpu= */
const char *alpha_tp_string;	/* -mtrap-precision=[p|s|i] */
const char *alpha_fprm_string;	/* -mfp-rounding-mode=[n|m|c|d] */
const char *alpha_fptm_string;	/* -mfp-trap-mode=[n|u|su|sui] */
const char *alpha_mlat_string;	/* -mmemory-latency= */

/* Save information from a "cmpxx" operation until the branch or scc is
   emitted.  */

struct alpha_compare alpha_compare;

/* Non-zero if inside of a function, because the Alpha asm can't
   handle .files inside of functions.  */

static int inside_function = FALSE;

/* The number of cycles of latency we should assume on memory reads.  */

int alpha_memory_latency = 3;

/* Whether the function needs the GP.  */

static int alpha_function_needs_gp;

/* The alias set for prologue/epilogue register save/restore.  */

static int alpha_sr_alias_set;

/* Declarations of static functions.  */
static void alpha_set_memflags_1
  PARAMS ((rtx, int, int, int));
static rtx alpha_emit_set_const_1
  PARAMS ((rtx, enum machine_mode, HOST_WIDE_INT, int));
static void alpha_expand_unaligned_load_words
  PARAMS ((rtx *out_regs, rtx smem, HOST_WIDE_INT words, HOST_WIDE_INT ofs));
static void alpha_expand_unaligned_store_words
  PARAMS ((rtx *out_regs, rtx smem, HOST_WIDE_INT words, HOST_WIDE_INT ofs));
static void alpha_sa_mask
  PARAMS ((unsigned long *imaskP, unsigned long *fmaskP));
static int alpha_does_function_need_gp
  PARAMS ((void));
static void alpha_init_machine_status
  PARAMS ((struct function *p));
static void alpha_mark_machine_status
  PARAMS ((struct function *p));
static int alpha_ra_ever_killed
  PARAMS ((void));
static rtx set_frame_related_p
  PARAMS ((void));
static const char *alpha_lookup_xfloating_lib_func
  PARAMS ((enum rtx_code));
static int alpha_compute_xfloating_mode_arg
  PARAMS ((enum rtx_code, enum alpha_fp_rounding_mode));
static void alpha_emit_xfloating_libcall
  PARAMS ((const char *, rtx, rtx[], int, rtx));
static rtx alpha_emit_xfloating_compare
  PARAMS ((enum rtx_code, rtx, rtx));

/* Get the number of args of a function in one of two ways.  */
#ifdef OPEN_VMS
#define NUM_ARGS current_function_args_info.num_args
#else
#define NUM_ARGS current_function_args_info
#endif

#define REG_PV 27
#define REG_RA 26

/* Parse target option strings. */

void
override_options ()
{
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

  alpha_cpu
    = TARGET_CPU_DEFAULT & MASK_CPU_EV6 ? PROCESSOR_EV6
      : (TARGET_CPU_DEFAULT & MASK_CPU_EV5 ? PROCESSOR_EV5 : PROCESSOR_EV4);

  if (alpha_cpu_string)
    {
      if (! strcmp (alpha_cpu_string, "ev4")
	  || ! strcmp (alpha_cpu_string, "ev45")
	  || ! strcmp (alpha_cpu_string, "21064"))
	{
	  alpha_cpu = PROCESSOR_EV4;
	  target_flags &= ~ (MASK_BWX | MASK_MAX | MASK_FIX | MASK_CIX);
	}
      else if (! strcmp (alpha_cpu_string, "ev5")
	       || ! strcmp (alpha_cpu_string, "21164"))
	{
	  alpha_cpu = PROCESSOR_EV5;
	  target_flags &= ~ (MASK_BWX | MASK_MAX | MASK_FIX | MASK_CIX);
	}
      else if (! strcmp (alpha_cpu_string, "ev56")
	       || ! strcmp (alpha_cpu_string, "21164a"))
	{
	  alpha_cpu = PROCESSOR_EV5;
	  target_flags |= MASK_BWX;
	  target_flags &= ~ (MASK_MAX | MASK_FIX | MASK_CIX);
	}
      else if (! strcmp (alpha_cpu_string, "pca56")
	       || ! strcmp (alpha_cpu_string, "21164PC")
	       || ! strcmp (alpha_cpu_string, "21164pc"))
	{
	  alpha_cpu = PROCESSOR_EV5;
	  target_flags |= MASK_BWX | MASK_MAX;
	  target_flags &= ~ (MASK_FIX | MASK_CIX);
	}
      else if (! strcmp (alpha_cpu_string, "ev6")
	       || ! strcmp (alpha_cpu_string, "21264"))
	{
	  alpha_cpu = PROCESSOR_EV6;
	  target_flags |= MASK_BWX | MASK_MAX | MASK_FIX;
	  target_flags &= ~ (MASK_CIX);
	}
      else if (! strcmp (alpha_cpu_string, "ev67")
	       || ! strcmp (alpha_cpu_string, "21264a"))
	{
	  alpha_cpu = PROCESSOR_EV6;
	  target_flags |= MASK_BWX | MASK_MAX | MASK_FIX | MASK_CIX;
	}
      else
	error ("bad value `%s' for -mcpu switch", alpha_cpu_string);
    }

  /* Do some sanity checks on the above options. */

  if ((alpha_fptm == ALPHA_FPTM_SU || alpha_fptm == ALPHA_FPTM_SUI)
      && alpha_tp != ALPHA_TP_INSN && alpha_cpu != PROCESSOR_EV6)
    {
      warning ("fp software completion requires -mtrap-precision=i");
      alpha_tp = ALPHA_TP_INSN;
    }

  if (alpha_cpu == PROCESSOR_EV6)
    {
      /* Except for EV6 pass 1 (not released), we always have precise
	 arithmetic traps.  Which means we can do software completion
	 without minding trap shadows.  */
      alpha_tp = ALPHA_TP_PROG;
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

  {
    char *end;
    int lat;

    if (!alpha_mlat_string)
      alpha_mlat_string = "L1";

    if (ISDIGIT ((unsigned char)alpha_mlat_string[0])
	&& (lat = strtol (alpha_mlat_string, &end, 10), *end == '\0'))
      ;
    else if ((alpha_mlat_string[0] == 'L' || alpha_mlat_string[0] == 'l')
	     && ISDIGIT ((unsigned char)alpha_mlat_string[1])
	     && alpha_mlat_string[2] == '\0')
      {
	static int const cache_latency[][4] = 
	{
	  { 3, 30, -1 },	/* ev4 -- Bcache is a guess */
	  { 2, 12, 38 },	/* ev5 -- Bcache from PC164 LMbench numbers */
	  { 3, 13, -1 },	/* ev6 -- Ho hum, doesn't exist yet */
	};

	lat = alpha_mlat_string[1] - '0';
	if (lat < 0 || lat > 3 || cache_latency[alpha_cpu][lat-1] == -1)
	  {
	    warning ("L%d cache latency unknown for %s",
		     lat, alpha_cpu_name[alpha_cpu]);
	    lat = 3;
	  }
	else
	  lat = cache_latency[alpha_cpu][lat-1];
      }
    else if (! strcmp (alpha_mlat_string, "main"))
      {
	/* Most current memories have about 370ns latency.  This is
	   a reasonable guess for a fast cpu.  */
	lat = 150;
      }
    else
      {
	warning ("bad value `%s' for -mmemory-latency", alpha_mlat_string);
	lat = 3;
      }

    alpha_memory_latency = lat;
  }

  /* Default the definition of "small data" to 8 bytes.  */
  if (!g_switch_set)
    g_switch_value = 8;

  /* Acquire a unique set number for our register saves and restores.  */
  alpha_sr_alias_set = new_alias_set ();

  /* Set up function hooks.  */
  init_machine_status = alpha_init_machine_status;
  mark_machine_status = alpha_mark_machine_status;
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
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return ((GET_CODE (op) == CONST_INT
	   && (unsigned HOST_WIDE_INT) INTVAL (op) < 0x100));
}

/* Return 1 if the operand is a valid second operand to an add insn.  */

int
add_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == CONST_INT)
    /* Constraints I, J, O and P are covered by K.  */
    return (CONST_OK_FOR_LETTER_P (INTVAL (op), 'K')
	    || CONST_OK_FOR_LETTER_P (INTVAL (op), 'L'));

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
    return (CONST_OK_FOR_LETTER_P (INTVAL (op), 'I')
	    || CONST_OK_FOR_LETTER_P (INTVAL (op), 'O'));

  return reg_not_elim_operand (op, mode);
}

/* Return 1 if OP is the constant 4 or 8.  */

int
const48_operand (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
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
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == CONST_INT
	  && (INTVAL (op) == 8 || INTVAL (op) == 16
	      || INTVAL (op) == 32 || INTVAL (op) == 64));
}

/* Return 1 if OP is a constant that is the width of an integral machine mode
   smaller than an integer.  */

int
mode_mask_operand (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
#if HOST_BITS_PER_WIDE_INT == 32
  if (GET_CODE (op) == CONST_DOUBLE)
    return (CONST_DOUBLE_LOW (op) == -1
	    && (CONST_DOUBLE_HIGH (op) == -1
		|| CONST_DOUBLE_HIGH (op) == 0));
#else
  if (GET_CODE (op) == CONST_DOUBLE)
    return (CONST_DOUBLE_LOW (op) == -1 && CONST_DOUBLE_HIGH (op) == 0);
#endif

  return (GET_CODE (op) == CONST_INT
	  && (INTVAL (op) == 0xff
	      || INTVAL (op) == 0xffff
	      || INTVAL (op) == (HOST_WIDE_INT)0xffffffff
#if HOST_BITS_PER_WIDE_INT == 64
	      || INTVAL (op) == -1
#endif
	      ));
}

/* Return 1 if OP is a multiple of 8 less than 64.  */

int
mul8_operand (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
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

/* Return 1 if OP is a hard floating-point register.  */

int
hard_fp_register_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return ((GET_CODE (op) == REG && REGNO_REG_CLASS (REGNO (op)) == FLOAT_REGS)
	  || (GET_CODE (op) == SUBREG
	      && hard_fp_register_operand (SUBREG_REG (op), mode)));
}

/* Return 1 if OP is a register or a constant integer.  */


int
reg_or_cint_operand (op, mode)
    register rtx op;
    enum machine_mode mode;
{
     return (GET_CODE (op) == CONST_INT
	     || register_operand (op, mode));
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
    case REG:  case MEM:  case CONST_DOUBLE:  case CONST_INT:  case LABEL_REF:
    case SYMBOL_REF:  case CONST:
      return 1;

    case SUBREG:
      return some_operand (SUBREG_REG (op), VOIDmode);

    default:
      break;
    }

  return 0;
}

/* Likewise, but don't accept constants.  */

int
some_ni_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (GET_MODE (op) != mode && mode != VOIDmode)
    return 0;

  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  return (GET_CODE (op) == REG || GET_CODE (op) == MEM);
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
    case ADDRESSOF:
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

    case CONSTANT_P_RTX:
      return 1;

    default:
      break;
    }

  return 0;
}

/* Return 1 if OP is a SYMBOL_REF for a function known to be in this
   file.  */

int
current_file_function_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
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
	  || (GET_CODE (op) == REG
	      && (TARGET_OPEN_VMS || TARGET_WINDOWS_NT || REGNO (op) == 27)));
}

/* Return 1 if OP is a valid Alpha comparison operator.  Here we know which
   comparisons are valid in which insn.  */

int
alpha_comparison_operator (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  enum rtx_code code = GET_CODE (op);

  if (mode != GET_MODE (op) && mode != VOIDmode)
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

  if ((mode != GET_MODE (op) && mode != VOIDmode)
      || GET_RTX_CLASS (code) != '<')
    return 0;

  code = swap_condition (code);
  return (code == EQ || code == LE || code == LT
	  || (mode == DImode && (code == LEU || code == LTU)));
}

/* Return 1 if OP is a signed comparison operation.  */

int
signed_comparison_operator (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  enum rtx_code code = GET_CODE (op);

  if (mode != GET_MODE (op) && mode != VOIDmode)
    return 0;

  return (code == EQ || code == NE
	  || code == LE || code == LT
	  || code == GE || code == GT);
}

/* Return 1 if OP is a valid Alpha floating point comparison operator.
   Here we know which comparisons are valid in which insn.  */

int
alpha_fp_comparison_operator (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  enum rtx_code code = GET_CODE (op);

  if (mode != GET_MODE (op) && mode != VOIDmode)
    return 0;

  return (code == EQ || code == LE || code == LT || code == UNORDERED);
}

/* Return 1 if this is a divide or modulus operator.  */

int
divmod_operator (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  switch (GET_CODE (op))
    {
    case DIV:  case MOD:  case UDIV:  case UMOD:
      return 1;

    default:
      break;
    }

  return 0;
}

/* Return 1 if this memory address is a known aligned register plus
   a constant.  It must be a valid address.  This means that we can do
   this as an aligned reference plus some offset.

   Take into account what reload will do.  */

int
aligned_memory_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  rtx base;

  if (reload_in_progress)
    {
      rtx tmp = op;
      if (GET_CODE (tmp) == SUBREG)
	tmp = SUBREG_REG (tmp);
      if (GET_CODE (tmp) == REG
	  && REGNO (tmp) >= FIRST_PSEUDO_REGISTER)
	{
	  op = reg_equiv_memory_loc[REGNO (tmp)];
	  if (op == 0)
	    return 0;
	}
    }

  if (GET_CODE (op) != MEM
      || GET_MODE (op) != mode)
    return 0;
  op = XEXP (op, 0);

  /* LEGITIMIZE_RELOAD_ADDRESS creates (plus (plus reg const_hi) const_lo)
     sorts of constructs.  Dig for the real base register.  */
  if (reload_in_progress
      && GET_CODE (op) == PLUS
      && GET_CODE (XEXP (op, 0)) == PLUS)
    base = XEXP (XEXP (op, 0), 0);
  else
    {
      if (! memory_address_p (mode, op))
	return 0;
      base = (GET_CODE (op) == PLUS ? XEXP (op, 0) : op);
    }

  return (GET_CODE (base) == REG
	  && REGNO_POINTER_ALIGN (REGNO (base)) >= 4);
}

/* Similar, but return 1 if OP is a MEM which is not alignable.  */

int
unaligned_memory_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  rtx base;

  if (reload_in_progress)
    {
      rtx tmp = op;
      if (GET_CODE (tmp) == SUBREG)
	tmp = SUBREG_REG (tmp);
      if (GET_CODE (tmp) == REG
	  && REGNO (tmp) >= FIRST_PSEUDO_REGISTER)
	{
	  op = reg_equiv_memory_loc[REGNO (tmp)];
	  if (op == 0)
	    return 0;
	}
    }

  if (GET_CODE (op) != MEM
      || GET_MODE (op) != mode)
    return 0;
  op = XEXP (op, 0);

  /* LEGITIMIZE_RELOAD_ADDRESS creates (plus (plus reg const_hi) const_lo)
     sorts of constructs.  Dig for the real base register.  */
  if (reload_in_progress
      && GET_CODE (op) == PLUS
      && GET_CODE (XEXP (op, 0)) == PLUS)
    base = XEXP (XEXP (op, 0), 0);
  else
    {
      if (! memory_address_p (mode, op))
	return 0;
      base = (GET_CODE (op) == PLUS ? XEXP (op, 0) : op);
    }

  return (GET_CODE (base) == REG
	  && REGNO_POINTER_ALIGN (REGNO (base)) < 4);
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
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == MEM
	  || (GET_CODE (op) == SUBREG && GET_CODE (SUBREG_REG (op)) == REG)
	  || (reload_in_progress && GET_CODE (op) == REG
	      && REGNO (op) >= FIRST_PSEUDO_REGISTER)
	  || (reload_in_progress && GET_CODE (op) == SUBREG
	      && GET_CODE (SUBREG_REG (op)) == REG
	      && REGNO (SUBREG_REG (op)) >= FIRST_PSEUDO_REGISTER));
}

/* Returns 1 if OP is not an eliminable register.

   This exists to cure a pathological abort in the s8addq (et al) patterns,

	long foo () { long t; bar(); return (long) &t * 26107; }

   which run afoul of a hack in reload to cure a (presumably) similar
   problem with lea-type instructions on other targets.  But there is
   one of us and many of them, so work around the problem by selectively
   preventing combine from making the optimization.  */

int
reg_not_elim_operand (op, mode)
      register rtx op;
      enum machine_mode mode;
{
  rtx inner = op;
  if (GET_CODE (op) == SUBREG)
    inner = SUBREG_REG (op);
  if (inner == frame_pointer_rtx || inner == arg_pointer_rtx)
    return 0;

  return register_operand (op, mode);
}

/* Return 1 is OP is a memory location that is not a reference (using
   an AND) to an unaligned location.  Take into account what reload
   will do.  */

int
normal_memory_operand (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (reload_in_progress)
    {
      rtx tmp = op;
      if (GET_CODE (tmp) == SUBREG)
	tmp = SUBREG_REG (tmp);
      if (GET_CODE (tmp) == REG
	  && REGNO (tmp) >= FIRST_PSEUDO_REGISTER)
	{
	  op = reg_equiv_memory_loc[REGNO (tmp)];

	  /* This may not have been assigned an equivalent address if it will
	     be eliminated.  In that case, it doesn't matter what we do.  */
	  if (op == 0)
	    return 1;
	}
    }

  return GET_CODE (op) == MEM && GET_CODE (XEXP (op, 0)) != AND;
}

/* Accept a register, but not a subreg of any kind.  This allows us to
   avoid pathological cases in reload wrt data movement common in 
   int->fp conversion.  */

int
reg_no_subreg_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == SUBREG)
    return 0;
  return register_operand (op, mode);
}

/* Recognize a addition operation that includes a constant.  Used to
   convince reload to canonize (plus (plus reg c1) c2) during register
   elimination.  */

int
addition_operation (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (GET_MODE (op) != mode && mode != VOIDmode)
    return 0;
  if (GET_CODE (op) == PLUS
      && register_operand (XEXP (op, 0), mode)
      && GET_CODE (XEXP (op, 1)) == CONST_INT
      && CONST_OK_FOR_LETTER_P (INTVAL (XEXP (op, 1)), 'K'))
    return 1;
  return 0;
}

/* Return 1 if this function can directly return via $26.  */

int
direct_return ()
{
  return (! TARGET_OPEN_VMS && reload_completed && alpha_sa_size () == 0
	  && get_frame_size () == 0
	  && current_function_outgoing_args_size == 0
	  && current_function_pretend_args_size == 0);
}

/* REF is an alignable memory location.  Place an aligned SImode
   reference into *PALIGNED_MEM and the number of bits to shift into
   *PBITNUM.  SCRATCH is a free register for use in reloading out
   of range stack slots.  */

void
get_aligned_mem (ref, paligned_mem, pbitnum)
     rtx ref;
     rtx *paligned_mem, *pbitnum;
{
  rtx base;
  HOST_WIDE_INT offset = 0;

  if (GET_CODE (ref) != MEM)
    abort ();

  if (reload_in_progress
      && ! memory_address_p (GET_MODE (ref), XEXP (ref, 0)))
    {
      base = find_replacement (&XEXP (ref, 0));

      if (! memory_address_p (GET_MODE (ref), base))
	abort ();
    }
  else
    {
      base = XEXP (ref, 0);
    }

  if (GET_CODE (base) == PLUS)
    offset += INTVAL (XEXP (base, 1)), base = XEXP (base, 0);

  *paligned_mem = gen_rtx_MEM (SImode, plus_constant (base, offset & ~3));
  MEM_COPY_ATTRIBUTES (*paligned_mem, ref);
  RTX_UNCHANGING_P (*paligned_mem) = RTX_UNCHANGING_P (ref);

  /* Sadly, we cannot use alias sets here because we may overlap other
     data in a different alias set.  */
  /* MEM_ALIAS_SET (*paligned_mem) = MEM_ALIAS_SET (ref); */

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

  if (GET_CODE (ref) != MEM)
    abort ();

  if (reload_in_progress
      && ! memory_address_p (GET_MODE (ref), XEXP (ref, 0)))
    {
      base = find_replacement (&XEXP (ref, 0));

      if (! memory_address_p (GET_MODE (ref), base))
	abort ();
    }
  else
    {
      base = XEXP (ref, 0);
    }

  if (GET_CODE (base) == PLUS)
    offset += INTVAL (XEXP (base, 1)), base = XEXP (base, 0);

  return plus_constant (base, offset + extra_offset);
}

/* Loading and storing HImode or QImode values to and from memory
   usually requires a scratch register.  The exceptions are loading
   QImode and HImode from an aligned address to a general register
   unless byte instructions are permitted. 

   We also cannot load an unaligned address or a paradoxical SUBREG
   into an FP register. 

   We also cannot do integral arithmetic into FP regs, as might result
   from register elimination into a DImode fp register.  */

enum reg_class
secondary_reload_class (class, mode, x, in)
     enum reg_class class;
     enum machine_mode mode;
     rtx x;
     int in;
{
  if ((mode == QImode || mode == HImode) && ! TARGET_BWX)
    {
      if (GET_CODE (x) == MEM
	  || (GET_CODE (x) == REG && REGNO (x) >= FIRST_PSEUDO_REGISTER)
	  || (GET_CODE (x) == SUBREG
	      && (GET_CODE (SUBREG_REG (x)) == MEM
		  || (GET_CODE (SUBREG_REG (x)) == REG
		      && REGNO (SUBREG_REG (x)) >= FIRST_PSEUDO_REGISTER))))
	{
	  if (!in || !aligned_memory_operand(x, mode))
	    return GENERAL_REGS;
	}
    }

  if (class == FLOAT_REGS)
    {
      if (GET_CODE (x) == MEM && GET_CODE (XEXP (x, 0)) == AND)
	return GENERAL_REGS;

      if (GET_CODE (x) == SUBREG
	  && (GET_MODE_SIZE (GET_MODE (x))
	      > GET_MODE_SIZE (GET_MODE (SUBREG_REG (x)))))
	return GENERAL_REGS;

      if (in && INTEGRAL_MODE_P (mode) && ! general_operand (x, mode))
	return GENERAL_REGS;
    }

  return NO_REGS;
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
      /* Sadly, we cannot use alias sets because the extra aliasing
	 produced by the AND interferes.  Given that two-byte quantities
	 are the only thing we would be able to differentiate anyway,
	 there does not seem to be any point in convoluting the early
	 out of the alias check.  */
      /* MEM_ALIAS_SET (x) = alias_set; */
      break;

    default:
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
  int in_struct_p, volatile_p, unchanging_p;

  if (GET_CODE (ref) != MEM)
    return;

  in_struct_p = MEM_IN_STRUCT_P (ref);
  volatile_p = MEM_VOLATILE_P (ref);
  unchanging_p = RTX_UNCHANGING_P (ref);

  /* This is only called from alpha.md, after having had something 
     generated from one of the insn patterns.  So if everything is
     zero, the pattern is already up-to-date.  */
  if (! in_struct_p && ! volatile_p && ! unchanging_p)
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
     a sign-extended 32-bit constant when compiling on a narrow machine.   */

  if (HOST_BITS_PER_WIDE_INT != 64
      || c >> 31 == -1 || c >> 31 == 0)
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
	{
	  /* We used to use copy_to_suggested_reg (GEN_INT (c), target, mode)
	     but that meant that we can't handle INT_MIN on 32-bit machines
	     (like NT/Alpha), because we recurse indefinitely through 
	     emit_move_insn to gen_movdi.  So instead, since we know exactly
	     what we want, create it explicitly.  */

	  if (target == NULL)
	    target = gen_reg_rtx (mode);
	  emit_insn (gen_rtx_SET (VOIDmode, target, GEN_INT (c)));
	  return target;
	}
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
			(unsigned HOST_WIDE_INT) (c >> bits), i))) != 0
	      || ((temp = (alpha_emit_set_const
			  (subtarget, mode,
			   ((unsigned HOST_WIDE_INT) c) >> bits, i)))
		  != 0))
	    return expand_binop (mode, ashl_optab, temp, GEN_INT (bits),
				 target, 0, OPTAB_WIDEN);

      /* Now try high-order zero bits.  Here we try the shifted-in bits as
	 all zero and all ones.  Be careful to avoid shifting outside the
	 mode and to avoid shifting outside the host wide int size.  */
      /* On narrow hosts, don't shift a 1 into the high bit, since we'll
	 confuse the recursive call and set all of the high 32 bits.  */

      if ((bits = (MIN (HOST_BITS_PER_WIDE_INT, GET_MODE_SIZE (mode) * 8)
		   - floor_log2 (c) - 1 - (HOST_BITS_PER_WIDE_INT < 64))) > 0)
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

/* Having failed to find a 3 insn sequence in alpha_emit_set_const,
   fall back to a straight forward decomposition.  We do this to avoid
   exponential run times encountered when looking for longer sequences
   with alpha_emit_set_const.  */

rtx
alpha_emit_set_long_const (target, c1, c2)
     rtx target;
     HOST_WIDE_INT c1, c2;
{
  HOST_WIDE_INT d1, d2, d3, d4;

  /* Decompose the entire word */
#if HOST_BITS_PER_WIDE_INT >= 64
  if (c2 != -(c1 < 0))
    abort ();
  d1 = ((c1 & 0xffff) ^ 0x8000) - 0x8000;
  c1 -= d1;
  d2 = ((c1 & 0xffffffff) ^ 0x80000000) - 0x80000000;
  c1 = (c1 - d2) >> 32;
  d3 = ((c1 & 0xffff) ^ 0x8000) - 0x8000;
  c1 -= d3;
  d4 = ((c1 & 0xffffffff) ^ 0x80000000) - 0x80000000;
  if (c1 != d4)
    abort ();
#else
  d1 = ((c1 & 0xffff) ^ 0x8000) - 0x8000;
  c1 -= d1;
  d2 = ((c1 & 0xffffffff) ^ 0x80000000) - 0x80000000;
  if (c1 != d2)
    abort ();
  c2 += (d2 < 0);
  d3 = ((c2 & 0xffff) ^ 0x8000) - 0x8000;
  c2 -= d3;
  d4 = ((c2 & 0xffffffff) ^ 0x80000000) - 0x80000000;
  if (c2 != d4)
    abort ();
#endif

  /* Construct the high word */
  if (d4)
    {
      emit_move_insn (target, GEN_INT (d4));
      if (d3)
	emit_move_insn (target, gen_rtx_PLUS (DImode, target, GEN_INT (d3)));
    }
  else
    emit_move_insn (target, GEN_INT (d3));

  /* Shift it into place */
  emit_move_insn (target, gen_rtx_ASHIFT (DImode, target, GEN_INT (32)));

  /* Add in the low bits.  */
  if (d2)
    emit_move_insn (target, gen_rtx_PLUS (DImode, target, GEN_INT (d2)));
  if (d1)
    emit_move_insn (target, gen_rtx_PLUS (DImode, target, GEN_INT (d1)));

  return target;
}

/* Generate an unsigned DImode to FP conversion.  This is the same code
   optabs would emit if we didn't have TFmode patterns.

   For SFmode, this is the only construction I've found that can pass
   gcc.c-torture/execute/ieee/rbug.c.  No scenario that uses DFmode
   intermediates will work, because you'll get intermediate rounding
   that ruins the end result.  Some of this could be fixed by turning
   on round-to-positive-infinity, but that requires diddling the fpsr,
   which kills performance.  I tried turning this around and converting
   to a negative number, so that I could turn on /m, but either I did
   it wrong or there's something else cause I wound up with the exact
   same single-bit error.  There is a branch-less form of this same code:

	srl     $16,1,$1
	and     $16,1,$2
	cmplt   $16,0,$3
	or      $1,$2,$2
	cmovge  $16,$16,$2
	itoft	$3,$f10
	itoft	$2,$f11
	cvtqs   $f11,$f11
	adds    $f11,$f11,$f0
	fcmoveq $f10,$f11,$f0

   I'm not using it because it's the same number of instructions as
   this branch-full form, and it has more serialized long latency
   instructions on the critical path.

   For DFmode, we can avoid rounding errors by breaking up the word
   into two pieces, converting them separately, and adding them back:

   LC0: .long 0,0x5f800000

	itoft	$16,$f11
	lda	$2,LC0
	cpyse	$f11,$f31,$f10
	cpyse	$f31,$f11,$f11
	s4addq	$1,$2,$1
	lds	$f12,0($1)
	cvtqt	$f10,$f10
	cvtqt	$f11,$f11
	addt	$f12,$f10,$f0
	addt	$f0,$f11,$f0

   This doesn't seem to be a clear-cut win over the optabs form.
   It probably all depends on the distribution of numbers being
   converted -- in the optabs form, all but high-bit-set has a
   much lower minimum execution time.  */

void
alpha_emit_floatuns (operands)
     rtx operands[2];
{
  rtx neglab, donelab, i0, i1, f0, in, out;
  enum machine_mode mode;

  out = operands[0];
  in = operands[1];
  mode = GET_MODE (out);
  neglab = gen_label_rtx ();
  donelab = gen_label_rtx ();
  i0 = gen_reg_rtx (DImode);
  i1 = gen_reg_rtx (DImode);
  f0 = gen_reg_rtx (mode);

  emit_cmp_and_jump_insns (in, const0_rtx, LT, const0_rtx, DImode, 0,
			   8, neglab);

  emit_insn (gen_rtx_SET (VOIDmode, out, gen_rtx_FLOAT (mode, in)));
  emit_jump_insn (gen_jump (donelab));

  emit_label (neglab);

  emit_insn (gen_lshrdi3 (i0, in, const1_rtx));
  emit_insn (gen_anddi3 (i1, in, const1_rtx));
  emit_insn (gen_iordi3 (i0, i0, i1));
  emit_insn (gen_rtx_SET (VOIDmode, f0, gen_rtx_FLOAT (mode, i0)));
  emit_insn (gen_rtx_SET (VOIDmode, out, gen_rtx_PLUS (mode, f0, f0)));

  emit_label (donelab);
}

/* Generate the comparison for a conditional branch.  */

rtx
alpha_emit_conditional_branch (code)
     enum rtx_code code;
{
  enum rtx_code cmp_code, branch_code;
  enum machine_mode cmp_mode, branch_mode = VOIDmode;
  rtx op0 = alpha_compare.op0, op1 = alpha_compare.op1;
  rtx tem;

  if (alpha_compare.fp_p && GET_MODE (op0) == TFmode)
    {
      if (! TARGET_HAS_XFLOATING_LIBS)
	abort ();

      /* X_floating library comparison functions return
	   -1  unordered
	    0  false
	    1  true
	 Convert the compare against the raw return value.  */

      op0 = alpha_emit_xfloating_compare (code, op0, op1);
      op1 = const0_rtx;
      alpha_compare.fp_p = 0;
      code = GT;
    }

  /* The general case: fold the comparison code to the types of compares
     that we have, choosing the branch as necessary.  */
  switch (code)
    {
    case EQ:  case LE:  case LT:  case LEU:  case LTU:
    case UNORDERED:
      /* We have these compares: */
      cmp_code = code, branch_code = NE;
      break;

    case NE:
    case ORDERED:
      /* These must be reversed. */
      cmp_code = reverse_condition (code), branch_code = EQ;
      break;

    case GE:  case GT: case GEU:  case GTU:
      /* For FP, we swap them, for INT, we reverse them.  */
      if (alpha_compare.fp_p)
	{
	  cmp_code = swap_condition (code);
	  branch_code = NE;
	  tem = op0, op0 = op1, op1 = tem;
	}
      else
	{
	  cmp_code = reverse_condition (code);
	  branch_code = EQ;
	}
      break;

    default:
      abort ();
    }

  if (alpha_compare.fp_p)
    {
      cmp_mode = DFmode;
      if (flag_fast_math)
	{
	  /* When we are not as concerned about non-finite values, and we
	     are comparing against zero, we can branch directly.  */
	  if (op1 == CONST0_RTX (DFmode))
	    cmp_code = NIL, branch_code = code;
	  else if (op0 == CONST0_RTX (DFmode))
	    {
	      /* Undo the swap we probably did just above.  */
	      tem = op0, op0 = op1, op1 = tem;
	      branch_code = swap_condition (cmp_code);
	      cmp_code = NIL;
	    }
	}
      else
	{
	  /* ??? We mark the the branch mode to be CCmode to prevent the
	     compare and branch from being combined, since the compare 
	     insn follows IEEE rules that the branch does not.  */
	  branch_mode = CCmode;
	}
    }
  else
    {
      cmp_mode = DImode;

      /* The following optimizations are only for signed compares.  */
      if (code != LEU && code != LTU && code != GEU && code != GTU)
	{
	  /* Whee.  Compare and branch against 0 directly.  */
	  if (op1 == const0_rtx)
	    cmp_code = NIL, branch_code = code;

	  /* We want to use cmpcc/bcc when we can, since there is a zero delay
	     bypass between logicals and br/cmov on EV5.  But we don't want to
	     force valid immediate constants into registers needlessly.  */
	  else if (GET_CODE (op1) == CONST_INT)
	    {
	      HOST_WIDE_INT v = INTVAL (op1), n = -v;

	      if (! CONST_OK_FOR_LETTER_P (v, 'I')
		  && (CONST_OK_FOR_LETTER_P (n, 'K')
		      || CONST_OK_FOR_LETTER_P (n, 'L')))
		{
		  cmp_code = PLUS, branch_code = code;
		  op1 = GEN_INT (n);
		}
	    }
	}
    }

  /* Force op0 into a register.  */
  if (GET_CODE (op0) != REG)
    op0 = force_reg (cmp_mode, op0);

  /* Emit an initial compare instruction, if necessary.  */
  tem = op0;
  if (cmp_code != NIL)
    {
      tem = gen_reg_rtx (cmp_mode);
      emit_move_insn (tem, gen_rtx_fmt_ee (cmp_code, cmp_mode, op0, op1));
    }

  /* Zero the operands.  */
  memset (&alpha_compare, 0, sizeof (alpha_compare));

  /* Return the branch comparison.  */
  return gen_rtx_fmt_ee (branch_code, branch_mode, tem, CONST0_RTX (cmp_mode));
}


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
  rtx op0 = alpha_compare.op0;
  rtx op1 = alpha_compare.op1;
  int fp_p = alpha_compare.fp_p;
  enum machine_mode cmp_mode
    = (GET_MODE (op0) == VOIDmode ? DImode : GET_MODE (op0));
  enum machine_mode cmp_op_mode = fp_p ? DFmode : DImode;
  enum machine_mode cmov_mode = VOIDmode;
  int local_fast_math = flag_fast_math;
  rtx tem;

  /* Zero the operands.  */
  memset (&alpha_compare, 0, sizeof (alpha_compare));

  if (fp_p != FLOAT_MODE_P (mode))
    {
      enum rtx_code cmp_code;

      if (! TARGET_FIX)
	return 0;

      /* If we have fp<->int register move instructions, do a cmov by
	 performing the comparison in fp registers, and move the
	 zero/non-zero value to integer registers, where we can then
	 use a normal cmov, or vice-versa.  */

      switch (code)
	{
	case EQ: case LE: case LT: case LEU: case LTU:
	  /* We have these compares.  */
	  cmp_code = code, code = NE;
	  break;

	case NE:
	  /* This must be reversed.  */
	  cmp_code = EQ, code = EQ;
	  break;

	case GE: case GT: case GEU: case GTU:
	  /* These must be swapped.  */
	  cmp_code = swap_condition (code);
	  code = NE;
	  tem = op0, op0 = op1, op1 = tem;
	  break;

	default:
	  abort ();
	}

      tem = gen_reg_rtx (cmp_op_mode);
      emit_insn (gen_rtx_SET (VOIDmode, tem,
			      gen_rtx_fmt_ee (cmp_code, cmp_op_mode,
					      op0, op1)));

      cmp_mode = cmp_op_mode = fp_p ? DImode : DFmode;
      op0 = gen_lowpart (cmp_op_mode, tem);
      op1 = CONST0_RTX (cmp_op_mode);
      fp_p = !fp_p;
      local_fast_math = 1;
    }

  /* We may be able to use a conditional move directly.
     This avoids emitting spurious compares. */
  if (signed_comparison_operator (cmp, VOIDmode)
      && (!fp_p || local_fast_math)
      && (op0 == CONST0_RTX (cmp_mode) || op1 == CONST0_RTX (cmp_mode)))
    return gen_rtx_fmt_ee (code, VOIDmode, op0, op1);

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

  /* ??? We mark the branch mode to be CCmode to prevent the compare
     and cmov from being combined, since the compare insn follows IEEE
     rules that the cmov does not.  */
  if (fp_p && !local_fast_math)
    cmov_mode = CCmode;

  tem = gen_reg_rtx (cmp_op_mode);
  emit_move_insn (tem, gen_rtx_fmt_ee (code, cmp_op_mode, op0, op1));
  return gen_rtx_fmt_ee (cmov_code, cmov_mode, tem, CONST0_RTX (cmp_op_mode));
}

/* Look up the function X_floating library function name for the
   given operation.  */

static const char *
alpha_lookup_xfloating_lib_func (code)
     enum rtx_code code;
{
  struct xfloating_op
    {
      enum rtx_code code;
      const char *func;
    };

  static const struct xfloating_op vms_xfloating_ops[] = 
    {
      { PLUS,		"OTS$ADD_X" },
      { MINUS,		"OTS$SUB_X" },
      { MULT,		"OTS$MUL_X" },
      { DIV,		"OTS$DIV_X" },
      { EQ,		"OTS$EQL_X" },
      { NE,		"OTS$NEQ_X" },
      { LT,		"OTS$LSS_X" },
      { LE,		"OTS$LEQ_X" },
      { GT,		"OTS$GTR_X" },
      { GE,		"OTS$GEQ_X" },
      { FIX,		"OTS$CVTXQ" },
      { FLOAT,		"OTS$CVTQX" },
      { UNSIGNED_FLOAT,	"OTS$CVTQUX" },
      { FLOAT_EXTEND,	"OTS$CVT_FLOAT_T_X" },
      { FLOAT_TRUNCATE,	"OTS$CVT_FLOAT_X_T" },
    };

  static const struct xfloating_op osf_xfloating_ops[] = 
    {
      { PLUS,		"_OtsAddX" },
      { MINUS,		"_OtsSubX" },
      { MULT,		"_OtsMulX" },
      { DIV,		"_OtsDivX" },
      { EQ,		"_OtsEqlX" },
      { NE,		"_OtsNeqX" },
      { LT,		"_OtsLssX" },
      { LE,		"_OtsLeqX" },
      { GT,		"_OtsGtrX" },
      { GE,		"_OtsGeqX" },
      { FIX,		"_OtsCvtXQ" },
      { FLOAT,		"_OtsCvtQX" },
      { UNSIGNED_FLOAT,	"_OtsCvtQUX" },
      { FLOAT_EXTEND,	"_OtsConvertFloatTX" },
      { FLOAT_TRUNCATE,	"_OtsConvertFloatXT" },
    };

  const struct xfloating_op *ops;
  const long n = sizeof(osf_xfloating_ops) / sizeof(osf_xfloating_ops[0]);
  long i;

  /* How irritating.  Nothing to key off for the table.  Hardcode
     knowledge of the G_floating routines.  */
  if (TARGET_FLOAT_VAX)
    {
      if (TARGET_OPEN_VMS)
	{
	  if (code == FLOAT_EXTEND)
	    return "OTS$CVT_FLOAT_G_X";
	  if (code == FLOAT_TRUNCATE)
	    return "OTS$CVT_FLOAT_X_G";
	}
      else
	{
	  if (code == FLOAT_EXTEND)
	    return "_OtsConvertFloatGX";
	  if (code == FLOAT_TRUNCATE)
	    return "_OtsConvertFloatXG";
	}
    }

  if (TARGET_OPEN_VMS)
    ops = vms_xfloating_ops;
  else
    ops = osf_xfloating_ops;

  for (i = 0; i < n; ++i)
    if (ops[i].code == code)
      return ops[i].func;

  abort();
}

/* Most X_floating operations take the rounding mode as an argument.
   Compute that here.  */

static int
alpha_compute_xfloating_mode_arg (code, round)
     enum rtx_code code;
     enum alpha_fp_rounding_mode round;
{
  int mode;

  switch (round)
    {
    case ALPHA_FPRM_NORM:
      mode = 2;
      break;
    case ALPHA_FPRM_MINF:
      mode = 1;
      break;
    case ALPHA_FPRM_CHOP:
      mode = 0;
      break;
    case ALPHA_FPRM_DYN:
      mode = 4;
      break;
    default:
      abort ();

    /* XXX For reference, round to +inf is mode = 3.  */
    }

  if (code == FLOAT_TRUNCATE && alpha_fptm == ALPHA_FPTM_N)
    mode |= 0x10000;

  return mode;
}

/* Emit an X_floating library function call.

   Note that these functions do not follow normal calling conventions:
   TFmode arguments are passed in two integer registers (as opposed to
   indirect); TFmode return values appear in R16+R17. 

   FUNC is the function name to call.
   TARGET is where the output belongs.
   OPERANDS are the inputs.
   NOPERANDS is the count of inputs.
   EQUIV is the expression equivalent for the function.
*/

static void
alpha_emit_xfloating_libcall (func, target, operands, noperands, equiv)
     const char *func;
     rtx target;
     rtx operands[];
     int noperands;
     rtx equiv;
{
  rtx usage = NULL_RTX, tmp, reg;
  int regno = 16, i;

  start_sequence ();

  for (i = 0; i < noperands; ++i)
    {
      switch (GET_MODE (operands[i]))
	{
	case TFmode:
	  reg = gen_rtx_REG (TFmode, regno);
	  regno += 2;
	  break;

	case DFmode:
	  reg = gen_rtx_REG (DFmode, regno + 32);
	  regno += 1;
	  break;

	case VOIDmode:
	  if (GET_CODE (operands[i]) != CONST_INT)
	    abort ();
	  /* FALLTHRU */
	case DImode:
	  reg = gen_rtx_REG (DImode, regno);
	  regno += 1;
	  break;

	default:
	  abort ();
	}

      emit_move_insn (reg, operands[i]);
      usage = alloc_EXPR_LIST (0, gen_rtx_USE (VOIDmode, reg), usage);
    }

  switch (GET_MODE (target))
    {
    case TFmode:
      reg = gen_rtx_REG (TFmode, 16);
      break;
    case DFmode:
      reg = gen_rtx_REG (DFmode, 32);
      break;
    case DImode:
      reg = gen_rtx_REG (DImode, 0);
      break;
    default:
      abort ();
    }

  tmp = gen_rtx_MEM (QImode, gen_rtx_SYMBOL_REF (Pmode, func));
  tmp = emit_call_insn (gen_call_value (reg, tmp, const0_rtx,
					const0_rtx, const0_rtx));
  CALL_INSN_FUNCTION_USAGE (tmp) = usage;

  tmp = get_insns ();
  end_sequence ();

  emit_libcall_block (tmp, target, reg, equiv);
}

/* Emit an X_floating library function call for arithmetic (+,-,*,/).  */

void
alpha_emit_xfloating_arith (code, operands)
     enum rtx_code code;
     rtx operands[];
{
  const char *func;
  int mode;
  rtx out_operands[3];

  func = alpha_lookup_xfloating_lib_func (code);
  mode = alpha_compute_xfloating_mode_arg (code, alpha_fprm);

  out_operands[0] = operands[1];
  out_operands[1] = operands[2];
  out_operands[2] = GEN_INT (mode);
  alpha_emit_xfloating_libcall (func, operands[0], out_operands, 3,  
				gen_rtx_fmt_ee (code, TFmode, operands[1],
						operands[2]));
}

/* Emit an X_floating library function call for a comparison.  */

static rtx
alpha_emit_xfloating_compare (code, op0, op1)
     enum rtx_code code;
     rtx op0, op1;
{
  const char *func;
  rtx out, operands[2];

  func = alpha_lookup_xfloating_lib_func (code);

  operands[0] = op0;
  operands[1] = op1;
  out = gen_reg_rtx (DImode);

  /* ??? Strange equiv cause what's actually returned is -1,0,1, not a
     proper boolean value.  */
  alpha_emit_xfloating_libcall (func, out, operands, 2, 
				gen_rtx_COMPARE (TFmode, op0, op1));

  return out;
}

/* Emit an X_floating library function call for a conversion.  */

void
alpha_emit_xfloating_cvt (code, operands)
     enum rtx_code code;
     rtx operands[];
{
  int noperands = 1, mode;
  rtx out_operands[2];
  const char *func;

  func = alpha_lookup_xfloating_lib_func (code);

  out_operands[0] = operands[1];

  switch (code)
    {
    case FIX:
      mode = alpha_compute_xfloating_mode_arg (code, ALPHA_FPRM_CHOP);
      out_operands[1] = GEN_INT (mode);
      noperands = 2;
      break;
    case FLOAT_TRUNCATE:
      mode = alpha_compute_xfloating_mode_arg (code, alpha_fprm);
      out_operands[1] = GEN_INT (mode);
      noperands = 2;
      break;
    default:
      break;
    }

  alpha_emit_xfloating_libcall (func, operands[0], out_operands, noperands,
				gen_rtx_fmt_e (code, GET_MODE (operands[0]),
					       operands[1]));
}

void
alpha_split_tfmode_pair (operands)
     rtx operands[4];
{
  if (GET_CODE (operands[1]) == REG)
    {
      operands[3] = gen_rtx_REG (DImode, REGNO (operands[1]) + 1);
      operands[2] = gen_rtx_REG (DImode, REGNO (operands[1]));
    }
  else if (GET_CODE (operands[1]) == MEM)
    {
      operands[3] = change_address (operands[1], DImode,
				    plus_constant (XEXP (operands[1], 0), 8));
      operands[2] = change_address (operands[1], DImode, NULL_RTX);
    }
  else if (operands[1] == CONST0_RTX (TFmode))
    operands[2] = operands[3] = const0_rtx;
  else
    abort ();

  if (GET_CODE (operands[0]) == REG)
    {
      operands[1] = gen_rtx_REG (DImode, REGNO (operands[0]) + 1);
      operands[0] = gen_rtx_REG (DImode, REGNO (operands[0]));
    }
  else if (GET_CODE (operands[0]) == MEM)
    {
      operands[1] = change_address (operands[0], DImode,
				    plus_constant (XEXP (operands[0], 0), 8));
      operands[0] = change_address (operands[0], DImode, NULL_RTX);
    }
  else
    abort ();
}

/* Use ext[wlq][lh] as the Architecture Handbook describes for extracting
   unaligned data:

           unsigned:                       signed:
   word:   ldq_u  r1,X(r11)                ldq_u  r1,X(r11)
           ldq_u  r2,X+1(r11)              ldq_u  r2,X+1(r11)
           lda    r3,X(r11)                lda    r3,X+2(r11)
           extwl  r1,r3,r1                 extql  r1,r3,r1
           extwh  r2,r3,r2                 extqh  r2,r3,r2
           or     r1.r2.r1                 or     r1,r2,r1
                                           sra    r1,48,r1

   long:   ldq_u  r1,X(r11)                ldq_u  r1,X(r11)
           ldq_u  r2,X+3(r11)              ldq_u  r2,X+3(r11)
           lda    r3,X(r11)                lda    r3,X(r11)
           extll  r1,r3,r1                 extll  r1,r3,r1
           extlh  r2,r3,r2                 extlh  r2,r3,r2
           or     r1.r2.r1                 addl   r1,r2,r1

   quad:   ldq_u  r1,X(r11)
           ldq_u  r2,X+7(r11)
           lda    r3,X(r11)
           extql  r1,r3,r1
           extqh  r2,r3,r2
           or     r1.r2.r1
*/

void
alpha_expand_unaligned_load (tgt, mem, size, ofs, sign)
     rtx tgt, mem;
     HOST_WIDE_INT size, ofs;
     int sign;
{
  rtx meml, memh, addr, extl, exth;
  enum machine_mode mode;

  meml = gen_reg_rtx (DImode);
  memh = gen_reg_rtx (DImode);
  addr = gen_reg_rtx (DImode);
  extl = gen_reg_rtx (DImode);
  exth = gen_reg_rtx (DImode);

  emit_move_insn (meml,
		  change_address (mem, DImode,
				  gen_rtx_AND (DImode, 
					       plus_constant (XEXP (mem, 0),
							      ofs),
					       GEN_INT (-8))));

  emit_move_insn (memh,
		  change_address (mem, DImode,
				  gen_rtx_AND (DImode, 
					       plus_constant (XEXP (mem, 0),
							      ofs + size - 1),
					       GEN_INT (-8))));

  if (sign && size == 2)
    {
      emit_move_insn (addr, plus_constant (XEXP (mem, 0), ofs+2));

      emit_insn (gen_extxl (extl, meml, GEN_INT (64), addr));
      emit_insn (gen_extqh (exth, memh, addr));

      /* We must use tgt here for the target.  Alpha-vms port fails if we use
	 addr for the target, because addr is marked as a pointer and combine
	 knows that pointers are always sign-extended 32 bit values.  */
      addr = expand_binop (DImode, ior_optab, extl, exth, tgt, 1, OPTAB_WIDEN);
      addr = expand_binop (DImode, ashr_optab, addr, GEN_INT (48), 
			   addr, 1, OPTAB_WIDEN);
    }
  else
    {
      emit_move_insn (addr, plus_constant (XEXP (mem, 0), ofs));
      emit_insn (gen_extxl (extl, meml, GEN_INT (size*8), addr));
      switch (size)
	{
	case 2:
	  emit_insn (gen_extwh (exth, memh, addr));
	  mode = HImode;
	  break;

	case 4:
	  emit_insn (gen_extlh (exth, memh, addr));
	  mode = SImode;
	  break;

	case 8:
	  emit_insn (gen_extqh (exth, memh, addr));
	  mode = DImode;
	  break;

	default:
	  abort();
	}

      addr = expand_binop (mode, ior_optab, gen_lowpart (mode, extl),
			   gen_lowpart (mode, exth), gen_lowpart (mode, tgt),
			   sign, OPTAB_WIDEN);
    }

  if (addr != tgt)
    emit_move_insn (tgt, gen_lowpart(GET_MODE (tgt), addr));
}

/* Similarly, use ins and msk instructions to perform unaligned stores.  */

void
alpha_expand_unaligned_store (dst, src, size, ofs)
     rtx dst, src;
     HOST_WIDE_INT size, ofs;
{
  rtx dstl, dsth, addr, insl, insh, meml, memh;
  
  dstl = gen_reg_rtx (DImode);
  dsth = gen_reg_rtx (DImode);
  insl = gen_reg_rtx (DImode);
  insh = gen_reg_rtx (DImode);

  meml = change_address (dst, DImode,
			 gen_rtx_AND (DImode, 
				      plus_constant (XEXP (dst, 0), ofs),
				      GEN_INT (-8)));
  memh = change_address (dst, DImode,
			 gen_rtx_AND (DImode, 
				      plus_constant (XEXP (dst, 0),
						     ofs+size-1),
				      GEN_INT (-8)));

  emit_move_insn (dsth, memh);
  emit_move_insn (dstl, meml);
  addr = copy_addr_to_reg (plus_constant (XEXP (dst, 0), ofs));

  if (src != const0_rtx)
    {
      emit_insn (gen_insxh (insh, gen_lowpart (DImode, src),
			    GEN_INT (size*8), addr));

      switch (size)
	{
	case 2:
	  emit_insn (gen_inswl (insl, gen_lowpart (HImode, src), addr));
	  break;
	case 4:
	  emit_insn (gen_insll (insl, gen_lowpart (SImode, src), addr));
	  break;
	case 8:
	  emit_insn (gen_insql (insl, src, addr));
	  break;
	}
    }

  emit_insn (gen_mskxh (dsth, dsth, GEN_INT (size*8), addr));

  switch (size)
    {
    case 2:
      emit_insn (gen_mskxl (dstl, dstl, GEN_INT (0xffff), addr));
      break;
    case 4:
      emit_insn (gen_mskxl (dstl, dstl, GEN_INT (0xffffffff), addr));
      break;
    case 8:
      {
#if HOST_BITS_PER_WIDE_INT == 32
	rtx msk = immed_double_const (0xffffffff, 0xffffffff, DImode);
#else
	rtx msk = immed_double_const (0xffffffffffffffff, 0, DImode);
#endif
	emit_insn (gen_mskxl (dstl, dstl, msk, addr));
      }
      break;
    }

  if (src != const0_rtx)
    {
      dsth = expand_binop (DImode, ior_optab, insh, dsth, dsth, 0, OPTAB_WIDEN);
      dstl = expand_binop (DImode, ior_optab, insl, dstl, dstl, 0, OPTAB_WIDEN);
    }
  
  /* Must store high before low for degenerate case of aligned.  */
  emit_move_insn (memh, dsth);
  emit_move_insn (meml, dstl);
}

/* The block move code tries to maximize speed by separating loads and
   stores at the expense of register pressure: we load all of the data
   before we store it back out.  There are two secondary effects worth
   mentioning, that this speeds copying to/from aligned and unaligned
   buffers, and that it makes the code significantly easier to write.  */

#define MAX_MOVE_WORDS	8

/* Load an integral number of consecutive unaligned quadwords.  */

static void
alpha_expand_unaligned_load_words (out_regs, smem, words, ofs)
     rtx *out_regs;
     rtx smem;
     HOST_WIDE_INT words, ofs;
{
  rtx const im8 = GEN_INT (-8);
  rtx const i64 = GEN_INT (64);
  rtx ext_tmps[MAX_MOVE_WORDS], data_regs[MAX_MOVE_WORDS+1];
  rtx sreg, areg;
  HOST_WIDE_INT i;

  /* Generate all the tmp registers we need.  */
  for (i = 0; i < words; ++i)
    {
      data_regs[i] = out_regs[i];
      ext_tmps[i] = gen_reg_rtx (DImode);
    }
  data_regs[words] = gen_reg_rtx (DImode);

  if (ofs != 0)
    smem = change_address (smem, GET_MODE (smem),
			   plus_constant (XEXP (smem, 0), ofs));
  
  /* Load up all of the source data.  */
  for (i = 0; i < words; ++i)
    {
      emit_move_insn (data_regs[i],
		      change_address (smem, DImode,
				      gen_rtx_AND (DImode,
						   plus_constant (XEXP(smem,0),
								  8*i),
						   im8)));
    }
  emit_move_insn (data_regs[words],
		  change_address (smem, DImode,
				  gen_rtx_AND (DImode,
					       plus_constant (XEXP(smem,0),
							      8*words - 1),
					       im8)));

  /* Extract the half-word fragments.  Unfortunately DEC decided to make
     extxh with offset zero a noop instead of zeroing the register, so 
     we must take care of that edge condition ourselves with cmov.  */

  sreg = copy_addr_to_reg (XEXP (smem, 0));
  areg = expand_binop (DImode, and_optab, sreg, GEN_INT (7), NULL, 
		       1, OPTAB_WIDEN);
  for (i = 0; i < words; ++i)
    {
      emit_insn (gen_extxl (data_regs[i], data_regs[i], i64, sreg));

      emit_insn (gen_extqh (ext_tmps[i], data_regs[i+1], sreg));
      emit_insn (gen_rtx_SET (VOIDmode, ext_tmps[i],
			      gen_rtx_IF_THEN_ELSE (DImode,
						    gen_rtx_EQ (DImode, areg,
								const0_rtx),
						    const0_rtx, ext_tmps[i])));
    }

  /* Merge the half-words into whole words.  */
  for (i = 0; i < words; ++i)
    {
      out_regs[i] = expand_binop (DImode, ior_optab, data_regs[i],
				  ext_tmps[i], data_regs[i], 1, OPTAB_WIDEN);
    }
}

/* Store an integral number of consecutive unaligned quadwords.  DATA_REGS
   may be NULL to store zeros.  */

static void
alpha_expand_unaligned_store_words (data_regs, dmem, words, ofs)
     rtx *data_regs;
     rtx dmem;
     HOST_WIDE_INT words, ofs;
{
  rtx const im8 = GEN_INT (-8);
  rtx const i64 = GEN_INT (64);
#if HOST_BITS_PER_WIDE_INT == 32
  rtx const im1 = immed_double_const (0xffffffff, 0xffffffff, DImode);
#else
  rtx const im1 = immed_double_const (0xffffffffffffffff, 0, DImode);
#endif
  rtx ins_tmps[MAX_MOVE_WORDS];
  rtx st_tmp_1, st_tmp_2, dreg;
  rtx st_addr_1, st_addr_2;
  HOST_WIDE_INT i;

  /* Generate all the tmp registers we need.  */
  if (data_regs != NULL)
    for (i = 0; i < words; ++i)
      ins_tmps[i] = gen_reg_rtx(DImode);
  st_tmp_1 = gen_reg_rtx(DImode);
  st_tmp_2 = gen_reg_rtx(DImode);
  
  if (ofs != 0)
    dmem = change_address (dmem, GET_MODE (dmem),
			   plus_constant (XEXP (dmem, 0), ofs));
  

  st_addr_2 = change_address (dmem, DImode,
			      gen_rtx_AND (DImode,
					   plus_constant (XEXP(dmem,0),
							  words*8 - 1),
				       im8));
  st_addr_1 = change_address (dmem, DImode,
			      gen_rtx_AND (DImode, 
					   XEXP (dmem, 0),
					   im8));

  /* Load up the destination end bits.  */
  emit_move_insn (st_tmp_2, st_addr_2);
  emit_move_insn (st_tmp_1, st_addr_1);

  /* Shift the input data into place.  */
  dreg = copy_addr_to_reg (XEXP (dmem, 0));
  if (data_regs != NULL)
    {
      for (i = words-1; i >= 0; --i)
	{
	  emit_insn (gen_insxh (ins_tmps[i], data_regs[i], i64, dreg));
	  emit_insn (gen_insql (data_regs[i], data_regs[i], dreg));
	}
      for (i = words-1; i > 0; --i)
	{
	  ins_tmps[i-1] = expand_binop (DImode, ior_optab, data_regs[i],
					ins_tmps[i-1], ins_tmps[i-1], 1,
					OPTAB_WIDEN);
	}
    }

  /* Split and merge the ends with the destination data.  */
  emit_insn (gen_mskxh (st_tmp_2, st_tmp_2, i64, dreg));
  emit_insn (gen_mskxl (st_tmp_1, st_tmp_1, im1, dreg));

  if (data_regs != NULL)
    {
      st_tmp_2 = expand_binop (DImode, ior_optab, st_tmp_2, ins_tmps[words-1],
			       st_tmp_2, 1, OPTAB_WIDEN);
      st_tmp_1 = expand_binop (DImode, ior_optab, st_tmp_1, data_regs[0],
			       st_tmp_1, 1, OPTAB_WIDEN);
    }

  /* Store it all.  */
  emit_move_insn (st_addr_2, st_tmp_2);
  for (i = words-1; i > 0; --i)
    {
      emit_move_insn (change_address (dmem, DImode,
				      gen_rtx_AND (DImode,
						   plus_constant(XEXP (dmem,0),
								 i*8),
					       im8)),
		      data_regs ? ins_tmps[i-1] : const0_rtx);
    }
  emit_move_insn (st_addr_1, st_tmp_1);
}


/* Expand string/block move operations.

   operands[0] is the pointer to the destination.
   operands[1] is the pointer to the source.
   operands[2] is the number of bytes to move.
   operands[3] is the alignment.  */

int
alpha_expand_block_move (operands)
     rtx operands[];
{
  rtx bytes_rtx	= operands[2];
  rtx align_rtx = operands[3];
  HOST_WIDE_INT orig_bytes = INTVAL (bytes_rtx);
  HOST_WIDE_INT bytes = orig_bytes;
  HOST_WIDE_INT src_align = INTVAL (align_rtx);
  HOST_WIDE_INT dst_align = src_align;
  rtx orig_src	= operands[1];
  rtx orig_dst	= operands[0];
  rtx data_regs[2*MAX_MOVE_WORDS+16];
  rtx tmp;
  int i, words, ofs, nregs = 0;
  
  if (bytes <= 0)
    return 1;
  if (bytes > MAX_MOVE_WORDS*8)
    return 0;

  /* Look for additional alignment information from recorded register info.  */

  tmp = XEXP (orig_src, 0);
  if (GET_CODE (tmp) == REG)
    {
      if (REGNO_POINTER_ALIGN (REGNO (tmp)) > src_align)
	src_align = REGNO_POINTER_ALIGN (REGNO (tmp));
    }
  else if (GET_CODE (tmp) == PLUS
	   && GET_CODE (XEXP (tmp, 0)) == REG
	   && GET_CODE (XEXP (tmp, 1)) == CONST_INT)
    {
      HOST_WIDE_INT c = INTVAL (XEXP (tmp, 1));
      int a = REGNO_POINTER_ALIGN (REGNO (XEXP (tmp, 0)));

      if (a > src_align)
	{
          if (a >= 8 && c % 8 == 0)
	    src_align = 8;
          else if (a >= 4 && c % 4 == 0)
	    src_align = 4;
          else if (a >= 2 && c % 2 == 0)
	    src_align = 2;
	}
    }
	
  tmp = XEXP (orig_dst, 0);
  if (GET_CODE (tmp) == REG)
    {
      if (REGNO_POINTER_ALIGN (REGNO (tmp)) > dst_align)
	dst_align = REGNO_POINTER_ALIGN (REGNO (tmp));
    }
  else if (GET_CODE (tmp) == PLUS
	   && GET_CODE (XEXP (tmp, 0)) == REG
	   && GET_CODE (XEXP (tmp, 1)) == CONST_INT)
    {
      HOST_WIDE_INT c = INTVAL (XEXP (tmp, 1));
      int a = REGNO_POINTER_ALIGN (REGNO (XEXP (tmp, 0)));

      if (a > dst_align)
	{
          if (a >= 8 && c % 8 == 0)
	    dst_align = 8;
          else if (a >= 4 && c % 4 == 0)
	    dst_align = 4;
          else if (a >= 2 && c % 2 == 0)
	    dst_align = 2;
	}
    }

  /*
   * Load the entire block into registers.
   */

  if (GET_CODE (XEXP (orig_src, 0)) == ADDRESSOF)
    {
      enum machine_mode mode;
      tmp = XEXP (XEXP (orig_src, 0), 0);

      /* Don't use the existing register if we're reading more than
	 is held in the register.  Nor if there is not a mode that
	 handles the exact size.  */
      mode = mode_for_size (bytes * BITS_PER_UNIT, MODE_INT, 1);
      if (mode != BLKmode
	  && GET_MODE_SIZE (GET_MODE (tmp)) >= bytes)
	{
	  if (mode == TImode)
	    {
	      data_regs[nregs] = gen_lowpart (DImode, tmp);
	      data_regs[nregs+1] = gen_highpart (DImode, tmp);
	      nregs += 2;
	    }
	  else
	    data_regs[nregs++] = gen_lowpart (mode, tmp);
	  goto src_done;
	}

      /* No appropriate mode; fall back on memory.  */
      orig_src = change_address (orig_src, GET_MODE (orig_src),
				 copy_addr_to_reg (XEXP (orig_src, 0)));
    }

  ofs = 0;
  if (src_align >= 8 && bytes >= 8)
    {
      words = bytes / 8;

      for (i = 0; i < words; ++i)
	data_regs[nregs+i] = gen_reg_rtx(DImode);

      for (i = 0; i < words; ++i)
	{
	  emit_move_insn (data_regs[nregs+i],
			  change_address (orig_src, DImode,
					  plus_constant (XEXP (orig_src, 0),
							 ofs + i*8)));
	}

      nregs += words;
      bytes -= words * 8;
      ofs += words * 8;
    }
  if (src_align >= 4 && bytes >= 4)
    {
      words = bytes / 4;

      for (i = 0; i < words; ++i)
	data_regs[nregs+i] = gen_reg_rtx(SImode);

      for (i = 0; i < words; ++i)
	{
	  emit_move_insn (data_regs[nregs+i],
			  change_address (orig_src, SImode,
					  plus_constant (XEXP (orig_src, 0),
							 ofs + i*4)));
	}

      nregs += words;
      bytes -= words * 4;
      ofs += words * 4;
    }
  if (bytes >= 16)
    {
      words = bytes / 8;

      for (i = 0; i < words+1; ++i)
	data_regs[nregs+i] = gen_reg_rtx(DImode);

      alpha_expand_unaligned_load_words (data_regs + nregs, orig_src,
					 words, ofs);

      nregs += words;
      bytes -= words * 8;
      ofs += words * 8;
    }
  if (!TARGET_BWX && bytes >= 8)
    {
      data_regs[nregs++] = tmp = gen_reg_rtx (DImode);
      alpha_expand_unaligned_load (tmp, orig_src, 8, ofs, 0);
      bytes -= 8;
      ofs += 8;
    }
  if (!TARGET_BWX && bytes >= 4)
    {
      data_regs[nregs++] = tmp = gen_reg_rtx (SImode);
      alpha_expand_unaligned_load (tmp, orig_src, 4, ofs, 0);
      bytes -= 4;
      ofs += 4;
    }
  if (bytes >= 2)
    {
      if (src_align >= 2)
	{
	  do {
	    data_regs[nregs++] = tmp = gen_reg_rtx (HImode);
	    emit_move_insn (tmp,
			    change_address (orig_src, HImode,
					    plus_constant (XEXP (orig_src, 0),
							   ofs)));
	    bytes -= 2;
	    ofs += 2;
	  } while (bytes >= 2);
	}
      else if (!TARGET_BWX)
	{
	  data_regs[nregs++] = tmp = gen_reg_rtx (HImode);
	  alpha_expand_unaligned_load (tmp, orig_src, 2, ofs, 0);
	  bytes -= 2;
	  ofs += 2;
	}
    }
  while (bytes > 0)
    {
      data_regs[nregs++] = tmp = gen_reg_rtx (QImode);
      emit_move_insn (tmp,
		      change_address (orig_src, QImode,
				      plus_constant (XEXP (orig_src, 0),
						     ofs)));
      bytes -= 1;
      ofs += 1;
    }
 src_done:

  if (nregs > (int)(sizeof(data_regs)/sizeof(*data_regs)))
    abort();

  /*
   * Now save it back out again.
   */

  i = 0, ofs = 0;

  if (GET_CODE (XEXP (orig_dst, 0)) == ADDRESSOF)
    {
      enum machine_mode mode;
      tmp = XEXP (XEXP (orig_dst, 0), 0);

      mode = mode_for_size (orig_bytes * BITS_PER_UNIT, MODE_INT, 1);
      if (GET_MODE (tmp) == mode)
	{
	  if (nregs == 1)
	    {
	      emit_move_insn (tmp, data_regs[0]);
	      i = 1;
	      goto dst_done;
	    }
	  else if (nregs == 2 && mode == TImode)
	    {
	      /* Undo the subregging done above when copying between
		 two TImode registers.  */
	      if (GET_CODE (data_regs[0]) == SUBREG
		  && GET_MODE (SUBREG_REG (data_regs[0])) == TImode)
		{
		  emit_move_insn (tmp, SUBREG_REG (data_regs[0]));
		}
	      else
		{
		  rtx seq;

		  start_sequence ();
		  emit_move_insn (gen_lowpart (DImode, tmp), data_regs[0]);
		  emit_move_insn (gen_highpart (DImode, tmp), data_regs[1]);
		  seq = get_insns ();
		  end_sequence ();

		  emit_no_conflict_block (seq, tmp, data_regs[0],
					  data_regs[1], NULL_RTX);
		}

	      i = 2;
	      goto dst_done;
	    }
	}

      /* ??? If nregs > 1, consider reconstructing the word in regs.  */
      /* ??? Optimize mode < dst_mode with strict_low_part.  */

      /* No appropriate mode; fall back on memory.  We can speed things
	 up by recognizing extra alignment information.  */
      orig_dst = change_address (orig_dst, GET_MODE (orig_dst),
				 copy_addr_to_reg (XEXP (orig_dst, 0)));
      dst_align = GET_MODE_SIZE (GET_MODE (tmp));
    }

  /* Write out the data in whatever chunks reading the source allowed.  */
  if (dst_align >= 8)
    {
      while (i < nregs && GET_MODE (data_regs[i]) == DImode)
	{
	  emit_move_insn (change_address (orig_dst, DImode,
					  plus_constant (XEXP (orig_dst, 0),
							 ofs)),
			  data_regs[i]);
	  ofs += 8;
	  i++;
	}
    }
  if (dst_align >= 4)
    {
      /* If the source has remaining DImode regs, write them out in
	 two pieces.  */
      while (i < nregs && GET_MODE (data_regs[i]) == DImode)
	{
	  tmp = expand_binop (DImode, lshr_optab, data_regs[i], GEN_INT (32),
			      NULL_RTX, 1, OPTAB_WIDEN);

	  emit_move_insn (change_address (orig_dst, SImode,
					  plus_constant (XEXP (orig_dst, 0),
							 ofs)),
			  gen_lowpart (SImode, data_regs[i]));
	  emit_move_insn (change_address (orig_dst, SImode,
					  plus_constant (XEXP (orig_dst, 0),
							 ofs+4)),
			  gen_lowpart (SImode, tmp));
	  ofs += 8;
	  i++;
	}

      while (i < nregs && GET_MODE (data_regs[i]) == SImode)
	{
	  emit_move_insn (change_address(orig_dst, SImode,
					 plus_constant (XEXP (orig_dst, 0),
							ofs)),
			  data_regs[i]);
	  ofs += 4;
	  i++;
	}
    }
  if (i < nregs && GET_MODE (data_regs[i]) == DImode)
    {
      /* Write out a remaining block of words using unaligned methods.  */

      for (words = 1; i+words < nregs ; ++words)
	if (GET_MODE (data_regs[i+words]) != DImode)
	  break;

      if (words == 1)
	alpha_expand_unaligned_store (orig_dst, data_regs[i], 8, ofs);
      else
        alpha_expand_unaligned_store_words (data_regs+i, orig_dst, words, ofs);
     
      i += words;
      ofs += words * 8;
    }

  /* Due to the above, this won't be aligned.  */
  /* ??? If we have more than one of these, consider constructing full
     words in registers and using alpha_expand_unaligned_store_words.  */
  while (i < nregs && GET_MODE (data_regs[i]) == SImode)
    {
      alpha_expand_unaligned_store (orig_dst, data_regs[i], 4, ofs);
      ofs += 4;
      i++;
    }

  if (dst_align >= 2)
    while (i < nregs && GET_MODE (data_regs[i]) == HImode)
      {
	emit_move_insn (change_address (orig_dst, HImode,
					plus_constant (XEXP (orig_dst, 0),
						       ofs)),
			data_regs[i]);
	i++;
	ofs += 2;
      }
  else
    while (i < nregs && GET_MODE (data_regs[i]) == HImode)
      {
	alpha_expand_unaligned_store (orig_dst, data_regs[i], 2, ofs);
	i++;
	ofs += 2;
      }
  while (i < nregs && GET_MODE (data_regs[i]) == QImode)
    {
      emit_move_insn (change_address (orig_dst, QImode,
				      plus_constant (XEXP (orig_dst, 0),
						     ofs)),
		      data_regs[i]);
      i++;
      ofs += 1;
    }
 dst_done:

  if (i != nregs)
    abort();

  return 1;
}

int
alpha_expand_block_clear (operands)
     rtx operands[];
{
  rtx bytes_rtx	= operands[1];
  rtx align_rtx = operands[2];
  HOST_WIDE_INT bytes = INTVAL (bytes_rtx);
  HOST_WIDE_INT align = INTVAL (align_rtx);
  rtx orig_dst	= operands[0];
  rtx tmp;
  HOST_WIDE_INT i, words, ofs = 0;
  
  if (bytes <= 0)
    return 1;
  if (bytes > MAX_MOVE_WORDS*8)
    return 0;

  /* Look for stricter alignment.  */

  tmp = XEXP (orig_dst, 0);
  if (GET_CODE (tmp) == REG)
    {
      if (REGNO_POINTER_ALIGN (REGNO (tmp)) > align)
	align = REGNO_POINTER_ALIGN (REGNO (tmp));
    }
  else if (GET_CODE (tmp) == PLUS
	   && GET_CODE (XEXP (tmp, 0)) == REG
	   && GET_CODE (XEXP (tmp, 1)) == CONST_INT)
    {
      HOST_WIDE_INT c = INTVAL (XEXP (tmp, 1));
      int a = REGNO_POINTER_ALIGN (REGNO (XEXP (tmp, 0)));

      if (a > align)
	{
          if (a >= 8 && c % 8 == 0)
	    align = 8;
          else if (a >= 4 && c % 4 == 0)
	    align = 4;
          else if (a >= 2 && c % 2 == 0)
	    align = 2;
	}
    }
  else if (GET_CODE (tmp) == ADDRESSOF)
    {
      enum machine_mode mode;

      mode = mode_for_size (bytes * BITS_PER_UNIT, MODE_INT, 1);
      if (GET_MODE (XEXP (tmp, 0)) == mode)
	{
	  emit_move_insn (XEXP (tmp, 0), const0_rtx);
	  return 1;
	}

      /* No appropriate mode; fall back on memory.  */
      orig_dst = change_address (orig_dst, GET_MODE (orig_dst),
				 copy_addr_to_reg (tmp));
      align = GET_MODE_SIZE (GET_MODE (XEXP (tmp, 0)));
    }

  /* Handle a block of contiguous words first.  */

  if (align >= 8 && bytes >= 8)
    {
      words = bytes / 8;

      for (i = 0; i < words; ++i)
	{
	  emit_move_insn (change_address(orig_dst, DImode,
					 plus_constant (XEXP (orig_dst, 0),
							ofs + i*8)),
			  const0_rtx);
	}

      bytes -= words * 8;
      ofs += words * 8;
    }
  if (align >= 4 && bytes >= 4)
    {
      words = bytes / 4;

      for (i = 0; i < words; ++i)
	{
	  emit_move_insn (change_address (orig_dst, SImode,
					  plus_constant (XEXP (orig_dst, 0),
							 ofs + i*4)),
			  const0_rtx);
	}

      bytes -= words * 4;
      ofs += words * 4;
    }
  if (bytes >= 16)
    {
      words = bytes / 8;

      alpha_expand_unaligned_store_words (NULL, orig_dst, words, ofs);

      bytes -= words * 8;
      ofs += words * 8;
    }

  /* Next clean up any trailing pieces.  We know from the contiguous
     block move that there are no aligned SImode or DImode hunks left.  */

  if (!TARGET_BWX && bytes >= 8)
    {
      alpha_expand_unaligned_store (orig_dst, const0_rtx, 8, ofs);
      bytes -= 8;
      ofs += 8;
    }
  if (!TARGET_BWX && bytes >= 4)
    {
      alpha_expand_unaligned_store (orig_dst, const0_rtx, 4, ofs);
      bytes -= 4;
      ofs += 4;
    }
  if (bytes >= 2)
    {
      if (align >= 2)
	{
	  do {
	    emit_move_insn (change_address (orig_dst, HImode,
					    plus_constant (XEXP (orig_dst, 0),
							   ofs)),
			    const0_rtx);
	    bytes -= 2;
	    ofs += 2;
	  } while (bytes >= 2);
	}
      else if (!TARGET_BWX)
	{
	  alpha_expand_unaligned_store (orig_dst, const0_rtx, 2, ofs);
	  bytes -= 2;
	  ofs += 2;
	}
    }
  while (bytes > 0)
    {
      emit_move_insn (change_address (orig_dst, QImode,
				      plus_constant (XEXP (orig_dst, 0),
						     ofs)),
		      const0_rtx);
      bytes -= 1;
      ofs += 1;
    }

  return 1;
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
  rtx set, set_src;
  enum attr_type insn_type, dep_insn_type;

  /* If the dependence is an anti-dependence, there is no cost.  For an
     output dependence, there is sometimes a cost, but it doesn't seem
     worth handling those few cases.  */

  if (REG_NOTE_KIND (link) != 0)
    return 0;

  /* If we can't recognize the insns, we can't really do anything.  */
  if (recog_memoized (insn) < 0 || recog_memoized (dep_insn) < 0)
    return cost;

  insn_type = get_attr_type (insn);
  dep_insn_type = get_attr_type (dep_insn);

  /* Bring in the user-defined memory latency.  */
  if (dep_insn_type == TYPE_ILD
      || dep_insn_type == TYPE_FLD
      || dep_insn_type == TYPE_LDSYM)
    cost += alpha_memory_latency-1;

  switch (alpha_cpu)
    {
    case PROCESSOR_EV4:
      /* On EV4, if INSN is a store insn and DEP_INSN is setting the data
	 being stored, we can sometimes lower the cost.  */

      if ((insn_type == TYPE_IST || insn_type == TYPE_FST)
	  && (set = single_set (dep_insn)) != 0
	  && GET_CODE (PATTERN (insn)) == SET
	  && rtx_equal_p (SET_DEST (set), SET_SRC (PATTERN (insn))))
	{
	  switch (dep_insn_type)
	    {
	    case TYPE_ILD:
	    case TYPE_FLD:
	      /* No savings here.  */
	      return cost;

	    case TYPE_IMUL:
	      /* In these cases, we save one cycle.  */
	      return cost - 1;

	    default:
	      /* In all other cases, we save two cycles.  */
	      return MAX (0, cost - 2);
	    }
	}

      /* Another case that needs adjustment is an arithmetic or logical
	 operation.  It's cost is usually one cycle, but we default it to
	 two in the MD file.  The only case that it is actually two is
	 for the address in loads, stores, and jumps.  */

      if (dep_insn_type == TYPE_IADD || dep_insn_type == TYPE_ILOG)
	{
	  switch (insn_type)
	    {
	    case TYPE_ILD:
	    case TYPE_IST:
	    case TYPE_FLD:
	    case TYPE_FST:
	    case TYPE_JSR:
	      return cost;
	    default:
	      return 1;
	    }
	}

      /* The final case is when a compare feeds into an integer branch;
	 the cost is only one cycle in that case.  */

      if (dep_insn_type == TYPE_ICMP && insn_type == TYPE_IBR)
	return 1;
      break;

    case PROCESSOR_EV5:
      /* And the lord DEC saith:  "A special bypass provides an effective
	 latency of 0 cycles for an ICMP or ILOG insn producing the test
	 operand of an IBR or ICMOV insn." */

      if ((dep_insn_type == TYPE_ICMP || dep_insn_type == TYPE_ILOG)
	  && (set = single_set (dep_insn)) != 0)
	{
	  /* A branch only has one input.  This must be it.  */
	  if (insn_type == TYPE_IBR)
	    return 0;
	  /* A conditional move has three, make sure it is the test.  */
	  if (insn_type == TYPE_ICMOV
	      && GET_CODE (set_src = PATTERN (insn)) == SET
	      && GET_CODE (set_src = SET_SRC (set_src)) == IF_THEN_ELSE
	      && rtx_equal_p (SET_DEST (set), XEXP (set_src, 0)))
	    return 0;
	}

      /* "The multiplier is unable to receive data from IEU bypass paths.
	 The instruction issues at the expected time, but its latency is
	 increased by the time it takes for the input data to become
	 available to the multiplier" -- which happens in pipeline stage
	 six, when results are comitted to the register file.  */

      if (insn_type == TYPE_IMUL)
	{
	  switch (dep_insn_type)
	    {
	    /* These insns produce their results in pipeline stage five.  */
	    case TYPE_ILD:
	    case TYPE_ICMOV:
	    case TYPE_IMUL:
	    case TYPE_MVI:
	      return cost + 1;

	    /* Other integer insns produce results in pipeline stage four.  */
	    default:
	      return cost + 2;
	    }
	}
      break;

    case PROCESSOR_EV6:
      /* There is additional latency to move the result of (most) FP 
         operations anywhere but the FP register file.  */

      if ((insn_type == TYPE_FST || insn_type == TYPE_FTOI)
	  && (dep_insn_type == TYPE_FADD ||
	      dep_insn_type == TYPE_FMUL ||
	      dep_insn_type == TYPE_FCMOV))
        return cost + 2;

      break;
    }

  /* Otherwise, return the default cost. */
  return cost;
}

/* Functions to save and restore alpha_return_addr_rtx.  */

static void
alpha_init_machine_status (p)
     struct function *p;
{
  p->machine =
    (struct machine_function *) xcalloc (1, sizeof (struct machine_function));
}

static void
alpha_mark_machine_status (p)
     struct function *p;
{
  struct machine_function *machine = p->machine;

  ggc_mark_rtx (machine->eh_epilogue_sp_ofs);
  ggc_mark_rtx (machine->ra_rtx);
}

/* Start the ball rolling with RETURN_ADDR_RTX.  */

rtx
alpha_return_addr (count, frame)
     int count;
     rtx frame ATTRIBUTE_UNUSED;
{
  rtx init, reg;

  if (count != 0)
    return const0_rtx;

  reg = cfun->machine->ra_rtx;
  if (reg == NULL)
    {
      /* No rtx yet.  Invent one, and initialize it from $26 in
	 the prologue.  */
      reg = gen_reg_rtx (Pmode);
      cfun->machine->ra_rtx = reg;
      init = gen_rtx_SET (VOIDmode, reg, gen_rtx_REG (Pmode, REG_RA));

      /* Emit the insn to the prologue with the other argument copies.  */
      push_topmost_sequence ();
      emit_insn_after (init, get_insns ());
      pop_topmost_sequence ();
    }

  return reg;
}

static int
alpha_ra_ever_killed ()
{
  rtx top;

#ifdef ASM_OUTPUT_MI_THUNK
  if (current_function_is_thunk)
    return 0;
#endif
  if (!cfun->machine->ra_rtx)
    return regs_ever_live[REG_RA];

  push_topmost_sequence ();
  top = get_insns ();
  pop_topmost_sequence ();

  return reg_set_between_p (gen_rtx_REG (Pmode, REG_RA), top, NULL_RTX);
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
	default:
	  abort ();
	}
      break;

    case '\'':
      /* Generates trap-mode suffix for instructions that accept the su
	 suffix only (cmpt et al).  */
      if (alpha_fptm >= ALPHA_FPTM_SU)
	fputs ("su", file);
      break;

    case '`':
      /* Generates trap-mode suffix for instructions that accept the
	 v and sv suffix.  The only instruction that needs this is cvtql.  */
      switch (alpha_fptm)
	{
	case ALPHA_FPTM_N:
	  break;
	case ALPHA_FPTM_U:
	  fputs ("v", file);
	  break;
	case ALPHA_FPTM_SU:
	case ALPHA_FPTM_SUI:
	  fputs ("sv", file);
	  break;
	}
      break;

    case '(':
      /* Generates trap-mode suffix for instructions that accept the
	 v, sv, and svi suffix.  The only instruction that needs this
	 is cvttq.  */
      switch (alpha_fptm)
	{
	case ALPHA_FPTM_N:
	  break;
	case ALPHA_FPTM_U:
	  fputs ("v", file);
	  break;
	case ALPHA_FPTM_SU:
	  fputs ("sv", file);
	  break;
	case ALPHA_FPTM_SUI:
	  fputs ("svi", file);
	  break;
	}
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
	case ALPHA_FPTM_N:
	case ALPHA_FPTM_U:
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
      /* 'b', 'w', 'l', or 'q' as the value of the constant.  */
      if (GET_CODE (x) != CONST_INT
	  || (INTVAL (x) != 8 && INTVAL (x) != 16
	      && INTVAL (x) != 32 && INTVAL (x) != 64))
	output_operand_lossage ("invalid %%M value");

      fprintf (file, "%s",
	       (INTVAL (x) == 8 ? "b"
		: INTVAL (x) == 16 ? "w"
		: INTVAL (x) == 32 ? "l"
		: "q"));
      break;

    case 'U':
      /* Similar, except do it from the mask.  */
      if (GET_CODE (x) == CONST_INT && INTVAL (x) == 0xff)
	fprintf (file, "b");
      else if (GET_CODE (x) == CONST_INT && INTVAL (x) == 0xffff)
	fprintf (file, "w");
      else if (GET_CODE (x) == CONST_INT && INTVAL (x) == 0xffffffff)
	fprintf (file, "l");
#if HOST_BITS_PER_WIDE_INT == 32
      else if (GET_CODE (x) == CONST_DOUBLE
	       && CONST_DOUBLE_HIGH (x) == 0
	       && CONST_DOUBLE_LOW (x) == -1)
	fprintf (file, "l");
      else if (GET_CODE (x) == CONST_DOUBLE
	       && CONST_DOUBLE_HIGH (x) == -1
	       && CONST_DOUBLE_LOW (x) == -1)
	fprintf (file, "q");
#else
      else if (GET_CODE (x) == CONST_INT && INTVAL (x) == -1)
	fprintf (file, "q");
      else if (GET_CODE (x) == CONST_DOUBLE
	       && CONST_DOUBLE_HIGH (x) == 0
	       && CONST_DOUBLE_LOW (x) == -1)
	fprintf (file, "q");
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
	else if (c == UNORDERED)
	  fprintf (file, "un");
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

void
print_operand_address (file, addr)
    FILE *file;
     rtx addr;
{
  int basereg = 31;
  HOST_WIDE_INT offset = 0;

  if (GET_CODE (addr) == AND)
    addr = XEXP (addr, 0);

  if (GET_CODE (addr) == PLUS
      && GET_CODE (XEXP (addr, 1)) == CONST_INT)
    {
      offset = INTVAL (XEXP (addr, 1));
      addr = XEXP (addr, 0);
    }
  if (GET_CODE (addr) == REG)
    basereg = REGNO (addr);
  else if (GET_CODE (addr) == SUBREG
	   && GET_CODE (SUBREG_REG (addr)) == REG)
    basereg = REGNO (SUBREG_REG (addr)) + SUBREG_WORD (addr);
  else if (GET_CODE (addr) == CONST_INT)
    offset = INTVAL (addr);
  else
    abort ();

  fprintf (file, HOST_WIDE_INT_PRINT_DEC, offset);
  fprintf (file, "($%d)", basereg);
}

/* Emit RTL insns to initialize the variable parts of a trampoline at
   TRAMP. FNADDR is an RTX for the address of the function's pure
   code.  CXT is an RTX for the static chain value for the function.

   The three offset parameters are for the individual template's
   layout.  A JMPOFS < 0 indicates that the trampoline does not 
   contain instructions at all.

   We assume here that a function will be called many more times than
   its address is taken (e.g., it might be passed to qsort), so we
   take the trouble to initialize the "hint" field in the JMP insn.
   Note that the hint field is PC (new) + 4 * bits 13:0.  */

void
alpha_initialize_trampoline (tramp, fnaddr, cxt, fnofs, cxtofs, jmpofs)
     rtx tramp, fnaddr, cxt;
     int fnofs, cxtofs, jmpofs;
{
  rtx temp, temp1, addr;
  /* VMS really uses DImode pointers in memory at this point.  */
  enum machine_mode mode = TARGET_OPEN_VMS ? Pmode : ptr_mode;

#ifdef POINTERS_EXTEND_UNSIGNED
  fnaddr = convert_memory_address (mode, fnaddr);
  cxt = convert_memory_address (mode, cxt);
#endif

  /* Store function address and CXT.  */
  addr = memory_address (mode, plus_constant (tramp, fnofs));
  emit_move_insn (gen_rtx_MEM (mode, addr), fnaddr);
  addr = memory_address (mode, plus_constant (tramp, cxtofs));
  emit_move_insn (gen_rtx_MEM (mode, addr), cxt);

  /* This has been disabled since the hint only has a 32k range, and in
     no existing OS is the stack within 32k of the text segment. */
  if (0 && jmpofs >= 0)
    {
      /* Compute hint value.  */
      temp = force_operand (plus_constant (tramp, jmpofs+4), NULL_RTX);
      temp = expand_binop (DImode, sub_optab, fnaddr, temp, temp, 1,
			   OPTAB_WIDEN);
      temp = expand_shift (RSHIFT_EXPR, Pmode, temp,
		           build_int_2 (2, 0), NULL_RTX, 1);
      temp = expand_and (gen_lowpart (SImode, temp), GEN_INT (0x3fff), 0);

      /* Merge in the hint.  */
      addr = memory_address (SImode, plus_constant (tramp, jmpofs));
      temp1 = force_reg (SImode, gen_rtx_MEM (SImode, addr));
      temp1 = expand_and (temp1, GEN_INT (0xffffc000), NULL_RTX);
      temp1 = expand_binop (SImode, ior_optab, temp1, temp, temp1, 1,
			    OPTAB_WIDEN);
      emit_move_insn (gen_rtx_MEM (SImode, addr), temp1);
    }

#ifdef TRANSFER_FROM_TRAMPOLINE
  emit_library_call (gen_rtx_SYMBOL_REF (Pmode, "__enable_execute_stack"),
		     0, VOIDmode, 1, addr, Pmode);
#endif

  if (jmpofs >= 0)
    emit_insn (gen_imb ());
}

/* Determine where to put an argument to a function.
   Value is zero to push the argument on the stack,
   or a hard register in which to store the argument.

   MODE is the argument's machine mode.
   TYPE is the data type of the argument (as a tree).
    This is null for libcalls where that information may
    not be available.
   CUM is a variable of type CUMULATIVE_ARGS which gives info about
    the preceding args and about the function being called.
   NAMED is nonzero if this argument is a named parameter
    (otherwise it is an extra parameter matching an ellipsis).

   On Alpha the first 6 words of args are normally in registers
   and the rest are pushed.  */

rtx
function_arg(cum, mode, type, named)
     CUMULATIVE_ARGS cum;
     enum machine_mode mode;
     tree type;
     int named ATTRIBUTE_UNUSED;
{
  int basereg;

  if (cum >= 6 || MUST_PASS_IN_STACK (mode, type))
    return NULL_RTX;

  if (FUNCTION_ARG_PASS_BY_REFERENCE (cum, mode, type, named))
    basereg = 16;
  else if (TARGET_FPREGS
	   && (GET_MODE_CLASS (mode) == MODE_COMPLEX_FLOAT
	       || GET_MODE_CLASS (mode) == MODE_FLOAT))
    basereg = 32 + 16;
  else
    basereg = 16;

  return gen_rtx_REG (mode, cum + basereg);
}

tree
alpha_build_va_list ()
{
  tree base, ofs, record, type_decl;

  if (TARGET_OPEN_VMS)
    return ptr_type_node;

  record = make_lang_type (RECORD_TYPE);
  type_decl = build_decl (TYPE_DECL, get_identifier ("__va_list_tag"), record);
  TREE_CHAIN (record) = type_decl;
  TYPE_NAME (record) = type_decl;

  /* C++? SET_IS_AGGR_TYPE (record, 1); */

  ofs = build_decl (FIELD_DECL, get_identifier ("__offset"),
		    integer_type_node);
  DECL_FIELD_CONTEXT (ofs) = record;

  base = build_decl (FIELD_DECL, get_identifier ("__base"),
		     ptr_type_node);
  DECL_FIELD_CONTEXT (base) = record;
  TREE_CHAIN (base) = ofs;

  TYPE_FIELDS (record) = base;
  layout_type (record);

  return record;
}

void
alpha_va_start (stdarg_p, valist, nextarg)
     int stdarg_p;
     tree valist;
     rtx nextarg ATTRIBUTE_UNUSED;
{
  HOST_WIDE_INT offset;
  tree t, offset_field, base_field;

  if (TARGET_OPEN_VMS)
    std_expand_builtin_va_start (stdarg_p, valist, nextarg);

  /* For Unix, SETUP_INCOMING_VARARGS moves the starting address base
     up by 48, storing fp arg registers in the first 48 bytes, and the
     integer arg registers in the next 48 bytes.  This is only done,
     however, if any integer registers need to be stored.

     If no integer registers need be stored, then we must subtract 48
     in order to account for the integer arg registers which are counted
     in argsize above, but which are not actually stored on the stack.  */

  if (NUM_ARGS <= 5 + stdarg_p)
    offset = 6 * UNITS_PER_WORD;
  else
    offset = -6 * UNITS_PER_WORD;

  base_field = TYPE_FIELDS (TREE_TYPE (valist));
  offset_field = TREE_CHAIN (base_field);

  base_field = build (COMPONENT_REF, TREE_TYPE (base_field),
		      valist, base_field);
  offset_field = build (COMPONENT_REF, TREE_TYPE (offset_field),
			valist, offset_field);

  t = make_tree (ptr_type_node, virtual_incoming_args_rtx);
  t = build (PLUS_EXPR, ptr_type_node, t, build_int_2 (offset, 0));
  t = build (MODIFY_EXPR, TREE_TYPE (base_field), base_field, t);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  t = build_int_2 (NUM_ARGS*UNITS_PER_WORD, 0);
  t = build (MODIFY_EXPR, TREE_TYPE (offset_field), offset_field, t);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
}

rtx
alpha_va_arg (valist, type)
     tree valist, type;
{
  HOST_WIDE_INT tsize;
  rtx addr;
  tree t;
  tree offset_field, base_field, addr_tree, addend;
  tree wide_type, wide_ofs;

  if (TARGET_OPEN_VMS)
    return std_expand_builtin_va_arg (valist, type);

  tsize = ((TREE_INT_CST_LOW (TYPE_SIZE (type)) / BITS_PER_UNIT + 7) / 8) * 8;

  base_field = TYPE_FIELDS (TREE_TYPE (valist));
  offset_field = TREE_CHAIN (base_field);

  base_field = build (COMPONENT_REF, TREE_TYPE (base_field),
		      valist, base_field);
  offset_field = build (COMPONENT_REF, TREE_TYPE (offset_field),
			valist, offset_field);

  wide_type = make_signed_type (64);
  wide_ofs = save_expr (build1 (CONVERT_EXPR, wide_type, offset_field));

  addend = wide_ofs;
  if (FLOAT_TYPE_P (type))
    {
      tree fpaddend, cond;

      fpaddend = fold (build (PLUS_EXPR, TREE_TYPE (addend),
			      addend, build_int_2 (-6*8, 0)));

      cond = fold (build (LT_EXPR, integer_type_node,
			  wide_ofs, build_int_2 (6*8, 0)));

      addend = fold (build (COND_EXPR, TREE_TYPE (addend), cond,
			    fpaddend, addend));
    }

  addr_tree = build (PLUS_EXPR, TREE_TYPE (base_field),
		     base_field, addend);

  addr = expand_expr (addr_tree, NULL_RTX, Pmode, EXPAND_NORMAL);
  addr = copy_to_reg (addr);

  t = build (MODIFY_EXPR, TREE_TYPE (offset_field), offset_field,
	     build (PLUS_EXPR, TREE_TYPE (offset_field), 
		    offset_field, build_int_2 (tsize, 0)));
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  return addr;
}

/* This page contains routines that are used to determine what the function
   prologue and epilogue code will do and write them out.  */

/* Compute the size of the save area in the stack.  */

/* These variables are used for communication between the following functions.
   They indicate various things about the current function being compiled
   that are used to tell what kind of prologue, epilogue and procedure
   descriptior to generate. */

/* Nonzero if we need a stack procedure.  */
static int vms_is_stack_procedure;

/* Register number (either FP or SP) that is used to unwind the frame.  */
static int vms_unwind_regno;

/* Register number used to save FP.  We need not have one for RA since
   we don't modify it for register procedures.  This is only defined
   for register frame procedures.  */
static int vms_save_fp_regno;

/* Register number used to reference objects off our PV.  */
static int vms_base_regno;

/* Compute register masks for saved registers.  */

static void
alpha_sa_mask (imaskP, fmaskP)
    unsigned long *imaskP;
    unsigned long *fmaskP;
{
  unsigned long imask = 0;
  unsigned long fmask = 0;
  int i;

#ifdef ASM_OUTPUT_MI_THUNK
  if (!current_function_is_thunk)
#endif
    {
      if (TARGET_OPEN_VMS && vms_is_stack_procedure)
	imask |= (1L << HARD_FRAME_POINTER_REGNUM);

      /* One for every register we have to save.  */
      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	if (! fixed_regs[i] && ! call_used_regs[i]
	    && regs_ever_live[i] && i != REG_RA)
	  {
	    if (i < 32)
	      imask |= (1L << i);
	    else
	      fmask |= (1L << (i - 32));
	  }

      if (imask || fmask || alpha_ra_ever_killed ())
	imask |= (1L << REG_RA);
    }

  *imaskP = imask;
  *fmaskP = fmask;
}

int
alpha_sa_size ()
{
  int sa_size = 0;
  int i;

#ifdef ASM_OUTPUT_MI_THUNK
  if (current_function_is_thunk)
    sa_size = 0;
  else
#endif
    {
      /* One for every register we have to save.  */
      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	if (! fixed_regs[i] && ! call_used_regs[i]
	    && regs_ever_live[i] && i != REG_RA)
	  sa_size++;
    }

  if (TARGET_OPEN_VMS)
    {
      /* Start by assuming we can use a register procedure if we don't
	 make any calls (REG_RA not used) or need to save any
	 registers and a stack procedure if we do.  */
      vms_is_stack_procedure = sa_size != 0 || alpha_ra_ever_killed ();

      /* Decide whether to refer to objects off our PV via FP or PV.
	 If we need FP for something else or if we receive a nonlocal
	 goto (which expects PV to contain the value), we must use PV.
	 Otherwise, start by assuming we can use FP.  */
      vms_base_regno = (frame_pointer_needed
			|| current_function_has_nonlocal_label
			|| vms_is_stack_procedure
			|| current_function_outgoing_args_size
			? REG_PV : HARD_FRAME_POINTER_REGNUM);

      /* If we want to copy PV into FP, we need to find some register
	 in which to save FP.  */

      vms_save_fp_regno = -1;
      if (vms_base_regno == HARD_FRAME_POINTER_REGNUM)
	for (i = 0; i < 32; i++)
	  if (! fixed_regs[i] && call_used_regs[i] && ! regs_ever_live[i])
	    vms_save_fp_regno = i;

      if (vms_save_fp_regno == -1)
	vms_base_regno = REG_PV, vms_is_stack_procedure = 1;

      /* Stack unwinding should be done via FP unless we use it for PV.  */
      vms_unwind_regno = (vms_base_regno == REG_PV
			  ? HARD_FRAME_POINTER_REGNUM : STACK_POINTER_REGNUM);

      /* If this is a stack procedure, allow space for saving FP and RA.  */
      if (vms_is_stack_procedure)
	sa_size += 2;
    }
  else
    {
      /* If some registers were saved but not RA, RA must also be saved,
	 so leave space for it.  */
      if (sa_size != 0 || alpha_ra_ever_killed ())
	sa_size++;

      /* Our size must be even (multiple of 16 bytes).  */
      if (sa_size & 1)
	sa_size++;
    }

  return sa_size * 8;
}

int
alpha_pv_save_size ()
{
  alpha_sa_size ();
  return vms_is_stack_procedure ? 8 : 0;
}

int
alpha_using_fp ()
{
  alpha_sa_size ();
  return vms_unwind_regno == HARD_FRAME_POINTER_REGNUM;
}

int
vms_valid_decl_attribute_p (decl, attributes, identifier, args)
     tree decl ATTRIBUTE_UNUSED;
     tree attributes ATTRIBUTE_UNUSED;
     tree identifier;
     tree args;
{
  if (is_attribute_p ("overlaid", identifier))
    return (args == NULL_TREE);
  return 0;
}

static int
alpha_does_function_need_gp ()
{
  rtx insn;

  /* We never need a GP for Windows/NT or VMS.  */
  if (TARGET_WINDOWS_NT || TARGET_OPEN_VMS)
    return 0;

#ifdef TARGET_PROFILING_NEEDS_GP
  if (profile_flag)
    return 1;
#endif

#ifdef ASM_OUTPUT_MI_THUNK
  if (current_function_is_thunk)
    return 1;
#endif

  /* If we need a GP (we have a LDSYM insn or a CALL_INSN), load it first. 
     Even if we are a static function, we still need to do this in case
     our address is taken and passed to something like qsort.  */

  push_topmost_sequence ();
  insn = get_insns ();
  pop_topmost_sequence ();

  for (; insn; insn = NEXT_INSN (insn))
    if (GET_RTX_CLASS (GET_CODE (insn)) == 'i'
	&& GET_CODE (PATTERN (insn)) != USE
	&& GET_CODE (PATTERN (insn)) != CLOBBER)
      {
	enum attr_type type = get_attr_type (insn);
	if (type == TYPE_LDSYM || type == TYPE_JSR)
	  return 1;
      }

  return 0;
}

/* Write a version stamp.  Don't write anything if we are running as a
   cross-compiler.  Otherwise, use the versions in /usr/include/stamp.h.  */

#ifdef HAVE_STAMP_H
#include <stamp.h>
#endif

void
alpha_write_verstamp (file)
     FILE *file ATTRIBUTE_UNUSED;
{
#ifdef MS_STAMP
  fprintf (file, "\t.verstamp %d %d\n", MS_STAMP, LS_STAMP);
#endif
}

/* Helper function to set RTX_FRAME_RELATED_P on instructions, including
   sequences.  */

static rtx
set_frame_related_p ()
{
  rtx seq = gen_sequence ();
  end_sequence ();

  if (GET_CODE (seq) == SEQUENCE)
    {
      int i = XVECLEN (seq, 0);
      while (--i >= 0)
	RTX_FRAME_RELATED_P (XVECEXP (seq, 0, i)) = 1;
     return emit_insn (seq);
    }
  else
    {
      seq = emit_insn (seq);
      RTX_FRAME_RELATED_P (seq) = 1;
      return seq;
    }
}

#define FRP(exp)  (start_sequence (), exp, set_frame_related_p ())

/* Write function prologue.  */

/* On vms we have two kinds of functions:

   - stack frame (PROC_STACK)
	these are 'normal' functions with local vars and which are
	calling other functions
   - register frame (PROC_REGISTER)
	keeps all data in registers, needs no stack

   We must pass this to the assembler so it can generate the
   proper pdsc (procedure descriptor)
   This is done with the '.pdesc' command.

   On not-vms, we don't really differentiate between the two, as we can
   simply allocate stack without saving registers.  */

void
alpha_expand_prologue ()
{
  /* Registers to save.  */
  unsigned long imask = 0;
  unsigned long fmask = 0;
  /* Stack space needed for pushing registers clobbered by us.  */
  HOST_WIDE_INT sa_size;
  /* Complete stack size needed.  */
  HOST_WIDE_INT frame_size;
  /* Offset from base reg to register save area.  */
  HOST_WIDE_INT reg_offset;
  rtx sa_reg, mem;
  int i;

  sa_size = alpha_sa_size ();

  frame_size = get_frame_size ();
  if (TARGET_OPEN_VMS)
    frame_size = ALPHA_ROUND (sa_size 
			      + (vms_is_stack_procedure ? 8 : 0)
			      + frame_size
			      + current_function_pretend_args_size);
  else
    frame_size = (ALPHA_ROUND (current_function_outgoing_args_size)
		  + sa_size
		  + ALPHA_ROUND (frame_size
				 + current_function_pretend_args_size));

  if (TARGET_OPEN_VMS)
    reg_offset = 8;
  else
    reg_offset = ALPHA_ROUND (current_function_outgoing_args_size);

  alpha_sa_mask (&imask, &fmask);

  /* Adjust the stack by the frame size.  If the frame size is > 4096
     bytes, we need to be sure we probe somewhere in the first and last
     4096 bytes (we can probably get away without the latter test) and
     every 8192 bytes in between.  If the frame size is > 32768, we
     do this in a loop.  Otherwise, we generate the explicit probe
     instructions. 

     Note that we are only allowed to adjust sp once in the prologue.  */

  if (frame_size <= 32768)
    {
      if (frame_size > 4096)
	{
	  int probed = 4096;

	  do
	    emit_insn (gen_probe_stack (GEN_INT (-probed)));
	  while ((probed += 8192) < frame_size);

	  /* We only have to do this probe if we aren't saving registers.  */
	  if (sa_size == 0 && probed + 4096 < frame_size)
	    emit_insn (gen_probe_stack (GEN_INT (-frame_size)));
	}

      if (frame_size != 0)
	{
	  FRP (emit_insn (gen_adddi3 (stack_pointer_rtx, stack_pointer_rtx,
				      GEN_INT (-frame_size))));
	}
    }
  else
    {
      /* Here we generate code to set R22 to SP + 4096 and set R23 to the
	 number of 8192 byte blocks to probe.  We then probe each block
	 in the loop and then set SP to the proper location.  If the
	 amount remaining is > 4096, we have to do one more probe if we
	 are not saving any registers.  */

      HOST_WIDE_INT blocks = (frame_size + 4096) / 8192;
      HOST_WIDE_INT leftover = frame_size + 4096 - blocks * 8192;
      rtx ptr = gen_rtx_REG (DImode, 22);
      rtx count = gen_rtx_REG (DImode, 23);
      rtx seq;

      emit_move_insn (count, GEN_INT (blocks));
      emit_insn (gen_adddi3 (ptr, stack_pointer_rtx, GEN_INT (4096)));

      /* Because of the difficulty in emitting a new basic block this
	 late in the compilation, generate the loop as a single insn.  */
      emit_insn (gen_prologue_stack_probe_loop (count, ptr));

      if (leftover > 4096 && sa_size == 0)
	{
	  rtx last = gen_rtx_MEM (DImode, plus_constant (ptr, -leftover));
	  MEM_VOLATILE_P (last) = 1;
	  emit_move_insn (last, const0_rtx);
	}

      if (TARGET_WINDOWS_NT)
	{
	  /* For NT stack unwind (done by 'reverse execution'), it's
	     not OK to take the result of a loop, even though the value
	     is already in ptr, so we reload it via a single operation
	     and subtract it to sp. 

	     Yes, that's correct -- we have to reload the whole constant
	     into a temporary via ldah+lda then subtract from sp.  To
	     ensure we get ldah+lda, we use a special pattern.  */

	  HOST_WIDE_INT lo, hi;
	  lo = ((frame_size & 0xffff) ^ 0x8000) - 0x8000;
	  hi = frame_size - lo;

	  emit_move_insn (ptr, GEN_INT (hi));
	  emit_insn (gen_nt_lda (ptr, GEN_INT (lo)));
	  seq = emit_insn (gen_subdi3 (stack_pointer_rtx, stack_pointer_rtx,
				       ptr));
	}
      else
	{
	  seq = emit_insn (gen_adddi3 (stack_pointer_rtx, ptr,
				       GEN_INT (-leftover)));
	}

      /* This alternative is special, because the DWARF code cannot
         possibly intuit through the loop above.  So we invent this
         note it looks at instead.  */
      RTX_FRAME_RELATED_P (seq) = 1;
      REG_NOTES (seq)
        = gen_rtx_EXPR_LIST (REG_FRAME_RELATED_EXPR,
			     gen_rtx_SET (VOIDmode, stack_pointer_rtx,
			       gen_rtx_PLUS (Pmode, stack_pointer_rtx,
					     GEN_INT (-frame_size))),
			     REG_NOTES (seq));
    }

  /* Cope with very large offsets to the register save area.  */
  sa_reg = stack_pointer_rtx;
  if (reg_offset + sa_size > 0x8000)
    {
      int low = ((reg_offset & 0xffff) ^ 0x8000) - 0x8000;
      HOST_WIDE_INT bias;

      if (low + sa_size <= 0x8000)
	bias = reg_offset - low, reg_offset = low;
      else 
	bias = reg_offset, reg_offset = 0;

      sa_reg = gen_rtx_REG (DImode, 24);
      FRP (emit_insn (gen_adddi3 (sa_reg, stack_pointer_rtx, GEN_INT (bias))));
    }
    
  /* Save regs in stack order.  Beginning with VMS PV.  */
  if (TARGET_OPEN_VMS && vms_is_stack_procedure)
    {
      mem = gen_rtx_MEM (DImode, stack_pointer_rtx);
      MEM_ALIAS_SET (mem) = alpha_sr_alias_set;
      FRP (emit_move_insn (mem, gen_rtx_REG (DImode, REG_PV)));
    }

  /* Save register RA next.  */
  if (imask & (1L << REG_RA))
    {
      mem = gen_rtx_MEM (DImode, plus_constant (sa_reg, reg_offset));
      MEM_ALIAS_SET (mem) = alpha_sr_alias_set;
      FRP (emit_move_insn (mem, gen_rtx_REG (DImode, REG_RA)));
      imask &= ~(1L << REG_RA);
      reg_offset += 8;
    }

  /* Now save any other registers required to be saved.  */
  for (i = 0; i < 32; i++)
    if (imask & (1L << i))
      {
	mem = gen_rtx_MEM (DImode, plus_constant (sa_reg, reg_offset));
	MEM_ALIAS_SET (mem) = alpha_sr_alias_set;
	FRP (emit_move_insn (mem, gen_rtx_REG (DImode, i)));
	reg_offset += 8;
      }

  for (i = 0; i < 32; i++)
    if (fmask & (1L << i))
      {
	mem = gen_rtx_MEM (DFmode, plus_constant (sa_reg, reg_offset));
	MEM_ALIAS_SET (mem) = alpha_sr_alias_set;
	FRP (emit_move_insn (mem, gen_rtx_REG (DFmode, i+32)));
	reg_offset += 8;
      }

  if (TARGET_OPEN_VMS)
    {
      if (!vms_is_stack_procedure)
	{
	  /* Register frame procedures fave the fp.  */
	  FRP (emit_move_insn (gen_rtx_REG (DImode, vms_save_fp_regno),
			       hard_frame_pointer_rtx));
	}

      if (vms_base_regno != REG_PV)
	FRP (emit_move_insn (gen_rtx_REG (DImode, vms_base_regno),
			     gen_rtx_REG (DImode, REG_PV)));

      if (vms_unwind_regno == HARD_FRAME_POINTER_REGNUM)
	{
	  FRP (emit_move_insn (hard_frame_pointer_rtx, stack_pointer_rtx));
	}

      /* If we have to allocate space for outgoing args, do it now.  */
      if (current_function_outgoing_args_size != 0)
	{
	  FRP (emit_move_insn (stack_pointer_rtx, 
	        plus_constant (hard_frame_pointer_rtx,
	         - ALPHA_ROUND (current_function_outgoing_args_size))));
	}
    }
  else
    {
      /* If we need a frame pointer, set it from the stack pointer.  */
      if (frame_pointer_needed)
	{
	  if (TARGET_CAN_FAULT_IN_PROLOGUE)
	    FRP (emit_move_insn (hard_frame_pointer_rtx, stack_pointer_rtx));
	  else
	    {
	      /* This must always be the last instruction in the
		 prologue, thus we emit a special move + clobber.  */
	      FRP (emit_insn (gen_init_fp (hard_frame_pointer_rtx,
				           stack_pointer_rtx, sa_reg)));
	    }
	}
    }

  /* The ABIs for VMS and OSF/1 say that while we can schedule insns into
     the prologue, for exception handling reasons, we cannot do this for
     any insn that might fault.  We could prevent this for mems with a
     (clobber:BLK (scratch)), but this doesn't work for fp insns.  So we
     have to prevent all such scheduling with a blockage.

     Linux, on the other hand, never bothered to implement OSF/1's 
     exception handling, and so doesn't care about such things.  Anyone
     planning to use dwarf2 frame-unwind info can also omit the blockage.  */

  if (! TARGET_CAN_FAULT_IN_PROLOGUE)
    emit_insn (gen_blockage ());
}

/* Output the textual info surrounding the prologue.  */

void
alpha_start_function (file, fnname, decl)
     FILE *file;
     const char *fnname;
     tree decl ATTRIBUTE_UNUSED;
{
  unsigned long imask = 0;
  unsigned long fmask = 0;
  /* Stack space needed for pushing registers clobbered by us.  */
  HOST_WIDE_INT sa_size;
  /* Complete stack size needed.  */
  HOST_WIDE_INT frame_size;
  /* Offset from base reg to register save area.  */
  HOST_WIDE_INT reg_offset;
  char *entry_label = (char *) alloca (strlen (fnname) + 6);
  int i;

  sa_size = alpha_sa_size ();

  frame_size = get_frame_size ();
  if (TARGET_OPEN_VMS)
    frame_size = ALPHA_ROUND (sa_size 
			      + (vms_is_stack_procedure ? 8 : 0)
			      + frame_size
			      + current_function_pretend_args_size);
  else
    frame_size = (ALPHA_ROUND (current_function_outgoing_args_size)
		  + sa_size
		  + ALPHA_ROUND (frame_size
				 + current_function_pretend_args_size));

  if (TARGET_OPEN_VMS)
    reg_offset = 8;
  else
    reg_offset = ALPHA_ROUND (current_function_outgoing_args_size);

  alpha_sa_mask (&imask, &fmask);

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

  /* Issue function start and label.  */
  if (TARGET_OPEN_VMS || !flag_inhibit_size_directive)
    {
      fputs ("\t.ent ", file);
      assemble_name (file, fnname);
      putc ('\n', file);
    }

  strcpy (entry_label, fnname);
  if (TARGET_OPEN_VMS)
    strcat (entry_label, "..en");
  ASM_OUTPUT_LABEL (file, entry_label);
  inside_function = TRUE;

  if (TARGET_OPEN_VMS)
    fprintf (file, "\t.base $%d\n", vms_base_regno);

  if (!TARGET_OPEN_VMS && TARGET_IEEE_CONFORMANT
      && !flag_inhibit_size_directive)
    {
      /* Set flags in procedure descriptor to request IEEE-conformant
	 math-library routines.  The value we set it to is PDSC_EXC_IEEE
	 (/usr/include/pdsc.h). */
      fputs ("\t.eflag 48\n", file);
    }

  /* Set up offsets to alpha virtual arg/local debugging pointer.  */
  alpha_auto_offset = -frame_size + current_function_pretend_args_size;
  alpha_arg_offset = -frame_size + 48;

  /* Describe our frame.  If the frame size is larger than an integer,
     print it as zero to avoid an assembler error.  We won't be
     properly describing such a frame, but that's the best we can do.  */
  if (TARGET_OPEN_VMS)
    {
      fprintf (file, "\t.frame $%d,", vms_unwind_regno);
      fprintf (file, HOST_WIDE_INT_PRINT_DEC,
	       frame_size >= (1l << 31) ? 0 : frame_size);
      fputs (",$26,", file);
      fprintf (file, HOST_WIDE_INT_PRINT_DEC, reg_offset);
      fputs ("\n", file);
    }
  else if (!flag_inhibit_size_directive)
    {
      fprintf (file, "\t.frame $%d,",
	       (frame_pointer_needed
		? HARD_FRAME_POINTER_REGNUM : STACK_POINTER_REGNUM));
      fprintf (file, HOST_WIDE_INT_PRINT_DEC,
	       frame_size >= (1l << 31) ? 0 : frame_size);
      fprintf (file, ",$26,%d\n", current_function_pretend_args_size);
    }

  /* Describe which registers were spilled.  */
  if (TARGET_OPEN_VMS)
    {
      if (imask)
        /* ??? Does VMS care if mask contains ra?  The old code did'nt
           set it, so I don't here.  */
	fprintf (file, "\t.mask 0x%lx,0\n", imask & ~(1L << REG_RA));
      if (fmask)
	fprintf (file, "\t.fmask 0x%lx,0\n", fmask);
      if (!vms_is_stack_procedure)
	fprintf (file, "\t.fp_save $%d\n", vms_save_fp_regno);
    }
  else if (!flag_inhibit_size_directive)
    {
      if (imask)
	{
	  fprintf (file, "\t.mask 0x%lx,", imask);
	  fprintf (file, HOST_WIDE_INT_PRINT_DEC,
		   frame_size >= (1l << 31) ? 0 : reg_offset - frame_size);
	  putc ('\n', file);

	  for (i = 0; i < 32; ++i)
	    if (imask & (1L << i))
	      reg_offset += 8;
	}

      if (fmask)
	{
	  fprintf (file, "\t.fmask 0x%lx,", fmask);
	  fprintf (file, HOST_WIDE_INT_PRINT_DEC,
		   frame_size >= (1l << 31) ? 0 : reg_offset - frame_size);
	  putc ('\n', file);
	}
    }

  /* Emit GP related things.  It is rather unfortunate about the alignment
     issues surrounding a CODE_LABEL that forces us to do the label in 
     plain text.  */
  if (!TARGET_OPEN_VMS && !TARGET_WINDOWS_NT)
    {
      alpha_function_needs_gp = alpha_does_function_need_gp ();
      if (alpha_function_needs_gp)
	fputs ("\tldgp $29,0($27)\n", file);

      putc ('$', file);
      assemble_name (file, fnname);
      fputs ("..ng:\n", file);
    }

#ifdef OPEN_VMS
  /* Ifdef'ed cause readonly_section and link_section are only
     available then.  */
  readonly_section ();
  fprintf (file, "\t.align 3\n");
  assemble_name (file, fnname); fputs ("..na:\n", file);
  fputs ("\t.ascii \"", file);
  assemble_name (file, fnname);
  fputs ("\\0\"\n", file);
      
  link_section ();
  fprintf (file, "\t.align 3\n");
  fputs ("\t.name ", file);
  assemble_name (file, fnname);
  fputs ("..na\n", file);
  ASM_OUTPUT_LABEL (file, fnname);
  fprintf (file, "\t.pdesc ");
  assemble_name (file, fnname);
  fprintf (file, "..en,%s\n", vms_is_stack_procedure ? "stack" : "reg");
  alpha_need_linkage (fnname, 1);
  text_section ();
#endif
}

/* Emit the .prologue note at the scheduled end of the prologue.  */

void
output_end_prologue (file)
     FILE *file;
{
  if (TARGET_OPEN_VMS)
    fputs ("\t.prologue\n", file);
  else if (TARGET_WINDOWS_NT)
    fputs ("\t.prologue 0\n", file);
  else if (!flag_inhibit_size_directive)
    fprintf (file, "\t.prologue %d\n", alpha_function_needs_gp);
}

/* Write function epilogue.  */

/* ??? At some point we will want to support full unwind, and so will 
   need to mark the epilogue as well.  At the moment, we just confuse
   dwarf2out.  */
#undef FRP
#define FRP(exp) exp

void
alpha_expand_epilogue ()
{
  /* Registers to save.  */
  unsigned long imask = 0;
  unsigned long fmask = 0;
  /* Stack space needed for pushing registers clobbered by us.  */
  HOST_WIDE_INT sa_size;
  /* Complete stack size needed.  */
  HOST_WIDE_INT frame_size;
  /* Offset from base reg to register save area.  */
  HOST_WIDE_INT reg_offset;
  int fp_is_frame_pointer, fp_offset;
  rtx sa_reg, sa_reg_exp = NULL;
  rtx sp_adj1, sp_adj2, mem;
  rtx eh_ofs;
  int i;

  sa_size = alpha_sa_size ();

  frame_size = get_frame_size ();
  if (TARGET_OPEN_VMS)
    frame_size = ALPHA_ROUND (sa_size 
			      + (vms_is_stack_procedure ? 8 : 0)
			      + frame_size
			      + current_function_pretend_args_size);
  else
    frame_size = (ALPHA_ROUND (current_function_outgoing_args_size)
		  + sa_size
		  + ALPHA_ROUND (frame_size
				 + current_function_pretend_args_size));

  if (TARGET_OPEN_VMS)
    reg_offset = 8;
  else
    reg_offset = ALPHA_ROUND (current_function_outgoing_args_size);

  alpha_sa_mask (&imask, &fmask);

  fp_is_frame_pointer = ((TARGET_OPEN_VMS && vms_is_stack_procedure)
			 || (!TARGET_OPEN_VMS && frame_pointer_needed));

  eh_ofs = cfun->machine->eh_epilogue_sp_ofs;
  if (sa_size)
    {
      /* If we have a frame pointer, restore SP from it.  */
      if ((TARGET_OPEN_VMS
	   && vms_unwind_regno == HARD_FRAME_POINTER_REGNUM)
	  || (!TARGET_OPEN_VMS && frame_pointer_needed))
	{
	  FRP (emit_move_insn (stack_pointer_rtx, hard_frame_pointer_rtx));
	}

      /* Cope with very large offsets to the register save area.  */
      sa_reg = stack_pointer_rtx;
      if (reg_offset + sa_size > 0x8000)
	{
	  int low = ((reg_offset & 0xffff) ^ 0x8000) - 0x8000;
	  HOST_WIDE_INT bias;

	  if (low + sa_size <= 0x8000)
	    bias = reg_offset - low, reg_offset = low;
	  else 
	    bias = reg_offset, reg_offset = 0;

	  sa_reg = gen_rtx_REG (DImode, 22);
	  sa_reg_exp = plus_constant (stack_pointer_rtx, bias);

	  FRP (emit_move_insn (sa_reg, sa_reg_exp));
	}
	  
      /* Restore registers in order, excepting a true frame pointer. */

      if (! eh_ofs)
	{
	  mem = gen_rtx_MEM (DImode, plus_constant(sa_reg, reg_offset));
	  MEM_ALIAS_SET (mem) = alpha_sr_alias_set;
          FRP (emit_move_insn (gen_rtx_REG (DImode, REG_RA), mem));
	}
      reg_offset += 8;
      imask &= ~(1L << REG_RA);

      for (i = 0; i < 32; ++i)
	if (imask & (1L << i))
	  {
	    if (i == HARD_FRAME_POINTER_REGNUM && fp_is_frame_pointer)
	      fp_offset = reg_offset;
	    else
	      {
		mem = gen_rtx_MEM (DImode, plus_constant(sa_reg, reg_offset));
		MEM_ALIAS_SET (mem) = alpha_sr_alias_set;
		FRP (emit_move_insn (gen_rtx_REG (DImode, i), mem));
	      }
	    reg_offset += 8;
	  }

      for (i = 0; i < 32; ++i)
	if (fmask & (1L << i))
	  {
	    mem = gen_rtx_MEM (DFmode, plus_constant(sa_reg, reg_offset));
	    MEM_ALIAS_SET (mem) = alpha_sr_alias_set;
	    FRP (emit_move_insn (gen_rtx_REG (DFmode, i+32), mem));
	    reg_offset += 8;
	  }
    }

  if (frame_size || eh_ofs)
    {
      sp_adj1 = stack_pointer_rtx;

      if (eh_ofs)
	{
	  sp_adj1 = gen_rtx_REG (DImode, 23);
	  emit_move_insn (sp_adj1,
			  gen_rtx_PLUS (Pmode, stack_pointer_rtx, eh_ofs));
	}

      /* If the stack size is large, begin computation into a temporary
	 register so as not to interfere with a potential fp restore,
	 which must be consecutive with an SP restore.  */
      if (frame_size < 32768)
	sp_adj2 = GEN_INT (frame_size);
      else if (frame_size < 0x40007fffL)
	{
	  int low = ((frame_size & 0xffff) ^ 0x8000) - 0x8000;

	  sp_adj2 = plus_constant (sp_adj1, frame_size - low);
	  if (sa_reg_exp && rtx_equal_p (sa_reg_exp, sp_adj2))
	    sp_adj1 = sa_reg;
	  else
	    {
	      sp_adj1 = gen_rtx_REG (DImode, 23);
	      FRP (emit_move_insn (sp_adj1, sp_adj2));
	    }
	  sp_adj2 = GEN_INT (low);
	}
      else
	{
	  rtx tmp = gen_rtx_REG (DImode, 23);
	  FRP (sp_adj2 = alpha_emit_set_const (tmp, DImode, frame_size, 3));
	  if (!sp_adj2)
	    {
	      /* We can't drop new things to memory this late, afaik,
		 so build it up by pieces.  */
	      FRP (sp_adj2 = alpha_emit_set_long_const (tmp, frame_size,
							-(frame_size < 0)));
	      if (!sp_adj2)
		abort ();
	    }
	}

      /* From now on, things must be in order.  So emit blockages.  */

      /* Restore the frame pointer.  */
      if (fp_is_frame_pointer)
	{
	  emit_insn (gen_blockage ());
	  mem = gen_rtx_MEM (DImode, plus_constant(sa_reg, fp_offset));
	  MEM_ALIAS_SET (mem) = alpha_sr_alias_set;
	  FRP (emit_move_insn (hard_frame_pointer_rtx, mem));
	}
      else if (TARGET_OPEN_VMS)
	{
	  emit_insn (gen_blockage ());
	  FRP (emit_move_insn (hard_frame_pointer_rtx,
			       gen_rtx_REG (DImode, vms_save_fp_regno)));
	}

      /* Restore the stack pointer.  */
      emit_insn (gen_blockage ());
      FRP (emit_move_insn (stack_pointer_rtx,
		           gen_rtx_PLUS (DImode, sp_adj1, sp_adj2)));
    }
  else 
    {
      if (TARGET_OPEN_VMS && !vms_is_stack_procedure)
        {
          emit_insn (gen_blockage ());
          FRP (emit_move_insn (hard_frame_pointer_rtx,
			       gen_rtx_REG (DImode, vms_save_fp_regno)));
        }
    }
}

/* Output the rest of the textual info surrounding the epilogue.  */

void
alpha_end_function (file, fnname, decl)
     FILE *file;
     const char *fnname;
     tree decl ATTRIBUTE_UNUSED;
{
  /* End the function.  */
  if (!flag_inhibit_size_directive)
    {
      fputs ("\t.end ", file);
      assemble_name (file, fnname);
      putc ('\n', file);
    }
  inside_function = FALSE;

  /* Show that we know this function if it is called again. 

     Don't do this for global functions in object files destined for a
     shared library because the function may be overridden by the application
     or other libraries.  Similarly, don't do this for weak functions.  */

  if (!DECL_WEAK (current_function_decl)
      && (!flag_pic || !TREE_PUBLIC (current_function_decl)))
    SYMBOL_REF_FLAG (XEXP (DECL_RTL (current_function_decl), 0)) = 1;
}

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

static const char *current_function_file = "";

/* Offsets to alpha virtual arg/local debugging pointers.  */

long alpha_arg_offset;
long alpha_auto_offset;

/* Emit a new filename to a stream.  */

void
alpha_output_filename (stream, name)
     FILE *stream;
     const char *name;
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
    unsigned long i     : 31;	/* Mask of int regs */
    unsigned long fp    : 31;	/* Mask of fp regs */
    unsigned long mem   :  1;	/* mem == imem | fpmem */
  } used, defd;
};

static void summarize_insn PARAMS ((rtx, struct shadow_summary *, int));
static void alpha_handle_trap_shadows PARAMS ((rtx));

/* Summary the effects of expression X on the machine.  Update SUM, a pointer
   to the summary structure.  SET is nonzero if the insn is setting the
   object, otherwise zero.  */

static void
summarize_insn (x, sum, set)
     rtx x;
     struct shadow_summary *sum;
     int set;
{
  const char *format_ptr;
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

    case ASM_OPERANDS:
      for (i = ASM_OPERANDS_INPUT_LENGTH (x) - 1; i >= 0; i--)
	summarize_insn (ASM_OPERANDS_INPUT (x, i), sum, 0);
      break;

    case PARALLEL:
      for (i = XVECLEN (x, 0) - 1; i >= 0; i--)
	summarize_insn (XVECEXP (x, 0, i), sum, 0);
      break;

    case SUBREG:
      summarize_insn (SUBREG_REG (x), sum, 0);
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

	  case 'i':
	    break;

	  default:
	    abort ();
	  }
    }
}

/* Ensure a sufficient number of `trapb' insns are in the code when
   the user requests code with a trap precision of functions or
   instructions.

   In naive mode, when the user requests a trap-precision of
   "instruction", a trapb is needed after every instruction that may
   generate a trap.  This ensures that the code is resumption safe but
   it is also slow.

   When optimizations are turned on, we delay issuing a trapb as long
   as possible.  In this context, a trap shadow is the sequence of
   instructions that starts with a (potentially) trap generating
   instruction and extends to the next trapb or call_pal instruction
   (but GCC never generates call_pal by itself).  We can delay (and
   therefore sometimes omit) a trapb subject to the following
   conditions:

   (a) On entry to the trap shadow, if any Alpha register or memory
   location contains a value that is used as an operand value by some
   instruction in the trap shadow (live on entry), then no instruction
   in the trap shadow may modify the register or memory location.

   (b) Within the trap shadow, the computation of the base register
   for a memory load or store instruction may not involve using the
   result of an instruction that might generate an UNPREDICTABLE
   result.

   (c) Within the trap shadow, no register may be used more than once
   as a destination register.  (This is to make life easier for the
   trap-handler.)

   (d) The trap shadow may not include any branch instructions.  */

static void
alpha_handle_trap_shadows (insns)
     rtx insns;
{
  struct shadow_summary shadow;
  int trap_pending, exception_nesting;
  rtx i, n;

  trap_pending = 0;
  exception_nesting = 0;
  shadow.used.i = 0;
  shadow.used.fp = 0;
  shadow.used.mem = 0;
  shadow.defd = shadow.used;
  
  for (i = insns; i ; i = NEXT_INSN (i))
    {
      if (GET_CODE (i) == NOTE)
	{
	  switch (NOTE_LINE_NUMBER (i))
	    {
	    case NOTE_INSN_EH_REGION_BEG:
	      exception_nesting++;
	      if (trap_pending)
		goto close_shadow;
	      break;

	    case NOTE_INSN_EH_REGION_END:
	      exception_nesting--;
	      if (trap_pending)
		goto close_shadow;
	      break;

	    case NOTE_INSN_EPILOGUE_BEG:
	      if (trap_pending && alpha_tp >= ALPHA_TP_FUNC)
		goto close_shadow;
	      break;
	    }
	}
      else if (trap_pending)
	{
	  if (alpha_tp == ALPHA_TP_FUNC)
	    {
	      if (GET_CODE (i) == JUMP_INSN
		  && GET_CODE (PATTERN (i)) == RETURN)
		goto close_shadow;
	    }
	  else if (alpha_tp == ALPHA_TP_INSN)
	    {
	      if (optimize > 0)
		{
		  struct shadow_summary sum;

		  sum.used.i = 0;
		  sum.used.fp = 0;
		  sum.used.mem = 0;
		  sum.defd = sum.used;

		  switch (GET_CODE (i))
		    {
		    case INSN:
		      /* Annoyingly, get_attr_trap will abort on these.  */
		      if (GET_CODE (PATTERN (i)) == USE
			  || GET_CODE (PATTERN (i)) == CLOBBER)
			break;

		      summarize_insn (PATTERN (i), &sum, 0);

		      if ((sum.defd.i & shadow.defd.i)
			  || (sum.defd.fp & shadow.defd.fp))
			{
			  /* (c) would be violated */
			  goto close_shadow;
			}

		      /* Combine shadow with summary of current insn: */
		      shadow.used.i   |= sum.used.i;
		      shadow.used.fp  |= sum.used.fp;
		      shadow.used.mem |= sum.used.mem;
		      shadow.defd.i   |= sum.defd.i;
		      shadow.defd.fp  |= sum.defd.fp;
		      shadow.defd.mem |= sum.defd.mem;

		      if ((sum.defd.i & shadow.used.i)
			  || (sum.defd.fp & shadow.used.fp)
			  || (sum.defd.mem & shadow.used.mem))
			{
			  /* (a) would be violated (also takes care of (b))  */
			  if (get_attr_trap (i) == TRAP_YES
			      && ((sum.defd.i & sum.used.i)
				  || (sum.defd.fp & sum.used.fp)))
			    abort ();

			  goto close_shadow;
			}
		      break;

		    case JUMP_INSN:
		    case CALL_INSN:
		    case CODE_LABEL:
		      goto close_shadow;

		    default:
		      abort ();
		    }
		}
	      else
		{
		close_shadow:
		  n = emit_insn_before (gen_trapb (), i);
		  PUT_MODE (n, TImode);
		  PUT_MODE (i, TImode);
		  trap_pending = 0;
		  shadow.used.i = 0;
		  shadow.used.fp = 0;
		  shadow.used.mem = 0;
		  shadow.defd = shadow.used;
		}
	    }
	}

      if ((exception_nesting > 0 || alpha_tp >= ALPHA_TP_FUNC)
	  && GET_CODE (i) == INSN
	  && GET_CODE (PATTERN (i)) != USE
	  && GET_CODE (PATTERN (i)) != CLOBBER
	  && get_attr_trap (i) == TRAP_YES)
	{
	  if (optimize && !trap_pending)
	    summarize_insn (PATTERN (i), &shadow, 0);
	  trap_pending = 1;
	}
    }
}

#ifdef HAIFA
/* Alpha can only issue instruction groups simultaneously if they are
   suitibly aligned.  This is very processor-specific.  */

enum alphaev4_pipe {
  EV4_STOP = 0,
  EV4_IB0 = 1,
  EV4_IB1 = 2,
  EV4_IBX = 4
};

enum alphaev5_pipe {
  EV5_STOP = 0,
  EV5_NONE = 1,
  EV5_E01 = 2,
  EV5_E0 = 4,
  EV5_E1 = 8,
  EV5_FAM = 16,
  EV5_FA = 32,
  EV5_FM = 64
};

static enum alphaev4_pipe alphaev4_insn_pipe PARAMS ((rtx));
static enum alphaev5_pipe alphaev5_insn_pipe PARAMS ((rtx));
static rtx alphaev4_next_group PARAMS ((rtx, int*, int*));
static rtx alphaev5_next_group PARAMS ((rtx, int*, int*));
static rtx alphaev4_next_nop PARAMS ((int*));
static rtx alphaev5_next_nop PARAMS ((int*));

static void alpha_align_insns
  PARAMS ((rtx, int, rtx (*)(rtx, int*, int*), rtx (*)(int*), int));

static enum alphaev4_pipe
alphaev4_insn_pipe (insn)
     rtx insn;
{
  if (recog_memoized (insn) < 0)
    return EV4_STOP;
  if (get_attr_length (insn) != 4)
    return EV4_STOP;

  switch (get_attr_type (insn))
    {
    case TYPE_ILD:
    case TYPE_FLD:
      return EV4_IBX;

    case TYPE_LDSYM:
    case TYPE_IADD:
    case TYPE_ILOG:
    case TYPE_ICMOV:
    case TYPE_ICMP:
    case TYPE_IST:
    case TYPE_FST:
    case TYPE_SHIFT:
    case TYPE_IMUL:
    case TYPE_FBR:
      return EV4_IB0;

    case TYPE_MISC:
    case TYPE_IBR:
    case TYPE_JSR:
    case TYPE_FCPYS:
    case TYPE_FCMOV:
    case TYPE_FADD:
    case TYPE_FDIV:
    case TYPE_FMUL:
      return EV4_IB1;

    default:
      abort();
    }
}

static enum alphaev5_pipe
alphaev5_insn_pipe (insn)
     rtx insn;
{
  if (recog_memoized (insn) < 0)
    return EV5_STOP;
  if (get_attr_length (insn) != 4)
    return EV5_STOP;

  switch (get_attr_type (insn))
    {
    case TYPE_ILD:
    case TYPE_FLD:
    case TYPE_LDSYM:
    case TYPE_IADD:
    case TYPE_ILOG:
    case TYPE_ICMOV:
    case TYPE_ICMP:
      return EV5_E01;

    case TYPE_IST:
    case TYPE_FST:
    case TYPE_SHIFT:
    case TYPE_IMUL:
    case TYPE_MISC:
    case TYPE_MVI:
      return EV5_E0;

    case TYPE_IBR:
    case TYPE_JSR:
      return EV5_E1;

    case TYPE_FCPYS:
      return EV5_FAM;

    case TYPE_FBR:
    case TYPE_FCMOV:
    case TYPE_FADD:
    case TYPE_FDIV:
      return EV5_FA;

    case TYPE_FMUL:
      return EV5_FM;

    default:
      abort();
    }
}

/* IN_USE is a mask of the slots currently filled within the insn group. 
   The mask bits come from alphaev4_pipe above.  If EV4_IBX is set, then
   the insn in EV4_IB0 can be swapped by the hardware into EV4_IB1. 

   LEN is, of course, the length of the group in bytes.  */

static rtx
alphaev4_next_group (insn, pin_use, plen)
     rtx insn;
     int *pin_use, *plen;
{
  int len, in_use;

  len = in_use = 0;

  if (GET_RTX_CLASS (GET_CODE (insn)) != 'i'
      || GET_CODE (PATTERN (insn)) == CLOBBER
      || GET_CODE (PATTERN (insn)) == USE)
    goto next_and_done;

  while (1)
    {
      enum alphaev4_pipe pipe;

      pipe = alphaev4_insn_pipe (insn);
      switch (pipe)
	{
	case EV4_STOP:
	  /* Force complex instructions to start new groups.  */
	  if (in_use)
	    goto done;

	  /* If this is a completely unrecognized insn, its an asm.
	     We don't know how long it is, so record length as -1 to
	     signal a needed realignment.  */
	  if (recog_memoized (insn) < 0)
	    len = -1;
	  else
	    len = get_attr_length (insn);
	  goto next_and_done;

	case EV4_IBX:
	  if (in_use & EV4_IB0)
	    {
	      if (in_use & EV4_IB1)
		goto done;
	      in_use |= EV4_IB1;
	    }
	  else
	    in_use |= EV4_IB0 | EV4_IBX;
	  break;

	case EV4_IB0:
	  if (in_use & EV4_IB0)
	    {
	      if (!(in_use & EV4_IBX) || (in_use & EV4_IB1))
		goto done;
	      in_use |= EV4_IB1;
	    }
	  in_use |= EV4_IB0;
	  break;

	case EV4_IB1:
	  if (in_use & EV4_IB1)
	    goto done;
	  in_use |= EV4_IB1;
	  break;

	default:
	  abort();
	}
      len += 4;
      
      /* Haifa doesn't do well scheduling branches.  */
      if (GET_CODE (insn) == JUMP_INSN)
	goto next_and_done;

    next:
      insn = next_nonnote_insn (insn);

      if (!insn || GET_RTX_CLASS (GET_CODE (insn)) != 'i')
	goto done;

      /* Let Haifa tell us where it thinks insn group boundaries are.  */
      if (GET_MODE (insn) == TImode)
	goto done;

      if (GET_CODE (insn) == CLOBBER || GET_CODE (insn) == USE)
	goto next;
    }

 next_and_done:
  insn = next_nonnote_insn (insn);

 done:
  *plen = len;
  *pin_use = in_use;
  return insn;
}

/* IN_USE is a mask of the slots currently filled within the insn group. 
   The mask bits come from alphaev5_pipe above.  If EV5_E01 is set, then
   the insn in EV5_E0 can be swapped by the hardware into EV5_E1. 

   LEN is, of course, the length of the group in bytes.  */

static rtx
alphaev5_next_group (insn, pin_use, plen)
     rtx insn;
     int *pin_use, *plen;
{
  int len, in_use;

  len = in_use = 0;

  if (GET_RTX_CLASS (GET_CODE (insn)) != 'i'
      || GET_CODE (PATTERN (insn)) == CLOBBER
      || GET_CODE (PATTERN (insn)) == USE)
    goto next_and_done;

  while (1)
    {
      enum alphaev5_pipe pipe;

      pipe = alphaev5_insn_pipe (insn);
      switch (pipe)
	{
	case EV5_STOP:
	  /* Force complex instructions to start new groups.  */
	  if (in_use)
	    goto done;

	  /* If this is a completely unrecognized insn, its an asm.
	     We don't know how long it is, so record length as -1 to
	     signal a needed realignment.  */
	  if (recog_memoized (insn) < 0)
	    len = -1;
	  else
	    len = get_attr_length (insn);
	  goto next_and_done;

	/* ??? Most of the places below, we would like to abort, as 
	   it would indicate an error either in Haifa, or in the 
	   scheduling description.  Unfortunately, Haifa never 
	   schedules the last instruction of the BB, so we don't
	   have an accurate TI bit to go off.  */
	case EV5_E01:
	  if (in_use & EV5_E0)
	    {
	      if (in_use & EV5_E1)
		goto done;
	      in_use |= EV5_E1;
	    }
	  else
	    in_use |= EV5_E0 | EV5_E01;
	  break;

	case EV5_E0:
	  if (in_use & EV5_E0)
	    {
	      if (!(in_use & EV5_E01) || (in_use & EV5_E1))
		goto done;
	      in_use |= EV5_E1;
	    }
	  in_use |= EV5_E0;
	  break;

	case EV5_E1:
	  if (in_use & EV5_E1)
	    goto done;
	  in_use |= EV5_E1;
	  break;

	case EV5_FAM:
	  if (in_use & EV5_FA)
	    {
	      if (in_use & EV5_FM)
		goto done;
	      in_use |= EV5_FM;
	    }
	  else
	    in_use |= EV5_FA | EV5_FAM;
	  break;

	case EV5_FA:
	  if (in_use & EV5_FA)
	    goto done;
	  in_use |= EV5_FA;
	  break;

	case EV5_FM:
	  if (in_use & EV5_FM)
	    goto done;
	  in_use |= EV5_FM;
	  break;

	case EV5_NONE:
	  break;

	default:
	  abort();
	}
      len += 4;
      
      /* Haifa doesn't do well scheduling branches.  */
      /* ??? If this is predicted not-taken, slotting continues, except
	 that no more IBR, FBR, or JSR insns may be slotted.  */
      if (GET_CODE (insn) == JUMP_INSN)
	goto next_and_done;

    next:
      insn = next_nonnote_insn (insn);

      if (!insn || GET_RTX_CLASS (GET_CODE (insn)) != 'i')
	goto done;

      /* Let Haifa tell us where it thinks insn group boundaries are.  */
      if (GET_MODE (insn) == TImode)
	goto done;

      if (GET_CODE (insn) == CLOBBER || GET_CODE (insn) == USE)
	goto next;
    }

 next_and_done:
  insn = next_nonnote_insn (insn);

 done:
  *plen = len;
  *pin_use = in_use;
  return insn;
}

static rtx
alphaev4_next_nop (pin_use)
     int *pin_use;
{
  int in_use = *pin_use;
  rtx nop;

  if (!(in_use & EV4_IB0))
    {
      in_use |= EV4_IB0;
      nop = gen_nop ();
    }
  else if ((in_use & (EV4_IBX|EV4_IB1)) == EV4_IBX)
    {
      in_use |= EV4_IB1;
      nop = gen_nop ();
    }
  else if (TARGET_FP && !(in_use & EV4_IB1))
    {
      in_use |= EV4_IB1;
      nop = gen_fnop ();
    }
  else
    nop = gen_unop ();

  *pin_use = in_use;
  return nop;
}

static rtx
alphaev5_next_nop (pin_use)
     int *pin_use;
{
  int in_use = *pin_use;
  rtx nop;

  if (!(in_use & EV5_E1))
    {
      in_use |= EV5_E1;
      nop = gen_nop ();
    }
  else if (TARGET_FP && !(in_use & EV5_FA))
    {
      in_use |= EV5_FA;
      nop = gen_fnop ();
    }
  else if (TARGET_FP && !(in_use & EV5_FM))
    {
      in_use |= EV5_FM;
      nop = gen_fnop ();
    }
  else
    nop = gen_unop ();

  *pin_use = in_use;
  return nop;
}

/* The instruction group alignment main loop.  */

static void
alpha_align_insns (insns, max_align, next_group, next_nop, gp_in_use)
     rtx insns;
     int max_align;
     rtx (*next_group) PARAMS ((rtx, int*, int*));
     rtx (*next_nop) PARAMS ((int*));
     int gp_in_use;
{
  /* ALIGN is the known alignment for the insn group.  */
  int align;
  /* OFS is the offset of the current insn in the insn group.  */
  int ofs;
  int prev_in_use, in_use, len;
  rtx i, next;

  /* Let shorten branches care for assigning alignments to code labels.  */
  shorten_branches (insns);

  align = (FUNCTION_BOUNDARY/BITS_PER_UNIT < max_align
	   ? FUNCTION_BOUNDARY/BITS_PER_UNIT : max_align);

  /* Account for the initial GP load, which happens before the scheduled
     prologue we emitted as RTL.  */
  ofs = prev_in_use = 0;
  if (alpha_does_function_need_gp())
    {
      ofs = 8 & (align - 1);
      prev_in_use = gp_in_use;
    }

  i = insns;
  if (GET_CODE (i) == NOTE)
    i = next_nonnote_insn (i);

  while (i)
    {
      next = (*next_group)(i, &in_use, &len);

      /* When we see a label, resync alignment etc.  */
      if (GET_CODE (i) == CODE_LABEL)
	{
	  int new_align = 1 << label_to_alignment (i);
	  if (new_align >= align)
	    {
	      align = new_align < max_align ? new_align : max_align;
	      ofs = 0;
	    }
	  else if (ofs & (new_align-1))
	    ofs = (ofs | (new_align-1)) + 1;
	  if (len != 0)
	    abort();
	}

      /* Handle complex instructions special.  */
      else if (in_use == 0)
	{
	  /* Asms will have length < 0.  This is a signal that we have
	     lost alignment knowledge.  Assume, however, that the asm
	     will not mis-align instructions.  */
	  if (len < 0)
	    {
	      ofs = 0;
	      align = 4;
	      len = 0;
	    }
	}

      /* If the known alignment is smaller than the recognized insn group,
	 realign the output.  */
      else if (align < len)
	{
	  int new_log_align = len > 8 ? 4 : 3;
	  rtx where;

	  where = prev_nonnote_insn (i);
	  if (!where || GET_CODE (where) != CODE_LABEL)
	    where = i;

	  emit_insn_before (gen_realign (GEN_INT (new_log_align)), where);
	  align = 1 << new_log_align;
	  ofs = 0;
	}

      /* If the group won't fit in the same INT16 as the previous,
	 we need to add padding to keep the group together.  Rather
	 than simply leaving the insn filling to the assembler, we
	 can make use of the knowledge of what sorts of instructions
	 were issued in the previous group to make sure that all of
	 the added nops are really free.  */
      else if (ofs + len > align)
	{
	  int nop_count = (align - ofs) / 4;
	  rtx where;

	  /* Insert nops before labels and branches to truely merge the
	     execution of the nops with the previous instruction group.  */
	  where = prev_nonnote_insn (i);
	  if (where)
	    {
	      if (GET_CODE (where) == CODE_LABEL)
		{
		  rtx where2 = prev_nonnote_insn (where);
		  if (where2 && GET_CODE (where2) == JUMP_INSN)
		    where = where2;
		}
	      else if (GET_CODE (where) != JUMP_INSN)
		where = i;
	    }
	  else
	    where = i;

	  do 
	    emit_insn_before ((*next_nop)(&prev_in_use), where);
	  while (--nop_count);
	  ofs = 0;
	}

      ofs = (ofs + len) & (align - 1);
      prev_in_use = in_use;
      i = next;
    }
}
#endif /* HAIFA */

/* Machine dependant reorg pass.  */

void
alpha_reorg (insns)
     rtx insns;
{
  if (alpha_tp != ALPHA_TP_PROG || flag_exceptions)
    alpha_handle_trap_shadows (insns);

#ifdef HAIFA
  /* Due to the number of extra trapb insns, don't bother fixing up
     alignment when trap precision is instruction.  Moreover, we can
     only do our job when sched2 is run and Haifa is our scheduler.  */
  if (optimize && !optimize_size
      && alpha_tp != ALPHA_TP_INSN
      && flag_schedule_insns_after_reload)
    {
      if (alpha_cpu == PROCESSOR_EV4)
	alpha_align_insns (insns, 8, alphaev4_next_group,
			   alphaev4_next_nop, EV4_IB0);
      else if (alpha_cpu == PROCESSOR_EV5)
	alpha_align_insns (insns, 16, alphaev5_next_group,
			   alphaev5_next_nop, EV5_E01 | EV5_E0);
    }
#endif
}


/* Check a floating-point value for validity for a particular machine mode.  */

static const char * const float_strings[] =
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
     int overflow ATTRIBUTE_UNUSED;
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

rtx
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
    const char *name;
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

      fprintf (stream, "$%s..lk:\n", lptr->name);
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
     const char *name ATTRIBUTE_UNUSED;
     int is_local ATTRIBUTE_UNUSED;
{
}

#endif /* OPEN_VMS */
