/* Subroutines used for code generation on the DEC Alpha.
   Copyright (C) 1992-2014 Free Software Foundation, Inc.
   Contributed by Richard Kenner (kenner@vlsi1.ultra.nyu.edu)

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */


#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "tree.h"
#include "stor-layout.h"
#include "calls.h"
#include "varasm.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "insn-config.h"
#include "conditions.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "recog.h"
#include "expr.h"
#include "optabs.h"
#include "reload.h"
#include "obstack.h"
#include "except.h"
#include "function.h"
#include "diagnostic-core.h"
#include "ggc.h"
#include "tm_p.h"
#include "target.h"
#include "target-def.h"
#include "common/common-target.h"
#include "debug.h"
#include "langhooks.h"
#include "splay-tree.h"
#include "pointer-set.h"
#include "hash-table.h"
#include "vec.h"
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-fold.h"
#include "tree-eh.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "gimplify.h"
#include "gimple-ssa.h"
#include "stringpool.h"
#include "tree-ssanames.h"
#include "tree-stdarg.h"
#include "tm-constrs.h"
#include "df.h"
#include "libfuncs.h"
#include "opts.h"
#include "params.h"

/* Specify which cpu to schedule for.  */
enum processor_type alpha_tune;

/* Which cpu we're generating code for.  */
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

/* Nonzero if inside of a function, because the Alpha asm can't
   handle .files inside of functions.  */

static int inside_function = FALSE;

/* The number of cycles of latency we should assume on memory reads.  */

int alpha_memory_latency = 3;

/* Whether the function needs the GP.  */

static int alpha_function_needs_gp;

/* The assembler name of the current function.  */

static const char *alpha_fnname;

/* The next explicit relocation sequence number.  */
extern GTY(()) int alpha_next_sequence_number;
int alpha_next_sequence_number = 1;

/* The literal and gpdisp sequence numbers for this insn, as printed
   by %# and %* respectively.  */
extern GTY(()) int alpha_this_literal_sequence_number;
extern GTY(()) int alpha_this_gpdisp_sequence_number;
int alpha_this_literal_sequence_number;
int alpha_this_gpdisp_sequence_number;

/* Costs of various operations on the different architectures.  */

struct alpha_rtx_cost_data
{
  unsigned char fp_add;
  unsigned char fp_mult;
  unsigned char fp_div_sf;
  unsigned char fp_div_df;
  unsigned char int_mult_si;
  unsigned char int_mult_di;
  unsigned char int_shift;
  unsigned char int_cmov;
  unsigned short int_div;
};

static struct alpha_rtx_cost_data const alpha_rtx_cost_data[PROCESSOR_MAX] =
{
  { /* EV4 */
    COSTS_N_INSNS (6),		/* fp_add */
    COSTS_N_INSNS (6),		/* fp_mult */
    COSTS_N_INSNS (34),		/* fp_div_sf */
    COSTS_N_INSNS (63),		/* fp_div_df */
    COSTS_N_INSNS (23),		/* int_mult_si */
    COSTS_N_INSNS (23),		/* int_mult_di */
    COSTS_N_INSNS (2),		/* int_shift */
    COSTS_N_INSNS (2),		/* int_cmov */
    COSTS_N_INSNS (97),		/* int_div */
  },
  { /* EV5 */
    COSTS_N_INSNS (4),		/* fp_add */
    COSTS_N_INSNS (4),		/* fp_mult */
    COSTS_N_INSNS (15),		/* fp_div_sf */
    COSTS_N_INSNS (22),		/* fp_div_df */
    COSTS_N_INSNS (8),		/* int_mult_si */
    COSTS_N_INSNS (12),		/* int_mult_di */
    COSTS_N_INSNS (1) + 1,	/* int_shift */
    COSTS_N_INSNS (1),		/* int_cmov */
    COSTS_N_INSNS (83),		/* int_div */
  },
  { /* EV6 */
    COSTS_N_INSNS (4),		/* fp_add */
    COSTS_N_INSNS (4),		/* fp_mult */
    COSTS_N_INSNS (12),		/* fp_div_sf */
    COSTS_N_INSNS (15),		/* fp_div_df */
    COSTS_N_INSNS (7),		/* int_mult_si */
    COSTS_N_INSNS (7),		/* int_mult_di */
    COSTS_N_INSNS (1),		/* int_shift */
    COSTS_N_INSNS (2),		/* int_cmov */
    COSTS_N_INSNS (86),		/* int_div */
  },
};

/* Similar but tuned for code size instead of execution latency.  The
   extra +N is fractional cost tuning based on latency.  It's used to
   encourage use of cheaper insns like shift, but only if there's just
   one of them.  */

static struct alpha_rtx_cost_data const alpha_rtx_cost_size =
{
  COSTS_N_INSNS (1),		/* fp_add */
  COSTS_N_INSNS (1),		/* fp_mult */
  COSTS_N_INSNS (1),		/* fp_div_sf */
  COSTS_N_INSNS (1) + 1,	/* fp_div_df */
  COSTS_N_INSNS (1) + 1,	/* int_mult_si */
  COSTS_N_INSNS (1) + 2,	/* int_mult_di */
  COSTS_N_INSNS (1),		/* int_shift */
  COSTS_N_INSNS (1),		/* int_cmov */
  COSTS_N_INSNS (6),		/* int_div */
};

/* Get the number of args of a function in one of two ways.  */
#if TARGET_ABI_OPEN_VMS
#define NUM_ARGS crtl->args.info.num_args
#else
#define NUM_ARGS crtl->args.info
#endif

#define REG_PV 27
#define REG_RA 26

/* Declarations of static functions.  */
static struct machine_function *alpha_init_machine_status (void);
static rtx alpha_emit_xfloating_compare (enum rtx_code *, rtx, rtx);

#if TARGET_ABI_OPEN_VMS
static void alpha_write_linkage (FILE *, const char *);
static bool vms_valid_pointer_mode (enum machine_mode);
#else
#define vms_patch_builtins()  gcc_unreachable()
#endif

#ifdef TARGET_ALTERNATE_LONG_DOUBLE_MANGLING
/* Implement TARGET_MANGLE_TYPE.  */

static const char *
alpha_mangle_type (const_tree type)
{
  if (TYPE_MAIN_VARIANT (type) == long_double_type_node
      && TARGET_LONG_DOUBLE_128)
    return "g";

  /* For all other types, use normal C++ mangling.  */
  return NULL;
}
#endif

/* Parse target option strings.  */

static void
alpha_option_override (void)
{
  static const struct cpu_table {
    const char *const name;
    const enum processor_type processor;
    const int flags;
    const unsigned short line_size; /* in bytes */
    const unsigned short l1_size;   /* in kb.  */
    const unsigned short l2_size;   /* in kb.  */
  } cpu_table[] = {
    /* EV4/LCA45 had 8k L1 caches; EV45 had 16k L1 caches.
       EV4/EV45 had 128k to 16M 32-byte direct Bcache.  LCA45
       had 64k to 8M 8-byte direct Bcache.  */
    { "ev4",	PROCESSOR_EV4, 0, 32, 8, 8*1024 },
    { "21064",	PROCESSOR_EV4, 0, 32, 8, 8*1024 },
    { "ev45",	PROCESSOR_EV4, 0, 32, 16, 16*1024 },

    /* EV5 or EV56 had 8k 32 byte L1, 96k 32 or 64 byte L2,
       and 1M to 16M 64 byte L3 (not modeled).
       PCA56 had 16k 64-byte cache; PCA57 had 32k Icache.
       PCA56 had 8k 64-byte cache; PCA57 had 16k Dcache.  */
    { "ev5",	PROCESSOR_EV5, 0, 32, 8, 96 },
    { "21164",	PROCESSOR_EV5, 0, 32, 8, 96 },
    { "ev56",	PROCESSOR_EV5, MASK_BWX, 32, 8, 96 },
    { "21164a",	PROCESSOR_EV5, MASK_BWX, 32, 8, 96 },
    { "pca56",	PROCESSOR_EV5, MASK_BWX|MASK_MAX, 64, 16, 4*1024 },
    { "21164PC",PROCESSOR_EV5, MASK_BWX|MASK_MAX, 64, 16, 4*1024 },
    { "21164pc",PROCESSOR_EV5, MASK_BWX|MASK_MAX, 64, 16, 4*1024 },

    /* EV6 had 64k 64 byte L1, 1M to 16M Bcache.  */
    { "ev6",	PROCESSOR_EV6, MASK_BWX|MASK_MAX|MASK_FIX, 64, 64, 16*1024 },
    { "21264",	PROCESSOR_EV6, MASK_BWX|MASK_MAX|MASK_FIX, 64, 64, 16*1024 },
    { "ev67",	PROCESSOR_EV6, MASK_BWX|MASK_MAX|MASK_FIX|MASK_CIX,
      64, 64, 16*1024 },
    { "21264a",	PROCESSOR_EV6, MASK_BWX|MASK_MAX|MASK_FIX|MASK_CIX,
      64, 64, 16*1024 }
  };

  int const ct_size = ARRAY_SIZE (cpu_table);
  int line_size = 0, l1_size = 0, l2_size = 0;
  int i;

#ifdef SUBTARGET_OVERRIDE_OPTIONS
  SUBTARGET_OVERRIDE_OPTIONS;
#endif

  /* Default to full IEEE compliance mode for Go language.  */
  if (strcmp (lang_hooks.name, "GNU Go") == 0
      && !(target_flags_explicit & MASK_IEEE))
    target_flags |= MASK_IEEE;

  alpha_fprm = ALPHA_FPRM_NORM;
  alpha_tp = ALPHA_TP_PROG;
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
	error ("bad value %qs for -mtrap-precision switch", alpha_tp_string);
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
	error ("bad value %qs for -mfp-rounding-mode switch",
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
	error ("bad value %qs for -mfp-trap-mode switch", alpha_fptm_string);
    }

  if (alpha_cpu_string)
    {
      for (i = 0; i < ct_size; i++)
	if (! strcmp (alpha_cpu_string, cpu_table [i].name))
	  {
	    alpha_tune = alpha_cpu = cpu_table[i].processor;
	    line_size = cpu_table[i].line_size;
	    l1_size = cpu_table[i].l1_size;
	    l2_size = cpu_table[i].l2_size;
	    target_flags &= ~ (MASK_BWX | MASK_MAX | MASK_FIX | MASK_CIX);
	    target_flags |= cpu_table[i].flags;
	    break;
	  }
      if (i == ct_size)
	error ("bad value %qs for -mcpu switch", alpha_cpu_string);
    }

  if (alpha_tune_string)
    {
      for (i = 0; i < ct_size; i++)
	if (! strcmp (alpha_tune_string, cpu_table [i].name))
	  {
	    alpha_tune = cpu_table[i].processor;
	    line_size = cpu_table[i].line_size;
	    l1_size = cpu_table[i].l1_size;
	    l2_size = cpu_table[i].l2_size;
	    break;
	  }
      if (i == ct_size)
	error ("bad value %qs for -mtune switch", alpha_tune_string);
    }

  if (line_size)
    maybe_set_param_value (PARAM_L1_CACHE_LINE_SIZE, line_size,
			   global_options.x_param_values,
			   global_options_set.x_param_values);
  if (l1_size)
    maybe_set_param_value (PARAM_L1_CACHE_SIZE, l1_size,
			   global_options.x_param_values,
			   global_options_set.x_param_values);
  if (l2_size)
    maybe_set_param_value (PARAM_L2_CACHE_SIZE, l2_size,
			   global_options.x_param_values,
			   global_options_set.x_param_values);

  /* Do some sanity checks on the above options.  */

  if ((alpha_fptm == ALPHA_FPTM_SU || alpha_fptm == ALPHA_FPTM_SUI)
      && alpha_tp != ALPHA_TP_INSN && alpha_cpu != PROCESSOR_EV6)
    {
      warning (0, "fp software completion requires -mtrap-precision=i");
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
	  warning (0, "rounding mode not supported for VAX floats");
	  alpha_fprm = ALPHA_FPRM_NORM;
	}
      if (alpha_fptm == ALPHA_FPTM_SUI)
	{
	  warning (0, "trap mode not supported for VAX floats");
	  alpha_fptm = ALPHA_FPTM_SU;
	}
      if (target_flags_explicit & MASK_LONG_DOUBLE_128)
	warning (0, "128-bit long double not supported for VAX floats");
      target_flags &= ~MASK_LONG_DOUBLE_128;
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
	  { 3, 12, 30 },	/* ev6 -- Bcache from DS20 LMbench.  */
	};

	lat = alpha_mlat_string[1] - '0';
	if (lat <= 0 || lat > 3 || cache_latency[alpha_tune][lat-1] == -1)
	  {
	    warning (0, "L%d cache latency unknown for %s",
		     lat, alpha_cpu_name[alpha_tune]);
	    lat = 3;
	  }
	else
	  lat = cache_latency[alpha_tune][lat-1];
      }
    else if (! strcmp (alpha_mlat_string, "main"))
      {
	/* Most current memories have about 370ns latency.  This is
	   a reasonable guess for a fast cpu.  */
	lat = 150;
      }
    else
      {
	warning (0, "bad value %qs for -mmemory-latency", alpha_mlat_string);
	lat = 3;
      }

    alpha_memory_latency = lat;
  }

  /* Default the definition of "small data" to 8 bytes.  */
  if (!global_options_set.x_g_switch_value)
    g_switch_value = 8;

  /* Infer TARGET_SMALL_DATA from -fpic/-fPIC.  */
  if (flag_pic == 1)
    target_flags |= MASK_SMALL_DATA;
  else if (flag_pic == 2)
    target_flags &= ~MASK_SMALL_DATA;

  /* Align labels and loops for optimal branching.  */
  /* ??? Kludge these by not doing anything if we don't optimize.  */
  if (optimize > 0)
    {
      if (align_loops <= 0)
	align_loops = 16;
      if (align_jumps <= 0)
	align_jumps = 16;
    }
  if (align_functions <= 0)
    align_functions = 16;

  /* Register variables and functions with the garbage collector.  */

  /* Set up function hooks.  */
  init_machine_status = alpha_init_machine_status;

  /* Tell the compiler when we're using VAX floating point.  */
  if (TARGET_FLOAT_VAX)
    {
      REAL_MODE_FORMAT (SFmode) = &vax_f_format;
      REAL_MODE_FORMAT (DFmode) = &vax_g_format;
      REAL_MODE_FORMAT (TFmode) = NULL;
    }

#ifdef TARGET_DEFAULT_LONG_DOUBLE_128
  if (!(target_flags_explicit & MASK_LONG_DOUBLE_128))
    target_flags |= MASK_LONG_DOUBLE_128;
#endif
}

/* Returns 1 if VALUE is a mask that contains full bytes of zero or ones.  */

int
zap_mask (HOST_WIDE_INT value)
{
  int i;

  for (i = 0; i < HOST_BITS_PER_WIDE_INT / HOST_BITS_PER_CHAR;
       i++, value >>= 8)
    if ((value & 0xff) != 0 && (value & 0xff) != 0xff)
      return 0;

  return 1;
}

/* Return true if OP is valid for a particular TLS relocation.
   We are already guaranteed that OP is a CONST.  */

int
tls_symbolic_operand_1 (rtx op, int size, int unspec)
{
  op = XEXP (op, 0);

  if (GET_CODE (op) != UNSPEC || XINT (op, 1) != unspec)
    return 0;
  op = XVECEXP (op, 0, 0);

  if (GET_CODE (op) != SYMBOL_REF)
    return 0;

  switch (SYMBOL_REF_TLS_MODEL (op))
    {
    case TLS_MODEL_LOCAL_DYNAMIC:
      return unspec == UNSPEC_DTPREL && size == alpha_tls_size;
    case TLS_MODEL_INITIAL_EXEC:
      return unspec == UNSPEC_TPREL && size == 64;
    case TLS_MODEL_LOCAL_EXEC:
      return unspec == UNSPEC_TPREL && size == alpha_tls_size;
    default:
      gcc_unreachable ();
    }
}

/* Used by aligned_memory_operand and unaligned_memory_operand to
   resolve what reload is going to do with OP if it's a register.  */

rtx
resolve_reload_operand (rtx op)
{
  if (reload_in_progress)
    {
      rtx tmp = op;
      if (GET_CODE (tmp) == SUBREG)
	tmp = SUBREG_REG (tmp);
      if (REG_P (tmp)
	  && REGNO (tmp) >= FIRST_PSEUDO_REGISTER)
	{
	  op = reg_equiv_memory_loc (REGNO (tmp));
	  if (op == 0)
	    return 0;
	}
    }
  return op;
}

/* The scalar modes supported differs from the default check-what-c-supports
   version in that sometimes TFmode is available even when long double
   indicates only DFmode.  */

static bool
alpha_scalar_mode_supported_p (enum machine_mode mode)
{
  switch (mode)
    {
    case QImode:
    case HImode:
    case SImode:
    case DImode:
    case TImode: /* via optabs.c */
      return true;

    case SFmode:
    case DFmode:
      return true;

    case TFmode:
      return TARGET_HAS_XFLOATING_LIBS;

    default:
      return false;
    }
}

/* Alpha implements a couple of integer vector mode operations when
   TARGET_MAX is enabled.  We do not check TARGET_MAX here, however,
   which allows the vectorizer to operate on e.g. move instructions,
   or when expand_vector_operations can do something useful.  */

static bool
alpha_vector_mode_supported_p (enum machine_mode mode)
{
  return mode == V8QImode || mode == V4HImode || mode == V2SImode;
}

/* Return 1 if this function can directly return via $26.  */

int
direct_return (void)
{
  return (TARGET_ABI_OSF
	  && reload_completed
	  && alpha_sa_size () == 0
	  && get_frame_size () == 0
	  && crtl->outgoing_args_size == 0
	  && crtl->args.pretend_args_size == 0);
}

/* Return the TLS model to use for SYMBOL.  */

static enum tls_model
tls_symbolic_operand_type (rtx symbol)
{
  enum tls_model model;

  if (GET_CODE (symbol) != SYMBOL_REF)
    return TLS_MODEL_NONE;
  model = SYMBOL_REF_TLS_MODEL (symbol);

  /* Local-exec with a 64-bit size is the same code as initial-exec.  */
  if (model == TLS_MODEL_LOCAL_EXEC && alpha_tls_size == 64)
    model = TLS_MODEL_INITIAL_EXEC;

  return model;
}

/* Return true if the function DECL will share the same GP as any
   function in the current unit of translation.  */

static bool
decl_has_samegp (const_tree decl)
{
  /* Functions that are not local can be overridden, and thus may
     not share the same gp.  */
  if (!(*targetm.binds_local_p) (decl))
    return false;

  /* If -msmall-data is in effect, assume that there is only one GP
     for the module, and so any local symbol has this property.  We
     need explicit relocations to be able to enforce this for symbols
     not defined in this unit of translation, however.  */
  if (TARGET_EXPLICIT_RELOCS && TARGET_SMALL_DATA)
    return true;

  /* Functions that are not external are defined in this UoT.  */
  /* ??? Irritatingly, static functions not yet emitted are still
     marked "external".  Apply this to non-static functions only.  */
  return !TREE_PUBLIC (decl) || !DECL_EXTERNAL (decl);
}

/* Return true if EXP should be placed in the small data section.  */

static bool
alpha_in_small_data_p (const_tree exp)
{
  /* We want to merge strings, so we never consider them small data.  */
  if (TREE_CODE (exp) == STRING_CST)
    return false;

  /* Functions are never in the small data area.  Duh.  */
  if (TREE_CODE (exp) == FUNCTION_DECL)
    return false;

  if (TREE_CODE (exp) == VAR_DECL && DECL_SECTION_NAME (exp))
    {
      const char *section = TREE_STRING_POINTER (DECL_SECTION_NAME (exp));
      if (strcmp (section, ".sdata") == 0
	  || strcmp (section, ".sbss") == 0)
	return true;
    }
  else
    {
      HOST_WIDE_INT size = int_size_in_bytes (TREE_TYPE (exp));

      /* If this is an incomplete type with size 0, then we can't put it
	 in sdata because it might be too big when completed.  */
      if (size > 0 && size <= g_switch_value)
	return true;
    }

  return false;
}

#if TARGET_ABI_OPEN_VMS
static bool
vms_valid_pointer_mode (enum machine_mode mode)
{
  return (mode == SImode || mode == DImode);
}

static bool
alpha_linkage_symbol_p (const char *symname)
{
  int symlen = strlen (symname);

  if (symlen > 4)
    return strcmp (&symname [symlen - 4], "..lk") == 0;

  return false;
}

#define LINKAGE_SYMBOL_REF_P(X) \
  ((GET_CODE (X) == SYMBOL_REF   \
    && alpha_linkage_symbol_p (XSTR (X, 0))) \
   || (GET_CODE (X) == CONST                 \
       && GET_CODE (XEXP (X, 0)) == PLUS     \
       && GET_CODE (XEXP (XEXP (X, 0), 0)) == SYMBOL_REF \
       && alpha_linkage_symbol_p (XSTR (XEXP (XEXP (X, 0), 0), 0))))
#endif

/* legitimate_address_p recognizes an RTL expression that is a valid
   memory address for an instruction.  The MODE argument is the
   machine mode for the MEM expression that wants to use this address.

   For Alpha, we have either a constant address or the sum of a
   register and a constant address, or just a register.  For DImode,
   any of those forms can be surrounded with an AND that clear the
   low-order three bits; this is an "unaligned" access.  */

static bool
alpha_legitimate_address_p (enum machine_mode mode, rtx x, bool strict)
{
  /* If this is an ldq_u type address, discard the outer AND.  */
  if (mode == DImode
      && GET_CODE (x) == AND
      && CONST_INT_P (XEXP (x, 1))
      && INTVAL (XEXP (x, 1)) == -8)
    x = XEXP (x, 0);

  /* Discard non-paradoxical subregs.  */
  if (GET_CODE (x) == SUBREG
      && (GET_MODE_SIZE (GET_MODE (x))
	  < GET_MODE_SIZE (GET_MODE (SUBREG_REG (x)))))
    x = SUBREG_REG (x);

  /* Unadorned general registers are valid.  */
  if (REG_P (x)
      && (strict
	  ? STRICT_REG_OK_FOR_BASE_P (x)
	  : NONSTRICT_REG_OK_FOR_BASE_P (x)))
    return true;

  /* Constant addresses (i.e. +/- 32k) are valid.  */
  if (CONSTANT_ADDRESS_P (x))
    return true;

#if TARGET_ABI_OPEN_VMS
  if (LINKAGE_SYMBOL_REF_P (x))
    return true;
#endif

  /* Register plus a small constant offset is valid.  */
  if (GET_CODE (x) == PLUS)
    {
      rtx ofs = XEXP (x, 1);
      x = XEXP (x, 0);

      /* Discard non-paradoxical subregs.  */
      if (GET_CODE (x) == SUBREG
          && (GET_MODE_SIZE (GET_MODE (x))
	      < GET_MODE_SIZE (GET_MODE (SUBREG_REG (x)))))
	x = SUBREG_REG (x);

      if (REG_P (x))
	{
	  if (! strict
	      && NONSTRICT_REG_OK_FP_BASE_P (x)
	      && CONST_INT_P (ofs))
	    return true;
	  if ((strict
	       ? STRICT_REG_OK_FOR_BASE_P (x)
	       : NONSTRICT_REG_OK_FOR_BASE_P (x))
	      && CONSTANT_ADDRESS_P (ofs))
	    return true;
	}
    }

  /* If we're managing explicit relocations, LO_SUM is valid, as are small
     data symbols.  Avoid explicit relocations of modes larger than word
     mode since i.e. $LC0+8($1) can fold around +/- 32k offset.  */
  else if (TARGET_EXPLICIT_RELOCS
	   && GET_MODE_SIZE (mode) <= UNITS_PER_WORD)
    {
      if (small_symbolic_operand (x, Pmode))
	return true;

      if (GET_CODE (x) == LO_SUM)
	{
	  rtx ofs = XEXP (x, 1);
	  x = XEXP (x, 0);

	  /* Discard non-paradoxical subregs.  */
	  if (GET_CODE (x) == SUBREG
	      && (GET_MODE_SIZE (GET_MODE (x))
		  < GET_MODE_SIZE (GET_MODE (SUBREG_REG (x)))))
	    x = SUBREG_REG (x);

	  /* Must have a valid base register.  */
	  if (! (REG_P (x)
		 && (strict
		     ? STRICT_REG_OK_FOR_BASE_P (x)
		     : NONSTRICT_REG_OK_FOR_BASE_P (x))))
	    return false;

	  /* The symbol must be local.  */
	  if (local_symbolic_operand (ofs, Pmode)
	      || dtp32_symbolic_operand (ofs, Pmode)
	      || tp32_symbolic_operand (ofs, Pmode))
	    return true;
	}
    }

  return false;
}

/* Build the SYMBOL_REF for __tls_get_addr.  */

static GTY(()) rtx tls_get_addr_libfunc;

static rtx
get_tls_get_addr (void)
{
  if (!tls_get_addr_libfunc)
    tls_get_addr_libfunc = init_one_libfunc ("__tls_get_addr");
  return tls_get_addr_libfunc;
}

/* Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new, valid address.  */

static rtx
alpha_legitimize_address_1 (rtx x, rtx scratch, enum machine_mode mode)
{
  HOST_WIDE_INT addend;

  /* If the address is (plus reg const_int) and the CONST_INT is not a
     valid offset, compute the high part of the constant and add it to
     the register.  Then our address is (plus temp low-part-const).  */
  if (GET_CODE (x) == PLUS
      && REG_P (XEXP (x, 0))
      && CONST_INT_P (XEXP (x, 1))
      && ! CONSTANT_ADDRESS_P (XEXP (x, 1)))
    {
      addend = INTVAL (XEXP (x, 1));
      x = XEXP (x, 0);
      goto split_addend;
    }

  /* If the address is (const (plus FOO const_int)), find the low-order
     part of the CONST_INT.  Then load FOO plus any high-order part of the
     CONST_INT into a register.  Our address is (plus reg low-part-const).
     This is done to reduce the number of GOT entries.  */
  if (can_create_pseudo_p ()
      && GET_CODE (x) == CONST
      && GET_CODE (XEXP (x, 0)) == PLUS
      && CONST_INT_P (XEXP (XEXP (x, 0), 1)))
    {
      addend = INTVAL (XEXP (XEXP (x, 0), 1));
      x = force_reg (Pmode, XEXP (XEXP (x, 0), 0));
      goto split_addend;
    }

  /* If we have a (plus reg const), emit the load as in (2), then add
     the two registers, and finally generate (plus reg low-part-const) as
     our address.  */
  if (can_create_pseudo_p ()
      && GET_CODE (x) == PLUS
      && REG_P (XEXP (x, 0))
      && GET_CODE (XEXP (x, 1)) == CONST
      && GET_CODE (XEXP (XEXP (x, 1), 0)) == PLUS
      && CONST_INT_P (XEXP (XEXP (XEXP (x, 1), 0), 1)))
    {
      addend = INTVAL (XEXP (XEXP (XEXP (x, 1), 0), 1));
      x = expand_simple_binop (Pmode, PLUS, XEXP (x, 0),
			       XEXP (XEXP (XEXP (x, 1), 0), 0),
			       NULL_RTX, 1, OPTAB_LIB_WIDEN);
      goto split_addend;
    }

  /* If this is a local symbol, split the address into HIGH/LO_SUM parts.
     Avoid modes larger than word mode since i.e. $LC0+8($1) can fold
     around +/- 32k offset.  */
  if (TARGET_EXPLICIT_RELOCS
      && GET_MODE_SIZE (mode) <= UNITS_PER_WORD
      && symbolic_operand (x, Pmode))
    {
      rtx r0, r16, eqv, tga, tp, insn, dest, seq;

      switch (tls_symbolic_operand_type (x))
	{
	case TLS_MODEL_NONE:
	  break;

	case TLS_MODEL_GLOBAL_DYNAMIC:
	  start_sequence ();

	  r0 = gen_rtx_REG (Pmode, 0);
	  r16 = gen_rtx_REG (Pmode, 16);
	  tga = get_tls_get_addr ();
	  dest = gen_reg_rtx (Pmode);
	  seq = GEN_INT (alpha_next_sequence_number++);

	  emit_insn (gen_movdi_er_tlsgd (r16, pic_offset_table_rtx, x, seq));
	  insn = gen_call_value_osf_tlsgd (r0, tga, seq);
	  insn = emit_call_insn (insn);
	  RTL_CONST_CALL_P (insn) = 1;
	  use_reg (&CALL_INSN_FUNCTION_USAGE (insn), r16);

          insn = get_insns ();
	  end_sequence ();

	  emit_libcall_block (insn, dest, r0, x);
	  return dest;

	case TLS_MODEL_LOCAL_DYNAMIC:
	  start_sequence ();

	  r0 = gen_rtx_REG (Pmode, 0);
	  r16 = gen_rtx_REG (Pmode, 16);
	  tga = get_tls_get_addr ();
	  scratch = gen_reg_rtx (Pmode);
	  seq = GEN_INT (alpha_next_sequence_number++);

	  emit_insn (gen_movdi_er_tlsldm (r16, pic_offset_table_rtx, seq));
	  insn = gen_call_value_osf_tlsldm (r0, tga, seq);
	  insn = emit_call_insn (insn);
	  RTL_CONST_CALL_P (insn) = 1;
	  use_reg (&CALL_INSN_FUNCTION_USAGE (insn), r16);

          insn = get_insns ();
	  end_sequence ();

	  eqv = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, const0_rtx),
				UNSPEC_TLSLDM_CALL);
	  emit_libcall_block (insn, scratch, r0, eqv);

	  eqv = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, x), UNSPEC_DTPREL);
	  eqv = gen_rtx_CONST (Pmode, eqv);

	  if (alpha_tls_size == 64)
	    {
	      dest = gen_reg_rtx (Pmode);
	      emit_insn (gen_rtx_SET (VOIDmode, dest, eqv));
	      emit_insn (gen_adddi3 (dest, dest, scratch));
	      return dest;
	    }
	  if (alpha_tls_size == 32)
	    {
	      insn = gen_rtx_HIGH (Pmode, eqv);
	      insn = gen_rtx_PLUS (Pmode, scratch, insn);
	      scratch = gen_reg_rtx (Pmode);
	      emit_insn (gen_rtx_SET (VOIDmode, scratch, insn));
	    }
	  return gen_rtx_LO_SUM (Pmode, scratch, eqv);

	case TLS_MODEL_INITIAL_EXEC:
	  eqv = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, x), UNSPEC_TPREL);
	  eqv = gen_rtx_CONST (Pmode, eqv);
	  tp = gen_reg_rtx (Pmode);
	  scratch = gen_reg_rtx (Pmode);
	  dest = gen_reg_rtx (Pmode);

	  emit_insn (gen_get_thread_pointerdi (tp));
	  emit_insn (gen_rtx_SET (VOIDmode, scratch, eqv));
	  emit_insn (gen_adddi3 (dest, tp, scratch));
	  return dest;

	case TLS_MODEL_LOCAL_EXEC:
	  eqv = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, x), UNSPEC_TPREL);
	  eqv = gen_rtx_CONST (Pmode, eqv);
	  tp = gen_reg_rtx (Pmode);

	  emit_insn (gen_get_thread_pointerdi (tp));
	  if (alpha_tls_size == 32)
	    {
	      insn = gen_rtx_HIGH (Pmode, eqv);
	      insn = gen_rtx_PLUS (Pmode, tp, insn);
	      tp = gen_reg_rtx (Pmode);
	      emit_insn (gen_rtx_SET (VOIDmode, tp, insn));
	    }
	  return gen_rtx_LO_SUM (Pmode, tp, eqv);

	default:
	  gcc_unreachable ();
	}

      if (local_symbolic_operand (x, Pmode))
	{
	  if (small_symbolic_operand (x, Pmode))
	    return x;
	  else
	    {
	      if (can_create_pseudo_p ())
	        scratch = gen_reg_rtx (Pmode);
	      emit_insn (gen_rtx_SET (VOIDmode, scratch,
				      gen_rtx_HIGH (Pmode, x)));
	      return gen_rtx_LO_SUM (Pmode, scratch, x);
	    }
	}
    }

  return NULL;

 split_addend:
  {
    HOST_WIDE_INT low, high;

    low = ((addend & 0xffff) ^ 0x8000) - 0x8000;
    addend -= low;
    high = ((addend & 0xffffffff) ^ 0x80000000) - 0x80000000;
    addend -= high;

    if (addend)
      x = expand_simple_binop (Pmode, PLUS, x, GEN_INT (addend),
			       (!can_create_pseudo_p () ? scratch : NULL_RTX),
			       1, OPTAB_LIB_WIDEN);
    if (high)
      x = expand_simple_binop (Pmode, PLUS, x, GEN_INT (high),
			       (!can_create_pseudo_p () ? scratch : NULL_RTX),
			       1, OPTAB_LIB_WIDEN);

    return plus_constant (Pmode, x, low);
  }
}


/* Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  Return X or the new, valid address.  */

static rtx
alpha_legitimize_address (rtx x, rtx oldx ATTRIBUTE_UNUSED,
			  enum machine_mode mode)
{
  rtx new_x = alpha_legitimize_address_1 (x, NULL_RTX, mode);
  return new_x ? new_x : x;
}

/* Return true if ADDR has an effect that depends on the machine mode it
   is used for.  On the Alpha this is true only for the unaligned modes.
   We can simplify the test since we know that the address must be valid.  */

static bool
alpha_mode_dependent_address_p (const_rtx addr,
				addr_space_t as ATTRIBUTE_UNUSED)
{
  return GET_CODE (addr) == AND;
}

/* Primarily this is required for TLS symbols, but given that our move
   patterns *ought* to be able to handle any symbol at any time, we
   should never be spilling symbolic operands to the constant pool, ever.  */

static bool
alpha_cannot_force_const_mem (enum machine_mode mode ATTRIBUTE_UNUSED, rtx x)
{
  enum rtx_code code = GET_CODE (x);
  return code == SYMBOL_REF || code == LABEL_REF || code == CONST;
}

/* We do not allow indirect calls to be optimized into sibling calls, nor
   can we allow a call to a function with a different GP to be optimized
   into a sibcall.  */

static bool
alpha_function_ok_for_sibcall (tree decl, tree exp ATTRIBUTE_UNUSED)
{
  /* Can't do indirect tail calls, since we don't know if the target
     uses the same GP.  */
  if (!decl)
    return false;

  /* Otherwise, we can make a tail call if the target function shares
     the same GP.  */
  return decl_has_samegp (decl);
}

int
some_small_symbolic_operand_int (rtx *px, void *data ATTRIBUTE_UNUSED)
{
  rtx x = *px;

  /* Don't re-split.  */
  if (GET_CODE (x) == LO_SUM)
    return -1;

  return small_symbolic_operand (x, Pmode) != 0;
}

static int
split_small_symbolic_operand_1 (rtx *px, void *data ATTRIBUTE_UNUSED)
{
  rtx x = *px;

  /* Don't re-split.  */
  if (GET_CODE (x) == LO_SUM)
    return -1;

  if (small_symbolic_operand (x, Pmode))
    {
      x = gen_rtx_LO_SUM (Pmode, pic_offset_table_rtx, x);
      *px = x;
      return -1;
    }

  return 0;
}

rtx
split_small_symbolic_operand (rtx x)
{
  x = copy_insn (x);
  for_each_rtx (&x, split_small_symbolic_operand_1, NULL);
  return x;
}

/* Indicate that INSN cannot be duplicated.  This is true for any insn
   that we've marked with gpdisp relocs, since those have to stay in
   1-1 correspondence with one another.

   Technically we could copy them if we could set up a mapping from one
   sequence number to another, across the set of insns to be duplicated.
   This seems overly complicated and error-prone since interblock motion
   from sched-ebb could move one of the pair of insns to a different block.

   Also cannot allow jsr insns to be duplicated.  If they throw exceptions,
   then they'll be in a different block from their ldgp.  Which could lead
   the bb reorder code to think that it would be ok to copy just the block
   containing the call and branch to the block containing the ldgp.  */

static bool
alpha_cannot_copy_insn_p (rtx insn)
{
  if (!reload_completed || !TARGET_EXPLICIT_RELOCS)
    return false;
  if (recog_memoized (insn) >= 0)
    return get_attr_cannot_copy (insn);
  else
    return false;
}


/* Try a machine-dependent way of reloading an illegitimate address
   operand.  If we find one, push the reload and return the new rtx.  */

rtx
alpha_legitimize_reload_address (rtx x,
				 enum machine_mode mode ATTRIBUTE_UNUSED,
				 int opnum, int type,
				 int ind_levels ATTRIBUTE_UNUSED)
{
  /* We must recognize output that we have already generated ourselves.  */
  if (GET_CODE (x) == PLUS
      && GET_CODE (XEXP (x, 0)) == PLUS
      && REG_P (XEXP (XEXP (x, 0), 0))
      && CONST_INT_P (XEXP (XEXP (x, 0), 1))
      && CONST_INT_P (XEXP (x, 1)))
    {
      push_reload (XEXP (x, 0), NULL_RTX, &XEXP (x, 0), NULL,
		   BASE_REG_CLASS, GET_MODE (x), VOIDmode, 0, 0,
		   opnum, (enum reload_type) type);
      return x;
    }

  /* We wish to handle large displacements off a base register by
     splitting the addend across an ldah and the mem insn.  This
     cuts number of extra insns needed from 3 to 1.  */
  if (GET_CODE (x) == PLUS
      && REG_P (XEXP (x, 0))
      && REGNO (XEXP (x, 0)) < FIRST_PSEUDO_REGISTER
      && REGNO_OK_FOR_BASE_P (REGNO (XEXP (x, 0)))
      && GET_CODE (XEXP (x, 1)) == CONST_INT)
    {
      HOST_WIDE_INT val = INTVAL (XEXP (x, 1));
      HOST_WIDE_INT low = ((val & 0xffff) ^ 0x8000) - 0x8000;
      HOST_WIDE_INT high
	= (((val - low) & 0xffffffff) ^ 0x80000000) - 0x80000000;

      /* Check for 32-bit overflow.  */
      if (high + low != val)
	return NULL_RTX;

      /* Reload the high part into a base reg; leave the low part
	 in the mem directly.  */
      x = gen_rtx_PLUS (GET_MODE (x),
			gen_rtx_PLUS (GET_MODE (x), XEXP (x, 0),
				      GEN_INT (high)),
			GEN_INT (low));

      push_reload (XEXP (x, 0), NULL_RTX, &XEXP (x, 0), NULL,
		   BASE_REG_CLASS, GET_MODE (x), VOIDmode, 0, 0,
		   opnum, (enum reload_type) type);
      return x;
    }

  return NULL_RTX;
}

/* Compute a (partial) cost for rtx X.  Return true if the complete
   cost has been computed, and false if subexpressions should be
   scanned.  In either case, *TOTAL contains the cost result.  */

static bool
alpha_rtx_costs (rtx x, int code, int outer_code, int opno, int *total,
		 bool speed)
{
  enum machine_mode mode = GET_MODE (x);
  bool float_mode_p = FLOAT_MODE_P (mode);
  const struct alpha_rtx_cost_data *cost_data;

  if (!speed)
    cost_data = &alpha_rtx_cost_size;
  else
    cost_data = &alpha_rtx_cost_data[alpha_tune];

  switch (code)
    {
    case CONST_INT:
      /* If this is an 8-bit constant, return zero since it can be used
	 nearly anywhere with no cost.  If it is a valid operand for an
	 ADD or AND, likewise return 0 if we know it will be used in that
	 context.  Otherwise, return 2 since it might be used there later.
	 All other constants take at least two insns.  */
      if (INTVAL (x) >= 0 && INTVAL (x) < 256)
	{
	  *total = 0;
	  return true;
	}
      /* FALLTHRU */

    case CONST_DOUBLE:
      if (x == CONST0_RTX (mode))
	*total = 0;
      else if ((outer_code == PLUS && add_operand (x, VOIDmode))
	       || (outer_code == AND && and_operand (x, VOIDmode)))
	*total = 0;
      else if (add_operand (x, VOIDmode) || and_operand (x, VOIDmode))
	*total = 2;
      else
	*total = COSTS_N_INSNS (2);
      return true;

    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
      if (TARGET_EXPLICIT_RELOCS && small_symbolic_operand (x, VOIDmode))
	*total = COSTS_N_INSNS (outer_code != MEM);
      else if (TARGET_EXPLICIT_RELOCS && local_symbolic_operand (x, VOIDmode))
	*total = COSTS_N_INSNS (1 + (outer_code != MEM));
      else if (tls_symbolic_operand_type (x))
	/* Estimate of cost for call_pal rduniq.  */
	/* ??? How many insns do we emit here?  More than one...  */
	*total = COSTS_N_INSNS (15);
      else
	/* Otherwise we do a load from the GOT.  */
	*total = COSTS_N_INSNS (!speed ? 1 : alpha_memory_latency);
      return true;

    case HIGH:
      /* This is effectively an add_operand.  */
      *total = 2;
      return true;

    case PLUS:
    case MINUS:
      if (float_mode_p)
	*total = cost_data->fp_add;
      else if (GET_CODE (XEXP (x, 0)) == MULT
	       && const48_operand (XEXP (XEXP (x, 0), 1), VOIDmode))
	{
	  *total = (rtx_cost (XEXP (XEXP (x, 0), 0),
			      (enum rtx_code) outer_code, opno, speed)
		    + rtx_cost (XEXP (x, 1),
				(enum rtx_code) outer_code, opno, speed)
		    + COSTS_N_INSNS (1));
	  return true;
	}
      return false;

    case MULT:
      if (float_mode_p)
	*total = cost_data->fp_mult;
      else if (mode == DImode)
	*total = cost_data->int_mult_di;
      else
	*total = cost_data->int_mult_si;
      return false;

    case ASHIFT:
      if (CONST_INT_P (XEXP (x, 1))
	  && INTVAL (XEXP (x, 1)) <= 3)
	{
	  *total = COSTS_N_INSNS (1);
	  return false;
	}
      /* FALLTHRU */

    case ASHIFTRT:
    case LSHIFTRT:
      *total = cost_data->int_shift;
      return false;

    case IF_THEN_ELSE:
      if (float_mode_p)
        *total = cost_data->fp_add;
      else
        *total = cost_data->int_cmov;
      return false;

    case DIV:
    case UDIV:
    case MOD:
    case UMOD:
      if (!float_mode_p)
	*total = cost_data->int_div;
      else if (mode == SFmode)
        *total = cost_data->fp_div_sf;
      else
        *total = cost_data->fp_div_df;
      return false;

    case MEM:
      *total = COSTS_N_INSNS (!speed ? 1 : alpha_memory_latency);
      return true;

    case NEG:
      if (! float_mode_p)
	{
	  *total = COSTS_N_INSNS (1);
	  return false;
	}
      /* FALLTHRU */

    case ABS:
      if (! float_mode_p)
	{
	  *total = COSTS_N_INSNS (1) + cost_data->int_cmov;
	  return false;
	}
      /* FALLTHRU */

    case FLOAT:
    case UNSIGNED_FLOAT:
    case FIX:
    case UNSIGNED_FIX:
    case FLOAT_TRUNCATE:
      *total = cost_data->fp_add;
      return false;

    case FLOAT_EXTEND:
      if (MEM_P (XEXP (x, 0)))
	*total = 0;
      else
	*total = cost_data->fp_add;
      return false;

    default:
      return false;
    }
}

/* REF is an alignable memory location.  Place an aligned SImode
   reference into *PALIGNED_MEM and the number of bits to shift into
   *PBITNUM.  SCRATCH is a free register for use in reloading out
   of range stack slots.  */

void
get_aligned_mem (rtx ref, rtx *paligned_mem, rtx *pbitnum)
{
  rtx base;
  HOST_WIDE_INT disp, offset;

  gcc_assert (MEM_P (ref));

  if (reload_in_progress
      && ! memory_address_p (GET_MODE (ref), XEXP (ref, 0)))
    {
      base = find_replacement (&XEXP (ref, 0));
      gcc_assert (memory_address_p (GET_MODE (ref), base));
    }
  else
    base = XEXP (ref, 0);

  if (GET_CODE (base) == PLUS)
    disp = INTVAL (XEXP (base, 1)), base = XEXP (base, 0);
  else
    disp = 0;

  /* Find the byte offset within an aligned word.  If the memory itself is
     claimed to be aligned, believe it.  Otherwise, aligned_memory_operand
     will have examined the base register and determined it is aligned, and
     thus displacements from it are naturally alignable.  */
  if (MEM_ALIGN (ref) >= 32)
    offset = 0;
  else
    offset = disp & 3;

  /* The location should not cross aligned word boundary.  */
  gcc_assert (offset + GET_MODE_SIZE (GET_MODE (ref))
	      <= GET_MODE_SIZE (SImode));

  /* Access the entire aligned word.  */
  *paligned_mem = widen_memory_access (ref, SImode, -offset);

  /* Convert the byte offset within the word to a bit offset.  */
  offset *= BITS_PER_UNIT;
  *pbitnum = GEN_INT (offset);
}

/* Similar, but just get the address.  Handle the two reload cases.
   Add EXTRA_OFFSET to the address we return.  */

rtx
get_unaligned_address (rtx ref)
{
  rtx base;
  HOST_WIDE_INT offset = 0;

  gcc_assert (MEM_P (ref));

  if (reload_in_progress
      && ! memory_address_p (GET_MODE (ref), XEXP (ref, 0)))
    {
      base = find_replacement (&XEXP (ref, 0));

      gcc_assert (memory_address_p (GET_MODE (ref), base));
    }
  else
    base = XEXP (ref, 0);

  if (GET_CODE (base) == PLUS)
    offset += INTVAL (XEXP (base, 1)), base = XEXP (base, 0);

  return plus_constant (Pmode, base, offset);
}

/* Compute a value X, such that X & 7 == (ADDR + OFS) & 7.
   X is always returned in a register.  */

rtx
get_unaligned_offset (rtx addr, HOST_WIDE_INT ofs)
{
  if (GET_CODE (addr) == PLUS)
    {
      ofs += INTVAL (XEXP (addr, 1));
      addr = XEXP (addr, 0);
    }

  return expand_simple_binop (Pmode, PLUS, addr, GEN_INT (ofs & 7),
			      NULL_RTX, 1, OPTAB_LIB_WIDEN);
}

/* On the Alpha, all (non-symbolic) constants except zero go into
   a floating-point register via memory.  Note that we cannot
   return anything that is not a subset of RCLASS, and that some
   symbolic constants cannot be dropped to memory.  */

enum reg_class
alpha_preferred_reload_class(rtx x, enum reg_class rclass)
{
  /* Zero is present in any register class.  */
  if (x == CONST0_RTX (GET_MODE (x)))
    return rclass;

  /* These sorts of constants we can easily drop to memory.  */
  if (CONST_INT_P (x)
      || GET_CODE (x) == CONST_DOUBLE
      || GET_CODE (x) == CONST_VECTOR)
    {
      if (rclass == FLOAT_REGS)
	return NO_REGS;
      if (rclass == ALL_REGS)
	return GENERAL_REGS;
      return rclass;
    }

  /* All other kinds of constants should not (and in the case of HIGH
     cannot) be dropped to memory -- instead we use a GENERAL_REGS
     secondary reload.  */
  if (CONSTANT_P (x))
    return (rclass == ALL_REGS ? GENERAL_REGS : rclass);

  return rclass;
}

/* Inform reload about cases where moving X with a mode MODE to a register in
   RCLASS requires an extra scratch or immediate register.  Return the class
   needed for the immediate register.  */

static reg_class_t
alpha_secondary_reload (bool in_p, rtx x, reg_class_t rclass_i,
			enum machine_mode mode, secondary_reload_info *sri)
{
  enum reg_class rclass = (enum reg_class) rclass_i;

  /* Loading and storing HImode or QImode values to and from memory
     usually requires a scratch register.  */
  if (!TARGET_BWX && (mode == QImode || mode == HImode || mode == CQImode))
    {
      if (any_memory_operand (x, mode))
	{
	  if (in_p)
	    {
	      if (!aligned_memory_operand (x, mode))
		sri->icode = direct_optab_handler (reload_in_optab, mode);
	    }
	  else
	    sri->icode = direct_optab_handler (reload_out_optab, mode);
	  return NO_REGS;
	}
    }

  /* We also cannot do integral arithmetic into FP regs, as might result
     from register elimination into a DImode fp register.  */
  if (rclass == FLOAT_REGS)
    {
      if (MEM_P (x) && GET_CODE (XEXP (x, 0)) == AND)
	return GENERAL_REGS;
      if (in_p && INTEGRAL_MODE_P (mode)
	  && !MEM_P (x) && !REG_P (x) && !CONST_INT_P (x))
	return GENERAL_REGS;
    }

  return NO_REGS;
}

/* Subfunction of the following function.  Update the flags of any MEM
   found in part of X.  */

static int
alpha_set_memflags_1 (rtx *xp, void *data)
{
  rtx x = *xp, orig = (rtx) data;

  if (!MEM_P (x))
    return 0;

  MEM_VOLATILE_P (x) = MEM_VOLATILE_P (orig);
  MEM_NOTRAP_P (x) = MEM_NOTRAP_P (orig);
  MEM_READONLY_P (x) = MEM_READONLY_P (orig);

  /* Sadly, we cannot use alias sets because the extra aliasing
     produced by the AND interferes.  Given that two-byte quantities
     are the only thing we would be able to differentiate anyway,
     there does not seem to be any point in convoluting the early
     out of the alias check.  */

  return -1;
}

/* Given SEQ, which is an INSN list, look for any MEMs in either
   a SET_DEST or a SET_SRC and copy the in-struct, unchanging, and
   volatile flags from REF into each of the MEMs found.  If REF is not
   a MEM, don't do anything.  */

void
alpha_set_memflags (rtx seq, rtx ref)
{
  rtx insn;

  if (!MEM_P (ref))
    return;

  /* This is only called from alpha.md, after having had something
     generated from one of the insn patterns.  So if everything is
     zero, the pattern is already up-to-date.  */
  if (!MEM_VOLATILE_P (ref)
      && !MEM_NOTRAP_P (ref)
      && !MEM_READONLY_P (ref))
    return;

  for (insn = seq; insn; insn = NEXT_INSN (insn))
    if (INSN_P (insn))
      for_each_rtx (&PATTERN (insn), alpha_set_memflags_1, (void *) ref);
    else
      gcc_unreachable ();
}

static rtx alpha_emit_set_const (rtx, enum machine_mode, HOST_WIDE_INT,
				 int, bool);

/* Internal routine for alpha_emit_set_const to check for N or below insns.
   If NO_OUTPUT is true, then we only check to see if N insns are possible,
   and return pc_rtx if successful.  */

static rtx
alpha_emit_set_const_1 (rtx target, enum machine_mode mode,
			HOST_WIDE_INT c, int n, bool no_output)
{
  HOST_WIDE_INT new_const;
  int i, bits;
  /* Use a pseudo if highly optimizing and still generating RTL.  */
  rtx subtarget
    = (flag_expensive_optimizations && can_create_pseudo_p () ? 0 : target);
  rtx temp, insn;

  /* If this is a sign-extended 32-bit constant, we can do this in at most
     three insns, so do it if we have enough insns left.  We always have
     a sign-extended 32-bit constant when compiling on a narrow machine.  */

  if (HOST_BITS_PER_WIDE_INT != 64
      || c >> 31 == -1 || c >> 31 == 0)
    {
      HOST_WIDE_INT low = ((c & 0xffff) ^ 0x8000) - 0x8000;
      HOST_WIDE_INT tmp1 = c - low;
      HOST_WIDE_INT high = (((tmp1 >> 16) & 0xffff) ^ 0x8000) - 0x8000;
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

	  if (no_output)
	    return pc_rtx;
	  if (target == NULL)
	    target = gen_reg_rtx (mode);
	  emit_insn (gen_rtx_SET (VOIDmode, target, GEN_INT (c)));
	  return target;
	}
      else if (n >= 2 + (extra != 0))
	{
	  if (no_output)
	    return pc_rtx;
	  if (!can_create_pseudo_p ())
	    {
	      emit_insn (gen_rtx_SET (VOIDmode, target, GEN_INT (high << 16)));
	      temp = target;
	    }
	  else
	    temp = copy_to_suggested_reg (GEN_INT (high << 16),
					  subtarget, mode);

	  /* As of 2002-02-23, addsi3 is only available when not optimizing.
	     This means that if we go through expand_binop, we'll try to
	     generate extensions, etc, which will require new pseudos, which
	     will fail during some split phases.  The SImode add patterns
	     still exist, but are not named.  So build the insns by hand.  */

	  if (extra != 0)
	    {
	      if (! subtarget)
		subtarget = gen_reg_rtx (mode);
	      insn = gen_rtx_PLUS (mode, temp, GEN_INT (extra << 16));
	      insn = gen_rtx_SET (VOIDmode, subtarget, insn);
	      emit_insn (insn);
	      temp = subtarget;
	    }

	  if (target == NULL)
	    target = gen_reg_rtx (mode);
	  insn = gen_rtx_PLUS (mode, temp, GEN_INT (low));
	  insn = gen_rtx_SET (VOIDmode, target, insn);
	  emit_insn (insn);
	  return target;
	}
    }

  /* If we couldn't do it that way, try some other methods.  But if we have
     no instructions left, don't bother.  Likewise, if this is SImode and
     we can't make pseudos, we can't do anything since the expand_binop
     and expand_unop calls will widen and try to make pseudos.  */

  if (n == 1 || (mode == SImode && !can_create_pseudo_p ()))
    return 0;

  /* Next, see if we can load a related constant and then shift and possibly
     negate it to get the constant we want.  Try this once each increasing
     numbers of insns.  */

  for (i = 1; i < n; i++)
    {
      /* First, see if minus some low bits, we've an easy load of
	 high bits.  */

      new_const = ((c & 0xffff) ^ 0x8000) - 0x8000;
      if (new_const != 0)
	{
          temp = alpha_emit_set_const (subtarget, mode, c - new_const, i, no_output);
	  if (temp)
	    {
	      if (no_output)
		return temp;
	      return expand_binop (mode, add_optab, temp, GEN_INT (new_const),
				   target, 0, OPTAB_WIDEN);
	    }
	}

      /* Next try complementing.  */
      temp = alpha_emit_set_const (subtarget, mode, ~c, i, no_output);
      if (temp)
	{
	  if (no_output)
	    return temp;
	  return expand_unop (mode, one_cmpl_optab, temp, target, 0);
	}

      /* Next try to form a constant and do a left shift.  We can do this
	 if some low-order bits are zero; the exact_log2 call below tells
	 us that information.  The bits we are shifting out could be any
	 value, but here we'll just try the 0- and sign-extended forms of
	 the constant.  To try to increase the chance of having the same
	 constant in more than one insn, start at the highest number of
	 bits to shift, but try all possibilities in case a ZAPNOT will
	 be useful.  */

      bits = exact_log2 (c & -c);
      if (bits > 0)
	for (; bits > 0; bits--)
	  {
	    new_const = c >> bits;
	    temp = alpha_emit_set_const (subtarget, mode, new_const, i, no_output);
	    if (!temp && c < 0)
	      {
		new_const = (unsigned HOST_WIDE_INT)c >> bits;
		temp = alpha_emit_set_const (subtarget, mode, new_const,
					     i, no_output);
	      }
	    if (temp)
	      {
		if (no_output)
		  return temp;
	        return expand_binop (mode, ashl_optab, temp, GEN_INT (bits),
				     target, 0, OPTAB_WIDEN);
	      }
	  }

      /* Now try high-order zero bits.  Here we try the shifted-in bits as
	 all zero and all ones.  Be careful to avoid shifting outside the
	 mode and to avoid shifting outside the host wide int size.  */
      /* On narrow hosts, don't shift a 1 into the high bit, since we'll
	 confuse the recursive call and set all of the high 32 bits.  */

      bits = (MIN (HOST_BITS_PER_WIDE_INT, GET_MODE_SIZE (mode) * 8)
	      - floor_log2 (c) - 1 - (HOST_BITS_PER_WIDE_INT < 64));
      if (bits > 0)
	for (; bits > 0; bits--)
	  {
	    new_const = c << bits;
	    temp = alpha_emit_set_const (subtarget, mode, new_const, i, no_output);
	    if (!temp)
	      {
		new_const = (c << bits) | (((HOST_WIDE_INT) 1 << bits) - 1);
	        temp = alpha_emit_set_const (subtarget, mode, new_const,
					     i, no_output);
	      }
	    if (temp)
	      {
		if (no_output)
		  return temp;
		return expand_binop (mode, lshr_optab, temp, GEN_INT (bits),
				     target, 1, OPTAB_WIDEN);
	      }
	  }

      /* Now try high-order 1 bits.  We get that with a sign-extension.
	 But one bit isn't enough here.  Be careful to avoid shifting outside
	 the mode and to avoid shifting outside the host wide int size.  */

      bits = (MIN (HOST_BITS_PER_WIDE_INT, GET_MODE_SIZE (mode) * 8)
	      - floor_log2 (~ c) - 2);
      if (bits > 0)
	for (; bits > 0; bits--)
	  {
	    new_const = c << bits;
	    temp = alpha_emit_set_const (subtarget, mode, new_const, i, no_output);
	    if (!temp)
	      {
		new_const = (c << bits) | (((HOST_WIDE_INT) 1 << bits) - 1);
	        temp = alpha_emit_set_const (subtarget, mode, new_const,
					     i, no_output);
	      }
	    if (temp)
	      {
		if (no_output)
		  return temp;
		return expand_binop (mode, ashr_optab, temp, GEN_INT (bits),
				     target, 0, OPTAB_WIDEN);
	      }
	  }
    }

#if HOST_BITS_PER_WIDE_INT == 64
  /* Finally, see if can load a value into the target that is the same as the
     constant except that all bytes that are 0 are changed to be 0xff.  If we
     can, then we can do a ZAPNOT to obtain the desired constant.  */

  new_const = c;
  for (i = 0; i < 64; i += 8)
    if ((new_const & ((HOST_WIDE_INT) 0xff << i)) == 0)
      new_const |= (HOST_WIDE_INT) 0xff << i;

  /* We are only called for SImode and DImode.  If this is SImode, ensure that
     we are sign extended to a full word.  */

  if (mode == SImode)
    new_const = ((new_const & 0xffffffff) ^ 0x80000000) - 0x80000000;

  if (new_const != c)
    {
      temp = alpha_emit_set_const (subtarget, mode, new_const, n - 1, no_output);
      if (temp)
	{
	  if (no_output)
	    return temp;
	  return expand_binop (mode, and_optab, temp, GEN_INT (c | ~ new_const),
			       target, 0, OPTAB_WIDEN);
	}
    }
#endif

  return 0;
}

/* Try to output insns to set TARGET equal to the constant C if it can be
   done in less than N insns.  Do all computations in MODE.  Returns the place
   where the output has been placed if it can be done and the insns have been
   emitted.  If it would take more than N insns, zero is returned and no
   insns and emitted.  */

static rtx
alpha_emit_set_const (rtx target, enum machine_mode mode,
		      HOST_WIDE_INT c, int n, bool no_output)
{
  enum machine_mode orig_mode = mode;
  rtx orig_target = target;
  rtx result = 0;
  int i;

  /* If we can't make any pseudos, TARGET is an SImode hard register, we
     can't load this constant in one insn, do this in DImode.  */
  if (!can_create_pseudo_p () && mode == SImode
      && REG_P (target) && REGNO (target) < FIRST_PSEUDO_REGISTER)
    {
      result = alpha_emit_set_const_1 (target, mode, c, 1, no_output);
      if (result)
	return result;

      target = no_output ? NULL : gen_lowpart (DImode, target);
      mode = DImode;
    }
  else if (mode == V8QImode || mode == V4HImode || mode == V2SImode)
    {
      target = no_output ? NULL : gen_lowpart (DImode, target);
      mode = DImode;
    }

  /* Try 1 insn, then 2, then up to N.  */
  for (i = 1; i <= n; i++)
    {
      result = alpha_emit_set_const_1 (target, mode, c, i, no_output);
      if (result)
	{
	  rtx insn, set;

	  if (no_output)
	    return result;

	  insn = get_last_insn ();
	  set = single_set (insn);
	  if (! CONSTANT_P (SET_SRC (set)))
	    set_unique_reg_note (get_last_insn (), REG_EQUAL, GEN_INT (c));
	  break;
	}
    }

  /* Allow for the case where we changed the mode of TARGET.  */
  if (result)
    {
      if (result == target)
	result = orig_target;
      else if (mode != orig_mode)
	result = gen_lowpart (orig_mode, result);
    }

  return result;
}

/* Having failed to find a 3 insn sequence in alpha_emit_set_const,
   fall back to a straight forward decomposition.  We do this to avoid
   exponential run times encountered when looking for longer sequences
   with alpha_emit_set_const.  */

static rtx
alpha_emit_set_long_const (rtx target, HOST_WIDE_INT c1, HOST_WIDE_INT c2)
{
  HOST_WIDE_INT d1, d2, d3, d4;

  /* Decompose the entire word */
#if HOST_BITS_PER_WIDE_INT >= 64
  gcc_assert (c2 == -(c1 < 0));
  d1 = ((c1 & 0xffff) ^ 0x8000) - 0x8000;
  c1 -= d1;
  d2 = ((c1 & 0xffffffff) ^ 0x80000000) - 0x80000000;
  c1 = (c1 - d2) >> 32;
  d3 = ((c1 & 0xffff) ^ 0x8000) - 0x8000;
  c1 -= d3;
  d4 = ((c1 & 0xffffffff) ^ 0x80000000) - 0x80000000;
  gcc_assert (c1 == d4);
#else
  d1 = ((c1 & 0xffff) ^ 0x8000) - 0x8000;
  c1 -= d1;
  d2 = ((c1 & 0xffffffff) ^ 0x80000000) - 0x80000000;
  gcc_assert (c1 == d2);
  c2 += (d2 < 0);
  d3 = ((c2 & 0xffff) ^ 0x8000) - 0x8000;
  c2 -= d3;
  d4 = ((c2 & 0xffffffff) ^ 0x80000000) - 0x80000000;
  gcc_assert (c2 == d4);
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

/* Given an integral CONST_INT, CONST_DOUBLE, or CONST_VECTOR, return 
   the low 64 bits.  */

static void
alpha_extract_integer (rtx x, HOST_WIDE_INT *p0, HOST_WIDE_INT *p1)
{
  HOST_WIDE_INT i0, i1;

  if (GET_CODE (x) == CONST_VECTOR)
    x = simplify_subreg (DImode, x, GET_MODE (x), 0);


  if (CONST_INT_P (x))
    {
      i0 = INTVAL (x);
      i1 = -(i0 < 0);
    }
  else if (HOST_BITS_PER_WIDE_INT >= 64)
    {
      i0 = CONST_DOUBLE_LOW (x);
      i1 = -(i0 < 0);
    }
  else
    {
      i0 = CONST_DOUBLE_LOW (x);
      i1 = CONST_DOUBLE_HIGH (x);
    }

  *p0 = i0;
  *p1 = i1;
}

/* Implement TARGET_LEGITIMATE_CONSTANT_P.  This is all constants for which
   we are willing to load the value into a register via a move pattern.
   Normally this is all symbolic constants, integral constants that
   take three or fewer instructions, and floating-point zero.  */

bool
alpha_legitimate_constant_p (enum machine_mode mode, rtx x)
{
  HOST_WIDE_INT i0, i1;

  switch (GET_CODE (x))
    {
    case LABEL_REF:
    case HIGH:
      return true;

    case CONST:
      if (GET_CODE (XEXP (x, 0)) == PLUS
	  && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT)
	x = XEXP (XEXP (x, 0), 0);
      else
	return true;

      if (GET_CODE (x) != SYMBOL_REF)
	return true;

      /* FALLTHRU */

    case SYMBOL_REF:
      /* TLS symbols are never valid.  */
      return SYMBOL_REF_TLS_MODEL (x) == 0;

    case CONST_DOUBLE:
      if (x == CONST0_RTX (mode))
	return true;
      if (FLOAT_MODE_P (mode))
	return false;
      goto do_integer;

    case CONST_VECTOR:
      if (x == CONST0_RTX (mode))
	return true;
      if (GET_MODE_CLASS (mode) != MODE_VECTOR_INT)
	return false;
      if (GET_MODE_SIZE (mode) != 8)
	return false;
      goto do_integer;

    case CONST_INT:
    do_integer:
      if (TARGET_BUILD_CONSTANTS)
	return true;
      alpha_extract_integer (x, &i0, &i1);
      if (HOST_BITS_PER_WIDE_INT >= 64 || i1 == (-i0 < 0))
        return alpha_emit_set_const_1 (x, mode, i0, 3, true) != NULL;
      return false;

    default:
      return false;
    }
}

/* Operand 1 is known to be a constant, and should require more than one
   instruction to load.  Emit that multi-part load.  */

bool
alpha_split_const_mov (enum machine_mode mode, rtx *operands)
{
  HOST_WIDE_INT i0, i1;
  rtx temp = NULL_RTX;

  alpha_extract_integer (operands[1], &i0, &i1);

  if (HOST_BITS_PER_WIDE_INT >= 64 || i1 == -(i0 < 0))
    temp = alpha_emit_set_const (operands[0], mode, i0, 3, false);

  if (!temp && TARGET_BUILD_CONSTANTS)
    temp = alpha_emit_set_long_const (operands[0], i0, i1);

  if (temp)
    {
      if (!rtx_equal_p (operands[0], temp))
	emit_move_insn (operands[0], temp);
      return true;
    }

  return false;
}

/* Expand a move instruction; return true if all work is done.
   We don't handle non-bwx subword loads here.  */

bool
alpha_expand_mov (enum machine_mode mode, rtx *operands)
{
  rtx tmp;

  /* If the output is not a register, the input must be.  */
  if (MEM_P (operands[0])
      && ! reg_or_0_operand (operands[1], mode))
    operands[1] = force_reg (mode, operands[1]);

  /* Allow legitimize_address to perform some simplifications.  */
  if (mode == Pmode && symbolic_operand (operands[1], mode))
    {
      tmp = alpha_legitimize_address_1 (operands[1], operands[0], mode);
      if (tmp)
	{
	  if (tmp == operands[0])
	    return true;
	  operands[1] = tmp;
	  return false;
	}
    }

  /* Early out for non-constants and valid constants.  */
  if (! CONSTANT_P (operands[1]) || input_operand (operands[1], mode))
    return false;

  /* Split large integers.  */
  if (CONST_INT_P (operands[1])
      || GET_CODE (operands[1]) == CONST_DOUBLE
      || GET_CODE (operands[1]) == CONST_VECTOR)
    {
      if (alpha_split_const_mov (mode, operands))
	return true;
    }

  /* Otherwise we've nothing left but to drop the thing to memory.  */
  tmp = force_const_mem (mode, operands[1]);

  if (tmp == NULL_RTX)
    return false;

  if (reload_in_progress)
    {
      emit_move_insn (operands[0], XEXP (tmp, 0));
      operands[1] = replace_equiv_address (tmp, operands[0]);
    }
  else
    operands[1] = validize_mem (tmp);
  return false;
}

/* Expand a non-bwx QImode or HImode move instruction;
   return true if all work is done.  */

bool
alpha_expand_mov_nobwx (enum machine_mode mode, rtx *operands)
{
  rtx seq;

  /* If the output is not a register, the input must be.  */
  if (MEM_P (operands[0]))
    operands[1] = force_reg (mode, operands[1]);

  /* Handle four memory cases, unaligned and aligned for either the input
     or the output.  The only case where we can be called during reload is
     for aligned loads; all other cases require temporaries.  */

  if (any_memory_operand (operands[1], mode))
    {
      if (aligned_memory_operand (operands[1], mode))
	{
	  if (reload_in_progress)
	    {
	      if (mode == QImode)
		seq = gen_reload_inqi_aligned (operands[0], operands[1]);
	      else
		seq = gen_reload_inhi_aligned (operands[0], operands[1]);
	      emit_insn (seq);
	    }
	  else
	    {
	      rtx aligned_mem, bitnum;
	      rtx scratch = gen_reg_rtx (SImode);
	      rtx subtarget;
	      bool copyout;

	      get_aligned_mem (operands[1], &aligned_mem, &bitnum);

	      subtarget = operands[0];
	      if (REG_P (subtarget))
		subtarget = gen_lowpart (DImode, subtarget), copyout = false;
	      else
		subtarget = gen_reg_rtx (DImode), copyout = true;

	      if (mode == QImode)
		seq = gen_aligned_loadqi (subtarget, aligned_mem,
					  bitnum, scratch);
	      else
		seq = gen_aligned_loadhi (subtarget, aligned_mem,
					  bitnum, scratch);
	      emit_insn (seq);

	      if (copyout)
		emit_move_insn (operands[0], gen_lowpart (mode, subtarget));
	    }
	}
      else
	{
	  /* Don't pass these as parameters since that makes the generated
	     code depend on parameter evaluation order which will cause
	     bootstrap failures.  */

	  rtx temp1, temp2, subtarget, ua;
	  bool copyout;

	  temp1 = gen_reg_rtx (DImode);
	  temp2 = gen_reg_rtx (DImode);

	  subtarget = operands[0];
	  if (REG_P (subtarget))
	    subtarget = gen_lowpart (DImode, subtarget), copyout = false;
	  else
	    subtarget = gen_reg_rtx (DImode), copyout = true;

	  ua = get_unaligned_address (operands[1]);
	  if (mode == QImode)
	    seq = gen_unaligned_loadqi (subtarget, ua, temp1, temp2);
	  else
	    seq = gen_unaligned_loadhi (subtarget, ua, temp1, temp2);

	  alpha_set_memflags (seq, operands[1]);
	  emit_insn (seq);

	  if (copyout)
	    emit_move_insn (operands[0], gen_lowpart (mode, subtarget));
	}
      return true;
    }

  if (any_memory_operand (operands[0], mode))
    {
      if (aligned_memory_operand (operands[0], mode))
	{
	  rtx aligned_mem, bitnum;
	  rtx temp1 = gen_reg_rtx (SImode);
	  rtx temp2 = gen_reg_rtx (SImode);

	  get_aligned_mem (operands[0], &aligned_mem, &bitnum);

	  emit_insn (gen_aligned_store (aligned_mem, operands[1], bitnum,
					temp1, temp2));
	}
      else
	{
	  rtx temp1 = gen_reg_rtx (DImode);
	  rtx temp2 = gen_reg_rtx (DImode);
	  rtx temp3 = gen_reg_rtx (DImode);
	  rtx ua = get_unaligned_address (operands[0]);

	  if (mode == QImode)
	    seq = gen_unaligned_storeqi (ua, operands[1], temp1, temp2, temp3);
	  else
	    seq = gen_unaligned_storehi (ua, operands[1], temp1, temp2, temp3);

	  alpha_set_memflags (seq, operands[0]);
	  emit_insn (seq);
	}
      return true;
    }

  return false;
}

/* Implement the movmisalign patterns.  One of the operands is a memory
   that is not naturally aligned.  Emit instructions to load it.  */

void
alpha_expand_movmisalign (enum machine_mode mode, rtx *operands)
{
  /* Honor misaligned loads, for those we promised to do so.  */
  if (MEM_P (operands[1]))
    {
      rtx tmp;

      if (register_operand (operands[0], mode))
	tmp = operands[0];
      else
	tmp = gen_reg_rtx (mode);

      alpha_expand_unaligned_load (tmp, operands[1], 8, 0, 0);
      if (tmp != operands[0])
	emit_move_insn (operands[0], tmp);
    }
  else if (MEM_P (operands[0]))
    {
      if (!reg_or_0_operand (operands[1], mode))
	operands[1] = force_reg (mode, operands[1]);
      alpha_expand_unaligned_store (operands[0], operands[1], 8, 0);
    }
  else
    gcc_unreachable ();
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
	cmplt	$16,0,$1
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
alpha_emit_floatuns (rtx operands[2])
{
  rtx neglab, donelab, i0, i1, f0, in, out;
  enum machine_mode mode;

  out = operands[0];
  in = force_reg (DImode, operands[1]);
  mode = GET_MODE (out);
  neglab = gen_label_rtx ();
  donelab = gen_label_rtx ();
  i0 = gen_reg_rtx (DImode);
  i1 = gen_reg_rtx (DImode);
  f0 = gen_reg_rtx (mode);

  emit_cmp_and_jump_insns (in, const0_rtx, LT, const0_rtx, DImode, 0, neglab);

  emit_insn (gen_rtx_SET (VOIDmode, out, gen_rtx_FLOAT (mode, in)));
  emit_jump_insn (gen_jump (donelab));
  emit_barrier ();

  emit_label (neglab);

  emit_insn (gen_lshrdi3 (i0, in, const1_rtx));
  emit_insn (gen_anddi3 (i1, in, const1_rtx));
  emit_insn (gen_iordi3 (i0, i0, i1));
  emit_insn (gen_rtx_SET (VOIDmode, f0, gen_rtx_FLOAT (mode, i0)));
  emit_insn (gen_rtx_SET (VOIDmode, out, gen_rtx_PLUS (mode, f0, f0)));

  emit_label (donelab);
}

/* Generate the comparison for a conditional branch.  */

void
alpha_emit_conditional_branch (rtx operands[], enum machine_mode cmp_mode)
{
  enum rtx_code cmp_code, branch_code;
  enum machine_mode branch_mode = VOIDmode;
  enum rtx_code code = GET_CODE (operands[0]);
  rtx op0 = operands[1], op1 = operands[2];
  rtx tem;

  if (cmp_mode == TFmode)
    {
      op0 = alpha_emit_xfloating_compare (&code, op0, op1);
      op1 = const0_rtx;
      cmp_mode = DImode;
    }

  /* The general case: fold the comparison code to the types of compares
     that we have, choosing the branch as necessary.  */
  switch (code)
    {
    case EQ:  case LE:  case LT:  case LEU:  case LTU:
    case UNORDERED:
      /* We have these compares.  */
      cmp_code = code, branch_code = NE;
      break;

    case NE:
    case ORDERED:
      /* These must be reversed.  */
      cmp_code = reverse_condition (code), branch_code = EQ;
      break;

    case GE:  case GT: case GEU:  case GTU:
      /* For FP, we swap them, for INT, we reverse them.  */
      if (cmp_mode == DFmode)
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
      gcc_unreachable ();
    }

  if (cmp_mode == DFmode)
    {
      if (flag_unsafe_math_optimizations && cmp_code != UNORDERED)
	{
	  /* When we are not as concerned about non-finite values, and we
	     are comparing against zero, we can branch directly.  */
	  if (op1 == CONST0_RTX (DFmode))
	    cmp_code = UNKNOWN, branch_code = code;
	  else if (op0 == CONST0_RTX (DFmode))
	    {
	      /* Undo the swap we probably did just above.  */
	      tem = op0, op0 = op1, op1 = tem;
	      branch_code = swap_condition (cmp_code);
	      cmp_code = UNKNOWN;
	    }
	}
      else
	{
	  /* ??? We mark the branch mode to be CCmode to prevent the
	     compare and branch from being combined, since the compare
	     insn follows IEEE rules that the branch does not.  */
	  branch_mode = CCmode;
	}
    }
  else
    {
      /* The following optimizations are only for signed compares.  */
      if (code != LEU && code != LTU && code != GEU && code != GTU)
	{
	  /* Whee.  Compare and branch against 0 directly.  */
	  if (op1 == const0_rtx)
	    cmp_code = UNKNOWN, branch_code = code;

	  /* If the constants doesn't fit into an immediate, but can
 	     be generated by lda/ldah, we adjust the argument and
 	     compare against zero, so we can use beq/bne directly.  */
	  /* ??? Don't do this when comparing against symbols, otherwise
	     we'll reduce (&x == 0x1234) to (&x-0x1234 == 0), which will
	     be declared false out of hand (at least for non-weak).  */
	  else if (CONST_INT_P (op1)
		   && (code == EQ || code == NE)
		   && !(symbolic_operand (op0, VOIDmode)
			|| (REG_P (op0) && REG_POINTER (op0))))
	    {
	      rtx n_op1 = GEN_INT (-INTVAL (op1));

	      if (! satisfies_constraint_I (op1)
		  && (satisfies_constraint_K (n_op1)
		      || satisfies_constraint_L (n_op1)))
		cmp_code = PLUS, branch_code = code, op1 = n_op1;
	    }
	}

      if (!reg_or_0_operand (op0, DImode))
	op0 = force_reg (DImode, op0);
      if (cmp_code != PLUS && !reg_or_8bit_operand (op1, DImode))
	op1 = force_reg (DImode, op1);
    }

  /* Emit an initial compare instruction, if necessary.  */
  tem = op0;
  if (cmp_code != UNKNOWN)
    {
      tem = gen_reg_rtx (cmp_mode);
      emit_move_insn (tem, gen_rtx_fmt_ee (cmp_code, cmp_mode, op0, op1));
    }

  /* Emit the branch instruction.  */
  tem = gen_rtx_SET (VOIDmode, pc_rtx,
		     gen_rtx_IF_THEN_ELSE (VOIDmode,
					   gen_rtx_fmt_ee (branch_code,
							   branch_mode, tem,
							   CONST0_RTX (cmp_mode)),
					   gen_rtx_LABEL_REF (VOIDmode,
							      operands[3]),
					   pc_rtx));
  emit_jump_insn (tem);
}

/* Certain simplifications can be done to make invalid setcc operations
   valid.  Return the final comparison, or NULL if we can't work.  */

bool
alpha_emit_setcc (rtx operands[], enum machine_mode cmp_mode)
{
  enum rtx_code cmp_code;
  enum rtx_code code = GET_CODE (operands[1]);
  rtx op0 = operands[2], op1 = operands[3];
  rtx tmp;

  if (cmp_mode == TFmode)
    {
      op0 = alpha_emit_xfloating_compare (&code, op0, op1);
      op1 = const0_rtx;
      cmp_mode = DImode;
    }

  if (cmp_mode == DFmode && !TARGET_FIX)
    return 0;

  /* The general case: fold the comparison code to the types of compares
     that we have, choosing the branch as necessary.  */

  cmp_code = UNKNOWN;
  switch (code)
    {
    case EQ:  case LE:  case LT:  case LEU:  case LTU:
    case UNORDERED:
      /* We have these compares.  */
      if (cmp_mode == DFmode)
	cmp_code = code, code = NE;
      break;

    case NE:
      if (cmp_mode == DImode && op1 == const0_rtx)
	break;
      /* FALLTHRU */

    case ORDERED:
      cmp_code = reverse_condition (code);
      code = EQ;
      break;

    case GE:  case GT: case GEU:  case GTU:
      /* These normally need swapping, but for integer zero we have
	 special patterns that recognize swapped operands.  */
      if (cmp_mode == DImode && op1 == const0_rtx)
	break;
      code = swap_condition (code);
      if (cmp_mode == DFmode)
	cmp_code = code, code = NE;
      tmp = op0, op0 = op1, op1 = tmp;
      break;

    default:
      gcc_unreachable ();
    }

  if (cmp_mode == DImode)
    {
      if (!register_operand (op0, DImode))
	op0 = force_reg (DImode, op0);
      if (!reg_or_8bit_operand (op1, DImode))
	op1 = force_reg (DImode, op1);
    }

  /* Emit an initial compare instruction, if necessary.  */
  if (cmp_code != UNKNOWN)
    {
      tmp = gen_reg_rtx (cmp_mode);
      emit_insn (gen_rtx_SET (VOIDmode, tmp,
			      gen_rtx_fmt_ee (cmp_code, cmp_mode, op0, op1)));

      op0 = cmp_mode != DImode ? gen_lowpart (DImode, tmp) : tmp;
      op1 = const0_rtx;
    }

  /* Emit the setcc instruction.  */
  emit_insn (gen_rtx_SET (VOIDmode, operands[0],
			  gen_rtx_fmt_ee (code, DImode, op0, op1)));
  return true;
}


/* Rewrite a comparison against zero CMP of the form
   (CODE (cc0) (const_int 0)) so it can be written validly in
   a conditional move (if_then_else CMP ...).
   If both of the operands that set cc0 are nonzero we must emit
   an insn to perform the compare (it can't be done within
   the conditional move).  */

rtx
alpha_emit_conditional_move (rtx cmp, enum machine_mode mode)
{
  enum rtx_code code = GET_CODE (cmp);
  enum rtx_code cmov_code = NE;
  rtx op0 = XEXP (cmp, 0);
  rtx op1 = XEXP (cmp, 1);
  enum machine_mode cmp_mode
    = (GET_MODE (op0) == VOIDmode ? DImode : GET_MODE (op0));
  enum machine_mode cmov_mode = VOIDmode;
  int local_fast_math = flag_unsafe_math_optimizations;
  rtx tem;

  if (cmp_mode == TFmode)
    {
      op0 = alpha_emit_xfloating_compare (&code, op0, op1);
      op1 = const0_rtx;
      cmp_mode = DImode;
    }

  gcc_assert (cmp_mode == DFmode || cmp_mode == DImode);

  if (FLOAT_MODE_P (cmp_mode) != FLOAT_MODE_P (mode))
    {
      enum rtx_code cmp_code;

      if (! TARGET_FIX)
	return 0;

      /* If we have fp<->int register move instructions, do a cmov by
	 performing the comparison in fp registers, and move the
	 zero/nonzero value to integer registers, where we can then
	 use a normal cmov, or vice-versa.  */

      switch (code)
	{
	case EQ: case LE: case LT: case LEU: case LTU:
	case UNORDERED:
	  /* We have these compares.  */
	  cmp_code = code, code = NE;
	  break;

	case NE:
	case ORDERED:
	  /* These must be reversed.  */
	  cmp_code = reverse_condition (code), code = EQ;
	  break;

	case GE: case GT: case GEU: case GTU:
	  /* These normally need swapping, but for integer zero we have
	     special patterns that recognize swapped operands.  */
	  if (cmp_mode == DImode && op1 == const0_rtx)
	    cmp_code = code, code = NE;
	  else
	    {
	      cmp_code = swap_condition (code);
	      code = NE;
	      tem = op0, op0 = op1, op1 = tem;
	    }
	  break;

	default:
	  gcc_unreachable ();
	}

      if (cmp_mode == DImode)
	{
	  if (!reg_or_0_operand (op0, DImode))
	    op0 = force_reg (DImode, op0);
	  if (!reg_or_8bit_operand (op1, DImode))
	    op1 = force_reg (DImode, op1);
	}

      tem = gen_reg_rtx (cmp_mode);
      emit_insn (gen_rtx_SET (VOIDmode, tem,
			      gen_rtx_fmt_ee (cmp_code, cmp_mode,
					      op0, op1)));

      cmp_mode = cmp_mode == DImode ? DFmode : DImode;
      op0 = gen_lowpart (cmp_mode, tem);
      op1 = CONST0_RTX (cmp_mode);
      cmp = gen_rtx_fmt_ee (code, VOIDmode, op0, op1);
      local_fast_math = 1;
    }

  if (cmp_mode == DImode)
    {
      if (!reg_or_0_operand (op0, DImode))
	op0 = force_reg (DImode, op0);
      if (!reg_or_8bit_operand (op1, DImode))
	op1 = force_reg (DImode, op1);
    }

  /* We may be able to use a conditional move directly.
     This avoids emitting spurious compares.  */
  if (signed_comparison_operator (cmp, VOIDmode)
      && (cmp_mode == DImode || local_fast_math)
      && (op0 == CONST0_RTX (cmp_mode) || op1 == CONST0_RTX (cmp_mode)))
    return gen_rtx_fmt_ee (code, VOIDmode, op0, op1);

  /* We can't put the comparison inside the conditional move;
     emit a compare instruction and put that inside the
     conditional move.  Make sure we emit only comparisons we have;
     swap or reverse as necessary.  */

  if (!can_create_pseudo_p ())
    return NULL_RTX;

  switch (code)
    {
    case EQ:  case LE:  case LT:  case LEU:  case LTU:
    case UNORDERED:
      /* We have these compares: */
      break;

    case NE:
    case ORDERED:
      /* These must be reversed.  */
      code = reverse_condition (code);
      cmov_code = EQ;
      break;

    case GE:  case GT:  case GEU:  case GTU:
      /* These normally need swapping, but for integer zero we have
	 special patterns that recognize swapped operands.  */
      if (cmp_mode == DImode && op1 == const0_rtx)
	break;
      code = swap_condition (code);
      tem = op0, op0 = op1, op1 = tem;
      break;

    default:
      gcc_unreachable ();
    }

  if (cmp_mode == DImode)
    {
      if (!reg_or_0_operand (op0, DImode))
	op0 = force_reg (DImode, op0);
      if (!reg_or_8bit_operand (op1, DImode))
	op1 = force_reg (DImode, op1);
    }

  /* ??? We mark the branch mode to be CCmode to prevent the compare
     and cmov from being combined, since the compare insn follows IEEE
     rules that the cmov does not.  */
  if (cmp_mode == DFmode && !local_fast_math)
    cmov_mode = CCmode;

  tem = gen_reg_rtx (cmp_mode);
  emit_move_insn (tem, gen_rtx_fmt_ee (code, cmp_mode, op0, op1));
  return gen_rtx_fmt_ee (cmov_code, cmov_mode, tem, CONST0_RTX (cmp_mode));
}

/* Simplify a conditional move of two constants into a setcc with
   arithmetic.  This is done with a splitter since combine would
   just undo the work if done during code generation.  It also catches
   cases we wouldn't have before cse.  */

int
alpha_split_conditional_move (enum rtx_code code, rtx dest, rtx cond,
			      rtx t_rtx, rtx f_rtx)
{
  HOST_WIDE_INT t, f, diff;
  enum machine_mode mode;
  rtx target, subtarget, tmp;

  mode = GET_MODE (dest);
  t = INTVAL (t_rtx);
  f = INTVAL (f_rtx);
  diff = t - f;

  if (((code == NE || code == EQ) && diff < 0)
      || (code == GE || code == GT))
    {
      code = reverse_condition (code);
      diff = t, t = f, f = diff;
      diff = t - f;
    }

  subtarget = target = dest;
  if (mode != DImode)
    {
      target = gen_lowpart (DImode, dest);
      if (can_create_pseudo_p ())
        subtarget = gen_reg_rtx (DImode);
      else
	subtarget = target;
    }
  /* Below, we must be careful to use copy_rtx on target and subtarget
     in intermediate insns, as they may be a subreg rtx, which may not
     be shared.  */

  if (f == 0 && exact_log2 (diff) > 0
      /* On EV6, we've got enough shifters to make non-arithmetic shifts
	 viable over a longer latency cmove.  On EV5, the E0 slot is a
	 scarce resource, and on EV4 shift has the same latency as a cmove.  */
      && (diff <= 8 || alpha_tune == PROCESSOR_EV6))
    {
      tmp = gen_rtx_fmt_ee (code, DImode, cond, const0_rtx);
      emit_insn (gen_rtx_SET (VOIDmode, copy_rtx (subtarget), tmp));

      tmp = gen_rtx_ASHIFT (DImode, copy_rtx (subtarget),
			    GEN_INT (exact_log2 (t)));
      emit_insn (gen_rtx_SET (VOIDmode, target, tmp));
    }
  else if (f == 0 && t == -1)
    {
      tmp = gen_rtx_fmt_ee (code, DImode, cond, const0_rtx);
      emit_insn (gen_rtx_SET (VOIDmode, copy_rtx (subtarget), tmp));

      emit_insn (gen_negdi2 (target, copy_rtx (subtarget)));
    }
  else if (diff == 1 || diff == 4 || diff == 8)
    {
      rtx add_op;

      tmp = gen_rtx_fmt_ee (code, DImode, cond, const0_rtx);
      emit_insn (gen_rtx_SET (VOIDmode, copy_rtx (subtarget), tmp));

      if (diff == 1)
	emit_insn (gen_adddi3 (target, copy_rtx (subtarget), GEN_INT (f)));
      else
	{
	  add_op = GEN_INT (f);
	  if (sext_add_operand (add_op, mode))
	    {
	      tmp = gen_rtx_MULT (DImode, copy_rtx (subtarget),
				  GEN_INT (diff));
	      tmp = gen_rtx_PLUS (DImode, tmp, add_op);
	      emit_insn (gen_rtx_SET (VOIDmode, target, tmp));
	    }
	  else
	    return 0;
	}
    }
  else
    return 0;

  return 1;
}

/* Look up the function X_floating library function name for the
   given operation.  */

struct GTY(()) xfloating_op
{
  const enum rtx_code code;
  const char *const GTY((skip)) osf_func;
  const char *const GTY((skip)) vms_func;
  rtx libcall;
};

static GTY(()) struct xfloating_op xfloating_ops[] =
{
  { PLUS,		"_OtsAddX", "OTS$ADD_X", 0 },
  { MINUS,		"_OtsSubX", "OTS$SUB_X", 0 },
  { MULT,		"_OtsMulX", "OTS$MUL_X", 0 },
  { DIV,		"_OtsDivX", "OTS$DIV_X", 0 },
  { EQ,			"_OtsEqlX", "OTS$EQL_X", 0 },
  { NE,			"_OtsNeqX", "OTS$NEQ_X", 0 },
  { LT,			"_OtsLssX", "OTS$LSS_X", 0 },
  { LE,			"_OtsLeqX", "OTS$LEQ_X", 0 },
  { GT,			"_OtsGtrX", "OTS$GTR_X", 0 },
  { GE,			"_OtsGeqX", "OTS$GEQ_X", 0 },
  { FIX,		"_OtsCvtXQ", "OTS$CVTXQ", 0 },
  { FLOAT,		"_OtsCvtQX", "OTS$CVTQX", 0 },
  { UNSIGNED_FLOAT,	"_OtsCvtQUX", "OTS$CVTQUX", 0 },
  { FLOAT_EXTEND,	"_OtsConvertFloatTX", "OTS$CVT_FLOAT_T_X", 0 },
  { FLOAT_TRUNCATE,	"_OtsConvertFloatXT", "OTS$CVT_FLOAT_X_T", 0 }
};

static GTY(()) struct xfloating_op vax_cvt_ops[] =
{
  { FLOAT_EXTEND,	"_OtsConvertFloatGX", "OTS$CVT_FLOAT_G_X", 0 },
  { FLOAT_TRUNCATE,	"_OtsConvertFloatXG", "OTS$CVT_FLOAT_X_G", 0 }
};

static rtx
alpha_lookup_xfloating_lib_func (enum rtx_code code)
{
  struct xfloating_op *ops = xfloating_ops;
  long n = ARRAY_SIZE (xfloating_ops);
  long i;

  gcc_assert (TARGET_HAS_XFLOATING_LIBS);

  /* How irritating.  Nothing to key off for the main table.  */
  if (TARGET_FLOAT_VAX && (code == FLOAT_EXTEND || code == FLOAT_TRUNCATE))
    {
      ops = vax_cvt_ops;
      n = ARRAY_SIZE (vax_cvt_ops);
    }

  for (i = 0; i < n; ++i, ++ops)
    if (ops->code == code)
      {
	rtx func = ops->libcall;
	if (!func)
	  {
	    func = init_one_libfunc (TARGET_ABI_OPEN_VMS
				     ? ops->vms_func : ops->osf_func);
	    ops->libcall = func;
	  }
        return func;
      }

  gcc_unreachable ();
}

/* Most X_floating operations take the rounding mode as an argument.
   Compute that here.  */

static int
alpha_compute_xfloating_mode_arg (enum rtx_code code,
				  enum alpha_fp_rounding_mode round)
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
      gcc_unreachable ();

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

   FUNC is the function to call.
   TARGET is where the output belongs.
   OPERANDS are the inputs.
   NOPERANDS is the count of inputs.
   EQUIV is the expression equivalent for the function.
*/

static void
alpha_emit_xfloating_libcall (rtx func, rtx target, rtx operands[],
			      int noperands, rtx equiv)
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
	  gcc_assert (CONST_INT_P (operands[i]));
	  /* FALLTHRU */
	case DImode:
	  reg = gen_rtx_REG (DImode, regno);
	  regno += 1;
	  break;

	default:
	  gcc_unreachable ();
	}

      emit_move_insn (reg, operands[i]);
      use_reg (&usage, reg);
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
      gcc_unreachable ();
    }

  tmp = gen_rtx_MEM (QImode, func);
  tmp = emit_call_insn (GEN_CALL_VALUE (reg, tmp, const0_rtx,
					const0_rtx, const0_rtx));
  CALL_INSN_FUNCTION_USAGE (tmp) = usage;
  RTL_CONST_CALL_P (tmp) = 1;

  tmp = get_insns ();
  end_sequence ();

  emit_libcall_block (tmp, target, reg, equiv);
}

/* Emit an X_floating library function call for arithmetic (+,-,*,/).  */

void
alpha_emit_xfloating_arith (enum rtx_code code, rtx operands[])
{
  rtx func;
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
alpha_emit_xfloating_compare (enum rtx_code *pcode, rtx op0, rtx op1)
{
  enum rtx_code cmp_code, res_code;
  rtx func, out, operands[2], note;

  /* X_floating library comparison functions return
	   -1  unordered
	    0  false
	    1  true
     Convert the compare against the raw return value.  */

  cmp_code = *pcode;
  switch (cmp_code)
    {
    case UNORDERED:
      cmp_code = EQ;
      res_code = LT;
      break;
    case ORDERED:
      cmp_code = EQ;
      res_code = GE;
      break;
    case NE:
      res_code = NE;
      break;
    case EQ:
    case LT:
    case GT:
    case LE:
    case GE:
      res_code = GT;
      break;
    default:
      gcc_unreachable ();
    }
  *pcode = res_code;

  func = alpha_lookup_xfloating_lib_func (cmp_code);

  operands[0] = op0;
  operands[1] = op1;
  out = gen_reg_rtx (DImode);

  /* What's actually returned is -1,0,1, not a proper boolean value.  */
  note = gen_rtx_fmt_ee (cmp_code, VOIDmode, op0, op1);
  note = gen_rtx_UNSPEC (DImode, gen_rtvec (1, note), UNSPEC_XFLT_COMPARE);
  alpha_emit_xfloating_libcall (func, out, operands, 2, note);

  return out;
}

/* Emit an X_floating library function call for a conversion.  */

void
alpha_emit_xfloating_cvt (enum rtx_code orig_code, rtx operands[])
{
  int noperands = 1, mode;
  rtx out_operands[2];
  rtx func;
  enum rtx_code code = orig_code;

  if (code == UNSIGNED_FIX)
    code = FIX;

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
				gen_rtx_fmt_e (orig_code,
					       GET_MODE (operands[0]),
					       operands[1]));
}

/* Split a TImode or TFmode move from OP[1] to OP[0] into a pair of
   DImode moves from OP[2,3] to OP[0,1].  If FIXUP_OVERLAP is true,
   guarantee that the sequence
     set (OP[0] OP[2])
     set (OP[1] OP[3])
   is valid.  Naturally, output operand ordering is little-endian.
   This is used by *movtf_internal and *movti_internal.  */
  
void
alpha_split_tmode_pair (rtx operands[4], enum machine_mode mode,
			bool fixup_overlap)
{
  switch (GET_CODE (operands[1]))
    {
    case REG:
      operands[3] = gen_rtx_REG (DImode, REGNO (operands[1]) + 1);
      operands[2] = gen_rtx_REG (DImode, REGNO (operands[1]));
      break;

    case MEM:
      operands[3] = adjust_address (operands[1], DImode, 8);
      operands[2] = adjust_address (operands[1], DImode, 0);
      break;

    case CONST_INT:
    case CONST_DOUBLE:
      gcc_assert (operands[1] == CONST0_RTX (mode));
      operands[2] = operands[3] = const0_rtx;
      break;

    default:
      gcc_unreachable ();
    }

  switch (GET_CODE (operands[0]))
    {
    case REG:
      operands[1] = gen_rtx_REG (DImode, REGNO (operands[0]) + 1);
      operands[0] = gen_rtx_REG (DImode, REGNO (operands[0]));
      break;

    case MEM:
      operands[1] = adjust_address (operands[0], DImode, 8);
      operands[0] = adjust_address (operands[0], DImode, 0);
      break;

    default:
      gcc_unreachable ();
    }

  if (fixup_overlap && reg_overlap_mentioned_p (operands[0], operands[3]))
    {
      rtx tmp;
      tmp = operands[0], operands[0] = operands[1], operands[1] = tmp;
      tmp = operands[2], operands[2] = operands[3], operands[3] = tmp;
    }
}

/* Implement negtf2 or abstf2.  Op0 is destination, op1 is source,
   op2 is a register containing the sign bit, operation is the
   logical operation to be performed.  */

void
alpha_split_tfmode_frobsign (rtx operands[3], rtx (*operation) (rtx, rtx, rtx))
{
  rtx high_bit = operands[2];
  rtx scratch;
  int move;

  alpha_split_tmode_pair (operands, TFmode, false);

  /* Detect three flavors of operand overlap.  */
  move = 1;
  if (rtx_equal_p (operands[0], operands[2]))
    move = 0;
  else if (rtx_equal_p (operands[1], operands[2]))
    {
      if (rtx_equal_p (operands[0], high_bit))
	move = 2;
      else
	move = -1;
    }

  if (move < 0)
    emit_move_insn (operands[0], operands[2]);

  /* ??? If the destination overlaps both source tf and high_bit, then
     assume source tf is dead in its entirety and use the other half
     for a scratch register.  Otherwise "scratch" is just the proper
     destination register.  */
  scratch = operands[move < 2 ? 1 : 3];

  emit_insn ((*operation) (scratch, high_bit, operands[3]));

  if (move > 0)
    {
      emit_move_insn (operands[0], operands[2]);
      if (move > 1)
	emit_move_insn (operands[1], scratch);
    }
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
alpha_expand_unaligned_load (rtx tgt, rtx mem, HOST_WIDE_INT size,
			     HOST_WIDE_INT ofs, int sign)
{
  rtx meml, memh, addr, extl, exth, tmp, mema;
  enum machine_mode mode;

  if (TARGET_BWX && size == 2)
    {
      meml = adjust_address (mem, QImode, ofs);
      memh = adjust_address (mem, QImode, ofs+1);
      extl = gen_reg_rtx (DImode);
      exth = gen_reg_rtx (DImode);
      emit_insn (gen_zero_extendqidi2 (extl, meml));
      emit_insn (gen_zero_extendqidi2 (exth, memh));
      exth = expand_simple_binop (DImode, ASHIFT, exth, GEN_INT (8),
				  NULL, 1, OPTAB_LIB_WIDEN);
      addr = expand_simple_binop (DImode, IOR, extl, exth,
				  NULL, 1, OPTAB_LIB_WIDEN);

      if (sign && GET_MODE (tgt) != HImode)
	{
	  addr = gen_lowpart (HImode, addr);
	  emit_insn (gen_extend_insn (tgt, addr, GET_MODE (tgt), HImode, 0));
	}
      else
	{
	  if (GET_MODE (tgt) != DImode)
	    addr = gen_lowpart (GET_MODE (tgt), addr);
	  emit_move_insn (tgt, addr);
	}
      return;
    }

  meml = gen_reg_rtx (DImode);
  memh = gen_reg_rtx (DImode);
  addr = gen_reg_rtx (DImode);
  extl = gen_reg_rtx (DImode);
  exth = gen_reg_rtx (DImode);

  mema = XEXP (mem, 0);
  if (GET_CODE (mema) == LO_SUM)
    mema = force_reg (Pmode, mema);

  /* AND addresses cannot be in any alias set, since they may implicitly
     alias surrounding code.  Ideally we'd have some alias set that
     covered all types except those with alignment 8 or higher.  */

  tmp = change_address (mem, DImode,
			gen_rtx_AND (DImode,
				     plus_constant (DImode, mema, ofs),
				     GEN_INT (-8)));
  set_mem_alias_set (tmp, 0);
  emit_move_insn (meml, tmp);

  tmp = change_address (mem, DImode,
			gen_rtx_AND (DImode,
				     plus_constant (DImode, mema,
						    ofs + size - 1),
				     GEN_INT (-8)));
  set_mem_alias_set (tmp, 0);
  emit_move_insn (memh, tmp);

  if (sign && size == 2)
    {
      emit_move_insn (addr, plus_constant (Pmode, mema, ofs+2));

      emit_insn (gen_extql (extl, meml, addr));
      emit_insn (gen_extqh (exth, memh, addr));

      /* We must use tgt here for the target.  Alpha-vms port fails if we use
	 addr for the target, because addr is marked as a pointer and combine
	 knows that pointers are always sign-extended 32-bit values.  */
      addr = expand_binop (DImode, ior_optab, extl, exth, tgt, 1, OPTAB_WIDEN);
      addr = expand_binop (DImode, ashr_optab, addr, GEN_INT (48),
			   addr, 1, OPTAB_WIDEN);
    }
  else
    {
      emit_move_insn (addr, plus_constant (Pmode, mema, ofs));
      emit_insn (gen_extxl (extl, meml, GEN_INT (size*8), addr));
      switch ((int) size)
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
	  gcc_unreachable ();
	}

      addr = expand_binop (mode, ior_optab, gen_lowpart (mode, extl),
			   gen_lowpart (mode, exth), gen_lowpart (mode, tgt),
			   sign, OPTAB_WIDEN);
    }

  if (addr != tgt)
    emit_move_insn (tgt, gen_lowpart (GET_MODE (tgt), addr));
}

/* Similarly, use ins and msk instructions to perform unaligned stores.  */

void
alpha_expand_unaligned_store (rtx dst, rtx src,
			      HOST_WIDE_INT size, HOST_WIDE_INT ofs)
{
  rtx dstl, dsth, addr, insl, insh, meml, memh, dsta;

  if (TARGET_BWX && size == 2)
    {
      if (src != const0_rtx)
	{
	  dstl = gen_lowpart (QImode, src);
	  dsth = expand_simple_binop (DImode, LSHIFTRT, src, GEN_INT (8),
				      NULL, 1, OPTAB_LIB_WIDEN);
	  dsth = gen_lowpart (QImode, dsth);
	}
      else
	dstl = dsth = const0_rtx;

      meml = adjust_address (dst, QImode, ofs);
      memh = adjust_address (dst, QImode, ofs+1);

      emit_move_insn (meml, dstl);
      emit_move_insn (memh, dsth);
      return;
    }

  dstl = gen_reg_rtx (DImode);
  dsth = gen_reg_rtx (DImode);
  insl = gen_reg_rtx (DImode);
  insh = gen_reg_rtx (DImode);

  dsta = XEXP (dst, 0);
  if (GET_CODE (dsta) == LO_SUM)
    dsta = force_reg (Pmode, dsta);

  /* AND addresses cannot be in any alias set, since they may implicitly
     alias surrounding code.  Ideally we'd have some alias set that
     covered all types except those with alignment 8 or higher.  */

  meml = change_address (dst, DImode,
			 gen_rtx_AND (DImode,
				      plus_constant (DImode, dsta, ofs),
				      GEN_INT (-8)));
  set_mem_alias_set (meml, 0);

  memh = change_address (dst, DImode,
			 gen_rtx_AND (DImode,
				      plus_constant (DImode, dsta,
						     ofs + size - 1),
				      GEN_INT (-8)));
  set_mem_alias_set (memh, 0);

  emit_move_insn (dsth, memh);
  emit_move_insn (dstl, meml);

  addr = copy_addr_to_reg (plus_constant (Pmode, dsta, ofs));

  if (src != CONST0_RTX (GET_MODE (src)))
    {
      emit_insn (gen_insxh (insh, gen_lowpart (DImode, src),
			    GEN_INT (size*8), addr));

      switch ((int) size)
	{
	case 2:
	  emit_insn (gen_inswl (insl, gen_lowpart (HImode, src), addr));
	  break;
	case 4:
	  emit_insn (gen_insll (insl, gen_lowpart (SImode, src), addr));
	  break;
	case 8:
	  emit_insn (gen_insql (insl, gen_lowpart (DImode, src), addr));
	  break;
	default:
	  gcc_unreachable ();
	}
    }

  emit_insn (gen_mskxh (dsth, dsth, GEN_INT (size*8), addr));

  switch ((int) size)
    {
    case 2:
      emit_insn (gen_mskwl (dstl, dstl, addr));
      break;
    case 4:
      emit_insn (gen_mskll (dstl, dstl, addr));
      break;
    case 8:
      emit_insn (gen_mskql (dstl, dstl, addr));
      break;
    default:
      gcc_unreachable ();
    }

  if (src != CONST0_RTX (GET_MODE (src)))
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
alpha_expand_unaligned_load_words (rtx *out_regs, rtx smem,
				   HOST_WIDE_INT words, HOST_WIDE_INT ofs)
{
  rtx const im8 = GEN_INT (-8);
  rtx ext_tmps[MAX_MOVE_WORDS], data_regs[MAX_MOVE_WORDS+1];
  rtx sreg, areg, tmp, smema;
  HOST_WIDE_INT i;

  smema = XEXP (smem, 0);
  if (GET_CODE (smema) == LO_SUM)
    smema = force_reg (Pmode, smema);

  /* Generate all the tmp registers we need.  */
  for (i = 0; i < words; ++i)
    {
      data_regs[i] = out_regs[i];
      ext_tmps[i] = gen_reg_rtx (DImode);
    }
  data_regs[words] = gen_reg_rtx (DImode);

  if (ofs != 0)
    smem = adjust_address (smem, GET_MODE (smem), ofs);

  /* Load up all of the source data.  */
  for (i = 0; i < words; ++i)
    {
      tmp = change_address (smem, DImode,
			    gen_rtx_AND (DImode,
					 plus_constant (DImode, smema, 8*i),
					 im8));
      set_mem_alias_set (tmp, 0);
      emit_move_insn (data_regs[i], tmp);
    }

  tmp = change_address (smem, DImode,
			gen_rtx_AND (DImode,
				     plus_constant (DImode, smema,
						    8*words - 1),
				     im8));
  set_mem_alias_set (tmp, 0);
  emit_move_insn (data_regs[words], tmp);

  /* Extract the half-word fragments.  Unfortunately DEC decided to make
     extxh with offset zero a noop instead of zeroing the register, so
     we must take care of that edge condition ourselves with cmov.  */

  sreg = copy_addr_to_reg (smema);
  areg = expand_binop (DImode, and_optab, sreg, GEN_INT (7), NULL,
		       1, OPTAB_WIDEN);
  for (i = 0; i < words; ++i)
    {
      emit_insn (gen_extql (data_regs[i], data_regs[i], sreg));
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
alpha_expand_unaligned_store_words (rtx *data_regs, rtx dmem,
				    HOST_WIDE_INT words, HOST_WIDE_INT ofs)
{
  rtx const im8 = GEN_INT (-8);
  rtx ins_tmps[MAX_MOVE_WORDS];
  rtx st_tmp_1, st_tmp_2, dreg;
  rtx st_addr_1, st_addr_2, dmema;
  HOST_WIDE_INT i;

  dmema = XEXP (dmem, 0);
  if (GET_CODE (dmema) == LO_SUM)
    dmema = force_reg (Pmode, dmema);

  /* Generate all the tmp registers we need.  */
  if (data_regs != NULL)
    for (i = 0; i < words; ++i)
      ins_tmps[i] = gen_reg_rtx(DImode);
  st_tmp_1 = gen_reg_rtx(DImode);
  st_tmp_2 = gen_reg_rtx(DImode);

  if (ofs != 0)
    dmem = adjust_address (dmem, GET_MODE (dmem), ofs);

  st_addr_2 = change_address (dmem, DImode,
			      gen_rtx_AND (DImode,
					   plus_constant (DImode, dmema,
							  words*8 - 1),
					   im8));
  set_mem_alias_set (st_addr_2, 0);

  st_addr_1 = change_address (dmem, DImode,
			      gen_rtx_AND (DImode, dmema, im8));
  set_mem_alias_set (st_addr_1, 0);

  /* Load up the destination end bits.  */
  emit_move_insn (st_tmp_2, st_addr_2);
  emit_move_insn (st_tmp_1, st_addr_1);

  /* Shift the input data into place.  */
  dreg = copy_addr_to_reg (dmema);
  if (data_regs != NULL)
    {
      for (i = words-1; i >= 0; --i)
	{
	  emit_insn (gen_insqh (ins_tmps[i], data_regs[i], dreg));
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
  emit_insn (gen_mskqh (st_tmp_2, st_tmp_2, dreg));
  emit_insn (gen_mskql (st_tmp_1, st_tmp_1, dreg));

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
      rtx tmp = change_address (dmem, DImode,
				gen_rtx_AND (DImode,
					     plus_constant (DImode,
							    dmema, i*8),
					     im8));
      set_mem_alias_set (tmp, 0);
      emit_move_insn (tmp, data_regs ? ins_tmps[i-1] : const0_rtx);
    }
  emit_move_insn (st_addr_1, st_tmp_1);
}


/* Expand string/block move operations.

   operands[0] is the pointer to the destination.
   operands[1] is the pointer to the source.
   operands[2] is the number of bytes to move.
   operands[3] is the alignment.  */

int
alpha_expand_block_move (rtx operands[])
{
  rtx bytes_rtx	= operands[2];
  rtx align_rtx = operands[3];
  HOST_WIDE_INT orig_bytes = INTVAL (bytes_rtx);
  HOST_WIDE_INT bytes = orig_bytes;
  HOST_WIDE_INT src_align = INTVAL (align_rtx) * BITS_PER_UNIT;
  HOST_WIDE_INT dst_align = src_align;
  rtx orig_src = operands[1];
  rtx orig_dst = operands[0];
  rtx data_regs[2 * MAX_MOVE_WORDS + 16];
  rtx tmp;
  unsigned int i, words, ofs, nregs = 0;

  if (orig_bytes <= 0)
    return 1;
  else if (orig_bytes > MAX_MOVE_WORDS * UNITS_PER_WORD)
    return 0;

  /* Look for additional alignment information from recorded register info.  */

  tmp = XEXP (orig_src, 0);
  if (REG_P (tmp))
    src_align = MAX (src_align, REGNO_POINTER_ALIGN (REGNO (tmp)));
  else if (GET_CODE (tmp) == PLUS
	   && REG_P (XEXP (tmp, 0))
	   && CONST_INT_P (XEXP (tmp, 1)))
    {
      unsigned HOST_WIDE_INT c = INTVAL (XEXP (tmp, 1));
      unsigned int a = REGNO_POINTER_ALIGN (REGNO (XEXP (tmp, 0)));

      if (a > src_align)
	{
          if (a >= 64 && c % 8 == 0)
	    src_align = 64;
          else if (a >= 32 && c % 4 == 0)
	    src_align = 32;
          else if (a >= 16 && c % 2 == 0)
	    src_align = 16;
	}
    }

  tmp = XEXP (orig_dst, 0);
  if (REG_P (tmp))
    dst_align = MAX (dst_align, REGNO_POINTER_ALIGN (REGNO (tmp)));
  else if (GET_CODE (tmp) == PLUS
	   && REG_P (XEXP (tmp, 0))
	   && CONST_INT_P (XEXP (tmp, 1)))
    {
      unsigned HOST_WIDE_INT c = INTVAL (XEXP (tmp, 1));
      unsigned int a = REGNO_POINTER_ALIGN (REGNO (XEXP (tmp, 0)));

      if (a > dst_align)
	{
          if (a >= 64 && c % 8 == 0)
	    dst_align = 64;
          else if (a >= 32 && c % 4 == 0)
	    dst_align = 32;
          else if (a >= 16 && c % 2 == 0)
	    dst_align = 16;
	}
    }

  ofs = 0;
  if (src_align >= 64 && bytes >= 8)
    {
      words = bytes / 8;

      for (i = 0; i < words; ++i)
	data_regs[nregs + i] = gen_reg_rtx (DImode);

      for (i = 0; i < words; ++i)
	emit_move_insn (data_regs[nregs + i],
			adjust_address (orig_src, DImode, ofs + i * 8));

      nregs += words;
      bytes -= words * 8;
      ofs += words * 8;
    }

  if (src_align >= 32 && bytes >= 4)
    {
      words = bytes / 4;

      for (i = 0; i < words; ++i)
	data_regs[nregs + i] = gen_reg_rtx (SImode);

      for (i = 0; i < words; ++i)
	emit_move_insn (data_regs[nregs + i],
			adjust_address (orig_src, SImode, ofs + i * 4));

      nregs += words;
      bytes -= words * 4;
      ofs += words * 4;
    }

  if (bytes >= 8)
    {
      words = bytes / 8;

      for (i = 0; i < words+1; ++i)
	data_regs[nregs + i] = gen_reg_rtx (DImode);

      alpha_expand_unaligned_load_words (data_regs + nregs, orig_src,
					 words, ofs);

      nregs += words;
      bytes -= words * 8;
      ofs += words * 8;
    }

  if (! TARGET_BWX && bytes >= 4)
    {
      data_regs[nregs++] = tmp = gen_reg_rtx (SImode);
      alpha_expand_unaligned_load (tmp, orig_src, 4, ofs, 0);
      bytes -= 4;
      ofs += 4;
    }

  if (bytes >= 2)
    {
      if (src_align >= 16)
	{
	  do {
	    data_regs[nregs++] = tmp = gen_reg_rtx (HImode);
	    emit_move_insn (tmp, adjust_address (orig_src, HImode, ofs));
	    bytes -= 2;
	    ofs += 2;
	  } while (bytes >= 2);
	}
      else if (! TARGET_BWX)
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
      emit_move_insn (tmp, adjust_address (orig_src, QImode, ofs));
      bytes -= 1;
      ofs += 1;
    }

  gcc_assert (nregs <= ARRAY_SIZE (data_regs));

  /* Now save it back out again.  */

  i = 0, ofs = 0;

  /* Write out the data in whatever chunks reading the source allowed.  */
  if (dst_align >= 64)
    {
      while (i < nregs && GET_MODE (data_regs[i]) == DImode)
	{
	  emit_move_insn (adjust_address (orig_dst, DImode, ofs),
			  data_regs[i]);
	  ofs += 8;
	  i++;
	}
    }

  if (dst_align >= 32)
    {
      /* If the source has remaining DImode regs, write them out in
	 two pieces.  */
      while (i < nregs && GET_MODE (data_regs[i]) == DImode)
	{
	  tmp = expand_binop (DImode, lshr_optab, data_regs[i], GEN_INT (32),
			      NULL_RTX, 1, OPTAB_WIDEN);

	  emit_move_insn (adjust_address (orig_dst, SImode, ofs),
			  gen_lowpart (SImode, data_regs[i]));
	  emit_move_insn (adjust_address (orig_dst, SImode, ofs + 4),
			  gen_lowpart (SImode, tmp));
	  ofs += 8;
	  i++;
	}

      while (i < nregs && GET_MODE (data_regs[i]) == SImode)
	{
	  emit_move_insn (adjust_address (orig_dst, SImode, ofs),
			  data_regs[i]);
	  ofs += 4;
	  i++;
	}
    }

  if (i < nregs && GET_MODE (data_regs[i]) == DImode)
    {
      /* Write out a remaining block of words using unaligned methods.  */

      for (words = 1; i + words < nregs; words++)
	if (GET_MODE (data_regs[i + words]) != DImode)
	  break;

      if (words == 1)
	alpha_expand_unaligned_store (orig_dst, data_regs[i], 8, ofs);
      else
        alpha_expand_unaligned_store_words (data_regs + i, orig_dst,
					    words, ofs);

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

  if (dst_align >= 16)
    while (i < nregs && GET_MODE (data_regs[i]) == HImode)
      {
	emit_move_insn (adjust_address (orig_dst, HImode, ofs), data_regs[i]);
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

  /* The remainder must be byte copies.  */
  while (i < nregs)
    {
      gcc_assert (GET_MODE (data_regs[i]) == QImode);
      emit_move_insn (adjust_address (orig_dst, QImode, ofs), data_regs[i]);
      i++;
      ofs += 1;
    }

  return 1;
}

int
alpha_expand_block_clear (rtx operands[])
{
  rtx bytes_rtx	= operands[1];
  rtx align_rtx = operands[3];
  HOST_WIDE_INT orig_bytes = INTVAL (bytes_rtx);
  HOST_WIDE_INT bytes = orig_bytes;
  HOST_WIDE_INT align = INTVAL (align_rtx) * BITS_PER_UNIT;
  HOST_WIDE_INT alignofs = 0;
  rtx orig_dst = operands[0];
  rtx tmp;
  int i, words, ofs = 0;

  if (orig_bytes <= 0)
    return 1;
  if (orig_bytes > MAX_MOVE_WORDS * UNITS_PER_WORD)
    return 0;

  /* Look for stricter alignment.  */
  tmp = XEXP (orig_dst, 0);
  if (REG_P (tmp))
    align = MAX (align, REGNO_POINTER_ALIGN (REGNO (tmp)));
  else if (GET_CODE (tmp) == PLUS
	   && REG_P (XEXP (tmp, 0))
	   && CONST_INT_P (XEXP (tmp, 1)))
    {
      HOST_WIDE_INT c = INTVAL (XEXP (tmp, 1));
      int a = REGNO_POINTER_ALIGN (REGNO (XEXP (tmp, 0)));

      if (a > align)
	{
          if (a >= 64)
	    align = a, alignofs = 8 - c % 8;
          else if (a >= 32)
	    align = a, alignofs = 4 - c % 4;
          else if (a >= 16)
	    align = a, alignofs = 2 - c % 2;
	}
    }

  /* Handle an unaligned prefix first.  */

  if (alignofs > 0)
    {
#if HOST_BITS_PER_WIDE_INT >= 64
      /* Given that alignofs is bounded by align, the only time BWX could
	 generate three stores is for a 7 byte fill.  Prefer two individual
	 stores over a load/mask/store sequence.  */
      if ((!TARGET_BWX || alignofs == 7)
	       && align >= 32
	       && !(alignofs == 4 && bytes >= 4))
	{
	  enum machine_mode mode = (align >= 64 ? DImode : SImode);
	  int inv_alignofs = (align >= 64 ? 8 : 4) - alignofs;
	  rtx mem, tmp;
	  HOST_WIDE_INT mask;

	  mem = adjust_address (orig_dst, mode, ofs - inv_alignofs);
	  set_mem_alias_set (mem, 0);

	  mask = ~(~(HOST_WIDE_INT)0 << (inv_alignofs * 8));
	  if (bytes < alignofs)
	    {
	      mask |= ~(HOST_WIDE_INT)0 << ((inv_alignofs + bytes) * 8);
	      ofs += bytes;
	      bytes = 0;
	    }
	  else
	    {
	      bytes -= alignofs;
	      ofs += alignofs;
	    }
	  alignofs = 0;

	  tmp = expand_binop (mode, and_optab, mem, GEN_INT (mask),
			      NULL_RTX, 1, OPTAB_WIDEN);

	  emit_move_insn (mem, tmp);
	}
#endif

      if (TARGET_BWX && (alignofs & 1) && bytes >= 1)
	{
	  emit_move_insn (adjust_address (orig_dst, QImode, ofs), const0_rtx);
	  bytes -= 1;
	  ofs += 1;
	  alignofs -= 1;
	}
      if (TARGET_BWX && align >= 16 && (alignofs & 3) == 2 && bytes >= 2)
	{
	  emit_move_insn (adjust_address (orig_dst, HImode, ofs), const0_rtx);
	  bytes -= 2;
	  ofs += 2;
	  alignofs -= 2;
	}
      if (alignofs == 4 && bytes >= 4)
	{
	  emit_move_insn (adjust_address (orig_dst, SImode, ofs), const0_rtx);
	  bytes -= 4;
	  ofs += 4;
	  alignofs = 0;
	}

      /* If we've not used the extra lead alignment information by now,
	 we won't be able to.  Downgrade align to match what's left over.  */
      if (alignofs > 0)
	{
	  alignofs = alignofs & -alignofs;
	  align = MIN (align, alignofs * BITS_PER_UNIT);
	}
    }

  /* Handle a block of contiguous long-words.  */

  if (align >= 64 && bytes >= 8)
    {
      words = bytes / 8;

      for (i = 0; i < words; ++i)
	emit_move_insn (adjust_address (orig_dst, DImode, ofs + i * 8),
			const0_rtx);

      bytes -= words * 8;
      ofs += words * 8;
    }

  /* If the block is large and appropriately aligned, emit a single
     store followed by a sequence of stq_u insns.  */

  if (align >= 32 && bytes > 16)
    {
      rtx orig_dsta;

      emit_move_insn (adjust_address (orig_dst, SImode, ofs), const0_rtx);
      bytes -= 4;
      ofs += 4;

      orig_dsta = XEXP (orig_dst, 0);
      if (GET_CODE (orig_dsta) == LO_SUM)
	orig_dsta = force_reg (Pmode, orig_dsta);

      words = bytes / 8;
      for (i = 0; i < words; ++i)
	{
	  rtx mem
	    = change_address (orig_dst, DImode,
			      gen_rtx_AND (DImode,
					   plus_constant (DImode, orig_dsta,
							  ofs + i*8),
					   GEN_INT (-8)));
	  set_mem_alias_set (mem, 0);
	  emit_move_insn (mem, const0_rtx);
	}

      /* Depending on the alignment, the first stq_u may have overlapped
	 with the initial stl, which means that the last stq_u didn't
	 write as much as it would appear.  Leave those questionable bytes
	 unaccounted for.  */
      bytes -= words * 8 - 4;
      ofs += words * 8 - 4;
    }

  /* Handle a smaller block of aligned words.  */

  if ((align >= 64 && bytes == 4)
      || (align == 32 && bytes >= 4))
    {
      words = bytes / 4;

      for (i = 0; i < words; ++i)
	emit_move_insn (adjust_address (orig_dst, SImode, ofs + i * 4),
			const0_rtx);

      bytes -= words * 4;
      ofs += words * 4;
    }

  /* An unaligned block uses stq_u stores for as many as possible.  */

  if (bytes >= 8)
    {
      words = bytes / 8;

      alpha_expand_unaligned_store_words (NULL, orig_dst, words, ofs);

      bytes -= words * 8;
      ofs += words * 8;
    }

  /* Next clean up any trailing pieces.  */

#if HOST_BITS_PER_WIDE_INT >= 64
  /* Count the number of bits in BYTES for which aligned stores could
     be emitted.  */
  words = 0;
  for (i = (TARGET_BWX ? 1 : 4); i * BITS_PER_UNIT <= align ; i <<= 1)
    if (bytes & i)
      words += 1;

  /* If we have appropriate alignment (and it wouldn't take too many
     instructions otherwise), mask out the bytes we need.  */
  if (TARGET_BWX ? words > 2 : bytes > 0)
    {
      if (align >= 64)
	{
	  rtx mem, tmp;
	  HOST_WIDE_INT mask;

	  mem = adjust_address (orig_dst, DImode, ofs);
	  set_mem_alias_set (mem, 0);

	  mask = ~(HOST_WIDE_INT)0 << (bytes * 8);

	  tmp = expand_binop (DImode, and_optab, mem, GEN_INT (mask),
			      NULL_RTX, 1, OPTAB_WIDEN);

	  emit_move_insn (mem, tmp);
	  return 1;
	}
      else if (align >= 32 && bytes < 4)
	{
	  rtx mem, tmp;
	  HOST_WIDE_INT mask;

	  mem = adjust_address (orig_dst, SImode, ofs);
	  set_mem_alias_set (mem, 0);

	  mask = ~(HOST_WIDE_INT)0 << (bytes * 8);

	  tmp = expand_binop (SImode, and_optab, mem, GEN_INT (mask),
			      NULL_RTX, 1, OPTAB_WIDEN);

	  emit_move_insn (mem, tmp);
	  return 1;
	}
    }
#endif

  if (!TARGET_BWX && bytes >= 4)
    {
      alpha_expand_unaligned_store (orig_dst, const0_rtx, 4, ofs);
      bytes -= 4;
      ofs += 4;
    }

  if (bytes >= 2)
    {
      if (align >= 16)
	{
	  do {
	    emit_move_insn (adjust_address (orig_dst, HImode, ofs),
			    const0_rtx);
	    bytes -= 2;
	    ofs += 2;
	  } while (bytes >= 2);
	}
      else if (! TARGET_BWX)
	{
	  alpha_expand_unaligned_store (orig_dst, const0_rtx, 2, ofs);
	  bytes -= 2;
	  ofs += 2;
	}
    }

  while (bytes > 0)
    {
      emit_move_insn (adjust_address (orig_dst, QImode, ofs), const0_rtx);
      bytes -= 1;
      ofs += 1;
    }

  return 1;
}

/* Returns a mask so that zap(x, value) == x & mask.  */

rtx
alpha_expand_zap_mask (HOST_WIDE_INT value)
{
  rtx result;
  int i;

  if (HOST_BITS_PER_WIDE_INT >= 64)
    {
      HOST_WIDE_INT mask = 0;

      for (i = 7; i >= 0; --i)
	{
	  mask <<= 8;
	  if (!((value >> i) & 1))
	    mask |= 0xff;
	}

      result = gen_int_mode (mask, DImode);
    }
  else
    {
      HOST_WIDE_INT mask_lo = 0, mask_hi = 0;

      gcc_assert (HOST_BITS_PER_WIDE_INT == 32);
      
      for (i = 7; i >= 4; --i)
	{
	  mask_hi <<= 8;
	  if (!((value >> i) & 1))
	    mask_hi |= 0xff;
	}

      for (i = 3; i >= 0; --i)
	{
	  mask_lo <<= 8;
	  if (!((value >> i) & 1))
	    mask_lo |= 0xff;
	}

      result = immed_double_const (mask_lo, mask_hi, DImode);
    }

  return result;
}

void
alpha_expand_builtin_vector_binop (rtx (*gen) (rtx, rtx, rtx),
				   enum machine_mode mode,
				   rtx op0, rtx op1, rtx op2)
{
  op0 = gen_lowpart (mode, op0);

  if (op1 == const0_rtx)
    op1 = CONST0_RTX (mode);
  else
    op1 = gen_lowpart (mode, op1);

  if (op2 == const0_rtx)
    op2 = CONST0_RTX (mode);
  else
    op2 = gen_lowpart (mode, op2);

  emit_insn ((*gen) (op0, op1, op2));
}

/* A subroutine of the atomic operation splitters.  Jump to LABEL if
   COND is true.  Mark the jump as unlikely to be taken.  */

static void
emit_unlikely_jump (rtx cond, rtx label)
{
  int very_unlikely = REG_BR_PROB_BASE / 100 - 1;
  rtx x;

  x = gen_rtx_IF_THEN_ELSE (VOIDmode, cond, label, pc_rtx);
  x = emit_jump_insn (gen_rtx_SET (VOIDmode, pc_rtx, x));
  add_int_reg_note (x, REG_BR_PROB, very_unlikely);
}

/* A subroutine of the atomic operation splitters.  Emit a load-locked
   instruction in MODE.  */

static void
emit_load_locked (enum machine_mode mode, rtx reg, rtx mem)
{
  rtx (*fn) (rtx, rtx) = NULL;
  if (mode == SImode)
    fn = gen_load_locked_si;
  else if (mode == DImode)
    fn = gen_load_locked_di;
  emit_insn (fn (reg, mem));
}

/* A subroutine of the atomic operation splitters.  Emit a store-conditional
   instruction in MODE.  */

static void
emit_store_conditional (enum machine_mode mode, rtx res, rtx mem, rtx val)
{
  rtx (*fn) (rtx, rtx, rtx) = NULL;
  if (mode == SImode)
    fn = gen_store_conditional_si;
  else if (mode == DImode)
    fn = gen_store_conditional_di;
  emit_insn (fn (res, mem, val));
}

/* Subroutines of the atomic operation splitters.  Emit barriers
   as needed for the memory MODEL.  */

static void
alpha_pre_atomic_barrier (enum memmodel model)
{
  if (need_atomic_barrier_p (model, true))
    emit_insn (gen_memory_barrier ());
}

static void
alpha_post_atomic_barrier (enum memmodel model)
{
  if (need_atomic_barrier_p (model, false))
    emit_insn (gen_memory_barrier ());
}

/* A subroutine of the atomic operation splitters.  Emit an insxl
   instruction in MODE.  */

static rtx
emit_insxl (enum machine_mode mode, rtx op1, rtx op2)
{
  rtx ret = gen_reg_rtx (DImode);
  rtx (*fn) (rtx, rtx, rtx);

  switch (mode)
    {
    case QImode:
      fn = gen_insbl;
      break;
    case HImode:
      fn = gen_inswl;
      break;
    case SImode:
      fn = gen_insll;
      break;
    case DImode:
      fn = gen_insql;
      break;
    default:
      gcc_unreachable ();
    }

  op1 = force_reg (mode, op1);
  emit_insn (fn (ret, op1, op2));

  return ret;
}

/* Expand an atomic fetch-and-operate pattern.  CODE is the binary operation
   to perform.  MEM is the memory on which to operate.  VAL is the second 
   operand of the binary operator.  BEFORE and AFTER are optional locations to
   return the value of MEM either before of after the operation.  SCRATCH is
   a scratch register.  */

void
alpha_split_atomic_op (enum rtx_code code, rtx mem, rtx val, rtx before,
		       rtx after, rtx scratch, enum memmodel model)
{
  enum machine_mode mode = GET_MODE (mem);
  rtx label, x, cond = gen_rtx_REG (DImode, REGNO (scratch));

  alpha_pre_atomic_barrier (model);

  label = gen_label_rtx ();
  emit_label (label);
  label = gen_rtx_LABEL_REF (DImode, label);

  if (before == NULL)
    before = scratch;
  emit_load_locked (mode, before, mem);

  if (code == NOT)
    {
      x = gen_rtx_AND (mode, before, val);
      emit_insn (gen_rtx_SET (VOIDmode, val, x));

      x = gen_rtx_NOT (mode, val);
    }
  else
    x = gen_rtx_fmt_ee (code, mode, before, val);
  if (after)
    emit_insn (gen_rtx_SET (VOIDmode, after, copy_rtx (x)));
  emit_insn (gen_rtx_SET (VOIDmode, scratch, x));

  emit_store_conditional (mode, cond, mem, scratch);

  x = gen_rtx_EQ (DImode, cond, const0_rtx);
  emit_unlikely_jump (x, label);

  alpha_post_atomic_barrier (model);
}

/* Expand a compare and swap operation.  */

void
alpha_split_compare_and_swap (rtx operands[])
{
  rtx cond, retval, mem, oldval, newval;
  bool is_weak;
  enum memmodel mod_s, mod_f;
  enum machine_mode mode;
  rtx label1, label2, x;

  cond = operands[0];
  retval = operands[1];
  mem = operands[2];
  oldval = operands[3];
  newval = operands[4];
  is_weak = (operands[5] != const0_rtx);
  mod_s = (enum memmodel) INTVAL (operands[6]);
  mod_f = (enum memmodel) INTVAL (operands[7]);
  mode = GET_MODE (mem);

  alpha_pre_atomic_barrier (mod_s);

  label1 = NULL_RTX;
  if (!is_weak)
    {
      label1 = gen_rtx_LABEL_REF (DImode, gen_label_rtx ());
      emit_label (XEXP (label1, 0));
    }
  label2 = gen_rtx_LABEL_REF (DImode, gen_label_rtx ());

  emit_load_locked (mode, retval, mem);

  x = gen_lowpart (DImode, retval);
  if (oldval == const0_rtx)
    {
      emit_move_insn (cond, const0_rtx);
      x = gen_rtx_NE (DImode, x, const0_rtx);
    }
  else
    {
      x = gen_rtx_EQ (DImode, x, oldval);
      emit_insn (gen_rtx_SET (VOIDmode, cond, x));
      x = gen_rtx_EQ (DImode, cond, const0_rtx);
    }
  emit_unlikely_jump (x, label2);

  emit_move_insn (cond, newval);
  emit_store_conditional (mode, cond, mem, gen_lowpart (mode, cond));

  if (!is_weak)
    {
      x = gen_rtx_EQ (DImode, cond, const0_rtx);
      emit_unlikely_jump (x, label1);
    }

  if (mod_f != MEMMODEL_RELAXED)
    emit_label (XEXP (label2, 0));

  alpha_post_atomic_barrier (mod_s);

  if (mod_f == MEMMODEL_RELAXED)
    emit_label (XEXP (label2, 0));
}

void
alpha_expand_compare_and_swap_12 (rtx operands[])
{
  rtx cond, dst, mem, oldval, newval, is_weak, mod_s, mod_f;
  enum machine_mode mode;
  rtx addr, align, wdst;
  rtx (*gen) (rtx, rtx, rtx, rtx, rtx, rtx, rtx, rtx, rtx);

  cond = operands[0];
  dst = operands[1];
  mem = operands[2];
  oldval = operands[3];
  newval = operands[4];
  is_weak = operands[5];
  mod_s = operands[6];
  mod_f = operands[7];
  mode = GET_MODE (mem);

  /* We forced the address into a register via mem_noofs_operand.  */
  addr = XEXP (mem, 0);
  gcc_assert (register_operand (addr, DImode));

  align = expand_simple_binop (Pmode, AND, addr, GEN_INT (-8),
			       NULL_RTX, 1, OPTAB_DIRECT);

  oldval = convert_modes (DImode, mode, oldval, 1);

  if (newval != const0_rtx)
    newval = emit_insxl (mode, newval, addr);

  wdst = gen_reg_rtx (DImode);
  if (mode == QImode)
    gen = gen_atomic_compare_and_swapqi_1;
  else
    gen = gen_atomic_compare_and_swaphi_1;
  emit_insn (gen (cond, wdst, mem, oldval, newval, align,
		  is_weak, mod_s, mod_f));

  emit_move_insn (dst, gen_lowpart (mode, wdst));
}

void
alpha_split_compare_and_swap_12 (rtx operands[])
{
  rtx cond, dest, orig_mem, oldval, newval, align, scratch;
  enum machine_mode mode;
  bool is_weak;
  enum memmodel mod_s, mod_f;
  rtx label1, label2, mem, addr, width, mask, x;

  cond = operands[0];
  dest = operands[1];
  orig_mem = operands[2];
  oldval = operands[3];
  newval = operands[4];
  align = operands[5];
  is_weak = (operands[6] != const0_rtx);
  mod_s = (enum memmodel) INTVAL (operands[7]);
  mod_f = (enum memmodel) INTVAL (operands[8]);
  scratch = operands[9];
  mode = GET_MODE (orig_mem);
  addr = XEXP (orig_mem, 0);

  mem = gen_rtx_MEM (DImode, align);
  MEM_VOLATILE_P (mem) = MEM_VOLATILE_P (orig_mem);
  if (MEM_ALIAS_SET (orig_mem) == ALIAS_SET_MEMORY_BARRIER)
    set_mem_alias_set (mem, ALIAS_SET_MEMORY_BARRIER);

  alpha_pre_atomic_barrier (mod_s);

  label1 = NULL_RTX;
  if (!is_weak)
    {
      label1 = gen_rtx_LABEL_REF (DImode, gen_label_rtx ());
      emit_label (XEXP (label1, 0));
    }
  label2 = gen_rtx_LABEL_REF (DImode, gen_label_rtx ());

  emit_load_locked (DImode, scratch, mem);
  
  width = GEN_INT (GET_MODE_BITSIZE (mode));
  mask = GEN_INT (mode == QImode ? 0xff : 0xffff);
  emit_insn (gen_extxl (dest, scratch, width, addr));

  if (oldval == const0_rtx)
    {
      emit_move_insn (cond, const0_rtx);
      x = gen_rtx_NE (DImode, dest, const0_rtx);
    }
  else
    {
      x = gen_rtx_EQ (DImode, dest, oldval);
      emit_insn (gen_rtx_SET (VOIDmode, cond, x));
      x = gen_rtx_EQ (DImode, cond, const0_rtx);
    }
  emit_unlikely_jump (x, label2);

  emit_insn (gen_mskxl (cond, scratch, mask, addr));

  if (newval != const0_rtx)
    emit_insn (gen_iordi3 (cond, cond, newval));

  emit_store_conditional (DImode, cond, mem, cond);

  if (!is_weak)
    {
      x = gen_rtx_EQ (DImode, cond, const0_rtx);
      emit_unlikely_jump (x, label1);
    }

  if (mod_f != MEMMODEL_RELAXED)
    emit_label (XEXP (label2, 0));

  alpha_post_atomic_barrier (mod_s);

  if (mod_f == MEMMODEL_RELAXED)
    emit_label (XEXP (label2, 0));
}

/* Expand an atomic exchange operation.  */

void
alpha_split_atomic_exchange (rtx operands[])
{
  rtx retval, mem, val, scratch;
  enum memmodel model;
  enum machine_mode mode;
  rtx label, x, cond;

  retval = operands[0];
  mem = operands[1];
  val = operands[2];
  model = (enum memmodel) INTVAL (operands[3]);
  scratch = operands[4];
  mode = GET_MODE (mem);
  cond = gen_lowpart (DImode, scratch);

  alpha_pre_atomic_barrier (model);

  label = gen_rtx_LABEL_REF (DImode, gen_label_rtx ());
  emit_label (XEXP (label, 0));

  emit_load_locked (mode, retval, mem);
  emit_move_insn (scratch, val);
  emit_store_conditional (mode, cond, mem, scratch);

  x = gen_rtx_EQ (DImode, cond, const0_rtx);
  emit_unlikely_jump (x, label);

  alpha_post_atomic_barrier (model);
}

void
alpha_expand_atomic_exchange_12 (rtx operands[])
{
  rtx dst, mem, val, model;
  enum machine_mode mode;
  rtx addr, align, wdst;
  rtx (*gen) (rtx, rtx, rtx, rtx, rtx);

  dst = operands[0];
  mem = operands[1];
  val = operands[2];
  model = operands[3];
  mode = GET_MODE (mem);

  /* We forced the address into a register via mem_noofs_operand.  */
  addr = XEXP (mem, 0);
  gcc_assert (register_operand (addr, DImode));

  align = expand_simple_binop (Pmode, AND, addr, GEN_INT (-8),
			       NULL_RTX, 1, OPTAB_DIRECT);

  /* Insert val into the correct byte location within the word.  */
  if (val != const0_rtx)
    val = emit_insxl (mode, val, addr);

  wdst = gen_reg_rtx (DImode);
  if (mode == QImode)
    gen = gen_atomic_exchangeqi_1;
  else
    gen = gen_atomic_exchangehi_1;
  emit_insn (gen (wdst, mem, val, align, model));

  emit_move_insn (dst, gen_lowpart (mode, wdst));
}

void
alpha_split_atomic_exchange_12 (rtx operands[])
{
  rtx dest, orig_mem, addr, val, align, scratch;
  rtx label, mem, width, mask, x;
  enum machine_mode mode;
  enum memmodel model;

  dest = operands[0];
  orig_mem = operands[1];
  val = operands[2];
  align = operands[3];
  model = (enum memmodel) INTVAL (operands[4]);
  scratch = operands[5];
  mode = GET_MODE (orig_mem);
  addr = XEXP (orig_mem, 0);

  mem = gen_rtx_MEM (DImode, align);
  MEM_VOLATILE_P (mem) = MEM_VOLATILE_P (orig_mem);
  if (MEM_ALIAS_SET (orig_mem) == ALIAS_SET_MEMORY_BARRIER)
    set_mem_alias_set (mem, ALIAS_SET_MEMORY_BARRIER);

  alpha_pre_atomic_barrier (model);

  label = gen_rtx_LABEL_REF (DImode, gen_label_rtx ());
  emit_label (XEXP (label, 0));

  emit_load_locked (DImode, scratch, mem);
  
  width = GEN_INT (GET_MODE_BITSIZE (mode));
  mask = GEN_INT (mode == QImode ? 0xff : 0xffff);
  emit_insn (gen_extxl (dest, scratch, width, addr));
  emit_insn (gen_mskxl (scratch, scratch, mask, addr));
  if (val != const0_rtx)
    emit_insn (gen_iordi3 (scratch, scratch, val));

  emit_store_conditional (DImode, scratch, mem, scratch);

  x = gen_rtx_EQ (DImode, scratch, const0_rtx);
  emit_unlikely_jump (x, label);

  alpha_post_atomic_barrier (model);
}

/* Adjust the cost of a scheduling dependency.  Return the new cost of
   a dependency LINK or INSN on DEP_INSN.  COST is the current cost.  */

static int
alpha_adjust_cost (rtx insn, rtx link, rtx dep_insn, int cost)
{
  enum attr_type dep_insn_type;

  /* If the dependence is an anti-dependence, there is no cost.  For an
     output dependence, there is sometimes a cost, but it doesn't seem
     worth handling those few cases.  */
  if (REG_NOTE_KIND (link) != 0)
    return cost;

  /* If we can't recognize the insns, we can't really do anything.  */
  if (recog_memoized (insn) < 0 || recog_memoized (dep_insn) < 0)
    return cost;

  dep_insn_type = get_attr_type (dep_insn);

  /* Bring in the user-defined memory latency.  */
  if (dep_insn_type == TYPE_ILD
      || dep_insn_type == TYPE_FLD
      || dep_insn_type == TYPE_LDSYM)
    cost += alpha_memory_latency-1;

  /* Everything else handled in DFA bypasses now.  */

  return cost;
}

/* The number of instructions that can be issued per cycle.  */

static int
alpha_issue_rate (void)
{
  return (alpha_tune == PROCESSOR_EV4 ? 2 : 4);
}

/* How many alternative schedules to try.  This should be as wide as the
   scheduling freedom in the DFA, but no wider.  Making this value too
   large results extra work for the scheduler.

   For EV4, loads can be issued to either IB0 or IB1, thus we have 2
   alternative schedules.  For EV5, we can choose between E0/E1 and
   FA/FM.  For EV6, an arithmetic insn can be issued to U0/U1/L0/L1.  */

static int
alpha_multipass_dfa_lookahead (void)
{
  return (alpha_tune == PROCESSOR_EV6 ? 4 : 2);
}

/* Machine-specific function data.  */

struct GTY(()) alpha_links;

struct GTY(()) machine_function
{
  /* For OSF.  */
  const char *some_ld_name;

  /* For flag_reorder_blocks_and_partition.  */
  rtx gp_save_rtx;

  /* For VMS condition handlers.  */
  bool uses_condition_handler;

  /* Linkage entries.  */
  splay_tree GTY ((param1_is (char *), param2_is (struct alpha_links *)))
    links;
};

/* How to allocate a 'struct machine_function'.  */

static struct machine_function *
alpha_init_machine_status (void)
{
  return ggc_cleared_alloc<machine_function> ();
}

/* Support for frame based VMS condition handlers.  */

/* A VMS condition handler may be established for a function with a call to
   __builtin_establish_vms_condition_handler, and cancelled with a call to
   __builtin_revert_vms_condition_handler.

   The VMS Condition Handling Facility knows about the existence of a handler
   from the procedure descriptor .handler field.  As the VMS native compilers,
   we store the user specified handler's address at a fixed location in the
   stack frame and point the procedure descriptor at a common wrapper which
   fetches the real handler's address and issues an indirect call.

   The indirection wrapper is "__gcc_shell_handler", provided by libgcc.

   We force the procedure kind to PT_STACK, and the fixed frame location is
   fp+8, just before the register save area. We use the handler_data field in
   the procedure descriptor to state the fp offset at which the installed
   handler address can be found.  */

#define VMS_COND_HANDLER_FP_OFFSET 8

/* Expand code to store the currently installed user VMS condition handler
   into TARGET and install HANDLER as the new condition handler.  */

void
alpha_expand_builtin_establish_vms_condition_handler (rtx target, rtx handler)
{
  rtx handler_slot_address = plus_constant (Pmode, hard_frame_pointer_rtx,
					    VMS_COND_HANDLER_FP_OFFSET);

  rtx handler_slot
    = gen_rtx_MEM (DImode, handler_slot_address);

  emit_move_insn (target, handler_slot);
  emit_move_insn (handler_slot, handler);

  /* Notify the start/prologue/epilogue emitters that the condition handler
     slot is needed.  In addition to reserving the slot space, this will force
     the procedure kind to PT_STACK so ensure that the hard_frame_pointer_rtx
     use above is correct.  */
  cfun->machine->uses_condition_handler = true;
}

/* Expand code to store the current VMS condition handler into TARGET and
   nullify it.  */

void
alpha_expand_builtin_revert_vms_condition_handler (rtx target)
{
  /* We implement this by establishing a null condition handler, with the tiny
     side effect of setting uses_condition_handler.  This is a little bit
     pessimistic if no actual builtin_establish call is ever issued, which is
     not a real problem and expected never to happen anyway.  */

  alpha_expand_builtin_establish_vms_condition_handler (target, const0_rtx);
}

/* Functions to save and restore alpha_return_addr_rtx.  */

/* Start the ball rolling with RETURN_ADDR_RTX.  */

rtx
alpha_return_addr (int count, rtx frame ATTRIBUTE_UNUSED)
{
  if (count != 0)
    return const0_rtx;

  return get_hard_reg_initial_val (Pmode, REG_RA);
}

/* Return or create a memory slot containing the gp value for the current
   function.  Needed only if TARGET_LD_BUGGY_LDGP.  */

rtx
alpha_gp_save_rtx (void)
{
  rtx seq, m = cfun->machine->gp_save_rtx;

  if (m == NULL)
    {
      start_sequence ();

      m = assign_stack_local (DImode, UNITS_PER_WORD, BITS_PER_WORD);
      m = validize_mem (m);
      emit_move_insn (m, pic_offset_table_rtx);

      seq = get_insns ();
      end_sequence ();

      /* We used to simply emit the sequence after entry_of_function.
	 However this breaks the CFG if the first instruction in the
	 first block is not the NOTE_INSN_BASIC_BLOCK, for example a
	 label.  Emit the sequence properly on the edge.  We are only
	 invoked from dw2_build_landing_pads and finish_eh_generation
	 will call commit_edge_insertions thanks to a kludge.  */
      insert_insn_on_edge (seq,
			   single_succ_edge (ENTRY_BLOCK_PTR_FOR_FN (cfun)));

      cfun->machine->gp_save_rtx = m;
    }

  return m;
}

static void
alpha_instantiate_decls (void)
{
  if (cfun->machine->gp_save_rtx != NULL_RTX)
    instantiate_decl_rtl (cfun->machine->gp_save_rtx);
}

static int
alpha_ra_ever_killed (void)
{
  rtx top;

  if (!has_hard_reg_initial_val (Pmode, REG_RA))
    return (int)df_regs_ever_live_p (REG_RA);

  push_topmost_sequence ();
  top = get_insns ();
  pop_topmost_sequence ();

  return reg_set_between_p (gen_rtx_REG (Pmode, REG_RA), top, NULL_RTX);
}


/* Return the trap mode suffix applicable to the current
   instruction, or NULL.  */

static const char *
get_trap_mode_suffix (void)
{
  enum attr_trap_suffix s = get_attr_trap_suffix (current_output_insn);

  switch (s)
    {
    case TRAP_SUFFIX_NONE:
      return NULL;

    case TRAP_SUFFIX_SU:
      if (alpha_fptm >= ALPHA_FPTM_SU)
	return "su";
      return NULL;

    case TRAP_SUFFIX_SUI:
      if (alpha_fptm >= ALPHA_FPTM_SUI)
	return "sui";
      return NULL;

    case TRAP_SUFFIX_V_SV:
      switch (alpha_fptm)
	{
	case ALPHA_FPTM_N:
	  return NULL;
	case ALPHA_FPTM_U:
	  return "v";
	case ALPHA_FPTM_SU:
	case ALPHA_FPTM_SUI:
	  return "sv";
	default:
	  gcc_unreachable ();
	}

    case TRAP_SUFFIX_V_SV_SVI:
      switch (alpha_fptm)
	{
	case ALPHA_FPTM_N:
	  return NULL;
	case ALPHA_FPTM_U:
	  return "v";
	case ALPHA_FPTM_SU:
	  return "sv";
	case ALPHA_FPTM_SUI:
	  return "svi";
	default:
	  gcc_unreachable ();
	}
      break;

    case TRAP_SUFFIX_U_SU_SUI:
      switch (alpha_fptm)
	{
	case ALPHA_FPTM_N:
	  return NULL;
	case ALPHA_FPTM_U:
	  return "u";
	case ALPHA_FPTM_SU:
	  return "su";
	case ALPHA_FPTM_SUI:
	  return "sui";
	default:
	  gcc_unreachable ();
	}
      break;
      
    default:
      gcc_unreachable ();
    }
  gcc_unreachable ();
}

/* Return the rounding mode suffix applicable to the current
   instruction, or NULL.  */

static const char *
get_round_mode_suffix (void)
{
  enum attr_round_suffix s = get_attr_round_suffix (current_output_insn);

  switch (s)
    {
    case ROUND_SUFFIX_NONE:
      return NULL;
    case ROUND_SUFFIX_NORMAL:
      switch (alpha_fprm)
	{
	case ALPHA_FPRM_NORM:
	  return NULL;
	case ALPHA_FPRM_MINF:
	  return "m";
	case ALPHA_FPRM_CHOP:
	  return "c";
	case ALPHA_FPRM_DYN:
	  return "d";
	default:
	  gcc_unreachable ();
	}
      break;

    case ROUND_SUFFIX_C:
      return "c";
      
    default:
      gcc_unreachable ();
    }
  gcc_unreachable ();
}

/* Locate some local-dynamic symbol still in use by this function
   so that we can print its name in some movdi_er_tlsldm pattern.  */

static int
get_some_local_dynamic_name_1 (rtx *px, void *data ATTRIBUTE_UNUSED)
{
  rtx x = *px;

  if (GET_CODE (x) == SYMBOL_REF
      && SYMBOL_REF_TLS_MODEL (x) == TLS_MODEL_LOCAL_DYNAMIC)
    {
      cfun->machine->some_ld_name = XSTR (x, 0);
      return 1;
    }

  return 0;
}

static const char *
get_some_local_dynamic_name (void)
{
  rtx insn;

  if (cfun->machine->some_ld_name)
    return cfun->machine->some_ld_name;

  for (insn = get_insns (); insn ; insn = NEXT_INSN (insn))
    if (INSN_P (insn)
	&& for_each_rtx (&PATTERN (insn), get_some_local_dynamic_name_1, 0))
      return cfun->machine->some_ld_name;

  gcc_unreachable ();
}

/* Print an operand.  Recognize special options, documented below.  */

void
print_operand (FILE *file, rtx x, int code)
{
  int i;

  switch (code)
    {
    case '~':
      /* Print the assembler name of the current function.  */
      assemble_name (file, alpha_fnname);
      break;

    case '&':
      assemble_name (file, get_some_local_dynamic_name ());
      break;

    case '/':
      {
	const char *trap = get_trap_mode_suffix ();
	const char *round = get_round_mode_suffix ();

	if (trap || round)
	  fprintf (file, "/%s%s", (trap ? trap : ""), (round ? round : ""));
	break;
      }

    case ',':
      /* Generates single precision instruction suffix.  */
      fputc ((TARGET_FLOAT_VAX ? 'f' : 's'), file);
      break;

    case '-':
      /* Generates double precision instruction suffix.  */
      fputc ((TARGET_FLOAT_VAX ? 'g' : 't'), file);
      break;

    case '#':
      if (alpha_this_literal_sequence_number == 0)
	alpha_this_literal_sequence_number = alpha_next_sequence_number++;
      fprintf (file, "%d", alpha_this_literal_sequence_number);
      break;

    case '*':
      if (alpha_this_gpdisp_sequence_number == 0)
	alpha_this_gpdisp_sequence_number = alpha_next_sequence_number++;
      fprintf (file, "%d", alpha_this_gpdisp_sequence_number);
      break;

    case 'H':
      if (GET_CODE (x) == HIGH)
	output_addr_const (file, XEXP (x, 0));
      else
	output_operand_lossage ("invalid %%H value");
      break;

    case 'J':
      {
	const char *lituse;

        if (GET_CODE (x) == UNSPEC && XINT (x, 1) == UNSPEC_TLSGD_CALL)
	  {
	    x = XVECEXP (x, 0, 0);
	    lituse = "lituse_tlsgd";
	  }
	else if (GET_CODE (x) == UNSPEC && XINT (x, 1) == UNSPEC_TLSLDM_CALL)
	  {
	    x = XVECEXP (x, 0, 0);
	    lituse = "lituse_tlsldm";
	  }
	else if (CONST_INT_P (x))
	  lituse = "lituse_jsr";
	else
	  {
	    output_operand_lossage ("invalid %%J value");
	    break;
	  }

	if (x != const0_rtx)
	  fprintf (file, "\t\t!%s!%d", lituse, (int) INTVAL (x));
      }
      break;

    case 'j':
      {
	const char *lituse;

#ifdef HAVE_AS_JSRDIRECT_RELOCS
	lituse = "lituse_jsrdirect";
#else
	lituse = "lituse_jsr";
#endif

	gcc_assert (INTVAL (x) != 0);
	fprintf (file, "\t\t!%s!%d", lituse, (int) INTVAL (x));
      }
      break;
    case 'r':
      /* If this operand is the constant zero, write it as "$31".  */
      if (REG_P (x))
	fprintf (file, "%s", reg_names[REGNO (x)]);
      else if (x == CONST0_RTX (GET_MODE (x)))
	fprintf (file, "$31");
      else
	output_operand_lossage ("invalid %%r value");
      break;

    case 'R':
      /* Similar, but for floating-point.  */
      if (REG_P (x))
	fprintf (file, "%s", reg_names[REGNO (x)]);
      else if (x == CONST0_RTX (GET_MODE (x)))
	fprintf (file, "$f31");
      else
	output_operand_lossage ("invalid %%R value");
      break;

    case 'N':
      /* Write the 1's complement of a constant.  */
      if (!CONST_INT_P (x))
	output_operand_lossage ("invalid %%N value");

      fprintf (file, HOST_WIDE_INT_PRINT_DEC, ~ INTVAL (x));
      break;

    case 'P':
      /* Write 1 << C, for a constant C.  */
      if (!CONST_INT_P (x))
	output_operand_lossage ("invalid %%P value");

      fprintf (file, HOST_WIDE_INT_PRINT_DEC, (HOST_WIDE_INT) 1 << INTVAL (x));
      break;

    case 'h':
      /* Write the high-order 16 bits of a constant, sign-extended.  */
      if (!CONST_INT_P (x))
	output_operand_lossage ("invalid %%h value");

      fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (x) >> 16);
      break;

    case 'L':
      /* Write the low-order 16 bits of a constant, sign-extended.  */
      if (!CONST_INT_P (x))
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

      else if (CONST_INT_P (x))
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
      if (!CONST_INT_P (x)
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
      if (CONST_INT_P (x))
	{
	  HOST_WIDE_INT value = INTVAL (x);

	  if (value == 0xff)
	    {
	      fputc ('b', file);
	      break;
	    }
	  if (value == 0xffff)
	    {
	      fputc ('w', file);
	      break;
	    }
	  if (value == 0xffffffff)
	    {
	      fputc ('l', file);
	      break;
	    }
	  if (value == -1)
	    {
	      fputc ('q', file);
	      break;
	    }
	}
      else if (HOST_BITS_PER_WIDE_INT == 32
	       && GET_CODE (x) == CONST_DOUBLE
	       && CONST_DOUBLE_LOW (x) == 0xffffffff
	       && CONST_DOUBLE_HIGH (x) == 0)
	{
	  fputc ('l', file);
	  break;
	}
      output_operand_lossage ("invalid %%U value");
      break;

    case 's':
      /* Write the constant value divided by 8.  */
      if (!CONST_INT_P (x)
	  || (unsigned HOST_WIDE_INT) INTVAL (x) >= 64
	  || (INTVAL (x) & 7) != 0)
	output_operand_lossage ("invalid %%s value");

      fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (x) / 8);
      break;

    case 'S':
      /* Same, except compute (64 - c) / 8 */

      if (!CONST_INT_P (x)
	  && (unsigned HOST_WIDE_INT) INTVAL (x) >= 64
	  && (INTVAL (x) & 7) != 8)
	output_operand_lossage ("invalid %%s value");

      fprintf (file, HOST_WIDE_INT_PRINT_DEC, (64 - INTVAL (x)) / 8);
      break;

    case 'C': case 'D': case 'c': case 'd':
      /* Write out comparison name.  */
      {
	enum rtx_code c = GET_CODE (x);

        if (!COMPARISON_P (x))
	  output_operand_lossage ("invalid %%C value");

	else if (code == 'D')
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
      if (MEM_P (x) && GET_CODE (XEXP (x, 0)) == AND)
	fprintf (file, "_u");
      break;

    case 0:
      if (REG_P (x))
	fprintf (file, "%s", reg_names[REGNO (x)]);
      else if (MEM_P (x))
	output_address (XEXP (x, 0));
      else if (GET_CODE (x) == CONST && GET_CODE (XEXP (x, 0)) == UNSPEC)
	{
	  switch (XINT (XEXP (x, 0), 1))
	    {
	    case UNSPEC_DTPREL:
	    case UNSPEC_TPREL:
	      output_addr_const (file, XVECEXP (XEXP (x, 0), 0, 0));
	      break;
	    default:
	      output_operand_lossage ("unknown relocation unspec");
	      break;
	    }
	}
      else
	output_addr_const (file, x);
      break;

    default:
      output_operand_lossage ("invalid %%xn code");
    }
}

void
print_operand_address (FILE *file, rtx addr)
{
  int basereg = 31;
  HOST_WIDE_INT offset = 0;

  if (GET_CODE (addr) == AND)
    addr = XEXP (addr, 0);

  if (GET_CODE (addr) == PLUS
      && CONST_INT_P (XEXP (addr, 1)))
    {
      offset = INTVAL (XEXP (addr, 1));
      addr = XEXP (addr, 0);
    }

  if (GET_CODE (addr) == LO_SUM)
    {
      const char *reloc16, *reloclo;
      rtx op1 = XEXP (addr, 1);

      if (GET_CODE (op1) == CONST && GET_CODE (XEXP (op1, 0)) == UNSPEC)
	{
	  op1 = XEXP (op1, 0);
	  switch (XINT (op1, 1))
	    {
	    case UNSPEC_DTPREL:
	      reloc16 = NULL;
	      reloclo = (alpha_tls_size == 16 ? "dtprel" : "dtprello");
	      break;
	    case UNSPEC_TPREL:
	      reloc16 = NULL;
	      reloclo = (alpha_tls_size == 16 ? "tprel" : "tprello");
	      break;
	    default:
	      output_operand_lossage ("unknown relocation unspec");
	      return;
	    }

	  output_addr_const (file, XVECEXP (op1, 0, 0));
	}
      else
	{
	  reloc16 = "gprel";
	  reloclo = "gprellow";
	  output_addr_const (file, op1);
	}

      if (offset)
	fprintf (file, "+" HOST_WIDE_INT_PRINT_DEC, offset);

      addr = XEXP (addr, 0);
      switch (GET_CODE (addr))
	{
	case REG:
	  basereg = REGNO (addr);
	  break;

	case SUBREG:
	  basereg = subreg_regno (addr);
	  break;

	default:
	  gcc_unreachable ();
	}

      fprintf (file, "($%d)\t\t!%s", basereg,
	       (basereg == 29 ? reloc16 : reloclo));
      return;
    }

  switch (GET_CODE (addr))
    {
    case REG:
      basereg = REGNO (addr);
      break;

    case SUBREG:
      basereg = subreg_regno (addr);
      break;

    case CONST_INT:
      offset = INTVAL (addr);
      break;

#if TARGET_ABI_OPEN_VMS
    case SYMBOL_REF:
      fprintf (file, "%s", XSTR (addr, 0));
      return;

    case CONST:
      gcc_assert (GET_CODE (XEXP (addr, 0)) == PLUS
		  && GET_CODE (XEXP (XEXP (addr, 0), 0)) == SYMBOL_REF);
      fprintf (file, "%s+" HOST_WIDE_INT_PRINT_DEC,
	       XSTR (XEXP (XEXP (addr, 0), 0), 0),
	       INTVAL (XEXP (XEXP (addr, 0), 1)));
      return;
    
#endif
    default:
      gcc_unreachable ();
    }

  fprintf (file, HOST_WIDE_INT_PRINT_DEC "($%d)", offset, basereg);
}

/* Emit RTL insns to initialize the variable parts of a trampoline at
   M_TRAMP.  FNDECL is target function's decl.  CHAIN_VALUE is an rtx
   for the static chain value for the function.  */

static void
alpha_trampoline_init (rtx m_tramp, tree fndecl, rtx chain_value)
{
  rtx fnaddr, mem, word1, word2;

  fnaddr = XEXP (DECL_RTL (fndecl), 0);

#ifdef POINTERS_EXTEND_UNSIGNED
  fnaddr = convert_memory_address (Pmode, fnaddr);
  chain_value = convert_memory_address (Pmode, chain_value);
#endif

  if (TARGET_ABI_OPEN_VMS)
    {
      const char *fnname;
      char *trname;

      /* Construct the name of the trampoline entry point.  */
      fnname = XSTR (fnaddr, 0);
      trname = (char *) alloca (strlen (fnname) + 5);
      strcpy (trname, fnname);
      strcat (trname, "..tr");
      fnname = ggc_alloc_string (trname, strlen (trname) + 1);
      word2 = gen_rtx_SYMBOL_REF (Pmode, fnname);

      /* Trampoline (or "bounded") procedure descriptor is constructed from
	 the function's procedure descriptor with certain fields zeroed IAW
	 the VMS calling standard. This is stored in the first quadword.  */
      word1 = force_reg (DImode, gen_const_mem (DImode, fnaddr));
      word1 = expand_and (DImode, word1,
			  GEN_INT (HOST_WIDE_INT_C (0xffff0fff0000fff0)),
			  NULL);
    }
  else
    {
      /* These 4 instructions are:
	    ldq $1,24($27)
	    ldq $27,16($27)
	    jmp $31,($27),0
	    nop
	 We don't bother setting the HINT field of the jump; the nop
	 is merely there for padding.  */
      word1 = GEN_INT (HOST_WIDE_INT_C (0xa77b0010a43b0018));
      word2 = GEN_INT (HOST_WIDE_INT_C (0x47ff041f6bfb0000));
    }

  /* Store the first two words, as computed above.  */
  mem = adjust_address (m_tramp, DImode, 0);
  emit_move_insn (mem, word1);
  mem = adjust_address (m_tramp, DImode, 8);
  emit_move_insn (mem, word2);

  /* Store function address and static chain value.  */
  mem = adjust_address (m_tramp, Pmode, 16);
  emit_move_insn (mem, fnaddr);
  mem = adjust_address (m_tramp, Pmode, 24);
  emit_move_insn (mem, chain_value);

  if (TARGET_ABI_OSF)
    {
      emit_insn (gen_imb ());
#ifdef HAVE_ENABLE_EXECUTE_STACK
      emit_library_call (init_one_libfunc ("__enable_execute_stack"),
			 LCT_NORMAL, VOIDmode, 1, XEXP (m_tramp, 0), Pmode);
#endif
    }
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

static rtx
alpha_function_arg (cumulative_args_t cum_v, enum machine_mode mode,
		    const_tree type, bool named ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  int basereg;
  int num_args;

  /* Don't get confused and pass small structures in FP registers.  */
  if (type && AGGREGATE_TYPE_P (type))
    basereg = 16;
  else
    {
#ifdef ENABLE_CHECKING
      /* With alpha_split_complex_arg, we shouldn't see any raw complex
	 values here.  */
      gcc_assert (!COMPLEX_MODE_P (mode));
#endif

      /* Set up defaults for FP operands passed in FP registers, and
	 integral operands passed in integer registers.  */
      if (TARGET_FPREGS && GET_MODE_CLASS (mode) == MODE_FLOAT)
	basereg = 32 + 16;
      else
	basereg = 16;
    }

  /* ??? Irritatingly, the definition of CUMULATIVE_ARGS is different for
     the two platforms, so we can't avoid conditional compilation.  */
#if TARGET_ABI_OPEN_VMS
    {
      if (mode == VOIDmode)
	return alpha_arg_info_reg_val (*cum);

      num_args = cum->num_args;
      if (num_args >= 6
	  || targetm.calls.must_pass_in_stack (mode, type))
	return NULL_RTX;
    }
#elif TARGET_ABI_OSF
    {
      if (*cum >= 6)
	return NULL_RTX;
      num_args = *cum;

      /* VOID is passed as a special flag for "last argument".  */
      if (type == void_type_node)
	basereg = 16;
      else if (targetm.calls.must_pass_in_stack (mode, type))
	return NULL_RTX;
    }
#else
#error Unhandled ABI
#endif

  return gen_rtx_REG (mode, num_args + basereg);
}

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)  */

static void
alpha_function_arg_advance (cumulative_args_t cum_v, enum machine_mode mode,
			    const_tree type, bool named ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  bool onstack = targetm.calls.must_pass_in_stack (mode, type);
  int increment = onstack ? 6 : ALPHA_ARG_SIZE (mode, type, named);

#if TARGET_ABI_OSF
  *cum += increment;
#else
  if (!onstack && cum->num_args < 6)
    cum->atypes[cum->num_args] = alpha_arg_type (mode);
  cum->num_args += increment;
#endif
}

static int
alpha_arg_partial_bytes (cumulative_args_t cum_v,
			 enum machine_mode mode ATTRIBUTE_UNUSED,
			 tree type ATTRIBUTE_UNUSED,
			 bool named ATTRIBUTE_UNUSED)
{
  int words = 0;
  CUMULATIVE_ARGS *cum ATTRIBUTE_UNUSED = get_cumulative_args (cum_v);

#if TARGET_ABI_OPEN_VMS
  if (cum->num_args < 6
      && 6 < cum->num_args + ALPHA_ARG_SIZE (mode, type, named))
    words = 6 - cum->num_args;
#elif TARGET_ABI_OSF
  if (*cum < 6 && 6 < *cum + ALPHA_ARG_SIZE (mode, type, named))
    words = 6 - *cum;
#else
#error Unhandled ABI
#endif

  return words * UNITS_PER_WORD;
}


/* Return true if TYPE must be returned in memory, instead of in registers.  */

static bool
alpha_return_in_memory (const_tree type, const_tree fndecl ATTRIBUTE_UNUSED)
{
  enum machine_mode mode = VOIDmode;
  int size;

  if (type)
    {
      mode = TYPE_MODE (type);

      /* All aggregates are returned in memory, except on OpenVMS where
	 records that fit 64 bits should be returned by immediate value
	 as required by section 3.8.7.1 of the OpenVMS Calling Standard.  */
      if (TARGET_ABI_OPEN_VMS
	  && TREE_CODE (type) != ARRAY_TYPE
	  && (unsigned HOST_WIDE_INT) int_size_in_bytes(type) <= 8)
	return false;

      if (AGGREGATE_TYPE_P (type))
	return true;
    }

  size = GET_MODE_SIZE (mode);
  switch (GET_MODE_CLASS (mode))
    {
    case MODE_VECTOR_FLOAT:
      /* Pass all float vectors in memory, like an aggregate.  */
      return true;

    case MODE_COMPLEX_FLOAT:
      /* We judge complex floats on the size of their element,
	 not the size of the whole type.  */
      size = GET_MODE_UNIT_SIZE (mode);
      break;

    case MODE_INT:
    case MODE_FLOAT:
    case MODE_COMPLEX_INT:
    case MODE_VECTOR_INT:
      break;

    default:
      /* ??? We get called on all sorts of random stuff from
	 aggregate_value_p.  We must return something, but it's not
	 clear what's safe to return.  Pretend it's a struct I
	 guess.  */
      return true;
    }

  /* Otherwise types must fit in one register.  */
  return size > UNITS_PER_WORD;
}

/* Return true if TYPE should be passed by invisible reference.  */

static bool
alpha_pass_by_reference (cumulative_args_t ca ATTRIBUTE_UNUSED,
			 enum machine_mode mode,
			 const_tree type ATTRIBUTE_UNUSED,
			 bool named ATTRIBUTE_UNUSED)
{
  return mode == TFmode || mode == TCmode;
}

/* Define how to find the value returned by a function.  VALTYPE is the
   data type of the value (as a tree).  If the precise function being
   called is known, FUNC is its FUNCTION_DECL; otherwise, FUNC is 0.
   MODE is set instead of VALTYPE for libcalls.

   On Alpha the value is found in $0 for integer functions and
   $f0 for floating-point functions.  */

rtx
function_value (const_tree valtype, const_tree func ATTRIBUTE_UNUSED,
		enum machine_mode mode)
{
  unsigned int regnum, dummy ATTRIBUTE_UNUSED;
  enum mode_class mclass;

  gcc_assert (!valtype || !alpha_return_in_memory (valtype, func));

  if (valtype)
    mode = TYPE_MODE (valtype);

  mclass = GET_MODE_CLASS (mode);
  switch (mclass)
    {
    case MODE_INT:
      /* Do the same thing as PROMOTE_MODE except for libcalls on VMS,
	 where we have them returning both SImode and DImode.  */
      if (!(TARGET_ABI_OPEN_VMS && valtype && AGGREGATE_TYPE_P (valtype)))
        PROMOTE_MODE (mode, dummy, valtype);
      /* FALLTHRU */

    case MODE_COMPLEX_INT:
    case MODE_VECTOR_INT:
      regnum = 0;
      break;

    case MODE_FLOAT:
      regnum = 32;
      break;

    case MODE_COMPLEX_FLOAT:
      {
	enum machine_mode cmode = GET_MODE_INNER (mode);

	return gen_rtx_PARALLEL
	  (VOIDmode,
	   gen_rtvec (2,
		      gen_rtx_EXPR_LIST (VOIDmode, gen_rtx_REG (cmode, 32),
				         const0_rtx),
		      gen_rtx_EXPR_LIST (VOIDmode, gen_rtx_REG (cmode, 33),
				         GEN_INT (GET_MODE_SIZE (cmode)))));
      }

    case MODE_RANDOM:
      /* We should only reach here for BLKmode on VMS.  */
      gcc_assert (TARGET_ABI_OPEN_VMS && mode == BLKmode);
      regnum = 0;
      break;

    default:
      gcc_unreachable ();
    }

  return gen_rtx_REG (mode, regnum);
}

/* TCmode complex values are passed by invisible reference.  We
   should not split these values.  */

static bool
alpha_split_complex_arg (const_tree type)
{
  return TYPE_MODE (type) != TCmode;
}

static tree
alpha_build_builtin_va_list (void)
{
  tree base, ofs, space, record, type_decl;

  if (TARGET_ABI_OPEN_VMS)
    return ptr_type_node;

  record = (*lang_hooks.types.make_type) (RECORD_TYPE);
  type_decl = build_decl (BUILTINS_LOCATION,
			  TYPE_DECL, get_identifier ("__va_list_tag"), record);
  TYPE_STUB_DECL (record) = type_decl;
  TYPE_NAME (record) = type_decl;

  /* C++? SET_IS_AGGR_TYPE (record, 1); */

  /* Dummy field to prevent alignment warnings.  */
  space = build_decl (BUILTINS_LOCATION,
		      FIELD_DECL, NULL_TREE, integer_type_node);
  DECL_FIELD_CONTEXT (space) = record;
  DECL_ARTIFICIAL (space) = 1;
  DECL_IGNORED_P (space) = 1;

  ofs = build_decl (BUILTINS_LOCATION,
		    FIELD_DECL, get_identifier ("__offset"),
		    integer_type_node);
  DECL_FIELD_CONTEXT (ofs) = record;
  DECL_CHAIN (ofs) = space;
  /* ??? This is a hack, __offset is marked volatile to prevent
     DCE that confuses stdarg optimization and results in
     gcc.c-torture/execute/stdarg-1.c failure.  See PR 41089.  */
  TREE_THIS_VOLATILE (ofs) = 1;

  base = build_decl (BUILTINS_LOCATION,
		     FIELD_DECL, get_identifier ("__base"),
		     ptr_type_node);
  DECL_FIELD_CONTEXT (base) = record;
  DECL_CHAIN (base) = ofs;

  TYPE_FIELDS (record) = base;
  layout_type (record);

  va_list_gpr_counter_field = ofs;
  return record;
}

#if TARGET_ABI_OSF
/* Helper function for alpha_stdarg_optimize_hook.  Skip over casts
   and constant additions.  */

static gimple
va_list_skip_additions (tree lhs)
{
  gimple stmt;

  for (;;)
    {
      enum tree_code code;

      stmt = SSA_NAME_DEF_STMT (lhs);

      if (gimple_code (stmt) == GIMPLE_PHI)
	return stmt;

      if (!is_gimple_assign (stmt)
	  || gimple_assign_lhs (stmt) != lhs)
	return NULL;

      if (TREE_CODE (gimple_assign_rhs1 (stmt)) != SSA_NAME)
	return stmt;
      code = gimple_assign_rhs_code (stmt);
      if (!CONVERT_EXPR_CODE_P (code)
	  && ((code != PLUS_EXPR && code != POINTER_PLUS_EXPR)
	      || TREE_CODE (gimple_assign_rhs2 (stmt)) != INTEGER_CST
	      || !tree_fits_uhwi_p (gimple_assign_rhs2 (stmt))))
	return stmt;

      lhs = gimple_assign_rhs1 (stmt);
    }
}

/* Check if LHS = RHS statement is
   LHS = *(ap.__base + ap.__offset + cst)
   or
   LHS = *(ap.__base
	   + ((ap.__offset + cst <= 47)
	      ? ap.__offset + cst - 48 : ap.__offset + cst) + cst2).
   If the former, indicate that GPR registers are needed,
   if the latter, indicate that FPR registers are needed.

   Also look for LHS = (*ptr).field, where ptr is one of the forms
   listed above.

   On alpha, cfun->va_list_gpr_size is used as size of the needed
   regs and cfun->va_list_fpr_size is a bitmask, bit 0 set if GPR
   registers are needed and bit 1 set if FPR registers are needed.
   Return true if va_list references should not be scanned for the
   current statement.  */

static bool
alpha_stdarg_optimize_hook (struct stdarg_info *si, const_gimple stmt)
{
  tree base, offset, rhs;
  int offset_arg = 1;
  gimple base_stmt;

  if (get_gimple_rhs_class (gimple_assign_rhs_code (stmt))
      != GIMPLE_SINGLE_RHS)
    return false;

  rhs = gimple_assign_rhs1 (stmt);
  while (handled_component_p (rhs))
    rhs = TREE_OPERAND (rhs, 0);
  if (TREE_CODE (rhs) != MEM_REF
      || TREE_CODE (TREE_OPERAND (rhs, 0)) != SSA_NAME)
    return false;

  stmt = va_list_skip_additions (TREE_OPERAND (rhs, 0));
  if (stmt == NULL
      || !is_gimple_assign (stmt)
      || gimple_assign_rhs_code (stmt) != POINTER_PLUS_EXPR)
    return false;

  base = gimple_assign_rhs1 (stmt);
  if (TREE_CODE (base) == SSA_NAME)
    {
      base_stmt = va_list_skip_additions (base);
      if (base_stmt
	  && is_gimple_assign (base_stmt)
	  && gimple_assign_rhs_code (base_stmt) == COMPONENT_REF)
	base = gimple_assign_rhs1 (base_stmt);
    }

  if (TREE_CODE (base) != COMPONENT_REF
      || TREE_OPERAND (base, 1) != TYPE_FIELDS (va_list_type_node))
    {
      base = gimple_assign_rhs2 (stmt);
      if (TREE_CODE (base) == SSA_NAME)
	{
	  base_stmt = va_list_skip_additions (base);
	  if (base_stmt
	      && is_gimple_assign (base_stmt)
	      && gimple_assign_rhs_code (base_stmt) == COMPONENT_REF)
	    base = gimple_assign_rhs1 (base_stmt);
	}

      if (TREE_CODE (base) != COMPONENT_REF
	  || TREE_OPERAND (base, 1) != TYPE_FIELDS (va_list_type_node))
	return false;

      offset_arg = 0;
    }

  base = get_base_address (base);
  if (TREE_CODE (base) != VAR_DECL
      || !bitmap_bit_p (si->va_list_vars, DECL_UID (base) + num_ssa_names))
    return false;

  offset = gimple_op (stmt, 1 + offset_arg);
  if (TREE_CODE (offset) == SSA_NAME)
    {
      gimple offset_stmt = va_list_skip_additions (offset);

      if (offset_stmt
	  && gimple_code (offset_stmt) == GIMPLE_PHI)
	{
	  HOST_WIDE_INT sub;
	  gimple arg1_stmt, arg2_stmt;
	  tree arg1, arg2;
	  enum tree_code code1, code2;

	  if (gimple_phi_num_args (offset_stmt) != 2)
	    goto escapes;

	  arg1_stmt
	    = va_list_skip_additions (gimple_phi_arg_def (offset_stmt, 0));
	  arg2_stmt
	    = va_list_skip_additions (gimple_phi_arg_def (offset_stmt, 1));
	  if (arg1_stmt == NULL
	      || !is_gimple_assign (arg1_stmt)
	      || arg2_stmt == NULL
	      || !is_gimple_assign (arg2_stmt))
	    goto escapes;

	  code1 = gimple_assign_rhs_code (arg1_stmt);
	  code2 = gimple_assign_rhs_code (arg2_stmt);
	  if (code1 == COMPONENT_REF
	      && (code2 == MINUS_EXPR || code2 == PLUS_EXPR))
	    /* Do nothing.  */;
	  else if (code2 == COMPONENT_REF
		   && (code1 == MINUS_EXPR || code1 == PLUS_EXPR))
	    {
	      gimple tem = arg1_stmt;
	      code2 = code1;
	      arg1_stmt = arg2_stmt;
	      arg2_stmt = tem;
	    }
	  else
	    goto escapes;

	  if (!tree_fits_shwi_p (gimple_assign_rhs2 (arg2_stmt)))
	    goto escapes;

	  sub = tree_to_shwi (gimple_assign_rhs2 (arg2_stmt));
	  if (code2 == MINUS_EXPR)
	    sub = -sub;
	  if (sub < -48 || sub > -32)
	    goto escapes;

	  arg1 = gimple_assign_rhs1 (arg1_stmt);
	  arg2 = gimple_assign_rhs1 (arg2_stmt);
	  if (TREE_CODE (arg2) == SSA_NAME)
	    {
	      arg2_stmt = va_list_skip_additions (arg2);
	      if (arg2_stmt == NULL
		  || !is_gimple_assign (arg2_stmt)
		  || gimple_assign_rhs_code (arg2_stmt) != COMPONENT_REF)
		goto escapes;
	      arg2 = gimple_assign_rhs1 (arg2_stmt);
	    }
	  if (arg1 != arg2)
	    goto escapes;

	  if (TREE_CODE (arg1) != COMPONENT_REF
	      || TREE_OPERAND (arg1, 1) != va_list_gpr_counter_field
	      || get_base_address (arg1) != base)
	    goto escapes;

	  /* Need floating point regs.  */
	  cfun->va_list_fpr_size |= 2;
	  return false;
	}
      if (offset_stmt
	  && is_gimple_assign (offset_stmt)
	  && gimple_assign_rhs_code (offset_stmt) == COMPONENT_REF)
	offset = gimple_assign_rhs1 (offset_stmt);
    }
  if (TREE_CODE (offset) != COMPONENT_REF
      || TREE_OPERAND (offset, 1) != va_list_gpr_counter_field
      || get_base_address (offset) != base)
    goto escapes;
  else
    /* Need general regs.  */
    cfun->va_list_fpr_size |= 1;
  return false;

escapes:
  si->va_list_escapes = true;
  return false;
}
#endif

/* Perform any needed actions needed for a function that is receiving a
   variable number of arguments.  */

static void
alpha_setup_incoming_varargs (cumulative_args_t pcum, enum machine_mode mode,
			      tree type, int *pretend_size, int no_rtl)
{
  CUMULATIVE_ARGS cum = *get_cumulative_args (pcum);

  /* Skip the current argument.  */
  targetm.calls.function_arg_advance (pack_cumulative_args (&cum), mode, type,
				      true);

#if TARGET_ABI_OPEN_VMS
  /* For VMS, we allocate space for all 6 arg registers plus a count.

     However, if NO registers need to be saved, don't allocate any space.
     This is not only because we won't need the space, but because AP
     includes the current_pretend_args_size and we don't want to mess up
     any ap-relative addresses already made.  */
  if (cum.num_args < 6)
    {
      if (!no_rtl)
	{
	  emit_move_insn (gen_rtx_REG (DImode, 1), virtual_incoming_args_rtx);
	  emit_insn (gen_arg_home ());
	}
      *pretend_size = 7 * UNITS_PER_WORD;
    }
#else
  /* On OSF/1 and friends, we allocate space for all 12 arg registers, but
     only push those that are remaining.  However, if NO registers need to
     be saved, don't allocate any space.  This is not only because we won't
     need the space, but because AP includes the current_pretend_args_size
     and we don't want to mess up any ap-relative addresses already made.

     If we are not to use the floating-point registers, save the integer
     registers where we would put the floating-point registers.  This is
     not the most efficient way to implement varargs with just one register
     class, but it isn't worth doing anything more efficient in this rare
     case.  */
  if (cum >= 6)
    return;

  if (!no_rtl)
    {
      int count;
      alias_set_type set = get_varargs_alias_set ();
      rtx tmp;

      count = cfun->va_list_gpr_size / UNITS_PER_WORD;
      if (count > 6 - cum)
	count = 6 - cum;

      /* Detect whether integer registers or floating-point registers
	 are needed by the detected va_arg statements.  See above for
	 how these values are computed.  Note that the "escape" value
	 is VA_LIST_MAX_FPR_SIZE, which is 255, which has both of 
	 these bits set.  */
      gcc_assert ((VA_LIST_MAX_FPR_SIZE & 3) == 3);

      if (cfun->va_list_fpr_size & 1)
	{
	  tmp = gen_rtx_MEM (BLKmode,
			     plus_constant (Pmode, virtual_incoming_args_rtx,
					    (cum + 6) * UNITS_PER_WORD));
	  MEM_NOTRAP_P (tmp) = 1;
	  set_mem_alias_set (tmp, set);
	  move_block_from_reg (16 + cum, tmp, count);
	}

      if (cfun->va_list_fpr_size & 2)
	{
	  tmp = gen_rtx_MEM (BLKmode,
			     plus_constant (Pmode, virtual_incoming_args_rtx,
					    cum * UNITS_PER_WORD));
	  MEM_NOTRAP_P (tmp) = 1;
	  set_mem_alias_set (tmp, set);
	  move_block_from_reg (16 + cum + TARGET_FPREGS*32, tmp, count);
	}
     }
  *pretend_size = 12 * UNITS_PER_WORD;
#endif
}

static void
alpha_va_start (tree valist, rtx nextarg ATTRIBUTE_UNUSED)
{
  HOST_WIDE_INT offset;
  tree t, offset_field, base_field;

  if (TREE_CODE (TREE_TYPE (valist)) == ERROR_MARK)
    return;

  /* For Unix, TARGET_SETUP_INCOMING_VARARGS moves the starting address base
     up by 48, storing fp arg registers in the first 48 bytes, and the
     integer arg registers in the next 48 bytes.  This is only done,
     however, if any integer registers need to be stored.

     If no integer registers need be stored, then we must subtract 48
     in order to account for the integer arg registers which are counted
     in argsize above, but which are not actually stored on the stack.
     Must further be careful here about structures straddling the last
     integer argument register; that futzes with pretend_args_size,
     which changes the meaning of AP.  */

  if (NUM_ARGS < 6)
    offset = TARGET_ABI_OPEN_VMS ? UNITS_PER_WORD : 6 * UNITS_PER_WORD;
  else
    offset = -6 * UNITS_PER_WORD + crtl->args.pretend_args_size;

  if (TARGET_ABI_OPEN_VMS)
    {
      t = make_tree (ptr_type_node, virtual_incoming_args_rtx);
      t = fold_build_pointer_plus_hwi (t, offset + NUM_ARGS * UNITS_PER_WORD);
      t = build2 (MODIFY_EXPR, TREE_TYPE (valist), valist, t);
      TREE_SIDE_EFFECTS (t) = 1;
      expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
    }
  else
    {
      base_field = TYPE_FIELDS (TREE_TYPE (valist));
      offset_field = DECL_CHAIN (base_field);

      base_field = build3 (COMPONENT_REF, TREE_TYPE (base_field),
			   valist, base_field, NULL_TREE);
      offset_field = build3 (COMPONENT_REF, TREE_TYPE (offset_field),
			     valist, offset_field, NULL_TREE);

      t = make_tree (ptr_type_node, virtual_incoming_args_rtx);
      t = fold_build_pointer_plus_hwi (t, offset);
      t = build2 (MODIFY_EXPR, TREE_TYPE (base_field), base_field, t);
      TREE_SIDE_EFFECTS (t) = 1;
      expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

      t = build_int_cst (NULL_TREE, NUM_ARGS * UNITS_PER_WORD);
      t = build2 (MODIFY_EXPR, TREE_TYPE (offset_field), offset_field, t);
      TREE_SIDE_EFFECTS (t) = 1;
      expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
    }
}

static tree
alpha_gimplify_va_arg_1 (tree type, tree base, tree offset,
			 gimple_seq *pre_p)
{
  tree type_size, ptr_type, addend, t, addr;
  gimple_seq internal_post;

  /* If the type could not be passed in registers, skip the block
     reserved for the registers.  */
  if (targetm.calls.must_pass_in_stack (TYPE_MODE (type), type))
    {
      t = build_int_cst (TREE_TYPE (offset), 6*8);
      gimplify_assign (offset,
		       build2 (MAX_EXPR, TREE_TYPE (offset), offset, t),
		       pre_p);
    }

  addend = offset;
  ptr_type = build_pointer_type_for_mode (type, ptr_mode, true);

  if (TREE_CODE (type) == COMPLEX_TYPE)
    {
      tree real_part, imag_part, real_temp;

      real_part = alpha_gimplify_va_arg_1 (TREE_TYPE (type), base,
					   offset, pre_p);

      /* Copy the value into a new temporary, lest the formal temporary
	 be reused out from under us.  */
      real_temp = get_initialized_tmp_var (real_part, pre_p, NULL);

      imag_part = alpha_gimplify_va_arg_1 (TREE_TYPE (type), base,
					   offset, pre_p);

      return build2 (COMPLEX_EXPR, type, real_temp, imag_part);
    }
  else if (TREE_CODE (type) == REAL_TYPE)
    {
      tree fpaddend, cond, fourtyeight;

      fourtyeight = build_int_cst (TREE_TYPE (addend), 6*8);
      fpaddend = fold_build2 (MINUS_EXPR, TREE_TYPE (addend),
			      addend, fourtyeight);
      cond = fold_build2 (LT_EXPR, boolean_type_node, addend, fourtyeight);
      addend = fold_build3 (COND_EXPR, TREE_TYPE (addend), cond,
			    fpaddend, addend);
    }

  /* Build the final address and force that value into a temporary.  */
  addr = fold_build_pointer_plus (fold_convert (ptr_type, base), addend);
  internal_post = NULL;
  gimplify_expr (&addr, pre_p, &internal_post, is_gimple_val, fb_rvalue);
  gimple_seq_add_seq (pre_p, internal_post);

  /* Update the offset field.  */
  type_size = TYPE_SIZE_UNIT (TYPE_MAIN_VARIANT (type));
  if (type_size == NULL || TREE_OVERFLOW (type_size))
    t = size_zero_node;
  else
    {
      t = size_binop (PLUS_EXPR, type_size, size_int (7));
      t = size_binop (TRUNC_DIV_EXPR, t, size_int (8));
      t = size_binop (MULT_EXPR, t, size_int (8));
    }
  t = fold_convert (TREE_TYPE (offset), t);
  gimplify_assign (offset, build2 (PLUS_EXPR, TREE_TYPE (offset), offset, t),
      		   pre_p);

  return build_va_arg_indirect_ref (addr);
}

static tree
alpha_gimplify_va_arg (tree valist, tree type, gimple_seq *pre_p,
		       gimple_seq *post_p)
{
  tree offset_field, base_field, offset, base, t, r;
  bool indirect;

  if (TARGET_ABI_OPEN_VMS)
    return std_gimplify_va_arg_expr (valist, type, pre_p, post_p);

  base_field = TYPE_FIELDS (va_list_type_node);
  offset_field = DECL_CHAIN (base_field);
  base_field = build3 (COMPONENT_REF, TREE_TYPE (base_field),
		       valist, base_field, NULL_TREE);
  offset_field = build3 (COMPONENT_REF, TREE_TYPE (offset_field),
			 valist, offset_field, NULL_TREE);

  /* Pull the fields of the structure out into temporaries.  Since we never
     modify the base field, we can use a formal temporary.  Sign-extend the
     offset field so that it's the proper width for pointer arithmetic.  */
  base = get_formal_tmp_var (base_field, pre_p);

  t = fold_convert (build_nonstandard_integer_type (64, 0), offset_field);
  offset = get_initialized_tmp_var (t, pre_p, NULL);

  indirect = pass_by_reference (NULL, TYPE_MODE (type), type, false);
  if (indirect)
    type = build_pointer_type_for_mode (type, ptr_mode, true);

  /* Find the value.  Note that this will be a stable indirection, or
     a composite of stable indirections in the case of complex.  */
  r = alpha_gimplify_va_arg_1 (type, base, offset, pre_p);

  /* Stuff the offset temporary back into its field.  */
  gimplify_assign (unshare_expr (offset_field),
		   fold_convert (TREE_TYPE (offset_field), offset), pre_p);

  if (indirect)
    r = build_va_arg_indirect_ref (r);

  return r;
}

/* Builtins.  */

enum alpha_builtin
{
  ALPHA_BUILTIN_CMPBGE,
  ALPHA_BUILTIN_EXTBL,
  ALPHA_BUILTIN_EXTWL,
  ALPHA_BUILTIN_EXTLL,
  ALPHA_BUILTIN_EXTQL,
  ALPHA_BUILTIN_EXTWH,
  ALPHA_BUILTIN_EXTLH,
  ALPHA_BUILTIN_EXTQH,
  ALPHA_BUILTIN_INSBL,
  ALPHA_BUILTIN_INSWL,
  ALPHA_BUILTIN_INSLL,
  ALPHA_BUILTIN_INSQL,
  ALPHA_BUILTIN_INSWH,
  ALPHA_BUILTIN_INSLH,
  ALPHA_BUILTIN_INSQH,
  ALPHA_BUILTIN_MSKBL,
  ALPHA_BUILTIN_MSKWL,
  ALPHA_BUILTIN_MSKLL,
  ALPHA_BUILTIN_MSKQL,
  ALPHA_BUILTIN_MSKWH,
  ALPHA_BUILTIN_MSKLH,
  ALPHA_BUILTIN_MSKQH,
  ALPHA_BUILTIN_UMULH,
  ALPHA_BUILTIN_ZAP,
  ALPHA_BUILTIN_ZAPNOT,
  ALPHA_BUILTIN_AMASK,
  ALPHA_BUILTIN_IMPLVER,
  ALPHA_BUILTIN_RPCC,
  ALPHA_BUILTIN_ESTABLISH_VMS_CONDITION_HANDLER,
  ALPHA_BUILTIN_REVERT_VMS_CONDITION_HANDLER,

  /* TARGET_MAX */
  ALPHA_BUILTIN_MINUB8,
  ALPHA_BUILTIN_MINSB8,
  ALPHA_BUILTIN_MINUW4,
  ALPHA_BUILTIN_MINSW4,
  ALPHA_BUILTIN_MAXUB8,
  ALPHA_BUILTIN_MAXSB8,
  ALPHA_BUILTIN_MAXUW4,
  ALPHA_BUILTIN_MAXSW4,
  ALPHA_BUILTIN_PERR,
  ALPHA_BUILTIN_PKLB,
  ALPHA_BUILTIN_PKWB,
  ALPHA_BUILTIN_UNPKBL,
  ALPHA_BUILTIN_UNPKBW,

  /* TARGET_CIX */
  ALPHA_BUILTIN_CTTZ,
  ALPHA_BUILTIN_CTLZ,
  ALPHA_BUILTIN_CTPOP,

  ALPHA_BUILTIN_max
};

static enum insn_code const code_for_builtin[ALPHA_BUILTIN_max] = {
  CODE_FOR_builtin_cmpbge,
  CODE_FOR_extbl,
  CODE_FOR_extwl,
  CODE_FOR_extll,
  CODE_FOR_extql,
  CODE_FOR_extwh,
  CODE_FOR_extlh,
  CODE_FOR_extqh,
  CODE_FOR_builtin_insbl,
  CODE_FOR_builtin_inswl,
  CODE_FOR_builtin_insll,
  CODE_FOR_insql,
  CODE_FOR_inswh,
  CODE_FOR_inslh,
  CODE_FOR_insqh,
  CODE_FOR_mskbl,
  CODE_FOR_mskwl,
  CODE_FOR_mskll,
  CODE_FOR_mskql,
  CODE_FOR_mskwh,
  CODE_FOR_msklh,
  CODE_FOR_mskqh,
  CODE_FOR_umuldi3_highpart,
  CODE_FOR_builtin_zap,
  CODE_FOR_builtin_zapnot,
  CODE_FOR_builtin_amask,
  CODE_FOR_builtin_implver,
  CODE_FOR_builtin_rpcc,
  CODE_FOR_builtin_establish_vms_condition_handler,
  CODE_FOR_builtin_revert_vms_condition_handler,

  /* TARGET_MAX */
  CODE_FOR_builtin_minub8,
  CODE_FOR_builtin_minsb8,
  CODE_FOR_builtin_minuw4,
  CODE_FOR_builtin_minsw4,
  CODE_FOR_builtin_maxub8,
  CODE_FOR_builtin_maxsb8,
  CODE_FOR_builtin_maxuw4,
  CODE_FOR_builtin_maxsw4,
  CODE_FOR_builtin_perr,
  CODE_FOR_builtin_pklb,
  CODE_FOR_builtin_pkwb,
  CODE_FOR_builtin_unpkbl,
  CODE_FOR_builtin_unpkbw,

  /* TARGET_CIX */
  CODE_FOR_ctzdi2,
  CODE_FOR_clzdi2,
  CODE_FOR_popcountdi2
};

struct alpha_builtin_def
{
  const char *name;
  enum alpha_builtin code;
  unsigned int target_mask;
  bool is_const;
};

static struct alpha_builtin_def const zero_arg_builtins[] = {
  { "__builtin_alpha_implver",	ALPHA_BUILTIN_IMPLVER,	0, true },
  { "__builtin_alpha_rpcc",	ALPHA_BUILTIN_RPCC,	0, false }
};

static struct alpha_builtin_def const one_arg_builtins[] = {
  { "__builtin_alpha_amask",	ALPHA_BUILTIN_AMASK,	0, true },
  { "__builtin_alpha_pklb",	ALPHA_BUILTIN_PKLB,	MASK_MAX, true },
  { "__builtin_alpha_pkwb",	ALPHA_BUILTIN_PKWB,	MASK_MAX, true },
  { "__builtin_alpha_unpkbl",	ALPHA_BUILTIN_UNPKBL,	MASK_MAX, true },
  { "__builtin_alpha_unpkbw",	ALPHA_BUILTIN_UNPKBW,	MASK_MAX, true },
  { "__builtin_alpha_cttz",	ALPHA_BUILTIN_CTTZ,	MASK_CIX, true },
  { "__builtin_alpha_ctlz",	ALPHA_BUILTIN_CTLZ,	MASK_CIX, true },
  { "__builtin_alpha_ctpop",	ALPHA_BUILTIN_CTPOP,	MASK_CIX, true }
};

static struct alpha_builtin_def const two_arg_builtins[] = {
  { "__builtin_alpha_cmpbge",	ALPHA_BUILTIN_CMPBGE,	0, true },
  { "__builtin_alpha_extbl",	ALPHA_BUILTIN_EXTBL,	0, true },
  { "__builtin_alpha_extwl",	ALPHA_BUILTIN_EXTWL,	0, true },
  { "__builtin_alpha_extll",	ALPHA_BUILTIN_EXTLL,	0, true },
  { "__builtin_alpha_extql",	ALPHA_BUILTIN_EXTQL,	0, true },
  { "__builtin_alpha_extwh",	ALPHA_BUILTIN_EXTWH,	0, true },
  { "__builtin_alpha_extlh",	ALPHA_BUILTIN_EXTLH,	0, true },
  { "__builtin_alpha_extqh",	ALPHA_BUILTIN_EXTQH,	0, true },
  { "__builtin_alpha_insbl",	ALPHA_BUILTIN_INSBL,	0, true },
  { "__builtin_alpha_inswl",	ALPHA_BUILTIN_INSWL,	0, true },
  { "__builtin_alpha_insll",	ALPHA_BUILTIN_INSLL,	0, true },
  { "__builtin_alpha_insql",	ALPHA_BUILTIN_INSQL,	0, true },
  { "__builtin_alpha_inswh",	ALPHA_BUILTIN_INSWH,	0, true },
  { "__builtin_alpha_inslh",	ALPHA_BUILTIN_INSLH,	0, true },
  { "__builtin_alpha_insqh",	ALPHA_BUILTIN_INSQH,	0, true },
  { "__builtin_alpha_mskbl",	ALPHA_BUILTIN_MSKBL,	0, true },
  { "__builtin_alpha_mskwl",	ALPHA_BUILTIN_MSKWL,	0, true },
  { "__builtin_alpha_mskll",	ALPHA_BUILTIN_MSKLL,	0, true },
  { "__builtin_alpha_mskql",	ALPHA_BUILTIN_MSKQL,	0, true },
  { "__builtin_alpha_mskwh",	ALPHA_BUILTIN_MSKWH,	0, true },
  { "__builtin_alpha_msklh",	ALPHA_BUILTIN_MSKLH,	0, true },
  { "__builtin_alpha_mskqh",	ALPHA_BUILTIN_MSKQH,	0, true },
  { "__builtin_alpha_umulh",	ALPHA_BUILTIN_UMULH,	0, true },
  { "__builtin_alpha_zap",	ALPHA_BUILTIN_ZAP,	0, true },
  { "__builtin_alpha_zapnot",	ALPHA_BUILTIN_ZAPNOT,	0, true },
  { "__builtin_alpha_minub8",	ALPHA_BUILTIN_MINUB8,	MASK_MAX, true },
  { "__builtin_alpha_minsb8",	ALPHA_BUILTIN_MINSB8,	MASK_MAX, true },
  { "__builtin_alpha_minuw4",	ALPHA_BUILTIN_MINUW4,	MASK_MAX, true },
  { "__builtin_alpha_minsw4",	ALPHA_BUILTIN_MINSW4,	MASK_MAX, true },
  { "__builtin_alpha_maxub8",	ALPHA_BUILTIN_MAXUB8,	MASK_MAX, true },
  { "__builtin_alpha_maxsb8",	ALPHA_BUILTIN_MAXSB8,	MASK_MAX, true },
  { "__builtin_alpha_maxuw4",	ALPHA_BUILTIN_MAXUW4,	MASK_MAX, true },
  { "__builtin_alpha_maxsw4",	ALPHA_BUILTIN_MAXSW4,	MASK_MAX, true },
  { "__builtin_alpha_perr",	ALPHA_BUILTIN_PERR,	MASK_MAX, true }
};

static GTY(()) tree alpha_dimode_u;
static GTY(()) tree alpha_v8qi_u;
static GTY(()) tree alpha_v8qi_s;
static GTY(()) tree alpha_v4hi_u;
static GTY(()) tree alpha_v4hi_s;

static GTY(()) tree alpha_builtins[(int) ALPHA_BUILTIN_max];

/* Return the alpha builtin for CODE.  */

static tree
alpha_builtin_decl (unsigned code, bool initialize_p ATTRIBUTE_UNUSED)
{
  if (code >= ALPHA_BUILTIN_max)
    return error_mark_node;
  return alpha_builtins[code];
}

/* Helper function of alpha_init_builtins.  Add the built-in specified
   by NAME, TYPE, CODE, and ECF.  */

static void
alpha_builtin_function (const char *name, tree ftype,
			enum alpha_builtin code, unsigned ecf)
{
  tree decl = add_builtin_function (name, ftype, (int) code,
				    BUILT_IN_MD, NULL, NULL_TREE);

  if (ecf & ECF_CONST)
    TREE_READONLY (decl) = 1;
  if (ecf & ECF_NOTHROW)
    TREE_NOTHROW (decl) = 1;

  alpha_builtins [(int) code] = decl;
}

/* Helper function of alpha_init_builtins.  Add the COUNT built-in
   functions pointed to by P, with function type FTYPE.  */

static void
alpha_add_builtins (const struct alpha_builtin_def *p, size_t count,
		    tree ftype)
{
  size_t i;

  for (i = 0; i < count; ++i, ++p)
    if ((target_flags & p->target_mask) == p->target_mask)
      alpha_builtin_function (p->name, ftype, p->code,
			      (p->is_const ? ECF_CONST : 0) | ECF_NOTHROW);
}

static void
alpha_init_builtins (void)
{
  tree ftype;

  alpha_dimode_u = lang_hooks.types.type_for_mode (DImode, 1);
  alpha_v8qi_u = build_vector_type (unsigned_intQI_type_node, 8);
  alpha_v8qi_s = build_vector_type (intQI_type_node, 8);
  alpha_v4hi_u = build_vector_type (unsigned_intHI_type_node, 4);
  alpha_v4hi_s = build_vector_type (intHI_type_node, 4);

  ftype = build_function_type_list (alpha_dimode_u, NULL_TREE);
  alpha_add_builtins (zero_arg_builtins, ARRAY_SIZE (zero_arg_builtins), ftype);

  ftype = build_function_type_list (alpha_dimode_u, alpha_dimode_u, NULL_TREE);
  alpha_add_builtins (one_arg_builtins, ARRAY_SIZE (one_arg_builtins), ftype);

  ftype = build_function_type_list (alpha_dimode_u, alpha_dimode_u,
				    alpha_dimode_u, NULL_TREE);
  alpha_add_builtins (two_arg_builtins, ARRAY_SIZE (two_arg_builtins), ftype);

  if (TARGET_ABI_OPEN_VMS)
    {
      ftype = build_function_type_list (ptr_type_node, ptr_type_node,
					NULL_TREE);
      alpha_builtin_function ("__builtin_establish_vms_condition_handler",
			      ftype,
			      ALPHA_BUILTIN_ESTABLISH_VMS_CONDITION_HANDLER,
			      0);

      ftype = build_function_type_list (ptr_type_node, void_type_node,
					NULL_TREE);
      alpha_builtin_function ("__builtin_revert_vms_condition_handler", ftype,
			      ALPHA_BUILTIN_REVERT_VMS_CONDITION_HANDLER, 0);

      vms_patch_builtins ();
    }
}

/* Expand an expression EXP that calls a built-in function,
   with result going to TARGET if that's convenient
   (and in mode MODE if that's convenient).
   SUBTARGET may be used as the target for computing one of EXP's operands.
   IGNORE is nonzero if the value is to be ignored.  */

static rtx
alpha_expand_builtin (tree exp, rtx target,
		      rtx subtarget ATTRIBUTE_UNUSED,
		      enum machine_mode mode ATTRIBUTE_UNUSED,
		      int ignore ATTRIBUTE_UNUSED)
{
#define MAX_ARGS 2

  tree fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  unsigned int fcode = DECL_FUNCTION_CODE (fndecl);
  tree arg;
  call_expr_arg_iterator iter;
  enum insn_code icode;
  rtx op[MAX_ARGS], pat;
  int arity;
  bool nonvoid;

  if (fcode >= ALPHA_BUILTIN_max)
    internal_error ("bad builtin fcode");
  icode = code_for_builtin[fcode];
  if (icode == 0)
    internal_error ("bad builtin fcode");

  nonvoid = TREE_TYPE (TREE_TYPE (fndecl)) != void_type_node;

  arity = 0;
  FOR_EACH_CALL_EXPR_ARG (arg, iter, exp)
    {
      const struct insn_operand_data *insn_op;

      if (arg == error_mark_node)
	return NULL_RTX;
      if (arity > MAX_ARGS)
	return NULL_RTX;

      insn_op = &insn_data[icode].operand[arity + nonvoid];

      op[arity] = expand_expr (arg, NULL_RTX, insn_op->mode, EXPAND_NORMAL);

      if (!(*insn_op->predicate) (op[arity], insn_op->mode))
	op[arity] = copy_to_mode_reg (insn_op->mode, op[arity]);
      arity++;
    }

  if (nonvoid)
    {
      enum machine_mode tmode = insn_data[icode].operand[0].mode;
      if (!target
	  || GET_MODE (target) != tmode
	  || !(*insn_data[icode].operand[0].predicate) (target, tmode))
	target = gen_reg_rtx (tmode);
    }

  switch (arity)
    {
    case 0:
      pat = GEN_FCN (icode) (target);
      break;
    case 1:
      if (nonvoid)
        pat = GEN_FCN (icode) (target, op[0]);
      else
	pat = GEN_FCN (icode) (op[0]);
      break;
    case 2:
      pat = GEN_FCN (icode) (target, op[0], op[1]);
      break;
    default:
      gcc_unreachable ();
    }
  if (!pat)
    return NULL_RTX;
  emit_insn (pat);

  if (nonvoid)
    return target;
  else
    return const0_rtx;
}


/* Several bits below assume HWI >= 64 bits.  This should be enforced
   by config.gcc.  */
#if HOST_BITS_PER_WIDE_INT < 64
# error "HOST_WIDE_INT too small"
#endif

/* Fold the builtin for the CMPBGE instruction.  This is a vector comparison
   with an 8-bit output vector.  OPINT contains the integer operands; bit N
   of OP_CONST is set if OPINT[N] is valid.  */

static tree
alpha_fold_builtin_cmpbge (unsigned HOST_WIDE_INT opint[], long op_const)
{
  if (op_const == 3)
    {
      int i, val;
      for (i = 0, val = 0; i < 8; ++i)
	{
	  unsigned HOST_WIDE_INT c0 = (opint[0] >> (i * 8)) & 0xff;
	  unsigned HOST_WIDE_INT c1 = (opint[1] >> (i * 8)) & 0xff;
	  if (c0 >= c1)
	    val |= 1 << i;
	}
      return build_int_cst (alpha_dimode_u, val);
    }
  else if (op_const == 2 && opint[1] == 0)
    return build_int_cst (alpha_dimode_u, 0xff);
  return NULL;
}

/* Fold the builtin for the ZAPNOT instruction.  This is essentially a 
   specialized form of an AND operation.  Other byte manipulation instructions
   are defined in terms of this instruction, so this is also used as a
   subroutine for other builtins.

   OP contains the tree operands; OPINT contains the extracted integer values.
   Bit N of OP_CONST it set if OPINT[N] is valid.  OP may be null if only
   OPINT may be considered.  */

static tree
alpha_fold_builtin_zapnot (tree *op, unsigned HOST_WIDE_INT opint[],
			   long op_const)
{
  if (op_const & 2)
    {
      unsigned HOST_WIDE_INT mask = 0;
      int i;

      for (i = 0; i < 8; ++i)
	if ((opint[1] >> i) & 1)
	  mask |= (unsigned HOST_WIDE_INT)0xff << (i * 8);

      if (op_const & 1)
	return build_int_cst (alpha_dimode_u, opint[0] & mask);

      if (op)
	return fold_build2 (BIT_AND_EXPR, alpha_dimode_u, op[0],
			    build_int_cst (alpha_dimode_u, mask));
    }
  else if ((op_const & 1) && opint[0] == 0)
    return build_int_cst (alpha_dimode_u, 0);
  return NULL;
}

/* Fold the builtins for the EXT family of instructions.  */

static tree
alpha_fold_builtin_extxx (tree op[], unsigned HOST_WIDE_INT opint[],
			  long op_const, unsigned HOST_WIDE_INT bytemask,
			  bool is_high)
{
  long zap_const = 2;
  tree *zap_op = NULL;

  if (op_const & 2)
    {
      unsigned HOST_WIDE_INT loc;

      loc = opint[1] & 7;
      loc *= BITS_PER_UNIT;

      if (loc != 0)
	{
	  if (op_const & 1)
	    {
	      unsigned HOST_WIDE_INT temp = opint[0];
	      if (is_high)
		temp <<= loc;
	      else
		temp >>= loc;
	      opint[0] = temp;
	      zap_const = 3;
	    }
	}
      else
	zap_op = op;
    }
  
  opint[1] = bytemask;
  return alpha_fold_builtin_zapnot (zap_op, opint, zap_const);
}

/* Fold the builtins for the INS family of instructions.  */

static tree
alpha_fold_builtin_insxx (tree op[], unsigned HOST_WIDE_INT opint[],
			  long op_const, unsigned HOST_WIDE_INT bytemask,
			  bool is_high)
{
  if ((op_const & 1) && opint[0] == 0)
    return build_int_cst (alpha_dimode_u, 0);

  if (op_const & 2)
    {
      unsigned HOST_WIDE_INT temp, loc, byteloc;
      tree *zap_op = NULL;

      loc = opint[1] & 7;
      bytemask <<= loc;

      temp = opint[0];
      if (is_high)
	{
	  byteloc = (64 - (loc * 8)) & 0x3f;
	  if (byteloc == 0)
	    zap_op = op;
	  else
	    temp >>= byteloc;
	  bytemask >>= 8;
	}
      else
	{
	  byteloc = loc * 8;
	  if (byteloc == 0)
	    zap_op = op;
	  else
	    temp <<= byteloc;
	}

      opint[0] = temp;
      opint[1] = bytemask;
      return alpha_fold_builtin_zapnot (zap_op, opint, op_const);
    }

  return NULL;
}

static tree
alpha_fold_builtin_mskxx (tree op[], unsigned HOST_WIDE_INT opint[],
			  long op_const, unsigned HOST_WIDE_INT bytemask,
			  bool is_high)
{
  if (op_const & 2)
    {
      unsigned HOST_WIDE_INT loc;

      loc = opint[1] & 7;
      bytemask <<= loc;

      if (is_high)
	bytemask >>= 8;

      opint[1] = bytemask ^ 0xff;
    }

  return alpha_fold_builtin_zapnot (op, opint, op_const);
}

static tree
alpha_fold_vector_minmax (enum tree_code code, tree op[], tree vtype)
{
  tree op0 = fold_convert (vtype, op[0]);
  tree op1 = fold_convert (vtype, op[1]);
  tree val = fold_build2 (code, vtype, op0, op1);
  return fold_build1 (VIEW_CONVERT_EXPR, alpha_dimode_u, val);
}

static tree
alpha_fold_builtin_perr (unsigned HOST_WIDE_INT opint[], long op_const)
{
  unsigned HOST_WIDE_INT temp = 0;
  int i;

  if (op_const != 3)
    return NULL;

  for (i = 0; i < 8; ++i)
    {
      unsigned HOST_WIDE_INT a = (opint[0] >> (i * 8)) & 0xff;
      unsigned HOST_WIDE_INT b = (opint[1] >> (i * 8)) & 0xff;
      if (a >= b)
	temp += a - b;
      else
	temp += b - a;
    }

  return build_int_cst (alpha_dimode_u, temp);
}

static tree
alpha_fold_builtin_pklb (unsigned HOST_WIDE_INT opint[], long op_const)
{
  unsigned HOST_WIDE_INT temp;

  if (op_const == 0)
    return NULL;

  temp = opint[0] & 0xff;
  temp |= (opint[0] >> 24) & 0xff00;

  return build_int_cst (alpha_dimode_u, temp);
}

static tree
alpha_fold_builtin_pkwb (unsigned HOST_WIDE_INT opint[], long op_const)
{
  unsigned HOST_WIDE_INT temp;

  if (op_const == 0)
    return NULL;

  temp = opint[0] & 0xff;
  temp |= (opint[0] >>  8) & 0xff00;
  temp |= (opint[0] >> 16) & 0xff0000;
  temp |= (opint[0] >> 24) & 0xff000000;

  return build_int_cst (alpha_dimode_u, temp);
}

static tree
alpha_fold_builtin_unpkbl (unsigned HOST_WIDE_INT opint[], long op_const)
{
  unsigned HOST_WIDE_INT temp;

  if (op_const == 0)
    return NULL;

  temp = opint[0] & 0xff;
  temp |= (opint[0] & 0xff00) << 24;

  return build_int_cst (alpha_dimode_u, temp);
}

static tree
alpha_fold_builtin_unpkbw (unsigned HOST_WIDE_INT opint[], long op_const)
{
  unsigned HOST_WIDE_INT temp;

  if (op_const == 0)
    return NULL;

  temp = opint[0] & 0xff;
  temp |= (opint[0] & 0x0000ff00) << 8;
  temp |= (opint[0] & 0x00ff0000) << 16;
  temp |= (opint[0] & 0xff000000) << 24;

  return build_int_cst (alpha_dimode_u, temp);
}

static tree
alpha_fold_builtin_cttz (unsigned HOST_WIDE_INT opint[], long op_const)
{
  unsigned HOST_WIDE_INT temp;

  if (op_const == 0)
    return NULL;

  if (opint[0] == 0)
    temp = 64;
  else
    temp = exact_log2 (opint[0] & -opint[0]);

  return build_int_cst (alpha_dimode_u, temp);
}

static tree
alpha_fold_builtin_ctlz (unsigned HOST_WIDE_INT opint[], long op_const)
{
  unsigned HOST_WIDE_INT temp;

  if (op_const == 0)
    return NULL;

  if (opint[0] == 0)
    temp = 64;
  else
    temp = 64 - floor_log2 (opint[0]) - 1;

  return build_int_cst (alpha_dimode_u, temp);
}

static tree
alpha_fold_builtin_ctpop (unsigned HOST_WIDE_INT opint[], long op_const)
{
  unsigned HOST_WIDE_INT temp, op;

  if (op_const == 0)
    return NULL;

  op = opint[0];
  temp = 0;
  while (op)
    temp++, op &= op - 1;

  return build_int_cst (alpha_dimode_u, temp);
}

/* Fold one of our builtin functions.  */

static tree
alpha_fold_builtin (tree fndecl, int n_args, tree *op,
		    bool ignore ATTRIBUTE_UNUSED)
{
  unsigned HOST_WIDE_INT opint[MAX_ARGS];
  long op_const = 0;
  int i;

  if (n_args > MAX_ARGS)
    return NULL;

  for (i = 0; i < n_args; i++)
    {
      tree arg = op[i];
      if (arg == error_mark_node)
	return NULL;

      opint[i] = 0;
      if (TREE_CODE (arg) == INTEGER_CST)
	{
          op_const |= 1L << i;
	  opint[i] = int_cst_value (arg);
	}
    }

  switch (DECL_FUNCTION_CODE (fndecl))
    {
    case ALPHA_BUILTIN_CMPBGE:
      return alpha_fold_builtin_cmpbge (opint, op_const);

    case ALPHA_BUILTIN_EXTBL:
      return alpha_fold_builtin_extxx (op, opint, op_const, 0x01, false);
    case ALPHA_BUILTIN_EXTWL:
      return alpha_fold_builtin_extxx (op, opint, op_const, 0x03, false);
    case ALPHA_BUILTIN_EXTLL:
      return alpha_fold_builtin_extxx (op, opint, op_const, 0x0f, false);
    case ALPHA_BUILTIN_EXTQL:
      return alpha_fold_builtin_extxx (op, opint, op_const, 0xff, false);
    case ALPHA_BUILTIN_EXTWH:
      return alpha_fold_builtin_extxx (op, opint, op_const, 0x03, true);
    case ALPHA_BUILTIN_EXTLH:
      return alpha_fold_builtin_extxx (op, opint, op_const, 0x0f, true);
    case ALPHA_BUILTIN_EXTQH:
      return alpha_fold_builtin_extxx (op, opint, op_const, 0xff, true);

    case ALPHA_BUILTIN_INSBL:
      return alpha_fold_builtin_insxx (op, opint, op_const, 0x01, false);
    case ALPHA_BUILTIN_INSWL:
      return alpha_fold_builtin_insxx (op, opint, op_const, 0x03, false);
    case ALPHA_BUILTIN_INSLL:
      return alpha_fold_builtin_insxx (op, opint, op_const, 0x0f, false);
    case ALPHA_BUILTIN_INSQL:
      return alpha_fold_builtin_insxx (op, opint, op_const, 0xff, false);
    case ALPHA_BUILTIN_INSWH:
      return alpha_fold_builtin_insxx (op, opint, op_const, 0x03, true);
    case ALPHA_BUILTIN_INSLH:
      return alpha_fold_builtin_insxx (op, opint, op_const, 0x0f, true);
    case ALPHA_BUILTIN_INSQH:
      return alpha_fold_builtin_insxx (op, opint, op_const, 0xff, true);

    case ALPHA_BUILTIN_MSKBL:
      return alpha_fold_builtin_mskxx (op, opint, op_const, 0x01, false);
    case ALPHA_BUILTIN_MSKWL:
      return alpha_fold_builtin_mskxx (op, opint, op_const, 0x03, false);
    case ALPHA_BUILTIN_MSKLL:
      return alpha_fold_builtin_mskxx (op, opint, op_const, 0x0f, false);
    case ALPHA_BUILTIN_MSKQL:
      return alpha_fold_builtin_mskxx (op, opint, op_const, 0xff, false);
    case ALPHA_BUILTIN_MSKWH:
      return alpha_fold_builtin_mskxx (op, opint, op_const, 0x03, true);
    case ALPHA_BUILTIN_MSKLH:
      return alpha_fold_builtin_mskxx (op, opint, op_const, 0x0f, true);
    case ALPHA_BUILTIN_MSKQH:
      return alpha_fold_builtin_mskxx (op, opint, op_const, 0xff, true);

    case ALPHA_BUILTIN_ZAP:
      opint[1] ^= 0xff;
      /* FALLTHRU */
    case ALPHA_BUILTIN_ZAPNOT:
      return alpha_fold_builtin_zapnot (op, opint, op_const);

    case ALPHA_BUILTIN_MINUB8:
      return alpha_fold_vector_minmax (MIN_EXPR, op, alpha_v8qi_u);
    case ALPHA_BUILTIN_MINSB8:
      return alpha_fold_vector_minmax (MIN_EXPR, op, alpha_v8qi_s);
    case ALPHA_BUILTIN_MINUW4:
      return alpha_fold_vector_minmax (MIN_EXPR, op, alpha_v4hi_u);
    case ALPHA_BUILTIN_MINSW4:
      return alpha_fold_vector_minmax (MIN_EXPR, op, alpha_v4hi_s);
    case ALPHA_BUILTIN_MAXUB8:
      return alpha_fold_vector_minmax (MAX_EXPR, op, alpha_v8qi_u);
    case ALPHA_BUILTIN_MAXSB8:
      return alpha_fold_vector_minmax (MAX_EXPR, op, alpha_v8qi_s);
    case ALPHA_BUILTIN_MAXUW4:
      return alpha_fold_vector_minmax (MAX_EXPR, op, alpha_v4hi_u);
    case ALPHA_BUILTIN_MAXSW4:
      return alpha_fold_vector_minmax (MAX_EXPR, op, alpha_v4hi_s);

    case ALPHA_BUILTIN_PERR:
      return alpha_fold_builtin_perr (opint, op_const);
    case ALPHA_BUILTIN_PKLB:
      return alpha_fold_builtin_pklb (opint, op_const);
    case ALPHA_BUILTIN_PKWB:
      return alpha_fold_builtin_pkwb (opint, op_const);
    case ALPHA_BUILTIN_UNPKBL:
      return alpha_fold_builtin_unpkbl (opint, op_const);
    case ALPHA_BUILTIN_UNPKBW:
      return alpha_fold_builtin_unpkbw (opint, op_const);

    case ALPHA_BUILTIN_CTTZ:
      return alpha_fold_builtin_cttz (opint, op_const);
    case ALPHA_BUILTIN_CTLZ:
      return alpha_fold_builtin_ctlz (opint, op_const);
    case ALPHA_BUILTIN_CTPOP:
      return alpha_fold_builtin_ctpop (opint, op_const);

    case ALPHA_BUILTIN_AMASK:
    case ALPHA_BUILTIN_IMPLVER:
    case ALPHA_BUILTIN_RPCC:
      /* None of these are foldable at compile-time.  */
    default:
      return NULL;
    }
}

bool
alpha_gimple_fold_builtin (gimple_stmt_iterator *gsi)
{
  bool changed = false;
  gimple stmt = gsi_stmt (*gsi);
  tree call = gimple_call_fn (stmt);
  gimple new_stmt = NULL;

  if (call)
    {
      tree fndecl = gimple_call_fndecl (stmt);

      if (fndecl)
	{
	  tree arg0, arg1;

	  switch (DECL_FUNCTION_CODE (fndecl))
	    {
	    case ALPHA_BUILTIN_UMULH:
	      arg0 = gimple_call_arg (stmt, 0);
	      arg1 = gimple_call_arg (stmt, 1);

	      new_stmt
		= gimple_build_assign_with_ops (MULT_HIGHPART_EXPR,
						gimple_call_lhs (stmt),
						arg0,
						arg1);
	      break;
	    default:
	      break;
	    }
	}
    }

  if (new_stmt)
    {
      gsi_replace (gsi, new_stmt, true);
      changed = true;
    }

  return changed;
}

/* This page contains routines that are used to determine what the function
   prologue and epilogue code will do and write them out.  */

/* Compute the size of the save area in the stack.  */

/* These variables are used for communication between the following functions.
   They indicate various things about the current function being compiled
   that are used to tell what kind of prologue, epilogue and procedure
   descriptor to generate.  */

/* Nonzero if we need a stack procedure.  */
enum alpha_procedure_types {PT_NULL = 0, PT_REGISTER = 1, PT_STACK = 2};
static enum alpha_procedure_types alpha_procedure_type;

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
alpha_sa_mask (unsigned long *imaskP, unsigned long *fmaskP)
{
  unsigned long imask = 0;
  unsigned long fmask = 0;
  unsigned int i;

  /* When outputting a thunk, we don't have valid register life info,
     but assemble_start_function wants to output .frame and .mask
     directives.  */
  if (cfun->is_thunk)
    {
      *imaskP = 0;
      *fmaskP = 0;
      return;
    }

  if (TARGET_ABI_OPEN_VMS && alpha_procedure_type == PT_STACK)
    imask |= (1UL << HARD_FRAME_POINTER_REGNUM);

  /* One for every register we have to save.  */
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if (! fixed_regs[i] && ! call_used_regs[i]
	&& df_regs_ever_live_p (i) && i != REG_RA)
      {
	if (i < 32)
	  imask |= (1UL << i);
	else
	  fmask |= (1UL << (i - 32));
      }

  /* We need to restore these for the handler.  */
  if (crtl->calls_eh_return)
    {
      for (i = 0; ; ++i)
	{
	  unsigned regno = EH_RETURN_DATA_REGNO (i);
	  if (regno == INVALID_REGNUM)
	    break;
	  imask |= 1UL << regno;
	}
    }

  /* If any register spilled, then spill the return address also.  */
  /* ??? This is required by the Digital stack unwind specification
     and isn't needed if we're doing Dwarf2 unwinding.  */
  if (imask || fmask || alpha_ra_ever_killed ())
    imask |= (1UL << REG_RA);

  *imaskP = imask;
  *fmaskP = fmask;
}

int
alpha_sa_size (void)
{
  unsigned long mask[2];
  int sa_size = 0;
  int i, j;

  alpha_sa_mask (&mask[0], &mask[1]);

  for (j = 0; j < 2; ++j)
    for (i = 0; i < 32; ++i)
      if ((mask[j] >> i) & 1)
	sa_size++;

  if (TARGET_ABI_OPEN_VMS)
    {
      /* Start with a stack procedure if we make any calls (REG_RA used), or
	 need a frame pointer, with a register procedure if we otherwise need
	 at least a slot, and with a null procedure in other cases.  */
      if ((mask[0] >> REG_RA) & 1 || frame_pointer_needed)
	alpha_procedure_type = PT_STACK;
      else if (get_frame_size() != 0)
	alpha_procedure_type = PT_REGISTER;
      else
	alpha_procedure_type = PT_NULL;

      /* Don't reserve space for saving FP & RA yet.  Do that later after we've
	 made the final decision on stack procedure vs register procedure.  */
      if (alpha_procedure_type == PT_STACK)
	sa_size -= 2;

      /* Decide whether to refer to objects off our PV via FP or PV.
	 If we need FP for something else or if we receive a nonlocal
	 goto (which expects PV to contain the value), we must use PV.
	 Otherwise, start by assuming we can use FP.  */

      vms_base_regno
	= (frame_pointer_needed
	   || cfun->has_nonlocal_label
	   || alpha_procedure_type == PT_STACK
	   || crtl->outgoing_args_size)
	  ? REG_PV : HARD_FRAME_POINTER_REGNUM;

      /* If we want to copy PV into FP, we need to find some register
	 in which to save FP.  */

      vms_save_fp_regno = -1;
      if (vms_base_regno == HARD_FRAME_POINTER_REGNUM)
	for (i = 0; i < 32; i++)
	  if (! fixed_regs[i] && call_used_regs[i] && ! df_regs_ever_live_p (i))
	    vms_save_fp_regno = i;

      /* A VMS condition handler requires a stack procedure in our
	 implementation. (not required by the calling standard).  */
      if ((vms_save_fp_regno == -1 && alpha_procedure_type == PT_REGISTER)
	  || cfun->machine->uses_condition_handler)
	vms_base_regno = REG_PV, alpha_procedure_type = PT_STACK;
      else if (alpha_procedure_type == PT_NULL)
	vms_base_regno = REG_PV;

      /* Stack unwinding should be done via FP unless we use it for PV.  */
      vms_unwind_regno = (vms_base_regno == REG_PV
			  ? HARD_FRAME_POINTER_REGNUM : STACK_POINTER_REGNUM);

      /* If this is a stack procedure, allow space for saving FP, RA and
	 a condition handler slot if needed.  */
      if (alpha_procedure_type == PT_STACK)
	sa_size += 2 + cfun->machine->uses_condition_handler;
    }
  else
    {
      /* Our size must be even (multiple of 16 bytes).  */
      if (sa_size & 1)
	sa_size++;
    }

  return sa_size * 8;
}

/* Define the offset between two registers, one to be eliminated,
   and the other its replacement, at the start of a routine.  */

HOST_WIDE_INT
alpha_initial_elimination_offset (unsigned int from,
				  unsigned int to ATTRIBUTE_UNUSED)
{
  HOST_WIDE_INT ret;

  ret = alpha_sa_size ();
  ret += ALPHA_ROUND (crtl->outgoing_args_size);

  switch (from)
    {
    case FRAME_POINTER_REGNUM:
      break;

    case ARG_POINTER_REGNUM:
      ret += (ALPHA_ROUND (get_frame_size ()
			   + crtl->args.pretend_args_size)
	      - crtl->args.pretend_args_size);
      break;

    default:
      gcc_unreachable ();
    }

  return ret;
}

#if TARGET_ABI_OPEN_VMS

/* Worker function for TARGET_CAN_ELIMINATE.  */

static bool
alpha_vms_can_eliminate (const int from ATTRIBUTE_UNUSED, const int to)
{
  /* We need the alpha_procedure_type to decide. Evaluate it now.  */
  alpha_sa_size ();

  switch (alpha_procedure_type)
    {
    case PT_NULL:
      /* NULL procedures have no frame of their own and we only
	 know how to resolve from the current stack pointer.  */
      return to == STACK_POINTER_REGNUM;

    case PT_REGISTER:
    case PT_STACK:
      /* We always eliminate except to the stack pointer if there is no
	 usable frame pointer at hand.  */
      return (to != STACK_POINTER_REGNUM
	      || vms_unwind_regno != HARD_FRAME_POINTER_REGNUM);
    }

  gcc_unreachable ();
}

/* FROM is to be eliminated for TO. Return the offset so that TO+offset
   designates the same location as FROM.  */

HOST_WIDE_INT
alpha_vms_initial_elimination_offset (unsigned int from, unsigned int to)
{ 
  /* The only possible attempts we ever expect are ARG or FRAME_PTR to
     HARD_FRAME or STACK_PTR.  We need the alpha_procedure_type to decide
     on the proper computations and will need the register save area size
     in most cases.  */

  HOST_WIDE_INT sa_size = alpha_sa_size ();

  /* PT_NULL procedures have no frame of their own and we only allow
     elimination to the stack pointer. This is the argument pointer and we
     resolve the soft frame pointer to that as well.  */
     
  if (alpha_procedure_type == PT_NULL)
    return 0;

  /* For a PT_STACK procedure the frame layout looks as follows

                      -----> decreasing addresses

		   <             size rounded up to 16       |   likewise   >
     --------------#------------------------------+++--------------+++-------#
     incoming args # pretended args | "frame" | regs sa | PV | outgoing args #
     --------------#---------------------------------------------------------#
                                   ^         ^              ^               ^
			      ARG_PTR FRAME_PTR HARD_FRAME_PTR       STACK_PTR

			      
     PT_REGISTER procedures are similar in that they may have a frame of their
     own. They have no regs-sa/pv/outgoing-args area.

     We first compute offset to HARD_FRAME_PTR, then add what we need to get
     to STACK_PTR if need be.  */
  
  {
    HOST_WIDE_INT offset;
    HOST_WIDE_INT pv_save_size = alpha_procedure_type == PT_STACK ? 8 : 0;

    switch (from)
      {
      case FRAME_POINTER_REGNUM:
	offset = ALPHA_ROUND (sa_size + pv_save_size);
	break;
      case ARG_POINTER_REGNUM:
	offset = (ALPHA_ROUND (sa_size + pv_save_size
			       + get_frame_size ()
			       + crtl->args.pretend_args_size)
		  - crtl->args.pretend_args_size);
	break;
      default:
	gcc_unreachable ();
      }
    
    if (to == STACK_POINTER_REGNUM)
      offset += ALPHA_ROUND (crtl->outgoing_args_size);
    
    return offset;
  }
}

#define COMMON_OBJECT "common_object"

static tree
common_object_handler (tree *node, tree name ATTRIBUTE_UNUSED,
		       tree args ATTRIBUTE_UNUSED, int flags ATTRIBUTE_UNUSED,
		       bool *no_add_attrs ATTRIBUTE_UNUSED)
{
  tree decl = *node;
  gcc_assert (DECL_P (decl));

  DECL_COMMON (decl) = 1;
  return NULL_TREE;
}

static const struct attribute_spec vms_attribute_table[] =
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req, handler,
       affects_type_identity } */
  { COMMON_OBJECT,   0, 1, true,  false, false, common_object_handler, false },
  { NULL,            0, 0, false, false, false, NULL, false }
};

void
vms_output_aligned_decl_common(FILE *file, tree decl, const char *name,
			       unsigned HOST_WIDE_INT size,
			       unsigned int align)
{
  tree attr = DECL_ATTRIBUTES (decl);
  fprintf (file, "%s", COMMON_ASM_OP);
  assemble_name (file, name);
  fprintf (file, "," HOST_WIDE_INT_PRINT_UNSIGNED, size);
  /* ??? Unlike on OSF/1, the alignment factor is not in log units.  */
  fprintf (file, ",%u", align / BITS_PER_UNIT);
  if (attr)
    {
      attr = lookup_attribute (COMMON_OBJECT, attr);
      if (attr)
        fprintf (file, ",%s",
		 IDENTIFIER_POINTER (TREE_VALUE (TREE_VALUE (attr))));
    }
  fputc ('\n', file);
}

#undef COMMON_OBJECT

#endif

static int
find_lo_sum_using_gp (rtx *px, void *data ATTRIBUTE_UNUSED)
{
  return GET_CODE (*px) == LO_SUM && XEXP (*px, 0) == pic_offset_table_rtx;
}

int
alpha_find_lo_sum_using_gp (rtx insn)
{
  return for_each_rtx (&PATTERN (insn), find_lo_sum_using_gp, NULL) > 0;
}

static int
alpha_does_function_need_gp (void)
{
  rtx insn;

  /* The GP being variable is an OSF abi thing.  */
  if (! TARGET_ABI_OSF)
    return 0;

  /* We need the gp to load the address of __mcount.  */
  if (TARGET_PROFILING_NEEDS_GP && crtl->profile)
    return 1;

  /* The code emitted by alpha_output_mi_thunk_osf uses the gp.  */
  if (cfun->is_thunk)
    return 1;

  /* The nonlocal receiver pattern assumes that the gp is valid for
     the nested function.  Reasonable because it's almost always set
     correctly already.  For the cases where that's wrong, make sure
     the nested function loads its gp on entry.  */
  if (crtl->has_nonlocal_goto)
    return 1;

  /* If we need a GP (we have a LDSYM insn or a CALL_INSN), load it first.
     Even if we are a static function, we still need to do this in case
     our address is taken and passed to something like qsort.  */

  push_topmost_sequence ();
  insn = get_insns ();
  pop_topmost_sequence ();

  for (; insn; insn = NEXT_INSN (insn))
    if (NONDEBUG_INSN_P (insn)
	&& GET_CODE (PATTERN (insn)) != USE
	&& GET_CODE (PATTERN (insn)) != CLOBBER
	&& get_attr_usegp (insn))
      return 1;

  return 0;
}


/* Helper function to set RTX_FRAME_RELATED_P on instructions, including
   sequences.  */

static rtx
set_frame_related_p (void)
{
  rtx seq = get_insns ();
  rtx insn;

  end_sequence ();

  if (!seq)
    return NULL_RTX;

  if (INSN_P (seq))
    {
      insn = seq;
      while (insn != NULL_RTX)
	{
	  RTX_FRAME_RELATED_P (insn) = 1;
	  insn = NEXT_INSN (insn);
	}
      seq = emit_insn (seq);
    }
  else
    {
      seq = emit_insn (seq);
      RTX_FRAME_RELATED_P (seq) = 1;
    }
  return seq;
}

#define FRP(exp)  (start_sequence (), exp, set_frame_related_p ())

/* Generates a store with the proper unwind info attached.  VALUE is
   stored at BASE_REG+BASE_OFS.  If FRAME_BIAS is nonzero, then BASE_REG
   contains SP+FRAME_BIAS, and that is the unwind info that should be
   generated.  If FRAME_REG != VALUE, then VALUE is being stored on
   behalf of FRAME_REG, and FRAME_REG should be present in the unwind.  */

static void
emit_frame_store_1 (rtx value, rtx base_reg, HOST_WIDE_INT frame_bias,
		    HOST_WIDE_INT base_ofs, rtx frame_reg)
{
  rtx addr, mem, insn;

  addr = plus_constant (Pmode, base_reg, base_ofs);
  mem = gen_frame_mem (DImode, addr);

  insn = emit_move_insn (mem, value);
  RTX_FRAME_RELATED_P (insn) = 1;

  if (frame_bias || value != frame_reg)
    {
      if (frame_bias)
	{
	  addr = plus_constant (Pmode, stack_pointer_rtx,
			        frame_bias + base_ofs);
	  mem = gen_rtx_MEM (DImode, addr);
	}

      add_reg_note (insn, REG_FRAME_RELATED_EXPR,
		    gen_rtx_SET (VOIDmode, mem, frame_reg));
    }
}

static void
emit_frame_store (unsigned int regno, rtx base_reg,
		  HOST_WIDE_INT frame_bias, HOST_WIDE_INT base_ofs)
{
  rtx reg = gen_rtx_REG (DImode, regno);
  emit_frame_store_1 (reg, base_reg, frame_bias, base_ofs, reg);
}

/* Compute the frame size.  SIZE is the size of the "naked" frame
   and SA_SIZE is the size of the register save area.  */

static HOST_WIDE_INT
compute_frame_size (HOST_WIDE_INT size, HOST_WIDE_INT sa_size)
{
  if (TARGET_ABI_OPEN_VMS)
    return ALPHA_ROUND (sa_size 
			+ (alpha_procedure_type == PT_STACK ? 8 : 0)
			+ size
			+ crtl->args.pretend_args_size);
  else
    return ALPHA_ROUND (crtl->outgoing_args_size)
	   + sa_size
	   + ALPHA_ROUND (size
			  + crtl->args.pretend_args_size);
}

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
alpha_expand_prologue (void)
{
  /* Registers to save.  */
  unsigned long imask = 0;
  unsigned long fmask = 0;
  /* Stack space needed for pushing registers clobbered by us.  */
  HOST_WIDE_INT sa_size, sa_bias;
  /* Complete stack size needed.  */
  HOST_WIDE_INT frame_size;
  /* Probed stack size; it additionally includes the size of
     the "reserve region" if any.  */
  HOST_WIDE_INT probed_size;
  /* Offset from base reg to register save area.  */
  HOST_WIDE_INT reg_offset;
  rtx sa_reg;
  int i;

  sa_size = alpha_sa_size ();
  frame_size = compute_frame_size (get_frame_size (), sa_size);

  if (flag_stack_usage_info)
    current_function_static_stack_size = frame_size;

  if (TARGET_ABI_OPEN_VMS)
    reg_offset = 8 + 8 * cfun->machine->uses_condition_handler;
  else
    reg_offset = ALPHA_ROUND (crtl->outgoing_args_size);

  alpha_sa_mask (&imask, &fmask);

  /* Emit an insn to reload GP, if needed.  */
  if (TARGET_ABI_OSF)
    {
      alpha_function_needs_gp = alpha_does_function_need_gp ();
      if (alpha_function_needs_gp)
	emit_insn (gen_prologue_ldgp ());
    }

  /* TARGET_PROFILING_NEEDS_GP actually implies that we need to insert
     the call to mcount ourselves, rather than having the linker do it
     magically in response to -pg.  Since _mcount has special linkage,
     don't represent the call as a call.  */
  if (TARGET_PROFILING_NEEDS_GP && crtl->profile)
    emit_insn (gen_prologue_mcount ());

  /* Adjust the stack by the frame size.  If the frame size is > 4096
     bytes, we need to be sure we probe somewhere in the first and last
     4096 bytes (we can probably get away without the latter test) and
     every 8192 bytes in between.  If the frame size is > 32768, we
     do this in a loop.  Otherwise, we generate the explicit probe
     instructions.

     Note that we are only allowed to adjust sp once in the prologue.  */

  probed_size = frame_size;
  if (flag_stack_check)
    probed_size += STACK_CHECK_PROTECT;

  if (probed_size <= 32768)
    {
      if (probed_size > 4096)
	{
	  int probed;

	  for (probed = 4096; probed < probed_size; probed += 8192)
	    emit_insn (gen_probe_stack (GEN_INT (-probed)));

	  /* We only have to do this probe if we aren't saving registers or
	     if we are probing beyond the frame because of -fstack-check.  */
	  if ((sa_size == 0 && probed_size > probed - 4096)
	      || flag_stack_check)
	    emit_insn (gen_probe_stack (GEN_INT (-probed_size)));
	}

      if (frame_size != 0)
	FRP (emit_insn (gen_adddi3 (stack_pointer_rtx, stack_pointer_rtx,
				    GEN_INT (-frame_size))));
    }
  else
    {
      /* Here we generate code to set R22 to SP + 4096 and set R23 to the
	 number of 8192 byte blocks to probe.  We then probe each block
	 in the loop and then set SP to the proper location.  If the
	 amount remaining is > 4096, we have to do one more probe if we
	 are not saving any registers or if we are probing beyond the
	 frame because of -fstack-check.  */

      HOST_WIDE_INT blocks = (probed_size + 4096) / 8192;
      HOST_WIDE_INT leftover = probed_size + 4096 - blocks * 8192;
      rtx ptr = gen_rtx_REG (DImode, 22);
      rtx count = gen_rtx_REG (DImode, 23);
      rtx seq;

      emit_move_insn (count, GEN_INT (blocks));
      emit_insn (gen_adddi3 (ptr, stack_pointer_rtx, GEN_INT (4096)));

      /* Because of the difficulty in emitting a new basic block this
	 late in the compilation, generate the loop as a single insn.  */
      emit_insn (gen_prologue_stack_probe_loop (count, ptr));

      if ((leftover > 4096 && sa_size == 0) || flag_stack_check)
	{
	  rtx last = gen_rtx_MEM (DImode,
				  plus_constant (Pmode, ptr, -leftover));
	  MEM_VOLATILE_P (last) = 1;
	  emit_move_insn (last, const0_rtx);
	}

      if (flag_stack_check)
	{
	  /* If -fstack-check is specified we have to load the entire
	     constant into a register and subtract from the sp in one go,
	     because the probed stack size is not equal to the frame size.  */
	  HOST_WIDE_INT lo, hi;
	  lo = ((frame_size & 0xffff) ^ 0x8000) - 0x8000;
	  hi = frame_size - lo;

	  emit_move_insn (ptr, GEN_INT (hi));
	  emit_insn (gen_adddi3 (ptr, ptr, GEN_INT (lo)));
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
      add_reg_note (seq, REG_FRAME_RELATED_EXPR,
		    gen_rtx_SET (VOIDmode, stack_pointer_rtx,
				 plus_constant (Pmode, stack_pointer_rtx,
						-frame_size)));
    }

  /* Cope with very large offsets to the register save area.  */
  sa_bias = 0;
  sa_reg = stack_pointer_rtx;
  if (reg_offset + sa_size > 0x8000)
    {
      int low = ((reg_offset & 0xffff) ^ 0x8000) - 0x8000;
      rtx sa_bias_rtx;

      if (low + sa_size <= 0x8000)
	sa_bias = reg_offset - low, reg_offset = low;
      else
	sa_bias = reg_offset, reg_offset = 0;

      sa_reg = gen_rtx_REG (DImode, 24);
      sa_bias_rtx = GEN_INT (sa_bias);

      if (add_operand (sa_bias_rtx, DImode))
	emit_insn (gen_adddi3 (sa_reg, stack_pointer_rtx, sa_bias_rtx));
      else
	{
	  emit_move_insn (sa_reg, sa_bias_rtx);
	  emit_insn (gen_adddi3 (sa_reg, stack_pointer_rtx, sa_reg));
	}
    }

  /* Save regs in stack order.  Beginning with VMS PV.  */
  if (TARGET_ABI_OPEN_VMS && alpha_procedure_type == PT_STACK)
    emit_frame_store (REG_PV, stack_pointer_rtx, 0, 0);

  /* Save register RA next.  */
  if (imask & (1UL << REG_RA))
    {
      emit_frame_store (REG_RA, sa_reg, sa_bias, reg_offset);
      imask &= ~(1UL << REG_RA);
      reg_offset += 8;
    }

  /* Now save any other registers required to be saved.  */
  for (i = 0; i < 31; i++)
    if (imask & (1UL << i))
      {
	emit_frame_store (i, sa_reg, sa_bias, reg_offset);
	reg_offset += 8;
      }

  for (i = 0; i < 31; i++)
    if (fmask & (1UL << i))
      {
	emit_frame_store (i+32, sa_reg, sa_bias, reg_offset);
	reg_offset += 8;
      }

  if (TARGET_ABI_OPEN_VMS)
    {
      /* Register frame procedures save the fp.  */
      if (alpha_procedure_type == PT_REGISTER)
	{
	  rtx insn = emit_move_insn (gen_rtx_REG (DImode, vms_save_fp_regno),
				     hard_frame_pointer_rtx);
	  add_reg_note (insn, REG_CFA_REGISTER, NULL);
	  RTX_FRAME_RELATED_P (insn) = 1;
	}

      if (alpha_procedure_type != PT_NULL && vms_base_regno != REG_PV)
	emit_insn (gen_force_movdi (gen_rtx_REG (DImode, vms_base_regno),
				    gen_rtx_REG (DImode, REG_PV)));

      if (alpha_procedure_type != PT_NULL
	  && vms_unwind_regno == HARD_FRAME_POINTER_REGNUM)
	FRP (emit_move_insn (hard_frame_pointer_rtx, stack_pointer_rtx));

      /* If we have to allocate space for outgoing args, do it now.  */
      if (crtl->outgoing_args_size != 0)
	{
	  rtx seq
	    = emit_move_insn (stack_pointer_rtx,
			      plus_constant
			      (Pmode, hard_frame_pointer_rtx,
			       - (ALPHA_ROUND
				  (crtl->outgoing_args_size))));

	  /* Only set FRAME_RELATED_P on the stack adjustment we just emitted
	     if ! frame_pointer_needed. Setting the bit will change the CFA
	     computation rule to use sp again, which would be wrong if we had
	     frame_pointer_needed, as this means sp might move unpredictably
	     later on.

	     Also, note that
	       frame_pointer_needed
	       => vms_unwind_regno == HARD_FRAME_POINTER_REGNUM
	     and
	       crtl->outgoing_args_size != 0
	       => alpha_procedure_type != PT_NULL,

	     so when we are not setting the bit here, we are guaranteed to
	     have emitted an FRP frame pointer update just before.  */
	  RTX_FRAME_RELATED_P (seq) = ! frame_pointer_needed;
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
	    /* This must always be the last instruction in the
	       prologue, thus we emit a special move + clobber.  */
	      FRP (emit_insn (gen_init_fp (hard_frame_pointer_rtx,
				           stack_pointer_rtx, sa_reg)));
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

/* Count the number of .file directives, so that .loc is up to date.  */
int num_source_filenames = 0;

/* Output the textual info surrounding the prologue.  */

void
alpha_start_function (FILE *file, const char *fnname,
		      tree decl ATTRIBUTE_UNUSED)
{
  unsigned long imask = 0;
  unsigned long fmask = 0;
  /* Stack space needed for pushing registers clobbered by us.  */
  HOST_WIDE_INT sa_size;
  /* Complete stack size needed.  */
  unsigned HOST_WIDE_INT frame_size;
  /* The maximum debuggable frame size.  */
  unsigned HOST_WIDE_INT max_frame_size = 1UL << 31;
  /* Offset from base reg to register save area.  */
  HOST_WIDE_INT reg_offset;
  char *entry_label = (char *) alloca (strlen (fnname) + 6);
  char *tramp_label = (char *) alloca (strlen (fnname) + 6);
  int i;

#if TARGET_ABI_OPEN_VMS
  vms_start_function (fnname);
#endif

  alpha_fnname = fnname;
  sa_size = alpha_sa_size ();
  frame_size = compute_frame_size (get_frame_size (), sa_size);

  if (TARGET_ABI_OPEN_VMS)
    reg_offset = 8 + 8 * cfun->machine->uses_condition_handler;
  else
    reg_offset = ALPHA_ROUND (crtl->outgoing_args_size);

  alpha_sa_mask (&imask, &fmask);

  /* Issue function start and label.  */
  if (TARGET_ABI_OPEN_VMS || !flag_inhibit_size_directive)
    {
      fputs ("\t.ent ", file);
      assemble_name (file, fnname);
      putc ('\n', file);

      /* If the function needs GP, we'll write the "..ng" label there.
	 Otherwise, do it here.  */
      if (TARGET_ABI_OSF
          && ! alpha_function_needs_gp
	  && ! cfun->is_thunk)
	{
	  putc ('$', file);
	  assemble_name (file, fnname);
	  fputs ("..ng:\n", file);
	}
    }
  /* Nested functions on VMS that are potentially called via trampoline
     get a special transfer entry point that loads the called functions
     procedure descriptor and static chain.  */
   if (TARGET_ABI_OPEN_VMS
       && !TREE_PUBLIC (decl)
       && DECL_CONTEXT (decl)
       && !TYPE_P (DECL_CONTEXT (decl))
       && TREE_CODE (DECL_CONTEXT (decl)) != TRANSLATION_UNIT_DECL)
     {
	strcpy (tramp_label, fnname);
	strcat (tramp_label, "..tr");
	ASM_OUTPUT_LABEL (file, tramp_label);
	fprintf (file, "\tldq $1,24($27)\n");
	fprintf (file, "\tldq $27,16($27)\n");
     }

  strcpy (entry_label, fnname);
  if (TARGET_ABI_OPEN_VMS)
    strcat (entry_label, "..en");

  ASM_OUTPUT_LABEL (file, entry_label);
  inside_function = TRUE;

  if (TARGET_ABI_OPEN_VMS)
    fprintf (file, "\t.base $%d\n", vms_base_regno);

  if (TARGET_ABI_OSF
      && TARGET_IEEE_CONFORMANT
      && !flag_inhibit_size_directive)
    {
      /* Set flags in procedure descriptor to request IEEE-conformant
	 math-library routines.  The value we set it to is PDSC_EXC_IEEE
	 (/usr/include/pdsc.h).  */
      fputs ("\t.eflag 48\n", file);
    }

  /* Set up offsets to alpha virtual arg/local debugging pointer.  */
  alpha_auto_offset = -frame_size + crtl->args.pretend_args_size;
  alpha_arg_offset = -frame_size + 48;

  /* Describe our frame.  If the frame size is larger than an integer,
     print it as zero to avoid an assembler error.  We won't be
     properly describing such a frame, but that's the best we can do.  */
  if (TARGET_ABI_OPEN_VMS)
    fprintf (file, "\t.frame $%d," HOST_WIDE_INT_PRINT_DEC ",$26,"
	     HOST_WIDE_INT_PRINT_DEC "\n",
	     vms_unwind_regno,
	     frame_size >= (1UL << 31) ? 0 : frame_size,
	     reg_offset);
  else if (!flag_inhibit_size_directive)
    fprintf (file, "\t.frame $%d," HOST_WIDE_INT_PRINT_DEC ",$26,%d\n",
	     (frame_pointer_needed
	      ? HARD_FRAME_POINTER_REGNUM : STACK_POINTER_REGNUM),
	     frame_size >= max_frame_size ? 0 : frame_size,
	     crtl->args.pretend_args_size);

  /* Describe which registers were spilled.  */
  if (TARGET_ABI_OPEN_VMS)
    {
      if (imask)
        /* ??? Does VMS care if mask contains ra?  The old code didn't
           set it, so I don't here.  */
	fprintf (file, "\t.mask 0x%lx,0\n", imask & ~(1UL << REG_RA));
      if (fmask)
	fprintf (file, "\t.fmask 0x%lx,0\n", fmask);
      if (alpha_procedure_type == PT_REGISTER)
	fprintf (file, "\t.fp_save $%d\n", vms_save_fp_regno);
    }
  else if (!flag_inhibit_size_directive)
    {
      if (imask)
	{
	  fprintf (file, "\t.mask 0x%lx," HOST_WIDE_INT_PRINT_DEC "\n", imask,
		   frame_size >= max_frame_size ? 0 : reg_offset - frame_size);

	  for (i = 0; i < 32; ++i)
	    if (imask & (1UL << i))
	      reg_offset += 8;
	}

      if (fmask)
	fprintf (file, "\t.fmask 0x%lx," HOST_WIDE_INT_PRINT_DEC "\n", fmask,
		 frame_size >= max_frame_size ? 0 : reg_offset - frame_size);
    }

#if TARGET_ABI_OPEN_VMS
  /* If a user condition handler has been installed at some point, emit
     the procedure descriptor bits to point the Condition Handling Facility
     at the indirection wrapper, and state the fp offset at which the user
     handler may be found.  */
  if (cfun->machine->uses_condition_handler)
    {
      fprintf (file, "\t.handler __gcc_shell_handler\n");
      fprintf (file, "\t.handler_data %d\n", VMS_COND_HANDLER_FP_OFFSET);
    }

#ifdef TARGET_VMS_CRASH_DEBUG
  /* Support of minimal traceback info.  */
  switch_to_section (readonly_data_section);
  fprintf (file, "\t.align 3\n");
  assemble_name (file, fnname); fputs ("..na:\n", file);
  fputs ("\t.ascii \"", file);
  assemble_name (file, fnname);
  fputs ("\\0\"\n", file);
  switch_to_section (text_section);
#endif
#endif /* TARGET_ABI_OPEN_VMS */
}

/* Emit the .prologue note at the scheduled end of the prologue.  */

static void
alpha_output_function_end_prologue (FILE *file)
{
  if (TARGET_ABI_OPEN_VMS)
    fputs ("\t.prologue\n", file);
  else if (!flag_inhibit_size_directive)
    fprintf (file, "\t.prologue %d\n",
	     alpha_function_needs_gp || cfun->is_thunk);
}

/* Write function epilogue.  */

void
alpha_expand_epilogue (void)
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
  rtx sp_adj1, sp_adj2, mem, reg, insn;
  rtx eh_ofs;
  rtx cfa_restores = NULL_RTX;
  int i;

  sa_size = alpha_sa_size ();
  frame_size = compute_frame_size (get_frame_size (), sa_size);

  if (TARGET_ABI_OPEN_VMS)
    {
       if (alpha_procedure_type == PT_STACK)
          reg_offset = 8 + 8 * cfun->machine->uses_condition_handler;
       else
          reg_offset = 0;
    }
  else
    reg_offset = ALPHA_ROUND (crtl->outgoing_args_size);

  alpha_sa_mask (&imask, &fmask);

  fp_is_frame_pointer
    = (TARGET_ABI_OPEN_VMS
       ? alpha_procedure_type == PT_STACK
       : frame_pointer_needed);
  fp_offset = 0;
  sa_reg = stack_pointer_rtx;

  if (crtl->calls_eh_return)
    eh_ofs = EH_RETURN_STACKADJ_RTX;
  else
    eh_ofs = NULL_RTX;

  if (sa_size)
    {
      /* If we have a frame pointer, restore SP from it.  */
      if (TARGET_ABI_OPEN_VMS
	  ? vms_unwind_regno == HARD_FRAME_POINTER_REGNUM
	  : frame_pointer_needed)
	emit_move_insn (stack_pointer_rtx, hard_frame_pointer_rtx);

      /* Cope with very large offsets to the register save area.  */
      if (reg_offset + sa_size > 0x8000)
	{
	  int low = ((reg_offset & 0xffff) ^ 0x8000) - 0x8000;
	  HOST_WIDE_INT bias;

	  if (low + sa_size <= 0x8000)
	    bias = reg_offset - low, reg_offset = low;
	  else
	    bias = reg_offset, reg_offset = 0;

	  sa_reg = gen_rtx_REG (DImode, 22);
	  sa_reg_exp = plus_constant (Pmode, stack_pointer_rtx, bias);

	  emit_move_insn (sa_reg, sa_reg_exp);
	}

      /* Restore registers in order, excepting a true frame pointer.  */

      mem = gen_frame_mem (DImode, plus_constant (Pmode, sa_reg, reg_offset));
      reg = gen_rtx_REG (DImode, REG_RA);
      emit_move_insn (reg, mem);
      cfa_restores = alloc_reg_note (REG_CFA_RESTORE, reg, cfa_restores);

      reg_offset += 8;
      imask &= ~(1UL << REG_RA);

      for (i = 0; i < 31; ++i)
	if (imask & (1UL << i))
	  {
	    if (i == HARD_FRAME_POINTER_REGNUM && fp_is_frame_pointer)
	      fp_offset = reg_offset;
	    else
	      {
		mem = gen_frame_mem (DImode,
				     plus_constant (Pmode, sa_reg,
						    reg_offset));
		reg = gen_rtx_REG (DImode, i);
		emit_move_insn (reg, mem);
		cfa_restores = alloc_reg_note (REG_CFA_RESTORE, reg,
					       cfa_restores);
	      }
	    reg_offset += 8;
	  }

      for (i = 0; i < 31; ++i)
	if (fmask & (1UL << i))
	  {
	    mem = gen_frame_mem (DFmode, plus_constant (Pmode, sa_reg,
						        reg_offset));
	    reg = gen_rtx_REG (DFmode, i+32);
	    emit_move_insn (reg, mem);
	    cfa_restores = alloc_reg_note (REG_CFA_RESTORE, reg, cfa_restores);
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
      if (frame_size < 32768 && !cfun->calls_alloca)
	sp_adj2 = GEN_INT (frame_size);
      else if (frame_size < 0x40007fffL)
	{
	  int low = ((frame_size & 0xffff) ^ 0x8000) - 0x8000;

	  sp_adj2 = plus_constant (Pmode, sp_adj1, frame_size - low);
	  if (sa_reg_exp && rtx_equal_p (sa_reg_exp, sp_adj2))
	    sp_adj1 = sa_reg;
	  else
	    {
	      sp_adj1 = gen_rtx_REG (DImode, 23);
	      emit_move_insn (sp_adj1, sp_adj2);
	    }
	  sp_adj2 = GEN_INT (low);
	}
      else
	{
	  rtx tmp = gen_rtx_REG (DImode, 23);
	  sp_adj2 = alpha_emit_set_const (tmp, DImode, frame_size, 3, false);
	  if (!sp_adj2)
	    {
	      /* We can't drop new things to memory this late, afaik,
		 so build it up by pieces.  */
	      sp_adj2 = alpha_emit_set_long_const (tmp, frame_size,
						   -(frame_size < 0));
	      gcc_assert (sp_adj2);
	    }
	}

      /* From now on, things must be in order.  So emit blockages.  */

      /* Restore the frame pointer.  */
      if (fp_is_frame_pointer)
	{
	  emit_insn (gen_blockage ());
	  mem = gen_frame_mem (DImode, plus_constant (Pmode, sa_reg,
						      fp_offset));
	  emit_move_insn (hard_frame_pointer_rtx, mem);
	  cfa_restores = alloc_reg_note (REG_CFA_RESTORE,
					 hard_frame_pointer_rtx, cfa_restores);
	}
      else if (TARGET_ABI_OPEN_VMS)
	{
	  emit_insn (gen_blockage ());
	  emit_move_insn (hard_frame_pointer_rtx,
			  gen_rtx_REG (DImode, vms_save_fp_regno));
	  cfa_restores = alloc_reg_note (REG_CFA_RESTORE,
					 hard_frame_pointer_rtx, cfa_restores);
	}

      /* Restore the stack pointer.  */
      emit_insn (gen_blockage ());
      if (sp_adj2 == const0_rtx)
	insn = emit_move_insn (stack_pointer_rtx, sp_adj1);
      else
	insn = emit_move_insn (stack_pointer_rtx,
			       gen_rtx_PLUS (DImode, sp_adj1, sp_adj2));
      REG_NOTES (insn) = cfa_restores;
      add_reg_note (insn, REG_CFA_DEF_CFA, stack_pointer_rtx);
      RTX_FRAME_RELATED_P (insn) = 1;
    }
  else
    {
      gcc_assert (cfa_restores == NULL);

      if (TARGET_ABI_OPEN_VMS && alpha_procedure_type == PT_REGISTER)
        {
          emit_insn (gen_blockage ());
          insn = emit_move_insn (hard_frame_pointer_rtx,
				 gen_rtx_REG (DImode, vms_save_fp_regno));
	  add_reg_note (insn, REG_CFA_RESTORE, hard_frame_pointer_rtx);
	  RTX_FRAME_RELATED_P (insn) = 1;
        }
    }
}

/* Output the rest of the textual info surrounding the epilogue.  */

void
alpha_end_function (FILE *file, const char *fnname, tree decl ATTRIBUTE_UNUSED)
{
  rtx insn;

  /* We output a nop after noreturn calls at the very end of the function to
     ensure that the return address always remains in the caller's code range,
     as not doing so might confuse unwinding engines.  */
  insn = get_last_insn ();
  if (!INSN_P (insn))
    insn = prev_active_insn (insn);
  if (insn && CALL_P (insn))
    output_asm_insn (get_insn_template (CODE_FOR_nop, NULL), NULL);

#if TARGET_ABI_OPEN_VMS
  /* Write the linkage entries.  */
  alpha_write_linkage (file, fnname);
#endif

  /* End the function.  */
  if (TARGET_ABI_OPEN_VMS
      || !flag_inhibit_size_directive)
    {
      fputs ("\t.end ", file);
      assemble_name (file, fnname);
      putc ('\n', file);
    }
  inside_function = FALSE;
}

#if TARGET_ABI_OSF
/* Emit a tail call to FUNCTION after adjusting THIS by DELTA.

   In order to avoid the hordes of differences between generated code
   with and without TARGET_EXPLICIT_RELOCS, and to avoid duplicating
   lots of code loading up large constants, generate rtl and emit it
   instead of going straight to text.

   Not sure why this idea hasn't been explored before...  */

static void
alpha_output_mi_thunk_osf (FILE *file, tree thunk_fndecl ATTRIBUTE_UNUSED,
			   HOST_WIDE_INT delta, HOST_WIDE_INT vcall_offset,
			   tree function)
{
  HOST_WIDE_INT hi, lo;
  rtx this_rtx, insn, funexp;

  /* We always require a valid GP.  */
  emit_insn (gen_prologue_ldgp ());
  emit_note (NOTE_INSN_PROLOGUE_END);

  /* Find the "this" pointer.  If the function returns a structure,
     the structure return pointer is in $16.  */
  if (aggregate_value_p (TREE_TYPE (TREE_TYPE (function)), function))
    this_rtx = gen_rtx_REG (Pmode, 17);
  else
    this_rtx = gen_rtx_REG (Pmode, 16);

  /* Add DELTA.  When possible we use ldah+lda.  Otherwise load the
     entire constant for the add.  */
  lo = ((delta & 0xffff) ^ 0x8000) - 0x8000;
  hi = (((delta - lo) & 0xffffffff) ^ 0x80000000) - 0x80000000;
  if (hi + lo == delta)
    {
      if (hi)
	emit_insn (gen_adddi3 (this_rtx, this_rtx, GEN_INT (hi)));
      if (lo)
	emit_insn (gen_adddi3 (this_rtx, this_rtx, GEN_INT (lo)));
    }
  else
    {
      rtx tmp = alpha_emit_set_long_const (gen_rtx_REG (Pmode, 0),
					   delta, -(delta < 0));
      emit_insn (gen_adddi3 (this_rtx, this_rtx, tmp));
    }

  /* Add a delta stored in the vtable at VCALL_OFFSET.  */
  if (vcall_offset)
    {
      rtx tmp, tmp2;

      tmp = gen_rtx_REG (Pmode, 0);
      emit_move_insn (tmp, gen_rtx_MEM (Pmode, this_rtx));

      lo = ((vcall_offset & 0xffff) ^ 0x8000) - 0x8000;
      hi = (((vcall_offset - lo) & 0xffffffff) ^ 0x80000000) - 0x80000000;
      if (hi + lo == vcall_offset)
	{
	  if (hi)
	    emit_insn (gen_adddi3 (tmp, tmp, GEN_INT (hi)));
	}
      else
	{
	  tmp2 = alpha_emit_set_long_const (gen_rtx_REG (Pmode, 1),
					    vcall_offset, -(vcall_offset < 0));
          emit_insn (gen_adddi3 (tmp, tmp, tmp2));
	  lo = 0;
	}
      if (lo)
	tmp2 = gen_rtx_PLUS (Pmode, tmp, GEN_INT (lo));
      else
	tmp2 = tmp;
      emit_move_insn (tmp, gen_rtx_MEM (Pmode, tmp2));

      emit_insn (gen_adddi3 (this_rtx, this_rtx, tmp));
    }

  /* Generate a tail call to the target function.  */
  if (! TREE_USED (function))
    {
      assemble_external (function);
      TREE_USED (function) = 1;
    }
  funexp = XEXP (DECL_RTL (function), 0);
  funexp = gen_rtx_MEM (FUNCTION_MODE, funexp);
  insn = emit_call_insn (gen_sibcall (funexp, const0_rtx));
  SIBLING_CALL_P (insn) = 1;

  /* Run just enough of rest_of_compilation to get the insns emitted.
     There's not really enough bulk here to make other passes such as
     instruction scheduling worth while.  Note that use_thunk calls
     assemble_start_function and assemble_end_function.  */
  insn = get_insns ();
  shorten_branches (insn);
  final_start_function (insn, file, 1);
  final (insn, file, 1);
  final_end_function ();
}
#endif /* TARGET_ABI_OSF */

/* Debugging support.  */

#include "gstab.h"

/* Name of the file containing the current function.  */

static const char *current_function_file = "";

/* Offsets to alpha virtual arg/local debugging pointers.  */

long alpha_arg_offset;
long alpha_auto_offset;

/* Emit a new filename to a stream.  */

void
alpha_output_filename (FILE *stream, const char *name)
{
  static int first_time = TRUE;

  if (first_time)
    {
      first_time = FALSE;
      ++num_source_filenames;
      current_function_file = name;
      fprintf (stream, "\t.file\t%d ", num_source_filenames);
      output_quoted_string (stream, name);
      fprintf (stream, "\n");
    }

  else if (name != current_function_file
	   && strcmp (name, current_function_file) != 0)
    {
      ++num_source_filenames;
      current_function_file = name;
      fprintf (stream, "\t.file\t%d ", num_source_filenames);

      output_quoted_string (stream, name);
      fprintf (stream, "\n");
    }
}

/* Structure to show the current status of registers and memory.  */

struct shadow_summary
{
  struct {
    unsigned int i     : 31;	/* Mask of int regs */
    unsigned int fp    : 31;	/* Mask of fp regs */
    unsigned int mem   :  1;	/* mem == imem | fpmem */
  } used, defd;
};

/* Summary the effects of expression X on the machine.  Update SUM, a pointer
   to the summary structure.  SET is nonzero if the insn is setting the
   object, otherwise zero.  */

static void
summarize_insn (rtx x, struct shadow_summary *sum, int set)
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
	unsigned long mask = ((unsigned long) 1) << (regno % 32);

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
    case SCRATCH:     case ASM_INPUT:
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
	    gcc_unreachable ();
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
alpha_handle_trap_shadows (void)
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

  for (i = get_insns (); i ; i = NEXT_INSN (i))
    {
      if (NOTE_P (i))
	{
	  switch (NOTE_KIND (i))
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
	      if (JUMP_P (i)
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
		      /* Annoyingly, get_attr_trap will die on these.  */
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
			  gcc_assert (get_attr_trap (i) != TRAP_YES
				      || (!(sum.defd.i & sum.used.i)
					  && !(sum.defd.fp & sum.used.fp)));

			  goto close_shadow;
			}
		      break;

		    case JUMP_INSN:
		    case CALL_INSN:
		    case CODE_LABEL:
		      goto close_shadow;

		    default:
		      gcc_unreachable ();
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
	  && NONJUMP_INSN_P (i)
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

/* Alpha can only issue instruction groups simultaneously if they are
   suitably aligned.  This is very processor-specific.  */
/* There are a number of entries in alphaev4_insn_pipe and alphaev5_insn_pipe
   that are marked "fake".  These instructions do not exist on that target,
   but it is possible to see these insns with deranged combinations of 
   command-line options, such as "-mtune=ev4 -mmax".  Instead of aborting,
   choose a result at random.  */

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

static enum alphaev4_pipe
alphaev4_insn_pipe (rtx insn)
{
  if (recog_memoized (insn) < 0)
    return EV4_STOP;
  if (get_attr_length (insn) != 4)
    return EV4_STOP;

  switch (get_attr_type (insn))
    {
    case TYPE_ILD:
    case TYPE_LDSYM:
    case TYPE_FLD:
    case TYPE_LD_L:
      return EV4_IBX;

    case TYPE_IADD:
    case TYPE_ILOG:
    case TYPE_ICMOV:
    case TYPE_ICMP:
    case TYPE_FST:
    case TYPE_SHIFT:
    case TYPE_IMUL:
    case TYPE_FBR:
    case TYPE_MVI:		/* fake */
      return EV4_IB0;

    case TYPE_IST:
    case TYPE_MISC:
    case TYPE_IBR:
    case TYPE_JSR:
    case TYPE_CALLPAL:
    case TYPE_FCPYS:
    case TYPE_FCMOV:
    case TYPE_FADD:
    case TYPE_FDIV:
    case TYPE_FMUL:
    case TYPE_ST_C:
    case TYPE_MB:
    case TYPE_FSQRT:		/* fake */
    case TYPE_FTOI:		/* fake */
    case TYPE_ITOF:		/* fake */
      return EV4_IB1;

    default:
      gcc_unreachable ();
    }
}

static enum alphaev5_pipe
alphaev5_insn_pipe (rtx insn)
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
    case TYPE_LD_L:
    case TYPE_ST_C:
    case TYPE_MB:
    case TYPE_FTOI:		/* fake */
    case TYPE_ITOF:		/* fake */
      return EV5_E0;

    case TYPE_IBR:
    case TYPE_JSR:
    case TYPE_CALLPAL:
      return EV5_E1;

    case TYPE_FCPYS:
      return EV5_FAM;

    case TYPE_FBR:
    case TYPE_FCMOV:
    case TYPE_FADD:
    case TYPE_FDIV:
    case TYPE_FSQRT:		/* fake */
      return EV5_FA;

    case TYPE_FMUL:
      return EV5_FM;

    default:
      gcc_unreachable ();
    }
}

/* IN_USE is a mask of the slots currently filled within the insn group.
   The mask bits come from alphaev4_pipe above.  If EV4_IBX is set, then
   the insn in EV4_IB0 can be swapped by the hardware into EV4_IB1.

   LEN is, of course, the length of the group in bytes.  */

static rtx
alphaev4_next_group (rtx insn, int *pin_use, int *plen)
{
  int len, in_use;

  len = in_use = 0;

  if (! INSN_P (insn)
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

	  /* If this is a completely unrecognized insn, it's an asm.
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
	  gcc_unreachable ();
	}
      len += 4;

      /* Haifa doesn't do well scheduling branches.  */
      if (JUMP_P (insn))
	goto next_and_done;

    next:
      insn = next_nonnote_insn (insn);

      if (!insn || ! INSN_P (insn))
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
alphaev5_next_group (rtx insn, int *pin_use, int *plen)
{
  int len, in_use;

  len = in_use = 0;

  if (! INSN_P (insn)
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

	  /* If this is a completely unrecognized insn, it's an asm.
	     We don't know how long it is, so record length as -1 to
	     signal a needed realignment.  */
	  if (recog_memoized (insn) < 0)
	    len = -1;
	  else
	    len = get_attr_length (insn);
	  goto next_and_done;

	/* ??? Most of the places below, we would like to assert never
	   happen, as it would indicate an error either in Haifa, or
	   in the scheduling description.  Unfortunately, Haifa never
	   schedules the last instruction of the BB, so we don't have
	   an accurate TI bit to go off.  */
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
	  gcc_unreachable ();
	}
      len += 4;

      /* Haifa doesn't do well scheduling branches.  */
      /* ??? If this is predicted not-taken, slotting continues, except
	 that no more IBR, FBR, or JSR insns may be slotted.  */
      if (JUMP_P (insn))
	goto next_and_done;

    next:
      insn = next_nonnote_insn (insn);

      if (!insn || ! INSN_P (insn))
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
alphaev4_next_nop (int *pin_use)
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
alphaev5_next_nop (int *pin_use)
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
alpha_align_insns (unsigned int max_align,
		   rtx (*next_group) (rtx, int *, int *),
		   rtx (*next_nop) (int *))
{
  /* ALIGN is the known alignment for the insn group.  */
  unsigned int align;
  /* OFS is the offset of the current insn in the insn group.  */
  int ofs;
  int prev_in_use, in_use, len, ldgp;
  rtx i, next;

  /* Let shorten branches care for assigning alignments to code labels.  */
  shorten_branches (get_insns ());

  if (align_functions < 4)
    align = 4;
  else if ((unsigned int) align_functions < max_align)
    align = align_functions;
  else
    align = max_align;

  ofs = prev_in_use = 0;
  i = get_insns ();
  if (NOTE_P (i))
    i = next_nonnote_insn (i);

  ldgp = alpha_function_needs_gp ? 8 : 0;

  while (i)
    {
      next = (*next_group) (i, &in_use, &len);

      /* When we see a label, resync alignment etc.  */
      if (LABEL_P (i))
	{
	  unsigned int new_align = 1 << label_to_alignment (i);

	  if (new_align >= align)
	    {
	      align = new_align < max_align ? new_align : max_align;
	      ofs = 0;
	    }

	  else if (ofs & (new_align-1))
	    ofs = (ofs | (new_align-1)) + 1;
	  gcc_assert (!len);
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
      else if ((int) align < len)
	{
	  unsigned int new_log_align = len > 8 ? 4 : 3;
	  rtx prev, where;

	  where = prev = prev_nonnote_insn (i);
	  if (!where || !LABEL_P (where))
	    where = i;

	  /* Can't realign between a call and its gp reload.  */
	  if (! (TARGET_EXPLICIT_RELOCS
		 && prev && CALL_P (prev)))
	    {
	      emit_insn_before (gen_realign (GEN_INT (new_log_align)), where);
	      align = 1 << new_log_align;
	      ofs = 0;
	    }
	}

      /* We may not insert padding inside the initial ldgp sequence.  */
      else if (ldgp > 0)
	ldgp -= len;

      /* If the group won't fit in the same INT16 as the previous,
	 we need to add padding to keep the group together.  Rather
	 than simply leaving the insn filling to the assembler, we
	 can make use of the knowledge of what sorts of instructions
	 were issued in the previous group to make sure that all of
	 the added nops are really free.  */
      else if (ofs + len > (int) align)
	{
	  int nop_count = (align - ofs) / 4;
	  rtx where;

	  /* Insert nops before labels, branches, and calls to truly merge
	     the execution of the nops with the previous instruction group.  */
	  where = prev_nonnote_insn (i);
	  if (where)
	    {
	      if (LABEL_P (where))
		{
		  rtx where2 = prev_nonnote_insn (where);
		  if (where2 && JUMP_P (where2))
		    where = where2;
		}
	      else if (NONJUMP_INSN_P (where))
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

/* Insert an unop between sibcall or noreturn function call and GP load.  */

static void
alpha_pad_function_end (void)
{
  rtx insn, next;

  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      if (!CALL_P (insn)
	  || !(SIBLING_CALL_P (insn)
	       || find_reg_note (insn, REG_NORETURN, NULL_RTX)))
        continue;

      /* Make sure we do not split a call and its corresponding
	 CALL_ARG_LOCATION note.  */
      next = NEXT_INSN (insn);
      if (next == NULL)
	continue;
      if (BARRIER_P (next))
	{
	  next = NEXT_INSN (next);
	  if (next == NULL)
	    continue;
	}
      if (NOTE_P (next) && NOTE_KIND (next) == NOTE_INSN_CALL_ARG_LOCATION)
	insn = next;

      next = next_active_insn (insn);
      if (next)
	{
	  rtx pat = PATTERN (next);

	  if (GET_CODE (pat) == SET
	      && GET_CODE (SET_SRC (pat)) == UNSPEC_VOLATILE
	      && XINT (SET_SRC (pat), 1) == UNSPECV_LDGP1)
	    emit_insn_after (gen_unop (), insn);
	}
    }
}

/* Machine dependent reorg pass.  */

static void
alpha_reorg (void)
{
  /* Workaround for a linker error that triggers when an exception
     handler immediatelly follows a sibcall or a noreturn function.

In the sibcall case:

     The instruction stream from an object file:

 1d8:   00 00 fb 6b     jmp     (t12)
 1dc:   00 00 ba 27     ldah    gp,0(ra)
 1e0:   00 00 bd 23     lda     gp,0(gp)
 1e4:   00 00 7d a7     ldq     t12,0(gp)
 1e8:   00 40 5b 6b     jsr     ra,(t12),1ec <__funcZ+0x1ec>

     was converted in the final link pass to:

   12003aa88:   67 fa ff c3     br      120039428 <...>
   12003aa8c:   00 00 fe 2f     unop
   12003aa90:   00 00 fe 2f     unop
   12003aa94:   48 83 7d a7     ldq     t12,-31928(gp)
   12003aa98:   00 40 5b 6b     jsr     ra,(t12),12003aa9c <__func+0x1ec>

And in the noreturn case:

     The instruction stream from an object file:

  54:   00 40 5b 6b     jsr     ra,(t12),58 <__func+0x58>
  58:   00 00 ba 27     ldah    gp,0(ra)
  5c:   00 00 bd 23     lda     gp,0(gp)
  60:   00 00 7d a7     ldq     t12,0(gp)
  64:   00 40 5b 6b     jsr     ra,(t12),68 <__func+0x68>

     was converted in the final link pass to:

   fdb24:       a0 03 40 d3     bsr     ra,fe9a8 <_called_func+0x8>
   fdb28:       00 00 fe 2f     unop
   fdb2c:       00 00 fe 2f     unop
   fdb30:       30 82 7d a7     ldq     t12,-32208(gp)
   fdb34:       00 40 5b 6b     jsr     ra,(t12),fdb38 <__func+0x68>

     GP load instructions were wrongly cleared by the linker relaxation
     pass.  This workaround prevents removal of GP loads by inserting
     an unop instruction between a sibcall or noreturn function call and
     exception handler prologue.  */

  if (current_function_has_exception_handlers ())
    alpha_pad_function_end ();

  if (alpha_tp != ALPHA_TP_PROG || flag_exceptions)
    alpha_handle_trap_shadows ();

  /* Due to the number of extra trapb insns, don't bother fixing up
     alignment when trap precision is instruction.  Moreover, we can
     only do our job when sched2 is run.  */
  if (optimize && !optimize_size
      && alpha_tp != ALPHA_TP_INSN
      && flag_schedule_insns_after_reload)
    {
      if (alpha_tune == PROCESSOR_EV4)
	alpha_align_insns (8, alphaev4_next_group, alphaev4_next_nop);
      else if (alpha_tune == PROCESSOR_EV5)
	alpha_align_insns (16, alphaev5_next_group, alphaev5_next_nop);
    }
}

static void
alpha_file_start (void)
{
  default_file_start ();

  fputs ("\t.set noreorder\n", asm_out_file);
  fputs ("\t.set volatile\n", asm_out_file);
  if (TARGET_ABI_OSF)
    fputs ("\t.set noat\n", asm_out_file);
  if (TARGET_EXPLICIT_RELOCS)
    fputs ("\t.set nomacro\n", asm_out_file);
  if (TARGET_SUPPORT_ARCH | TARGET_BWX | TARGET_MAX | TARGET_FIX | TARGET_CIX)
    {
      const char *arch;

      if (alpha_cpu == PROCESSOR_EV6 || TARGET_FIX || TARGET_CIX)
	arch = "ev6";
      else if (TARGET_MAX)
	arch = "pca56";
      else if (TARGET_BWX)
	arch = "ev56";
      else if (alpha_cpu == PROCESSOR_EV5)
	arch = "ev5";
      else
	arch = "ev4";

      fprintf (asm_out_file, "\t.arch %s\n", arch);
    }
}

/* Since we don't have a .dynbss section, we should not allow global
   relocations in the .rodata section.  */

static int
alpha_elf_reloc_rw_mask (void)
{
  return flag_pic ? 3 : 2;
}

/* Return a section for X.  The only special thing we do here is to
   honor small data.  */

static section *
alpha_elf_select_rtx_section (enum machine_mode mode, rtx x,
			      unsigned HOST_WIDE_INT align)
{
  if (TARGET_SMALL_DATA && GET_MODE_SIZE (mode) <= g_switch_value)
    /* ??? Consider using mergeable sdata sections.  */
    return sdata_section;
  else
    return default_elf_select_rtx_section (mode, x, align);
}

static unsigned int
alpha_elf_section_type_flags (tree decl, const char *name, int reloc)
{
  unsigned int flags = 0;

  if (strcmp (name, ".sdata") == 0
      || strncmp (name, ".sdata.", 7) == 0
      || strncmp (name, ".gnu.linkonce.s.", 16) == 0
      || strcmp (name, ".sbss") == 0
      || strncmp (name, ".sbss.", 6) == 0
      || strncmp (name, ".gnu.linkonce.sb.", 17) == 0)
    flags = SECTION_SMALL;

  flags |= default_section_type_flags (decl, name, reloc);
  return flags;
}

/* Structure to collect function names for final output in link section.  */
/* Note that items marked with GTY can't be ifdef'ed out.  */

enum reloc_kind
{
  KIND_LINKAGE,
  KIND_CODEADDR
};

struct GTY(()) alpha_links
{
  rtx func;
  rtx linkage;
  enum reloc_kind rkind;
};

#if TARGET_ABI_OPEN_VMS

/* Return the VMS argument type corresponding to MODE.  */

enum avms_arg_type
alpha_arg_type (enum machine_mode mode)
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
alpha_arg_info_reg_val (CUMULATIVE_ARGS cum)
{
  unsigned HOST_WIDE_INT regval = cum.num_args;
  int i;

  for (i = 0; i < 6; i++)
    regval |= ((int) cum.atypes[i]) << (i * 3 + 8);

  return GEN_INT (regval);
}


/* Return a SYMBOL_REF representing the reference to the .linkage entry
   of function FUNC built for calls made from CFUNDECL.  LFLAG is 1 if
   this is the reference to the linkage pointer value, 0 if this is the
   reference to the function entry value.  RFLAG is 1 if this a reduced
   reference (code address only), 0 if this is a full reference.  */

rtx
alpha_use_linkage (rtx func, bool lflag, bool rflag)
{
  struct alpha_links *al = NULL;
  const char *name = XSTR (func, 0);

  if (cfun->machine->links)
    {
      splay_tree_node lnode;

      /* Is this name already defined?  */
      lnode = splay_tree_lookup (cfun->machine->links, (splay_tree_key) name);
      if (lnode)
	al = (struct alpha_links *) lnode->value;
    }
  else
    cfun->machine->links = splay_tree_new_ggc
      ((splay_tree_compare_fn) strcmp,
       ggc_alloc_splay_tree_str_alpha_links_splay_tree_s,
       ggc_alloc_splay_tree_str_alpha_links_splay_tree_node_s);

  if (al == NULL)
    {
      size_t buf_len;
      char *linksym;
      tree id;

      if (name[0] == '*')
	name++;

      /* Follow transparent alias, as this is used for CRTL translations.  */
      id = maybe_get_identifier (name);
      if (id)
        {
          while (IDENTIFIER_TRANSPARENT_ALIAS (id))
            id = TREE_CHAIN (id);
          name = IDENTIFIER_POINTER (id);
        }

      buf_len = strlen (name) + 8 + 9;
      linksym = (char *) alloca (buf_len);
      snprintf (linksym, buf_len, "$%d..%s..lk", cfun->funcdef_no, name);

      al = ggc_alloc<alpha_links> ();
      al->func = func;
      al->linkage = gen_rtx_SYMBOL_REF (Pmode, ggc_strdup (linksym));

      splay_tree_insert (cfun->machine->links,
                         (splay_tree_key) ggc_strdup (name),
			 (splay_tree_value) al);
    }

  al->rkind = rflag ? KIND_CODEADDR : KIND_LINKAGE;

  if (lflag)
    return gen_rtx_MEM (Pmode, plus_constant (Pmode, al->linkage, 8));
  else
    return al->linkage;
}

static int
alpha_write_one_linkage (splay_tree_node node, void *data)
{
  const char *const name = (const char *) node->key;
  struct alpha_links *link = (struct alpha_links *) node->value;
  FILE *stream = (FILE *) data;

  ASM_OUTPUT_INTERNAL_LABEL (stream, XSTR (link->linkage, 0));
  if (link->rkind == KIND_CODEADDR)
    {
      /* External and used, request code address.  */
      fprintf (stream, "\t.code_address ");
    }
  else
    {
      if (!SYMBOL_REF_EXTERNAL_P (link->func)
          && SYMBOL_REF_LOCAL_P (link->func))
	{
	  /* Locally defined, build linkage pair.  */
	  fprintf (stream, "\t.quad %s..en\n", name);
	  fprintf (stream, "\t.quad ");
	}
      else
	{
	  /* External, request linkage pair.  */
	  fprintf (stream, "\t.linkage ");
	}
    }
  assemble_name (stream, name);
  fputs ("\n", stream);

  return 0;
}

static void
alpha_write_linkage (FILE *stream, const char *funname)
{
  fprintf (stream, "\t.link\n");
  fprintf (stream, "\t.align 3\n");
  in_section = NULL;

#ifdef TARGET_VMS_CRASH_DEBUG
  fputs ("\t.name ", stream);
  assemble_name (stream, funname);
  fputs ("..na\n", stream);
#endif

  ASM_OUTPUT_LABEL (stream, funname);
  fprintf (stream, "\t.pdesc ");
  assemble_name (stream, funname);
  fprintf (stream, "..en,%s\n",
	   alpha_procedure_type == PT_STACK ? "stack"
	   : alpha_procedure_type == PT_REGISTER ? "reg" : "null");

  if (cfun->machine->links)
    {
      splay_tree_foreach (cfun->machine->links, alpha_write_one_linkage, stream);
      /* splay_tree_delete (func->links); */
    }
}

/* Switch to an arbitrary section NAME with attributes as specified
   by FLAGS.  ALIGN specifies any known alignment requirements for
   the section; 0 if the default should be used.  */

static void
vms_asm_named_section (const char *name, unsigned int flags, 
		       tree decl ATTRIBUTE_UNUSED)
{
  fputc ('\n', asm_out_file);
  fprintf (asm_out_file, ".section\t%s", name);

  if (flags & SECTION_DEBUG)
    fprintf (asm_out_file, ",NOWRT");

  fputc ('\n', asm_out_file);
}

/* Record an element in the table of global constructors.  SYMBOL is
   a SYMBOL_REF of the function to be called; PRIORITY is a number
   between 0 and MAX_INIT_PRIORITY.

   Differs from default_ctors_section_asm_out_constructor in that the
   width of the .ctors entry is always 64 bits, rather than the 32 bits
   used by a normal pointer.  */

static void
vms_asm_out_constructor (rtx symbol, int priority ATTRIBUTE_UNUSED)
{
  switch_to_section (ctors_section);
  assemble_align (BITS_PER_WORD);
  assemble_integer (symbol, UNITS_PER_WORD, BITS_PER_WORD, 1);
}

static void
vms_asm_out_destructor (rtx symbol, int priority ATTRIBUTE_UNUSED)
{
  switch_to_section (dtors_section);
  assemble_align (BITS_PER_WORD);
  assemble_integer (symbol, UNITS_PER_WORD, BITS_PER_WORD, 1);
}
#else
rtx
alpha_use_linkage (rtx func ATTRIBUTE_UNUSED,
		   bool lflag ATTRIBUTE_UNUSED,
		   bool rflag ATTRIBUTE_UNUSED)
{
  return NULL_RTX;
}

#endif /* TARGET_ABI_OPEN_VMS */

static void
alpha_init_libfuncs (void)
{
  if (TARGET_ABI_OPEN_VMS)
    {
      /* Use the VMS runtime library functions for division and
	 remainder.  */
      set_optab_libfunc (sdiv_optab, SImode, "OTS$DIV_I");
      set_optab_libfunc (sdiv_optab, DImode, "OTS$DIV_L");
      set_optab_libfunc (udiv_optab, SImode, "OTS$DIV_UI");
      set_optab_libfunc (udiv_optab, DImode, "OTS$DIV_UL");
      set_optab_libfunc (smod_optab, SImode, "OTS$REM_I");
      set_optab_libfunc (smod_optab, DImode, "OTS$REM_L");
      set_optab_libfunc (umod_optab, SImode, "OTS$REM_UI");
      set_optab_libfunc (umod_optab, DImode, "OTS$REM_UL");
      abort_libfunc = init_one_libfunc ("decc$abort");
      memcmp_libfunc = init_one_libfunc ("decc$memcmp");
#ifdef MEM_LIBFUNCS_INIT
      MEM_LIBFUNCS_INIT;
#endif
    }
}

/* On the Alpha, we use this to disable the floating-point registers
   when they don't exist.  */

static void
alpha_conditional_register_usage (void)
{
  int i;
  if (! TARGET_FPREGS)
    for (i = 32; i < 63; i++)
      fixed_regs[i] = call_used_regs[i] = 1;
}

/* Canonicalize a comparison from one we don't have to one we do have.  */

static void
alpha_canonicalize_comparison (int *code, rtx *op0, rtx *op1,
			       bool op0_preserve_value)
{
  if (!op0_preserve_value
      && (*code == GE || *code == GT || *code == GEU || *code == GTU)
      && (REG_P (*op1) || *op1 == const0_rtx))
    {
      rtx tem = *op0;
      *op0 = *op1;
      *op1 = tem;
      *code = (int)swap_condition ((enum rtx_code)*code);
    }

  if ((*code == LT || *code == LTU)
      && CONST_INT_P (*op1) && INTVAL (*op1) == 256)
    {
      *code = *code == LT ? LE : LEU;
      *op1 = GEN_INT (255);
    }
}

/* Initialize the GCC target structure.  */
#if TARGET_ABI_OPEN_VMS
# undef TARGET_ATTRIBUTE_TABLE
# define TARGET_ATTRIBUTE_TABLE vms_attribute_table
# undef TARGET_CAN_ELIMINATE
# define TARGET_CAN_ELIMINATE alpha_vms_can_eliminate
#endif

#undef TARGET_IN_SMALL_DATA_P
#define TARGET_IN_SMALL_DATA_P alpha_in_small_data_p

#undef TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\t.word\t"
#undef TARGET_ASM_ALIGNED_DI_OP
#define TARGET_ASM_ALIGNED_DI_OP "\t.quad\t"

/* Default unaligned ops are provided for ELF systems.  To get unaligned
   data for non-ELF systems, we have to turn off auto alignment.  */
#if TARGET_ABI_OPEN_VMS
#undef TARGET_ASM_UNALIGNED_HI_OP
#define TARGET_ASM_UNALIGNED_HI_OP "\t.align 0\n\t.word\t"
#undef TARGET_ASM_UNALIGNED_SI_OP
#define TARGET_ASM_UNALIGNED_SI_OP "\t.align 0\n\t.long\t"
#undef TARGET_ASM_UNALIGNED_DI_OP
#define TARGET_ASM_UNALIGNED_DI_OP "\t.align 0\n\t.quad\t"
#endif

#undef  TARGET_ASM_RELOC_RW_MASK
#define TARGET_ASM_RELOC_RW_MASK  alpha_elf_reloc_rw_mask
#undef	TARGET_ASM_SELECT_RTX_SECTION
#define	TARGET_ASM_SELECT_RTX_SECTION  alpha_elf_select_rtx_section
#undef  TARGET_SECTION_TYPE_FLAGS
#define TARGET_SECTION_TYPE_FLAGS  alpha_elf_section_type_flags

#undef TARGET_ASM_FUNCTION_END_PROLOGUE
#define TARGET_ASM_FUNCTION_END_PROLOGUE alpha_output_function_end_prologue

#undef TARGET_INIT_LIBFUNCS
#define TARGET_INIT_LIBFUNCS alpha_init_libfuncs

#undef TARGET_LEGITIMIZE_ADDRESS
#define TARGET_LEGITIMIZE_ADDRESS alpha_legitimize_address
#undef TARGET_MODE_DEPENDENT_ADDRESS_P
#define TARGET_MODE_DEPENDENT_ADDRESS_P alpha_mode_dependent_address_p

#undef TARGET_ASM_FILE_START
#define TARGET_ASM_FILE_START alpha_file_start

#undef TARGET_SCHED_ADJUST_COST
#define TARGET_SCHED_ADJUST_COST alpha_adjust_cost
#undef TARGET_SCHED_ISSUE_RATE
#define TARGET_SCHED_ISSUE_RATE alpha_issue_rate
#undef TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD
#define TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD \
  alpha_multipass_dfa_lookahead

#undef TARGET_HAVE_TLS
#define TARGET_HAVE_TLS HAVE_AS_TLS

#undef  TARGET_BUILTIN_DECL
#define TARGET_BUILTIN_DECL  alpha_builtin_decl
#undef  TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS alpha_init_builtins
#undef  TARGET_EXPAND_BUILTIN
#define TARGET_EXPAND_BUILTIN alpha_expand_builtin
#undef  TARGET_FOLD_BUILTIN
#define TARGET_FOLD_BUILTIN alpha_fold_builtin
#undef  TARGET_GIMPLE_FOLD_BUILTIN
#define TARGET_GIMPLE_FOLD_BUILTIN alpha_gimple_fold_builtin

#undef TARGET_FUNCTION_OK_FOR_SIBCALL
#define TARGET_FUNCTION_OK_FOR_SIBCALL alpha_function_ok_for_sibcall
#undef TARGET_CANNOT_COPY_INSN_P
#define TARGET_CANNOT_COPY_INSN_P alpha_cannot_copy_insn_p
#undef TARGET_LEGITIMATE_CONSTANT_P
#define TARGET_LEGITIMATE_CONSTANT_P alpha_legitimate_constant_p
#undef TARGET_CANNOT_FORCE_CONST_MEM
#define TARGET_CANNOT_FORCE_CONST_MEM alpha_cannot_force_const_mem

#if TARGET_ABI_OSF
#undef TARGET_ASM_OUTPUT_MI_THUNK
#define TARGET_ASM_OUTPUT_MI_THUNK alpha_output_mi_thunk_osf
#undef TARGET_ASM_CAN_OUTPUT_MI_THUNK
#define TARGET_ASM_CAN_OUTPUT_MI_THUNK hook_bool_const_tree_hwi_hwi_const_tree_true
#undef TARGET_STDARG_OPTIMIZE_HOOK
#define TARGET_STDARG_OPTIMIZE_HOOK alpha_stdarg_optimize_hook
#endif

/* Use 16-bits anchor.  */
#undef TARGET_MIN_ANCHOR_OFFSET
#define TARGET_MIN_ANCHOR_OFFSET -0x7fff - 1
#undef TARGET_MAX_ANCHOR_OFFSET
#define TARGET_MAX_ANCHOR_OFFSET 0x7fff
#undef TARGET_USE_BLOCKS_FOR_CONSTANT_P
#define TARGET_USE_BLOCKS_FOR_CONSTANT_P hook_bool_mode_const_rtx_true

#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS alpha_rtx_costs
#undef TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST hook_int_rtx_mode_as_bool_0

#undef TARGET_MACHINE_DEPENDENT_REORG
#define TARGET_MACHINE_DEPENDENT_REORG alpha_reorg

#undef TARGET_PROMOTE_FUNCTION_MODE
#define TARGET_PROMOTE_FUNCTION_MODE default_promote_function_mode_always_promote
#undef TARGET_PROMOTE_PROTOTYPES
#define TARGET_PROMOTE_PROTOTYPES hook_bool_const_tree_false
#undef TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY alpha_return_in_memory
#undef TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE alpha_pass_by_reference
#undef TARGET_SETUP_INCOMING_VARARGS
#define TARGET_SETUP_INCOMING_VARARGS alpha_setup_incoming_varargs
#undef TARGET_STRICT_ARGUMENT_NAMING
#define TARGET_STRICT_ARGUMENT_NAMING hook_bool_CUMULATIVE_ARGS_true
#undef TARGET_PRETEND_OUTGOING_VARARGS_NAMED
#define TARGET_PRETEND_OUTGOING_VARARGS_NAMED hook_bool_CUMULATIVE_ARGS_true
#undef TARGET_SPLIT_COMPLEX_ARG
#define TARGET_SPLIT_COMPLEX_ARG alpha_split_complex_arg
#undef TARGET_GIMPLIFY_VA_ARG_EXPR
#define TARGET_GIMPLIFY_VA_ARG_EXPR alpha_gimplify_va_arg
#undef TARGET_ARG_PARTIAL_BYTES
#define TARGET_ARG_PARTIAL_BYTES alpha_arg_partial_bytes
#undef TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG alpha_function_arg
#undef TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE alpha_function_arg_advance
#undef TARGET_TRAMPOLINE_INIT
#define TARGET_TRAMPOLINE_INIT alpha_trampoline_init

#undef TARGET_INSTANTIATE_DECLS
#define TARGET_INSTANTIATE_DECLS alpha_instantiate_decls

#undef TARGET_SECONDARY_RELOAD
#define TARGET_SECONDARY_RELOAD alpha_secondary_reload

#undef TARGET_SCALAR_MODE_SUPPORTED_P
#define TARGET_SCALAR_MODE_SUPPORTED_P alpha_scalar_mode_supported_p
#undef TARGET_VECTOR_MODE_SUPPORTED_P
#define TARGET_VECTOR_MODE_SUPPORTED_P alpha_vector_mode_supported_p

#undef TARGET_BUILD_BUILTIN_VA_LIST
#define TARGET_BUILD_BUILTIN_VA_LIST alpha_build_builtin_va_list

#undef TARGET_EXPAND_BUILTIN_VA_START
#define TARGET_EXPAND_BUILTIN_VA_START alpha_va_start

/* The Alpha architecture does not require sequential consistency.  See
   http://www.cs.umd.edu/~pugh/java/memoryModel/AlphaReordering.html
   for an example of how it can be violated in practice.  */
#undef TARGET_RELAXED_ORDERING
#define TARGET_RELAXED_ORDERING true

#undef TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE alpha_option_override

#ifdef TARGET_ALTERNATE_LONG_DOUBLE_MANGLING
#undef TARGET_MANGLE_TYPE
#define TARGET_MANGLE_TYPE alpha_mangle_type
#endif

#undef TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P alpha_legitimate_address_p

#undef TARGET_CONDITIONAL_REGISTER_USAGE
#define TARGET_CONDITIONAL_REGISTER_USAGE alpha_conditional_register_usage

#undef TARGET_CANONICALIZE_COMPARISON
#define TARGET_CANONICALIZE_COMPARISON alpha_canonicalize_comparison

struct gcc_target targetm = TARGET_INITIALIZER;


#include "gt-alpha.h"
