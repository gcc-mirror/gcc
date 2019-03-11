/* Common hooks for IBM RS/6000.
   Copyright (C) 1991-2019 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "diagnostic-core.h"
#include "tm.h"
#include "common/common-target.h"
#include "common/common-target-def.h"
#include "opts.h"
#include "flags.h"
#include "params.h"

/* Implement TARGET_OPTION_OPTIMIZATION_TABLE.  */
static const struct default_options rs6000_option_optimization_table[] =
  {
    /* Enable -fsched-pressure for first pass instruction scheduling.  */
    { OPT_LEVELS_1_PLUS, OPT_fsched_pressure, NULL, 1 },
    { OPT_LEVELS_NONE, 0, NULL, 0 }
  };

/* Implement TARGET_OPTION_INIT_STRUCT.  */

static void
rs6000_option_init_struct (struct gcc_options *opts)
{
  if (DEFAULT_ABI == ABI_DARWIN)
    /* The Darwin libraries never set errno, so we might as well
       avoid calling them when that's the only reason we would.  */
    opts->x_flag_errno_math = 0;

  /* Enable section anchors by default.  */
  if (!TARGET_MACHO)
    opts->x_flag_section_anchors = 1;
}

/* Implement TARGET_OPTION_DEFAULT_PARAMS.  */

static void
rs6000_option_default_params (void)
{
  /* Double growth factor to counter reduced min jump length.  */
  set_default_param_value (PARAM_MAX_GROW_COPY_BB_INSNS, 16);
}

/* If not otherwise specified by a target, make 'long double' equivalent to
   'double'.  */

#ifndef RS6000_DEFAULT_LONG_DOUBLE_SIZE
#define RS6000_DEFAULT_LONG_DOUBLE_SIZE 64
#endif

/* Implement TARGET_HANDLE_OPTION.  */

static bool
rs6000_handle_option (struct gcc_options *opts, struct gcc_options *opts_set,
		      const struct cl_decoded_option *decoded,
		      location_t loc)
{
  enum fpu_type_t fpu_type = FPU_NONE;
  char *p, *q;
  size_t code = decoded->opt_index;
  const char *arg = decoded->arg;
  int value = decoded->value;

  switch (code)
    {
    case OPT_mfull_toc:
      opts->x_rs6000_isa_flags &= ~OPTION_MASK_MINIMAL_TOC;
      opts->x_TARGET_NO_FP_IN_TOC = 0;
      opts->x_TARGET_NO_SUM_IN_TOC = 0;
      opts_set->x_rs6000_isa_flags |= OPTION_MASK_MINIMAL_TOC;
#ifdef TARGET_USES_SYSV4_OPT
      /* Note, V.4 no longer uses a normal TOC, so make -mfull-toc, be
	 just the same as -mminimal-toc.  */
      opts->x_rs6000_isa_flags |= OPTION_MASK_MINIMAL_TOC;
      opts_set->x_rs6000_isa_flags |= OPTION_MASK_MINIMAL_TOC;
#endif
      break;

#ifdef TARGET_USES_SYSV4_OPT
    case OPT_mtoc:
      /* Make -mtoc behave like -mminimal-toc.  */
      opts->x_rs6000_isa_flags |= OPTION_MASK_MINIMAL_TOC;
      opts_set->x_rs6000_isa_flags |= OPTION_MASK_MINIMAL_TOC;
      break;
#endif

#ifdef TARGET_USES_AIX64_OPT
    case OPT_maix64:
#else
    case OPT_m64:
#endif
      opts->x_rs6000_isa_flags |= OPTION_MASK_POWERPC64;
      opts->x_rs6000_isa_flags |= (~opts_set->x_rs6000_isa_flags
				   & OPTION_MASK_PPC_GFXOPT);
      opts_set->x_rs6000_isa_flags |= OPTION_MASK_POWERPC64;
      break;

#ifdef TARGET_USES_AIX64_OPT
    case OPT_maix32:
#else
    case OPT_m32:
#endif
      opts->x_rs6000_isa_flags &= ~OPTION_MASK_POWERPC64;
      opts_set->x_rs6000_isa_flags |= OPTION_MASK_POWERPC64;
      break;

    case OPT_mminimal_toc:
      if (value == 1)
	{
	  opts->x_TARGET_NO_FP_IN_TOC = 0;
	  opts->x_TARGET_NO_SUM_IN_TOC = 0;
	}
      break;

    case OPT_mpowerpc_gpopt:
    case OPT_mpowerpc_gfxopt:
      break;

    case OPT_mdebug_:
      p = ASTRDUP (arg);
      opts->x_rs6000_debug = 0;

      while ((q = strtok (p, ",")) != NULL)
	{
	  unsigned mask = 0;
	  bool invert;

	  p = NULL;
	  if (*q == '!')
	    {
	      invert = true;
	      q++;
	    }
	  else
	    invert = false;

	  if (! strcmp (q, "all"))
	    mask = MASK_DEBUG_ALL;
	  else if (! strcmp (q, "stack"))
	    mask = MASK_DEBUG_STACK;
	  else if (! strcmp (q, "arg"))
	    mask = MASK_DEBUG_ARG;
	  else if (! strcmp (q, "reg"))
	    mask = MASK_DEBUG_REG;
	  else if (! strcmp (q, "addr"))
	    mask = MASK_DEBUG_ADDR;
	  else if (! strcmp (q, "cost"))
	    mask = MASK_DEBUG_COST;
	  else if (! strcmp (q, "target"))
	    mask = MASK_DEBUG_TARGET;
	  else if (! strcmp (q, "builtin"))
	    mask = MASK_DEBUG_BUILTIN;
	  else
	    error_at (loc, "unknown %<-mdebug-%s%> switch", q);

	  if (invert)
	    opts->x_rs6000_debug &= ~mask;
	  else	
	    opts->x_rs6000_debug |= mask;
	}
      break;

#ifdef TARGET_USES_SYSV4_OPT
    case OPT_mrelocatable:
      if (value == 1)
	{
	  opts->x_rs6000_isa_flags |= OPTION_MASK_MINIMAL_TOC;
	  opts_set->x_rs6000_isa_flags |= OPTION_MASK_MINIMAL_TOC;
	  opts->x_TARGET_NO_FP_IN_TOC = 1;
	}
      break;

    case OPT_mrelocatable_lib:
      if (value == 1)
	{
	  opts->x_rs6000_isa_flags |= (OPTION_MASK_RELOCATABLE
				       | OPTION_MASK_MINIMAL_TOC);
	  opts_set->x_rs6000_isa_flags |= (OPTION_MASK_RELOCATABLE
					   | OPTION_MASK_MINIMAL_TOC);
	  opts->x_TARGET_NO_FP_IN_TOC = 1;
	}
      else
	{
	  opts->x_rs6000_isa_flags &= ~OPTION_MASK_RELOCATABLE;
	  opts_set->x_rs6000_isa_flags |= OPTION_MASK_RELOCATABLE;
	}
      break;
#endif

    case OPT_mabi_altivec:
      /* Enabling the AltiVec ABI turns off the SPE ABI.  */
      opts->x_rs6000_spe_abi = 0;
      break;

    case OPT_mabi_spe:
      opts->x_rs6000_altivec_abi = 0;
      break;

    case OPT_mlong_double_:
      if (value != 64 && value != 128)
	{
	  error_at (loc, "unknown switch %<-mlong-double-%s%>", arg);
	  opts->x_rs6000_long_double_type_size
	    = RS6000_DEFAULT_LONG_DOUBLE_SIZE;
	  return false;
	}
      break;

    case OPT_msingle_float:
      if (!TARGET_SINGLE_FPU) 
	warning_at (loc, 0,
		    "%<-msingle-float%> option equivalent to %<-mhard-float%>");
      /* -msingle-float implies -mno-double-float and TARGET_HARD_FLOAT. */
      opts->x_rs6000_double_float = 0;
      opts->x_rs6000_isa_flags &= ~OPTION_MASK_SOFT_FLOAT;
      opts_set->x_rs6000_isa_flags |= OPTION_MASK_SOFT_FLOAT;
      break;

    case OPT_mdouble_float:
      /* -mdouble-float implies -msingle-float and TARGET_HARD_FLOAT. */
      opts->x_rs6000_single_float = 1;
      opts->x_rs6000_isa_flags &= ~OPTION_MASK_SOFT_FLOAT;
      opts_set->x_rs6000_isa_flags |= OPTION_MASK_SOFT_FLOAT;
      break;

    case OPT_msimple_fpu:
      if (!TARGET_SINGLE_FPU) 
	warning_at (loc, 0, "%<-msimple-fpu%> option ignored");
      break;

    case OPT_mhard_float:
      /* -mhard_float implies -msingle-float and -mdouble-float. */
      opts->x_rs6000_single_float = opts->x_rs6000_double_float = 1;
      break;

    case OPT_msoft_float:
      /* -msoft_float implies -mnosingle-float and -mnodouble-float. */
      opts->x_rs6000_single_float = opts->x_rs6000_double_float = 0;
      break;

    case OPT_mfpu_:
      fpu_type = (enum fpu_type_t) value;
      if (fpu_type != FPU_NONE)
	{
	  /* If -mfpu is not none, then turn off SOFT_FLOAT, turn on
	     HARD_FLOAT. */
	  opts->x_rs6000_isa_flags &= ~OPTION_MASK_SOFT_FLOAT;
	  opts_set->x_rs6000_isa_flags |= OPTION_MASK_SOFT_FLOAT;
	  opts->x_rs6000_xilinx_fpu = 1;
	  if (fpu_type == FPU_SF_LITE || fpu_type == FPU_SF_FULL) 
	    opts->x_rs6000_single_float = 1;
	  if (fpu_type == FPU_DF_LITE || fpu_type == FPU_DF_FULL) 
	    opts->x_rs6000_single_float = opts->x_rs6000_double_float = 1;
	  if (fpu_type == FPU_SF_LITE || fpu_type == FPU_DF_LITE) 
	    opts->x_rs6000_simple_fpu = 1;
	}
      else
	{
	  /* -mfpu=none is equivalent to -msoft-float.  */
	  opts->x_rs6000_isa_flags |= OPTION_MASK_SOFT_FLOAT;
	  opts_set->x_rs6000_isa_flags |= OPTION_MASK_SOFT_FLOAT;
	  opts->x_rs6000_single_float = opts->x_rs6000_double_float = 0;
	}
      break;

    case OPT_mrecip:
      opts->x_rs6000_recip_name = (value) ? "default" : "none";
      break;
    }
  return true;
}

/* -fsplit-stack uses a field in the TCB, available with glibc-2.19.
   We also allow 2.18 because alignment padding guarantees that the
   space is available there too.  */

static bool
rs6000_supports_split_stack (bool report,
			     struct gcc_options *opts ATTRIBUTE_UNUSED)
{
#ifndef TARGET_GLIBC_MAJOR
#define TARGET_GLIBC_MAJOR 0
#endif
#ifndef TARGET_GLIBC_MINOR
#define TARGET_GLIBC_MINOR 0
#endif
  /* Note: Can't test DEFAULT_ABI here, it isn't set until later.  */
  if (TARGET_GLIBC_MAJOR * 1000 + TARGET_GLIBC_MINOR >= 2018
      && TARGET_64BIT
      && TARGET_ELF)
    return true;

  if (report)
    error ("%<-fsplit-stack%> currently only supported on PowerPC64 GNU/Linux with glibc-2.18 or later");
  return false;
}

#undef TARGET_HANDLE_OPTION
#define TARGET_HANDLE_OPTION rs6000_handle_option

#undef TARGET_OPTION_INIT_STRUCT
#define TARGET_OPTION_INIT_STRUCT rs6000_option_init_struct

#undef TARGET_OPTION_DEFAULT_PARAMS
#define TARGET_OPTION_DEFAULT_PARAMS rs6000_option_default_params

#undef TARGET_OPTION_OPTIMIZATION_TABLE
#define TARGET_OPTION_OPTIMIZATION_TABLE rs6000_option_optimization_table

#undef TARGET_SUPPORTS_SPLIT_STACK
#define TARGET_SUPPORTS_SPLIT_STACK rs6000_supports_split_stack

struct gcc_targetm_common targetm_common = TARGETM_COMMON_INITIALIZER;
