/* Common hooks for IBM RS/6000.
   Copyright (C) 1991-2021 Free Software Foundation, Inc.

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

/* Implement TARGET_OPTION_OPTIMIZATION_TABLE.  */
static const struct default_options rs6000_option_optimization_table[] =
  {
    /* Split multi-word types early.  */
    { OPT_LEVELS_ALL, OPT_fsplit_wide_types_early, NULL, 1 },
    /* Enable -fsched-pressure for first pass instruction scheduling.  */
    { OPT_LEVELS_1_PLUS, OPT_fsched_pressure, NULL, 1 },
    /* Enable -munroll-only-small-loops with -funroll-loops to unroll small
       loops at -O2 and above by default.  */
    { OPT_LEVELS_2_PLUS_SPEED_ONLY, OPT_funroll_loops, NULL, 1 },
    { OPT_LEVELS_2_PLUS_SPEED_ONLY, OPT_munroll_only_small_loops, NULL, 1 },

    /* -frename-registers leads to non-optimal codegen and performance
       on rs6000, turn it off by default.  */
    { OPT_LEVELS_ALL, OPT_frename_registers, NULL, 0 },

    /* Double growth factor to counter reduced min jump length.  */
    { OPT_LEVELS_ALL, OPT__param_max_grow_copy_bb_insns_, NULL, 16 },
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

  /* By default, always emit DWARF-2 unwind info.  This allows debugging
     without maintaining a stack frame back-chain.  It also allows the
     debugger to find out where on-entry register values are stored at any
     point in a function, without having to analyze the executable code (which
     isn't even possible to do in the general case).  */
#ifdef OBJECT_FORMAT_ELF
  opts->x_flag_asynchronous_unwind_tables = 1;
#endif
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

    case OPT_mlong_double_:
      if (value != 64 && value != 128)
	{
	  error_at (loc, "unknown switch %<-mlong-double-%s%>", arg);
	  opts->x_rs6000_long_double_type_size
	    = RS6000_DEFAULT_LONG_DOUBLE_SIZE;
	  return false;
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

#undef TARGET_OPTION_OPTIMIZATION_TABLE
#define TARGET_OPTION_OPTIMIZATION_TABLE rs6000_option_optimization_table

#undef TARGET_SUPPORTS_SPLIT_STACK
#define TARGET_SUPPORTS_SPLIT_STACK rs6000_supports_split_stack

struct gcc_targetm_common targetm_common = TARGETM_COMMON_INITIALIZER;
