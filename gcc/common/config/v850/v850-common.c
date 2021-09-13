/* Common hooks for NEC V850 series.
   Copyright (C) 1996-2021 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

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

/* Information about the various small memory areas.  */
static const int small_memory_physical_max[(int) SMALL_MEMORY_max] =
{
  256,
  65536,
  32768,
};

/* Set the maximum size of small memory area TYPE to the value given
   by SIZE in structure OPTS (option text OPT passed at location LOC).  */

static void
v850_handle_memory_option (enum small_memory_type type,
			   struct gcc_options *opts, const char *opt,
			   int size, location_t loc)
{
  if (size > small_memory_physical_max[type])
    error_at (loc, "value passed in %qs is too large", opt);
  else
    opts->x_small_memory_max[type] = size;
}

/* Implement TARGET_HANDLE_OPTION.  */

static bool
v850_handle_option (struct gcc_options *opts,
		    struct gcc_options *opts_set ATTRIBUTE_UNUSED,
		    const struct cl_decoded_option *decoded,
		    location_t loc)
{
  size_t code = decoded->opt_index;
  int value = decoded->value;

  switch (code)
    {
    case OPT_mspace:
      opts->x_target_flags |= MASK_EP | MASK_PROLOG_FUNCTION;
      return true;

    case OPT_mv850:
      opts->x_target_flags &= ~(MASK_CPU ^ MASK_V850);
      return true;

    case OPT_mv850e:
    case OPT_mv850e1:
    case OPT_mv850es:
      opts->x_target_flags &= ~(MASK_CPU ^ MASK_V850E);
      return true;

    case OPT_mv850e2:
      opts->x_target_flags &= ~(MASK_CPU ^ MASK_V850E2);
      return true;

    case OPT_mv850e2v3:
      opts->x_target_flags &= ~(MASK_CPU ^ MASK_V850E2V3);
      return true;

    case OPT_mtda_:
      v850_handle_memory_option (SMALL_MEMORY_TDA, opts,
				 decoded->orig_option_with_args_text,
				 value, loc);
      return true;

    case OPT_msda_:
      v850_handle_memory_option (SMALL_MEMORY_SDA, opts,
				 decoded->orig_option_with_args_text,
				 value, loc);
      return true;

    case OPT_mzda_:
      v850_handle_memory_option (SMALL_MEMORY_ZDA, opts,
				 decoded->orig_option_with_args_text,
				 value, loc);
      return true;

    default:
      return true;
    }
}

/* Implement TARGET_OPTION_OPTIMIZATION_TABLE.  */

static const struct default_options v850_option_optimization_table[] =
  {
    /* Note - we no longer enable MASK_EP when optimizing.  This is
       because of a hardware bug which stops the SLD and SST instructions
       from correctly detecting some hazards.  If the user is sure that
       their hardware is fixed or that their program will not encounter
       the conditions that trigger the bug then they can enable -mep by
       hand.  */
    { OPT_LEVELS_1_PLUS, OPT_mprolog_function, NULL, 1 },
    { OPT_LEVELS_NONE, 0, NULL, 0 }
  };

#undef  TARGET_DEFAULT_TARGET_FLAGS
#define TARGET_DEFAULT_TARGET_FLAGS \
  (MASK_DEFAULT | MASK_APP_REGS | MASK_BIG_SWITCH)
#undef  TARGET_HANDLE_OPTION
#define TARGET_HANDLE_OPTION v850_handle_option
#undef  TARGET_OPTION_OPTIMIZATION_TABLE
#define TARGET_OPTION_OPTIMIZATION_TABLE v850_option_optimization_table

struct gcc_targetm_common targetm_common = TARGETM_COMMON_INITIALIZER;
