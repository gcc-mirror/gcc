/* Common hooks for IA64.
   Copyright (C) 1999-2017 Free Software Foundation, Inc.

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
#include "diagnostic-core.h"
#include "tm.h"
#include "memmodel.h"
#include "tm_p.h"
#include "common/common-target.h"
#include "common/common-target-def.h"
#include "opts.h"
#include "flags.h"
#include "params.h"

/* Implement overriding of the optimization options.  */
static const struct default_options ia64_option_optimization_table[] =
  {
    { OPT_LEVELS_1_PLUS, OPT_fomit_frame_pointer, NULL, 1 },
#ifdef SUBTARGET_OPTIMIZATION_OPTIONS
    SUBTARGET_OPTIMIZATION_OPTIONS,
#endif
    { OPT_LEVELS_NONE, 0, NULL, 0 }
  };

/* Implement TARGET_HANDLE_OPTION.  */

static bool
ia64_handle_option (struct gcc_options *opts ATTRIBUTE_UNUSED,
		    struct gcc_options *opts_set ATTRIBUTE_UNUSED,
		    const struct cl_decoded_option *decoded,
		    location_t loc)
{
  size_t code = decoded->opt_index;
  const char *arg = decoded->arg;
  int value = decoded->value;

  switch (code)
    {
    case OPT_mtls_size_:
      if (value != 14 && value != 22 && value != 64)
	error_at (loc, "bad value %<%s%> for -mtls-size= switch", arg);
      return true;

    default:
      return true;
    }
}

/* Implement TARGET_EXCEPT_UNWIND_INFO.  */

enum unwind_info_type
ia64_except_unwind_info (struct gcc_options *opts)
{
  /* Honor the --enable-sjlj-exceptions configure switch.  */
#ifdef CONFIG_SJLJ_EXCEPTIONS
  if (CONFIG_SJLJ_EXCEPTIONS)
    return UI_SJLJ;
#endif

  /* For simplicity elsewhere in this file, indicate that all unwind
     info is disabled if we're not emitting unwind tables.  */
  if (!opts->x_flag_exceptions && !opts->x_flag_unwind_tables)
    return UI_NONE;

  return UI_TARGET;
}

/* Implement TARGET_OPTION_DEFAULT_PARAMS.  */

static void
ia64_option_default_params (void)
{
  /* Let the scheduler form additional regions.  */
  set_default_param_value (PARAM_MAX_SCHED_EXTEND_REGIONS_ITERS, 2);

  /* Set the default values for cache-related parameters.  */
  set_default_param_value (PARAM_SIMULTANEOUS_PREFETCHES, 6);
  set_default_param_value (PARAM_L1_CACHE_LINE_SIZE, 32);

  set_default_param_value (PARAM_SCHED_MEM_TRUE_DEP_COST, 4);
}

#undef TARGET_OPTION_OPTIMIZATION_TABLE
#define TARGET_OPTION_OPTIMIZATION_TABLE ia64_option_optimization_table
#undef TARGET_OPTION_DEFAULT_PARAMS
#define TARGET_OPTION_DEFAULT_PARAMS ia64_option_default_params

#undef TARGET_EXCEPT_UNWIND_INFO
#define TARGET_EXCEPT_UNWIND_INFO  ia64_except_unwind_info

#undef TARGET_DEFAULT_TARGET_FLAGS
#define TARGET_DEFAULT_TARGET_FLAGS (TARGET_DEFAULT | TARGET_CPU_DEFAULT)
#undef TARGET_HANDLE_OPTION
#define TARGET_HANDLE_OPTION ia64_handle_option

struct gcc_targetm_common targetm_common = TARGETM_COMMON_INITIALIZER;
