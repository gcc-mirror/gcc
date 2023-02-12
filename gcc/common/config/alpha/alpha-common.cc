/* Common hooks for DEC Alpha.
   Copyright (C) 1992-2023 Free Software Foundation, Inc.

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
#include "common/common-target.h"
#include "common/common-target-def.h"
#include "opts.h"
#include "flags.h"

/* Implement TARGET_OPTION_OPTIMIZATION_TABLE.  */
static const struct default_options alpha_option_optimization_table[] =
  {
    /* Enable redundant extension instructions removal at -O2 and higher.  */
    { OPT_LEVELS_2_PLUS, OPT_free, NULL, 1 },
    { OPT_LEVELS_NONE, 0, NULL, 0 }
  };

/* Implement TARGET_OPTION_INIT_STRUCT.  */

static void
alpha_option_init_struct (struct gcc_options *opts ATTRIBUTE_UNUSED)
{
#if TARGET_ABI_OPEN_VMS
  /* Enable section anchors by default.  */
  opts->x_flag_section_anchors = 1;
#endif
}

/* Implement TARGET_HANDLE_OPTION.  */

static bool
alpha_handle_option (struct gcc_options *opts,
		     struct gcc_options *opts_set ATTRIBUTE_UNUSED,
		     const struct cl_decoded_option *decoded,
		     location_t loc)
{
  size_t code = decoded->opt_index;
  const char *arg = decoded->arg;
  int value = decoded->value;

  switch (code)
    {
    case OPT_mfp_regs:
      if (value == 0)
	opts->x_target_flags |= MASK_SOFT_FP;
      break;

    case OPT_mieee:
    case OPT_mieee_with_inexact:
      opts->x_target_flags |= MASK_IEEE_CONFORMANT;
      break;

    case OPT_mtls_size_:
      if (value != 16 && value != 32 && value != 64)
	error_at (loc, "bad value %qs for %<-mtls-size%> switch", arg);
      break;
    }

  return true;
}

#undef TARGET_DEFAULT_TARGET_FLAGS
#define TARGET_DEFAULT_TARGET_FLAGS \
  (TARGET_DEFAULT | TARGET_CPU_DEFAULT | TARGET_DEFAULT_EXPLICIT_RELOCS)
#undef TARGET_HANDLE_OPTION
#define TARGET_HANDLE_OPTION alpha_handle_option

#undef TARGET_OPTION_INIT_STRUCT
#define TARGET_OPTION_INIT_STRUCT alpha_option_init_struct

#undef TARGET_OPTION_OPTIMIZATION_TABLE
#define TARGET_OPTION_OPTIMIZATION_TABLE alpha_option_optimization_table

struct gcc_targetm_common targetm_common = TARGETM_COMMON_INITIALIZER;
