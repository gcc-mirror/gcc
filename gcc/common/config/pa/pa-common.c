/* HPPA common hooks.
   Copyright (C) 1992-2016 Free Software Foundation, Inc.

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
#include "common/common-target.h"
#include "common/common-target-def.h"
#include "opts.h"
#include "flags.h"

/* Implement TARGET_OPTION_OPTIMIZATION_TABLE.  */
static const struct default_options pa_option_optimization_table[] =
  {
    { OPT_LEVELS_1_PLUS, OPT_fomit_frame_pointer, NULL, 1 },
    { OPT_LEVELS_NONE, 0, NULL, 0 }
  };

/* Implement TARGET_HANDLE_OPTION.  */

static bool
pa_handle_option (struct gcc_options *opts,
		  struct gcc_options *opts_set ATTRIBUTE_UNUSED,
		  const struct cl_decoded_option *decoded,
		  location_t loc ATTRIBUTE_UNUSED)
{
  size_t code = decoded->opt_index;

  switch (code)
    {
    case OPT_mnosnake:
    case OPT_mpa_risc_1_0:
    case OPT_march_1_0:
      opts->x_target_flags &= ~(MASK_PA_11 | MASK_PA_20);
      return true;

    case OPT_msnake:
    case OPT_mpa_risc_1_1:
    case OPT_march_1_1:
      opts->x_target_flags &= ~MASK_PA_20;
      opts->x_target_flags |= MASK_PA_11;
      return true;

    case OPT_mpa_risc_2_0:
    case OPT_march_2_0:
      opts->x_target_flags |= MASK_PA_11 | MASK_PA_20;
      return true;

    default:
      return true;
    }
}

#undef TARGET_OPTION_OPTIMIZATION_TABLE
#define TARGET_OPTION_OPTIMIZATION_TABLE pa_option_optimization_table
#undef TARGET_DEFAULT_TARGET_FLAGS
#define TARGET_DEFAULT_TARGET_FLAGS (TARGET_DEFAULT | TARGET_CPU_DEFAULT)
#undef TARGET_HANDLE_OPTION
#define TARGET_HANDLE_OPTION pa_handle_option

struct gcc_targetm_common targetm_common = TARGETM_COMMON_INITIALIZER;
