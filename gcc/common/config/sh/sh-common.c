/* Common hooks for Renesas / SuperH SH.
   Copyright (C) 1993-2013 Free Software Foundation, Inc.

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
#include "params.h"

/* Set default optimization options.  */
static const struct default_options sh_option_optimization_table[] =
  {
    { OPT_LEVELS_1_PLUS, OPT_fomit_frame_pointer, NULL, 1 },
    { OPT_LEVELS_1_PLUS_SPEED_ONLY, OPT_mdiv_, "inv:minlat", 1 },
    { OPT_LEVELS_SIZE, OPT_mdiv_, SH_DIV_STR_FOR_SIZE, 1 },
    { OPT_LEVELS_0_ONLY, OPT_mdiv_, "", 1 },
    { OPT_LEVELS_SIZE, OPT_mcbranchdi, NULL, 0 },
    /* We can't meaningfully test TARGET_SHMEDIA here, because -m
       options haven't been parsed yet, hence we'd read only the
       default.  sh_target_reg_class will return NO_REGS if this is
       not SHMEDIA, so it's OK to always set
       flag_branch_target_load_optimize.  */
    { OPT_LEVELS_2_PLUS, OPT_fbranch_target_load_optimize, NULL, 1 },
    { OPT_LEVELS_NONE, 0, NULL, 0 }
  };

/* Implement TARGET_HANDLE_OPTION.  */

static bool
sh_handle_option (struct gcc_options *opts,
		  struct gcc_options *opts_set ATTRIBUTE_UNUSED,
		  const struct cl_decoded_option *decoded,
		  location_t loc ATTRIBUTE_UNUSED)
{
  size_t code = decoded->opt_index;

  switch (code)
    {
    case OPT_m1:
      opts->x_target_flags = (opts->x_target_flags & ~MASK_ARCH) | SELECT_SH1;
      return true;

    case OPT_m2:
      opts->x_target_flags = (opts->x_target_flags & ~MASK_ARCH) | SELECT_SH2;
      return true;

    case OPT_m2a:
      opts->x_target_flags = (opts->x_target_flags & ~MASK_ARCH) | SELECT_SH2A;
      return true;

    case OPT_m2a_nofpu:
      opts->x_target_flags
	= (opts->x_target_flags & ~MASK_ARCH) | SELECT_SH2A_NOFPU;
      return true;

    case OPT_m2a_single:
      opts->x_target_flags
	= (opts->x_target_flags & ~MASK_ARCH) | SELECT_SH2A_SINGLE;
      return true;

    case OPT_m2a_single_only:
      opts->x_target_flags
	= (opts->x_target_flags & ~MASK_ARCH) | SELECT_SH2A_SINGLE_ONLY;
      return true;

    case OPT_m2e:
      opts->x_target_flags = (opts->x_target_flags & ~MASK_ARCH) | SELECT_SH2E;
      return true;

    case OPT_m3:
      opts->x_target_flags = (opts->x_target_flags & ~MASK_ARCH) | SELECT_SH3;
      return true;

    case OPT_m3e:
      opts->x_target_flags = (opts->x_target_flags & ~MASK_ARCH) | SELECT_SH3E;
      return true;

    case OPT_m4:
    case OPT_m4_100:
    case OPT_m4_200:
    case OPT_m4_300:
      opts->x_target_flags = (opts->x_target_flags & ~MASK_ARCH) | SELECT_SH4;
      return true;

    case OPT_m4_nofpu:
    case OPT_m4_100_nofpu:
    case OPT_m4_200_nofpu:
    case OPT_m4_300_nofpu:
    case OPT_m4_340:
    case OPT_m4_400:
    case OPT_m4_500:
      opts->x_target_flags
	= (opts->x_target_flags & ~MASK_ARCH) | SELECT_SH4_NOFPU;
      return true;

    case OPT_m4_single:
    case OPT_m4_100_single:
    case OPT_m4_200_single:
    case OPT_m4_300_single:
      opts->x_target_flags
	= (opts->x_target_flags & ~MASK_ARCH) | SELECT_SH4_SINGLE;
      return true;

    case OPT_m4_single_only:
    case OPT_m4_100_single_only:
    case OPT_m4_200_single_only:
    case OPT_m4_300_single_only:
      opts->x_target_flags
	= (opts->x_target_flags & ~MASK_ARCH) | SELECT_SH4_SINGLE_ONLY;
      return true;

    case OPT_m4a:
      opts->x_target_flags = (opts->x_target_flags & ~MASK_ARCH) | SELECT_SH4A;
      return true;

    case OPT_m4a_nofpu:
    case OPT_m4al:
      opts->x_target_flags
	= (opts->x_target_flags & ~MASK_ARCH) | SELECT_SH4A_NOFPU;
      return true;

    case OPT_m4a_single:
      opts->x_target_flags
	= (opts->x_target_flags & ~MASK_ARCH) | SELECT_SH4A_SINGLE;
      return true;

    case OPT_m4a_single_only:
      opts->x_target_flags
	= (opts->x_target_flags & ~MASK_ARCH) | SELECT_SH4A_SINGLE_ONLY;
      return true;

    case OPT_m5_32media:
      opts->x_target_flags
	= (opts->x_target_flags & ~MASK_ARCH) | SELECT_SH5_32MEDIA;
      return true;

    case OPT_m5_32media_nofpu:
      opts->x_target_flags
	= (opts->x_target_flags & ~MASK_ARCH) | SELECT_SH5_32MEDIA_NOFPU;
      return true;

    case OPT_m5_64media:
      opts->x_target_flags
	= (opts->x_target_flags & ~MASK_ARCH) | SELECT_SH5_64MEDIA;
      return true;

    case OPT_m5_64media_nofpu:
      opts->x_target_flags
	= (opts->x_target_flags & ~MASK_ARCH) | SELECT_SH5_64MEDIA_NOFPU;
      return true;

    case OPT_m5_compact:
      opts->x_target_flags
	= (opts->x_target_flags & ~MASK_ARCH) | SELECT_SH5_COMPACT;
      return true;

    case OPT_m5_compact_nofpu:
      opts->x_target_flags
	= (opts->x_target_flags & ~MASK_ARCH) | SELECT_SH5_COMPACT_NOFPU;
      return true;

    default:
      return true;
    }
}

/* Implement TARGET_OPTION_DEFAULT_PARAMS.  */
static void
sh_option_default_params (void)
{
  set_default_param_value (PARAM_SIMULTANEOUS_PREFETCHES, 2);
}

#undef TARGET_OPTION_OPTIMIZATION_TABLE
#define TARGET_OPTION_OPTIMIZATION_TABLE sh_option_optimization_table
#undef TARGET_OPTION_DEFAULT_PARAMS
#define TARGET_OPTION_DEFAULT_PARAMS sh_option_default_params
#undef TARGET_DEFAULT_TARGET_FLAGS
#define TARGET_DEFAULT_TARGET_FLAGS TARGET_DEFAULT
#undef TARGET_HANDLE_OPTION
#define TARGET_HANDLE_OPTION sh_handle_option

struct gcc_targetm_common targetm_common = TARGETM_COMMON_INITIALIZER;
