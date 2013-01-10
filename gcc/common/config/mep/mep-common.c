/* Common hooks for Toshiba Media Processor.
   Copyright (C) 2001-2013 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

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

static const struct default_options mep_option_optimization_table[] =
  {
    /* The first scheduling pass often increases register pressure and
       tends to result in more spill code.  Only run it when
       specifically asked.  */
    { OPT_LEVELS_ALL, OPT_fschedule_insns, NULL, 0 },

    /* Using $fp doesn't gain us much, even when debugging is
       important.  */
    { OPT_LEVELS_ALL, OPT_fomit_frame_pointer, NULL, 1 },

    { OPT_LEVELS_NONE, 0, NULL, 0 }
  };

static bool
mep_handle_option (struct gcc_options *opts,
		   struct gcc_options *opts_set ATTRIBUTE_UNUSED,
		   const struct cl_decoded_option *decoded,
		   location_t loc ATTRIBUTE_UNUSED)
{
  size_t code = decoded->opt_index;

  switch (code)
    {
    case OPT_mall_opts:
      opts->x_target_flags |= MEP_ALL_OPTS;
      break;

    case OPT_mno_opts:
      opts->x_target_flags &= ~ MEP_ALL_OPTS;
      break;

    case OPT_mcop64:
      opts->x_target_flags |= MASK_COP;
      opts->x_target_flags |= MASK_64BIT_CR_REGS;
      break;

    case OPT_mivc2:
      opts->x_target_flags |= MASK_COP;
      opts->x_target_flags |= MASK_64BIT_CR_REGS;
      opts->x_target_flags |= MASK_VLIW;
      opts->x_target_flags |= MASK_OPT_VL64;
      opts->x_target_flags |= MASK_IVC2;

      /* Remaining handling of this option deferred.  */
      break;

    default:
      break;
    }
  return TRUE;
}

#undef  TARGET_HANDLE_OPTION
#define TARGET_HANDLE_OPTION            mep_handle_option
#undef  TARGET_OPTION_OPTIMIZATION_TABLE
#define TARGET_OPTION_OPTIMIZATION_TABLE	mep_option_optimization_table
#undef  TARGET_DEFAULT_TARGET_FLAGS
#define TARGET_DEFAULT_TARGET_FLAGS	TARGET_DEFAULT

struct gcc_targetm_common targetm_common = TARGETM_COMMON_INITIALIZER;
