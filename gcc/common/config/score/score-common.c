/* Common hooks for Sunplus S+CORE.
   Copyright (C) 2005-2014 Free Software Foundation, Inc.

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
#include "tm.h"
#include "common/common-target.h"
#include "common/common-target-def.h"
#include "opts.h"
#include "flags.h"

/* Implement TARGET_OPTION_OPTIMIZATION_TABLE.  */
static const struct default_options score_option_optimization_table[] =
  {
    { OPT_LEVELS_1_PLUS, OPT_fomit_frame_pointer, NULL, 1 },
    { OPT_LEVELS_NONE, 0, NULL, 0 }
  };

#undef TARGET_DEFAULT_TARGET_FLAGS
#define TARGET_DEFAULT_TARGET_FLAGS     TARGET_DEFAULT

#undef TARGET_HANDLE_OPTION
#define TARGET_HANDLE_OPTION            score_handle_option

#undef TARGET_OPTION_OPTIMIZATION_TABLE
#define TARGET_OPTION_OPTIMIZATION_TABLE score_option_optimization_table

#define MASK_ALL_CPU_BITS	(MASK_SCORE7 | MASK_SCORE7D)

/* Implement TARGET_HANDLE_OPTION.  */
static bool
score_handle_option (struct gcc_options *opts,
		     struct gcc_options *opts_set ATTRIBUTE_UNUSED,
		     const struct cl_decoded_option *decoded,
		     location_t loc ATTRIBUTE_UNUSED)
{
  size_t code = decoded->opt_index;
  int value = decoded->value;

  switch (code)
    {
    case OPT_mscore7d:
      opts->x_target_flags &= ~(MASK_ALL_CPU_BITS);
      opts->x_target_flags |= MASK_SCORE7 | MASK_SCORE7D;
      return true;

    case OPT_march_:
      opts->x_target_flags &= ~(MASK_ALL_CPU_BITS);
      opts->x_target_flags |= value;
      return true;

    default:
      return true;
    }
}

struct gcc_targetm_common targetm_common = TARGETM_COMMON_INITIALIZER;
