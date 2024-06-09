/* Common hooks for Matsushita MN10300 series.
   Copyright (C) 1996-2024 Free Software Foundation, Inc.

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
static const struct default_options mn10300_option_optimization_table[] =
  {
    /* The STC algorithm produces the smallest code at -Os.  */
    { OPT_LEVELS_2_PLUS, OPT_freorder_blocks_algorithm_, NULL,
      REORDER_BLOCKS_ALGORITHM_STC },
    { OPT_LEVELS_NONE, 0, NULL, 0 }
  };

/* Implement TARGET_HANDLE_OPTION.  */

static bool
mn10300_handle_option (struct gcc_options *opts,
		       struct gcc_options *opts_set ATTRIBUTE_UNUSED,
		       const struct cl_decoded_option *decoded,
		       location_t loc ATTRIBUTE_UNUSED)
{
  size_t code = decoded->opt_index;
  int value = decoded->value;

  switch (code)
    {
    case OPT_mam33:
      opts->x_mn10300_processor = value ? PROCESSOR_AM33 : PROCESSOR_MN10300;
      return true;

    case OPT_mam33_2:
      opts->x_mn10300_processor = (value
				   ? PROCESSOR_AM33_2
				   : MIN (PROCESSOR_AM33, PROCESSOR_DEFAULT));
      return true;

    case OPT_mam34:
      opts->x_mn10300_processor = (value ? PROCESSOR_AM34 : PROCESSOR_DEFAULT);
      return true;

    default:
      return true;
    }
}

#undef  TARGET_EXCEPT_UNWIND_INFO
#define TARGET_EXCEPT_UNWIND_INFO sjlj_except_unwind_info

#undef  TARGET_DEFAULT_TARGET_FLAGS
#define TARGET_DEFAULT_TARGET_FLAGS MASK_MULT_BUG | MASK_PTR_A0D0 | MASK_ALLOW_LIW | MASK_ALLOW_SETLB
#undef  TARGET_HANDLE_OPTION
#define TARGET_HANDLE_OPTION mn10300_handle_option
#undef  TARGET_OPTION_OPTIMIZATION_TABLE
#define TARGET_OPTION_OPTIMIZATION_TABLE mn10300_option_optimization_table

struct gcc_targetm_common targetm_common = TARGETM_COMMON_INITIALIZER;
