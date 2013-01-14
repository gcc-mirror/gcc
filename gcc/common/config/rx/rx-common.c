/* Common hooks for Renesas RX.
   Copyright (C) 2008-2013 Free Software Foundation, Inc.

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

/* Extra processing for target specific command line options.  */

static bool
rx_handle_option (struct gcc_options *opts,
		  struct gcc_options *opts_set ATTRIBUTE_UNUSED,
		  const struct cl_decoded_option *decoded,
		  location_t loc)
{
  size_t code = decoded->opt_index;
  int value = decoded->value;

  switch (code)
    {
    case OPT_mint_register_:
      /* Make sure that the -mint-register option is in range.  Other
	 handling in rx_option_override.  */
      return value >= 0 && value <= 4;
      break;

    case OPT_mmax_constant_size_:
      /* Make sure that the -mmax-constant_size option is in range.  */
      return value >= 0 && value <= 4;

    case OPT_mcpu_:
      if ((enum rx_cpu_types) value == RX200)
	opts->x_target_flags |= MASK_NO_USE_FPU;
      break;
      
    case OPT_fpu:
      if (opts->x_rx_cpu_type == RX200)
	error_at (loc, "the RX200 cpu does not have FPU hardware");
      break;

    default:
      break;
    }

  return true;
}

/* Implement TARGET_OPTION_OPTIMIZATION_TABLE.  */
static const struct default_options rx_option_optimization_table[] =
  {
    { OPT_LEVELS_1_PLUS, OPT_fomit_frame_pointer, NULL, 1 },
    { OPT_LEVELS_NONE, 0, NULL, 0 }
  };

#undef  TARGET_HANDLE_OPTION
#define TARGET_HANDLE_OPTION			rx_handle_option

#undef  TARGET_OPTION_OPTIMIZATION_TABLE
#define TARGET_OPTION_OPTIMIZATION_TABLE	rx_option_optimization_table

#undef  TARGET_EXCEPT_UNWIND_INFO
#define TARGET_EXCEPT_UNWIND_INFO		sjlj_except_unwind_info

struct gcc_targetm_common targetm_common = TARGETM_COMMON_INITIALIZER;
