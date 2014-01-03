/* Common hooks for CRIS.
   Copyright (C) 1998-2014 Free Software Foundation, Inc.

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

static const struct default_options cris_option_optimization_table[] =
  {
    { OPT_LEVELS_2_PLUS, OPT_fomit_frame_pointer, NULL, 1 },
    { OPT_LEVELS_NONE, 0, NULL, 0 }
  };

/* TARGET_HANDLE_OPTION worker.  We just store the values into local
   variables here.  Checks for correct semantics are in
   cris_option_override.  */

static bool
cris_handle_option (struct gcc_options *opts,
		    struct gcc_options *opts_set ATTRIBUTE_UNUSED,
		    const struct cl_decoded_option *decoded,
		    location_t loc ATTRIBUTE_UNUSED)
{
  size_t code = decoded->opt_index;

  switch (code)
    {
    case OPT_metrax100:
      opts->x_target_flags
	|= (MASK_SVINTO
	    + MASK_ETRAX4_ADD
	    + MASK_ALIGN_BY_32);
      break;

    case OPT_mno_etrax100:
      opts->x_target_flags
	&= ~(MASK_SVINTO
	     + MASK_ETRAX4_ADD
	     + MASK_ALIGN_BY_32);
      break;

    case OPT_m32_bit:
    case OPT_m32bit:
      opts->x_target_flags
	|= (MASK_STACK_ALIGN
	    + MASK_CONST_ALIGN
	    + MASK_DATA_ALIGN
	    + MASK_ALIGN_BY_32);
      break;

    case OPT_m16_bit:
    case OPT_m16bit:
      opts->x_target_flags
	|= (MASK_STACK_ALIGN
	    + MASK_CONST_ALIGN
	    + MASK_DATA_ALIGN);
      break;

    case OPT_m8_bit:
    case OPT_m8bit:
      opts->x_target_flags
	&= ~(MASK_STACK_ALIGN
	     + MASK_CONST_ALIGN
	     + MASK_DATA_ALIGN);
      break;

    default:
      break;
    }

  return true;
}

#undef TARGET_DEFAULT_TARGET_FLAGS
#define TARGET_DEFAULT_TARGET_FLAGS (TARGET_DEFAULT | CRIS_SUBTARGET_DEFAULT)
#undef TARGET_HANDLE_OPTION
#define TARGET_HANDLE_OPTION cris_handle_option
#undef TARGET_OPTION_OPTIMIZATION_TABLE
#define TARGET_OPTION_OPTIMIZATION_TABLE cris_option_optimization_table

struct gcc_targetm_common targetm_common = TARGETM_COMMON_INITIALIZER;
