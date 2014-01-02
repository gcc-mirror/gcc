/* Common hooks for SPU.
   Copyright (C) 2006-2014 Free Software Foundation, Inc.

   This file is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3 of the License, or (at your option) 
   any later version.

   This file is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
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
#include "params.h"

static void
spu_option_init_struct (struct gcc_options *opts)
{
  /* With so many registers this is better on by default. */
  opts->x_flag_rename_registers = 1;
}

/* Implement TARGET_OPTION_DEFAULT_PARAMS.  */
static void
spu_option_default_params (void)
{
  /* Override some of the default param values.  With so many registers
     larger values are better for these params.  */
  set_default_param_value (PARAM_MAX_PENDING_LIST_LENGTH, 128);
}

#undef TARGET_DEFAULT_TARGET_FLAGS
#define TARGET_DEFAULT_TARGET_FLAGS (TARGET_DEFAULT)

#undef TARGET_OPTION_INIT_STRUCT
#define TARGET_OPTION_INIT_STRUCT spu_option_init_struct

#undef TARGET_OPTION_DEFAULT_PARAMS
#define TARGET_OPTION_DEFAULT_PARAMS spu_option_default_params

#undef TARGET_EXCEPT_UNWIND_INFO
#define TARGET_EXCEPT_UNWIND_INFO  sjlj_except_unwind_info

struct gcc_targetm_common targetm_common = TARGETM_COMMON_INITIALIZER;
