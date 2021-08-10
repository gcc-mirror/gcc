/* Common hooks for LoongArch.
   Copyright (C) 2020-2021 Free Software Foundation, Inc.

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

/* Implement TARGET_HANDLE_OPTION.  */

static bool
loongarch_handle_option (struct gcc_options *opts,
			 struct gcc_options *opts_set ATTRIBUTE_UNUSED,
			 const struct cl_decoded_option *decoded,
			 location_t loc ATTRIBUTE_UNUSED)
{
  size_t code = decoded->opt_index;

  switch (code)
    {
    case OPT_mno_flush_func:
      opts->x_loongarch_cache_flush_func = NULL;
      return true;

    default:
      return true;
    }
}

#undef TARGET_DEFAULT_TARGET_FLAGS
#define TARGET_DEFAULT_TARGET_FLAGS		\
  (TARGET_DEFAULT				\
   | TARGET_CPU_DEFAULT				\
   | MASK_CHECK_ZERO_DIV)
#undef TARGET_HANDLE_OPTION
#define TARGET_HANDLE_OPTION loongarch_handle_option

struct gcc_targetm_common targetm_common = TARGETM_COMMON_INITIALIZER;
