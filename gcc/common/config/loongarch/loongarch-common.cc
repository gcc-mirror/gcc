/* Common hooks for LoongArch.
   Copyright (C) 2021-2023 Free Software Foundation, Inc.

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
#include "diagnostic-core.h"

#undef	TARGET_OPTION_OPTIMIZATION_TABLE
#define TARGET_OPTION_OPTIMIZATION_TABLE loongarch_option_optimization_table

/* Set default optimization options.  */
static const struct default_options loongarch_option_optimization_table[] =
{
  { OPT_LEVELS_ALL, OPT_fasynchronous_unwind_tables, NULL, 1 },
  { OPT_LEVELS_1_PLUS, OPT_fsection_anchors, NULL, 1 },
  { OPT_LEVELS_NONE, 0, NULL, 0 }
};

struct gcc_targetm_common targetm_common = TARGETM_COMMON_INITIALIZER;
