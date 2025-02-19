/* Common hooks for GCN
   Copyright (C) 2016-2025 Free Software Foundation, Inc.

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

/* Set default optimization options.  */
static const struct default_options gcn_option_optimization_table[] =
  {
    { OPT_LEVELS_3_PLUS, OPT_fomit_frame_pointer, NULL, 1 },
    { OPT_LEVELS_NONE, 0, NULL, 0 }
  };

#undef  TARGET_OPTION_OPTIMIZATION_TABLE
#define TARGET_OPTION_OPTIMIZATION_TABLE gcn_option_optimization_table

static enum unwind_info_type
gcn_except_unwind_info (struct gcc_options *opts ATTRIBUTE_UNUSED)
{
  return UI_TARGET;
}

#undef  TARGET_EXCEPT_UNWIND_INFO
#define TARGET_EXCEPT_UNWIND_INFO gcn_except_unwind_info

struct gcc_targetm_common targetm_common = TARGETM_COMMON_INITIALIZER;
