/* Common hooks for TILE-Gx.
   Copyright (C) 2011, 2012
   Free Software Foundation, Inc.
   Contributed by Walter Lee (walt@tilera.com)

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
#include "diagnostic-core.h"
#include "tm.h"
#include "common/common-target.h"
#include "common/common-target-def.h"
#include "opts.h"
#include "flags.h"

static const struct default_options tilegx_option_optimization_table[] = {
  {OPT_LEVELS_1_PLUS, OPT_fomit_frame_pointer, NULL, 1},
  /* Scheduling and bundling are super important for our architecture, so
     enable them at -O1. */
  {OPT_LEVELS_1_PLUS, OPT_fschedule_insns, NULL, 1},
  {OPT_LEVELS_1_PLUS, OPT_fschedule_insns2, NULL, 1},
  {OPT_LEVELS_NONE, 0, NULL, 0}
};


static void
tilegx_option_init_struct (struct gcc_options *opts)
{
  opts->x_flag_asynchronous_unwind_tables = 1;
}


#undef  TARGET_OPTION_OPTIMIZATION_TABLE
#define TARGET_OPTION_OPTIMIZATION_TABLE tilegx_option_optimization_table

#undef  TARGET_OPTION_INIT_STRUCT
#define TARGET_OPTION_INIT_STRUCT tilegx_option_init_struct

struct gcc_targetm_common targetm_common = TARGETM_COMMON_INITIALIZER;
