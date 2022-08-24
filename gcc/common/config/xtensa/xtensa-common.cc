/* Common hooks for Tensilica's Xtensa architecture.
   Copyright (C) 2001-2022 Free Software Foundation, Inc.

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

/* Implement TARGET_OPTION_OPTIMIZATION_TABLE.  */

static const struct default_options xtensa_option_optimization_table[] =
  {
    /* Reordering blocks for Xtensa is not a good idea unless the
       compiler understands the range of conditional branches.
       Currently all branch relaxation for Xtensa is handled in the
       assembler, so GCC cannot do a good job of reordering blocks.
       Do not enable reordering unless it is explicitly requested.  */
    { OPT_LEVELS_ALL, OPT_freorder_blocks, NULL, 0 },
    /* Split multi-word types early (pre-GCC10 behavior).  */
    { OPT_LEVELS_ALL, OPT_fsplit_wide_types_early, NULL, 1 },
    { OPT_LEVELS_NONE, 0, NULL, 0 }
  };

#undef TARGET_DEFAULT_TARGET_FLAGS
#define TARGET_DEFAULT_TARGET_FLAGS (TARGET_DEFAULT)
#undef TARGET_OPTION_OPTIMIZATION_TABLE
#define TARGET_OPTION_OPTIMIZATION_TABLE xtensa_option_optimization_table

struct gcc_targetm_common targetm_common = TARGETM_COMMON_INITIALIZER;
