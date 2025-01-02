/* Common hooks for Adapteva Epiphany
   Copyright (C) 1994-2025 Free Software Foundation, Inc.
   Contributed by Embecosm on behalf of Adapteva, Inc.

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
#include "common/common-target.h"
#include "opts.h"
#include "options.h"

#define TARGET_OPTION_OPTIMIZATION_TABLE epiphany_option_optimization_table

#define TARGET_DEFAULT_TARGET_FLAGS \
  (MASK_CMOVE | MASK_SOFT_CMPSF | MASK_SPLIT_LOHI | MASK_ROUND_NEAREST \
   | MASK_VECT_DOUBLE | MASK_POST_INC | MASK_POST_MODIFY)

#define TARGET_HAVE_NAMED_SECTIONS true

#include "common/common-target-def.h"

/* Implement TARGET_OPTION_OPTIMIZATION_TABLE.  */
static const struct default_options epiphany_option_optimization_table[] =
  {
    /* Enable section anchors by default at -O1 or higher.  */
    { OPT_LEVELS_1_PLUS, OPT_fsection_anchors, NULL, 1 },
    { OPT_LEVELS_NONE, 0, NULL, 0 }
  };

struct gcc_targetm_common targetm_common = TARGETM_COMMON_INITIALIZER;
