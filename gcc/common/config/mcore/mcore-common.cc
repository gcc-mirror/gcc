/* Common hooks for Motorola MCore.
   Copyright (C) 1993-2024 Free Software Foundation, Inc.

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
#include "tm.h"
#include "common/common-target.h"
#include "common/common-target-def.h"

/* What options are we going to default to specific settings when
   -O* happens; the user can subsequently override these settings.
  
   Omitting the frame pointer is a very good idea on the MCore.
   Scheduling isn't worth anything on the current MCore implementation.  */

static const struct default_options mcore_option_optimization_table[] =
  {
    { OPT_LEVELS_1_PLUS, OPT_ffunction_cse, NULL, 0 },
    { OPT_LEVELS_ALL, OPT_fcaller_saves, NULL, 0 },
    { OPT_LEVELS_ALL, OPT_fschedule_insns, NULL, 0 },
    { OPT_LEVELS_ALL, OPT_fschedule_insns2, NULL, 0 },
    { OPT_LEVELS_SIZE, OPT_mhardlit, NULL, 0 },
    { OPT_LEVELS_NONE, 0, NULL, 0 }
  };

#undef  TARGET_DEFAULT_TARGET_FLAGS
#define TARGET_DEFAULT_TARGET_FLAGS	TARGET_DEFAULT

#undef TARGET_OPTION_OPTIMIZATION_TABLE
#define TARGET_OPTION_OPTIMIZATION_TABLE mcore_option_optimization_table

#undef TARGET_EXCEPT_UNWIND_INFO
#define TARGET_EXCEPT_UNWIND_INFO sjlj_except_unwind_info

struct gcc_targetm_common targetm_common = TARGETM_COMMON_INITIALIZER;
