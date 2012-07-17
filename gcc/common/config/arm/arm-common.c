/* Common hooks for ARM.
   Copyright (C) 1991, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001,
   2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011
   Free Software Foundation, Inc.

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
#include "tm_p.h"
#include "common/common-target.h"
#include "common/common-target-def.h"
#include "opts.h"
#include "flags.h"

/* Set default optimization options.  */
static const struct default_options arm_option_optimization_table[] =
  {
    /* Enable section anchors by default at -O1 or higher.  */
    { OPT_LEVELS_1_PLUS, OPT_fsection_anchors, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_fomit_frame_pointer, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_fsched_pressure, NULL, 1 },
    { OPT_LEVELS_NONE, 0, NULL, 0 }
  };

/* Implement TARGET_EXCEPT_UNWIND_INFO.  */

enum unwind_info_type
arm_except_unwind_info (struct gcc_options *opts)
{
  /* Honor the --enable-sjlj-exceptions configure switch.  */
#ifdef CONFIG_SJLJ_EXCEPTIONS
  if (CONFIG_SJLJ_EXCEPTIONS)
    return UI_SJLJ;
#endif

  /* If not using ARM EABI unwind tables... */
  if (ARM_UNWIND_INFO)
    {
      /* For simplicity elsewhere in this file, indicate that all unwind
	 info is disabled if we're not emitting unwind tables.  */
      if (!opts->x_flag_exceptions && !opts->x_flag_unwind_tables)
	return UI_NONE;
      else
	return UI_TARGET;
    }

  /* ... we use sjlj exceptions for backwards compatibility.  */
  return UI_SJLJ;
}

#undef  TARGET_DEFAULT_TARGET_FLAGS
#define TARGET_DEFAULT_TARGET_FLAGS (TARGET_DEFAULT | MASK_SCHED_PROLOG)

#undef  TARGET_OPTION_OPTIMIZATION_TABLE
#define TARGET_OPTION_OPTIMIZATION_TABLE arm_option_optimization_table

#undef TARGET_EXCEPT_UNWIND_INFO
#define TARGET_EXCEPT_UNWIND_INFO  arm_except_unwind_info

struct gcc_targetm_common targetm_common = TARGETM_COMMON_INITIALIZER;
