/* Common hooks for ARM.
   Copyright (C) 1991-2014 Free Software Foundation, Inc.

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

#define ARM_CPU_NAME_LENGTH 20

/* Truncate NAME at the first '.' character seen, or return
   NAME unmodified.  */

const char *
arm_rewrite_selected_cpu (const char *name)
{
  static char output_buf[ARM_CPU_NAME_LENGTH + 1] = {0};
  char *arg_pos;

  strncpy (output_buf, name, ARM_CPU_NAME_LENGTH);
  arg_pos = strchr (output_buf, '.');

  /* If we found a '.' truncate the entry at that point.  */
  if (arg_pos)
    *arg_pos = '\0';

  return output_buf;
}

/* Called by the driver to rewrite a name passed to the -mcpu
   argument in preparation to be passed to the assembler.  The
   name will be in ARGV[0], ARGC should always be 1.  */

const char *
arm_rewrite_mcpu (int argc, const char **argv)
{
  gcc_assert (argc == 1);
  return arm_rewrite_selected_cpu (argv[0]);
}

#undef ARM_CPU_NAME_LENGTH


#undef  TARGET_DEFAULT_TARGET_FLAGS
#define TARGET_DEFAULT_TARGET_FLAGS (TARGET_DEFAULT | MASK_SCHED_PROLOG)

#undef  TARGET_OPTION_OPTIMIZATION_TABLE
#define TARGET_OPTION_OPTIMIZATION_TABLE arm_option_optimization_table

#undef TARGET_EXCEPT_UNWIND_INFO
#define TARGET_EXCEPT_UNWIND_INFO  arm_except_unwind_info

struct gcc_targetm_common targetm_common = TARGETM_COMMON_INITIALIZER;
