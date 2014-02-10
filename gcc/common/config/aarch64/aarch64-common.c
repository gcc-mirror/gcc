/* Common hooks for AArch64.
   Copyright (C) 2012-2014 Free Software Foundation, Inc.
   Contributed by ARM Ltd.

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

#ifdef  TARGET_BIG_ENDIAN_DEFAULT
#undef  TARGET_DEFAULT_TARGET_FLAGS
#define TARGET_DEFAULT_TARGET_FLAGS (MASK_BIG_END)
#endif

#undef  TARGET_HANDLE_OPTION
#define TARGET_HANDLE_OPTION aarch64_handle_option

#undef	TARGET_OPTION_OPTIMIZATION_TABLE
#define TARGET_OPTION_OPTIMIZATION_TABLE aarch_option_optimization_table

/* Set default optimization options.  */
static const struct default_options aarch_option_optimization_table[] =
  {
    /* Enable section anchors by default at -O1 or higher.  */
    { OPT_LEVELS_1_PLUS, OPT_fsection_anchors, NULL, 1 },
    /* Enable redundant extension instructions removal at -O2 and higher.  */
    { OPT_LEVELS_2_PLUS, OPT_free, NULL, 1 },
    { OPT_LEVELS_NONE, 0, NULL, 0 }
  };

/* Implement TARGET_HANDLE_OPTION.
   This function handles the target specific options for CPU/target selection.

   -mcpu=CPU is shorthand for -march=ARCH_FOR_CPU, -mtune=CPU.
   If either of -march or -mtune is given, they override their
   respective component of -mcpu.  This logic is implemented
   in config/aarch64/aarch64.c:aarch64_override_options.  */

static bool
aarch64_handle_option (struct gcc_options *opts,
		       struct gcc_options *opts_set ATTRIBUTE_UNUSED,
		       const struct cl_decoded_option *decoded,
		       location_t loc ATTRIBUTE_UNUSED)
{
  size_t code = decoded->opt_index;
  const char *arg = decoded->arg;

  switch (code)
    {
    case OPT_march_:
      opts->x_aarch64_arch_string = arg;
      return true;

    case OPT_mcpu_:
      opts->x_aarch64_cpu_string = arg;
      return true;

    case OPT_mtune_:
      opts->x_aarch64_tune_string = arg;
      return true;

    default:
      return true;
    }
}

struct gcc_targetm_common targetm_common = TARGETM_COMMON_INITIALIZER;

#define AARCH64_CPU_NAME_LENGTH 20

/* Truncate NAME at the first '.' character seen, or return
   NAME unmodified.  */

const char *
aarch64_rewrite_selected_cpu (const char *name)
{
  static char output_buf[AARCH64_CPU_NAME_LENGTH + 1] = {0};
  char *arg_pos;

  strncpy (output_buf, name, AARCH64_CPU_NAME_LENGTH);
  arg_pos = strchr (output_buf, '.');

  /* If we found a '.' truncate the entry at that point.  */
  if (arg_pos)
    *arg_pos = '\0';

  return output_buf;
}

/* Called by the driver to rewrite a name passed to the -mcpu
   argument in preparation to be passed to the assembler.  The
   names passed from the commend line will be in ARGV, we want
   to use the right-most argument, which should be in
   ARGV[ARGC - 1].  ARGC should always be greater than 0.  */

const char *
aarch64_rewrite_mcpu (int argc, const char **argv)
{
  gcc_assert (argc);
  return aarch64_rewrite_selected_cpu (argv[argc - 1]);
}

#undef AARCH64_CPU_NAME_LENGTH

