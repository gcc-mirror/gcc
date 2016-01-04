/* Common hooks for Motorola 68000 family.
   Copyright (C) 1987-2016 Free Software Foundation, Inc.

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
#include "diagnostic-core.h"
#include "tm.h"
#include "common/common-target.h"
#include "common/common-target-def.h"
#include "opts.h"
#include "flags.h"

/* Implement TARGET_HANDLE_OPTION.  */

static bool
m68k_handle_option (struct gcc_options *opts,
		    struct gcc_options *opts_set,
		    const struct cl_decoded_option *decoded,
		    location_t loc)
{
  size_t code = decoded->opt_index;
  const char *arg = decoded->arg;
  int value = decoded->value;

  switch (code)
    {
    case OPT_m68020_40:
      opts->x_m68k_tune_option = u68020_40;
      opts_set->x_m68k_tune_option = (uarch_type) 1;
      opts->x_m68k_cpu_option = m68020;
      opts_set->x_m68k_cpu_option = (target_device) 1;
      return true;

    case OPT_m68020_60:
      opts->x_m68k_tune_option = u68020_60;
      opts_set->x_m68k_tune_option = (uarch_type) 1;
      opts->x_m68k_cpu_option = m68020;
      opts_set->x_m68k_cpu_option = (target_device) 1;
      return true;

    case OPT_mshared_library_id_:
      if (value > MAX_LIBRARY_ID)
	error_at (loc, "-mshared-library-id=%s is not between 0 and %d",
		  arg, MAX_LIBRARY_ID);
      else
        {
	  char *tmp;
	  asprintf (&tmp, "%d", (value * -4) - 4);
	  opts->x_m68k_library_id_string = tmp;
	}
      return true;

    default:
      return true;
    }
}

#undef TARGET_HANDLE_OPTION
#define TARGET_HANDLE_OPTION m68k_handle_option

struct gcc_targetm_common targetm_common = TARGETM_COMMON_INITIALIZER;
