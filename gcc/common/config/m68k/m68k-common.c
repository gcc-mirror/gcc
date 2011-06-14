/* Common hooks for Motorola 68000 family.
   Copyright (C) 1987, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000,
   2001, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011
   Free Software Foundation, Inc.

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
		    struct gcc_options *opts_set ATTRIBUTE_UNUSED,
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
      opts->x_m68k_cpu_option = m68020;
      return true;

    case OPT_m68020_60:
      opts->x_m68k_tune_option = u68020_60;
      opts->x_m68k_cpu_option = m68020;
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
