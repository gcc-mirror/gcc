/* Default common target hook functions.
   Copyright (C) 2003-2018 Free Software Foundation, Inc.

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
#include "common/common-targhooks.h"
#include "opts.h"

/* Determine the exception handling mechanism for the target.  */

enum unwind_info_type
default_except_unwind_info (struct gcc_options *opts ATTRIBUTE_UNUSED)
{
  /* Obey the configure switch to turn on sjlj exceptions.  */
#ifdef CONFIG_SJLJ_EXCEPTIONS
  if (CONFIG_SJLJ_EXCEPTIONS)
    return UI_SJLJ;
#endif

  /* ??? Change all users to the hook, then poison this.  */
#ifdef DWARF2_UNWIND_INFO
  if (DWARF2_UNWIND_INFO)
    return UI_DWARF2;
#endif

  return UI_SJLJ;
}

/* To be used by targets that force dwarf2 unwind enabled.  */

enum unwind_info_type
dwarf2_except_unwind_info (struct gcc_options *opts ATTRIBUTE_UNUSED)
{
  /* Obey the configure switch to turn on sjlj exceptions.  */
#ifdef CONFIG_SJLJ_EXCEPTIONS
  if (CONFIG_SJLJ_EXCEPTIONS)
    return UI_SJLJ;
#endif

  return UI_DWARF2;
}

/* To be used by targets that force sjlj unwind enabled.  */

enum unwind_info_type
sjlj_except_unwind_info (struct gcc_options *opts ATTRIBUTE_UNUSED)
{
  return UI_SJLJ;
}

/* Default version of TARGET_HANDLE_OPTION.  */

bool
default_target_handle_option (struct gcc_options *opts ATTRIBUTE_UNUSED,
			      struct gcc_options *opts_set ATTRIBUTE_UNUSED,
			      const struct cl_decoded_option *decoded ATTRIBUTE_UNUSED,
			      location_t loc ATTRIBUTE_UNUSED)
{
  return true;
}

/* Default version of TARGET_GET_VALID_OPTION_VALUES.  */

vec<const char *>
default_get_valid_option_values (int, const char *)
{
  return vec<const char *> ();
}

const struct default_options empty_optimization_table[] =
  {
    { OPT_LEVELS_NONE, 0, NULL, 0 }
  };
