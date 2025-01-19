/* Functions for Linux Android as target machine for GNU C compiler.
   Copyright (C) 2013-2025 Free Software Foundation, Inc.

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
#include "tm.h"
#include "tree.h"
#include "linux-protos.h"
#include "target.h"
#include "targhooks.h"

bool
linux_libc_has_function (enum function_class fn_class,
			 tree type ATTRIBUTE_UNUSED)
{
  if (OPTION_GLIBC || OPTION_MUSL)
    return true;
  if (OPTION_BIONIC)
    if (fn_class == function_c94
	|| fn_class == function_c99_misc
	|| fn_class == function_sincos)
	return true;

  return false;
}

unsigned
linux_libm_function_max_error (unsigned cfn, machine_mode mode,
			       bool boundary_p)
{
  if (OPTION_GLIBC)
    return glibc_linux_libm_function_max_error (cfn, mode, boundary_p);
  return default_libm_function_max_error (cfn, mode, boundary_p);
}

unsigned
linux_fortify_source_default_level ()
{
  if (OPTION_GLIBC && TARGET_GLIBC_MAJOR == 2 && TARGET_GLIBC_MINOR >= 35)
    return 3;

  return 2;
}
