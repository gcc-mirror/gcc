/* Functions for Linux Android as target machine for GNU C compiler.
   Copyright (C) 2013-2016 Free Software Foundation, Inc.

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

#undef TARGET_PRINTF_POINTER_FORMAT
#define TARGET_PRINTF_POINTER_FORMAT gnu_libc_printf_pointer_format

bool
linux_libc_has_function (enum function_class fn_class)
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

/* Glibc formats pointers as if by "%zx" except for the null pointer
   which outputs "(nil)".  It ignores the pound ('#') format flag but
   interprets the space and plus flags the same as in the integer
   directive.  */

const char*
gnu_libc_printf_pointer_format (tree arg, const char **flags)
{
  *flags = " +";

  return arg && integer_zerop (arg) ? "(nil)" : "%#zx";
}
