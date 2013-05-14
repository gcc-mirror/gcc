/* Functions for Linux Android as target machine for GNU C compiler.
   Copyright (C) 2013.
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
#include "tm.h"
#include "linux-protos.h"

/* Android does not support GNU indirect functions.  */

bool
linux_android_has_ifunc_p (void)
{
  return TARGET_ANDROID ? false : HAVE_GNU_INDIRECT_FUNCTION;
}
