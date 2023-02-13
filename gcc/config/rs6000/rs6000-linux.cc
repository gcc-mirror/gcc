/* Functions for Linux on PowerPC.
   Copyright (C) 2013-2023 Free Software Foundation, Inc.

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

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"

/* Implement TARGET_FLOAT_EXCEPTIONS_ROUNDING_SUPPORTED_P.  */

bool
rs6000_linux_float_exceptions_rounding_supported_p (void)
{
  /* glibc has support for exceptions and rounding modes for software
     floating point.  */
  if (OPTION_GLIBC)
    return true;
  else
    return TARGET_HARD_FLOAT;
}
