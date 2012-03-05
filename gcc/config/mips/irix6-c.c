/* IRIX 6 support needed only by C/C++ frontends.
   Copyright (C) 2012 Free Software Foundation, Inc.

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
#include "tree.h"
#include "tm.h"
#include "c-family/c-common.h"

/* For C99, intmax_t, uintmax_t are always long long int, otherwise the
   type differs between 32-bit and 64-bit compilations.  */
void
irix6_c_common_override_options (void)
{
  if (flag_isoc99 || c_dialect_cxx ())
    long_intmax = 0;
  else
    /* Cannot use LONG_TYPE_SIZE == 64.  LONG_TYPE_SIZE is only set in
       mips_option_override after C_COMMON_OVERRIDE_OPTIONS.  */
    long_intmax = mips_abi == ABI_64;
}
