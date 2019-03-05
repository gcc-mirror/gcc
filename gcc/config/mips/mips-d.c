/* Subroutines for the D front end on the MIPS architecture.
   Copyright (C) 2017-2019 Free Software Foundation, Inc.

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
#include "d/d-target.h"
#include "d/d-target-def.h"

/* Implement TARGET_D_CPU_VERSIONS for MIPS targets.  */

void
mips_d_target_versions (void)
{
  if (TARGET_64BIT)
    d_add_builtin_version ("MIPS64");
  else
    d_add_builtin_version ("MIPS32");

  if (mips_abi == ABI_32)
    d_add_builtin_version ("MIPS_O32");
  else if (mips_abi == ABI_EABI)
    d_add_builtin_version ("MIPS_EABI");
  else if (mips_abi == ABI_N32)
    d_add_builtin_version ("MIPS_N32");
  else if (mips_abi == ABI_64)
    d_add_builtin_version ("MIPS_N64");
  else if (mips_abi == ABI_O64)
    d_add_builtin_version ("MIPS_O64");

  if (TARGET_HARD_FLOAT_ABI)
    {
      d_add_builtin_version ("MIPS_HardFloat");
      d_add_builtin_version ("D_HardFloat");
    }
  else if (TARGET_SOFT_FLOAT_ABI)
    {
      d_add_builtin_version ("MIPS_SoftFloat");
      d_add_builtin_version ("D_SoftFloat");
    }
}
