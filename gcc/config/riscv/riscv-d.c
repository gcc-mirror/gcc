/* Subroutines for the D front end on the RISC-V architecture.
   Copyright (C) 2017-2020 Free Software Foundation, Inc.

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
#include "target.h"
#include "d/d-target.h"
#include "d/d-target-def.h"

/* Implement TARGET_D_CPU_VERSIONS for RISC-V targets.  */

void
riscv_d_target_versions (void)
{
  if (TARGET_64BIT)
    d_add_builtin_version ("RISCV64");
  else
    d_add_builtin_version ("RISCV32");

  if (TARGET_HARD_FLOAT)
    d_add_builtin_version ("D_HardFloat");
  else
    d_add_builtin_version ("D_SoftFloat");
}
