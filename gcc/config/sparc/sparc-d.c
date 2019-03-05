/* Subroutines for the D front end on the SPARC architecture.
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

/* Implement TARGET_D_CPU_VERSIONS for SPARC targets.  */

void
sparc_d_target_versions (void)
{
  if (TARGET_64BIT)
    d_add_builtin_version ("SPARC64");
  else
    d_add_builtin_version ("SPARC");

  if (TARGET_V8PLUS)
    d_add_builtin_version ("SPARC_V8Plus");

  if (TARGET_FPU)
    {
      d_add_builtin_version ("D_HardFloat");
      d_add_builtin_version ("SPARC_HardFloat");
    }
  else
    {
      d_add_builtin_version ("D_SoftFloat");
      d_add_builtin_version ("SPARC_SoftFloat");
    }
}
