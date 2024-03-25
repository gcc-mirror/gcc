/* Subroutines for the D front end on the LoongArch architecture.
   Copyright (C) 2023-2024 Free Software Foundation, Inc.

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
#include "tm_d.h"
#include "d/d-target.h"
#include "d/d-target-def.h"

/* Implement TARGET_D_CPU_VERSIONS for LoongArch targets.  */

void
loongarch_d_target_versions (void)
{
  if (TARGET_64BIT)
    d_add_builtin_version ("LoongArch64");
  else
    d_add_builtin_version ("LoongArch32");

  if (TARGET_HARD_FLOAT_ABI)
    {
      d_add_builtin_version ("LoongArch_HardFloat");
      d_add_builtin_version ("D_HardFloat");
    }
  else if (TARGET_SOFT_FLOAT_ABI)
    {
      d_add_builtin_version ("LoongArch_SoftFloat");
      d_add_builtin_version ("D_SoftFloat");
    }
}

/* Handle a call to `__traits(getTargetInfo, "floatAbi")'.  */

static tree
loongarch_d_handle_target_float_abi (void)
{
  const char *abi;

  if (TARGET_HARD_FLOAT_ABI)
    abi = "hard";
  else if (TARGET_SOFT_FLOAT_ABI)
    abi = "soft";
  else
    abi = "";

  return build_string_literal (strlen (abi) + 1, abi);
}

/* Implement TARGET_D_REGISTER_CPU_TARGET_INFO.  */

void
loongarch_d_register_target_info (void)
{
  const struct d_target_info_spec handlers[] = {
    {"floatAbi", loongarch_d_handle_target_float_abi},
    {NULL, NULL},
  };

  d_add_target_info_handlers (handlers);
}
