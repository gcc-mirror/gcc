/* Subroutines for the D front end on the RISC-V architecture.
   Copyright (C) 2017-2023 Free Software Foundation, Inc.

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
#include "target.h"
#include "tm_d.h"
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

/* Handle a call to `__traits(getTargetInfo, "floatAbi")'.  */

static tree
riscv_d_handle_target_float_abi (void)
{
  const char *abi;

  switch (riscv_abi)
    {
    case ABI_ILP32E:
    case ABI_ILP32:
    case ABI_LP64:
      abi = "soft";
      break;

    case ABI_ILP32F:
    case ABI_LP64F:
      abi = "single";
      break;

    case ABI_ILP32D:
    case ABI_LP64D:
      abi = "double";
      break;

    default:
      abi = "";
      break;
    }

  return build_string_literal (strlen (abi) + 1, abi);
}

/* Implement TARGET_D_REGISTER_CPU_TARGET_INFO.  */

void
riscv_d_register_target_info (void)
{
  const struct d_target_info_spec handlers[] = {
    { "floatAbi", riscv_d_handle_target_float_abi },
    { NULL, NULL },
  };

  d_add_target_info_handlers (handlers);
}
