/* Subroutines for the D front end on the ARM architecture.
   Copyright (C) 2017-2025 Free Software Foundation, Inc.

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
#include "tm_d.h"
#include "d/d-target.h"
#include "d/d-target-def.h"
#include "arm-protos.h"

/* Implement TARGET_D_CPU_VERSIONS for ARM targets.  */

void
arm_d_target_versions (void)
{
  d_add_builtin_version ("ARM");

  if (TARGET_THUMB || TARGET_THUMB2)
    {
      d_add_builtin_version ("Thumb");
      d_add_builtin_version ("ARM_Thumb");
    }

  if (TARGET_HARD_FLOAT_ABI)
    d_add_builtin_version ("ARM_HardFloat");
  else
    {
      if (TARGET_SOFT_FLOAT)
	d_add_builtin_version ("ARM_SoftFloat");
      else if (TARGET_HARD_FLOAT)
	d_add_builtin_version ("ARM_SoftFP");
    }

  if (TARGET_SOFT_FLOAT)
    d_add_builtin_version ("D_SoftFloat");
  else if (TARGET_HARD_FLOAT)
    d_add_builtin_version ("D_HardFloat");
}

/* Handle a call to `__traits(getTargetInfo, "floatAbi")'.  */

static tree
arm_d_handle_target_float_abi (void)
{
  const char *abi;

  switch (arm_float_abi)
    {
    case ARM_FLOAT_ABI_HARD:
      abi = "hard";
      break;

    case ARM_FLOAT_ABI_SOFT:
      abi = "soft";
      break;

    case ARM_FLOAT_ABI_SOFTFP:
      abi = "softfp";
      break;

    default:
      abi = "";
      break;
    }

  return build_string_literal (strlen (abi) + 1, abi);
}

/* Implement TARGET_D_REGISTER_CPU_TARGET_INFO.  */

void
arm_d_register_target_info (void)
{
  const struct d_target_info_spec handlers[] = {
    { "floatAbi", arm_d_handle_target_float_abi },
    { NULL, NULL },
  };

  d_add_target_info_handlers (handlers);
}
