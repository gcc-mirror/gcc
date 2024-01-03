/* Subroutines for the D front end on the HPPA architecture.
   Copyright (C) 2018-2024 Free Software Foundation, Inc.

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

/* Implement TARGET_D_CPU_VERSIONS for HPPA targets.  */

void
pa_d_target_versions (void)
{
  if (TARGET_64BIT)
    d_add_builtin_version ("HPPA64");
  else
    d_add_builtin_version ("HPPA");

  if (TARGET_SOFT_FLOAT)
    d_add_builtin_version ("D_SoftFloat");
  else
    d_add_builtin_version ("D_HardFloat");
}

/* Handle a call to `__traits(getTargetInfo, "floatAbi")'.  */

static tree
pa_d_handle_target_float_abi (void)
{
  const char *abi;

  if (TARGET_SOFT_FLOAT)
    abi = "soft";
  else
    abi = "hard";

  return build_string_literal (strlen (abi) + 1, abi);
}

/* Handle a call to `__traits(getTargetInfo, "objectFormat")'.  */

static tree
pa_d_handle_target_object_format (void)
{
  const char *objfmt = NULL;

  if (TARGET_SOM)
    objfmt = "som";

  if (objfmt == NULL)
    return NULL_TREE;

  return build_string_literal (strlen (objfmt) + 1, objfmt);
}

/* Implement TARGET_D_REGISTER_CPU_TARGET_INFO.  */

void
pa_d_register_target_info (void)
{
  const struct d_target_info_spec handlers[] = {
    { "floatAbi", pa_d_handle_target_float_abi },
    { "objectFormat", pa_d_handle_target_object_format },
    { NULL, NULL },
  };

  d_add_target_info_handlers (handlers);
}
