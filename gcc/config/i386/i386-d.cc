/* Subroutines for the D front end on the x86 architecture.
   Copyright (C) 2017-2024 Free Software Foundation, Inc.

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

/* Implement TARGET_D_CPU_VERSIONS for x86 targets.  */

void
ix86_d_target_versions (void)
{
  if (TARGET_64BIT)
    {
      d_add_builtin_version ("X86_64");

      if (TARGET_X32)
	d_add_builtin_version ("D_X32");
    }
  else
    d_add_builtin_version ("X86");

  if (TARGET_80387)
    d_add_builtin_version ("D_HardFloat");
  else
    d_add_builtin_version ("D_SoftFloat");
}

/* Handle a call to `__traits(getTargetInfo, "floatAbi")'.  */

static tree
ix86_d_handle_target_float_abi (void)
{
  const char *abi;

  if (! (TARGET_80387 || TARGET_FLOAT_RETURNS_IN_80387))
    abi = "soft";
  else
    abi = "hard";

  return build_string_literal (strlen (abi) + 1, abi);
}

/* Handle a call to `__traits(getTargetInfo, "objectFormat")'.  */

static tree
ix86_d_handle_target_object_format (void)
{
  const char *objfmt = NULL;

  if (TARGET_MACHO)
    objfmt = "macho";
  else if (TARGET_COFF || TARGET_PECOFF)
    objfmt = "coff";

  if (objfmt == NULL)
    return NULL_TREE;

  return build_string_literal (strlen (objfmt) + 1, objfmt);
}

/* Implement TARGET_D_REGISTER_CPU_TARGET_INFO.  */

void
ix86_d_register_target_info (void)
{
  const struct d_target_info_spec handlers[] = {
    { "floatAbi", ix86_d_handle_target_float_abi },
    { "objectFormat", ix86_d_handle_target_object_format },
    { NULL, NULL },
  };

  d_add_target_info_handlers (handlers);
}

/* Implement TARGET_D_HAS_STDCALL_CONVENTION for x86 targets.  */

bool
ix86_d_has_stdcall_convention (unsigned int *link_system,
			       unsigned int *link_windows)
{
  if (ix86_abi == MS_ABI)
    {
      *link_system = 1;
      *link_windows = (!TARGET_64BIT) ? 1 : 0;
    }
  else
    {
      *link_system = 0;
      *link_windows = 0;
    }

  return true;
}
