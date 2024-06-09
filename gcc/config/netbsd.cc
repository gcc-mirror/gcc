/* Functions for generic NetBSD as target machine for GNU C compiler.
   Copyright (C) 2017-2024 Free Software Foundation, Inc.

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
#include "tm.h"
#include "tree.h"
#include "varasm.h"
#include "netbsd-protos.h"

static void
netbsd_patch_builtin (enum built_in_function fncode)
{
  tree fn = builtin_decl_explicit (fncode);
  tree sym;
  char *newname;

  if (!fn)
    return;

  sym = DECL_ASSEMBLER_NAME (fn);
  newname = ACONCAT (("__c99_", IDENTIFIER_POINTER (sym), NULL));

  set_user_assembler_name (fn, newname);

  fn = builtin_decl_implicit (fncode);
  if (fn)
    set_user_assembler_name (fn, newname);
}

void
netbsd_patch_builtins (void)
{
  netbsd_patch_builtin (BUILT_IN_CABSF);
  netbsd_patch_builtin (BUILT_IN_CABS);
  netbsd_patch_builtin (BUILT_IN_CABSL);
}
