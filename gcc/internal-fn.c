/* Internal functions.
   Copyright (C) 2011 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "internal-fn.h"
#include "tree.h"
#include "expr.h"
#include "optabs.h"
#include "gimple.h"

/* The names of each internal function, indexed by function number.  */
const char *const internal_fn_name_array[] = {
#define DEF_INTERNAL_FN(CODE, FLAGS) #CODE,
#include "internal-fn.def"
#undef DEF_INTERNAL_FN
  "<invalid-fn>"
};

/* The ECF_* flags of each internal function, indexed by function number.  */
const int internal_fn_flags_array[] = {
#define DEF_INTERNAL_FN(CODE, FLAGS) FLAGS,
#include "internal-fn.def"
#undef DEF_INTERNAL_FN
  0
};

/* Routines to expand each internal function, indexed by function number.
   Each routine has the prototype:

       expand_<NAME> (gimple stmt)

   where STMT is the statement that performs the call. */
static void (*const internal_fn_expanders[]) (gimple) = {
#define DEF_INTERNAL_FN(CODE, FLAGS) expand_##CODE,
#include "internal-fn.def"
#undef DEF_INTERNAL_FN
  0
};

/* Expand STMT, which is a call to internal function FN.  */

void
expand_internal_call (gimple stmt)
{
  internal_fn_expanders[(int) gimple_call_internal_fn (stmt)] (stmt);
}
