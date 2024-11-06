/* C++ specific Solaris system support.
   Copyright (C) 2011-2024 Free Software Foundation, Inc.

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

#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "cp/cp-tree.h"
#include "stringpool.h"

/* Before GCC 4.7, g++ defined __cplusplus 1 to avoid coping with the C++98
   overloads in Solaris system headers.  Since this was fixed, 4 structure
   types would move to namespace std, breaking the Solaris libstdc++ ABI.
   To avoid this, we forcefully keep those types in the global namespace.
   This can be removed once the next major version of libstdc++ is
   released.  */

/* Cache the identifiers of the affected types to speed up lookup.  */
#define NUM_FGID 4
static GTY(()) tree force_global_identifiers[NUM_FGID];

/* Check if DECL is one of the affected types and move it to the global
   namespace if so.  */
tree
solaris_cxx_decl_mangling_context (const_tree decl)
{
  static bool init = false;
  int i = 0;

  if (!init)
    {
      force_global_identifiers[i++] = get_identifier ("div_t");
      force_global_identifiers[i++] = get_identifier ("ldiv_t");
      force_global_identifiers[i++] = get_identifier ("lconv");
      force_global_identifiers[i++] = get_identifier ("tm");
      init = true;
    }

  if (!(DECL_P (decl) && DECL_NAMESPACE_STD_P (CP_DECL_CONTEXT (decl))))
    return NULL_TREE;

  for (i = 0; i < NUM_FGID; i++)
    if (DECL_NAME (decl) == force_global_identifiers[i])
	return global_namespace;

  return NULL_TREE;
}
