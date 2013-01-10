/* Stubs for C++ specific Solaris system support.
   Copyright (C) 2011-2013 Free Software Foundation, Inc.

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
#include "tree.h"
#include "tm.h"
#include "tm_p.h"

/* Stub implemenation of TARGET_CXX_DECL_MANGLING_CONTEXT for non-C++
   frontends.  */
tree
solaris_cxx_decl_mangling_context (const_tree decl ATTRIBUTE_UNUSED)
{
  return NULL_TREE;
}
