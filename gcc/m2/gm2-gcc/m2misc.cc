/* m2misc.cc miscellaneous tree debugging functions.

Copyright (C) 2012-2024 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius@glam.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "gcc-consolidation.h"

#include "../m2-tree.h"
#include "tree-iterator.h"

#define m2misc_c
#include "m2block.h"
#include "m2misc.h"
#include "m2tree.h"

/* C error entry to error.  */

void
m2misc_cerror (const char *message)
{
  error (message);
}

/* modula2 entry for cerror.  */

void
m2misc_error (const char *message)
{
  m2misc_cerror (message);
}

/* DebugTree - display the tree t.  */

void
m2misc_DebugTree (tree t)
{
  debug_tree (t);
}

/* DebugTree - display the trees chained in t.  */

void
m2misc_DebugTreeChain (tree t)
{
  for (; (t != NULL); t = TREE_CHAIN (t))
    debug_tree (t);
}

/* DebugTree - display the current statement list.  */

void
m2misc_printStmt (void)
{
  if (m2block_cur_stmt_list () != NULL)
    debug_tree (m2block_cur_stmt_list ());
}
