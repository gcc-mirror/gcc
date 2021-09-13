/* Tree-dumping functionality for C-family languages.
   Copyright (C) 2002-2021 Free Software Foundation, Inc.
   Written by Mark Mitchell <mark@codesourcery.com>

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
#include "c-common.h"
#include "tree-dump.h"

/* Dump any C-specific tree codes and attributes of common codes.  */

static void
dump_stmt (dump_info_p di, const_tree t)
{
  if (EXPR_HAS_LOCATION (t))
    dump_int (di, "line", EXPR_LINENO (t));
}

bool
c_dump_tree (void *dump_info, tree t)
{
  enum tree_code code;
  dump_info_p di = (dump_info_p) dump_info;

  /* Figure out what kind of node this is.  */
  code = TREE_CODE (t);

  switch (code)
    {
    case FIELD_DECL:
      if (DECL_C_BIT_FIELD (t))
	dump_string (di, "bitfield");
      break;

    case BREAK_STMT:
    case CONTINUE_STMT:
      dump_stmt (di, t);
      break;

    case DO_STMT:
      dump_stmt (di, t);
      dump_child ("body", DO_BODY (t));
      dump_child ("cond", DO_COND (t));
      break;

    case FOR_STMT:
      dump_stmt (di, t);
      dump_child ("init", FOR_INIT_STMT (t));
      dump_child ("cond", FOR_COND (t));
      dump_child ("expr", FOR_EXPR (t));
      dump_child ("body", FOR_BODY (t));
      break;

    case SWITCH_STMT:
      dump_stmt (di, t);
      dump_child ("cond", SWITCH_STMT_COND (t));
      dump_child ("body", SWITCH_STMT_BODY (t));
      break;

    case WHILE_STMT:
      dump_stmt (di, t);
      dump_child ("cond", WHILE_COND (t));
      dump_child ("body", WHILE_BODY (t));
      break;

    default:
      break;
    }

  return false;
}
