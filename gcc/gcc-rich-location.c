/* Implementation of gcc_rich_location class
   Copyright (C) 2014-2016 Free Software Foundation, Inc.

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
#include "tm.h"
#include "rtl.h"
#include "hash-set.h"
#include "machmode.h"
#include "vec.h"
#include "double-int.h"
#include "input.h"
#include "alias.h"
#include "symtab.h"
#include "wide-int.h"
#include "inchash.h"
#include "tree-core.h"
#include "tree.h"
#include "diagnostic-core.h"
#include "gcc-rich-location.h"
#include "print-tree.h"
#include "pretty-print.h"
#include "intl.h"
#include "cpplib.h"
#include "diagnostic.h"

/* Add a range to the rich_location, covering expression EXPR. */

void
gcc_rich_location::add_expr (tree expr)
{
  gcc_assert (expr);

  if (CAN_HAVE_RANGE_P (expr))
    add_range (EXPR_LOCATION (expr), false);
}

/* If T is an expression, add a range for it to the rich_location.  */

void
gcc_rich_location::maybe_add_expr (tree t)
{
  if (EXPR_P (t))
    add_expr (t);
}

/* Add a fixit hint suggesting replacing the range at MISSPELLED_TOKEN_LOC
   with the identifier HINT_ID.  */

void
gcc_rich_location::add_fixit_misspelled_id (location_t misspelled_token_loc,
					    tree hint_id)
{
  gcc_assert (TREE_CODE (hint_id) == IDENTIFIER_NODE);

  source_range misspelled_token_range
    = get_range_from_loc (line_table, misspelled_token_loc);
  add_fixit_replace (misspelled_token_range, IDENTIFIER_POINTER (hint_id));
}
