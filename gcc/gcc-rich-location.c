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

/* Extract any source range information from EXPR and write it
   to *R.  */

static bool
get_range_for_expr (tree expr, location_range *r)
{
  if (EXPR_HAS_RANGE (expr))
    {
      source_range sr = EXPR_LOCATION_RANGE (expr);

      /* Do we have meaningful data?  */
      if (sr.m_start && sr.m_finish)
	{
	  r->m_start = expand_location (sr.m_start);
	  r->m_finish = expand_location (sr.m_finish);
	  return true;
	}
    }

  return false;
}

/* Add a range to the rich_location, covering expression EXPR. */

void
gcc_rich_location::add_expr (tree expr)
{
  gcc_assert (expr);

  location_range r;
  r.m_show_caret_p = false;
  if (get_range_for_expr (expr, &r))
    add_range (&r);
}

/* If T is an expression, add a range for it to the rich_location.  */

void
gcc_rich_location::maybe_add_expr (tree t)
{
  if (EXPR_P (t))
    add_expr (t);
}
