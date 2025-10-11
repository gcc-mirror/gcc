/* Lowering routines for all things related to BOOL values.
   Copyright (C) 2025 Jose E. Marchesi.

   Written by Jose E. Marchesi.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"

#include "tree.h"
#include "fold-const.h"
#include "diagnostic.h"
#include "langhooks.h"
#include "tm.h"
#include "function.h"
#include "cgraph.h"
#include "toplev.h"
#include "varasm.h"
#include "predict.h"
#include "stor-layout.h"
#include "tree-iterator.h"
#include "stringpool.h"
#include "print-tree.h"
#include "gimplify.h"
#include "dumpfile.h"
#include "convert.h"

#include "a68.h"

/* Return a tree with the yielind of SKIP of a BOOL mode.  */

tree
a68_get_bool_skip_tree (void)
{
  return build_int_cst (a68_bool_type, 0);
}

/* The absolute value of a BOOL is a non-zero INT for TRUE and zero for
   FALSE.  */

tree
a68_bool_abs (tree val)
{
  return fold_convert (a68_int_type, val);
}

/* Given two boolean values, build an expression that calculates whether A = B.  */

tree
a68_bool_eq (tree a, tree b, location_t loc)
{
  return fold_build2_loc (loc, EQ_EXPR, boolean_type_node, a, b);
}

/* Given two boolean values, build an expression that calculates whether A /=
   B.  */

tree
a68_bool_ne (tree a, tree b, location_t loc)
{
  return fold_build2_loc (loc, NE_EXPR, boolean_type_node, a, b);
}
