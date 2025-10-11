/* Lowering routines for all things related to STRINGs.
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

/* Return a tree with the yielind of SKIP of a CHAR mode.  */

tree
a68_get_char_skip_tree (void)
{
  return build_int_cst (a68_char_type, ' ');
}

/* Return the maximum valid character code that can be stored in a CHAR.  */
tree
a68_char_max (void)
{
  /* 0x10FFFF is the maximum valid code point in Unicode.  */
  return build_int_cst (a68_char_type, 0x10FFFF);
}

/* Given an integral value, if it denotes a char code build the corresponding
   CHAR.  Otherwise raise a run-time error.  */

tree
a68_char_repr (NODE_T *p, tree val)
{
  /* UCS-4 (UTF-32) encodes the Unicode code points using the identity
     function.  Valid code points are in the ranges [U+0000,U+D7FF] and
     [U+E000,U+10FFFF].  */

  tree c = save_expr (val);
  tree val_type = TREE_TYPE (val);

  /* (c >= 0 && c < 0xd800) */
  tree range1 = fold_build2 (TRUTH_AND_EXPR, integer_type_node,
			     fold_build2 (GE_EXPR, integer_type_node,
					  c, fold_convert (val_type, integer_zero_node)),
			     fold_build2 (LT_EXPR, integer_type_node,
					  c, build_int_cst (val_type, 0xd800)));
  /* (c >= 0xe000 && c < 0x110000) */
  tree range2 = fold_build2 (TRUTH_AND_EXPR, integer_type_node,
			     fold_build2 (GE_EXPR, integer_type_node,
					  c, build_int_cst (val_type, 0xe000)),
			     fold_build2 (LT_EXPR, integer_type_node,
					  c, build_int_cst (val_type, 0x110000)));
  tree notvalid = fold_build1 (TRUTH_NOT_EXPR,
			       integer_type_node,
			       fold_build2 (TRUTH_OR_EXPR, integer_type_node,
					    range1, range2));

  /* Call to the runtime run-time error handler.  */
  unsigned int lineno = NUMBER (LINE (INFO (p)));
  const char *filename_str = FILENAME (LINE (INFO (p)));
  tree filename = build_string_literal (strlen (filename_str) + 1,
					    filename_str);
  tree call = a68_build_libcall (A68_LIBCALL_INVALIDCHARERROR,
				 void_type_node, 3,
				 filename,
				 build_int_cst (unsigned_type_node, lineno),
				 fold_convert (a68_int_type, c));

  /* Return the REPR of the given integer value, or raise run-time error.  */
  return fold_build2 (COMPOUND_EXPR, a68_char_type,
		      fold_build3 (COND_EXPR, integer_type_node,
				   notvalid,
				   call, integer_zero_node),
		      fold_convert (a68_char_type, c));
}

/* the ABS of a CHAR is an INT containing an unique value for each permissable
   char value.  */

tree
a68_char_abs (tree val)
{
  return fold_convert (a68_int_type, val);
}

/* Given two characters, build an expression that calculates whether A = B.  */

tree
a68_char_eq (tree a, tree b, location_t loc)
{
  return fold_build2_loc (loc, EQ_EXPR, boolean_type_node, a, b);
}

/* Given two characters, build an expression that calculates whether A /=
   B.  */

tree
a68_char_ne (tree a, tree b, location_t loc)
{
  return fold_build2_loc (loc, NE_EXPR, boolean_type_node, a, b);
}

/* Given two characters, build an expression that calculates
   whether A < B.  */

tree
a68_char_lt (tree a, tree b, location_t loc)
{
  return fold_build2_loc (loc, LT_EXPR, boolean_type_node, a, b);
}

/* Given two characters, build an expression that calculates
   whether A <= B.  */

tree
a68_char_le (tree a, tree b, location_t loc)
{
  return fold_build2_loc (loc, LE_EXPR, boolean_type_node, a, b);
}

/* Given two characters, build an expression that calculates
   whether A > B.  */

tree
a68_char_gt (tree a, tree b, location_t loc)
{
  return fold_build2_loc (loc, GT_EXPR, boolean_type_node, a, b);
}

/* Given two characters, build an expression that calculates
   whether A >= B.  */

tree
a68_char_ge (tree a, tree b, location_t loc)
{
  return fold_build2_loc (loc, GE_EXPR, boolean_type_node, a, b);
}
