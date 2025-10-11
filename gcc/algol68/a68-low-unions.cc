/* Lowering routines for all things related to unions.
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

/* Algol 68 unions are implemented in this front-end as a data structure
   consisting of an overhead followed by a value:

     overhead%
     value%

   Where overhead% is an index that identifies the kind of object currently
   united, and value% is a GENERIC union.  The value currently united in the
   union is the overhead%-th field in value%.

   At the language level there are no values of union modes in Algol 68.  All
   values are built from either SKIP (for uninitialized UNION values) or as the
   result of an uniting coercion.  */

/* Given an union mode P and a mode Q, return whether Q is a mode in P.  */

bool
a68_union_contains_mode (MOID_T *p, MOID_T *q)
{
  while (EQUIVALENT (p) != NO_MOID)
    p = EQUIVALENT (p);

  for (PACK_T *pack = PACK (p); pack != NO_PACK; FORWARD (pack))
    {
      MOID_T *m = MOID (pack);

      if (a68_is_equal_modes (q, m, SAFE_DEFLEXING)
	  || (m == M_STRING && IS_ROW (q) && SUB (q) == M_CHAR)
	  || (q == M_STRING && IS_ROW (m) && SUB (m) == M_CHAR))
	return true;
    }

  return false;
}

/* Given an union mode P and a mode Q, return an integer with the index of the
   occurrence of Q in P.  */

int
a68_united_mode_index (MOID_T *p, MOID_T *q)
{
  int ret = 0;
  while (EQUIVALENT (p) != NO_MOID)
    p = EQUIVALENT (p);
  for (PACK_T *pack = PACK (p); pack != NO_PACK; FORWARD (pack))
    {
      MOID_T *m = MOID (pack);

      if (a68_is_equal_modes (q, m, SAFE_DEFLEXING)
	  || (m == M_STRING && IS_ROW (q) && SUB (q) == M_CHAR)
	  || (q == M_STRING && IS_ROW (m) && SUB (m) == M_CHAR))
	return ret;
      ret += 1;
    }

  /* Not found.  Shouldn't happen.  */
  gcc_unreachable ();
  return 0;
}

/* Given two united modes FROM and TO, and an overhead FROM_OVERHEAD in mode
   FROM, return the corresponding overhead in mode TO.

   This function assumes that the mode with FROM_OVERHEAD in mode FROM exists
   in TO.  */

tree
a68_union_translate_overhead (MOID_T *from, tree from_overhead,
			      MOID_T *to)
{
  /* Note that the initialization value for to_overhead should never be used.
     XXX perhaps translate it to a run-time call to abort/compiler-error.  */
  tree to_overhead = size_int (0);

  from_overhead = save_expr (from_overhead);

  int i = 0;
  for (PACK_T *pack = PACK (from); pack != NO_PACK; FORWARD (pack), ++i)
    {
      MOID_T *mode = MOID (pack);

      if (a68_union_contains_mode (to, mode))
	{
	  to_overhead = fold_build3 (COND_EXPR, sizetype,
				     fold_build2 (EQ_EXPR, boolean_type_node,
						  from_overhead,
						  size_int (i)),
				     size_int (a68_united_mode_index (to, mode)),
				     to_overhead);
	}
    }

  return to_overhead;
}

/* Get the overhead of a given united value EXP.  */

tree
a68_union_overhead (tree exp)
{
  tree type = TREE_TYPE (exp);
  tree overhead_field = TYPE_FIELDS (type);
  return fold_build3 (COMPONENT_REF,
		      TREE_TYPE (overhead_field),
		      exp,
		      overhead_field,
		      NULL_TREE);
}

/* Set the overhead of a given united value EXP to OVERHEAD.  */

tree
a68_union_set_overhead (tree exp, tree overhead)
{
  tree type = TREE_TYPE (exp);
  tree overhead_field = TYPE_FIELDS (type);
  return fold_build2 (MODIFY_EXPR,
		      TREE_TYPE (overhead),
		      fold_build3 (COMPONENT_REF,
				   TREE_TYPE (overhead_field),
				   exp,
				   overhead_field,
				   NULL_TREE),
		      overhead);
}

/* Get the cunion in the given union EXP.  */

tree
a68_union_cunion (tree exp)
{
  tree type = TREE_TYPE (exp);
  tree value_field = TREE_CHAIN (TYPE_FIELDS (type));
  return fold_build3 (COMPONENT_REF,
		      TREE_TYPE (value_field),
		      exp,
		      value_field,
		      NULL_TREE);
}

/* Build a SKIP value for a given union mode M.

   The SKIP value computed is:

   overhead% refers to the first united mode in the union
   value% is the SKIP for the first united mode in the union
*/

tree
a68_get_union_skip_tree (MOID_T *m)
{
  tree type = CTYPE (m);
  tree overhead_field = TYPE_FIELDS (type);
  tree value_field = TREE_CHAIN (TYPE_FIELDS (type));

  /* Overhead selects the first union alternative.  */
  tree overhead = size_zero_node;
  /* First union alternative.

     Note that the first union alternative corresponds to the last alternative
     in the mode as written in the source program.  */
  tree value_type = TREE_TYPE (value_field);
  tree first_alternative_field = TYPE_FIELDS (value_type);
  tree value = build_constructor_va (TREE_TYPE (value_field),
				     1,
				     first_alternative_field,
				     a68_get_skip_tree (MOID (PACK (m))));
  return build_constructor_va (CTYPE (m),
			       2,
			       overhead_field, overhead,
			       value_field, value);
}

/* Return the alternative (value) at the index INDEX in the united value
   EXP.  */

tree
a68_union_alternative (tree exp, int index)
{
  tree type = TREE_TYPE (exp);
  tree value_field = TREE_CHAIN (TYPE_FIELDS (type));
  tree value = fold_build3 (COMPONENT_REF,
			    TREE_TYPE (value_field),
			    exp,
			    value_field,
			    NULL_TREE);

  /* Get the current alternative in the value union.  */
  tree value_type = TREE_TYPE (value_field);
  tree alternative_field = TYPE_FIELDS (value_type);
  for (int i = 0; i < index; ++i)
    {
      gcc_assert (TREE_CHAIN (alternative_field));
      alternative_field = TREE_CHAIN (alternative_field);
    }

  /* Get the current alternative from the value.  */
  return fold_build3 (COMPONENT_REF,
		      TREE_TYPE (alternative_field),
		      value,
		      alternative_field,
		      NULL_TREE);
}

/* Return a constructor for an union of mode MODE, holding the value in EXP
   which is of mode EXP_MODE.  */

tree
a68_union_value (MOID_T *mode, tree exp, MOID_T *exp_mode)
{
  tree type = CTYPE (mode);
  tree overhead_field = TYPE_FIELDS (type);
  tree value_field = TREE_CHAIN (TYPE_FIELDS (type));

  int alternative_index = a68_united_mode_index (mode, exp_mode);
  tree overhead = build_int_cst (sizetype, alternative_index);

  /* Get the field for the alternative corresponding to alternative_index.  */
  tree value_type = TREE_TYPE (value_field);
  tree alternative_field = TYPE_FIELDS (value_type);
  for (int i = 0; i < alternative_index; ++i)
    {
      gcc_assert (TREE_CHAIN (alternative_field));
      alternative_field = TREE_CHAIN (alternative_field);
    }

  tree value = build_constructor_va (TREE_TYPE (value_field),
				     1,
				     alternative_field,
				     a68_consolidate_ref (exp_mode, exp));
  return build_constructor_va (type,
			       2,
			       overhead_field, overhead,
			       value_field, value);
}
