/* Lowering routines for all things related to COMPL values.
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

/* Build a new COMPL value with real part RE and imaginary part IM, of mode
   MODE.  */

tree
a68_complex_i (MOID_T *mode, tree re, tree im)
{
  tree compl_type = CTYPE (mode);

  tree re_field = TYPE_FIELDS (compl_type);
  tree im_field = TREE_CHAIN (re_field);
  return build_constructor_va (CTYPE (mode), 2,
			       re_field, re,
			       im_field, im);
}

/* Given a COMPL value Z, get its real part.  */

tree
a68_complex_re (tree z)
{
  tree re_field = TYPE_FIELDS (TREE_TYPE (z));
  return fold_build3 (COMPONENT_REF, TREE_TYPE (re_field),
		      z, re_field, NULL_TREE);
}

tree
a68_complex_im (tree z)
{
  tree im_field = TREE_CHAIN (TYPE_FIELDS (TREE_TYPE (z)));
  return fold_build3 (COMPONENT_REF, TREE_TYPE (im_field),
		      z, im_field, NULL_TREE);
}

/* Return the conjugate of the given complex Z of mode MODE.  */

tree
a68_complex_conj (MOID_T *mode, tree z)
{
  tree re_field = TYPE_FIELDS (TREE_TYPE (z));
  tree complex_type = build_complex_type (TREE_TYPE (re_field), false /* named */);

  z = save_expr (z);
  tree complex = fold_build2 (COMPLEX_EXPR, complex_type,
			      a68_complex_re (z), a68_complex_im (z));
  tree conj = fold_build1 (CONJ_EXPR, TREE_TYPE (complex), complex);

  return a68_complex_i (mode,
			fold_build1 (REALPART_EXPR, TREE_TYPE (TREE_TYPE (z)), conj),
			fold_build1 (IMAGPART_EXPR, TREE_TYPE (TREE_TYPE (z)), conj));
}

/* Widen a real R to a complex of mode MODE.  */

tree
a68_complex_widen_from_real (MOID_T *mode, tree r)
{
  tree compl_type = CTYPE (mode);
  gcc_assert (compl_type != NULL_TREE);

  /* Sanity check.  */
  if (mode == M_COMPLEX)
    gcc_assert (TREE_TYPE (r) == a68_real_type);
  else if (mode == M_LONG_COMPLEX)
    gcc_assert (TREE_TYPE (r) == a68_long_real_type);
  else if (mode == M_LONG_LONG_COMPLEX)
    gcc_assert (TREE_TYPE (r) == a68_long_long_real_type);
  else
    gcc_unreachable ();

  a68_push_range (mode);
  tree res = a68_lower_tmpvar ("compl%", compl_type,
			       a68_get_skip_tree (mode));

  /* Look for the "re" field.  */
  tree field_id = a68_get_mangled_identifier ("re");
  tree field = NULL_TREE;
  for (tree f = TYPE_FIELDS (compl_type); f; f = DECL_CHAIN (f))
    {
      if (field_id == DECL_NAME (f))
	{
	  field = f;
	  break;
	}
    }
  gcc_assert (field != NULL_TREE);

  /* Set it to the given real value.  */
  a68_add_stmt (fold_build2 (MODIFY_EXPR,
			     TREE_TYPE (r),
			     fold_build3 (COMPONENT_REF,
					  TREE_TYPE (field),
					  res, field,
					  NULL_TREE),
			     r));
  a68_add_stmt (res);
  return a68_pop_range ();
}
