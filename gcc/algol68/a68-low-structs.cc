/* Lowering routines for all things related to structs.
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

/* Return a tree with the yielding of SKIP for the given structured mode.  */

tree
a68_get_struct_skip_tree (MOID_T *m)
{
  /* Build a constructor that assigns SKIPs to each field in the struct
     type.  */

  vec <constructor_elt, va_gc> *ve = NULL;
  tree field = TYPE_FIELDS (CTYPE (m));
  for (PACK_T *elem = PACK (m); elem; FORWARD (elem))
    {
      gcc_assert (field != NULL_TREE);
      CONSTRUCTOR_APPEND_ELT (ve, field, a68_get_skip_tree (MOID (elem)));
      field = DECL_CHAIN (field);
    }

  return build_constructor (CTYPE (m), ve);
}
