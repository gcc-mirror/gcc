/* Mudflap: narrow-pointer bounds-checking by tree rewriting:
   C front-end interface.

   Copyright (C) 2002, 2003, 2004 Free Software Foundation, Inc.
   Contributed by Frank Ch. Eigler <fche@redhat.com>
   and Graydon Hoare <graydon@redhat.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */


#include "config.h"
#include "errors.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "tree-inline.h"
#include "c-tree.h"
#include "c-common.h"
#include "diagnostic.h"
#include "output.h"
#include "varray.h"
#include "tree-mudflap.h"
#include "target.h"
#include "flags.h"
#include "rtl.h"
#include "toplev.h"
#include "function.h"



/* ------------------------------------------------------------------------ */


/* Initialize the global tree nodes that correspond to mf-runtime.h
   declarations.  */
tree
mflang_lookup_decl (const char* name)
{
  tree decl = lookup_name (get_identifier (name));
  if (decl == NULL_TREE)
    internal_error ("mudflap: cannot find declaration of `%s' from mf-runtime.h",
		    name);

  return decl;
}


/* Emit a synthetic CTOR function for the current file.  Populate it from
   the enqueued __mf_register calls.  Compile the function.  */

void
mflang_flush_calls (tree enqueued_call_stmt_chain)
{
  tree fnname, t1, t2, cs;

  /* Short-circuit!  */
  if (enqueued_call_stmt_chain == NULL_TREE)
    return;

  fnname = get_identifier ("__mudflap_static_initializer");
  t1 = build_tree_list (NULL_TREE, void_type_node);
  t2 = tree_cons (NULL, NULL, t1);
  start_function (t1, build_nt (CALL_EXPR, fnname, t2, NULL), NULL);
  store_parm_decls ();

  DECL_STATIC_CONSTRUCTOR (current_function_decl) = 1;
  TREE_PUBLIC (current_function_decl) = 0;
  TREE_USED (current_function_decl) = 1;
  mf_mark (current_function_decl);

  cs = c_begin_compound_stmt (true);
  c_expand_expr_stmt (enqueued_call_stmt_chain);
  add_stmt (c_end_compound_stmt (cs, true));

  finish_function ();
}
