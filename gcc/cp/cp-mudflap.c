/* Mudflap: narrow-pointer bounds-checking by tree rewriting:
   C++ front-end interface.

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
#include "cp-tree.h"
#include "c-common.h"
#include "diagnostic.h"
#include "output.h"
#include "varray.h"
#include "tree-mudflap.h"
#include "target.h"
#include "flags.h"
#include "rtl.h"
#include "toplev.h"


/* Initialize the global tree nodes that correspond to mf-runtime.h
   declarations.  */
tree
mflang_lookup_decl (const char* name)
{
  tree decl = lookup_name (get_identifier (name), 1);
  if (decl == NULL_TREE)
    internal_error ("mudflap: cannot find declaration of `%s' from mf-runtime.h",
		    name);

  return decl;
}


/* Emit a synthetic CTOR function for the current file.  Populate it from
   the enqueued __mf_register calls.  Register it with the constructors.  */

void
mflang_flush_calls (tree enqueued_call_stmt_chain)
{
  tree fnname, fndecl, body;

  /* Short-circuit!  */
  if (enqueued_call_stmt_chain == NULL_TREE)
    return;

  /* Create a ctor function declaration.  */
  fnname = get_identifier ("__static_initialization_and_destruction_mudflap");

  start_function (void_list_node,
		  make_call_declarator (fnname, void_list_node, NULL_TREE,
					NULL_TREE),
		  NULL_TREE, SF_DEFAULT);

  TREE_PUBLIC (current_function_decl) = 0;
  TREE_USED (current_function_decl) = 1;
  DECL_ARTIFICIAL (current_function_decl) = 1;
  mf_mark (current_function_decl);

  /* Generate the body, one statement at a time.  */
  body = begin_compound_stmt (BCS_FN_BODY);

  while (enqueued_call_stmt_chain)
    {
      tree next = TREE_CHAIN (enqueued_call_stmt_chain);
      finish_expr_stmt (enqueued_call_stmt_chain);
      enqueued_call_stmt_chain = next;
    }

  finish_compound_stmt (body);
  fndecl = finish_function (0);

  /* NB: We cannot call expand_or_defer_fn here, since that goes through
     the callgraph queue.  This queue will have already been processed by the
     time this function is running.  */
  expand_body (fndecl);
  if (targetm.have_ctors_dtors)
    (* targetm.asm_out.constructor) (XEXP (DECL_RTL (fndecl), 0),
                                     DEFAULT_INIT_PRIORITY);
  else
    /* By this time, it's too late to do this:
       static_ctors = tree_cons (NULL_TREE, fndecl, static_ctors); */
    abort ();
}
