/* Default language-specific hooks.
   Copyright 2001 Free Software Foundation, Inc.
   Contributed by Alexandre Oliva  <aoliva@redhat.com>

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "toplev.h"
#include "tree.h"
#include "tree-inline.h"
#include "rtl.h"
#include "insn-config.h"
#include "integrate.h"
#include "langhooks.h"


/* lang_hooks.tree_inlining.walk_subtrees is called by walk_tree()
   after handling common cases, but before walking code-specific
   sub-trees.  If this hook is overridden for a language, it should
   handle language-specific tree codes, as well as language-specific
   information associated to common tree codes.  If a tree node is
   completely handled within this function, it should set *SUBTREES to
   0, so that generic handling isn't attempted.  For language-specific
   tree codes, generic handling would abort(), so make sure it is set
   properly.  Both SUBTREES and *SUBTREES is guaranteed to be non-zero
   when the function is called.  */

tree
tree_inlining_default_hook_walk_subtrees (tp,subtrees,func,data,htab)
     tree *tp ATTRIBUTE_UNUSED;
     int *subtrees ATTRIBUTE_UNUSED;
     walk_tree_fn func ATTRIBUTE_UNUSED;
     void *data ATTRIBUTE_UNUSED;
     void *htab ATTRIBUTE_UNUSED;
{
  return NULL_TREE;
}

/* lang_hooks.tree_inlining.cannot_inline_tree_fn is called to
   determine whether there are language-specific reasons for not
   inlining a given function.  */

int
tree_inlining_default_hook_cannot_inline_tree_fn (fnp)
     tree *fnp ATTRIBUTE_UNUSED;
{
  return 0;
}

/* lang_hooks.tree_inlining.disregard_inline_limits is called to
   determine whether a function should be considered for inlining even
   if it would exceed inlining limits.  */

int
tree_inlining_default_hook_disregard_inline_limits (fn)
     tree fn ATTRIBUTE_UNUSED;
{
  return 0;
}

/* lang_hooks.tree_inlining.add_pending_fn_decls is called before
   starting to inline a function, to push any language-specific
   functions that should not be inlined into the current function,
   into VAFNP.  PFN is the top of varray, and should be returned if no
   functions are pushed into VAFNP.  The top of the varray should be
   returned.  */

tree
tree_inlining_default_hook_add_pending_fn_decls (vafnp, pfn)
     void *vafnp ATTRIBUTE_UNUSED;
     tree pfn;
{
  return pfn;
}

/* lang_hooks.tree_inlining.tree_chain_matters_p indicates whether the
   TREE_CHAIN of a language-specific tree node is relevant, i.e.,
   whether it should be walked, copied and preserved across copies.  */

int
tree_inlining_default_hook_tree_chain_matters_p (t)
     tree t ATTRIBUTE_UNUSED;
{
  return 0;
}

/* lang_hooks.tree_inlining.auto_var_in_fn_p is called to determine
   whether VT is an automatic variable defined in function FT.  */

int
tree_inlining_default_hook_auto_var_in_fn_p (var, fn)
     tree var, fn;
{
  return (DECL_P (var) && DECL_CONTEXT (var) == fn
	  && (((TREE_CODE (var) == VAR_DECL || TREE_CODE (var) == PARM_DECL)
	       && ! TREE_STATIC (var))
	      || TREE_CODE (var) == LABEL_DECL
	      || TREE_CODE (var) == RESULT_DECL));
}

/* lang_hooks.tree_inlining.copy_res_decl_for_inlining should return a
   declaration for the result RES of function FN to be inlined into
   CALLER.  NDP points to an integer that should be set in case a new
   declaration wasn't created (presumably because RES was of aggregate
   type, such that a TARGET_EXPR is used for the result).  TEXPS is a
   pointer to a varray with the stack of TARGET_EXPRs seen while
   inlining functions into caller; the top of TEXPS is supposed to
   match RES.  */

tree
tree_inlining_default_hook_copy_res_decl_for_inlining (res, fn, caller,
						       dm, ndp, texps)
     tree res, fn, caller;
     void *dm ATTRIBUTE_UNUSED;
     int *ndp ATTRIBUTE_UNUSED;
     void *texps ATTRIBUTE_UNUSED;
{
  return copy_decl_for_inlining (res, fn, caller);
}

/* lang_hooks.tree_inlining.anon_aggr_type_p determines whether T is a
   type node representing an anonymous aggregate (union, struct, etc),
   i.e., one whose members are in the same scope as the union itself.  */

int
tree_inlining_default_hook_anon_aggr_type_p (t)
     tree t ATTRIBUTE_UNUSED;
{
  return 0;
}

