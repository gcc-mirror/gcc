/* Tree lowering pass.  This pass gimplifies the tree representation built
   by the C-based front ends.  The structure of gimplified, or
   language-independent, trees is dictated by the grammar described in this
   file.
   Copyright (C) 2002, 2003, 2004, 2005, 2007 Free Software Foundation, Inc.
   Lowering of expressions contributed by Sebastian Pop <s.pop@laposte.net>
   Re-written to support lowering of whole function trees, documentation
   and miscellaneous cleanups by Diego Novillo <dnovillo@redhat.com>

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
#include "tree.h"
#include "varray.h"
#include "c-tree.h"
#include "c-common.h"
#include "tree-gimple.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "tree-flow.h"
#include "tree-inline.h"
#include "diagnostic.h"
#include "langhooks.h"
#include "langhooks-def.h"
#include "flags.h"
#include "rtl.h"
#include "toplev.h"
#include "tree-dump.h"
#include "c-pretty-print.h"
#include "cgraph.h"


/*  The gimplification pass converts the language-dependent trees
    (ld-trees) emitted by the parser into language-independent trees
    (li-trees) that are the target of SSA analysis and transformations.

    Language-independent trees are based on the SIMPLE intermediate
    representation used in the McCAT compiler framework:

    "Designing the McCAT Compiler Based on a Family of Structured
    Intermediate Representations,"
    L. Hendren, C. Donawa, M. Emami, G. Gao, Justiani, and B. Sridharan,
    Proceedings of the 5th International Workshop on Languages and
    Compilers for Parallel Computing, no. 757 in Lecture Notes in
    Computer Science, New Haven, Connecticut, pp. 406-420,
    Springer-Verlag, August 3-5, 1992.

    http://www-acaps.cs.mcgill.ca/info/McCAT/McCAT.html

    Basically, we walk down gimplifying the nodes that we encounter.  As we
    walk back up, we check that they fit our constraints, and copy them
    into temporaries if not.  */

/* Gimplification of statement trees.  */

/* Convert the tree representation of FNDECL from C frontend trees to
   GENERIC.  */

void
c_genericize (tree fndecl)
{
  FILE *dump_orig;
  int local_dump_flags;
  struct cgraph_node *cgn;

  /* Dump the C-specific tree IR.  */
  dump_orig = dump_begin (TDI_original, &local_dump_flags);
  if (dump_orig)
    {
      fprintf (dump_orig, "\n;; Function %s",
	       lang_hooks.decl_printable_name (fndecl, 2));
      fprintf (dump_orig, " (%s)\n",
	       IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (fndecl)));
      fprintf (dump_orig, ";; enabled by -%s\n", dump_flag_name (TDI_original));
      fprintf (dump_orig, "\n");

      if (local_dump_flags & TDF_RAW)
	dump_node (DECL_SAVED_TREE (fndecl),
		   TDF_SLIM | local_dump_flags, dump_orig);
      else
	print_c_tree (dump_orig, DECL_SAVED_TREE (fndecl));
      fprintf (dump_orig, "\n");

      dump_end (TDI_original, dump_orig);
    }

  /* Go ahead and gimplify for now.  */
  gimplify_function_tree (fndecl);

  /* Dump the genericized tree IR.  */
  dump_function (TDI_generic, fndecl);

  /* Genericize all nested functions now.  We do things in this order so
     that items like VLA sizes are expanded properly in the context of
     the correct function.  */
  cgn = cgraph_node (fndecl);
  for (cgn = cgn->nested; cgn ; cgn = cgn->next_nested)
    c_genericize (cgn->decl);
}

static void
add_block_to_enclosing (tree block)
{
  tree enclosing;

  for (enclosing = gimple_current_bind_expr ();
       enclosing; enclosing = TREE_CHAIN (enclosing))
    if (BIND_EXPR_BLOCK (enclosing))
      break;

  enclosing = BIND_EXPR_BLOCK (enclosing);
  BLOCK_SUBBLOCKS (enclosing) = chainon (BLOCK_SUBBLOCKS (enclosing), block);
}

/* Genericize a scope by creating a new BIND_EXPR.
   BLOCK is either a BLOCK representing the scope or a chain of _DECLs.
     In the latter case, we need to create a new BLOCK and add it to the
     BLOCK_SUBBLOCKS of the enclosing block.
   BODY is a chain of C _STMT nodes for the contents of the scope, to be
     genericized.  */

tree
c_build_bind_expr (tree block, tree body)
{
  tree decls, bind;

  if (block == NULL_TREE)
    decls = NULL_TREE;
  else if (TREE_CODE (block) == BLOCK)
    decls = BLOCK_VARS (block);
  else
    {
      decls = block;
      if (DECL_ARTIFICIAL (decls))
	block = NULL_TREE;
      else
	{
	  block = make_node (BLOCK);
	  BLOCK_VARS (block) = decls;
	  add_block_to_enclosing (block);
	}
    }

  if (!body)
    body = build_empty_stmt ();
  if (decls || block)
    {
      bind = build3 (BIND_EXPR, void_type_node, decls, body, block);
      TREE_SIDE_EFFECTS (bind) = 1;
    }
  else
    bind = body;

  return bind;
}

/* Gimplification of expression trees.  */

/* Gimplify a C99 compound literal expression.  This just means adding
   the DECL_EXPR before the current statement and using its anonymous
   decl instead.  */

static enum gimplify_status
gimplify_compound_literal_expr (tree *expr_p, tree *pre_p)
{
  tree decl_s = COMPOUND_LITERAL_EXPR_DECL_STMT (*expr_p);
  tree decl = DECL_EXPR_DECL (decl_s);
  /* Mark the decl as addressable if the compound literal
     expression is addressable now, otherwise it is marked too late
     after we gimplify the initialization expression.  */
  if (TREE_ADDRESSABLE (*expr_p))
    TREE_ADDRESSABLE (decl) = 1;

  /* Preliminarily mark non-addressed complex variables as eligible
     for promotion to gimple registers.  We'll transform their uses
     as we find them.  */
  if ((TREE_CODE (TREE_TYPE (decl)) == COMPLEX_TYPE
       || TREE_CODE (TREE_TYPE (decl)) == VECTOR_TYPE)
      && !TREE_THIS_VOLATILE (decl)
      && !needs_to_live_in_memory (decl))
    DECL_GIMPLE_REG_P (decl) = 1;

  /* This decl isn't mentioned in the enclosing block, so add it to the
     list of temps.  FIXME it seems a bit of a kludge to say that
     anonymous artificial vars aren't pushed, but everything else is.  */
  if (DECL_NAME (decl) == NULL_TREE && !DECL_SEEN_IN_BIND_EXPR_P (decl))
    gimple_add_tmp_var (decl);

  gimplify_and_add (decl_s, pre_p);
  *expr_p = decl;
  return GS_OK;
}

/* Optimize embedded COMPOUND_LITERAL_EXPRs within a CONSTRUCTOR,
   return a new CONSTRUCTOR if something changed.  */

static tree
optimize_compound_literals_in_ctor (tree orig_ctor)
{
  tree ctor = orig_ctor;
  VEC(constructor_elt,gc) *elts = CONSTRUCTOR_ELTS (ctor);
  unsigned int idx, num = VEC_length (constructor_elt, elts);

  for (idx = 0; idx < num; idx++)
    {
      tree value = VEC_index (constructor_elt, elts, idx)->value;
      tree newval = value;
      if (TREE_CODE (value) == CONSTRUCTOR)
	newval = optimize_compound_literals_in_ctor (value);
      else if (TREE_CODE (value) == COMPOUND_LITERAL_EXPR)
	{
	  tree decl_s = COMPOUND_LITERAL_EXPR_DECL_STMT (value);
	  tree decl = DECL_EXPR_DECL (decl_s);
	  tree init = DECL_INITIAL (decl);

	  if (!TREE_ADDRESSABLE (value)
	      && !TREE_ADDRESSABLE (decl)
	      && init)
	    newval = init;
	}
      if (newval == value)
	continue;

      if (ctor == orig_ctor)
	{
	  ctor = copy_node (orig_ctor);
	  CONSTRUCTOR_ELTS (ctor) = VEC_copy (constructor_elt, gc, elts);
	  elts = CONSTRUCTOR_ELTS (ctor);
	}
      VEC_index (constructor_elt, elts, idx)->value = newval;
    }
  return ctor;
}

/* Do C-specific gimplification.  Args are as for gimplify_expr.  */

int
c_gimplify_expr (tree *expr_p, tree *pre_p, tree *post_p ATTRIBUTE_UNUSED)
{
  enum tree_code code = TREE_CODE (*expr_p);

  switch (code)
    {
    case DECL_EXPR:
      /* This is handled mostly by gimplify.c, but we have to deal with
	 not warning about int x = x; as it is a GCC extension to turn off
	 this warning but only if warn_init_self is zero.  */
      if (TREE_CODE (DECL_EXPR_DECL (*expr_p)) == VAR_DECL
	  && !DECL_EXTERNAL (DECL_EXPR_DECL (*expr_p))
	  && !TREE_STATIC (DECL_EXPR_DECL (*expr_p))
	  && (DECL_INITIAL (DECL_EXPR_DECL (*expr_p))
	      == DECL_EXPR_DECL (*expr_p))
	  && !warn_init_self)
	TREE_NO_WARNING (DECL_EXPR_DECL (*expr_p)) = 1;
      return GS_UNHANDLED;

    case COMPOUND_LITERAL_EXPR:
      return gimplify_compound_literal_expr (expr_p, pre_p);

    case INIT_EXPR:
    case MODIFY_EXPR:
      if (TREE_CODE (TREE_OPERAND (*expr_p, 1)) == COMPOUND_LITERAL_EXPR)
	{
	  tree complit = TREE_OPERAND (*expr_p, 1);
	  tree decl_s = COMPOUND_LITERAL_EXPR_DECL_STMT (complit);
	  tree decl = DECL_EXPR_DECL (decl_s);
	  tree init = DECL_INITIAL (decl);

	  /* struct T x = (struct T) { 0, 1, 2 } can be optimized
	     into struct T x = { 0, 1, 2 } if the address of the
	     compound literal has never been taken.  */
	  if (!TREE_ADDRESSABLE (complit)
	      && !TREE_ADDRESSABLE (decl)
	      && init)
	    {
	      *expr_p = copy_node (*expr_p);
	      TREE_OPERAND (*expr_p, 1) = init;
	      return GS_OK;
	    }
	}
      else if (TREE_CODE (TREE_OPERAND (*expr_p, 1)) == CONSTRUCTOR)
	{
	  tree ctor
	    = optimize_compound_literals_in_ctor (TREE_OPERAND (*expr_p, 1));

	  if (ctor != TREE_OPERAND (*expr_p, 1))
	    {
	      *expr_p = copy_node (*expr_p);
	      TREE_OPERAND (*expr_p, 1) = ctor;
	      return GS_OK;
	    }
	}
      return GS_UNHANDLED;

    default:
      return GS_UNHANDLED;
    }
}
