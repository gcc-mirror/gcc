/* Tree lowering pass.  This pass gimplifies the tree representation built
   by the C-based front ends.  The structure of gimplified, or
   language-independent, trees is dictated by the grammar described in this
   file.
   Copyright (C) 2002-2017 Free Software Foundation, Inc.
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
#include "function.h"
#include "basic-block.h"
#include "tree.h"
#include "gimple.h"
#include "cgraph.h"
#include "c-pretty-print.h"
#include "gimplify.h"
#include "langhooks.h"
#include "dumpfile.h"
#include "cilk.h"
#include "c-ubsan.h"

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

/* Callback for c_genericize.  */

static tree
ubsan_walk_array_refs_r (tree *tp, int *walk_subtrees, void *data)
{
  hash_set<tree> *pset = (hash_set<tree> *) data;

  if (TREE_CODE (*tp) == BIND_EXPR)
    {
      /* Since walk_tree doesn't call the callback function on the decls
	 in BIND_EXPR_VARS, we have to walk them manually, so we can avoid
	 instrumenting DECL_INITIAL of TREE_STATIC vars.  */
      *walk_subtrees = 0;
      for (tree decl = BIND_EXPR_VARS (*tp); decl; decl = DECL_CHAIN (decl))
	{
	  if (TREE_STATIC (decl))
	    continue;
	  walk_tree (&DECL_INITIAL (decl), ubsan_walk_array_refs_r, pset,
		     pset);
	  walk_tree (&DECL_SIZE (decl), ubsan_walk_array_refs_r, pset, pset);
	  walk_tree (&DECL_SIZE_UNIT (decl), ubsan_walk_array_refs_r, pset,
		     pset);
	}
      walk_tree (&BIND_EXPR_BODY (*tp), ubsan_walk_array_refs_r, pset, pset);
    }
  else if (TREE_CODE (*tp) == ADDR_EXPR
	   && TREE_CODE (TREE_OPERAND (*tp, 0)) == ARRAY_REF)
    {
      ubsan_maybe_instrument_array_ref (&TREE_OPERAND (*tp, 0), true);
      /* Make sure ubsan_maybe_instrument_array_ref is not called again
	 on the ARRAY_REF, the above call might not instrument anything
	 as the index might be constant or masked, so ensure it is not
	 walked again and walk its subtrees manually.  */
      tree aref = TREE_OPERAND (*tp, 0);
      pset->add (aref);
      *walk_subtrees = 0;
      walk_tree (&TREE_OPERAND (aref, 0), ubsan_walk_array_refs_r, pset, pset);
      walk_tree (&TREE_OPERAND (aref, 1), ubsan_walk_array_refs_r, pset, pset);
      walk_tree (&TREE_OPERAND (aref, 2), ubsan_walk_array_refs_r, pset, pset);
      walk_tree (&TREE_OPERAND (aref, 3), ubsan_walk_array_refs_r, pset, pset);
    }
  else if (TREE_CODE (*tp) == ARRAY_REF)
    ubsan_maybe_instrument_array_ref (tp, false);
  return NULL_TREE;
}

/* Gimplification of statement trees.  */

/* Convert the tree representation of FNDECL from C frontend trees to
   GENERIC.  */

void
c_genericize (tree fndecl)
{
  FILE *dump_orig;
  int local_dump_flags;
  struct cgraph_node *cgn;

  if (flag_sanitize & SANITIZE_BOUNDS)
    {
      hash_set<tree> pset;
      walk_tree (&DECL_SAVED_TREE (fndecl), ubsan_walk_array_refs_r, &pset,
		 &pset);
    }

  if (warn_duplicated_branches)
    walk_tree_without_duplicates (&DECL_SAVED_TREE (fndecl),
				  do_warn_duplicated_branches_r, NULL);

  /* Dump the C-specific tree IR.  */
  dump_orig = get_dump_info (TDI_original, &local_dump_flags);
  if (dump_orig)
    {
      fprintf (dump_orig, "\n;; Function %s",
	       lang_hooks.decl_printable_name (fndecl, 2));
      fprintf (dump_orig, " (%s)\n",
	       (!DECL_ASSEMBLER_NAME_SET_P (fndecl) ? "null"
		: IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (fndecl))));
      fprintf (dump_orig, ";; enabled by -%s\n", dump_flag_name (TDI_original));
      fprintf (dump_orig, "\n");

      if (local_dump_flags & TDF_RAW)
	dump_node (DECL_SAVED_TREE (fndecl),
		   TDF_SLIM | local_dump_flags, dump_orig);
      else
	print_c_tree (dump_orig, DECL_SAVED_TREE (fndecl));
      fprintf (dump_orig, "\n");
    }

  /* Dump all nested functions now.  */
  cgn = cgraph_node::get_create (fndecl);
  for (cgn = cgn->nested; cgn ; cgn = cgn->next_nested)
    c_genericize (cgn->decl);
}

static void
add_block_to_enclosing (tree block)
{
  unsigned i;
  tree enclosing;
  gbind *bind;
  vec<gbind *> stack = gimple_bind_expr_stack ();

  FOR_EACH_VEC_ELT (stack, i, bind)
    if (gimple_bind_block (bind))
      break;

  enclosing = gimple_bind_block (bind);
  BLOCK_SUBBLOCKS (enclosing) = chainon (BLOCK_SUBBLOCKS (enclosing), block);
}

/* Genericize a scope by creating a new BIND_EXPR.
   BLOCK is either a BLOCK representing the scope or a chain of _DECLs.
     In the latter case, we need to create a new BLOCK and add it to the
     BLOCK_SUBBLOCKS of the enclosing block.
   BODY is a chain of C _STMT nodes for the contents of the scope, to be
     genericized.  */

tree
c_build_bind_expr (location_t loc, tree block, tree body)
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
    body = build_empty_stmt (loc);
  if (decls || block)
    {
      bind = build3 (BIND_EXPR, void_type_node, decls, body, block);
      TREE_SIDE_EFFECTS (bind) = 1;
      SET_EXPR_LOCATION (bind, loc);
    }
  else
    bind = body;

  return bind;
}

/* Gimplification of expression trees.  */

/* Do C-specific gimplification on *EXPR_P.  PRE_P and POST_P are as in
   gimplify_expr.  */

int
c_gimplify_expr (tree *expr_p, gimple_seq *pre_p ATTRIBUTE_UNUSED,
		 gimple_seq *post_p ATTRIBUTE_UNUSED)
{
  enum tree_code code = TREE_CODE (*expr_p);

  switch (code)
    {
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
      {
	/* We used to convert the right operand of a shift-expression
	   to an integer_type_node in the FEs.  But it is unnecessary
	   and not desirable for diagnostics and sanitizers.  We keep
	   this here to not pessimize the code, but we convert to an
	   unsigned type, because negative shift counts are undefined
	   anyway.
	   We should get rid of this conversion when we have a proper
	   type demotion/promotion pass.  */
	tree *op1_p = &TREE_OPERAND (*expr_p, 1);
	if (!VECTOR_TYPE_P (TREE_TYPE (*op1_p))
	    && !types_compatible_p (TYPE_MAIN_VARIANT (TREE_TYPE (*op1_p)),
				    unsigned_type_node)
	    && !types_compatible_p (TYPE_MAIN_VARIANT (TREE_TYPE (*op1_p)),
				    integer_type_node))
	  *op1_p = convert (unsigned_type_node, *op1_p);
	break;
      }

    case DECL_EXPR:
      /* This is handled mostly by gimplify.c, but we have to deal with
	 not warning about int x = x; as it is a GCC extension to turn off
	 this warning but only if warn_init_self is zero.  */
      if (VAR_P (DECL_EXPR_DECL (*expr_p))
	  && !DECL_EXTERNAL (DECL_EXPR_DECL (*expr_p))
	  && !TREE_STATIC (DECL_EXPR_DECL (*expr_p))
	  && (DECL_INITIAL (DECL_EXPR_DECL (*expr_p)) == DECL_EXPR_DECL (*expr_p))
	  && !warn_init_self)
	TREE_NO_WARNING (DECL_EXPR_DECL (*expr_p)) = 1;
      break;

    case PREINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
      {
	tree type = TREE_TYPE (TREE_OPERAND (*expr_p, 0));
	if (INTEGRAL_TYPE_P (type) && c_promoting_integer_type_p (type))
	  {
	    if (!TYPE_OVERFLOW_WRAPS (type))
	      type = unsigned_type_for (type);
	    return gimplify_self_mod_expr (expr_p, pre_p, post_p, 1, type);
	  }
	break;
      }

    case CILK_SPAWN_STMT:
      gcc_assert(fn_contains_cilk_spawn_p (cfun)
		 && cilk_detect_spawn_and_unwrap (expr_p));

      if (!seen_error ())
	{
	  cilk_gimplify_call_params_in_spawned_fn (expr_p, pre_p);
	  return (enum gimplify_status) gimplify_cilk_spawn (expr_p);
	}
      return GS_ERROR;

    case MODIFY_EXPR:
    case INIT_EXPR:
    case CALL_EXPR:
      if (fn_contains_cilk_spawn_p (cfun)
	  && cilk_detect_spawn_and_unwrap (expr_p)
	  /* If an error is found, the spawn wrapper is removed and the
	     original expression (MODIFY/INIT/CALL_EXPR) is processes as
	     it is supposed to be.  */
	  && !seen_error ())
	{
	  cilk_gimplify_call_params_in_spawned_fn (expr_p, pre_p);
	  return (enum gimplify_status) gimplify_cilk_spawn (expr_p);
	}

    default:;
    }

  return GS_UNHANDLED;
}
