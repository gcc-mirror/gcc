/* Control and data flow functions for trees.
   Copyright 2001, 2002, 2003, 2004 Free Software Foundation, Inc.
   Contributed by Alexandre Oliva <aoliva@redhat.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "toplev.h"
#include "tree.h"
#include "tree-inline.h"
#include "rtl.h"
#include "expr.h"
#include "flags.h"
#include "params.h"
#include "input.h"
#include "insn-config.h"
#include "integrate.h"
#include "varray.h"
#include "hashtab.h"
#include "splay-tree.h"
#include "langhooks.h"
#include "cgraph.h"
#include "intl.h"
#include "diagnostic.h"

/* This should be eventually be generalized to other languages, but
   this would require a shared function-as-trees infrastructure.  */
#ifndef INLINER_FOR_JAVA
#include "c-common.h"
#else /* INLINER_FOR_JAVA */
#include "parse.h"
#include "java-tree.h"
#endif /* INLINER_FOR_JAVA */

/* 0 if we should not perform inlining.
   1 if we should expand functions calls inline at the tree level.
   2 if we should consider *all* functions to be inline
   candidates.  */

int flag_inline_trees = 0;

/* To Do:

   o In order to make inlining-on-trees work, we pessimized
     function-local static constants.  In particular, they are now
     always output, even when not addressed.  Fix this by treating
     function-local static constants just like global static
     constants; the back-end already knows not to output them if they
     are not needed.

   o Provide heuristics to clamp inlining of recursive template
     calls?  */

/* Data required for function inlining.  */

typedef struct inline_data
{
  /* A stack of the functions we are inlining.  For example, if we are
     compiling `f', which calls `g', which calls `h', and we are
     inlining the body of `h', the stack will contain, `h', followed
     by `g', followed by `f'.  The first few elements of the stack may
     contain other functions that we know we should not recurse into,
     even though they are not directly being inlined.  */
  varray_type fns;
  /* The index of the first element of FNS that really represents an
     inlined function.  */
  unsigned first_inlined_fn;
  /* The label to jump to when a return statement is encountered.  If
     this value is NULL, then return statements will simply be
     remapped as return statements, rather than as jumps.  */
  tree ret_label;
  /* The map from local declarations in the inlined function to
     equivalents in the function into which it is being inlined.  */
  splay_tree decl_map;
  /* Nonzero if we are currently within the cleanup for a
     TARGET_EXPR.  */
  int in_target_cleanup_p;
  /* A list of the functions current function has inlined.  */
  varray_type inlined_fns;
  /* We use the same mechanism to build clones that we do to perform
     inlining.  However, there are a few places where we need to
     distinguish between those two situations.  This flag is true if
     we are cloning, rather than inlining.  */
  bool cloning_p;
  /* Hash table used to prevent walk_tree from visiting the same node
     umpteen million times.  */
  htab_t tree_pruner;
  /* Decl of function we are inlining into.  */
  tree decl;
  tree current_decl;
} inline_data;

/* Prototypes.  */

static tree declare_return_variable (inline_data *, tree, tree *);
static tree copy_body_r (tree *, int *, void *);
static tree copy_body (inline_data *);
static tree expand_call_inline (tree *, int *, void *);
static void expand_calls_inline (tree *, inline_data *);
static bool inlinable_function_p (tree);
static tree remap_decl (tree, inline_data *);
static tree remap_type (tree, inline_data *);
#ifndef INLINER_FOR_JAVA
static tree initialize_inlined_parameters (inline_data *, tree, tree);
static void remap_block (tree, tree, inline_data *);
static void copy_scope_stmt (tree *, int *, inline_data *);
#else /* INLINER_FOR_JAVA */
static tree initialize_inlined_parameters (inline_data *, tree, tree, tree);
static void remap_block (tree *, tree, inline_data *);
static tree add_stmt_to_compound (tree, tree, tree);
#endif /* INLINER_FOR_JAVA */

/* Remap DECL during the copying of the BLOCK tree for the function.  */

static tree
remap_decl (tree decl, inline_data *id)
{
  splay_tree_node n;
  tree fn;

  /* We only remap local variables in the current function.  */
  fn = VARRAY_TOP_TREE (id->fns);
  if (! (*lang_hooks.tree_inlining.auto_var_in_fn_p) (decl, fn))
    return NULL_TREE;

  /* See if we have remapped this declaration.  */
  n = splay_tree_lookup (id->decl_map, (splay_tree_key) decl);

  /* If we didn't already have an equivalent for this declaration,
     create one now.  */
  if (!n)
    {
      tree t;

      /* Make a copy of the variable or label.  */
      t = copy_decl_for_inlining (decl, fn, VARRAY_TREE (id->fns, 0));

      /* Remap types, if necessary.  */
      TREE_TYPE (t) = remap_type (TREE_TYPE (t), id);
      if (TREE_CODE (t) == TYPE_DECL)
        DECL_ORIGINAL_TYPE (t) = remap_type (DECL_ORIGINAL_TYPE (t), id);
      else if (TREE_CODE (t) == PARM_DECL)
        DECL_ARG_TYPE_AS_WRITTEN (t)
	  = remap_type (DECL_ARG_TYPE_AS_WRITTEN (t), id);

      /* Remap sizes as necessary.  */
      walk_tree (&DECL_SIZE (t), copy_body_r, id, NULL);
      walk_tree (&DECL_SIZE_UNIT (t), copy_body_r, id, NULL);

#ifndef INLINER_FOR_JAVA
      if (! DECL_NAME (t) && TREE_TYPE (t)
	  && (*lang_hooks.tree_inlining.anon_aggr_type_p) (TREE_TYPE (t)))
	{
	  /* For a VAR_DECL of anonymous type, we must also copy the
	     member VAR_DECLS here and rechain the DECL_ANON_UNION_ELEMS.  */
	  tree members = NULL;
	  tree src;

	  for (src = DECL_ANON_UNION_ELEMS (t); src;
	       src = TREE_CHAIN (src))
	    {
	      tree member = remap_decl (TREE_VALUE (src), id);

	      if (TREE_PURPOSE (src))
		abort ();
	      members = tree_cons (NULL, member, members);
	    }
	  DECL_ANON_UNION_ELEMS (t) = nreverse (members);
	}
#endif /* not INLINER_FOR_JAVA */

      /* Remember it, so that if we encounter this local entity
	 again we can reuse this copy.  */
      n = splay_tree_insert (id->decl_map,
			     (splay_tree_key) decl,
			     (splay_tree_value) t);
    }

  return (tree) n->value;
}

static tree
remap_type (tree type, inline_data *id)
{
  splay_tree_node node;
  tree new, t;

  if (type == NULL)
    return type;

  /* See if we have remapped this type.  */
  node = splay_tree_lookup (id->decl_map, (splay_tree_key) type);
  if (node)
    return (tree) node->value;

  /* The type only needs remapping if it's variably modified.  */
  if (! variably_modified_type_p (type))
    {
      splay_tree_insert (id->decl_map, (splay_tree_key) type,
			 (splay_tree_value) type);
      return type;
    }
  
  /* We do need a copy.  build and register it now.  */
  new = copy_node (type);
  splay_tree_insert (id->decl_map, (splay_tree_key) type,
		     (splay_tree_value) new);

  /* This is a new type, not a copy of an old type.  Need to reassociate
     variants.  We can handle everything except the main variant lazily.  */
  t = TYPE_MAIN_VARIANT (type);
  if (type != t)
    {
      t = remap_type (t, id);
      TYPE_MAIN_VARIANT (new) = t;
      TYPE_NEXT_VARIANT (new) = TYPE_MAIN_VARIANT (t);
      TYPE_NEXT_VARIANT (t) = new;
    }
  else
    {
      TYPE_MAIN_VARIANT (new) = new;
      TYPE_NEXT_VARIANT (new) = NULL;
    }

  /* Lazily create pointer and reference types.  */
  TYPE_POINTER_TO (new) = NULL;
  TYPE_REFERENCE_TO (new) = NULL;

  switch (TREE_CODE (new))
    {
    case INTEGER_TYPE:
    case REAL_TYPE:
    case ENUMERAL_TYPE:
    case BOOLEAN_TYPE:
    case CHAR_TYPE:
      t = TYPE_MIN_VALUE (new);
      if (t && TREE_CODE (t) != INTEGER_CST)
        walk_tree (&TYPE_MIN_VALUE (new), copy_body_r, id, NULL);
      t = TYPE_MAX_VALUE (new);
      if (t && TREE_CODE (t) != INTEGER_CST)
        walk_tree (&TYPE_MAX_VALUE (new), copy_body_r, id, NULL);
      return new;
    
    case POINTER_TYPE:
      TREE_TYPE (new) = t = remap_type (TREE_TYPE (new), id);
      if (TYPE_MODE (new) == ptr_mode)
        TYPE_POINTER_TO (t) = new;
      return new;

    case REFERENCE_TYPE:
      TREE_TYPE (new) = t = remap_type (TREE_TYPE (new), id);
      if (TYPE_MODE (new) == ptr_mode)
        TYPE_REFERENCE_TO (t) = new;
      return new;

    case METHOD_TYPE:
    case FUNCTION_TYPE:
      TREE_TYPE (new) = remap_type (TREE_TYPE (new), id);
      walk_tree (&TYPE_ARG_TYPES (new), copy_body_r, id, NULL);
      return new;

    case ARRAY_TYPE:
      TREE_TYPE (new) = remap_type (TREE_TYPE (new), id);
      TYPE_DOMAIN (new) = remap_type (TYPE_DOMAIN (new), id);
      break;

    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      walk_tree (&TYPE_FIELDS (new), copy_body_r, id, NULL);
      break;

    case FILE_TYPE:
    case SET_TYPE:
    case OFFSET_TYPE:
    default:
      /* Shouldn't have been thought variable sized.  */
      abort ();
    }

  walk_tree (&TYPE_SIZE (new), copy_body_r, id, NULL);
  walk_tree (&TYPE_SIZE_UNIT (new), copy_body_r, id, NULL);

  return new;
}

#ifndef INLINER_FOR_JAVA
/* Copy the SCOPE_STMT_BLOCK associated with SCOPE_STMT to contain
   remapped versions of the variables therein.  And hook the new block
   into the block-tree.  If non-NULL, the DECLS are declarations to
   add to use instead of the BLOCK_VARS in the old block.  */
#else /* INLINER_FOR_JAVA */
/* Copy the BLOCK to contain remapped versions of the variables
   therein.  And hook the new block into the block-tree.  */
#endif /* INLINER_FOR_JAVA */

static void
#ifndef INLINER_FOR_JAVA
remap_block (tree scope_stmt, tree decls, inline_data *id)
#else /* INLINER_FOR_JAVA */
remap_block (tree *block, tree decls, inline_data *id)
#endif /* INLINER_FOR_JAVA */
{
#ifndef INLINER_FOR_JAVA
  /* We cannot do this in the cleanup for a TARGET_EXPR since we do
     not know whether or not expand_expr will actually write out the
     code we put there.  If it does not, then we'll have more BLOCKs
     than block-notes, and things will go awry.  At some point, we
     should make the back-end handle BLOCK notes in a tidier way,
     without requiring a strict correspondence to the block-tree; then
     this check can go.  */
  if (id->in_target_cleanup_p)
    {
      SCOPE_STMT_BLOCK (scope_stmt) = NULL_TREE;
      return;
    }

  /* If this is the beginning of a scope, remap the associated BLOCK.  */
  if (SCOPE_BEGIN_P (scope_stmt) && SCOPE_STMT_BLOCK (scope_stmt))
    {
      tree old_block;
      tree new_block;
      tree old_var;
      tree fn;

      /* Make the new block.  */
      old_block = SCOPE_STMT_BLOCK (scope_stmt);
      new_block = make_node (BLOCK);
      TREE_USED (new_block) = TREE_USED (old_block);
      BLOCK_ABSTRACT_ORIGIN (new_block) = old_block;
      SCOPE_STMT_BLOCK (scope_stmt) = new_block;

      /* Remap its variables.  */
      for (old_var = decls ? decls : BLOCK_VARS (old_block);
	   old_var;
	   old_var = TREE_CHAIN (old_var))
	{
	  tree new_var;

	  /* Remap the variable.  */
	  new_var = remap_decl (old_var, id);
	  /* If we didn't remap this variable, so we can't mess with
	     its TREE_CHAIN.  If we remapped this variable to
	     something other than a declaration (say, if we mapped it
	     to a constant), then we must similarly omit any mention
	     of it here.  */
	  if (!new_var || !DECL_P (new_var))
	    ;
	  else
	    {
	      TREE_CHAIN (new_var) = BLOCK_VARS (new_block);
	      BLOCK_VARS (new_block) = new_var;
	    }
	}
      /* We put the BLOCK_VARS in reverse order; fix that now.  */
      BLOCK_VARS (new_block) = nreverse (BLOCK_VARS (new_block));
      fn = VARRAY_TREE (id->fns, 0);
      if (id->cloning_p)
	/* We're building a clone; DECL_INITIAL is still
	   error_mark_node, and current_binding_level is the parm
	   binding level.  */
	(*lang_hooks.decls.insert_block) (new_block);
      else
	{
	  /* Attach this new block after the DECL_INITIAL block for the
	     function into which this block is being inlined.  In
	     rest_of_compilation we will straighten out the BLOCK tree.  */
	  tree *first_block;
	  if (DECL_INITIAL (fn))
	    first_block = &BLOCK_CHAIN (DECL_INITIAL (fn));
	  else
	    first_block = &DECL_INITIAL (fn);
	  BLOCK_CHAIN (new_block) = *first_block;
	  *first_block = new_block;
	}
      /* Remember the remapped block.  */
      splay_tree_insert (id->decl_map,
			 (splay_tree_key) old_block,
			 (splay_tree_value) new_block);
    }
  /* If this is the end of a scope, set the SCOPE_STMT_BLOCK to be the
     remapped block.  */
  else if (SCOPE_END_P (scope_stmt) && SCOPE_STMT_BLOCK (scope_stmt))
    {
      splay_tree_node n;

      /* Find this block in the table of remapped things.  */
      n = splay_tree_lookup (id->decl_map,
			     (splay_tree_key) SCOPE_STMT_BLOCK (scope_stmt));
      if (! n)
	abort ();
      SCOPE_STMT_BLOCK (scope_stmt) = (tree) n->value;
    }
#else /* INLINER_FOR_JAVA */
  tree old_block;
  tree new_block;
  tree old_var;
  tree fn;

  /* Make the new block.  */
  old_block = *block;
  new_block = make_node (BLOCK);
  TREE_USED (new_block) = TREE_USED (old_block);
  BLOCK_ABSTRACT_ORIGIN (new_block) = old_block;
  BLOCK_SUBBLOCKS (new_block) = BLOCK_SUBBLOCKS (old_block);
  TREE_SIDE_EFFECTS (new_block) = TREE_SIDE_EFFECTS (old_block);
  TREE_TYPE (new_block) = TREE_TYPE (old_block);
  *block = new_block;

  /* Remap its variables.  */
  for (old_var = decls ? decls : BLOCK_VARS (old_block);
       old_var;
       old_var = TREE_CHAIN (old_var))
    {
      tree new_var;

      /* All local class initialization flags go in the outermost
	 scope.  */
      if (LOCAL_CLASS_INITIALIZATION_FLAG_P (old_var))
	{
	  /* We may already have one.  */
	  if (! splay_tree_lookup (id->decl_map, (splay_tree_key) old_var))
	    {
	      tree outermost_block;
	      new_var = remap_decl (old_var, id);
	      DECL_ABSTRACT_ORIGIN (new_var) = NULL;
	      outermost_block = DECL_SAVED_TREE (current_function_decl);
	      TREE_CHAIN (new_var) = BLOCK_VARS (outermost_block);
	      BLOCK_VARS (outermost_block) = new_var;
	    }
	  continue;
	}

      /* Remap the variable.  */
      new_var = remap_decl (old_var, id);
      /* If we didn't remap this variable, so we can't mess with
	 its TREE_CHAIN.  If we remapped this variable to
	 something other than a declaration (say, if we mapped it
	 to a constant), then we must similarly omit any mention
	 of it here.  */
      if (!new_var || !DECL_P (new_var))
	;
      else
	{
	  TREE_CHAIN (new_var) = BLOCK_VARS (new_block);
	  BLOCK_VARS (new_block) = new_var;
	}
    }
  /* We put the BLOCK_VARS in reverse order; fix that now.  */
  BLOCK_VARS (new_block) = nreverse (BLOCK_VARS (new_block));
  fn = VARRAY_TREE (id->fns, 0);
  /* Remember the remapped block.  */
  splay_tree_insert (id->decl_map,
		     (splay_tree_key) old_block,
		     (splay_tree_value) new_block);
#endif /* INLINER_FOR_JAVA */
}

#ifndef INLINER_FOR_JAVA
/* Copy the SCOPE_STMT pointed to by TP.  */

static void
copy_scope_stmt (tree *tp, int *walk_subtrees, inline_data *id)
{
  tree block;

  /* Remember whether or not this statement was nullified.  When
     making a copy, copy_tree_r always sets SCOPE_NULLIFIED_P (and
     doesn't copy the SCOPE_STMT_BLOCK) to free callers from having to
     deal with copying BLOCKs if they do not wish to do so.  */
  block = SCOPE_STMT_BLOCK (*tp);
  /* Copy (and replace) the statement.  */
  copy_tree_r (tp, walk_subtrees, NULL);
  /* Restore the SCOPE_STMT_BLOCK.  */
  SCOPE_STMT_BLOCK (*tp) = block;

  /* Remap the associated block.  */
  remap_block (*tp, NULL_TREE, id);
}
#endif /* not INLINER_FOR_JAVA */

/* Called from copy_body via walk_tree.  DATA is really an
   `inline_data *'.  */
static tree
copy_body_r (tree *tp, int *walk_subtrees, void *data)
{
  inline_data* id;
  tree fn;

  /* Set up.  */
  id = (inline_data *) data;
  fn = VARRAY_TOP_TREE (id->fns);

#if 0
  /* All automatic variables should have a DECL_CONTEXT indicating
     what function they come from.  */
  if ((TREE_CODE (*tp) == VAR_DECL || TREE_CODE (*tp) == LABEL_DECL)
      && DECL_NAMESPACE_SCOPE_P (*tp))
    if (! DECL_EXTERNAL (*tp) && ! TREE_STATIC (*tp))
      abort ();
#endif

#ifdef INLINER_FOR_JAVA
  if (TREE_CODE (*tp) == BLOCK)
    remap_block (tp, NULL_TREE, id);
#endif

  /* If this is a RETURN_STMT, change it into an EXPR_STMT and a
     GOTO_STMT with the RET_LABEL as its target.  */
#ifndef INLINER_FOR_JAVA
  if (TREE_CODE (*tp) == RETURN_STMT && id->ret_label)
#else /* INLINER_FOR_JAVA */
  if (TREE_CODE (*tp) == RETURN_EXPR && id->ret_label)
#endif /* INLINER_FOR_JAVA */
    {
      tree return_stmt = *tp;
      tree goto_stmt;

      /* Build the GOTO_STMT.  */
#ifndef INLINER_FOR_JAVA
      goto_stmt = build_stmt (GOTO_STMT, id->ret_label);
      TREE_CHAIN (goto_stmt) = TREE_CHAIN (return_stmt);
      GOTO_FAKE_P (goto_stmt) = 1;
#else /* INLINER_FOR_JAVA */
      tree assignment = TREE_OPERAND (return_stmt, 0);
      goto_stmt = build1 (GOTO_EXPR, void_type_node, id->ret_label);
      TREE_SIDE_EFFECTS (goto_stmt) = 1;
#endif /* INLINER_FOR_JAVA */

      /* If we're returning something, just turn that into an
	 assignment into the equivalent of the original
	 RESULT_DECL.  */
#ifndef INLINER_FOR_JAVA
      if (RETURN_STMT_EXPR (return_stmt))
	{
	  *tp = build_stmt (EXPR_STMT,
			    RETURN_STMT_EXPR (return_stmt));
	  STMT_IS_FULL_EXPR_P (*tp) = 1;
	  /* And then jump to the end of the function.  */
	  TREE_CHAIN (*tp) = goto_stmt;
	}
#else /* INLINER_FOR_JAVA */
      if (assignment)
	{
	  copy_body_r (&assignment, walk_subtrees, data);
	  *tp = build (COMPOUND_EXPR, void_type_node, assignment, goto_stmt);
	  TREE_SIDE_EFFECTS (*tp) = 1;
	}
#endif /* INLINER_FOR_JAVA */
      /* If we're not returning anything just do the jump.  */
      else
	*tp = goto_stmt;
    }
  /* Local variables and labels need to be replaced by equivalent
     variables.  We don't want to copy static variables; there's only
     one of those, no matter how many times we inline the containing
     function.
     We do not also want to copy the label which we put into
     GOTO_STMT which replaced RETURN_STMT.  */
  else if (*tp != id->ret_label
	   && (*lang_hooks.tree_inlining.auto_var_in_fn_p) (*tp, fn))
    {
      tree new_decl;

      /* Remap the declaration.  */
      new_decl = remap_decl (*tp, id);
      if (! new_decl)
	abort ();
      /* Replace this variable with the copy.  */
      STRIP_TYPE_NOPS (new_decl);
      *tp = new_decl;
    }
#if 0
  else if (nonstatic_local_decl_p (*tp)
	   && DECL_CONTEXT (*tp) != VARRAY_TREE (id->fns, 0))
    abort ();
#endif
  else if (TREE_CODE (*tp) == SAVE_EXPR)
    remap_save_expr (tp, id->decl_map, VARRAY_TREE (id->fns, 0),
		     walk_subtrees);
  else if (TREE_CODE (*tp) == UNSAVE_EXPR)
    /* UNSAVE_EXPRs should not be generated until expansion time.  */
    abort ();
#ifndef INLINER_FOR_JAVA
  /* For a SCOPE_STMT, we must copy the associated block so that we
     can write out debugging information for the inlined variables.  */
  else if (TREE_CODE (*tp) == SCOPE_STMT && !id->in_target_cleanup_p)
    copy_scope_stmt (tp, walk_subtrees, id);
#else /* INLINER_FOR_JAVA */
  else if (TREE_CODE (*tp) == LABELED_BLOCK_EXPR)
    {
      /* We need a new copy of this labeled block; the EXIT_BLOCK_EXPR
         will refer to it, so save a copy ready for remapping.  We
         save it in the decl_map, although it isn't a decl.  */
      tree new_block = copy_node (*tp);
      splay_tree_insert (id->decl_map,
			 (splay_tree_key) *tp,
			 (splay_tree_value) new_block);
      *tp = new_block;
    }
  else if (TREE_CODE (*tp) == EXIT_BLOCK_EXPR)
    {
      splay_tree_node n
	= splay_tree_lookup (id->decl_map,
			     (splay_tree_key) TREE_OPERAND (*tp, 0));
      /* We _must_ have seen the enclosing LABELED_BLOCK_EXPR.  */
      if (! n)
	abort ();
      *tp = copy_node (*tp);
      TREE_OPERAND (*tp, 0) = (tree) n->value;
    }
#endif /* INLINER_FOR_JAVA */
  /* Types may need remapping as well.  */
  else if (TYPE_P (*tp))
    *tp = remap_type (*tp, id);

  /* Otherwise, just copy the node.  Note that copy_tree_r already
     knows not to copy VAR_DECLs, etc., so this is safe.  */
  else
    {
      if (TREE_CODE (*tp) == MODIFY_EXPR
	  && TREE_OPERAND (*tp, 0) == TREE_OPERAND (*tp, 1)
	  && ((*lang_hooks.tree_inlining.auto_var_in_fn_p)
	      (TREE_OPERAND (*tp, 0), fn)))
	{
	  /* Some assignments VAR = VAR; don't generate any rtl code
	     and thus don't count as variable modification.  Avoid
	     keeping bogosities like 0 = 0.  */
	  tree decl = TREE_OPERAND (*tp, 0), value;
	  splay_tree_node n;

	  n = splay_tree_lookup (id->decl_map, (splay_tree_key) decl);
	  if (n)
	    {
	      value = (tree) n->value;
	      STRIP_TYPE_NOPS (value);
	      if (TREE_CONSTANT (value) || TREE_READONLY_DECL_P (value))
		{
		  *tp = value;
		  return copy_body_r (tp, walk_subtrees, data);
		}
	    }
	}
      else if (TREE_CODE (*tp) == ADDR_EXPR
	       && ((*lang_hooks.tree_inlining.auto_var_in_fn_p)
		   (TREE_OPERAND (*tp, 0), fn)))
	{
	  /* Get rid of &* from inline substitutions.  It can occur when
	     someone takes the address of a parm or return slot passed by
	     invisible reference.  */
	  tree decl = TREE_OPERAND (*tp, 0), value;
	  splay_tree_node n;

	  n = splay_tree_lookup (id->decl_map, (splay_tree_key) decl);
	  if (n)
	    {
	      value = (tree) n->value;
	      if (TREE_CODE (value) == INDIRECT_REF)
		{
		  *tp = convert (TREE_TYPE (*tp), TREE_OPERAND (value, 0));
		  return copy_body_r (tp, walk_subtrees, data);
		}
	    }
	}

      copy_tree_r (tp, walk_subtrees, NULL);

      TREE_TYPE (*tp) = remap_type (TREE_TYPE (*tp), id);

      /* The copied TARGET_EXPR has never been expanded, even if the
	 original node was expanded already.  */
      if (TREE_CODE (*tp) == TARGET_EXPR && TREE_OPERAND (*tp, 3))
	{
	  TREE_OPERAND (*tp, 1) = TREE_OPERAND (*tp, 3);
	  TREE_OPERAND (*tp, 3) = NULL_TREE;
	}
    }

  /* Keep iterating.  */
  return NULL_TREE;
}

/* Make a copy of the body of FN so that it can be inserted inline in
   another function.  */

static tree
copy_body (inline_data *id)
{
  tree body;

  body = DECL_SAVED_TREE (VARRAY_TOP_TREE (id->fns));
  walk_tree (&body, copy_body_r, id, NULL);

  return body;
}

/* Generate code to initialize the parameters of the function at the
   top of the stack in ID from the ARGS (presented as a TREE_LIST).  */

static tree
#ifndef INLINER_FOR_JAVA
initialize_inlined_parameters (inline_data *id, tree args, tree fn)
#else /* INLINER_FOR_JAVA */
initialize_inlined_parameters (inline_data *id, tree args, tree fn, tree block)
#endif /* INLINER_FOR_JAVA */
{
  tree init_stmts;
  tree parms;
  tree a;
  tree p;
#ifdef INLINER_FOR_JAVA
  tree vars = NULL_TREE;
#endif /* INLINER_FOR_JAVA */
  int argnum = 0;

  /* Figure out what the parameters are.  */
  parms = 
DECL_ARGUMENTS (fn);

  /* Start with no initializations whatsoever.  */
  init_stmts = NULL_TREE;

  /* Loop through the parameter declarations, replacing each with an
     equivalent VAR_DECL, appropriately initialized.  */
  for (p = parms, a = args; p;
       a = a ? TREE_CHAIN (a) : a, p = TREE_CHAIN (p))
    {
#ifndef INLINER_FOR_JAVA
      tree init_stmt;
      tree cleanup;
#endif /* not INLINER_FOR_JAVA */
      tree var;
      tree value;
      tree var_sub;

      ++argnum;

      /* Find the initializer.  */
      value = (*lang_hooks.tree_inlining.convert_parm_for_inlining)
	      (p, a ? TREE_VALUE (a) : NULL_TREE, fn, argnum);

      /* If the parameter is never assigned to, we may not need to
	 create a new variable here at all.  Instead, we may be able
	 to just use the argument value.  */
      if (TREE_READONLY (p)
	  && !TREE_ADDRESSABLE (p)
	  && value && !TREE_SIDE_EFFECTS (value))
	{
	  /* Simplify the value, if possible.  */
	  value = fold (DECL_P (value) ? decl_constant_value (value) : value);

	  /* We can't risk substituting complex expressions.  They
	     might contain variables that will be assigned to later.
	     Theoretically, we could check the expression to see if
	     all of the variables that determine its value are
	     read-only, but we don't bother.  */
	  if (TREE_CONSTANT (value) || TREE_READONLY_DECL_P (value))
	    {
	      /* If this is a declaration, wrap it a NOP_EXPR so that
		 we don't try to put the VALUE on the list of
		 BLOCK_VARS.  */
	      if (DECL_P (value))
		value = build1 (NOP_EXPR, TREE_TYPE (value), value);

	      /* If this is a constant, make sure it has the right type.  */
	      else if (TREE_TYPE (value) != TREE_TYPE (p))
		value = fold (build1 (NOP_EXPR, TREE_TYPE (p), value));

	      splay_tree_insert (id->decl_map,
				 (splay_tree_key) p,
				 (splay_tree_value) value);
	      continue;
	    }
	}

      /* Make an equivalent VAR_DECL.  */
      var = copy_decl_for_inlining (p, fn, VARRAY_TREE (id->fns, 0));

      /* See if the frontend wants to pass this by invisible reference.  If
	 so, our new VAR_DECL will have REFERENCE_TYPE, and we need to
	 replace uses of the PARM_DECL with dereferences.  */
      if (TREE_TYPE (var) != TREE_TYPE (p)
	  && POINTER_TYPE_P (TREE_TYPE (var))
	  && TREE_TYPE (TREE_TYPE (var)) == TREE_TYPE (p))
	var_sub = build1 (INDIRECT_REF, TREE_TYPE (p), var);
      else
	var_sub = var;

      /* Register the VAR_DECL as the equivalent for the PARM_DECL;
	 that way, when the PARM_DECL is encountered, it will be
	 automatically replaced by the VAR_DECL.  */
      splay_tree_insert (id->decl_map,
			 (splay_tree_key) p,
			 (splay_tree_value) var_sub);

      /* Declare this new variable.  */
#ifndef INLINER_FOR_JAVA
      init_stmt = build_stmt (DECL_STMT, var);
      TREE_CHAIN (init_stmt) = init_stmts;
      init_stmts = init_stmt;
#else /* INLINER_FOR_JAVA */
      TREE_CHAIN (var) = vars;
      vars = var;
#endif /* INLINER_FOR_JAVA */

      /* Initialize this VAR_DECL from the equivalent argument.  If
	 the argument is an object, created via a constructor or copy,
	 this will not result in an extra copy: the TARGET_EXPR
	 representing the argument will be bound to VAR, and the
	 object will be constructed in VAR.  */
      if (! TYPE_NEEDS_CONSTRUCTING (TREE_TYPE (p)))
#ifndef INLINER_FOR_JAVA
	DECL_INITIAL (var) = value;
      else
	{
	  /* Even if P was TREE_READONLY, the new VAR should not be.
	     In the original code, we would have constructed a
	     temporary, and then the function body would have never
	     changed the value of P.  However, now, we will be
	     constructing VAR directly.  The constructor body may
	     change its value multiple times as it is being
	     constructed.  Therefore, it must not be TREE_READONLY;
	     the back-end assumes that TREE_READONLY variable is
	     assigned to only once.  */
	  TREE_READONLY (var) = 0;

	  /* Build a run-time initialization.  */
	  init_stmt = build_stmt (EXPR_STMT,
				  build (INIT_EXPR, TREE_TYPE (p),
					 var, value));
	  /* Add this initialization to the list.  Note that we want the
	     declaration *after* the initialization because we are going
	     to reverse all the initialization statements below.  */
	  TREE_CHAIN (init_stmt) = init_stmts;
	  init_stmts = init_stmt;
	}

      /* See if we need to clean up the declaration.  */
      cleanup = (*lang_hooks.maybe_build_cleanup) (var);
      if (cleanup)
	{
	  tree cleanup_stmt;
	  /* Build the cleanup statement.  */
	  cleanup_stmt = build_stmt (CLEANUP_STMT, var, cleanup);
	  /* Add it to the *front* of the list; the list will be
	     reversed below.  */
	  TREE_CHAIN (cleanup_stmt) = init_stmts;
	  init_stmts = cleanup_stmt;
	}
#else /* INLINER_FOR_JAVA */
	{
	  tree assignment = build (MODIFY_EXPR, TREE_TYPE (p), var, value);
	  init_stmts = add_stmt_to_compound (init_stmts, TREE_TYPE (p),
					     assignment);
	}
      else
	{
	  /* Java objects don't ever need constructing when being
             passed as arguments because only call by reference is
             supported.  */
	  abort ();
	}
#endif /* INLINER_FOR_JAVA */
    }

#ifndef INLINER_FOR_JAVA
  /* Evaluate trailing arguments.  */
  for (; a; a = TREE_CHAIN (a))
    {
      tree init_stmt;
      tree value = TREE_VALUE (a);

      if (! value || ! TREE_SIDE_EFFECTS (value))
	continue;

      init_stmt = build_stmt (EXPR_STMT, value);
      TREE_CHAIN (init_stmt) = init_stmts;
      init_stmts = init_stmt;
    }

  /* The initialization statements have been built up in reverse
     order.  Straighten them out now.  */
  return nreverse (init_stmts);
#else /* INLINER_FOR_JAVA */
  BLOCK_VARS (block) = nreverse (vars);
  return init_stmts;
#endif /* INLINER_FOR_JAVA */
}

/* Declare a return variable to replace the RESULT_DECL for the
   function we are calling.  An appropriate DECL_STMT is returned.
   The USE_STMT is filled in to contain a use of the declaration to
   indicate the return value of the function.  */

#ifndef INLINER_FOR_JAVA
static tree
declare_return_variable (struct inline_data *id, tree return_slot_addr,
			 tree *use_stmt)
#else /* INLINER_FOR_JAVA */
static tree
declare_return_variable (struct inline_data *id, tree return_slot_addr,
			 tree *var)
#endif /* INLINER_FOR_JAVA */
{
  tree fn = VARRAY_TOP_TREE (id->fns);
  tree result = DECL_RESULT (fn);
#ifndef INLINER_FOR_JAVA
  tree var;
#endif /* not INLINER_FOR_JAVA */
  int need_return_decl = 1;

  /* We don't need to do anything for functions that don't return
     anything.  */
  if (!result || VOID_TYPE_P (TREE_TYPE (result)))
    {
#ifndef INLINER_FOR_JAVA
      *use_stmt = NULL_TREE;
#else /* INLINER_FOR_JAVA */
      *var = NULL_TREE;
#endif /* INLINER_FOR_JAVA */
      return NULL_TREE;
    }

#ifndef INLINER_FOR_JAVA
  var = ((*lang_hooks.tree_inlining.copy_res_decl_for_inlining)
	 (result, fn, VARRAY_TREE (id->fns, 0), id->decl_map,
	  &need_return_decl, return_slot_addr));

  /* Register the VAR_DECL as the equivalent for the RESULT_DECL; that
     way, when the RESULT_DECL is encountered, it will be
     automatically replaced by the VAR_DECL.  */
  splay_tree_insert (id->decl_map,
		     (splay_tree_key) result,
		     (splay_tree_value) var);

  /* Build the USE_STMT.  If the return type of the function was
     promoted, convert it back to the expected type.  */
  if (TREE_TYPE (var) == TREE_TYPE (TREE_TYPE (fn)))
    *use_stmt = build_stmt (EXPR_STMT, var);
  else
    *use_stmt = build_stmt (EXPR_STMT,
			    build1 (NOP_EXPR, TREE_TYPE (TREE_TYPE (fn)),
				    var));
  TREE_ADDRESSABLE (*use_stmt) = 1;

  /* Build the declaration statement if FN does not return an
     aggregate.  */
  if (need_return_decl)
    return build_stmt (DECL_STMT, var);
#else /* INLINER_FOR_JAVA */
  *var = ((*lang_hooks.tree_inlining.copy_res_decl_for_inlining)
	 (result, fn, VARRAY_TREE (id->fns, 0), id->decl_map,
	  &need_return_decl, return_slot_addr));

  splay_tree_insert (id->decl_map,
		     (splay_tree_key) result,
		     (splay_tree_value) *var);
  DECL_IGNORED_P (*var) = 1;
  if (need_return_decl)
    return *var;
#endif /* INLINER_FOR_JAVA */
  /* If FN does return an aggregate, there's no need to declare the
     return variable; we're using a variable in our caller's frame.  */
  else
    return NULL_TREE;
}

/* Returns nonzero if a function can be inlined as a tree.  */

bool
tree_inlinable_function_p (tree fn)
{
  return inlinable_function_p (fn);
}

static const char *inline_forbidden_reason;

static tree
inline_forbidden_p_1 (tree *nodep, int *walk_subtrees ATTRIBUTE_UNUSED,
		      void *fnp)
{
  tree node = *nodep;
  tree fn = (tree) fnp;
  tree t;

  switch (TREE_CODE (node))
    {
    case CALL_EXPR:
      /* Refuse to inline alloca call unless user explicitly forced so as
	 this may change program's memory overhead drastically when the
	 function using alloca is called in loop.  In GCC present in
	 SPEC2000 inlining into schedule_block cause it to require 2GB of
	 RAM instead of 256MB.  */
      if (alloca_call_p (node)
	  && !lookup_attribute ("always_inline", DECL_ATTRIBUTES (fn)))
	{
	  inline_forbidden_reason
	    = N_("%Jfunction '%F' can never be inlined because it uses "
		 "alloca (override using the always_inline attribute)");
	  return node;
	}
      t = get_callee_fndecl (node);
      if (! t)
	break;


      /* We cannot inline functions that call setjmp.  */
      if (setjmp_call_p (t))
	{
	  inline_forbidden_reason
	    = N_("%Jfunction '%F' can never be inlined because it uses setjmp");
	  return node;
	}

      if (DECL_BUILT_IN (t))
	switch (DECL_FUNCTION_CODE (t))
	  {
	    /* We cannot inline functions that take a variable number of
	       arguments.  */
	  case BUILT_IN_VA_START:
	  case BUILT_IN_STDARG_START:
	  case BUILT_IN_NEXT_ARG:
	  case BUILT_IN_VA_END:
	    {
	      inline_forbidden_reason
		= N_("%Jfunction '%F' can never be inlined because it "
		     "uses variable argument lists");
	      return node;
	    }
	  case BUILT_IN_LONGJMP:
	    {
	      /* We can't inline functions that call __builtin_longjmp at
		 all.  The non-local goto machinery really requires the
		 destination be in a different function.  If we allow the
		 function calling __builtin_longjmp to be inlined into the
		 function calling __builtin_setjmp, Things will Go Awry.  */
	      /* ??? Need front end help to identify "regular" non-local
		 goto.  */
	      if (DECL_BUILT_IN_CLASS (t) == BUILT_IN_NORMAL)
		{
		  inline_forbidden_reason
		    = N_("%Jfunction '%F' can never be inlined because "
			 "it uses setjmp-longjmp exception handling");
		  return node;
		}
	    }

	  default:
	    break;
	  }
      break;

#ifndef INLINER_FOR_JAVA
    case DECL_STMT:
      /* We cannot inline functions that contain other functions.  */
      if (TREE_CODE (TREE_OPERAND (node, 0)) == FUNCTION_DECL
	  && DECL_INITIAL (TREE_OPERAND (node, 0)))
	{
	  inline_forbidden_reason
	    = N_("%Jfunction '%F' can never be inlined "
		 "because it contains a nested function");
	  return node;
	}
      break;

    case GOTO_STMT:
    case GOTO_EXPR:
      t = TREE_OPERAND (node, 0);

      /* We will not inline a function which uses computed goto.  The
	 addresses of its local labels, which may be tucked into
	 global storage, are of course not constant across
	 instantiations, which causes unexpected behavior.  */
      if (TREE_CODE (t) != LABEL_DECL)
	{
	  inline_forbidden_reason
	    = N_("%Jfunction '%F' can never be inlined "
		 "because it contains a computed goto");
	  return node;
	}

      /* We cannot inline a nested function that jumps to a nonlocal
         label.  */
      if (TREE_CODE (t) == LABEL_DECL && DECL_CONTEXT (t) != fn)
	{
	  inline_forbidden_reason
	    = N_("%Jfunction '%F' can never be inlined "
		 "because it contains a nonlocal goto");
	  return node;
	}

      break;

    case RECORD_TYPE:
    case UNION_TYPE:
      /* We cannot inline a function of the form

	   void F (int i) { struct S { int ar[i]; } s; }

	 Attempting to do so produces a catch-22.
	 If walk_tree examines the TYPE_FIELDS chain of RECORD_TYPE/
	 UNION_TYPE nodes, then it goes into infinite recursion on a
	 structure containing a pointer to its own type.  If it doesn't,
	 then the type node for S doesn't get adjusted properly when
	 F is inlined, and we abort in find_function_data.  */
      for (t = TYPE_FIELDS (node); t; t = TREE_CHAIN (t))
	if (variably_modified_type_p (TREE_TYPE (t)))
	  {
	    inline_forbidden_reason
	      = N_("%Jfunction '%F' can never be inlined "
		   "because it uses variable sized variables");
	    return node;
	  }
#endif
    default:
      break;
    }

  return NULL_TREE;
}

/* Return subexpression representing possible alloca call, if any.  */
static tree
inline_forbidden_p (tree fndecl)
{
  location_t saved_loc = input_location;
  tree ret = walk_tree_without_duplicates
		(&DECL_SAVED_TREE (fndecl), inline_forbidden_p_1, fndecl);
  input_location = saved_loc;
  return ret;
}

/* Returns nonzero if FN is a function that does not have any
   fundamental inline blocking properties.  */

static bool
inlinable_function_p (tree fn)
{
  bool inlinable = true;

  /* If we've already decided this function shouldn't be inlined,
     there's no need to check again.  */
  if (DECL_UNINLINABLE (fn))
    return false;

  /* See if there is any language-specific reason it cannot be
     inlined.  (It is important that this hook be called early because
     in C++ it may result in template instantiation.)
     If the function is not inlinable for language-specific reasons,
     it is left up to the langhook to explain why.  */
  inlinable = !(*lang_hooks.tree_inlining.cannot_inline_tree_fn) (&fn);

  /* If we don't have the function body available, we can't inline it.
     However, this should not be recorded since we also get here for
     forward declared inline functions.  Therefore, return at once.  */
  if (!DECL_SAVED_TREE (fn))
    return false;

  /* If we're not inlining at all, then we cannot inline this function.  */
  else if (!flag_inline_trees)
    inlinable = false;

  /* Only try to inline functions if DECL_INLINE is set.  This should be
     true for all functions declared `inline', and for all other functions
     as well with -finline-functions.

     Don't think of disregarding DECL_INLINE when flag_inline_trees == 2;
     it's the front-end that must set DECL_INLINE in this case, because
     dwarf2out loses if a function that does not have DECL_INLINE set is
     inlined anyway.  That is why we have both DECL_INLINE and
     DECL_DECLARED_INLINE_P.  */
  /* FIXME: When flag_inline_trees dies, the check for flag_unit_at_a_time
	    here should be redundant.  */
  else if (!DECL_INLINE (fn) && !flag_unit_at_a_time)
    inlinable = false;

#ifdef INLINER_FOR_JAVA
  /* Synchronized methods can't be inlined.  This is a bug.  */
  else if (METHOD_SYNCHRONIZED (fn))
    inlinable = false;
#endif /* INLINER_FOR_JAVA */

  else if (inline_forbidden_p (fn))
    {
      /* See if we should warn about uninlinable functions.  Previously,
	 some of these warnings would be issued while trying to expand
	 the function inline, but that would cause multiple warnings
	 about functions that would for example call alloca.  But since
	 this a property of the function, just one warning is enough.
	 As a bonus we can now give more details about the reason why a
	 function is not inlinable.
	 We only warn for functions declared `inline' by the user.  */
      bool do_warning = (warn_inline
			 && DECL_INLINE (fn)
			 && DECL_DECLARED_INLINE_P (fn)
			 && !DECL_IN_SYSTEM_HEADER (fn));

      if (lookup_attribute ("always_inline",
			    DECL_ATTRIBUTES (fn)))
	sorry (inline_forbidden_reason, fn, fn);
      else if (do_warning)
	warning (inline_forbidden_reason, fn, fn);

      inlinable = false;
    }

  /* Squirrel away the result so that we don't have to check again.  */
  DECL_UNINLINABLE (fn) = !inlinable;

  return inlinable;
}

/* If *TP is a CALL_EXPR, replace it with its inline expansion.  */

static tree
expand_call_inline (tree *tp, int *walk_subtrees, void *data)
{
  inline_data *id;
  tree t;
  tree expr;
  tree stmt;
#ifndef INLINER_FOR_JAVA
  tree chain;
  tree scope_stmt;
  tree use_stmt;
#else /* INLINER_FOR_JAVA */
  tree retvar;
#endif /* INLINER_FOR_JAVA */
  tree fn;
  tree arg_inits;
  tree *inlined_body;
  splay_tree st;
  tree args;
  tree return_slot_addr;
  const char *reason;

  /* See what we've got.  */
  id = (inline_data *) data;
  t = *tp;

  /* Recurse, but letting recursive invocations know that we are
     inside the body of a TARGET_EXPR.  */
  if (TREE_CODE (*tp) == TARGET_EXPR)
    {
#ifndef INLINER_FOR_JAVA
      int i, len = first_rtl_op (TARGET_EXPR);

      /* We're walking our own subtrees.  */
      *walk_subtrees = 0;

      /* Actually walk over them.  This loop is the body of
	 walk_trees, omitting the case where the TARGET_EXPR
	 itself is handled.  */
      for (i = 0; i < len; ++i)
	{
	  if (i == 2)
	    ++id->in_target_cleanup_p;
	  walk_tree (&TREE_OPERAND (*tp, i), expand_call_inline, data,
		     id->tree_pruner);
	  if (i == 2)
	    --id->in_target_cleanup_p;
	}

      return NULL_TREE;
#else /* INLINER_FOR_JAVA */
      abort ();
#endif /* INLINER_FOR_JAVA */
    }
  else if (TREE_CODE (t) == EXPR_WITH_FILE_LOCATION)
    {
      /* We're walking the subtree directly.  */
      *walk_subtrees = 0;
      /* Update the source position.  */
      push_srcloc (EXPR_WFL_FILENAME (t), EXPR_WFL_LINENO (t));
      walk_tree (&EXPR_WFL_NODE (t), expand_call_inline, data,
		 id->tree_pruner);
      /* Restore the original source position.  */
      pop_srcloc ();

      return NULL_TREE;
    }

  if (TYPE_P (t))
    /* Because types were not copied in copy_body, CALL_EXPRs beneath
       them should not be expanded.  This can happen if the type is a
       dynamic array type, for example.  */
    *walk_subtrees = 0;

  /* From here on, we're only interested in CALL_EXPRs.  */
  if (TREE_CODE (t) != CALL_EXPR)
    return NULL_TREE;

  /* First, see if we can figure out what function is being called.
     If we cannot, then there is no hope of inlining the function.  */
  fn = get_callee_fndecl (t);
  if (!fn)
    return NULL_TREE;

  /* Turn forward declarations into real ones.  */
  fn = cgraph_node (fn)->decl;

  /* If fn is a declaration of a function in a nested scope that was
     globally declared inline, we don't set its DECL_INITIAL.
     However, we can't blindly follow DECL_ABSTRACT_ORIGIN because the
     C++ front-end uses it for cdtors to refer to their internal
     declarations, that are not real functions.  Fortunately those
     don't have trees to be saved, so we can tell by checking their
     DECL_SAVED_TREE.  */
  if (! DECL_INITIAL (fn)
      && DECL_ABSTRACT_ORIGIN (fn)
      && DECL_SAVED_TREE (DECL_ABSTRACT_ORIGIN (fn)))
    fn = DECL_ABSTRACT_ORIGIN (fn);

  /* Don't try to inline functions that are not well-suited to
     inlining.  */
  if (!cgraph_inline_p (id->current_decl, fn, &reason))
    {
      if (lookup_attribute ("always_inline", DECL_ATTRIBUTES (fn)))
	{
	  sorry ("%Jinlining failed in call to '%F': %s", fn, fn, reason);
	  sorry ("called from here");
	}
      else if (warn_inline && DECL_DECLARED_INLINE_P (fn)
	       && !DECL_IN_SYSTEM_HEADER (fn)
	       && strlen (reason)
	       && !lookup_attribute ("noinline", DECL_ATTRIBUTES (fn)))
	{
	  warning ("%Jinlining failed in call to '%F': %s", fn, fn, reason);
	  warning ("called from here");
	}
      return NULL_TREE;
    }

  if (! (*lang_hooks.tree_inlining.start_inlining) (fn))
    return NULL_TREE;

  /* Set the current filename and line number to the function we are
     inlining so that when we create new _STMT nodes here they get
     line numbers corresponding to the function we are calling.  We
     wrap the whole inlined body in an EXPR_WITH_FILE_AND_LINE as well
     because individual statements don't record the filename.  */
  push_srcloc (DECL_SOURCE_FILE (fn), DECL_SOURCE_LINE (fn));

#ifndef INLINER_FOR_JAVA
  /* Build a statement-expression containing code to initialize the
     arguments, the actual inline expansion of the body, and a label
     for the return statements within the function to jump to.  The
     type of the statement expression is the return type of the
     function call.  */
  expr = build1 (STMT_EXPR, TREE_TYPE (TREE_TYPE (fn)), make_node (COMPOUND_STMT));
  /* There is no scope associated with the statement-expression.  */
  STMT_EXPR_NO_SCOPE (expr) = 1;
  if (lookup_attribute ("warn_unused_result",
			TYPE_ATTRIBUTES (TREE_TYPE (fn))))
    STMT_EXPR_WARN_UNUSED_RESULT (expr) = 1;
  stmt = STMT_EXPR_STMT (expr);
#else /* INLINER_FOR_JAVA */
  /* Build a block containing code to initialize the arguments, the
     actual inline expansion of the body, and a label for the return
     statements within the function to jump to.  The type of the
     statement expression is the return type of the function call.  */
  stmt = NULL;
  expr = build (BLOCK, TREE_TYPE (TREE_TYPE (fn)), stmt);
#endif /* INLINER_FOR_JAVA */

  /* Local declarations will be replaced by their equivalents in this
     map.  */
  st = id->decl_map;
  id->decl_map = splay_tree_new (splay_tree_compare_pointers,
				 NULL, NULL);

  /* Initialize the parameters.  */
  args = TREE_OPERAND (t, 1);
  return_slot_addr = NULL_TREE;
  if (CALL_EXPR_HAS_RETURN_SLOT_ADDR (t))
    {
      return_slot_addr = TREE_VALUE (args);
      args = TREE_CHAIN (args);
    }

#ifndef INLINER_FOR_JAVA
  arg_inits = initialize_inlined_parameters (id, args, fn);
  /* Expand any inlined calls in the initializers.  Do this before we
     push FN on the stack of functions we are inlining; we want to
     inline calls to FN that appear in the initializers for the
     parameters.  */
  expand_calls_inline (&arg_inits, id);
  /* And add them to the tree.  */
  COMPOUND_BODY (stmt) = chainon (COMPOUND_BODY (stmt), arg_inits);
#else /* INLINER_FOR_JAVA */
  arg_inits = initialize_inlined_parameters (id, args, fn, expr);
  if (arg_inits)
    {
      /* Expand any inlined calls in the initializers.  Do this before we
	 push FN on the stack of functions we are inlining; we want to
	 inline calls to FN that appear in the initializers for the
	 parameters.  */
      expand_calls_inline (&arg_inits, id);

      /* And add them to the tree.  */
      BLOCK_EXPR_BODY (expr) = add_stmt_to_compound (BLOCK_EXPR_BODY (expr),
						     TREE_TYPE (arg_inits),
						     arg_inits);
    }
#endif /* INLINER_FOR_JAVA */

  /* Record the function we are about to inline so that we can avoid
     recursing into it.  */
  VARRAY_PUSH_TREE (id->fns, fn);

  /* Record the function we are about to inline if optimize_function
     has not been called on it yet and we don't have it in the list.  */
  if (! DECL_INLINED_FNS (fn))
    {
      int i;

      for (i = VARRAY_ACTIVE_SIZE (id->inlined_fns) - 1; i >= 0; i--)
	if (VARRAY_TREE (id->inlined_fns, i) == fn)
	  break;
      if (i < 0)
	VARRAY_PUSH_TREE (id->inlined_fns, fn);
    }

  /* Return statements in the function body will be replaced by jumps
     to the RET_LABEL.  */
  id->ret_label = build_decl (LABEL_DECL, NULL_TREE, NULL_TREE);
  DECL_CONTEXT (id->ret_label) = VARRAY_TREE (id->fns, 0);

  if (! DECL_INITIAL (fn)
      || TREE_CODE (DECL_INITIAL (fn)) != BLOCK)
    abort ();

#ifndef INLINER_FOR_JAVA
  /* Create a block to put the parameters in.  We have to do this
     after the parameters have been remapped because remapping
     parameters is different from remapping ordinary variables.  */
  scope_stmt = build_stmt (SCOPE_STMT, DECL_INITIAL (fn));
  SCOPE_BEGIN_P (scope_stmt) = 1;
  SCOPE_NO_CLEANUPS_P (scope_stmt) = 1;
  remap_block (scope_stmt, DECL_ARGUMENTS (fn), id);
  TREE_CHAIN (scope_stmt) = COMPOUND_BODY (stmt);
  COMPOUND_BODY (stmt) = scope_stmt;

  /* Tell the debugging backends that this block represents the
     outermost scope of the inlined function.  */
  if (SCOPE_STMT_BLOCK (scope_stmt))
    BLOCK_ABSTRACT_ORIGIN (SCOPE_STMT_BLOCK (scope_stmt)) = DECL_ORIGIN (fn);

  /* Declare the return variable for the function.  */
  COMPOUND_BODY (stmt)
    = chainon (COMPOUND_BODY (stmt),
	       declare_return_variable (id, return_slot_addr, &use_stmt));
#else /* INLINER_FOR_JAVA */
  {
    /* Declare the return variable for the function.  */
    tree decl = declare_return_variable (id, return_slot_addr, &retvar);
    if (retvar)
      {
	tree *next = &BLOCK_VARS (expr);
	while (*next)
	  next = &TREE_CHAIN (*next);
	*next = decl;
      }
  }
#endif /* INLINER_FOR_JAVA */

  /* After we've initialized the parameters, we insert the body of the
     function itself.  */
#ifndef INLINER_FOR_JAVA
  inlined_body = &COMPOUND_BODY (stmt);
  while (*inlined_body)
    inlined_body = &TREE_CHAIN (*inlined_body);
  *inlined_body = copy_body (id);
#else /* INLINER_FOR_JAVA */
  {
    tree new_body;
    java_inlining_map_static_initializers (fn, id->decl_map);
    new_body = copy_body (id);
    TREE_TYPE (new_body) = TREE_TYPE (TREE_TYPE (fn));
    BLOCK_EXPR_BODY (expr)
      = add_stmt_to_compound (BLOCK_EXPR_BODY (expr),
			      TREE_TYPE (new_body), new_body);
    inlined_body = &BLOCK_EXPR_BODY (expr);
  }
#endif /* INLINER_FOR_JAVA */

  /* After the body of the function comes the RET_LABEL.  This must come
     before we evaluate the returned value below, because that evaluation
     may cause RTL to be generated.  */
#ifndef INLINER_FOR_JAVA
  COMPOUND_BODY (stmt)
    = chainon (COMPOUND_BODY (stmt),
	       build_stmt (LABEL_STMT, id->ret_label));
#else /* INLINER_FOR_JAVA */
  {
    tree label = build1 (LABEL_EXPR, void_type_node, id->ret_label);
    BLOCK_EXPR_BODY (expr)
      = add_stmt_to_compound (BLOCK_EXPR_BODY (expr), void_type_node, label);
    TREE_SIDE_EFFECTS (label) = TREE_SIDE_EFFECTS (t);
  }
#endif /* INLINER_FOR_JAVA */

  /* Finally, mention the returned value so that the value of the
     statement-expression is the returned value of the function.  */
#ifndef INLINER_FOR_JAVA
  COMPOUND_BODY (stmt) = chainon (COMPOUND_BODY (stmt), use_stmt);

  /* Close the block for the parameters.  */
  scope_stmt = build_stmt (SCOPE_STMT, DECL_INITIAL (fn));
  SCOPE_NO_CLEANUPS_P (scope_stmt) = 1;
  remap_block (scope_stmt, NULL_TREE, id);
  COMPOUND_BODY (stmt)
    = chainon (COMPOUND_BODY (stmt), scope_stmt);
#else /* INLINER_FOR_JAVA */
  if (retvar)
    {
      /* Mention the retvar.  If the return type of the function was
	 promoted, convert it back to the expected type.  */
      if (TREE_TYPE (TREE_TYPE (fn)) != TREE_TYPE (retvar))
	retvar = build1 (NOP_EXPR, TREE_TYPE (TREE_TYPE (fn)), retvar);
      BLOCK_EXPR_BODY (expr)
	= add_stmt_to_compound (BLOCK_EXPR_BODY (expr),
				TREE_TYPE (retvar), retvar);
    }

  java_inlining_merge_static_initializers (fn, id->decl_map);
#endif /* INLINER_FOR_JAVA */

  /* Clean up.  */
  splay_tree_delete (id->decl_map);
  id->decl_map = st;

  /* Although, from the semantic viewpoint, the new expression has
     side-effects only if the old one did, it is not possible, from
     the technical viewpoint, to evaluate the body of a function
     multiple times without serious havoc.  */
  TREE_SIDE_EFFECTS (expr) = 1;

  /* Replace the call by the inlined body.  Wrap it in an
     EXPR_WITH_FILE_LOCATION so that we'll get debugging line notes
     pointing to the right place.  */
#ifndef INLINER_FOR_JAVA
  chain = TREE_CHAIN (*tp);
#endif /* INLINER_FOR_JAVA */
  *tp = build_expr_wfl (expr, DECL_SOURCE_FILE (fn), DECL_SOURCE_LINE (fn),
			/*col=*/0);
  EXPR_WFL_EMIT_LINE_NOTE (*tp) = 1;
#ifndef INLINER_FOR_JAVA
  TREE_CHAIN (*tp) = chain;
#endif /* not INLINER_FOR_JAVA */
  pop_srcloc ();

  /* If the value of the new expression is ignored, that's OK.  We
     don't warn about this for CALL_EXPRs, so we shouldn't warn about
     the equivalent inlined version either.  */
  TREE_USED (*tp) = 1;

  /* Update callgraph if needed.  */
  if (id->decl)
    {
      cgraph_remove_call (id->decl, fn);
      cgraph_create_edges (id->decl, *inlined_body);
    }

  /* Recurse into the body of the just inlined function.  */
  {
    tree old_decl = id->current_decl;
    id->current_decl = fn;
    expand_calls_inline (inlined_body, id);
    id->current_decl = old_decl;
  }
  VARRAY_POP (id->fns);

  /* Don't walk into subtrees.  We've already handled them above.  */
  *walk_subtrees = 0;

  (*lang_hooks.tree_inlining.end_inlining) (fn);

  /* Keep iterating.  */
  return NULL_TREE;
}
/* Walk over the entire tree *TP, replacing CALL_EXPRs with inline
   expansions as appropriate.  */

static void
expand_calls_inline (tree *tp, inline_data *id)
{
  /* Search through *TP, replacing all calls to inline functions by
     appropriate equivalents.  Use walk_tree in no-duplicates mode
     to avoid exponential time complexity.  (We can't just use
     walk_tree_without_duplicates, because of the special TARGET_EXPR
     handling in expand_calls.  The hash table is set up in
     optimize_function.  */
  walk_tree (tp, expand_call_inline, id, id->tree_pruner);
}

/* Expand calls to inline functions in the body of FN.  */

void
optimize_inline_calls (tree fn)
{
  inline_data id;
  tree prev_fn;

  /* There is no point in performing inlining if errors have already
     occurred -- and we might crash if we try to inline invalid
     code.  */
  if (errorcount || sorrycount)
    return;

  /* Clear out ID.  */
  memset (&id, 0, sizeof (id));

  id.decl = fn;
  id.current_decl = fn;
  /* Don't allow recursion into FN.  */
  VARRAY_TREE_INIT (id.fns, 32, "fns");
  VARRAY_PUSH_TREE (id.fns, fn);
  /* Or any functions that aren't finished yet.  */
  prev_fn = NULL_TREE;
  if (current_function_decl)
    {
      VARRAY_PUSH_TREE (id.fns, current_function_decl);
      prev_fn = current_function_decl;
    }

  prev_fn = ((*lang_hooks.tree_inlining.add_pending_fn_decls)
	     (&id.fns, prev_fn));

  /* Create the list of functions this call will inline.  */
  VARRAY_TREE_INIT (id.inlined_fns, 32, "inlined_fns");

  /* Keep track of the low-water mark, i.e., the point where the first
     real inlining is represented in ID.FNS.  */
  id.first_inlined_fn = VARRAY_ACTIVE_SIZE (id.fns);

  /* Replace all calls to inline functions with the bodies of those
     functions.  */
  id.tree_pruner = htab_create (37, htab_hash_pointer,
				htab_eq_pointer, NULL);
  expand_calls_inline (&DECL_SAVED_TREE (fn), &id);

  /* Clean up.  */
  htab_delete (id.tree_pruner);
  if (DECL_LANG_SPECIFIC (fn))
    {
      tree ifn = make_tree_vec (VARRAY_ACTIVE_SIZE (id.inlined_fns));

      if (VARRAY_ACTIVE_SIZE (id.inlined_fns))
	memcpy (&TREE_VEC_ELT (ifn, 0), &VARRAY_TREE (id.inlined_fns, 0),
		VARRAY_ACTIVE_SIZE (id.inlined_fns) * sizeof (tree));
      DECL_INLINED_FNS (fn) = ifn;
    }
}

/* FN is a function that has a complete body, and CLONE is a function
   whose body is to be set to a copy of FN, mapping argument
   declarations according to the ARG_MAP splay_tree.  */

void
clone_body (tree clone, tree fn, void *arg_map)
{
  inline_data id;

  /* Clone the body, as if we were making an inline call.  But, remap
     the parameters in the callee to the parameters of caller.  If
     there's an in-charge parameter, map it to an appropriate
     constant.  */
  memset (&id, 0, sizeof (id));
  VARRAY_TREE_INIT (id.fns, 2, "fns");
  VARRAY_PUSH_TREE (id.fns, clone);
  VARRAY_PUSH_TREE (id.fns, fn);
  id.decl_map = (splay_tree)arg_map;

  /* Cloning is treated slightly differently from inlining.  Set
     CLONING_P so that it's clear which operation we're performing.  */
  id.cloning_p = true;

  /* Actually copy the body.  */
  TREE_CHAIN (DECL_SAVED_TREE (clone)) = copy_body (&id);
}

/* Apply FUNC to all the sub-trees of TP in a pre-order traversal.
   FUNC is called with the DATA and the address of each sub-tree.  If
   FUNC returns a non-NULL value, the traversal is aborted, and the
   value returned by FUNC is returned.  If HTAB is non-NULL it is used
   to record the nodes visited, and to avoid visiting a node more than
   once.  */

tree
walk_tree (tree *tp, walk_tree_fn func, void *data, void *htab_)
{
  htab_t htab = (htab_t) htab_;
  enum tree_code code;
  int walk_subtrees;
  tree result;

#define WALK_SUBTREE(NODE)				\
  do							\
    {							\
      result = walk_tree (&(NODE), func, data, htab);	\
      if (result)					\
	return result;					\
    }							\
  while (0)

#define WALK_SUBTREE_TAIL(NODE)				\
  do							\
    {							\
       tp = & (NODE);					\
       goto tail_recurse;				\
    }							\
  while (0)

 tail_recurse:
  /* Skip empty subtrees.  */
  if (!*tp)
    return NULL_TREE;

  if (htab)
    {
      void **slot;

      /* Don't walk the same tree twice, if the user has requested
         that we avoid doing so.  */
      slot = htab_find_slot (htab, *tp, INSERT);
      if (*slot)
	return NULL_TREE;
      *slot = *tp;
    }

  /* Call the function.  */
  walk_subtrees = 1;
  result = (*func) (tp, &walk_subtrees, data);

  /* If we found something, return it.  */
  if (result)
    return result;

  code = TREE_CODE (*tp);

#ifndef INLINER_FOR_JAVA
  /* Even if we didn't, FUNC may have decided that there was nothing
     interesting below this point in the tree.  */
  if (!walk_subtrees)
    {
      if (STATEMENT_CODE_P (code) || code == TREE_LIST
	  || (*lang_hooks.tree_inlining.tree_chain_matters_p) (*tp))
	/* But we still need to check our siblings.  */
	WALK_SUBTREE_TAIL (TREE_CHAIN (*tp));
      else
	return NULL_TREE;
    }

  /* Handle common cases up front.  */
  if (IS_EXPR_CODE_CLASS (TREE_CODE_CLASS (code)))
#else /* INLINER_FOR_JAVA */
  if (code != EXIT_BLOCK_EXPR
      && code != SAVE_EXPR
      && IS_EXPR_CODE_CLASS (TREE_CODE_CLASS (code)))
#endif /* INLINER_FOR_JAVA */
    {
      int i, len;

#ifndef INLINER_FOR_JAVA
      /* Set lineno here so we get the right instantiation context
	 if we call instantiate_decl from inlinable_function_p.  */
      if (STATEMENT_CODE_P (code) && !STMT_LINENO_FOR_FN_P (*tp))
	input_line = STMT_LINENO (*tp);
#endif /* not INLINER_FOR_JAVA */

      /* Walk over all the sub-trees of this operand.  */
      len = first_rtl_op (code);
      /* TARGET_EXPRs are peculiar: operands 1 and 3 can be the same.
	 But, we only want to walk once.  */
      if (code == TARGET_EXPR
	  && TREE_OPERAND (*tp, 3) == TREE_OPERAND (*tp, 1))
	--len;
      /* Go through the subtrees.  We need to do this in forward order so
         that the scope of a FOR_EXPR is handled properly.  */
      for (i = 0; i < len; ++i)
	WALK_SUBTREE (TREE_OPERAND (*tp, i));

#ifndef INLINER_FOR_JAVA
      /* For statements, we also walk the chain so that we cover the
	 entire statement tree.  */
      if (STATEMENT_CODE_P (code))
	{
	  if (code == DECL_STMT
	      && DECL_STMT_DECL (*tp)
	      && DECL_P (DECL_STMT_DECL (*tp)))
	    {
	      /* Walk the DECL_INITIAL and DECL_SIZE.  We don't want to walk
		 into declarations that are just mentioned, rather than
		 declared; they don't really belong to this part of the tree.
		 And, we can see cycles: the initializer for a declaration can
		 refer to the declaration itself.  */
	      WALK_SUBTREE (DECL_INITIAL (DECL_STMT_DECL (*tp)));
	      WALK_SUBTREE (DECL_SIZE (DECL_STMT_DECL (*tp)));
	      WALK_SUBTREE (DECL_SIZE_UNIT (DECL_STMT_DECL (*tp)));
	      WALK_SUBTREE (TREE_TYPE (*tp));
	    }

	  /* This can be tail-recursion optimized if we write it this way.  */
	  WALK_SUBTREE_TAIL (TREE_CHAIN (*tp));
	}

#endif /* not INLINER_FOR_JAVA */
      /* We didn't find what we were looking for.  */
      return NULL_TREE;
    }
  else if (TREE_CODE_CLASS (code) == 'd')
    {
      WALK_SUBTREE_TAIL (TREE_TYPE (*tp));
    }
  else if (TREE_CODE_CLASS (code) == 't')
    {
      WALK_SUBTREE (TYPE_SIZE (*tp));
      WALK_SUBTREE (TYPE_SIZE_UNIT (*tp));
      /* Also examine various special fields, below.  */
    }

  result = (*lang_hooks.tree_inlining.walk_subtrees) (tp, &walk_subtrees, func,
						      data, htab);
  if (result || ! walk_subtrees)
    return result;

  /* Not one of the easy cases.  We must explicitly go through the
     children.  */
  switch (code)
    {
    case ERROR_MARK:
    case IDENTIFIER_NODE:
    case INTEGER_CST:
    case REAL_CST:
    case VECTOR_CST:
    case STRING_CST:
    case REAL_TYPE:
    case COMPLEX_TYPE:
    case VECTOR_TYPE:
    case VOID_TYPE:
    case BOOLEAN_TYPE:
    case UNION_TYPE:
    case ENUMERAL_TYPE:
    case BLOCK:
    case RECORD_TYPE:
    case CHAR_TYPE:
    case PLACEHOLDER_EXPR:
      /* None of these have subtrees other than those already walked
         above.  */
      break;

    case POINTER_TYPE:
    case REFERENCE_TYPE:
      WALK_SUBTREE_TAIL (TREE_TYPE (*tp));
      break;

    case TREE_LIST:
      WALK_SUBTREE (TREE_VALUE (*tp));
      WALK_SUBTREE_TAIL (TREE_CHAIN (*tp));
      break;

    case TREE_VEC:
      {
	int len = TREE_VEC_LENGTH (*tp);

	if (len == 0)
	  break;

	/* Walk all elements but the first.  */
	while (--len)
	  WALK_SUBTREE (TREE_VEC_ELT (*tp, len));

	/* Now walk the first one as a tail call.  */
	WALK_SUBTREE_TAIL (TREE_VEC_ELT (*tp, 0));
      }

    case COMPLEX_CST:
      WALK_SUBTREE (TREE_REALPART (*tp));
      WALK_SUBTREE_TAIL (TREE_IMAGPART (*tp));

    case CONSTRUCTOR:
      WALK_SUBTREE_TAIL (CONSTRUCTOR_ELTS (*tp));

    case METHOD_TYPE:
      WALK_SUBTREE (TYPE_METHOD_BASETYPE (*tp));
      /* Fall through.  */

    case FUNCTION_TYPE:
      WALK_SUBTREE (TREE_TYPE (*tp));
      {
	tree arg = TYPE_ARG_TYPES (*tp);

	/* We never want to walk into default arguments.  */
	for (; arg; arg = TREE_CHAIN (arg))
	  WALK_SUBTREE (TREE_VALUE (arg));
      }
      break;

    case ARRAY_TYPE:
      WALK_SUBTREE (TREE_TYPE (*tp));
      WALK_SUBTREE_TAIL (TYPE_DOMAIN (*tp));

    case INTEGER_TYPE:
      WALK_SUBTREE (TYPE_MIN_VALUE (*tp));
      WALK_SUBTREE_TAIL (TYPE_MAX_VALUE (*tp));

    case OFFSET_TYPE:
      WALK_SUBTREE (TREE_TYPE (*tp));
      WALK_SUBTREE_TAIL (TYPE_OFFSET_BASETYPE (*tp));

#ifdef INLINER_FOR_JAVA
    case EXIT_BLOCK_EXPR:
      WALK_SUBTREE_TAIL (TREE_OPERAND (*tp, 1));

    case SAVE_EXPR:
      WALK_SUBTREE_TAIL (TREE_OPERAND (*tp, 0));
#endif /* INLINER_FOR_JAVA */

    default:
      abort ();
    }

  /* We didn't find what we were looking for.  */
  return NULL_TREE;

#undef WALK_SUBTREE
#undef WALK_SUBTREE_TAIL
}

/* Like walk_tree, but does not walk duplicate nodes more than
   once.  */

tree
walk_tree_without_duplicates (tree *tp, walk_tree_fn func, void *data)
{
  tree result;
  htab_t htab;

  htab = htab_create (37, htab_hash_pointer, htab_eq_pointer, NULL);
  result = walk_tree (tp, func, data, htab);
  htab_delete (htab);
  return result;
}

/* Passed to walk_tree.  Copies the node pointed to, if appropriate.  */

tree
copy_tree_r (tree *tp, int *walk_subtrees, void *data ATTRIBUTE_UNUSED)
{
  enum tree_code code = TREE_CODE (*tp);

  /* We make copies of most nodes.  */
  if (IS_EXPR_CODE_CLASS (TREE_CODE_CLASS (code))
      || TREE_CODE_CLASS (code) == 'c'
      || code == TREE_LIST
      || code == TREE_VEC
      || (*lang_hooks.tree_inlining.tree_chain_matters_p) (*tp))
    {
      /* Because the chain gets clobbered when we make a copy, we save it
	 here.  */
      tree chain = TREE_CHAIN (*tp);

      /* Copy the node.  */
      *tp = copy_node (*tp);

      /* Now, restore the chain, if appropriate.  That will cause
	 walk_tree to walk into the chain as well.  */
      if (code == PARM_DECL || code == TREE_LIST
#ifndef INLINER_FOR_JAVA
	  || (*lang_hooks.tree_inlining.tree_chain_matters_p) (*tp)
	  || STATEMENT_CODE_P (code))
	TREE_CHAIN (*tp) = chain;

      /* For now, we don't update BLOCKs when we make copies.  So, we
	 have to nullify all scope-statements.  */
      if (TREE_CODE (*tp) == SCOPE_STMT)
	SCOPE_STMT_BLOCK (*tp) = NULL_TREE;
#else /* INLINER_FOR_JAVA */
	  || (*lang_hooks.tree_inlining.tree_chain_matters_p) (*tp))
	TREE_CHAIN (*tp) = chain;
#endif /* INLINER_FOR_JAVA */
    }
  else if (TREE_CODE_CLASS (code) == 't')
    *walk_subtrees = 0;

  return NULL_TREE;
}

/* The SAVE_EXPR pointed to by TP is being copied.  If ST contains
   information indicating to what new SAVE_EXPR this one should be
   mapped, use that one.  Otherwise, create a new node and enter it in
   ST.  FN is the function into which the copy will be placed.  */

void
remap_save_expr (tree *tp, void *st_, tree fn, int *walk_subtrees)
{
  splay_tree st = (splay_tree) st_;
  splay_tree_node n;

  /* See if we already encountered this SAVE_EXPR.  */
  n = splay_tree_lookup (st, (splay_tree_key) *tp);

  /* If we didn't already remap this SAVE_EXPR, do so now.  */
  if (!n)
    {
      tree t = copy_node (*tp);

      /* The SAVE_EXPR is now part of the function into which we
	 are inlining this body.  */
      SAVE_EXPR_CONTEXT (t) = fn;
      /* And we haven't evaluated it yet.  */
      SAVE_EXPR_RTL (t) = NULL_RTX;
      /* Remember this SAVE_EXPR.  */
      n = splay_tree_insert (st,
			     (splay_tree_key) *tp,
			     (splay_tree_value) t);
      /* Make sure we don't remap an already-remapped SAVE_EXPR.  */
      splay_tree_insert (st, (splay_tree_key) t, (splay_tree_value) t);
    }
  else
    /* We've already walked into this SAVE_EXPR, so we needn't do it
       again.  */
    *walk_subtrees = 0;

  /* Replace this SAVE_EXPR with the copy.  */
  *tp = (tree) n->value;
}

#ifdef INLINER_FOR_JAVA
/* Add STMT to EXISTING if possible, otherwise create a new
   COMPOUND_EXPR and add STMT to it.  */

static tree
add_stmt_to_compound (tree existing, tree type, tree stmt)
{
  if (!stmt)
    return existing;
  else if (existing)
    return build (COMPOUND_EXPR, type, existing, stmt);
  else
    return stmt;
}

#endif /* INLINER_FOR_JAVA */
