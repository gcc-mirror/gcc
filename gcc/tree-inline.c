/* Tree inlining.
   Copyright 2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.
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
#include "pointer-set.h"
#include "splay-tree.h"
#include "langhooks.h"
#include "cgraph.h"
#include "intl.h"
#include "tree-mudflap.h"
#include "tree-flow.h"
#include "function.h"
#include "diagnostic.h"
#include "debug.h"

/* I'm not real happy about this, but we need to handle gimple and
   non-gimple trees.  */
#include "tree-iterator.h"
#include "tree-gimple.h"

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
  /* The VAR_DECL for the return value.  */
  tree retvar;
  /* The map from local declarations in the inlined function to
     equivalents in the function into which it is being inlined.  */
  splay_tree decl_map;
  /* Nonzero if we are currently within the cleanup for a
     TARGET_EXPR.  */
  int in_target_cleanup_p;
  /* We use the same mechanism to build clones that we do to perform
     inlining.  However, there are a few places where we need to
     distinguish between those two situations.  This flag is true if
     we are cloning, rather than inlining.  */
  bool cloning_p;
  /* Similarly for saving function body.  */
  bool saving_p;
  /* Hash table used to prevent walk_tree from visiting the same node
     umpteen million times.  */
  htab_t tree_pruner;
  /* Callgraph node of function we are inlining into.  */
  struct cgraph_node *node;
  /* Callgraph node of currently inlined function.  */
  struct cgraph_node *current_node;
  /* Statement iterator.  We need this so we can keep the tree in
     gimple form when we insert the inlined function.   It is not
     used when we are not dealing with gimple trees.  */
  tree_stmt_iterator tsi;
} inline_data;

/* Prototypes.  */

/* The approximate number of instructions per statement.  This number
   need not be particularly accurate; it is used only to make
   decisions about when a function is too big to inline.  */
#define INSNS_PER_STMT (10)

static tree copy_body_r (tree *, int *, void *);
static tree copy_body (inline_data *);
static tree expand_call_inline (tree *, int *, void *);
static void expand_calls_inline (tree *, inline_data *);
static bool inlinable_function_p (tree);
static tree remap_decl (tree, inline_data *);
static tree remap_type (tree, inline_data *);
static tree initialize_inlined_parameters (inline_data *, tree,
					   tree, tree, tree);
static void remap_block (tree *, inline_data *);
static tree remap_decls (tree, inline_data *);
static void copy_bind_expr (tree *, int *, inline_data *);
static tree mark_local_for_remap_r (tree *, int *, void *);
static void unsave_expr_1 (tree);
static tree unsave_r (tree *, int *, void *);
static void declare_inline_vars (tree bind_expr, tree vars);
static void remap_save_expr (tree *, void *, int *);

/* Insert a tree->tree mapping for ID.  Despite the name suggests
   that the trees should be variables, it is used for more than that.  */

static void
insert_decl_map (inline_data *id, tree key, tree value)
{
  splay_tree_insert (id->decl_map, (splay_tree_key) key,
		     (splay_tree_value) value);

  /* Always insert an identity map as well.  If we see this same new
     node again, we won't want to duplicate it a second time.  */
  if (key != value)
    splay_tree_insert (id->decl_map, (splay_tree_key) value,
		       (splay_tree_value) value);
}

/* Remap DECL during the copying of the BLOCK tree for the function.
   We are only called to remap local variables in the current function.  */

static tree
remap_decl (tree decl, inline_data *id)
{
  splay_tree_node n = splay_tree_lookup (id->decl_map, (splay_tree_key) decl);
  tree fn = VARRAY_TOP_TREE (id->fns);

  /* See if we have remapped this declaration.  If we didn't already have an
     equivalent for this declaration, create one now.  */
  if (!n)
    {
      /* Make a copy of the variable or label.  */
      tree t = copy_decl_for_inlining (decl, fn, VARRAY_TREE (id->fns, 0));

      /* Remember it, so that if we encounter this local entity again
	 we can reuse this copy.  Do this early because remap_type may
	 need this decl for TYPE_STUB_DECL.  */
      insert_decl_map (id, decl, t);

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

      /* If fields, do likewise for offset and qualifier.  */
      if (TREE_CODE (t) == FIELD_DECL)
	{
	  walk_tree (&DECL_FIELD_OFFSET (t), copy_body_r, id, NULL);
	  if (TREE_CODE (DECL_CONTEXT (t)) == QUAL_UNION_TYPE)
	    walk_tree (&DECL_QUALIFIER (t), copy_body_r, id, NULL);
	}

#if 0
      /* FIXME handle anon aggrs.  */
      if (! DECL_NAME (t) && TREE_TYPE (t)
	  && lang_hooks.tree_inlining.anon_aggr_type_p (TREE_TYPE (t)))
	{
	  /* For a VAR_DECL of anonymous type, we must also copy the
	     member VAR_DECLS here and rechain the DECL_ANON_UNION_ELEMS.  */
	  tree members = NULL;
	  tree src;

	  for (src = DECL_ANON_UNION_ELEMS (t); src;
	       src = TREE_CHAIN (src))
	    {
	      tree member = remap_decl (TREE_VALUE (src), id);

	      gcc_assert (!TREE_PURPOSE (src));
	      members = tree_cons (NULL, member, members);
	    }
	  DECL_ANON_UNION_ELEMS (t) = nreverse (members);
	}
#endif

      return t;
    }

  return unshare_expr ((tree) n->value);
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

  /* The type only needs remapping if it's variably modified by a variable
     in the function we are inlining.  */
  if (! variably_modified_type_p (type, VARRAY_TOP_TREE (id->fns)))
    {
      insert_decl_map (id, type, type);
      return type;
    }

  /* We do need a copy.  build and register it now.  If this is a pointer or
     reference type, remap the designated type and make a new pointer or
     reference type.  */
  if (TREE_CODE (type) == POINTER_TYPE)
    {
      new = build_pointer_type_for_mode (remap_type (TREE_TYPE (type), id),
					 TYPE_MODE (type),
					 TYPE_REF_CAN_ALIAS_ALL (type));
      insert_decl_map (id, type, new);
      return new;
    }
  else if (TREE_CODE (type) == REFERENCE_TYPE)
    {
      new = build_reference_type_for_mode (remap_type (TREE_TYPE (type), id),
					    TYPE_MODE (type),
					    TYPE_REF_CAN_ALIAS_ALL (type));
      insert_decl_map (id, type, new);
      return new;
    }
  else
    new = copy_node (type);

  insert_decl_map (id, type, new);

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

  if (TYPE_STUB_DECL (type))
    TYPE_STUB_DECL (new) = remap_decl (TYPE_STUB_DECL (type), id);

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
    case OFFSET_TYPE:
    default:
      /* Shouldn't have been thought variable sized.  */
      gcc_unreachable ();
    }

  walk_tree (&TYPE_SIZE (new), copy_body_r, id, NULL);
  walk_tree (&TYPE_SIZE_UNIT (new), copy_body_r, id, NULL);

  return new;
}

static tree
remap_decls (tree decls, inline_data *id)
{
  tree old_var;
  tree new_decls = NULL_TREE;

  /* Remap its variables.  */
  for (old_var = decls; old_var; old_var = TREE_CHAIN (old_var))
    {
      tree new_var;

      /* Remap the variable.  */
      new_var = remap_decl (old_var, id);

      /* If we didn't remap this variable, so we can't mess with its
	 TREE_CHAIN.  If we remapped this variable to the return slot, it's
	 already declared somewhere else, so don't declare it here.  */
      if (!new_var || new_var == id->retvar)
	;
      else
	{
	  gcc_assert (DECL_P (new_var));
	  TREE_CHAIN (new_var) = new_decls;
	  new_decls = new_var;
	}
    }

  return nreverse (new_decls);
}

/* Copy the BLOCK to contain remapped versions of the variables
   therein.  And hook the new block into the block-tree.  */

static void
remap_block (tree *block, inline_data *id)
{
  tree old_block;
  tree new_block;
  tree fn;

  /* Make the new block.  */
  old_block = *block;
  new_block = make_node (BLOCK);
  TREE_USED (new_block) = TREE_USED (old_block);
  BLOCK_ABSTRACT_ORIGIN (new_block) = old_block;
  *block = new_block;

  /* Remap its variables.  */
  BLOCK_VARS (new_block) = remap_decls (BLOCK_VARS (old_block), id);

  fn = VARRAY_TREE (id->fns, 0);
#if 1
  /* FIXME!  It shouldn't be so hard to manage blocks.  Rebuilding them in
     rest_of_compilation is a good start.  */
  if (id->cloning_p)
    /* We're building a clone; DECL_INITIAL is still
       error_mark_node, and current_binding_level is the parm
       binding level.  */
    lang_hooks.decls.insert_block (new_block);
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
#endif
  /* Remember the remapped block.  */
  insert_decl_map (id, old_block, new_block);
}

static void
copy_statement_list (tree *tp)
{
  tree_stmt_iterator oi, ni;
  tree new;

  new = alloc_stmt_list ();
  ni = tsi_start (new);
  oi = tsi_start (*tp);
  *tp = new;

  for (; !tsi_end_p (oi); tsi_next (&oi))
    tsi_link_after (&ni, tsi_stmt (oi), TSI_NEW_STMT);
}

static void
copy_bind_expr (tree *tp, int *walk_subtrees, inline_data *id)
{
  tree block = BIND_EXPR_BLOCK (*tp);
  /* Copy (and replace) the statement.  */
  copy_tree_r (tp, walk_subtrees, NULL);
  if (block)
    {
      remap_block (&block, id);
      BIND_EXPR_BLOCK (*tp) = block;
    }

  if (BIND_EXPR_VARS (*tp))
    /* This will remap a lot of the same decls again, but this should be
       harmless.  */
    BIND_EXPR_VARS (*tp) = remap_decls (BIND_EXPR_VARS (*tp), id);
}

/* Called from copy_body via walk_tree.  DATA is really an `inline_data *'.  */

static tree
copy_body_r (tree *tp, int *walk_subtrees, void *data)
{
  inline_data *id = (inline_data *) data;
  tree fn = VARRAY_TOP_TREE (id->fns);

#if 0
  /* All automatic variables should have a DECL_CONTEXT indicating
     what function they come from.  */
  if ((TREE_CODE (*tp) == VAR_DECL || TREE_CODE (*tp) == LABEL_DECL)
      && DECL_NAMESPACE_SCOPE_P (*tp))
    gcc_assert (DECL_EXTERNAL (*tp) || TREE_STATIC (*tp));
#endif

  /* If this is a RETURN_EXPR, change it into a MODIFY_EXPR and a
     GOTO_EXPR with the RET_LABEL as its target.  */
  if (TREE_CODE (*tp) == RETURN_EXPR && id->ret_label)
    {
      tree return_stmt = *tp;
      tree goto_stmt;

      /* Build the GOTO_EXPR.  */
      tree assignment = TREE_OPERAND (return_stmt, 0);
      goto_stmt = build1 (GOTO_EXPR, void_type_node, id->ret_label);
      TREE_USED (id->ret_label) = 1;

      /* If we're returning something, just turn that into an
	 assignment into the equivalent of the original
	 RESULT_DECL.  */
      if (assignment)
        {
	  /* Do not create a statement containing a naked RESULT_DECL.  */
	  if (TREE_CODE (assignment) == RESULT_DECL)
	    gimplify_stmt (&assignment);

	  *tp = build (BIND_EXPR, void_type_node, NULL, NULL, NULL);
	  append_to_statement_list (assignment, &BIND_EXPR_BODY (*tp));
	  append_to_statement_list (goto_stmt, &BIND_EXPR_BODY (*tp));
        }
      /* If we're not returning anything just do the jump.  */
      else
	*tp = goto_stmt;
    }
  /* Local variables and labels need to be replaced by equivalent
     variables.  We don't want to copy static variables; there's only
     one of those, no matter how many times we inline the containing
     function.  Similarly for globals from an outer function.  */
  else if (lang_hooks.tree_inlining.auto_var_in_fn_p (*tp, fn))
    {
      tree new_decl;

      /* Remap the declaration.  */
      new_decl = remap_decl (*tp, id);
      gcc_assert (new_decl);
      /* Replace this variable with the copy.  */
      STRIP_TYPE_NOPS (new_decl);
      *tp = new_decl;
      *walk_subtrees = 0;
    }
  else if (TREE_CODE (*tp) == STATEMENT_LIST)
    copy_statement_list (tp);
  else if (TREE_CODE (*tp) == SAVE_EXPR)
    remap_save_expr (tp, id->decl_map, walk_subtrees);
  else if (TREE_CODE (*tp) == BIND_EXPR)
    copy_bind_expr (tp, walk_subtrees, id);
  /* Types may need remapping as well.  */
  else if (TYPE_P (*tp))
    *tp = remap_type (*tp, id);

  /* If this is a constant, we have to copy the node iff the type will be
     remapped.  copy_tree_r will not copy a constant.  */
  else if (TREE_CODE_CLASS (TREE_CODE (*tp)) == tcc_constant)
    {
      tree new_type = remap_type (TREE_TYPE (*tp), id);

      if (new_type == TREE_TYPE (*tp))
	*walk_subtrees = 0;

      else if (TREE_CODE (*tp) == INTEGER_CST)
	*tp = build_int_cst_wide (new_type, TREE_INT_CST_LOW (*tp),
				  TREE_INT_CST_HIGH (*tp));
      else
	{
	  *tp = copy_node (*tp);
	  TREE_TYPE (*tp) = new_type;
	}
    }

  /* Otherwise, just copy the node.  Note that copy_tree_r already
     knows not to copy VAR_DECLs, etc., so this is safe.  */
  else
    {
      tree old_node = *tp;

      if (TREE_CODE (*tp) == MODIFY_EXPR
	  && TREE_OPERAND (*tp, 0) == TREE_OPERAND (*tp, 1)
	  && (lang_hooks.tree_inlining.auto_var_in_fn_p
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
		  *tp = build_empty_stmt ();
		  return copy_body_r (tp, walk_subtrees, data);
		}
	    }
	}
      else if (TREE_CODE (*tp) == INDIRECT_REF)
	{
	  /* Get rid of *& from inline substitutions that can happen when a
	     pointer argument is an ADDR_EXPR.  */
	  tree decl = TREE_OPERAND (*tp, 0), value;
	  splay_tree_node n;

	  n = splay_tree_lookup (id->decl_map, (splay_tree_key) decl);
	  if (n)
	    {
	      value = (tree) n->value;
	      STRIP_NOPS (value);
	      if (TREE_CODE (value) == ADDR_EXPR
		  && (lang_hooks.types_compatible_p
		      (TREE_TYPE (*tp), TREE_TYPE (TREE_OPERAND (value, 0)))))
		{
		  *tp = TREE_OPERAND (value, 0);
		  return copy_body_r (tp, walk_subtrees, data);
		}
	    }
	}

      copy_tree_r (tp, walk_subtrees, NULL);

      if (TREE_CODE (*tp) == CALL_EXPR && id->node && get_callee_fndecl (*tp))
	{
	  if (id->saving_p)
	    {
	      struct cgraph_node *node;
              struct cgraph_edge *edge;

	      for (node = id->node->next_clone; node; node = node->next_clone)
		{
		  edge = cgraph_edge (node, old_node);
		  gcc_assert (edge);
		  edge->call_expr = *tp;
		}
	    }
	  else
	    {
              struct cgraph_edge *edge
		= cgraph_edge (id->current_node, old_node);

	      if (edge)
	        cgraph_clone_edge (edge, id->node, *tp);
	    }
	}

      TREE_TYPE (*tp) = remap_type (TREE_TYPE (*tp), id);

      /* The copied TARGET_EXPR has never been expanded, even if the
	 original node was expanded already.  */
      if (TREE_CODE (*tp) == TARGET_EXPR && TREE_OPERAND (*tp, 3))
	{
	  TREE_OPERAND (*tp, 1) = TREE_OPERAND (*tp, 3);
	  TREE_OPERAND (*tp, 3) = NULL_TREE;
	}

      /* Variable substitution need not be simple.  In particular, the
	 INDIRECT_REF substitution above.  Make sure that TREE_CONSTANT
	 and friends are up-to-date.  */
      else if (TREE_CODE (*tp) == ADDR_EXPR)
	{
	  walk_tree (&TREE_OPERAND (*tp, 0), copy_body_r, id, NULL);
	  recompute_tree_invarant_for_addr_expr (*tp);
	  *walk_subtrees = 0;
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
  tree fndecl = VARRAY_TOP_TREE (id->fns);

  if (fndecl == current_function_decl
      && cfun->saved_tree)
    body = cfun->saved_tree;
  else
    body = DECL_SAVED_TREE (fndecl);
  walk_tree (&body, copy_body_r, id, NULL);

  return body;
}

/* Return true if VALUE is an ADDR_EXPR of an automatic variable
   defined in function FN, or of a data member thereof.  */

static bool
self_inlining_addr_expr (tree value, tree fn)
{
  tree var;

  if (TREE_CODE (value) != ADDR_EXPR)
    return false;

  var = get_base_address (TREE_OPERAND (value, 0));
	      
  return var && lang_hooks.tree_inlining.auto_var_in_fn_p (var, fn);
}

static void
setup_one_parameter (inline_data *id, tree p, tree value, tree fn,
		     tree *init_stmts, tree *vars, bool *gimplify_init_stmts_p)
{
  tree init_stmt;
  tree var;

  /* If the parameter is never assigned to, we may not need to
     create a new variable here at all.  Instead, we may be able
     to just use the argument value.  */
  if (TREE_READONLY (p)
      && !TREE_ADDRESSABLE (p)
      && value && !TREE_SIDE_EFFECTS (value))
    {
      /* We can't risk substituting complex expressions.  They
	 might contain variables that will be assigned to later.
	 Theoretically, we could check the expression to see if
	 all of the variables that determine its value are
	 read-only, but we don't bother.  */
      /* We may produce non-gimple trees by adding NOPs or introduce
	 invalid sharing when operand is not really constant.
	 It is not big deal to prohibit constant propagation here as
	 we will constant propagate in DOM1 pass anyway.  */
      if (is_gimple_min_invariant (value)
	  && lang_hooks.types_compatible_p (TREE_TYPE (value), TREE_TYPE (p))
	  /* We have to be very careful about ADDR_EXPR.  Make sure
	     the base variable isn't a local variable of the inlined
	     function, e.g., when doing recursive inlining, direct or
	     mutually-recursive or whatever, which is why we don't
	     just test whether fn == current_function_decl.  */
	  && ! self_inlining_addr_expr (value, fn))
	{
	  insert_decl_map (id, p, value);
	  return;
	}
    }

  /* Make an equivalent VAR_DECL.  Note that we must NOT remap the type
     here since the type of this decl must be visible to the calling
     function.  */
  var = copy_decl_for_inlining (p, fn, VARRAY_TREE (id->fns, 0));

  /* Register the VAR_DECL as the equivalent for the PARM_DECL;
     that way, when the PARM_DECL is encountered, it will be
     automatically replaced by the VAR_DECL.  */
  insert_decl_map (id, p, var);

  /* Declare this new variable.  */
  TREE_CHAIN (var) = *vars;
  *vars = var;

  /* Make gimplifier happy about this variable.  */
  DECL_SEEN_IN_BIND_EXPR_P (var) = 1;

  /* Even if P was TREE_READONLY, the new VAR should not be.
     In the original code, we would have constructed a
     temporary, and then the function body would have never
     changed the value of P.  However, now, we will be
     constructing VAR directly.  The constructor body may
     change its value multiple times as it is being
     constructed.  Therefore, it must not be TREE_READONLY;
     the back-end assumes that TREE_READONLY variable is
     assigned to only once.  */
  if (TYPE_NEEDS_CONSTRUCTING (TREE_TYPE (p)))
    TREE_READONLY (var) = 0;

  /* Initialize this VAR_DECL from the equivalent argument.  Convert
     the argument to the proper type in case it was promoted.  */
  if (value)
    {
      tree rhs = fold_convert (TREE_TYPE (var), value);

      if (rhs == error_mark_node)
	return;

      /* We want to use MODIFY_EXPR, not INIT_EXPR here so that we
	 keep our trees in gimple form.  */
      init_stmt = build (MODIFY_EXPR, TREE_TYPE (var), var, rhs);
      append_to_statement_list (init_stmt, init_stmts);

      /* If we did not create a gimple value and we did not create a gimple
	 cast of a gimple value, then we will need to gimplify INIT_STMTS
	 at the end.  Note that is_gimple_cast only checks the outer
	 tree code, not its operand.  Thus the explicit check that it's
	 operand is a gimple value.  */
      if (!is_gimple_val (rhs)
	  && (!is_gimple_cast (rhs)
	      || !is_gimple_val (TREE_OPERAND (rhs, 0))))
	*gimplify_init_stmts_p = true;
    }
}

/* Generate code to initialize the parameters of the function at the
   top of the stack in ID from the ARGS (presented as a TREE_LIST).  */

static tree
initialize_inlined_parameters (inline_data *id, tree args, tree static_chain,
			       tree fn, tree bind_expr)
{
  tree init_stmts = NULL_TREE;
  tree parms;
  tree a;
  tree p;
  tree vars = NULL_TREE;
  bool gimplify_init_stmts_p = false;
  int argnum = 0;

  /* Figure out what the parameters are.  */
  parms = DECL_ARGUMENTS (fn);
  if (fn == current_function_decl)
    parms = cfun->saved_args;

  /* Loop through the parameter declarations, replacing each with an
     equivalent VAR_DECL, appropriately initialized.  */
  for (p = parms, a = args; p;
       a = a ? TREE_CHAIN (a) : a, p = TREE_CHAIN (p))
    {
      tree value;

      ++argnum;

      /* Find the initializer.  */
      value = lang_hooks.tree_inlining.convert_parm_for_inlining
	      (p, a ? TREE_VALUE (a) : NULL_TREE, fn, argnum);

      setup_one_parameter (id, p, value, fn, &init_stmts, &vars,
			   &gimplify_init_stmts_p);
    }

  /* Evaluate trailing arguments.  */
  for (; a; a = TREE_CHAIN (a))
    {
      tree value = TREE_VALUE (a);
      append_to_statement_list (value, &init_stmts);
    }

  /* Initialize the static chain.  */
  p = DECL_STRUCT_FUNCTION (fn)->static_chain_decl;
  if (fn == current_function_decl)
    p = DECL_STRUCT_FUNCTION (fn)->saved_static_chain_decl;
  if (p)
    {
      /* No static chain?  Seems like a bug in tree-nested.c.  */
      gcc_assert (static_chain);

      setup_one_parameter (id, p, static_chain, fn, &init_stmts, &vars,
			   &gimplify_init_stmts_p);
    }

  if (gimplify_init_stmts_p)
    gimplify_body (&init_stmts, current_function_decl, false);

  declare_inline_vars (bind_expr, vars);
  return init_stmts;
}

/* Declare a return variable to replace the RESULT_DECL for the function we
   are calling.  RETURN_SLOT_ADDR, if non-null, was a fake parameter that
   took the address of the result.  MODIFY_DEST, if non-null, was the LHS of
   the MODIFY_EXPR to which this call is the RHS.

   The return value is a (possibly null) value that is the result of the
   function as seen by the callee.  *USE_P is a (possibly null) value that
   holds the result as seen by the caller.  */

static tree
declare_return_variable (inline_data *id, tree return_slot_addr,
			 tree modify_dest, tree *use_p)
{
  tree callee = VARRAY_TOP_TREE (id->fns);
  tree caller = VARRAY_TREE (id->fns, 0);
  tree result = DECL_RESULT (callee);
  tree callee_type = TREE_TYPE (result);
  tree caller_type = TREE_TYPE (TREE_TYPE (callee));
  tree var, use;

  /* We don't need to do anything for functions that don't return
     anything.  */
  if (!result || VOID_TYPE_P (callee_type))
    {
      *use_p = NULL_TREE;
      return NULL_TREE;
    }

  /* If there was a return slot, then the return value is the
     dereferenced address of that object.  */
  if (return_slot_addr)
    {
      /* The front end shouldn't have used both return_slot_addr and
	 a modify expression.  */
      gcc_assert (!modify_dest);
      if (DECL_BY_REFERENCE (result))
	var = return_slot_addr;
      else
	var = build_fold_indirect_ref (return_slot_addr);
      use = NULL;
      goto done;
    }

  /* All types requiring non-trivial constructors should have been handled.  */
  gcc_assert (!TREE_ADDRESSABLE (callee_type));

  /* Attempt to avoid creating a new temporary variable.  */
  if (modify_dest)
    {
      bool use_it = false;

      /* We can't use MODIFY_DEST if there's type promotion involved.  */
      if (!lang_hooks.types_compatible_p (caller_type, callee_type))
	use_it = false;

      /* ??? If we're assigning to a variable sized type, then we must
	 reuse the destination variable, because we've no good way to
	 create variable sized temporaries at this point.  */
      else if (TREE_CODE (TYPE_SIZE_UNIT (caller_type)) != INTEGER_CST)
	use_it = true;

      /* If the callee cannot possibly modify MODIFY_DEST, then we can
	 reuse it as the result of the call directly.  Don't do this if
	 it would promote MODIFY_DEST to addressable.  */
      else if (!TREE_STATIC (modify_dest)
	       && !TREE_ADDRESSABLE (modify_dest)
	       && !TREE_ADDRESSABLE (result))
	use_it = true;

      if (use_it)
	{
	  var = modify_dest;
	  use = NULL;
	  goto done;
	}
    }

  gcc_assert (TREE_CODE (TYPE_SIZE_UNIT (callee_type)) == INTEGER_CST);

  var = copy_decl_for_inlining (result, callee, caller);
  DECL_SEEN_IN_BIND_EXPR_P (var) = 1;
  DECL_STRUCT_FUNCTION (caller)->unexpanded_var_list
    = tree_cons (NULL_TREE, var,
		 DECL_STRUCT_FUNCTION (caller)->unexpanded_var_list);

  /* Do not have the rest of GCC warn about this variable as it should
     not be visible to the user.  */
  TREE_NO_WARNING (var) = 1;

  /* Build the use expr.  If the return type of the function was
     promoted, convert it back to the expected type.  */
  use = var;
  if (!lang_hooks.types_compatible_p (TREE_TYPE (var), caller_type))
    use = fold_convert (caller_type, var);

 done:
  /* Register the VAR_DECL as the equivalent for the RESULT_DECL; that
     way, when the RESULT_DECL is encountered, it will be
     automatically replaced by the VAR_DECL.  */
  insert_decl_map (id, result, var);

  /* Remember this so we can ignore it in remap_decls.  */
  id->retvar = var;

  *use_p = use;
  return var;
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
	    = N_("%Jfunction %qF can never be inlined because it uses "
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
	    = N_("%Jfunction %qF can never be inlined because it uses setjmp");
	  return node;
	}

      if (DECL_BUILT_IN_CLASS (t) == BUILT_IN_NORMAL)
	switch (DECL_FUNCTION_CODE (t))
	  {
	    /* We cannot inline functions that take a variable number of
	       arguments.  */
	  case BUILT_IN_VA_START:
	  case BUILT_IN_STDARG_START:
	  case BUILT_IN_NEXT_ARG:
	  case BUILT_IN_VA_END:
	    inline_forbidden_reason
	      = N_("%Jfunction %qF can never be inlined because it "
		   "uses variable argument lists");
	    return node;

	  case BUILT_IN_LONGJMP:
	    /* We can't inline functions that call __builtin_longjmp at
	       all.  The non-local goto machinery really requires the
	       destination be in a different function.  If we allow the
	       function calling __builtin_longjmp to be inlined into the
	       function calling __builtin_setjmp, Things will Go Awry.  */
	    inline_forbidden_reason
	      = N_("%Jfunction %qF can never be inlined because "
		   "it uses setjmp-longjmp exception handling");
	    return node;

	  case BUILT_IN_NONLOCAL_GOTO:
	    /* Similarly.  */
	    inline_forbidden_reason
	      = N_("%Jfunction %qF can never be inlined because "
		   "it uses non-local goto");
	    return node;

	  case BUILT_IN_RETURN:
	  case BUILT_IN_APPLY_ARGS:
	    /* If a __builtin_apply_args caller would be inlined,
	       it would be saving arguments of the function it has
	       been inlined into.  Similarly __builtin_return would
	       return from the function the inline has been inlined into.  */
	    inline_forbidden_reason
	      = N_("%Jfunction %qF can never be inlined because "
		   "it uses __builtin_return or __builtin_apply_args");
	    return node;

	  default:
	    break;
	  }
      break;

    case GOTO_EXPR:
      t = TREE_OPERAND (node, 0);

      /* We will not inline a function which uses computed goto.  The
	 addresses of its local labels, which may be tucked into
	 global storage, are of course not constant across
	 instantiations, which causes unexpected behavior.  */
      if (TREE_CODE (t) != LABEL_DECL)
	{
	  inline_forbidden_reason
	    = N_("%Jfunction %qF can never be inlined "
		 "because it contains a computed goto");
	  return node;
	}
      break;

    case LABEL_EXPR:
      t = TREE_OPERAND (node, 0);
      if (DECL_NONLOCAL (t))
	{
	  /* We cannot inline a function that receives a non-local goto
	     because we cannot remap the destination label used in the
	     function that is performing the non-local goto.  */
	  inline_forbidden_reason
	    = N_("%Jfunction %qF can never be inlined "
		 "because it receives a non-local goto");
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
	 F is inlined, and we abort in find_function_data.

	 ??? This is likely no longer true, but it's too late in the 4.0
	 cycle to try to find out.  This should be checked for 4.1.  */
      for (t = TYPE_FIELDS (node); t; t = TREE_CHAIN (t))
	if (variably_modified_type_p (TREE_TYPE (t), NULL))
	  {
	    inline_forbidden_reason
	      = N_("%Jfunction %qF can never be inlined "
		   "because it uses variable sized variables");
	    return node;
	  }

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
  tree ret = walk_tree_without_duplicates (&DECL_SAVED_TREE (fndecl),
					   inline_forbidden_p_1, fndecl);

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
  inlinable = !lang_hooks.tree_inlining.cannot_inline_tree_fn (&fn);

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

      if (lookup_attribute ("always_inline", DECL_ATTRIBUTES (fn)))
	sorry (inline_forbidden_reason, fn, fn);
      else if (do_warning)
	warning (inline_forbidden_reason, fn, fn);

      inlinable = false;
    }

  /* Squirrel away the result so that we don't have to check again.  */
  DECL_UNINLINABLE (fn) = !inlinable;

  return inlinable;
}

/* Estimate the cost of a memory move.  Use machine dependent
   word size and take possible memcpy call into account.  */

int
estimate_move_cost (tree type)
{
  HOST_WIDE_INT size;

  size = int_size_in_bytes (type);

  if (size < 0 || size > MOVE_MAX_PIECES * MOVE_RATIO)
    /* Cost of a memcpy call, 3 arguments and the call.  */
    return 4;
  else
    return ((size + MOVE_MAX_PIECES - 1) / MOVE_MAX_PIECES);
}

/* Used by estimate_num_insns.  Estimate number of instructions seen
   by given statement.  */

static tree
estimate_num_insns_1 (tree *tp, int *walk_subtrees, void *data)
{
  int *count = data;
  tree x = *tp;

  if (IS_TYPE_OR_DECL_P (x))
    {
      *walk_subtrees = 0;
      return NULL;
    }
  /* Assume that constants and references counts nothing.  These should
     be majorized by amount of operations among them we count later
     and are common target of CSE and similar optimizations.  */
  else if (CONSTANT_CLASS_P (x) || REFERENCE_CLASS_P (x))
    return NULL;

  switch (TREE_CODE (x))
    {
    /* Containers have no cost.  */
    case TREE_LIST:
    case TREE_VEC:
    case BLOCK:
    case COMPONENT_REF:
    case BIT_FIELD_REF:
    case INDIRECT_REF:
    case ALIGN_INDIRECT_REF:
    case MISALIGNED_INDIRECT_REF:
    case ARRAY_REF:
    case ARRAY_RANGE_REF:
    case OBJ_TYPE_REF:
    case EXC_PTR_EXPR: /* ??? */
    case FILTER_EXPR: /* ??? */
    case COMPOUND_EXPR:
    case BIND_EXPR:
    case WITH_CLEANUP_EXPR:
    case NOP_EXPR:
    case VIEW_CONVERT_EXPR:
    case SAVE_EXPR:
    case ADDR_EXPR:
    case COMPLEX_EXPR:
    case RANGE_EXPR:
    case CASE_LABEL_EXPR:
    case SSA_NAME:
    case CATCH_EXPR:
    case EH_FILTER_EXPR:
    case STATEMENT_LIST:
    case ERROR_MARK:
    case NON_LVALUE_EXPR:
    case FDESC_EXPR:
    case VA_ARG_EXPR:
    case TRY_CATCH_EXPR:
    case TRY_FINALLY_EXPR:
    case LABEL_EXPR:
    case GOTO_EXPR:
    case RETURN_EXPR:
    case EXIT_EXPR:
    case LOOP_EXPR:
    case PHI_NODE:
    case WITH_SIZE_EXPR:
      break;

    /* We don't account constants for now.  Assume that the cost is amortized
       by operations that do use them.  We may re-consider this decision once
       we are able to optimize the tree before estimating it's size and break
       out static initializers.  */
    case IDENTIFIER_NODE:
    case INTEGER_CST:
    case REAL_CST:
    case COMPLEX_CST:
    case VECTOR_CST:
    case STRING_CST:
      *walk_subtrees = 0;
      return NULL;

    /* Try to estimate the cost of assignments.  We have three cases to
       deal with:
	1) Simple assignments to registers;
	2) Stores to things that must live in memory.  This includes
	   "normal" stores to scalars, but also assignments of large
	   structures, or constructors of big arrays;
	3) TARGET_EXPRs.

       Let us look at the first two cases, assuming we have "a = b + C":
       <modify_expr <var_decl "a"> <plus_expr <var_decl "b"> <constant C>>
       If "a" is a GIMPLE register, the assignment to it is free on almost
       any target, because "a" usually ends up in a real register.  Hence
       the only cost of this expression comes from the PLUS_EXPR, and we
       can ignore the MODIFY_EXPR.
       If "a" is not a GIMPLE register, the assignment to "a" will most
       likely be a real store, so the cost of the MODIFY_EXPR is the cost
       of moving something into "a", which we compute using the function
       estimate_move_cost.

       The third case deals with TARGET_EXPRs, for which the semantics are
       that a temporary is assigned, unless the TARGET_EXPR itself is being
       assigned to something else.  In the latter case we do not need the
       temporary.  E.g. in <modify_expr <var_decl "a"> <target_expr>>, the
       MODIFY_EXPR is free.  */
    case INIT_EXPR:
    case MODIFY_EXPR:
      /* Is the right and side a TARGET_EXPR?  */
      if (TREE_CODE (TREE_OPERAND (x, 1)) == TARGET_EXPR)
	break;
      /* ... fall through ...  */

    case TARGET_EXPR:
      x = TREE_OPERAND (x, 0);
      /* Is this an assignments to a register?  */
      if (is_gimple_reg (x))
	break;
      /* Otherwise it's a store, so fall through to compute the move cost.  */
      
    case CONSTRUCTOR:
      *count += estimate_move_cost (TREE_TYPE (x));
      break;

    /* Assign cost of 1 to usual operations.
       ??? We may consider mapping RTL costs to this.  */
    case COND_EXPR:

    case PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:

    case FIX_TRUNC_EXPR:
    case FIX_CEIL_EXPR:
    case FIX_FLOOR_EXPR:
    case FIX_ROUND_EXPR:

    case NEGATE_EXPR:
    case FLOAT_EXPR:
    case MIN_EXPR:
    case MAX_EXPR:
    case ABS_EXPR:

    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:

    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case BIT_AND_EXPR:
    case BIT_NOT_EXPR:

    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_XOR_EXPR:
    case TRUTH_NOT_EXPR:

    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
    case ORDERED_EXPR:
    case UNORDERED_EXPR:

    case UNLT_EXPR:
    case UNLE_EXPR:
    case UNGT_EXPR:
    case UNGE_EXPR:
    case UNEQ_EXPR:
    case LTGT_EXPR:

    case CONVERT_EXPR:

    case CONJ_EXPR:

    case PREDECREMENT_EXPR:
    case PREINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:

    case SWITCH_EXPR:

    case ASM_EXPR:

    case REALIGN_LOAD_EXPR:

    case RESX_EXPR:
      *count += 1;
      break;

    /* Few special cases of expensive operations.  This is useful
       to avoid inlining on functions having too many of these.  */
    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case EXACT_DIV_EXPR:
    case TRUNC_MOD_EXPR:
    case CEIL_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case ROUND_MOD_EXPR:
    case RDIV_EXPR:
      *count += 10;
      break;
    case CALL_EXPR:
      {
	tree decl = get_callee_fndecl (x);
	tree arg;

	if (decl && DECL_BUILT_IN_CLASS (decl) == BUILT_IN_NORMAL)
	  switch (DECL_FUNCTION_CODE (decl))
	    {
	    case BUILT_IN_CONSTANT_P:
	      *walk_subtrees = 0;
	      return NULL_TREE;
	    case BUILT_IN_EXPECT:
	      return NULL_TREE;
	    default:
	      break;
	    }

	/* Our cost must be kept in sync with cgraph_estimate_size_after_inlining
	   that does use function declaration to figure out the arguments.  */
	if (!decl)
	  {
	    for (arg = TREE_OPERAND (x, 1); arg; arg = TREE_CHAIN (arg))
	      *count += estimate_move_cost (TREE_TYPE (TREE_VALUE (arg)));
	  }
	else
	  {
	    for (arg = DECL_ARGUMENTS (decl); arg; arg = TREE_CHAIN (arg))
	      *count += estimate_move_cost (TREE_TYPE (arg));
	  }

	*count += PARAM_VALUE (PARAM_INLINE_CALL_COST);
	break;
      }
    default:
      /* Abort here se we know we don't miss any nodes.  */
      gcc_unreachable ();
    }
  return NULL;
}

/* Estimate number of instructions that will be created by expanding EXPR.  */

int
estimate_num_insns (tree expr)
{
  int num = 0;
  walk_tree_without_duplicates (&expr, estimate_num_insns_1, &num);
  return num;
}

/* If *TP is a CALL_EXPR, replace it with its inline expansion.  */

static tree
expand_call_inline (tree *tp, int *walk_subtrees, void *data)
{
  inline_data *id;
  tree t;
  tree expr;
  tree stmt;
  tree use_retvar;
  tree decl;
  tree fn;
  tree arg_inits;
  tree *inlined_body;
  splay_tree st;
  tree args;
  tree return_slot_addr;
  tree modify_dest;
  location_t saved_location;
  struct cgraph_edge *edge;
  const char *reason;

  /* See what we've got.  */
  id = (inline_data *) data;
  t = *tp;

  /* Set input_location here so we get the right instantiation context
     if we call instantiate_decl from inlinable_function_p.  */
  saved_location = input_location;
  if (EXPR_HAS_LOCATION (t))
    input_location = EXPR_LOCATION (t);

  /* Recurse, but letting recursive invocations know that we are
     inside the body of a TARGET_EXPR.  */
  if (TREE_CODE (*tp) == TARGET_EXPR)
    {
#if 0
      int i, len = TREE_CODE_LENGTH (TARGET_EXPR);

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

      goto egress;
#endif
    }

  if (TYPE_P (t))
    /* Because types were not copied in copy_body, CALL_EXPRs beneath
       them should not be expanded.  This can happen if the type is a
       dynamic array type, for example.  */
    *walk_subtrees = 0;

  /* From here on, we're only interested in CALL_EXPRs.  */
  if (TREE_CODE (t) != CALL_EXPR)
    goto egress;

  /* First, see if we can figure out what function is being called.
     If we cannot, then there is no hope of inlining the function.  */
  fn = get_callee_fndecl (t);
  if (!fn)
    goto egress;

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

  /* Objective C and fortran still calls tree_rest_of_compilation directly.
     Kill this check once this is fixed.  */
  if (!id->current_node->analyzed)
    goto egress;

  edge = cgraph_edge (id->current_node, t);

  /* Constant propagation on argument done during previous inlining
     may create new direct call.  Produce an edge for it.  */
  if (!edge)
    {
      struct cgraph_node *dest = cgraph_node (fn);

      /* We have missing edge in the callgraph.  This can happen in one case
         where previous inlining turned indirect call into direct call by
         constant propagating arguments.  In all other cases we hit a bug
         (incorrect node sharing is most common reason for missing edges.  */
      gcc_assert (dest->needed || !flag_unit_at_a_time);
      cgraph_create_edge (id->node, dest, t)->inline_failed
	= N_("originally indirect function call not considered for inlining");
      goto egress;
    }

  /* Don't try to inline functions that are not well-suited to
     inlining.  */
  if (!cgraph_inline_p (edge, &reason))
    {
      if (lookup_attribute ("always_inline", DECL_ATTRIBUTES (fn)))
	{
	  sorry ("%Jinlining failed in call to %qF: %s", fn, fn, reason);
	  sorry ("called from here");
	}
      else if (warn_inline && DECL_DECLARED_INLINE_P (fn)
	       && !DECL_IN_SYSTEM_HEADER (fn)
	       && strlen (reason)
	       && !lookup_attribute ("noinline", DECL_ATTRIBUTES (fn)))
	{
	  warning ("%Jinlining failed in call to %qF: %s", fn, fn, reason);
	  warning ("called from here");
	}
      goto egress;
    }

#ifdef ENABLE_CHECKING
  if (edge->callee->decl != id->node->decl)
    verify_cgraph_node (edge->callee);
#endif

  if (! lang_hooks.tree_inlining.start_inlining (fn))
    goto egress;

  /* Build a block containing code to initialize the arguments, the
     actual inline expansion of the body, and a label for the return
     statements within the function to jump to.  The type of the
     statement expression is the return type of the function call.  */
  stmt = NULL;
  expr = build (BIND_EXPR, void_type_node, NULL_TREE,
		stmt, make_node (BLOCK));
  BLOCK_ABSTRACT_ORIGIN (BIND_EXPR_BLOCK (expr)) = fn;

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
      TREE_TYPE (expr) = void_type_node;
    }

  arg_inits = initialize_inlined_parameters (id, args, TREE_OPERAND (t, 2),
					     fn, expr);
  if (arg_inits)
    {
      /* Expand any inlined calls in the initializers.  Do this before we
	 push FN on the stack of functions we are inlining; we want to
	 inline calls to FN that appear in the initializers for the
	 parameters.

	 Note we need to save and restore the saved tree statement iterator
	 to avoid having it clobbered by expand_calls_inline.  */
      tree_stmt_iterator save_tsi;

      save_tsi = id->tsi;
      expand_calls_inline (&arg_inits, id);
      id->tsi = save_tsi;

      /* And add them to the tree.  */
      append_to_statement_list (arg_inits, &BIND_EXPR_BODY (expr));
    }

  /* Record the function we are about to inline so that we can avoid
     recursing into it.  */
  VARRAY_PUSH_TREE (id->fns, fn);

  /* Return statements in the function body will be replaced by jumps
     to the RET_LABEL.  */
  id->ret_label = build_decl (LABEL_DECL, NULL_TREE, NULL_TREE);
  DECL_ARTIFICIAL (id->ret_label) = 1;
  DECL_IGNORED_P (id->ret_label) = 1;
  DECL_CONTEXT (id->ret_label) = VARRAY_TREE (id->fns, 0);
  insert_decl_map (id, id->ret_label, id->ret_label);

  gcc_assert (DECL_INITIAL (fn));
  gcc_assert (TREE_CODE (DECL_INITIAL (fn)) == BLOCK);

  /* Find the lhs to which the result of this call is assigned.  */
  modify_dest = tsi_stmt (id->tsi);
  if (TREE_CODE (modify_dest) == MODIFY_EXPR)
    {
      modify_dest = TREE_OPERAND (modify_dest, 0);

      /* The function which we are inlining might not return a value,
	 in which case we should issue a warning that the function
	 does not return a value.  In that case the optimizers will
	 see that the variable to which the value is assigned was not
	 initialized.  We do not want to issue a warning about that
	 uninitialized variable.  */
      if (DECL_P (modify_dest))
	TREE_NO_WARNING (modify_dest) = 1;
    }
  else
    modify_dest = NULL;

  /* Declare the return variable for the function.  */
  decl = declare_return_variable (id, return_slot_addr,
				  modify_dest, &use_retvar);

  /* After we've initialized the parameters, we insert the body of the
     function itself.  */
  {
    struct cgraph_node *old_node = id->current_node;
    tree copy;

    id->current_node = edge->callee;
    copy = copy_body (id);

    /* If the function uses a return slot, then it may legitimately
       fall through while still returning a value, so we have to skip
       the warning here.  */
    if (warn_return_type
	&& !TREE_NO_WARNING (fn)
	&& !VOID_TYPE_P (TREE_TYPE (TREE_TYPE (fn)))
	&& return_slot_addr == NULL_TREE
	&& block_may_fallthru (copy))
      {
	warning ("control may reach end of non-void function %qD being inlined",
		 fn);
	TREE_NO_WARNING (fn) = 1;
      }

    append_to_statement_list (copy, &BIND_EXPR_BODY (expr));
    id->current_node = old_node;
  }
  inlined_body = &BIND_EXPR_BODY (expr);

  /* After the body of the function comes the RET_LABEL.  This must come
     before we evaluate the returned value below, because that evaluation
     may cause RTL to be generated.  */
  if (TREE_USED (id->ret_label))
    {
      tree label = build1 (LABEL_EXPR, void_type_node, id->ret_label);
      append_to_statement_list (label, &BIND_EXPR_BODY (expr));
    }

  /* Clean up.  */
  splay_tree_delete (id->decl_map);
  id->decl_map = st;

  /* Although, from the semantic viewpoint, the new expression has
     side-effects only if the old one did, it is not possible, from
     the technical viewpoint, to evaluate the body of a function
     multiple times without serious havoc.  */
  TREE_SIDE_EFFECTS (expr) = 1;

  tsi_link_before (&id->tsi, expr, TSI_SAME_STMT);

  /* If the inlined function returns a result that we care about,
     then we're going to need to splice in a MODIFY_EXPR.  Otherwise
     the call was a standalone statement and we can just replace it
     with the BIND_EXPR inline representation of the called function.  */
  if (!use_retvar || !modify_dest)
    *tsi_stmt_ptr (id->tsi) = build_empty_stmt ();
  else
    *tp = use_retvar;

  /* When we gimplify a function call, we may clear TREE_SIDE_EFFECTS on
     the call if it is to a "const" function.  Thus the copy of
     TREE_SIDE_EFFECTS from the CALL_EXPR to the BIND_EXPR above with
     result in TREE_SIDE_EFFECTS not being set for the inlined copy of a
     "const" function.

     Unfortunately, that is wrong as inlining the function can create/expose
     interesting side effects (such as setting of a return value).

     The easiest solution is to simply recalculate TREE_SIDE_EFFECTS for
     the toplevel expression.  */
  recalculate_side_effects (expr);
  
  /* Output the inlining info for this abstract function, since it has been
     inlined.  If we don't do this now, we can lose the information about the
     variables in the function when the blocks get blown away as soon as we
     remove the cgraph node.  */
  (*debug_hooks->outlining_inline_function) (edge->callee->decl);

  /* Update callgraph if needed.  */
  cgraph_remove_node (edge->callee);

  /* Recurse into the body of the just inlined function.  */
  expand_calls_inline (inlined_body, id);
  VARRAY_POP (id->fns);

  /* Don't walk into subtrees.  We've already handled them above.  */
  *walk_subtrees = 0;

  lang_hooks.tree_inlining.end_inlining (fn);

  /* Keep iterating.  */
 egress:
  input_location = saved_location;
  return NULL_TREE;
}

static void
expand_calls_inline (tree *stmt_p, inline_data *id)
{
  tree stmt = *stmt_p;
  enum tree_code code = TREE_CODE (stmt);
  int dummy;

  switch (code)
    {
    case STATEMENT_LIST:
      {
	tree_stmt_iterator i;
	tree new;

	for (i = tsi_start (stmt); !tsi_end_p (i); )
	  {
	    id->tsi = i;
	    expand_calls_inline (tsi_stmt_ptr (i), id);

	    new = tsi_stmt (i);
	    if (TREE_CODE (new) == STATEMENT_LIST)
	      {
		tsi_link_before (&i, new, TSI_SAME_STMT);
		tsi_delink (&i);
	      }
	    else
	      tsi_next (&i);
	  }
      }
      break;

    case COND_EXPR:
      expand_calls_inline (&COND_EXPR_THEN (stmt), id);
      expand_calls_inline (&COND_EXPR_ELSE (stmt), id);
      break;

    case CATCH_EXPR:
      expand_calls_inline (&CATCH_BODY (stmt), id);
      break;

    case EH_FILTER_EXPR:
      expand_calls_inline (&EH_FILTER_FAILURE (stmt), id);
      break;

    case TRY_CATCH_EXPR:
    case TRY_FINALLY_EXPR:
      expand_calls_inline (&TREE_OPERAND (stmt, 0), id);
      expand_calls_inline (&TREE_OPERAND (stmt, 1), id);
      break;

    case BIND_EXPR:
      expand_calls_inline (&BIND_EXPR_BODY (stmt), id);
      break;

    case COMPOUND_EXPR:
      /* We're gimple.  We should have gotten rid of all these.  */
      gcc_unreachable ();

    case RETURN_EXPR:
      stmt_p = &TREE_OPERAND (stmt, 0);
      stmt = *stmt_p;
      if (!stmt || TREE_CODE (stmt) != MODIFY_EXPR)
	break;

      /* FALLTHRU */

    case MODIFY_EXPR:
      stmt_p = &TREE_OPERAND (stmt, 1);
      stmt = *stmt_p;
      if (TREE_CODE (stmt) == WITH_SIZE_EXPR)
	{
	  stmt_p = &TREE_OPERAND (stmt, 0);
	  stmt = *stmt_p;
	}
      if (TREE_CODE (stmt) != CALL_EXPR)
	break;

      /* FALLTHRU */

    case CALL_EXPR:
      expand_call_inline (stmt_p, &dummy, id);
      break;

    default:
      break;
    }
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

  id.current_node = id.node = cgraph_node (fn);
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

  prev_fn = lang_hooks.tree_inlining.add_pending_fn_decls (&id.fns, prev_fn);

  /* Keep track of the low-water mark, i.e., the point where the first
     real inlining is represented in ID.FNS.  */
  id.first_inlined_fn = VARRAY_ACTIVE_SIZE (id.fns);

  /* Replace all calls to inline functions with the bodies of those
     functions.  */
  id.tree_pruner = htab_create (37, htab_hash_pointer, htab_eq_pointer, NULL);
  expand_calls_inline (&DECL_SAVED_TREE (fn), &id);

  /* Clean up.  */
  htab_delete (id.tree_pruner);

#ifdef ENABLE_CHECKING
    {
      struct cgraph_edge *e;

      verify_cgraph_node (id.node);

      /* Double check that we inlined everything we are supposed to inline.  */
      for (e = id.node->callees; e; e = e->next_callee)
	gcc_assert (e->inline_failed);
    }
#endif
}

/* FN is a function that has a complete body, and CLONE is a function whose
   body is to be set to a copy of FN, mapping argument declarations according
   to the ARG_MAP splay_tree.  */

void
clone_body (tree clone, tree fn, void *arg_map)
{
  inline_data id;

  /* Clone the body, as if we were making an inline call.  But, remap the
     parameters in the callee to the parameters of caller.  If there's an
     in-charge parameter, map it to an appropriate constant.  */
  memset (&id, 0, sizeof (id));
  VARRAY_TREE_INIT (id.fns, 2, "fns");
  VARRAY_PUSH_TREE (id.fns, clone);
  VARRAY_PUSH_TREE (id.fns, fn);
  id.decl_map = (splay_tree)arg_map;

  /* Cloning is treated slightly differently from inlining.  Set
     CLONING_P so that it's clear which operation we're performing.  */
  id.cloning_p = true;

  /* Actually copy the body.  */
  append_to_statement_list_force (copy_body (&id), &DECL_SAVED_TREE (clone));
}

/* Make and return duplicate of body in FN.  Put copies of DECL_ARGUMENTS
   in *arg_copy and of the static chain, if any, in *sc_copy.  */

tree
save_body (tree fn, tree *arg_copy, tree *sc_copy)
{
  inline_data id;
  tree body, *parg;

  memset (&id, 0, sizeof (id));
  VARRAY_TREE_INIT (id.fns, 1, "fns");
  VARRAY_PUSH_TREE (id.fns, fn);
  id.node = cgraph_node (fn);
  id.saving_p = true;
  id.decl_map = splay_tree_new (splay_tree_compare_pointers, NULL, NULL);
  *arg_copy = DECL_ARGUMENTS (fn);

  for (parg = arg_copy; *parg; parg = &TREE_CHAIN (*parg))
    {
      tree new = copy_node (*parg);

      lang_hooks.dup_lang_specific_decl (new);
      DECL_ABSTRACT_ORIGIN (new) = DECL_ORIGIN (*parg);
      insert_decl_map (&id, *parg, new);
      TREE_CHAIN (new) = TREE_CHAIN (*parg);
      *parg = new;
    }

  *sc_copy = DECL_STRUCT_FUNCTION (fn)->static_chain_decl;
  if (*sc_copy)
    {
      tree new = copy_node (*sc_copy);

      lang_hooks.dup_lang_specific_decl (new);
      DECL_ABSTRACT_ORIGIN (new) = DECL_ORIGIN (*sc_copy);
      insert_decl_map (&id, *sc_copy, new);
      TREE_CHAIN (new) = TREE_CHAIN (*sc_copy);
      *sc_copy = new;
    }

  insert_decl_map (&id, DECL_RESULT (fn), DECL_RESULT (fn));

  /* Actually copy the body.  */
  body = copy_body (&id);

  /* Clean up.  */
  splay_tree_delete (id.decl_map);
  return body;
}

#define WALK_SUBTREE(NODE)				\
  do							\
    {							\
      result = walk_tree (&(NODE), func, data, pset);	\
      if (result)					\
	return result;					\
    }							\
  while (0)

/* This is a subroutine of walk_tree that walks field of TYPE that are to
   be walked whenever a type is seen in the tree.  Rest of operands and return
   value are as for walk_tree.  */

static tree
walk_type_fields (tree type, walk_tree_fn func, void *data,
		  struct pointer_set_t *pset)
{
  tree result = NULL_TREE;

  switch (TREE_CODE (type))
    {
    case POINTER_TYPE:
    case REFERENCE_TYPE:
      /* We have to worry about mutually recursive pointers.  These can't
	 be written in C.  They can in Ada.  It's pathological, but
	 there's an ACATS test (c38102a) that checks it.  Deal with this
	 by checking if we're pointing to another pointer, that one
	 points to another pointer, that one does too, and we have no htab.
	 If so, get a hash table.  We check three levels deep to avoid
	 the cost of the hash table if we don't need one.  */
      if (POINTER_TYPE_P (TREE_TYPE (type))
	  && POINTER_TYPE_P (TREE_TYPE (TREE_TYPE (type)))
	  && POINTER_TYPE_P (TREE_TYPE (TREE_TYPE (TREE_TYPE (type))))
	  && !pset)
	{
	  result = walk_tree_without_duplicates (&TREE_TYPE (type),
						 func, data);
	  if (result)
	    return result;

	  break;
	}

      /* ... fall through ... */

    case COMPLEX_TYPE:
      WALK_SUBTREE (TREE_TYPE (type));
      break;

    case METHOD_TYPE:
      WALK_SUBTREE (TYPE_METHOD_BASETYPE (type));

      /* Fall through.  */

    case FUNCTION_TYPE:
      WALK_SUBTREE (TREE_TYPE (type));
      {
	tree arg;

	/* We never want to walk into default arguments.  */
	for (arg = TYPE_ARG_TYPES (type); arg; arg = TREE_CHAIN (arg))
	  WALK_SUBTREE (TREE_VALUE (arg));
      }
      break;

    case ARRAY_TYPE:
      /* Don't follow this nodes's type if a pointer for fear that we'll
	 have infinite recursion.  Those types are uninteresting anyway.  */
      if (!POINTER_TYPE_P (TREE_TYPE (type))
	  && TREE_CODE (TREE_TYPE (type)) != OFFSET_TYPE)
	WALK_SUBTREE (TREE_TYPE (type));
      WALK_SUBTREE (TYPE_DOMAIN (type));
      break;

    case BOOLEAN_TYPE:
    case ENUMERAL_TYPE:
    case INTEGER_TYPE:
    case CHAR_TYPE:
    case REAL_TYPE:
      WALK_SUBTREE (TYPE_MIN_VALUE (type));
      WALK_SUBTREE (TYPE_MAX_VALUE (type));
      break;

    case OFFSET_TYPE:
      WALK_SUBTREE (TREE_TYPE (type));
      WALK_SUBTREE (TYPE_OFFSET_BASETYPE (type));
      break;

    default:
      break;
    }

  return NULL_TREE;
}

/* Apply FUNC to all the sub-trees of TP in a pre-order traversal.  FUNC is
   called with the DATA and the address of each sub-tree.  If FUNC returns a
   non-NULL value, the traversal is aborted, and the value returned by FUNC
   is returned.  If PSET is non-NULL it is used to record the nodes visited,
   and to avoid visiting a node more than once.  */

tree
walk_tree (tree *tp, walk_tree_fn func, void *data, struct pointer_set_t *pset)
{
  enum tree_code code;
  int walk_subtrees;
  tree result;

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

  /* Don't walk the same tree twice, if the user has requested
     that we avoid doing so.  */
  if (pset && pointer_set_insert (pset, *tp))
    return NULL_TREE;

  /* Call the function.  */
  walk_subtrees = 1;
  result = (*func) (tp, &walk_subtrees, data);

  /* If we found something, return it.  */
  if (result)
    return result;

  code = TREE_CODE (*tp);

  /* Even if we didn't, FUNC may have decided that there was nothing
     interesting below this point in the tree.  */
  if (!walk_subtrees)
    {
      if (code == TREE_LIST)
	/* But we still need to check our siblings.  */
	WALK_SUBTREE_TAIL (TREE_CHAIN (*tp));
      else
	return NULL_TREE;
    }

  result = lang_hooks.tree_inlining.walk_subtrees (tp, &walk_subtrees, func,
						   data, pset);
  if (result || ! walk_subtrees)
    return result;

  /* If this is a DECL_EXPR, walk into various fields of the type that it's
     defining.  We only want to walk into these fields of a type in this
     case.  Note that decls get walked as part of the processing of a
     BIND_EXPR.

     ??? Precisely which fields of types that we are supposed to walk in
     this case vs. the normal case aren't well defined.  */
  if (code == DECL_EXPR
      && TREE_CODE (DECL_EXPR_DECL (*tp)) == TYPE_DECL
      && TREE_CODE (TREE_TYPE (DECL_EXPR_DECL (*tp))) != ERROR_MARK)
    {
      tree *type_p = &TREE_TYPE (DECL_EXPR_DECL (*tp));

      /* Call the function for the type.  See if it returns anything or
	 doesn't want us to continue.  If we are to continue, walk both
	 the normal fields and those for the declaration case.  */
      result = (*func) (type_p, &walk_subtrees, data);
      if (result || !walk_subtrees)
	return NULL_TREE;

      result = walk_type_fields (*type_p, func, data, pset);
      if (result)
	return result;

      WALK_SUBTREE (TYPE_SIZE (*type_p));
      WALK_SUBTREE (TYPE_SIZE_UNIT (*type_p));

      /* If this is a record type, also walk the fields.  */
      if (TREE_CODE (*type_p) == RECORD_TYPE
	  || TREE_CODE (*type_p) == UNION_TYPE
	  || TREE_CODE (*type_p) == QUAL_UNION_TYPE)
	{
	  tree field;

	  for (field = TYPE_FIELDS (*type_p); field;
	       field = TREE_CHAIN (field))
	    {
	      /* We'd like to look at the type of the field, but we can easily
		 get infinite recursion.  So assume it's pointed to elsewhere
		 in the tree.  Also, ignore things that aren't fields.  */
	      if (TREE_CODE (field) != FIELD_DECL)
		continue;

	      WALK_SUBTREE (DECL_FIELD_OFFSET (field));
	      WALK_SUBTREE (DECL_SIZE (field));
	      WALK_SUBTREE (DECL_SIZE_UNIT (field));
	      if (TREE_CODE (*type_p) == QUAL_UNION_TYPE)
		WALK_SUBTREE (DECL_QUALIFIER (field));
	    }
	}
    }

  else if (code != SAVE_EXPR
	   && code != BIND_EXPR
	   && IS_EXPR_CODE_CLASS (TREE_CODE_CLASS (code)))
    {
      int i, len;

      /* Walk over all the sub-trees of this operand.  */
      len = TREE_CODE_LENGTH (code);
      /* TARGET_EXPRs are peculiar: operands 1 and 3 can be the same.
	 But, we only want to walk once.  */
      if (code == TARGET_EXPR
	  && TREE_OPERAND (*tp, 3) == TREE_OPERAND (*tp, 1))
	--len;

      /* Go through the subtrees.  We need to do this in forward order so
         that the scope of a FOR_EXPR is handled properly.  */
#ifdef DEBUG_WALK_TREE
      for (i = 0; i < len; ++i)
	WALK_SUBTREE (TREE_OPERAND (*tp, i));
#else
      for (i = 0; i < len - 1; ++i)
	WALK_SUBTREE (TREE_OPERAND (*tp, i));

      if (len)
	{
	  /* The common case is that we may tail recurse here.  */
	  if (code != BIND_EXPR
	      && !TREE_CHAIN (*tp))
	    WALK_SUBTREE_TAIL (TREE_OPERAND (*tp, len - 1));
	  else
	    WALK_SUBTREE (TREE_OPERAND (*tp, len - 1));
	}
#endif
    }

  /* If this is a type, walk the needed fields in the type.  */
  else if (TYPE_P (*tp))
    {
      result = walk_type_fields (*tp, func, data, pset);
      if (result)
	return result;
    }
  else
    {
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
	case BLOCK:
	case PLACEHOLDER_EXPR:
	case SSA_NAME:
	case FIELD_DECL:
	case RESULT_DECL:
	  /* None of thse have subtrees other than those already walked
	     above.  */
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

	case SAVE_EXPR:
	  WALK_SUBTREE_TAIL (TREE_OPERAND (*tp, 0));

	case BIND_EXPR:
	  {
	    tree decl;
	    for (decl = BIND_EXPR_VARS (*tp); decl; decl = TREE_CHAIN (decl))
	      {
		/* Walk the DECL_INITIAL and DECL_SIZE.  We don't want to walk
		   into declarations that are just mentioned, rather than
		   declared; they don't really belong to this part of the tree.
		   And, we can see cycles: the initializer for a declaration
		   can refer to the declaration itself.  */
		WALK_SUBTREE (DECL_INITIAL (decl));
		WALK_SUBTREE (DECL_SIZE (decl));
		WALK_SUBTREE (DECL_SIZE_UNIT (decl));
	      }
	    WALK_SUBTREE_TAIL (BIND_EXPR_BODY (*tp));
	  }

	case STATEMENT_LIST:
	  {
	    tree_stmt_iterator i;
	    for (i = tsi_start (*tp); !tsi_end_p (i); tsi_next (&i))
	      WALK_SUBTREE (*tsi_stmt_ptr (i));
	  }
	  break;

	default:
	  /* ??? This could be a language-defined node.  We really should make
	     a hook for it, but right now just ignore it.  */
	  break;
	}
    }

  /* We didn't find what we were looking for.  */
  return NULL_TREE;

#undef WALK_SUBTREE
#undef WALK_SUBTREE_TAIL
}

/* Like walk_tree, but does not walk duplicate nodes more than once.  */

tree
walk_tree_without_duplicates (tree *tp, walk_tree_fn func, void *data)
{
  tree result;
  struct pointer_set_t *pset;

  pset = pointer_set_create ();
  result = walk_tree (tp, func, data, pset);
  pointer_set_destroy (pset);
  return result;
}

/* Passed to walk_tree.  Copies the node pointed to, if appropriate.  */

tree
copy_tree_r (tree *tp, int *walk_subtrees, void *data ATTRIBUTE_UNUSED)
{
  enum tree_code code = TREE_CODE (*tp);

  /* We make copies of most nodes.  */
  if (IS_EXPR_CODE_CLASS (TREE_CODE_CLASS (code))
      || code == TREE_LIST
      || code == TREE_VEC
      || code == TYPE_DECL)
    {
      /* Because the chain gets clobbered when we make a copy, we save it
	 here.  */
      tree chain = TREE_CHAIN (*tp);
      tree new;

      /* Copy the node.  */
      new = copy_node (*tp);

      /* Propagate mudflap marked-ness.  */
      if (flag_mudflap && mf_marked_p (*tp))
        mf_mark (new);

      *tp = new;

      /* Now, restore the chain, if appropriate.  That will cause
	 walk_tree to walk into the chain as well.  */
      if (code == PARM_DECL || code == TREE_LIST)
	TREE_CHAIN (*tp) = chain;

      /* For now, we don't update BLOCKs when we make copies.  So, we
	 have to nullify all BIND_EXPRs.  */
      if (TREE_CODE (*tp) == BIND_EXPR)
	BIND_EXPR_BLOCK (*tp) = NULL_TREE;
    }

  else if (TREE_CODE_CLASS (code) == tcc_type)
    *walk_subtrees = 0;
  else if (TREE_CODE_CLASS (code) == tcc_declaration)
    *walk_subtrees = 0;
  else if (TREE_CODE_CLASS (code) == tcc_constant)
    *walk_subtrees = 0;
  else
    gcc_assert (code != STATEMENT_LIST);
  return NULL_TREE;
}

/* The SAVE_EXPR pointed to by TP is being copied.  If ST contains
   information indicating to what new SAVE_EXPR this one should be mapped,
   use that one.  Otherwise, create a new node and enter it in ST.  */

static void
remap_save_expr (tree *tp, void *st_, int *walk_subtrees)
{
  splay_tree st = (splay_tree) st_;
  splay_tree_node n;
  tree t;

  /* See if we already encountered this SAVE_EXPR.  */
  n = splay_tree_lookup (st, (splay_tree_key) *tp);

  /* If we didn't already remap this SAVE_EXPR, do so now.  */
  if (!n)
    {
      t = copy_node (*tp);

      /* Remember this SAVE_EXPR.  */
      splay_tree_insert (st, (splay_tree_key) *tp, (splay_tree_value) t);
      /* Make sure we don't remap an already-remapped SAVE_EXPR.  */
      splay_tree_insert (st, (splay_tree_key) t, (splay_tree_value) t);
    }
  else
    {
      /* We've already walked into this SAVE_EXPR; don't do it again.  */
      *walk_subtrees = 0;
      t = (tree) n->value;
    }

  /* Replace this SAVE_EXPR with the copy.  */
  *tp = t;
}

/* Called via walk_tree.  If *TP points to a DECL_STMT for a local label,
   copies the declaration and enters it in the splay_tree in DATA (which is
   really an `inline_data *').  */

static tree
mark_local_for_remap_r (tree *tp, int *walk_subtrees ATTRIBUTE_UNUSED,
			void *data)
{
  inline_data *id = (inline_data *) data;

  /* Don't walk into types.  */
  if (TYPE_P (*tp))
    *walk_subtrees = 0;

  else if (TREE_CODE (*tp) == LABEL_EXPR)
    {
      tree decl = TREE_OPERAND (*tp, 0);

      /* Copy the decl and remember the copy.  */
      insert_decl_map (id, decl,
		       copy_decl_for_inlining (decl, DECL_CONTEXT (decl),
					       DECL_CONTEXT (decl)));
    }

  return NULL_TREE;
}

/* Perform any modifications to EXPR required when it is unsaved.  Does
   not recurse into EXPR's subtrees.  */

static void
unsave_expr_1 (tree expr)
{
  switch (TREE_CODE (expr))
    {
    case TARGET_EXPR:
      /* Don't mess with a TARGET_EXPR that hasn't been expanded.
         It's OK for this to happen if it was part of a subtree that
         isn't immediately expanded, such as operand 2 of another
         TARGET_EXPR.  */
      if (TREE_OPERAND (expr, 1))
	break;

      TREE_OPERAND (expr, 1) = TREE_OPERAND (expr, 3);
      TREE_OPERAND (expr, 3) = NULL_TREE;
      break;

    default:
      break;
    }
}

/* Called via walk_tree when an expression is unsaved.  Using the
   splay_tree pointed to by ST (which is really a `splay_tree'),
   remaps all local declarations to appropriate replacements.  */

static tree
unsave_r (tree *tp, int *walk_subtrees, void *data)
{
  inline_data *id = (inline_data *) data;
  splay_tree st = id->decl_map;
  splay_tree_node n;

  /* Only a local declaration (variable or label).  */
  if ((TREE_CODE (*tp) == VAR_DECL && !TREE_STATIC (*tp))
      || TREE_CODE (*tp) == LABEL_DECL)
    {
      /* Lookup the declaration.  */
      n = splay_tree_lookup (st, (splay_tree_key) *tp);

      /* If it's there, remap it.  */
      if (n)
	*tp = (tree) n->value;
    }

  else if (TREE_CODE (*tp) == STATEMENT_LIST)
    copy_statement_list (tp);
  else if (TREE_CODE (*tp) == BIND_EXPR)
    copy_bind_expr (tp, walk_subtrees, id);
  else if (TREE_CODE (*tp) == SAVE_EXPR)
    remap_save_expr (tp, st, walk_subtrees);
  else
    {
      copy_tree_r (tp, walk_subtrees, NULL);

      /* Do whatever unsaving is required.  */
      unsave_expr_1 (*tp);
    }

  /* Keep iterating.  */
  return NULL_TREE;
}

/* Copies everything in EXPR and replaces variables, labels
   and SAVE_EXPRs local to EXPR.  */

tree
unsave_expr_now (tree expr)
{
  inline_data id;

  /* There's nothing to do for NULL_TREE.  */
  if (expr == 0)
    return expr;

  /* Set up ID.  */
  memset (&id, 0, sizeof (id));
  VARRAY_TREE_INIT (id.fns, 1, "fns");
  VARRAY_PUSH_TREE (id.fns, current_function_decl);
  id.decl_map = splay_tree_new (splay_tree_compare_pointers, NULL, NULL);

  /* Walk the tree once to find local labels.  */
  walk_tree_without_duplicates (&expr, mark_local_for_remap_r, &id);

  /* Walk the tree again, copying, remapping, and unsaving.  */
  walk_tree (&expr, unsave_r, &id, NULL);

  /* Clean up.  */
  splay_tree_delete (id.decl_map);

  return expr;
}

/* Allow someone to determine if SEARCH is a child of TOP from gdb.  */

static tree
debug_find_tree_1 (tree *tp, int *walk_subtrees ATTRIBUTE_UNUSED, void *data)
{
  if (*tp == data)
    return (tree) data;
  else
    return NULL;
}

bool
debug_find_tree (tree top, tree search)
{
  return walk_tree_without_duplicates (&top, debug_find_tree_1, search) != 0;
}

/* Declare the variables created by the inliner.  Add all the variables in
   VARS to BIND_EXPR.  */

static void
declare_inline_vars (tree bind_expr, tree vars)
{
  tree t;
  for (t = vars; t; t = TREE_CHAIN (t))
    DECL_SEEN_IN_BIND_EXPR_P (t) = 1;

  add_var_to_bind_expr (bind_expr, vars);
}
