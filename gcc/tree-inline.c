/* Tree inlining.
   Copyright 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011
   Free Software Foundation, Inc.
   Contributed by Alexandre Oliva <aoliva@redhat.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "diagnostic-core.h"
#include "tree.h"
#include "tree-inline.h"
#include "flags.h"
#include "params.h"
#include "input.h"
#include "insn-config.h"
#include "hashtab.h"
#include "langhooks.h"
#include "basic-block.h"
#include "tree-iterator.h"
#include "cgraph.h"
#include "intl.h"
#include "tree-mudflap.h"
#include "tree-flow.h"
#include "function.h"
#include "tree-flow.h"
#include "tree-pretty-print.h"
#include "except.h"
#include "debug.h"
#include "pointer-set.h"
#include "ipa-prop.h"
#include "value-prof.h"
#include "tree-pass.h"
#include "target.h"
#include "integrate.h"

#include "rtl.h"	/* FIXME: For asm_str_count.  */

/* I'm not real happy about this, but we need to handle gimple and
   non-gimple trees.  */
#include "gimple.h"

/* Inlining, Cloning, Versioning, Parallelization

   Inlining: a function body is duplicated, but the PARM_DECLs are
   remapped into VAR_DECLs, and non-void RETURN_EXPRs become
   MODIFY_EXPRs that store to a dedicated returned-value variable.
   The duplicated eh_region info of the copy will later be appended
   to the info for the caller; the eh_region info in copied throwing
   statements and RESX statements are adjusted accordingly.

   Cloning: (only in C++) We have one body for a con/de/structor, and
   multiple function decls, each with a unique parameter list.
   Duplicate the body, using the given splay tree; some parameters
   will become constants (like 0 or 1).

   Versioning: a function body is duplicated and the result is a new
   function rather than into blocks of an existing function as with
   inlining.  Some parameters will become constants.

   Parallelization: a region of a function is duplicated resulting in
   a new function.  Variables may be replaced with complex expressions
   to enable shared variable semantics.

   All of these will simultaneously lookup any callgraph edges.  If
   we're going to inline the duplicated function body, and the given
   function has some cloned callgraph nodes (one for each place this
   function will be inlined) those callgraph edges will be duplicated.
   If we're cloning the body, those callgraph edges will be
   updated to point into the new body.  (Note that the original
   callgraph node and edge list will not be altered.)

   See the CALL_EXPR handling case in copy_tree_body_r ().  */

/* To Do:

   o In order to make inlining-on-trees work, we pessimized
     function-local static constants.  In particular, they are now
     always output, even when not addressed.  Fix this by treating
     function-local static constants just like global static
     constants; the back-end already knows not to output them if they
     are not needed.

   o Provide heuristics to clamp inlining of recursive template
     calls?  */


/* Weights that estimate_num_insns uses to estimate the size of the
   produced code.  */

eni_weights eni_size_weights;

/* Weights that estimate_num_insns uses to estimate the time necessary
   to execute the produced code.  */

eni_weights eni_time_weights;

/* Prototypes.  */

static tree declare_return_variable (copy_body_data *, tree, tree, basic_block);
static void remap_block (tree *, copy_body_data *);
static void copy_bind_expr (tree *, int *, copy_body_data *);
static tree mark_local_for_remap_r (tree *, int *, void *);
static void unsave_expr_1 (tree);
static tree unsave_r (tree *, int *, void *);
static void declare_inline_vars (tree, tree);
static void remap_save_expr (tree *, void *, int *);
static void prepend_lexical_block (tree current_block, tree new_block);
static tree copy_decl_to_var (tree, copy_body_data *);
static tree copy_result_decl_to_var (tree, copy_body_data *);
static tree copy_decl_maybe_to_var (tree, copy_body_data *);
static gimple remap_gimple_stmt (gimple, copy_body_data *);
static bool delete_unreachable_blocks_update_callgraph (copy_body_data *id);

/* Insert a tree->tree mapping for ID.  Despite the name suggests
   that the trees should be variables, it is used for more than that.  */

void
insert_decl_map (copy_body_data *id, tree key, tree value)
{
  *pointer_map_insert (id->decl_map, key) = value;

  /* Always insert an identity map as well.  If we see this same new
     node again, we won't want to duplicate it a second time.  */
  if (key != value)
    *pointer_map_insert (id->decl_map, value) = value;
}

/* Insert a tree->tree mapping for ID.  This is only used for
   variables.  */

static void
insert_debug_decl_map (copy_body_data *id, tree key, tree value)
{
  if (!gimple_in_ssa_p (id->src_cfun))
    return;

  if (!MAY_HAVE_DEBUG_STMTS)
    return;

  if (!target_for_debug_bind (key))
    return;

  gcc_assert (TREE_CODE (key) == PARM_DECL);
  gcc_assert (TREE_CODE (value) == VAR_DECL);

  if (!id->debug_map)
    id->debug_map = pointer_map_create ();

  *pointer_map_insert (id->debug_map, key) = value;
}

/* If nonzero, we're remapping the contents of inlined debug
   statements.  If negative, an error has occurred, such as a
   reference to a variable that isn't available in the inlined
   context.  */
static int processing_debug_stmt = 0;

/* Construct new SSA name for old NAME. ID is the inline context.  */

static tree
remap_ssa_name (tree name, copy_body_data *id)
{
  tree new_tree;
  tree *n;

  gcc_assert (TREE_CODE (name) == SSA_NAME);

  n = (tree *) pointer_map_contains (id->decl_map, name);
  if (n)
    return unshare_expr (*n);

  if (processing_debug_stmt)
    {
      if (TREE_CODE (SSA_NAME_VAR (name)) == PARM_DECL
	  && SSA_NAME_IS_DEFAULT_DEF (name)
	  && id->entry_bb == NULL
	  && single_succ_p (ENTRY_BLOCK_PTR))
	{
	  tree vexpr = make_node (DEBUG_EXPR_DECL);
	  gimple def_temp;
	  gimple_stmt_iterator gsi;
	  tree val = SSA_NAME_VAR (name);

	  n = (tree *) pointer_map_contains (id->decl_map, val);
	  if (n != NULL)
	    val = *n;
	  if (TREE_CODE (val) != PARM_DECL)
	    {
	      processing_debug_stmt = -1;
	      return name;
	    }
	  def_temp = gimple_build_debug_source_bind (vexpr, val, NULL);
	  DECL_ARTIFICIAL (vexpr) = 1;
	  TREE_TYPE (vexpr) = TREE_TYPE (name);
	  DECL_MODE (vexpr) = DECL_MODE (SSA_NAME_VAR (name));
	  gsi = gsi_after_labels (single_succ (ENTRY_BLOCK_PTR));
	  gsi_insert_before (&gsi, def_temp, GSI_SAME_STMT);
	  return vexpr;
	}

      processing_debug_stmt = -1;
      return name;
    }

  /* Do not set DEF_STMT yet as statement is not copied yet. We do that
     in copy_bb.  */
  new_tree = remap_decl (SSA_NAME_VAR (name), id);

  /* We might've substituted constant or another SSA_NAME for
     the variable.

     Replace the SSA name representing RESULT_DECL by variable during
     inlining:  this saves us from need to introduce PHI node in a case
     return value is just partly initialized.  */
  if ((TREE_CODE (new_tree) == VAR_DECL || TREE_CODE (new_tree) == PARM_DECL)
      && (TREE_CODE (SSA_NAME_VAR (name)) != RESULT_DECL
	  || !id->transform_return_to_modify))
    {
      struct ptr_info_def *pi;
      new_tree = make_ssa_name (new_tree, NULL);
      insert_decl_map (id, name, new_tree);
      SSA_NAME_OCCURS_IN_ABNORMAL_PHI (new_tree)
	= SSA_NAME_OCCURS_IN_ABNORMAL_PHI (name);
      TREE_TYPE (new_tree) = TREE_TYPE (SSA_NAME_VAR (new_tree));
      /* At least IPA points-to info can be directly transferred.  */
      if (id->src_cfun->gimple_df
	  && id->src_cfun->gimple_df->ipa_pta
	  && (pi = SSA_NAME_PTR_INFO (name))
	  && !pi->pt.anything)
	{
	  struct ptr_info_def *new_pi = get_ptr_info (new_tree);
	  new_pi->pt = pi->pt;
	}
      if (gimple_nop_p (SSA_NAME_DEF_STMT (name)))
	{
	  /* By inlining function having uninitialized variable, we might
	     extend the lifetime (variable might get reused).  This cause
	     ICE in the case we end up extending lifetime of SSA name across
	     abnormal edge, but also increase register pressure.

	     We simply initialize all uninitialized vars by 0 except
	     for case we are inlining to very first BB.  We can avoid
	     this for all BBs that are not inside strongly connected
	     regions of the CFG, but this is expensive to test.  */
	  if (id->entry_bb
	      && is_gimple_reg (SSA_NAME_VAR (name))
	      && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (name)
	      && TREE_CODE (SSA_NAME_VAR (name)) != PARM_DECL
	      && (id->entry_bb != EDGE_SUCC (ENTRY_BLOCK_PTR, 0)->dest
		  || EDGE_COUNT (id->entry_bb->preds) != 1))
	    {
	      gimple_stmt_iterator gsi = gsi_last_bb (id->entry_bb);
	      gimple init_stmt;
	      tree zero = build_zero_cst (TREE_TYPE (new_tree));

	      init_stmt = gimple_build_assign (new_tree, zero);
	      gsi_insert_after (&gsi, init_stmt, GSI_NEW_STMT);
	      SSA_NAME_IS_DEFAULT_DEF (new_tree) = 0;
	    }
	  else
	    {
	      SSA_NAME_DEF_STMT (new_tree) = gimple_build_nop ();
	      if (gimple_default_def (id->src_cfun, SSA_NAME_VAR (name))
		  == name)
	        set_default_def (SSA_NAME_VAR (new_tree), new_tree);
	    }
	}
    }
  else
    insert_decl_map (id, name, new_tree);
  return new_tree;
}

/* Remap DECL during the copying of the BLOCK tree for the function.  */

tree
remap_decl (tree decl, copy_body_data *id)
{
  tree *n;

  /* We only remap local variables in the current function.  */

  /* See if we have remapped this declaration.  */

  n = (tree *) pointer_map_contains (id->decl_map, decl);

  if (!n && processing_debug_stmt)
    {
      processing_debug_stmt = -1;
      return decl;
    }

  /* If we didn't already have an equivalent for this declaration,
     create one now.  */
  if (!n)
    {
      /* Make a copy of the variable or label.  */
      tree t = id->copy_decl (decl, id);

      /* Remember it, so that if we encounter this local entity again
	 we can reuse this copy.  Do this early because remap_type may
	 need this decl for TYPE_STUB_DECL.  */
      insert_decl_map (id, decl, t);

      if (!DECL_P (t))
	return t;

      /* Remap types, if necessary.  */
      TREE_TYPE (t) = remap_type (TREE_TYPE (t), id);
      if (TREE_CODE (t) == TYPE_DECL)
        DECL_ORIGINAL_TYPE (t) = remap_type (DECL_ORIGINAL_TYPE (t), id);

      /* Remap sizes as necessary.  */
      walk_tree (&DECL_SIZE (t), copy_tree_body_r, id, NULL);
      walk_tree (&DECL_SIZE_UNIT (t), copy_tree_body_r, id, NULL);

      /* If fields, do likewise for offset and qualifier.  */
      if (TREE_CODE (t) == FIELD_DECL)
	{
	  walk_tree (&DECL_FIELD_OFFSET (t), copy_tree_body_r, id, NULL);
	  if (TREE_CODE (DECL_CONTEXT (t)) == QUAL_UNION_TYPE)
	    walk_tree (&DECL_QUALIFIER (t), copy_tree_body_r, id, NULL);
	}

      if ((TREE_CODE (t) == VAR_DECL
	   || TREE_CODE (t) == RESULT_DECL
	   || TREE_CODE (t) == PARM_DECL)
	  && id->src_fn && DECL_STRUCT_FUNCTION (id->src_fn)
	  && gimple_referenced_vars (DECL_STRUCT_FUNCTION (id->src_fn))
	  /* We don't want to mark as referenced VAR_DECLs that were
	     not marked as such in the src function.  */
	  && (TREE_CODE (decl) != VAR_DECL
	      || referenced_var_lookup (DECL_STRUCT_FUNCTION (id->src_fn),
					DECL_UID (decl))))
	add_referenced_var (t);
      return t;
    }

  if (id->do_not_unshare)
    return *n;
  else
    return unshare_expr (*n);
}

static tree
remap_type_1 (tree type, copy_body_data *id)
{
  tree new_tree, t;

  /* We do need a copy.  build and register it now.  If this is a pointer or
     reference type, remap the designated type and make a new pointer or
     reference type.  */
  if (TREE_CODE (type) == POINTER_TYPE)
    {
      new_tree = build_pointer_type_for_mode (remap_type (TREE_TYPE (type), id),
					 TYPE_MODE (type),
					 TYPE_REF_CAN_ALIAS_ALL (type));
      if (TYPE_ATTRIBUTES (type) || TYPE_QUALS (type))
	new_tree = build_type_attribute_qual_variant (new_tree,
						      TYPE_ATTRIBUTES (type),
						      TYPE_QUALS (type));
      insert_decl_map (id, type, new_tree);
      return new_tree;
    }
  else if (TREE_CODE (type) == REFERENCE_TYPE)
    {
      new_tree = build_reference_type_for_mode (remap_type (TREE_TYPE (type), id),
					    TYPE_MODE (type),
					    TYPE_REF_CAN_ALIAS_ALL (type));
      if (TYPE_ATTRIBUTES (type) || TYPE_QUALS (type))
	new_tree = build_type_attribute_qual_variant (new_tree,
						      TYPE_ATTRIBUTES (type),
						      TYPE_QUALS (type));
      insert_decl_map (id, type, new_tree);
      return new_tree;
    }
  else
    new_tree = copy_node (type);

  insert_decl_map (id, type, new_tree);

  /* This is a new type, not a copy of an old type.  Need to reassociate
     variants.  We can handle everything except the main variant lazily.  */
  t = TYPE_MAIN_VARIANT (type);
  if (type != t)
    {
      t = remap_type (t, id);
      TYPE_MAIN_VARIANT (new_tree) = t;
      TYPE_NEXT_VARIANT (new_tree) = TYPE_NEXT_VARIANT (t);
      TYPE_NEXT_VARIANT (t) = new_tree;
    }
  else
    {
      TYPE_MAIN_VARIANT (new_tree) = new_tree;
      TYPE_NEXT_VARIANT (new_tree) = NULL;
    }

  if (TYPE_STUB_DECL (type))
    TYPE_STUB_DECL (new_tree) = remap_decl (TYPE_STUB_DECL (type), id);

  /* Lazily create pointer and reference types.  */
  TYPE_POINTER_TO (new_tree) = NULL;
  TYPE_REFERENCE_TO (new_tree) = NULL;

  switch (TREE_CODE (new_tree))
    {
    case INTEGER_TYPE:
    case REAL_TYPE:
    case FIXED_POINT_TYPE:
    case ENUMERAL_TYPE:
    case BOOLEAN_TYPE:
      t = TYPE_MIN_VALUE (new_tree);
      if (t && TREE_CODE (t) != INTEGER_CST)
        walk_tree (&TYPE_MIN_VALUE (new_tree), copy_tree_body_r, id, NULL);

      t = TYPE_MAX_VALUE (new_tree);
      if (t && TREE_CODE (t) != INTEGER_CST)
        walk_tree (&TYPE_MAX_VALUE (new_tree), copy_tree_body_r, id, NULL);
      return new_tree;

    case FUNCTION_TYPE:
      TREE_TYPE (new_tree) = remap_type (TREE_TYPE (new_tree), id);
      walk_tree (&TYPE_ARG_TYPES (new_tree), copy_tree_body_r, id, NULL);
      return new_tree;

    case ARRAY_TYPE:
      TREE_TYPE (new_tree) = remap_type (TREE_TYPE (new_tree), id);
      TYPE_DOMAIN (new_tree) = remap_type (TYPE_DOMAIN (new_tree), id);
      break;

    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      {
	tree f, nf = NULL;

	for (f = TYPE_FIELDS (new_tree); f ; f = DECL_CHAIN (f))
	  {
	    t = remap_decl (f, id);
	    DECL_CONTEXT (t) = new_tree;
	    DECL_CHAIN (t) = nf;
	    nf = t;
	  }
	TYPE_FIELDS (new_tree) = nreverse (nf);
      }
      break;

    case OFFSET_TYPE:
    default:
      /* Shouldn't have been thought variable sized.  */
      gcc_unreachable ();
    }

  walk_tree (&TYPE_SIZE (new_tree), copy_tree_body_r, id, NULL);
  walk_tree (&TYPE_SIZE_UNIT (new_tree), copy_tree_body_r, id, NULL);

  return new_tree;
}

tree
remap_type (tree type, copy_body_data *id)
{
  tree *node;
  tree tmp;

  if (type == NULL)
    return type;

  /* See if we have remapped this type.  */
  node = (tree *) pointer_map_contains (id->decl_map, type);
  if (node)
    return *node;

  /* The type only needs remapping if it's variably modified.  */
  if (! variably_modified_type_p (type, id->src_fn))
    {
      insert_decl_map (id, type, type);
      return type;
    }

  id->remapping_type_depth++;
  tmp = remap_type_1 (type, id);
  id->remapping_type_depth--;

  return tmp;
}

/* Return previously remapped type of TYPE in ID.  Return NULL if TYPE
   is NULL or TYPE has not been remapped before.  */

static tree
remapped_type (tree type, copy_body_data *id)
{
  tree *node;

  if (type == NULL)
    return type;

  /* See if we have remapped this type.  */
  node = (tree *) pointer_map_contains (id->decl_map, type);
  if (node)
    return *node;
  else
    return NULL;
}

  /* The type only needs remapping if it's variably modified.  */
/* Decide if DECL can be put into BLOCK_NONLOCAL_VARs.  */

static bool
can_be_nonlocal (tree decl, copy_body_data *id)
{
  /* We can not duplicate function decls.  */
  if (TREE_CODE (decl) == FUNCTION_DECL)
    return true;

  /* Local static vars must be non-local or we get multiple declaration
     problems.  */
  if (TREE_CODE (decl) == VAR_DECL
      && !auto_var_in_fn_p (decl, id->src_fn))
    return true;

  /* At the moment dwarf2out can handle only these types of nodes.  We
     can support more later.  */
  if (TREE_CODE (decl) != VAR_DECL && TREE_CODE (decl) != PARM_DECL)
    return false;

  /* We must use global type.  We call remapped_type instead of
     remap_type since we don't want to remap this type here if it
     hasn't been remapped before.  */
  if (TREE_TYPE (decl) != remapped_type (TREE_TYPE (decl), id))
    return false;

  /* Wihtout SSA we can't tell if variable is used.  */
  if (!gimple_in_ssa_p (cfun))
    return false;

  /* Live variables must be copied so we can attach DECL_RTL.  */
  if (var_ann (decl))
    return false;

  return true;
}

static tree
remap_decls (tree decls, VEC(tree,gc) **nonlocalized_list, copy_body_data *id)
{
  tree old_var;
  tree new_decls = NULL_TREE;

  /* Remap its variables.  */
  for (old_var = decls; old_var; old_var = DECL_CHAIN (old_var))
    {
      tree new_var;

      if (can_be_nonlocal (old_var, id))
	{
	  if (TREE_CODE (old_var) == VAR_DECL
	      && ! DECL_EXTERNAL (old_var)
	      && (var_ann (old_var) || !gimple_in_ssa_p (cfun)))
	    add_local_decl (cfun, old_var);
	  if ((!optimize || debug_info_level > DINFO_LEVEL_TERSE)
	      && !DECL_IGNORED_P (old_var)
	      && nonlocalized_list)
	    VEC_safe_push (tree, gc, *nonlocalized_list, old_var);
	  continue;
	}

      /* Remap the variable.  */
      new_var = remap_decl (old_var, id);

      /* If we didn't remap this variable, we can't mess with its
	 TREE_CHAIN.  If we remapped this variable to the return slot, it's
	 already declared somewhere else, so don't declare it here.  */

      if (new_var == id->retvar)
	;
      else if (!new_var)
        {
	  if ((!optimize || debug_info_level > DINFO_LEVEL_TERSE)
	      && !DECL_IGNORED_P (old_var)
	      && nonlocalized_list)
	    VEC_safe_push (tree, gc, *nonlocalized_list, old_var);
	}
      else
	{
	  gcc_assert (DECL_P (new_var));
	  DECL_CHAIN (new_var) = new_decls;
	  new_decls = new_var;
 
	  /* Also copy value-expressions.  */
	  if (TREE_CODE (new_var) == VAR_DECL
	      && DECL_HAS_VALUE_EXPR_P (new_var))
	    {
	      tree tem = DECL_VALUE_EXPR (new_var);
	      bool old_regimplify = id->regimplify;
	      id->remapping_type_depth++;
	      walk_tree (&tem, copy_tree_body_r, id, NULL);
	      id->remapping_type_depth--;
	      id->regimplify = old_regimplify;
	      SET_DECL_VALUE_EXPR (new_var, tem);
	    }
	}
    }

  return nreverse (new_decls);
}

/* Copy the BLOCK to contain remapped versions of the variables
   therein.  And hook the new block into the block-tree.  */

static void
remap_block (tree *block, copy_body_data *id)
{
  tree old_block;
  tree new_block;

  /* Make the new block.  */
  old_block = *block;
  new_block = make_node (BLOCK);
  TREE_USED (new_block) = TREE_USED (old_block);
  BLOCK_ABSTRACT_ORIGIN (new_block) = old_block;
  BLOCK_SOURCE_LOCATION (new_block) = BLOCK_SOURCE_LOCATION (old_block);
  BLOCK_NONLOCALIZED_VARS (new_block)
    = VEC_copy (tree, gc, BLOCK_NONLOCALIZED_VARS (old_block));
  *block = new_block;

  /* Remap its variables.  */
  BLOCK_VARS (new_block) = remap_decls (BLOCK_VARS (old_block),
  					&BLOCK_NONLOCALIZED_VARS (new_block),
					id);

  if (id->transform_lang_insert_block)
    id->transform_lang_insert_block (new_block);

  /* Remember the remapped block.  */
  insert_decl_map (id, old_block, new_block);
}

/* Copy the whole block tree and root it in id->block.  */
static tree
remap_blocks (tree block, copy_body_data *id)
{
  tree t;
  tree new_tree = block;

  if (!block)
    return NULL;

  remap_block (&new_tree, id);
  gcc_assert (new_tree != block);
  for (t = BLOCK_SUBBLOCKS (block); t ; t = BLOCK_CHAIN (t))
    prepend_lexical_block (new_tree, remap_blocks (t, id));
  /* Blocks are in arbitrary order, but make things slightly prettier and do
     not swap order when producing a copy.  */
  BLOCK_SUBBLOCKS (new_tree) = blocks_nreverse (BLOCK_SUBBLOCKS (new_tree));
  return new_tree;
}

static void
copy_statement_list (tree *tp)
{
  tree_stmt_iterator oi, ni;
  tree new_tree;

  new_tree = alloc_stmt_list ();
  ni = tsi_start (new_tree);
  oi = tsi_start (*tp);
  TREE_TYPE (new_tree) = TREE_TYPE (*tp);
  *tp = new_tree;

  for (; !tsi_end_p (oi); tsi_next (&oi))
    {
      tree stmt = tsi_stmt (oi);
      if (TREE_CODE (stmt) == STATEMENT_LIST)
	/* This copy is not redundant; tsi_link_after will smash this
	   STATEMENT_LIST into the end of the one we're building, and we
	   don't want to do that with the original.  */
	copy_statement_list (&stmt);
      tsi_link_after (&ni, stmt, TSI_CONTINUE_LINKING);
    }
}

static void
copy_bind_expr (tree *tp, int *walk_subtrees, copy_body_data *id)
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
    BIND_EXPR_VARS (*tp) = remap_decls (BIND_EXPR_VARS (*tp), NULL, id);
}


/* Create a new gimple_seq by remapping all the statements in BODY
   using the inlining information in ID.  */

static gimple_seq
remap_gimple_seq (gimple_seq body, copy_body_data *id)
{
  gimple_stmt_iterator si;
  gimple_seq new_body = NULL;

  for (si = gsi_start (body); !gsi_end_p (si); gsi_next (&si))
    {
      gimple new_stmt = remap_gimple_stmt (gsi_stmt (si), id);
      gimple_seq_add_stmt (&new_body, new_stmt);
    }

  return new_body;
}


/* Copy a GIMPLE_BIND statement STMT, remapping all the symbols in its
   block using the mapping information in ID.  */

static gimple
copy_gimple_bind (gimple stmt, copy_body_data *id)
{
  gimple new_bind;
  tree new_block, new_vars;
  gimple_seq body, new_body;

  /* Copy the statement.  Note that we purposely don't use copy_stmt
     here because we need to remap statements as we copy.  */
  body = gimple_bind_body (stmt);
  new_body = remap_gimple_seq (body, id);

  new_block = gimple_bind_block (stmt);
  if (new_block)
    remap_block (&new_block, id);

  /* This will remap a lot of the same decls again, but this should be
     harmless.  */
  new_vars = gimple_bind_vars (stmt);
  if (new_vars)
    new_vars = remap_decls (new_vars, NULL, id);

  new_bind = gimple_build_bind (new_vars, new_body, new_block);

  return new_bind;
}


/* Remap the GIMPLE operand pointed to by *TP.  DATA is really a
   'struct walk_stmt_info *'.  DATA->INFO is a 'copy_body_data *'.
   WALK_SUBTREES is used to indicate walk_gimple_op whether to keep
   recursing into the children nodes of *TP.  */

static tree
remap_gimple_op_r (tree *tp, int *walk_subtrees, void *data)
{
  struct walk_stmt_info *wi_p = (struct walk_stmt_info *) data;
  copy_body_data *id = (copy_body_data *) wi_p->info;
  tree fn = id->src_fn;

  if (TREE_CODE (*tp) == SSA_NAME)
    {
      *tp = remap_ssa_name (*tp, id);
      *walk_subtrees = 0;
      return NULL;
    }
  else if (auto_var_in_fn_p (*tp, fn))
    {
      /* Local variables and labels need to be replaced by equivalent
	 variables.  We don't want to copy static variables; there's
	 only one of those, no matter how many times we inline the
	 containing function.  Similarly for globals from an outer
	 function.  */
      tree new_decl;

      /* Remap the declaration.  */
      new_decl = remap_decl (*tp, id);
      gcc_assert (new_decl);
      /* Replace this variable with the copy.  */
      STRIP_TYPE_NOPS (new_decl);
      /* ???  The C++ frontend uses void * pointer zero to initialize
         any other type.  This confuses the middle-end type verification.
	 As cloned bodies do not go through gimplification again the fixup
	 there doesn't trigger.  */
      if (TREE_CODE (new_decl) == INTEGER_CST
	  && !useless_type_conversion_p (TREE_TYPE (*tp), TREE_TYPE (new_decl)))
	new_decl = fold_convert (TREE_TYPE (*tp), new_decl);
      *tp = new_decl;
      *walk_subtrees = 0;
    }
  else if (TREE_CODE (*tp) == STATEMENT_LIST)
    gcc_unreachable ();
  else if (TREE_CODE (*tp) == SAVE_EXPR)
    gcc_unreachable ();
  else if (TREE_CODE (*tp) == LABEL_DECL
	   && (!DECL_CONTEXT (*tp)
	       || decl_function_context (*tp) == id->src_fn))
    /* These may need to be remapped for EH handling.  */
    *tp = remap_decl (*tp, id);
  else if (TYPE_P (*tp))
    /* Types may need remapping as well.  */
    *tp = remap_type (*tp, id);
  else if (CONSTANT_CLASS_P (*tp))
    {
      /* If this is a constant, we have to copy the node iff the type
	 will be remapped.  copy_tree_r will not copy a constant.  */
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
  else
    {
      /* Otherwise, just copy the node.  Note that copy_tree_r already
	 knows not to copy VAR_DECLs, etc., so this is safe.  */

      /* We should never have TREE_BLOCK set on non-statements.  */
      if (EXPR_P (*tp))
	gcc_assert (!TREE_BLOCK (*tp));

      if (TREE_CODE (*tp) == MEM_REF)
	{
	  tree ptr = TREE_OPERAND (*tp, 0);
	  tree type = remap_type (TREE_TYPE (*tp), id);
	  tree old = *tp;

	  /* We need to re-canonicalize MEM_REFs from inline substitutions
	     that can happen when a pointer argument is an ADDR_EXPR.
	     Recurse here manually to allow that.  */
	  walk_tree (&ptr, remap_gimple_op_r, data, NULL);
	  *tp = fold_build2 (MEM_REF, type,
			     ptr, TREE_OPERAND (*tp, 1));
	  TREE_THIS_NOTRAP (*tp) = TREE_THIS_NOTRAP (old);
	  TREE_THIS_VOLATILE (*tp) = TREE_THIS_VOLATILE (old);
	  TREE_NO_WARNING (*tp) = TREE_NO_WARNING (old);
	  *walk_subtrees = 0;
	  return NULL;
	}

      /* Here is the "usual case".  Copy this tree node, and then
	 tweak some special cases.  */
      copy_tree_r (tp, walk_subtrees, NULL);

      if (TREE_CODE (*tp) != OMP_CLAUSE)
	TREE_TYPE (*tp) = remap_type (TREE_TYPE (*tp), id);

      /* Global variables we haven't seen yet need to go into referenced
	 vars.  If not referenced from types only.  */
      if (gimple_in_ssa_p (cfun)
	  && TREE_CODE (*tp) == VAR_DECL
	  && id->remapping_type_depth == 0
	  && !processing_debug_stmt)
	add_referenced_var (*tp);

      if (TREE_CODE (*tp) == TARGET_EXPR && TREE_OPERAND (*tp, 3))
	{
	  /* The copied TARGET_EXPR has never been expanded, even if the
	     original node was expanded already.  */
	  TREE_OPERAND (*tp, 1) = TREE_OPERAND (*tp, 3);
	  TREE_OPERAND (*tp, 3) = NULL_TREE;
	}
      else if (TREE_CODE (*tp) == ADDR_EXPR)
	{
	  /* Variable substitution need not be simple.  In particular,
	     the MEM_REF substitution above.  Make sure that
	     TREE_CONSTANT and friends are up-to-date.  But make sure
	     to not improperly set TREE_BLOCK on some sub-expressions.  */
	  int invariant = is_gimple_min_invariant (*tp);
	  tree block = id->block;
	  id->block = NULL_TREE;
	  walk_tree (&TREE_OPERAND (*tp, 0), remap_gimple_op_r, data, NULL);
	  id->block = block;
	  recompute_tree_invariant_for_addr_expr (*tp);

	  /* If this used to be invariant, but is not any longer,
	     then regimplification is probably needed.  */
	  if (invariant && !is_gimple_min_invariant (*tp))
	    id->regimplify = true;

	  *walk_subtrees = 0;
	}
    }

  /* Keep iterating.  */
  return NULL_TREE;
}


/* Called from copy_body_id via walk_tree.  DATA is really a
   `copy_body_data *'.  */

tree
copy_tree_body_r (tree *tp, int *walk_subtrees, void *data)
{
  copy_body_data *id = (copy_body_data *) data;
  tree fn = id->src_fn;
  tree new_block;

  /* Begin by recognizing trees that we'll completely rewrite for the
     inlining context.  Our output for these trees is completely
     different from out input (e.g. RETURN_EXPR is deleted, and morphs
     into an edge).  Further down, we'll handle trees that get
     duplicated and/or tweaked.  */

  /* When requested, RETURN_EXPRs should be transformed to just the
     contained MODIFY_EXPR.  The branch semantics of the return will
     be handled elsewhere by manipulating the CFG rather than a statement.  */
  if (TREE_CODE (*tp) == RETURN_EXPR && id->transform_return_to_modify)
    {
      tree assignment = TREE_OPERAND (*tp, 0);

      /* If we're returning something, just turn that into an
	 assignment into the equivalent of the original RESULT_DECL.
	 If the "assignment" is just the result decl, the result
	 decl has already been set (e.g. a recent "foo (&result_decl,
	 ...)"); just toss the entire RETURN_EXPR.  */
      if (assignment && TREE_CODE (assignment) == MODIFY_EXPR)
	{
	  /* Replace the RETURN_EXPR with (a copy of) the
	     MODIFY_EXPR hanging underneath.  */
	  *tp = copy_node (assignment);
	}
      else /* Else the RETURN_EXPR returns no value.  */
	{
	  *tp = NULL;
	  return (tree) (void *)1;
	}
    }
  else if (TREE_CODE (*tp) == SSA_NAME)
    {
      *tp = remap_ssa_name (*tp, id);
      *walk_subtrees = 0;
      return NULL;
    }

  /* Local variables and labels need to be replaced by equivalent
     variables.  We don't want to copy static variables; there's only
     one of those, no matter how many times we inline the containing
     function.  Similarly for globals from an outer function.  */
  else if (auto_var_in_fn_p (*tp, fn))
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
  else if (TREE_CODE (*tp) == SAVE_EXPR
	   || TREE_CODE (*tp) == TARGET_EXPR)
    remap_save_expr (tp, id->decl_map, walk_subtrees);
  else if (TREE_CODE (*tp) == LABEL_DECL
	   && (! DECL_CONTEXT (*tp)
	       || decl_function_context (*tp) == id->src_fn))
    /* These may need to be remapped for EH handling.  */
    *tp = remap_decl (*tp, id);
  else if (TREE_CODE (*tp) == BIND_EXPR)
    copy_bind_expr (tp, walk_subtrees, id);
  /* Types may need remapping as well.  */
  else if (TYPE_P (*tp))
    *tp = remap_type (*tp, id);

  /* If this is a constant, we have to copy the node iff the type will be
     remapped.  copy_tree_r will not copy a constant.  */
  else if (CONSTANT_CLASS_P (*tp))
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
      /* Here we handle trees that are not completely rewritten.
	 First we detect some inlining-induced bogosities for
	 discarding.  */
      if (TREE_CODE (*tp) == MODIFY_EXPR
	  && TREE_OPERAND (*tp, 0) == TREE_OPERAND (*tp, 1)
	  && (auto_var_in_fn_p (TREE_OPERAND (*tp, 0), fn)))
	{
	  /* Some assignments VAR = VAR; don't generate any rtl code
	     and thus don't count as variable modification.  Avoid
	     keeping bogosities like 0 = 0.  */
	  tree decl = TREE_OPERAND (*tp, 0), value;
	  tree *n;

	  n = (tree *) pointer_map_contains (id->decl_map, decl);
	  if (n)
	    {
	      value = *n;
	      STRIP_TYPE_NOPS (value);
	      if (TREE_CONSTANT (value) || TREE_READONLY (value))
		{
		  *tp = build_empty_stmt (EXPR_LOCATION (*tp));
		  return copy_tree_body_r (tp, walk_subtrees, data);
		}
	    }
	}
      else if (TREE_CODE (*tp) == INDIRECT_REF)
	{
	  /* Get rid of *& from inline substitutions that can happen when a
	     pointer argument is an ADDR_EXPR.  */
	  tree decl = TREE_OPERAND (*tp, 0);
	  tree *n;

	  n = (tree *) pointer_map_contains (id->decl_map, decl);
	  if (n)
	    {
	      tree new_tree;
	      tree old;
	      /* If we happen to get an ADDR_EXPR in n->value, strip
	         it manually here as we'll eventually get ADDR_EXPRs
		 which lie about their types pointed to.  In this case
		 build_fold_indirect_ref wouldn't strip the INDIRECT_REF,
		 but we absolutely rely on that.  As fold_indirect_ref
	         does other useful transformations, try that first, though.  */
	      tree type = TREE_TYPE (TREE_TYPE (*n));
	      if (id->do_not_unshare)
		new_tree = *n;
	      else
		new_tree = unshare_expr (*n);
	      old = *tp;
	      *tp = gimple_fold_indirect_ref (new_tree);
	      if (! *tp)
	        {
		  if (TREE_CODE (new_tree) == ADDR_EXPR)
		    {
		      *tp = fold_indirect_ref_1 (EXPR_LOCATION (new_tree),
						 type, new_tree);
		      /* ???  We should either assert here or build
			 a VIEW_CONVERT_EXPR instead of blindly leaking
			 incompatible types to our IL.  */
		      if (! *tp)
			*tp = TREE_OPERAND (new_tree, 0);
		    }
	          else
		    {
	              *tp = build1 (INDIRECT_REF, type, new_tree);
		      TREE_THIS_VOLATILE (*tp) = TREE_THIS_VOLATILE (old);
		      TREE_SIDE_EFFECTS (*tp) = TREE_SIDE_EFFECTS (old);
		      TREE_READONLY (*tp) = TREE_READONLY (old);
		      TREE_THIS_NOTRAP (*tp) = TREE_THIS_NOTRAP (old);
		    }
		}
	      *walk_subtrees = 0;
	      return NULL;
	    }
	}
      else if (TREE_CODE (*tp) == MEM_REF)
	{
	  /* We need to re-canonicalize MEM_REFs from inline substitutions
	     that can happen when a pointer argument is an ADDR_EXPR.  */
	  tree decl = TREE_OPERAND (*tp, 0);
	  tree *n;

	  n = (tree *) pointer_map_contains (id->decl_map, decl);
	  if (n)
	    {
	      tree old = *tp;
	      *tp = fold_build2 (MEM_REF, TREE_TYPE (*tp),
				 unshare_expr (*n), TREE_OPERAND (*tp, 1));
	      TREE_THIS_VOLATILE (*tp) = TREE_THIS_VOLATILE (old);
	      TREE_NO_WARNING (*tp) = TREE_NO_WARNING (old);
	      *walk_subtrees = 0;
	      return NULL;
	    }
	}

      /* Here is the "usual case".  Copy this tree node, and then
	 tweak some special cases.  */
      copy_tree_r (tp, walk_subtrees, NULL);

      /* Global variables we haven't seen yet needs to go into referenced
	 vars.  If not referenced from types or debug stmts only.  */
      if (gimple_in_ssa_p (cfun)
	  && TREE_CODE (*tp) == VAR_DECL
	  && id->remapping_type_depth == 0
	  && !processing_debug_stmt)
	add_referenced_var (*tp);

      /* If EXPR has block defined, map it to newly constructed block.
         When inlining we want EXPRs without block appear in the block
	 of function call if we are not remapping a type.  */
      if (EXPR_P (*tp))
	{
	  new_block = id->remapping_type_depth == 0 ? id->block : NULL;
	  if (TREE_BLOCK (*tp))
	    {
	      tree *n;
	      n = (tree *) pointer_map_contains (id->decl_map,
						 TREE_BLOCK (*tp));
	      gcc_assert (n || id->remapping_type_depth != 0);
	      if (n)
		new_block = *n;
	    }
	  TREE_BLOCK (*tp) = new_block;
	}

      if (TREE_CODE (*tp) != OMP_CLAUSE)
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
	  int invariant = is_gimple_min_invariant (*tp);
	  walk_tree (&TREE_OPERAND (*tp, 0), copy_tree_body_r, id, NULL);

	  /* Handle the case where we substituted an INDIRECT_REF
	     into the operand of the ADDR_EXPR.  */
	  if (TREE_CODE (TREE_OPERAND (*tp, 0)) == INDIRECT_REF)
	    *tp = TREE_OPERAND (TREE_OPERAND (*tp, 0), 0);
	  else
	    recompute_tree_invariant_for_addr_expr (*tp);

	  /* If this used to be invariant, but is not any longer,
	     then regimplification is probably needed.  */
	  if (invariant && !is_gimple_min_invariant (*tp))
	    id->regimplify = true;

	  *walk_subtrees = 0;
	}
    }

  /* Keep iterating.  */
  return NULL_TREE;
}

/* Helper for remap_gimple_stmt.  Given an EH region number for the
   source function, map that to the duplicate EH region number in
   the destination function.  */

static int
remap_eh_region_nr (int old_nr, copy_body_data *id)
{
  eh_region old_r, new_r;
  void **slot;

  old_r = get_eh_region_from_number_fn (id->src_cfun, old_nr);
  slot = pointer_map_contains (id->eh_map, old_r);
  new_r = (eh_region) *slot;

  return new_r->index;
}

/* Similar, but operate on INTEGER_CSTs.  */

static tree
remap_eh_region_tree_nr (tree old_t_nr, copy_body_data *id)
{
  int old_nr, new_nr;

  old_nr = tree_low_cst (old_t_nr, 0);
  new_nr = remap_eh_region_nr (old_nr, id);

  return build_int_cst (integer_type_node, new_nr);
}

/* Helper for copy_bb.  Remap statement STMT using the inlining
   information in ID.  Return the new statement copy.  */

static gimple
remap_gimple_stmt (gimple stmt, copy_body_data *id)
{
  gimple copy = NULL;
  struct walk_stmt_info wi;
  tree new_block;
  bool skip_first = false;

  /* Begin by recognizing trees that we'll completely rewrite for the
     inlining context.  Our output for these trees is completely
     different from out input (e.g. RETURN_EXPR is deleted, and morphs
     into an edge).  Further down, we'll handle trees that get
     duplicated and/or tweaked.  */

  /* When requested, GIMPLE_RETURNs should be transformed to just the
     contained GIMPLE_ASSIGN.  The branch semantics of the return will
     be handled elsewhere by manipulating the CFG rather than the
     statement.  */
  if (gimple_code (stmt) == GIMPLE_RETURN && id->transform_return_to_modify)
    {
      tree retval = gimple_return_retval (stmt);

      /* If we're returning something, just turn that into an
	 assignment into the equivalent of the original RESULT_DECL.
	 If RETVAL is just the result decl, the result decl has
	 already been set (e.g. a recent "foo (&result_decl, ...)");
	 just toss the entire GIMPLE_RETURN.  */
      if (retval
	  && (TREE_CODE (retval) != RESULT_DECL
	      && (TREE_CODE (retval) != SSA_NAME
		  || TREE_CODE (SSA_NAME_VAR (retval)) != RESULT_DECL)))
        {
	  copy = gimple_build_assign (id->retvar, retval);
	  /* id->retvar is already substituted.  Skip it on later remapping.  */
	  skip_first = true;
	}
      else
	return gimple_build_nop ();
    }
  else if (gimple_has_substatements (stmt))
    {
      gimple_seq s1, s2;

      /* When cloning bodies from the C++ front end, we will be handed bodies
	 in High GIMPLE form.  Handle here all the High GIMPLE statements that
	 have embedded statements.  */
      switch (gimple_code (stmt))
	{
	case GIMPLE_BIND:
	  copy = copy_gimple_bind (stmt, id);
	  break;

	case GIMPLE_CATCH:
	  s1 = remap_gimple_seq (gimple_catch_handler (stmt), id);
	  copy = gimple_build_catch (gimple_catch_types (stmt), s1);
	  break;

	case GIMPLE_EH_FILTER:
	  s1 = remap_gimple_seq (gimple_eh_filter_failure (stmt), id);
	  copy = gimple_build_eh_filter (gimple_eh_filter_types (stmt), s1);
	  break;

	case GIMPLE_TRY:
	  s1 = remap_gimple_seq (gimple_try_eval (stmt), id);
	  s2 = remap_gimple_seq (gimple_try_cleanup (stmt), id);
	  copy = gimple_build_try (s1, s2, gimple_try_kind (stmt));
	  break;

	case GIMPLE_WITH_CLEANUP_EXPR:
	  s1 = remap_gimple_seq (gimple_wce_cleanup (stmt), id);
	  copy = gimple_build_wce (s1);
	  break;

	case GIMPLE_OMP_PARALLEL:
	  s1 = remap_gimple_seq (gimple_omp_body (stmt), id);
	  copy = gimple_build_omp_parallel
	           (s1,
		    gimple_omp_parallel_clauses (stmt),
		    gimple_omp_parallel_child_fn (stmt),
		    gimple_omp_parallel_data_arg (stmt));
	  break;

	case GIMPLE_OMP_TASK:
	  s1 = remap_gimple_seq (gimple_omp_body (stmt), id);
	  copy = gimple_build_omp_task
	           (s1,
		    gimple_omp_task_clauses (stmt),
		    gimple_omp_task_child_fn (stmt),
		    gimple_omp_task_data_arg (stmt),
		    gimple_omp_task_copy_fn (stmt),
		    gimple_omp_task_arg_size (stmt),
		    gimple_omp_task_arg_align (stmt));
	  break;

	case GIMPLE_OMP_FOR:
	  s1 = remap_gimple_seq (gimple_omp_body (stmt), id);
	  s2 = remap_gimple_seq (gimple_omp_for_pre_body (stmt), id);
	  copy = gimple_build_omp_for (s1, gimple_omp_for_clauses (stmt),
				       gimple_omp_for_collapse (stmt), s2);
	  {
	    size_t i;
	    for (i = 0; i < gimple_omp_for_collapse (stmt); i++)
	      {
		gimple_omp_for_set_index (copy, i,
					  gimple_omp_for_index (stmt, i));
		gimple_omp_for_set_initial (copy, i,
					    gimple_omp_for_initial (stmt, i));
		gimple_omp_for_set_final (copy, i,
					  gimple_omp_for_final (stmt, i));
		gimple_omp_for_set_incr (copy, i,
					 gimple_omp_for_incr (stmt, i));
		gimple_omp_for_set_cond (copy, i,
					 gimple_omp_for_cond (stmt, i));
	      }
	  }
	  break;

	case GIMPLE_OMP_MASTER:
	  s1 = remap_gimple_seq (gimple_omp_body (stmt), id);
	  copy = gimple_build_omp_master (s1);
	  break;

	case GIMPLE_OMP_ORDERED:
	  s1 = remap_gimple_seq (gimple_omp_body (stmt), id);
	  copy = gimple_build_omp_ordered (s1);
	  break;

	case GIMPLE_OMP_SECTION:
	  s1 = remap_gimple_seq (gimple_omp_body (stmt), id);
	  copy = gimple_build_omp_section (s1);
	  break;

	case GIMPLE_OMP_SECTIONS:
	  s1 = remap_gimple_seq (gimple_omp_body (stmt), id);
	  copy = gimple_build_omp_sections
	           (s1, gimple_omp_sections_clauses (stmt));
	  break;

	case GIMPLE_OMP_SINGLE:
	  s1 = remap_gimple_seq (gimple_omp_body (stmt), id);
	  copy = gimple_build_omp_single
	           (s1, gimple_omp_single_clauses (stmt));
	  break;

	case GIMPLE_OMP_CRITICAL:
	  s1 = remap_gimple_seq (gimple_omp_body (stmt), id);
	  copy
	    = gimple_build_omp_critical (s1, gimple_omp_critical_name (stmt));
	  break;

	case GIMPLE_TRANSACTION:
	  s1 = remap_gimple_seq (gimple_transaction_body (stmt), id);
	  copy = gimple_build_transaction (s1, gimple_transaction_label (stmt));
	  gimple_transaction_set_subcode (copy, gimple_transaction_subcode (stmt));
	  break;

	default:
	  gcc_unreachable ();
	}
    }
  else
    {
      if (gimple_assign_copy_p (stmt)
	  && gimple_assign_lhs (stmt) == gimple_assign_rhs1 (stmt)
	  && auto_var_in_fn_p (gimple_assign_lhs (stmt), id->src_fn))
	{
	  /* Here we handle statements that are not completely rewritten.
	     First we detect some inlining-induced bogosities for
	     discarding.  */

	  /* Some assignments VAR = VAR; don't generate any rtl code
	     and thus don't count as variable modification.  Avoid
	     keeping bogosities like 0 = 0.  */
	  tree decl = gimple_assign_lhs (stmt), value;
	  tree *n;

	  n = (tree *) pointer_map_contains (id->decl_map, decl);
	  if (n)
	    {
	      value = *n;
	      STRIP_TYPE_NOPS (value);
	      if (TREE_CONSTANT (value) || TREE_READONLY (value))
		return gimple_build_nop ();
	    }
	}

      if (gimple_debug_bind_p (stmt))
	{
	  copy = gimple_build_debug_bind (gimple_debug_bind_get_var (stmt),
					  gimple_debug_bind_get_value (stmt),
					  stmt);
	  VEC_safe_push (gimple, heap, id->debug_stmts, copy);
	  return copy;
	}
      if (gimple_debug_source_bind_p (stmt))
	{
	  copy = gimple_build_debug_source_bind
		   (gimple_debug_source_bind_get_var (stmt),
		    gimple_debug_source_bind_get_value (stmt), stmt);
	  VEC_safe_push (gimple, heap, id->debug_stmts, copy);
	  return copy;
	}

      /* Create a new deep copy of the statement.  */
      copy = gimple_copy (stmt);

      /* Remap the region numbers for __builtin_eh_{pointer,filter},
	 RESX and EH_DISPATCH.  */
      if (id->eh_map)
	switch (gimple_code (copy))
	  {
	  case GIMPLE_CALL:
	    {
	      tree r, fndecl = gimple_call_fndecl (copy);
	      if (fndecl && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL)
		switch (DECL_FUNCTION_CODE (fndecl))
		  {
		  case BUILT_IN_EH_COPY_VALUES:
		    r = gimple_call_arg (copy, 1);
		    r = remap_eh_region_tree_nr (r, id);
		    gimple_call_set_arg (copy, 1, r);
		    /* FALLTHRU */

		  case BUILT_IN_EH_POINTER:
		  case BUILT_IN_EH_FILTER:
		    r = gimple_call_arg (copy, 0);
		    r = remap_eh_region_tree_nr (r, id);
		    gimple_call_set_arg (copy, 0, r);
		    break;

		  default:
		    break;
		  }

	      /* Reset alias info if we didn't apply measures to
		 keep it valid over inlining by setting DECL_PT_UID.  */
	      if (!id->src_cfun->gimple_df
		  || !id->src_cfun->gimple_df->ipa_pta)
		gimple_call_reset_alias_info (copy);
	    }
	    break;

	  case GIMPLE_RESX:
	    {
	      int r = gimple_resx_region (copy);
	      r = remap_eh_region_nr (r, id);
	      gimple_resx_set_region (copy, r);
	    }
	    break;

	  case GIMPLE_EH_DISPATCH:
	    {
	      int r = gimple_eh_dispatch_region (copy);
	      r = remap_eh_region_nr (r, id);
	      gimple_eh_dispatch_set_region (copy, r);
	    }
	    break;

	  default:
	    break;
	  }
    }

  /* If STMT has a block defined, map it to the newly constructed
     block.  When inlining we want statements without a block to
     appear in the block of the function call.  */
  new_block = id->block;
  if (gimple_block (copy))
    {
      tree *n;
      n = (tree *) pointer_map_contains (id->decl_map, gimple_block (copy));
      gcc_assert (n);
      new_block = *n;
    }

  gimple_set_block (copy, new_block);

  if (gimple_debug_bind_p (copy) || gimple_debug_source_bind_p (copy))
    return copy;

  /* Remap all the operands in COPY.  */
  memset (&wi, 0, sizeof (wi));
  wi.info = id;
  if (skip_first)
    walk_tree (gimple_op_ptr (copy, 1), remap_gimple_op_r, &wi, NULL);
  else
    walk_gimple_op (copy, remap_gimple_op_r, &wi);

  /* Clear the copied virtual operands.  We are not remapping them here
     but are going to recreate them from scratch.  */
  if (gimple_has_mem_ops (copy))
    {
      gimple_set_vdef (copy, NULL_TREE);
      gimple_set_vuse (copy, NULL_TREE);
    }

  return copy;
}


/* Copy basic block, scale profile accordingly.  Edges will be taken care of
   later  */

static basic_block
copy_bb (copy_body_data *id, basic_block bb, int frequency_scale,
         gcov_type count_scale)
{
  gimple_stmt_iterator gsi, copy_gsi, seq_gsi;
  basic_block copy_basic_block;
  tree decl;
  gcov_type freq;
  basic_block prev;

  /* Search for previous copied basic block.  */
  prev = bb->prev_bb;
  while (!prev->aux)
    prev = prev->prev_bb;

  /* create_basic_block() will append every new block to
     basic_block_info automatically.  */
  copy_basic_block = create_basic_block (NULL, (void *) 0,
                                         (basic_block) prev->aux);
  copy_basic_block->count = bb->count * count_scale / REG_BR_PROB_BASE;

  /* We are going to rebuild frequencies from scratch.  These values
     have just small importance to drive canonicalize_loop_headers.  */
  freq = ((gcov_type)bb->frequency * frequency_scale / REG_BR_PROB_BASE);

  /* We recompute frequencies after inlining, so this is quite safe.  */
  if (freq > BB_FREQ_MAX)
    freq = BB_FREQ_MAX;
  copy_basic_block->frequency = freq;

  copy_gsi = gsi_start_bb (copy_basic_block);

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple stmt = gsi_stmt (gsi);
      gimple orig_stmt = stmt;

      id->regimplify = false;
      stmt = remap_gimple_stmt (stmt, id);
      if (gimple_nop_p (stmt))
	continue;

      gimple_duplicate_stmt_histograms (cfun, stmt, id->src_cfun, orig_stmt);
      seq_gsi = copy_gsi;

      /* With return slot optimization we can end up with
	 non-gimple (foo *)&this->m, fix that here.  */
      if (is_gimple_assign (stmt)
	  && gimple_assign_rhs_code (stmt) == NOP_EXPR
	  && !is_gimple_val (gimple_assign_rhs1 (stmt)))
	{
	  tree new_rhs;
	  new_rhs = force_gimple_operand_gsi (&seq_gsi,
					      gimple_assign_rhs1 (stmt),
					      true, NULL, false,
					      GSI_CONTINUE_LINKING);
	  gimple_assign_set_rhs1 (stmt, new_rhs);
	  id->regimplify = false;
	}

      gsi_insert_after (&seq_gsi, stmt, GSI_NEW_STMT);

      if (id->regimplify)
	gimple_regimplify_operands (stmt, &seq_gsi);

      /* If copy_basic_block has been empty at the start of this iteration,
	 call gsi_start_bb again to get at the newly added statements.  */
      if (gsi_end_p (copy_gsi))
	copy_gsi = gsi_start_bb (copy_basic_block);
      else
	gsi_next (&copy_gsi);

      /* Process the new statement.  The call to gimple_regimplify_operands
	 possibly turned the statement into multiple statements, we
	 need to process all of them.  */
      do
	{
	  tree fn;

	  stmt = gsi_stmt (copy_gsi);
	  if (is_gimple_call (stmt)
	      && gimple_call_va_arg_pack_p (stmt)
	      && id->gimple_call)
	    {
	      /* __builtin_va_arg_pack () should be replaced by
		 all arguments corresponding to ... in the caller.  */
	      tree p;
	      gimple new_call;
	      VEC(tree, heap) *argarray;
	      size_t nargs = gimple_call_num_args (id->gimple_call);
	      size_t n;

	      for (p = DECL_ARGUMENTS (id->src_fn); p; p = DECL_CHAIN (p))
		nargs--;

	      /* Create the new array of arguments.  */
	      n = nargs + gimple_call_num_args (stmt);
	      argarray = VEC_alloc (tree, heap, n);
	      VEC_safe_grow (tree, heap, argarray, n);

	      /* Copy all the arguments before '...'  */
	      memcpy (VEC_address (tree, argarray),
		      gimple_call_arg_ptr (stmt, 0),
		      gimple_call_num_args (stmt) * sizeof (tree));

	      /* Append the arguments passed in '...'  */
	      memcpy (VEC_address(tree, argarray) + gimple_call_num_args (stmt),
		      gimple_call_arg_ptr (id->gimple_call, 0)
			+ (gimple_call_num_args (id->gimple_call) - nargs),
		      nargs * sizeof (tree));

	      new_call = gimple_build_call_vec (gimple_call_fn (stmt),
						argarray);

	      VEC_free (tree, heap, argarray);

	      /* Copy all GIMPLE_CALL flags, location and block, except
		 GF_CALL_VA_ARG_PACK.  */
	      gimple_call_copy_flags (new_call, stmt);
	      gimple_call_set_va_arg_pack (new_call, false);
	      gimple_set_location (new_call, gimple_location (stmt));
	      gimple_set_block (new_call, gimple_block (stmt));
	      gimple_call_set_lhs (new_call, gimple_call_lhs (stmt));

	      gsi_replace (&copy_gsi, new_call, false);
	      stmt = new_call;
	    }
	  else if (is_gimple_call (stmt)
		   && id->gimple_call
		   && (decl = gimple_call_fndecl (stmt))
		   && DECL_BUILT_IN_CLASS (decl) == BUILT_IN_NORMAL
		   && DECL_FUNCTION_CODE (decl) == BUILT_IN_VA_ARG_PACK_LEN)
	    {
	      /* __builtin_va_arg_pack_len () should be replaced by
		 the number of anonymous arguments.  */
	      size_t nargs = gimple_call_num_args (id->gimple_call);
	      tree count, p;
	      gimple new_stmt;

	      for (p = DECL_ARGUMENTS (id->src_fn); p; p = DECL_CHAIN (p))
		nargs--;

	      count = build_int_cst (integer_type_node, nargs);
	      new_stmt = gimple_build_assign (gimple_call_lhs (stmt), count);
	      gsi_replace (&copy_gsi, new_stmt, false);
	      stmt = new_stmt;
	    }

	  /* Statements produced by inlining can be unfolded, especially
	     when we constant propagated some operands.  We can't fold
	     them right now for two reasons:
	     1) folding require SSA_NAME_DEF_STMTs to be correct
	     2) we can't change function calls to builtins.
	     So we just mark statement for later folding.  We mark
	     all new statements, instead just statements that has changed
	     by some nontrivial substitution so even statements made
	     foldable indirectly are updated.  If this turns out to be
	     expensive, copy_body can be told to watch for nontrivial
	     changes.  */
	  if (id->statements_to_fold)
	    pointer_set_insert (id->statements_to_fold, stmt);

	  /* We're duplicating a CALL_EXPR.  Find any corresponding
	     callgraph edges and update or duplicate them.  */
	  if (is_gimple_call (stmt))
	    {
	      struct cgraph_edge *edge;
	      int flags;

	      switch (id->transform_call_graph_edges)
		{
		case CB_CGE_DUPLICATE:
		  edge = cgraph_edge (id->src_node, orig_stmt);
		  if (edge)
		    {
		      int edge_freq = edge->frequency;
		      edge = cgraph_clone_edge (edge, id->dst_node, stmt,
					        gimple_uid (stmt),
					        REG_BR_PROB_BASE, CGRAPH_FREQ_BASE,
					        true);
		      /* We could also just rescale the frequency, but
		         doing so would introduce roundoff errors and make
			 verifier unhappy.  */
		      edge->frequency
		        = compute_call_stmt_bb_frequency (id->dst_node->decl,
							  copy_basic_block);
		      if (dump_file
		      	  && profile_status_for_function (cfun) != PROFILE_ABSENT
			  && (edge_freq > edge->frequency + 10
			      || edge_freq < edge->frequency - 10))
			{
			  fprintf (dump_file, "Edge frequency estimated by "
			           "cgraph %i diverge from inliner's estimate %i\n",
			  	   edge_freq,
				   edge->frequency);
			  fprintf (dump_file,
			  	   "Orig bb: %i, orig bb freq %i, new bb freq %i\n",
				   bb->index,
				   bb->frequency,
				   copy_basic_block->frequency);
			}
		      stmt = cgraph_redirect_edge_call_stmt_to_callee (edge);
		    }
		  break;

		case CB_CGE_MOVE_CLONES:
		  cgraph_set_call_stmt_including_clones (id->dst_node,
							 orig_stmt, stmt);
		  edge = cgraph_edge (id->dst_node, stmt);
		  break;

		case CB_CGE_MOVE:
		  edge = cgraph_edge (id->dst_node, orig_stmt);
		  if (edge)
		    cgraph_set_call_stmt (edge, stmt);
		  break;

		default:
		  gcc_unreachable ();
		}

	      /* Constant propagation on argument done during inlining
		 may create new direct call.  Produce an edge for it.  */
	      if ((!edge
		   || (edge->indirect_inlining_edge
		       && id->transform_call_graph_edges == CB_CGE_MOVE_CLONES))
		  && id->dst_node->analyzed
		  && (fn = gimple_call_fndecl (stmt)) != NULL)
		{
		  struct cgraph_node *dest = cgraph_get_node (fn);

		  /* We have missing edge in the callgraph.  This can happen
		     when previous inlining turned an indirect call into a
		     direct call by constant propagating arguments or we are
		     producing dead clone (for further cloning).  In all
		     other cases we hit a bug (incorrect node sharing is the
		     most common reason for missing edges).  */
		  gcc_assert (dest->needed || !dest->analyzed
			      || dest->address_taken
		  	      || !id->src_node->analyzed
			      || !id->dst_node->analyzed);
		  if (id->transform_call_graph_edges == CB_CGE_MOVE_CLONES)
		    cgraph_create_edge_including_clones
		      (id->dst_node, dest, orig_stmt, stmt, bb->count,
		       compute_call_stmt_bb_frequency (id->dst_node->decl,
		       				       copy_basic_block),
		       CIF_ORIGINALLY_INDIRECT_CALL);
		  else
		    cgraph_create_edge (id->dst_node, dest, stmt,
					bb->count,
					compute_call_stmt_bb_frequency
					  (id->dst_node->decl, copy_basic_block))->inline_failed
		      = CIF_ORIGINALLY_INDIRECT_CALL;
		  if (dump_file)
		    {
		      fprintf (dump_file, "Created new direct edge to %s\n",
			       cgraph_node_name (dest));
		    }
		}

	      flags = gimple_call_flags (stmt);
	      if (flags & ECF_MAY_BE_ALLOCA)
		cfun->calls_alloca = true;
	      if (flags & ECF_RETURNS_TWICE)
		cfun->calls_setjmp = true;
	    }

	  maybe_duplicate_eh_stmt_fn (cfun, stmt, id->src_cfun, orig_stmt,
				      id->eh_map, id->eh_lp_nr);

	  if (gimple_in_ssa_p (cfun) && !is_gimple_debug (stmt))
	    {
	      ssa_op_iter i;
	      tree def;

	      find_new_referenced_vars (gsi_stmt (copy_gsi));
	      FOR_EACH_SSA_TREE_OPERAND (def, stmt, i, SSA_OP_DEF)
		if (TREE_CODE (def) == SSA_NAME)
		  SSA_NAME_DEF_STMT (def) = stmt;
	    }

	  gsi_next (&copy_gsi);
	}
      while (!gsi_end_p (copy_gsi));

      copy_gsi = gsi_last_bb (copy_basic_block);
    }

  return copy_basic_block;
}

/* Inserting Single Entry Multiple Exit region in SSA form into code in SSA
   form is quite easy, since dominator relationship for old basic blocks does
   not change.

   There is however exception where inlining might change dominator relation
   across EH edges from basic block within inlined functions destinating
   to landing pads in function we inline into.

   The function fills in PHI_RESULTs of such PHI nodes if they refer
   to gimple regs.  Otherwise, the function mark PHI_RESULT of such
   PHI nodes for renaming.  For non-gimple regs, renaming is safe: the
   EH edges are abnormal and SSA_NAME_OCCURS_IN_ABNORMAL_PHI must be
   set, and this means that there will be no overlapping live ranges
   for the underlying symbol.

   This might change in future if we allow redirecting of EH edges and
   we might want to change way build CFG pre-inlining to include
   all the possible edges then.  */
static void
update_ssa_across_abnormal_edges (basic_block bb, basic_block ret_bb,
				  bool can_throw, bool nonlocal_goto)
{
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, bb->succs)
    if (!e->dest->aux
	|| ((basic_block)e->dest->aux)->index == ENTRY_BLOCK)
      {
	gimple phi;
	gimple_stmt_iterator si;

	if (!nonlocal_goto)
	  gcc_assert (e->flags & EDGE_EH);

	if (!can_throw)
	  gcc_assert (!(e->flags & EDGE_EH));

	for (si = gsi_start_phis (e->dest); !gsi_end_p (si); gsi_next (&si))
	  {
	    edge re;

	    phi = gsi_stmt (si);

	    /* There shouldn't be any PHI nodes in the ENTRY_BLOCK.  */
	    gcc_assert (!e->dest->aux);

	    gcc_assert ((e->flags & EDGE_EH)
			|| SSA_NAME_OCCURS_IN_ABNORMAL_PHI (PHI_RESULT (phi)));

	    if (!is_gimple_reg (PHI_RESULT (phi)))
	      {
		mark_sym_for_renaming (SSA_NAME_VAR (PHI_RESULT (phi)));
		continue;
	      }

	    re = find_edge (ret_bb, e->dest);
	    gcc_assert (re);
	    gcc_assert ((re->flags & (EDGE_EH | EDGE_ABNORMAL))
			== (e->flags & (EDGE_EH | EDGE_ABNORMAL)));

	    SET_USE (PHI_ARG_DEF_PTR_FROM_EDGE (phi, e),
		     USE_FROM_PTR (PHI_ARG_DEF_PTR_FROM_EDGE (phi, re)));
	  }
      }
}


/* Copy edges from BB into its copy constructed earlier, scale profile
   accordingly.  Edges will be taken care of later.  Assume aux
   pointers to point to the copies of each BB.  Return true if any
   debug stmts are left after a statement that must end the basic block.  */

static bool
copy_edges_for_bb (basic_block bb, gcov_type count_scale, basic_block ret_bb)
{
  basic_block new_bb = (basic_block) bb->aux;
  edge_iterator ei;
  edge old_edge;
  gimple_stmt_iterator si;
  int flags;
  bool need_debug_cleanup = false;

  /* Use the indices from the original blocks to create edges for the
     new ones.  */
  FOR_EACH_EDGE (old_edge, ei, bb->succs)
    if (!(old_edge->flags & EDGE_EH))
      {
	edge new_edge;

	flags = old_edge->flags;

	/* Return edges do get a FALLTHRU flag when the get inlined.  */
	if (old_edge->dest->index == EXIT_BLOCK && !old_edge->flags
	    && old_edge->dest->aux != EXIT_BLOCK_PTR)
	  flags |= EDGE_FALLTHRU;
	new_edge = make_edge (new_bb, (basic_block) old_edge->dest->aux, flags);
	new_edge->count = old_edge->count * count_scale / REG_BR_PROB_BASE;
	new_edge->probability = old_edge->probability;
      }

  if (bb->index == ENTRY_BLOCK || bb->index == EXIT_BLOCK)
    return false;

  for (si = gsi_start_bb (new_bb); !gsi_end_p (si);)
    {
      gimple copy_stmt;
      bool can_throw, nonlocal_goto;

      copy_stmt = gsi_stmt (si);
      if (!is_gimple_debug (copy_stmt))
	{
	  update_stmt (copy_stmt);
	  if (gimple_in_ssa_p (cfun))
	    mark_symbols_for_renaming (copy_stmt);
	}

      /* Do this before the possible split_block.  */
      gsi_next (&si);

      /* If this tree could throw an exception, there are two
         cases where we need to add abnormal edge(s): the
         tree wasn't in a region and there is a "current
         region" in the caller; or the original tree had
         EH edges.  In both cases split the block after the tree,
         and add abnormal edge(s) as needed; we need both
         those from the callee and the caller.
         We check whether the copy can throw, because the const
         propagation can change an INDIRECT_REF which throws
         into a COMPONENT_REF which doesn't.  If the copy
         can throw, the original could also throw.  */
      can_throw = stmt_can_throw_internal (copy_stmt);
      nonlocal_goto = stmt_can_make_abnormal_goto (copy_stmt);

      if (can_throw || nonlocal_goto)
	{
	  if (!gsi_end_p (si))
	    {
	      while (!gsi_end_p (si) && is_gimple_debug (gsi_stmt (si)))
		gsi_next (&si);
	      if (gsi_end_p (si))
		need_debug_cleanup = true;
	    }
	  if (!gsi_end_p (si))
	    /* Note that bb's predecessor edges aren't necessarily
	       right at this point; split_block doesn't care.  */
	    {
	      edge e = split_block (new_bb, copy_stmt);

	      new_bb = e->dest;
	      new_bb->aux = e->src->aux;
	      si = gsi_start_bb (new_bb);
	    }
	}

      if (gimple_code (copy_stmt) == GIMPLE_EH_DISPATCH)
	make_eh_dispatch_edges (copy_stmt);
      else if (can_throw)
	make_eh_edges (copy_stmt);

      if (nonlocal_goto)
	make_abnormal_goto_edges (gimple_bb (copy_stmt), true);

      if ((can_throw || nonlocal_goto)
	  && gimple_in_ssa_p (cfun))
	update_ssa_across_abnormal_edges (gimple_bb (copy_stmt), ret_bb,
					  can_throw, nonlocal_goto);
    }
  return need_debug_cleanup;
}

/* Copy the PHIs.  All blocks and edges are copied, some blocks
   was possibly split and new outgoing EH edges inserted.
   BB points to the block of original function and AUX pointers links
   the original and newly copied blocks.  */

static void
copy_phis_for_bb (basic_block bb, copy_body_data *id)
{
  basic_block const new_bb = (basic_block) bb->aux;
  edge_iterator ei;
  gimple phi;
  gimple_stmt_iterator si;
  edge new_edge;
  bool inserted = false;

  for (si = gsi_start (phi_nodes (bb)); !gsi_end_p (si); gsi_next (&si))
    {
      tree res, new_res;
      gimple new_phi;

      phi = gsi_stmt (si);
      res = PHI_RESULT (phi);
      new_res = res;
      if (is_gimple_reg (res))
	{
	  walk_tree (&new_res, copy_tree_body_r, id, NULL);
	  SSA_NAME_DEF_STMT (new_res)
	    = new_phi = create_phi_node (new_res, new_bb);
	  FOR_EACH_EDGE (new_edge, ei, new_bb->preds)
	    {
	      edge old_edge = find_edge ((basic_block) new_edge->src->aux, bb);
	      tree arg;
	      tree new_arg;
	      tree block = id->block;
	      edge_iterator ei2;

	      /* When doing partial cloning, we allow PHIs on the entry block
		 as long as all the arguments are the same.  Find any input
		 edge to see argument to copy.  */
	      if (!old_edge)
		FOR_EACH_EDGE (old_edge, ei2, bb->preds)
		  if (!old_edge->src->aux)
		    break;

	      arg = PHI_ARG_DEF_FROM_EDGE (phi, old_edge);
	      new_arg = arg;
	      id->block = NULL_TREE;
	      walk_tree (&new_arg, copy_tree_body_r, id, NULL);
	      id->block = block;
	      gcc_assert (new_arg);
	      /* With return slot optimization we can end up with
	         non-gimple (foo *)&this->m, fix that here.  */
	      if (TREE_CODE (new_arg) != SSA_NAME
		  && TREE_CODE (new_arg) != FUNCTION_DECL
		  && !is_gimple_val (new_arg))
		{
		  gimple_seq stmts = NULL;
		  new_arg = force_gimple_operand (new_arg, &stmts, true, NULL);
		  gsi_insert_seq_on_edge (new_edge, stmts);
		  inserted = true;
		}
	      add_phi_arg (new_phi, new_arg, new_edge,
			   gimple_phi_arg_location_from_edge (phi, old_edge));
	    }
	}
    }

  /* Commit the delayed edge insertions.  */
  if (inserted)
    FOR_EACH_EDGE (new_edge, ei, new_bb->preds)
      gsi_commit_one_edge_insert (new_edge, NULL);
}


/* Wrapper for remap_decl so it can be used as a callback.  */

static tree
remap_decl_1 (tree decl, void *data)
{
  return remap_decl (decl, (copy_body_data *) data);
}

/* Build struct function and associated datastructures for the new clone
   NEW_FNDECL to be build.  CALLEE_FNDECL is the original */

static void
initialize_cfun (tree new_fndecl, tree callee_fndecl, gcov_type count)
{
  struct function *src_cfun = DECL_STRUCT_FUNCTION (callee_fndecl);
  gcov_type count_scale;

  if (ENTRY_BLOCK_PTR_FOR_FUNCTION (src_cfun)->count)
    count_scale = (REG_BR_PROB_BASE * count
		   / ENTRY_BLOCK_PTR_FOR_FUNCTION (src_cfun)->count);
  else
    count_scale = REG_BR_PROB_BASE;

  /* Register specific tree functions.  */
  gimple_register_cfg_hooks ();

  /* Get clean struct function.  */
  push_struct_function (new_fndecl);

  /* We will rebuild these, so just sanity check that they are empty.  */
  gcc_assert (VALUE_HISTOGRAMS (cfun) == NULL);
  gcc_assert (cfun->local_decls == NULL);
  gcc_assert (cfun->cfg == NULL);
  gcc_assert (cfun->decl == new_fndecl);

  /* Copy items we preserve during cloning.  */
  cfun->static_chain_decl = src_cfun->static_chain_decl;
  cfun->nonlocal_goto_save_area = src_cfun->nonlocal_goto_save_area;
  cfun->function_end_locus = src_cfun->function_end_locus;
  cfun->curr_properties = src_cfun->curr_properties;
  cfun->last_verified = src_cfun->last_verified;
  cfun->va_list_gpr_size = src_cfun->va_list_gpr_size;
  cfun->va_list_fpr_size = src_cfun->va_list_fpr_size;
  cfun->has_nonlocal_label = src_cfun->has_nonlocal_label;
  cfun->stdarg = src_cfun->stdarg;
  cfun->after_inlining = src_cfun->after_inlining;
  cfun->can_throw_non_call_exceptions
    = src_cfun->can_throw_non_call_exceptions;
  cfun->returns_struct = src_cfun->returns_struct;
  cfun->returns_pcc_struct = src_cfun->returns_pcc_struct;
  cfun->after_tree_profile = src_cfun->after_tree_profile;

  init_empty_tree_cfg ();

  profile_status_for_function (cfun) = profile_status_for_function (src_cfun);
  ENTRY_BLOCK_PTR->count =
    (ENTRY_BLOCK_PTR_FOR_FUNCTION (src_cfun)->count * count_scale /
     REG_BR_PROB_BASE);
  ENTRY_BLOCK_PTR->frequency
    = ENTRY_BLOCK_PTR_FOR_FUNCTION (src_cfun)->frequency;
  EXIT_BLOCK_PTR->count =
    (EXIT_BLOCK_PTR_FOR_FUNCTION (src_cfun)->count * count_scale /
     REG_BR_PROB_BASE);
  EXIT_BLOCK_PTR->frequency =
    EXIT_BLOCK_PTR_FOR_FUNCTION (src_cfun)->frequency;
  if (src_cfun->eh)
    init_eh_for_function ();

  if (src_cfun->gimple_df)
    {
      init_tree_ssa (cfun);
      cfun->gimple_df->in_ssa_p = true;
      init_ssa_operands ();
    }
  pop_cfun ();
}

/* Helper function for copy_cfg_body.  Move debug stmts from the end
   of NEW_BB to the beginning of successor basic blocks when needed.  If the
   successor has multiple predecessors, reset them, otherwise keep
   their value.  */

static void
maybe_move_debug_stmts_to_successors (copy_body_data *id, basic_block new_bb)
{
  edge e;
  edge_iterator ei;
  gimple_stmt_iterator si = gsi_last_nondebug_bb (new_bb);

  if (gsi_end_p (si)
      || gsi_one_before_end_p (si)
      || !(stmt_can_throw_internal (gsi_stmt (si))
	   || stmt_can_make_abnormal_goto (gsi_stmt (si))))
    return;

  FOR_EACH_EDGE (e, ei, new_bb->succs)
    {
      gimple_stmt_iterator ssi = gsi_last_bb (new_bb);
      gimple_stmt_iterator dsi = gsi_after_labels (e->dest);
      while (is_gimple_debug (gsi_stmt (ssi)))
	{
	  gimple stmt = gsi_stmt (ssi), new_stmt;
	  tree var;
	  tree value;

	  /* For the last edge move the debug stmts instead of copying
	     them.  */
	  if (ei_one_before_end_p (ei))
	    {
	      si = ssi;
	      gsi_prev (&ssi);
	      if (!single_pred_p (e->dest) && gimple_debug_bind_p (stmt))
		gimple_debug_bind_reset_value (stmt);
	      gsi_remove (&si, false);
	      gsi_insert_before (&dsi, stmt, GSI_SAME_STMT);
	      continue;
	    }

	  if (gimple_debug_bind_p (stmt))
	    {
	      var = gimple_debug_bind_get_var (stmt);
	      if (single_pred_p (e->dest))
		{
		  value = gimple_debug_bind_get_value (stmt);
		  value = unshare_expr (value);
		}
	      else
		value = NULL_TREE;
	      new_stmt = gimple_build_debug_bind (var, value, stmt);
	    }
	  else if (gimple_debug_source_bind_p (stmt))
	    {
	      var = gimple_debug_source_bind_get_var (stmt);
	      value = gimple_debug_source_bind_get_value (stmt);
	      new_stmt = gimple_build_debug_source_bind (var, value, stmt);
	    }
	  else
	    gcc_unreachable ();
	  gsi_insert_before (&dsi, new_stmt, GSI_SAME_STMT);
	  VEC_safe_push (gimple, heap, id->debug_stmts, new_stmt);
	  gsi_prev (&ssi);
	}
    }
}

/* Make a copy of the body of FN so that it can be inserted inline in
   another function.  Walks FN via CFG, returns new fndecl.  */

static tree
copy_cfg_body (copy_body_data * id, gcov_type count, int frequency_scale,
	       basic_block entry_block_map, basic_block exit_block_map,
	       bitmap blocks_to_copy, basic_block new_entry)
{
  tree callee_fndecl = id->src_fn;
  /* Original cfun for the callee, doesn't change.  */
  struct function *src_cfun = DECL_STRUCT_FUNCTION (callee_fndecl);
  struct function *cfun_to_copy;
  basic_block bb;
  tree new_fndecl = NULL;
  bool need_debug_cleanup = false;
  gcov_type count_scale;
  int last;
  int incoming_frequency = 0;
  gcov_type incoming_count = 0;

  if (ENTRY_BLOCK_PTR_FOR_FUNCTION (src_cfun)->count)
    count_scale = (REG_BR_PROB_BASE * count
		   / ENTRY_BLOCK_PTR_FOR_FUNCTION (src_cfun)->count);
  else
    count_scale = REG_BR_PROB_BASE;

  /* Register specific tree functions.  */
  gimple_register_cfg_hooks ();

  /* If we are inlining just region of the function, make sure to connect new entry
     to ENTRY_BLOCK_PTR.  Since new entry can be part of loop, we must compute
     frequency and probability of ENTRY_BLOCK_PTR based on the frequencies and
     probabilities of edges incoming from nonduplicated region.  */
  if (new_entry)
    {
      edge e;
      edge_iterator ei;

      FOR_EACH_EDGE (e, ei, new_entry->preds)
	if (!e->src->aux)
	  {
	    incoming_frequency += EDGE_FREQUENCY (e);
	    incoming_count += e->count;
	  }
      incoming_count = incoming_count * count_scale / REG_BR_PROB_BASE;
      incoming_frequency
	= incoming_frequency * frequency_scale / REG_BR_PROB_BASE;
      ENTRY_BLOCK_PTR->count = incoming_count;
      ENTRY_BLOCK_PTR->frequency = incoming_frequency;
    }

  /* Must have a CFG here at this point.  */
  gcc_assert (ENTRY_BLOCK_PTR_FOR_FUNCTION
	      (DECL_STRUCT_FUNCTION (callee_fndecl)));

  cfun_to_copy = id->src_cfun = DECL_STRUCT_FUNCTION (callee_fndecl);

  ENTRY_BLOCK_PTR_FOR_FUNCTION (cfun_to_copy)->aux = entry_block_map;
  EXIT_BLOCK_PTR_FOR_FUNCTION (cfun_to_copy)->aux = exit_block_map;
  entry_block_map->aux = ENTRY_BLOCK_PTR_FOR_FUNCTION (cfun_to_copy);
  exit_block_map->aux = EXIT_BLOCK_PTR_FOR_FUNCTION (cfun_to_copy);

  /* Duplicate any exception-handling regions.  */
  if (cfun->eh)
    id->eh_map = duplicate_eh_regions (cfun_to_copy, NULL, id->eh_lp_nr,
				       remap_decl_1, id);

  /* Use aux pointers to map the original blocks to copy.  */
  FOR_EACH_BB_FN (bb, cfun_to_copy)
    if (!blocks_to_copy || bitmap_bit_p (blocks_to_copy, bb->index))
      {
	basic_block new_bb = copy_bb (id, bb, frequency_scale, count_scale);
	bb->aux = new_bb;
	new_bb->aux = bb;
      }

  last = last_basic_block;

  /* Now that we've duplicated the blocks, duplicate their edges.  */
  FOR_ALL_BB_FN (bb, cfun_to_copy)
    if (!blocks_to_copy
        || (bb->index > 0 && bitmap_bit_p (blocks_to_copy, bb->index)))
      need_debug_cleanup |= copy_edges_for_bb (bb, count_scale, exit_block_map);

  if (new_entry)
    {
      edge e = make_edge (entry_block_map, (basic_block)new_entry->aux, EDGE_FALLTHRU);
      e->probability = REG_BR_PROB_BASE;
      e->count = incoming_count;
    }

  if (gimple_in_ssa_p (cfun))
    FOR_ALL_BB_FN (bb, cfun_to_copy)
      if (!blocks_to_copy
	  || (bb->index > 0 && bitmap_bit_p (blocks_to_copy, bb->index)))
	copy_phis_for_bb (bb, id);

  FOR_ALL_BB_FN (bb, cfun_to_copy)
    if (bb->aux)
      {
	if (need_debug_cleanup
	    && bb->index != ENTRY_BLOCK
	    && bb->index != EXIT_BLOCK)
	  maybe_move_debug_stmts_to_successors (id, (basic_block) bb->aux);
	((basic_block)bb->aux)->aux = NULL;
	bb->aux = NULL;
      }

  /* Zero out AUX fields of newly created block during EH edge
     insertion. */
  for (; last < last_basic_block; last++)
    {
      if (need_debug_cleanup)
	maybe_move_debug_stmts_to_successors (id, BASIC_BLOCK (last));
      BASIC_BLOCK (last)->aux = NULL;
    }
  entry_block_map->aux = NULL;
  exit_block_map->aux = NULL;

  if (id->eh_map)
    {
      pointer_map_destroy (id->eh_map);
      id->eh_map = NULL;
    }

  return new_fndecl;
}

/* Copy the debug STMT using ID.  We deal with these statements in a
   special way: if any variable in their VALUE expression wasn't
   remapped yet, we won't remap it, because that would get decl uids
   out of sync, causing codegen differences between -g and -g0.  If
   this arises, we drop the VALUE expression altogether.  */

static void
copy_debug_stmt (gimple stmt, copy_body_data *id)
{
  tree t, *n;
  struct walk_stmt_info wi;

  t = id->block;
  if (gimple_block (stmt))
    {
      n = (tree *) pointer_map_contains (id->decl_map, gimple_block (stmt));
      if (n)
	t = *n;
    }
  gimple_set_block (stmt, t);

  /* Remap all the operands in COPY.  */
  memset (&wi, 0, sizeof (wi));
  wi.info = id;

  processing_debug_stmt = 1;

  if (gimple_debug_source_bind_p (stmt))
    t = gimple_debug_source_bind_get_var (stmt);
  else
    t = gimple_debug_bind_get_var (stmt);

  if (TREE_CODE (t) == PARM_DECL && id->debug_map
      && (n = (tree *) pointer_map_contains (id->debug_map, t)))
    {
      gcc_assert (TREE_CODE (*n) == VAR_DECL);
      t = *n;
    }
  else if (TREE_CODE (t) == VAR_DECL
	   && !TREE_STATIC (t)
	   && gimple_in_ssa_p (cfun)
	   && !pointer_map_contains (id->decl_map, t)
	   && !var_ann (t))
    /* T is a non-localized variable.  */;
  else
    walk_tree (&t, remap_gimple_op_r, &wi, NULL);

  if (gimple_debug_bind_p (stmt))
    {
      gimple_debug_bind_set_var (stmt, t);

      if (gimple_debug_bind_has_value_p (stmt))
	walk_tree (gimple_debug_bind_get_value_ptr (stmt),
		   remap_gimple_op_r, &wi, NULL);

      /* Punt if any decl couldn't be remapped.  */
      if (processing_debug_stmt < 0)
	gimple_debug_bind_reset_value (stmt);
    }
  else if (gimple_debug_source_bind_p (stmt))
    {
      gimple_debug_source_bind_set_var (stmt, t);
      walk_tree (gimple_debug_source_bind_get_value_ptr (stmt),
		 remap_gimple_op_r, &wi, NULL);
    }

  processing_debug_stmt = 0;

  update_stmt (stmt);
  if (gimple_in_ssa_p (cfun))
    mark_symbols_for_renaming (stmt);
}

/* Process deferred debug stmts.  In order to give values better odds
   of being successfully remapped, we delay the processing of debug
   stmts until all other stmts that might require remapping are
   processed.  */

static void
copy_debug_stmts (copy_body_data *id)
{
  size_t i;
  gimple stmt;

  if (!id->debug_stmts)
    return;

  FOR_EACH_VEC_ELT (gimple, id->debug_stmts, i, stmt)
    copy_debug_stmt (stmt, id);

  VEC_free (gimple, heap, id->debug_stmts);
}

/* Make a copy of the body of SRC_FN so that it can be inserted inline in
   another function.  */

static tree
copy_tree_body (copy_body_data *id)
{
  tree fndecl = id->src_fn;
  tree body = DECL_SAVED_TREE (fndecl);

  walk_tree (&body, copy_tree_body_r, id, NULL);

  return body;
}

/* Make a copy of the body of FN so that it can be inserted inline in
   another function.  */

static tree
copy_body (copy_body_data *id, gcov_type count, int frequency_scale,
	   basic_block entry_block_map, basic_block exit_block_map,
	   bitmap blocks_to_copy, basic_block new_entry)
{
  tree fndecl = id->src_fn;
  tree body;

  /* If this body has a CFG, walk CFG and copy.  */
  gcc_assert (ENTRY_BLOCK_PTR_FOR_FUNCTION (DECL_STRUCT_FUNCTION (fndecl)));
  body = copy_cfg_body (id, count, frequency_scale, entry_block_map, exit_block_map,
		        blocks_to_copy, new_entry);
  copy_debug_stmts (id);

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

  return var && auto_var_in_fn_p (var, fn);
}

/* Append to BB a debug annotation that binds VAR to VALUE, inheriting
   lexical block and line number information from base_stmt, if given,
   or from the last stmt of the block otherwise.  */

static gimple
insert_init_debug_bind (copy_body_data *id,
			basic_block bb, tree var, tree value,
			gimple base_stmt)
{
  gimple note;
  gimple_stmt_iterator gsi;
  tree tracked_var;

  if (!gimple_in_ssa_p (id->src_cfun))
    return NULL;

  if (!MAY_HAVE_DEBUG_STMTS)
    return NULL;

  tracked_var = target_for_debug_bind (var);
  if (!tracked_var)
    return NULL;

  if (bb)
    {
      gsi = gsi_last_bb (bb);
      if (!base_stmt && !gsi_end_p (gsi))
	base_stmt = gsi_stmt (gsi);
    }

  note = gimple_build_debug_bind (tracked_var, value, base_stmt);

  if (bb)
    {
      if (!gsi_end_p (gsi))
	gsi_insert_after (&gsi, note, GSI_SAME_STMT);
      else
	gsi_insert_before (&gsi, note, GSI_SAME_STMT);
    }

  return note;
}

static void
insert_init_stmt (copy_body_data *id, basic_block bb, gimple init_stmt)
{
  /* If VAR represents a zero-sized variable, it's possible that the
     assignment statement may result in no gimple statements.  */
  if (init_stmt)
    {
      gimple_stmt_iterator si = gsi_last_bb (bb);

      /* We can end up with init statements that store to a non-register
         from a rhs with a conversion.  Handle that here by forcing the
	 rhs into a temporary.  gimple_regimplify_operands is not
	 prepared to do this for us.  */
      if (!is_gimple_debug (init_stmt)
	  && !is_gimple_reg (gimple_assign_lhs (init_stmt))
	  && is_gimple_reg_type (TREE_TYPE (gimple_assign_lhs (init_stmt)))
	  && gimple_assign_rhs_class (init_stmt) == GIMPLE_UNARY_RHS)
	{
	  tree rhs = build1 (gimple_assign_rhs_code (init_stmt),
			     gimple_expr_type (init_stmt),
			     gimple_assign_rhs1 (init_stmt));
	  rhs = force_gimple_operand_gsi (&si, rhs, true, NULL_TREE, false,
					  GSI_NEW_STMT);
	  gimple_assign_set_rhs_code (init_stmt, TREE_CODE (rhs));
	  gimple_assign_set_rhs1 (init_stmt, rhs);
	}
      gsi_insert_after (&si, init_stmt, GSI_NEW_STMT);
      gimple_regimplify_operands (init_stmt, &si);
      mark_symbols_for_renaming (init_stmt);

      if (!is_gimple_debug (init_stmt) && MAY_HAVE_DEBUG_STMTS)
	{
	  tree var, def = gimple_assign_lhs (init_stmt);

	  if (TREE_CODE (def) == SSA_NAME)
	    var = SSA_NAME_VAR (def);
	  else
	    var = def;

	  insert_init_debug_bind (id, bb, var, def, init_stmt);
	}
    }
}

/* Initialize parameter P with VALUE.  If needed, produce init statement
   at the end of BB.  When BB is NULL, we return init statement to be
   output later.  */
static gimple
setup_one_parameter (copy_body_data *id, tree p, tree value, tree fn,
		     basic_block bb, tree *vars)
{
  gimple init_stmt = NULL;
  tree var;
  tree rhs = value;
  tree def = (gimple_in_ssa_p (cfun)
	      ? gimple_default_def (id->src_cfun, p) : NULL);

  if (value
      && value != error_mark_node
      && !useless_type_conversion_p (TREE_TYPE (p), TREE_TYPE (value)))
    {
      /* If we can match up types by promotion/demotion do so.  */
      if (fold_convertible_p (TREE_TYPE (p), value))
	rhs = fold_convert (TREE_TYPE (p), value);
      else
	{
	  /* ???  For valid programs we should not end up here.
	     Still if we end up with truly mismatched types here, fall back
	     to using a VIEW_CONVERT_EXPR or a literal zero to not leak invalid
	     GIMPLE to the following passes.  */
	  if (!is_gimple_reg_type (TREE_TYPE (value))
	      || TYPE_SIZE (TREE_TYPE (p)) == TYPE_SIZE (TREE_TYPE (value)))
	    rhs = fold_build1 (VIEW_CONVERT_EXPR, TREE_TYPE (p), value);
	  else
	    rhs = build_zero_cst (TREE_TYPE (p));
	}
    }

  /* Make an equivalent VAR_DECL.  Note that we must NOT remap the type
     here since the type of this decl must be visible to the calling
     function.  */
  var = copy_decl_to_var (p, id);

  /* We're actually using the newly-created var.  */
  if (gimple_in_ssa_p (cfun) && TREE_CODE (var) == VAR_DECL)
    add_referenced_var (var);

  /* Declare this new variable.  */
  DECL_CHAIN (var) = *vars;
  *vars = var;

  /* Make gimplifier happy about this variable.  */
  DECL_SEEN_IN_BIND_EXPR_P (var) = 1;

  /* If the parameter is never assigned to, has no SSA_NAMEs created,
     we would not need to create a new variable here at all, if it
     weren't for debug info.  Still, we can just use the argument
     value.  */
  if (TREE_READONLY (p)
      && !TREE_ADDRESSABLE (p)
      && value && !TREE_SIDE_EFFECTS (value)
      && !def)
    {
      /* We may produce non-gimple trees by adding NOPs or introduce
	 invalid sharing when operand is not really constant.
	 It is not big deal to prohibit constant propagation here as
	 we will constant propagate in DOM1 pass anyway.  */
      if (is_gimple_min_invariant (value)
	  && useless_type_conversion_p (TREE_TYPE (p),
						 TREE_TYPE (value))
	  /* We have to be very careful about ADDR_EXPR.  Make sure
	     the base variable isn't a local variable of the inlined
	     function, e.g., when doing recursive inlining, direct or
	     mutually-recursive or whatever, which is why we don't
	     just test whether fn == current_function_decl.  */
	  && ! self_inlining_addr_expr (value, fn))
	{
	  insert_decl_map (id, p, value);
	  insert_debug_decl_map (id, p, var);
	  return insert_init_debug_bind (id, bb, var, value, NULL);
	}
    }

  /* Register the VAR_DECL as the equivalent for the PARM_DECL;
     that way, when the PARM_DECL is encountered, it will be
     automatically replaced by the VAR_DECL.  */
  insert_decl_map (id, p, var);

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

  /* If there is no setup required and we are in SSA, take the easy route
     replacing all SSA names representing the function parameter by the
     SSA name passed to function.

     We need to construct map for the variable anyway as it might be used
     in different SSA names when parameter is set in function.

     Do replacement at -O0 for const arguments replaced by constant.
     This is important for builtin_constant_p and other construct requiring
     constant argument to be visible in inlined function body.  */
  if (gimple_in_ssa_p (cfun) && rhs && def && is_gimple_reg (p)
      && (optimize
          || (TREE_READONLY (p)
	      && is_gimple_min_invariant (rhs)))
      && (TREE_CODE (rhs) == SSA_NAME
	  || is_gimple_min_invariant (rhs))
      && !SSA_NAME_OCCURS_IN_ABNORMAL_PHI (def))
    {
      insert_decl_map (id, def, rhs);
      return insert_init_debug_bind (id, bb, var, rhs, NULL);
    }

  /* If the value of argument is never used, don't care about initializing
     it.  */
  if (optimize && gimple_in_ssa_p (cfun) && !def && is_gimple_reg (p))
    {
      gcc_assert (!value || !TREE_SIDE_EFFECTS (value));
      return insert_init_debug_bind (id, bb, var, rhs, NULL);
    }

  /* Initialize this VAR_DECL from the equivalent argument.  Convert
     the argument to the proper type in case it was promoted.  */
  if (value)
    {
      if (rhs == error_mark_node)
	{
	  insert_decl_map (id, p, var);
	  return insert_init_debug_bind (id, bb, var, rhs, NULL);
	}

      STRIP_USELESS_TYPE_CONVERSION (rhs);

      /* We want to use MODIFY_EXPR, not INIT_EXPR here so that we
	 keep our trees in gimple form.  */
      if (def && gimple_in_ssa_p (cfun) && is_gimple_reg (p))
	{
	  def = remap_ssa_name (def, id);
          init_stmt = gimple_build_assign (def, rhs);
	  SSA_NAME_IS_DEFAULT_DEF (def) = 0;
	  set_default_def (var, NULL);
	}
      else
        init_stmt = gimple_build_assign (var, rhs);

      if (bb && init_stmt)
        insert_init_stmt (id, bb, init_stmt);
    }
  return init_stmt;
}

/* Generate code to initialize the parameters of the function at the
   top of the stack in ID from the GIMPLE_CALL STMT.  */

static void
initialize_inlined_parameters (copy_body_data *id, gimple stmt,
			       tree fn, basic_block bb)
{
  tree parms;
  size_t i;
  tree p;
  tree vars = NULL_TREE;
  tree static_chain = gimple_call_chain (stmt);

  /* Figure out what the parameters are.  */
  parms = DECL_ARGUMENTS (fn);

  /* Loop through the parameter declarations, replacing each with an
     equivalent VAR_DECL, appropriately initialized.  */
  for (p = parms, i = 0; p; p = DECL_CHAIN (p), i++)
    {
      tree val;
      val = i < gimple_call_num_args (stmt) ? gimple_call_arg (stmt, i) : NULL;
      setup_one_parameter (id, p, val, fn, bb, &vars);
    }
  /* After remapping parameters remap their types.  This has to be done
     in a second loop over all parameters to appropriately remap
     variable sized arrays when the size is specified in a
     parameter following the array.  */
  for (p = parms, i = 0; p; p = DECL_CHAIN (p), i++)
    {
      tree *varp = (tree *) pointer_map_contains (id->decl_map, p);
      if (varp
	  && TREE_CODE (*varp) == VAR_DECL)
	{
	  tree def = (gimple_in_ssa_p (cfun) && is_gimple_reg (p)
		      ? gimple_default_def (id->src_cfun, p) : NULL);
	  tree var = *varp;
	  TREE_TYPE (var) = remap_type (TREE_TYPE (var), id);
	  /* Also remap the default definition if it was remapped
	     to the default definition of the parameter replacement
	     by the parameter setup.  */
	  if (def)
	    {
	      tree *defp = (tree *) pointer_map_contains (id->decl_map, def);
	      if (defp
		  && TREE_CODE (*defp) == SSA_NAME
		  && SSA_NAME_VAR (*defp) == var)
		TREE_TYPE (*defp) = TREE_TYPE (var);
	    }
	}
    }

  /* Initialize the static chain.  */
  p = DECL_STRUCT_FUNCTION (fn)->static_chain_decl;
  gcc_assert (fn != current_function_decl);
  if (p)
    {
      /* No static chain?  Seems like a bug in tree-nested.c.  */
      gcc_assert (static_chain);

      setup_one_parameter (id, p, static_chain, fn, bb, &vars);
    }

  declare_inline_vars (id->block, vars);
}


/* Declare a return variable to replace the RESULT_DECL for the
   function we are calling.  An appropriate DECL_STMT is returned.
   The USE_STMT is filled to contain a use of the declaration to
   indicate the return value of the function.

   RETURN_SLOT, if non-null is place where to store the result.  It
   is set only for CALL_EXPR_RETURN_SLOT_OPT.  MODIFY_DEST, if non-null,
   was the LHS of the MODIFY_EXPR to which this call is the RHS.

   The return value is a (possibly null) value that holds the result
   as seen by the caller.  */

static tree
declare_return_variable (copy_body_data *id, tree return_slot, tree modify_dest,
			 basic_block entry_bb)
{
  tree callee = id->src_fn;
  tree result = DECL_RESULT (callee);
  tree callee_type = TREE_TYPE (result);
  tree caller_type;
  tree var, use;

  /* Handle type-mismatches in the function declaration return type
     vs. the call expression.  */
  if (modify_dest)
    caller_type = TREE_TYPE (modify_dest);
  else
    caller_type = TREE_TYPE (TREE_TYPE (callee));

  /* We don't need to do anything for functions that don't return
     anything.  */
  if (!result || VOID_TYPE_P (callee_type))
    return NULL_TREE;

  /* If there was a return slot, then the return value is the
     dereferenced address of that object.  */
  if (return_slot)
    {
      /* The front end shouldn't have used both return_slot and
	 a modify expression.  */
      gcc_assert (!modify_dest);
      if (DECL_BY_REFERENCE (result))
	{
	  tree return_slot_addr = build_fold_addr_expr (return_slot);
	  STRIP_USELESS_TYPE_CONVERSION (return_slot_addr);

	  /* We are going to construct *&return_slot and we can't do that
	     for variables believed to be not addressable.

	     FIXME: This check possibly can match, because values returned
	     via return slot optimization are not believed to have address
	     taken by alias analysis.  */
	  gcc_assert (TREE_CODE (return_slot) != SSA_NAME);
	  var = return_slot_addr;
	}
      else
	{
	  var = return_slot;
	  gcc_assert (TREE_CODE (var) != SSA_NAME);
	  TREE_ADDRESSABLE (var) |= TREE_ADDRESSABLE (result);
	}
      if ((TREE_CODE (TREE_TYPE (result)) == COMPLEX_TYPE
           || TREE_CODE (TREE_TYPE (result)) == VECTOR_TYPE)
	  && !DECL_GIMPLE_REG_P (result)
	  && DECL_P (var))
	DECL_GIMPLE_REG_P (var) = 0;
      use = NULL;
      goto done;
    }

  /* All types requiring non-trivial constructors should have been handled.  */
  gcc_assert (!TREE_ADDRESSABLE (callee_type));

  /* Attempt to avoid creating a new temporary variable.  */
  if (modify_dest
      && TREE_CODE (modify_dest) != SSA_NAME)
    {
      bool use_it = false;

      /* We can't use MODIFY_DEST if there's type promotion involved.  */
      if (!useless_type_conversion_p (callee_type, caller_type))
	use_it = false;

      /* ??? If we're assigning to a variable sized type, then we must
	 reuse the destination variable, because we've no good way to
	 create variable sized temporaries at this point.  */
      else if (TREE_CODE (TYPE_SIZE_UNIT (caller_type)) != INTEGER_CST)
	use_it = true;

      /* If the callee cannot possibly modify MODIFY_DEST, then we can
	 reuse it as the result of the call directly.  Don't do this if
	 it would promote MODIFY_DEST to addressable.  */
      else if (TREE_ADDRESSABLE (result))
	use_it = false;
      else
	{
	  tree base_m = get_base_address (modify_dest);

	  /* If the base isn't a decl, then it's a pointer, and we don't
	     know where that's going to go.  */
	  if (!DECL_P (base_m))
	    use_it = false;
	  else if (is_global_var (base_m))
	    use_it = false;
	  else if ((TREE_CODE (TREE_TYPE (result)) == COMPLEX_TYPE
		    || TREE_CODE (TREE_TYPE (result)) == VECTOR_TYPE)
		   && !DECL_GIMPLE_REG_P (result)
		   && DECL_GIMPLE_REG_P (base_m))
	    use_it = false;
	  else if (!TREE_ADDRESSABLE (base_m))
	    use_it = true;
	}

      if (use_it)
	{
	  var = modify_dest;
	  use = NULL;
	  goto done;
	}
    }

  gcc_assert (TREE_CODE (TYPE_SIZE_UNIT (callee_type)) == INTEGER_CST);

  var = copy_result_decl_to_var (result, id);
  if (gimple_in_ssa_p (cfun))
    add_referenced_var (var);

  DECL_SEEN_IN_BIND_EXPR_P (var) = 1;

  /* Do not have the rest of GCC warn about this variable as it should
     not be visible to the user.  */
  TREE_NO_WARNING (var) = 1;

  declare_inline_vars (id->block, var);

  /* Build the use expr.  If the return type of the function was
     promoted, convert it back to the expected type.  */
  use = var;
  if (!useless_type_conversion_p (caller_type, TREE_TYPE (var)))
    {
      /* If we can match up types by promotion/demotion do so.  */
      if (fold_convertible_p (caller_type, var))
	use = fold_convert (caller_type, var);
      else
	{
	  /* ???  For valid programs we should not end up here.
	     Still if we end up with truly mismatched types here, fall back
	     to using a MEM_REF to not leak invalid GIMPLE to the following
	     passes.  */
	  /* Prevent var from being written into SSA form.  */
	  if (TREE_CODE (TREE_TYPE (var)) == VECTOR_TYPE
	      || TREE_CODE (TREE_TYPE (var)) == COMPLEX_TYPE)
	    DECL_GIMPLE_REG_P (var) = false;
	  else if (is_gimple_reg_type (TREE_TYPE (var)))
	    TREE_ADDRESSABLE (var) = true;
	  use = fold_build2 (MEM_REF, caller_type,
			     build_fold_addr_expr (var),
			     build_int_cst (ptr_type_node, 0));
	}
    }

  STRIP_USELESS_TYPE_CONVERSION (use);

  if (DECL_BY_REFERENCE (result))
    {
      TREE_ADDRESSABLE (var) = 1;
      var = build_fold_addr_expr (var);
    }

 done:
  /* Register the VAR_DECL as the equivalent for the RESULT_DECL; that
     way, when the RESULT_DECL is encountered, it will be
     automatically replaced by the VAR_DECL.  

     When returning by reference, ensure that RESULT_DECL remaps to
     gimple_val.  */
  if (DECL_BY_REFERENCE (result)
      && !is_gimple_val (var))
    {
      tree temp = create_tmp_var (TREE_TYPE (result), "retvalptr");
      if (gimple_in_ssa_p (id->src_cfun))
	add_referenced_var (temp);
      insert_decl_map (id, result, temp);
      /* When RESULT_DECL is in SSA form, we need to use it's default_def
	 SSA_NAME.  */
      if (gimple_in_ssa_p (id->src_cfun) && gimple_default_def (id->src_cfun, result))
        temp = remap_ssa_name (gimple_default_def (id->src_cfun, result), id);
      insert_init_stmt (id, entry_bb, gimple_build_assign (temp, var));
    }
  else
    insert_decl_map (id, result, var);

  /* Remember this so we can ignore it in remap_decls.  */
  id->retvar = var;

  return use;
}

/* Callback through walk_tree.  Determine if a DECL_INITIAL makes reference
   to a local label.  */

static tree
has_label_address_in_static_1 (tree *nodep, int *walk_subtrees, void *fnp)
{
  tree node = *nodep;
  tree fn = (tree) fnp;

  if (TREE_CODE (node) == LABEL_DECL && DECL_CONTEXT (node) == fn)
    return node;

  if (TYPE_P (node))
    *walk_subtrees = 0;

  return NULL_TREE;
}

/* Determine if the function can be copied.  If so return NULL.  If
   not return a string describng the reason for failure.  */

static const char *
copy_forbidden (struct function *fun, tree fndecl)
{
  const char *reason = fun->cannot_be_copied_reason;
  tree decl;
  unsigned ix;

  /* Only examine the function once.  */
  if (fun->cannot_be_copied_set)
    return reason;

  /* We cannot copy a function that receives a non-local goto
     because we cannot remap the destination label used in the
     function that is performing the non-local goto.  */
  /* ??? Actually, this should be possible, if we work at it.
     No doubt there's just a handful of places that simply
     assume it doesn't happen and don't substitute properly.  */
  if (fun->has_nonlocal_label)
    {
      reason = G_("function %q+F can never be copied "
		  "because it receives a non-local goto");
      goto fail;
    }

  FOR_EACH_LOCAL_DECL (fun, ix, decl)
    if (TREE_CODE (decl) == VAR_DECL
	&& TREE_STATIC (decl)
	&& !DECL_EXTERNAL (decl)
	&& DECL_INITIAL (decl)
	&& walk_tree_without_duplicates (&DECL_INITIAL (decl),
					 has_label_address_in_static_1,
					 fndecl))
      {
	reason = G_("function %q+F can never be copied because it saves "
		    "address of local label in a static variable");
	goto fail;
      }

 fail:
  fun->cannot_be_copied_reason = reason;
  fun->cannot_be_copied_set = true;
  return reason;
}


static const char *inline_forbidden_reason;

/* A callback for walk_gimple_seq to handle statements.  Returns non-null
   iff a function can not be inlined.  Also sets the reason why. */

static tree
inline_forbidden_p_stmt (gimple_stmt_iterator *gsi, bool *handled_ops_p,
			 struct walk_stmt_info *wip)
{
  tree fn = (tree) wip->info;
  tree t;
  gimple stmt = gsi_stmt (*gsi);

  switch (gimple_code (stmt))
    {
    case GIMPLE_CALL:
      /* Refuse to inline alloca call unless user explicitly forced so as
	 this may change program's memory overhead drastically when the
	 function using alloca is called in loop.  In GCC present in
	 SPEC2000 inlining into schedule_block cause it to require 2GB of
	 RAM instead of 256MB.  Don't do so for alloca calls emitted for
	 VLA objects as those can't cause unbounded growth (they're always
	 wrapped inside stack_save/stack_restore regions.  */
      if (gimple_alloca_call_p (stmt)
	  && !gimple_call_alloca_for_var_p (stmt)
	  && !lookup_attribute ("always_inline", DECL_ATTRIBUTES (fn)))
	{
	  inline_forbidden_reason
	    = G_("function %q+F can never be inlined because it uses "
		 "alloca (override using the always_inline attribute)");
	  *handled_ops_p = true;
	  return fn;
	}

      t = gimple_call_fndecl (stmt);
      if (t == NULL_TREE)
	break;

      /* We cannot inline functions that call setjmp.  */
      if (setjmp_call_p (t))
	{
	  inline_forbidden_reason
	    = G_("function %q+F can never be inlined because it uses setjmp");
	  *handled_ops_p = true;
	  return t;
	}

      if (DECL_BUILT_IN_CLASS (t) == BUILT_IN_NORMAL)
	switch (DECL_FUNCTION_CODE (t))
	  {
	    /* We cannot inline functions that take a variable number of
	       arguments.  */
	  case BUILT_IN_VA_START:
	  case BUILT_IN_NEXT_ARG:
	  case BUILT_IN_VA_END:
	    inline_forbidden_reason
	      = G_("function %q+F can never be inlined because it "
		   "uses variable argument lists");
	    *handled_ops_p = true;
	    return t;

	  case BUILT_IN_LONGJMP:
	    /* We can't inline functions that call __builtin_longjmp at
	       all.  The non-local goto machinery really requires the
	       destination be in a different function.  If we allow the
	       function calling __builtin_longjmp to be inlined into the
	       function calling __builtin_setjmp, Things will Go Awry.  */
	    inline_forbidden_reason
	      = G_("function %q+F can never be inlined because "
		   "it uses setjmp-longjmp exception handling");
	    *handled_ops_p = true;
	    return t;

	  case BUILT_IN_NONLOCAL_GOTO:
	    /* Similarly.  */
	    inline_forbidden_reason
	      = G_("function %q+F can never be inlined because "
		   "it uses non-local goto");
	    *handled_ops_p = true;
	    return t;

	  case BUILT_IN_RETURN:
	  case BUILT_IN_APPLY_ARGS:
	    /* If a __builtin_apply_args caller would be inlined,
	       it would be saving arguments of the function it has
	       been inlined into.  Similarly __builtin_return would
	       return from the function the inline has been inlined into.  */
	    inline_forbidden_reason
	      = G_("function %q+F can never be inlined because "
		   "it uses __builtin_return or __builtin_apply_args");
	    *handled_ops_p = true;
	    return t;

	  default:
	    break;
	  }
      break;

    case GIMPLE_GOTO:
      t = gimple_goto_dest (stmt);

      /* We will not inline a function which uses computed goto.  The
	 addresses of its local labels, which may be tucked into
	 global storage, are of course not constant across
	 instantiations, which causes unexpected behavior.  */
      if (TREE_CODE (t) != LABEL_DECL)
	{
	  inline_forbidden_reason
	    = G_("function %q+F can never be inlined "
		 "because it contains a computed goto");
	  *handled_ops_p = true;
	  return t;
	}
      break;

    default:
      break;
    }

  *handled_ops_p = false;
  return NULL_TREE;
}

/* Return true if FNDECL is a function that cannot be inlined into
   another one.  */

static bool
inline_forbidden_p (tree fndecl)
{
  struct function *fun = DECL_STRUCT_FUNCTION (fndecl);
  struct walk_stmt_info wi;
  struct pointer_set_t *visited_nodes;
  basic_block bb;
  bool forbidden_p = false;

  /* First check for shared reasons not to copy the code.  */
  inline_forbidden_reason = copy_forbidden (fun, fndecl);
  if (inline_forbidden_reason != NULL)
    return true;

  /* Next, walk the statements of the function looking for
     constraucts we can't handle, or are non-optimal for inlining.  */
  visited_nodes = pointer_set_create ();
  memset (&wi, 0, sizeof (wi));
  wi.info = (void *) fndecl;
  wi.pset = visited_nodes;

  FOR_EACH_BB_FN (bb, fun)
    {
      gimple ret;
      gimple_seq seq = bb_seq (bb);
      ret = walk_gimple_seq (seq, inline_forbidden_p_stmt, NULL, &wi);
      forbidden_p = (ret != NULL);
      if (forbidden_p)
	break;
    }

  pointer_set_destroy (visited_nodes);
  return forbidden_p;
}

/* Returns nonzero if FN is a function that does not have any
   fundamental inline blocking properties.  */

bool
tree_inlinable_function_p (tree fn)
{
  bool inlinable = true;
  bool do_warning;
  tree always_inline;

  /* If we've already decided this function shouldn't be inlined,
     there's no need to check again.  */
  if (DECL_UNINLINABLE (fn))
    return false;

  /* We only warn for functions declared `inline' by the user.  */
  do_warning = (warn_inline
		&& DECL_DECLARED_INLINE_P (fn)
		&& !DECL_NO_INLINE_WARNING_P (fn)
		&& !DECL_IN_SYSTEM_HEADER (fn));

  always_inline = lookup_attribute ("always_inline", DECL_ATTRIBUTES (fn));

  if (flag_no_inline
      && always_inline == NULL)
    {
      if (do_warning)
        warning (OPT_Winline, "function %q+F can never be inlined because it "
                 "is suppressed using -fno-inline", fn);
      inlinable = false;
    }

  else if (!function_attribute_inlinable_p (fn))
    {
      if (do_warning)
        warning (OPT_Winline, "function %q+F can never be inlined because it "
                 "uses attributes conflicting with inlining", fn);
      inlinable = false;
    }

  else if (inline_forbidden_p (fn))
    {
      /* See if we should warn about uninlinable functions.  Previously,
	 some of these warnings would be issued while trying to expand
	 the function inline, but that would cause multiple warnings
	 about functions that would for example call alloca.  But since
	 this a property of the function, just one warning is enough.
	 As a bonus we can now give more details about the reason why a
	 function is not inlinable.  */
      if (always_inline)
	error (inline_forbidden_reason, fn);
      else if (do_warning)
	warning (OPT_Winline, inline_forbidden_reason, fn);

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

  gcc_assert (!VOID_TYPE_P (type));

  if (TREE_CODE (type) == VECTOR_TYPE)
    {
      enum machine_mode inner = TYPE_MODE (TREE_TYPE (type));
      enum machine_mode simd
	= targetm.vectorize.preferred_simd_mode (inner);
      int simd_mode_size = GET_MODE_SIZE (simd);
      return ((GET_MODE_SIZE (TYPE_MODE (type)) + simd_mode_size - 1)
	      / simd_mode_size);
    }

  size = int_size_in_bytes (type);

  if (size < 0 || size > MOVE_MAX_PIECES * MOVE_RATIO (!optimize_size))
    /* Cost of a memcpy call, 3 arguments and the call.  */
    return 4;
  else
    return ((size + MOVE_MAX_PIECES - 1) / MOVE_MAX_PIECES);
}

/* Returns cost of operation CODE, according to WEIGHTS  */

static int
estimate_operator_cost (enum tree_code code, eni_weights *weights,
			tree op1 ATTRIBUTE_UNUSED, tree op2)
{
  switch (code)
    {
    /* These are "free" conversions, or their presumed cost
       is folded into other operations.  */
    case RANGE_EXPR:
    CASE_CONVERT:
    case COMPLEX_EXPR:
    case PAREN_EXPR:
    case VIEW_CONVERT_EXPR:
      return 0;

    /* Assign cost of 1 to usual operations.
       ??? We may consider mapping RTL costs to this.  */
    case COND_EXPR:
    case VEC_COND_EXPR:
    case VEC_PERM_EXPR:

    case PLUS_EXPR:
    case POINTER_PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
    case FMA_EXPR:

    case ADDR_SPACE_CONVERT_EXPR:
    case FIXED_CONVERT_EXPR:
    case FIX_TRUNC_EXPR:

    case NEGATE_EXPR:
    case FLOAT_EXPR:
    case MIN_EXPR:
    case MAX_EXPR:
    case ABS_EXPR:

    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:
    case VEC_LSHIFT_EXPR:
    case VEC_RSHIFT_EXPR:

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

    case CONJ_EXPR:

    case PREDECREMENT_EXPR:
    case PREINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:

    case REALIGN_LOAD_EXPR:

    case REDUC_MAX_EXPR:
    case REDUC_MIN_EXPR:
    case REDUC_PLUS_EXPR:
    case WIDEN_SUM_EXPR:
    case WIDEN_MULT_EXPR:
    case DOT_PROD_EXPR:
    case WIDEN_MULT_PLUS_EXPR:
    case WIDEN_MULT_MINUS_EXPR:
    case WIDEN_LSHIFT_EXPR:

    case VEC_WIDEN_MULT_HI_EXPR:
    case VEC_WIDEN_MULT_LO_EXPR:
    case VEC_UNPACK_HI_EXPR:
    case VEC_UNPACK_LO_EXPR:
    case VEC_UNPACK_FLOAT_HI_EXPR:
    case VEC_UNPACK_FLOAT_LO_EXPR:
    case VEC_PACK_TRUNC_EXPR:
    case VEC_PACK_SAT_EXPR:
    case VEC_PACK_FIX_TRUNC_EXPR:
    case VEC_EXTRACT_EVEN_EXPR:
    case VEC_EXTRACT_ODD_EXPR:
    case VEC_INTERLEAVE_HIGH_EXPR:
    case VEC_INTERLEAVE_LOW_EXPR:
    case VEC_WIDEN_LSHIFT_HI_EXPR:
    case VEC_WIDEN_LSHIFT_LO_EXPR:

      return 1;

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
      if (TREE_CODE (op2) != INTEGER_CST)
        return weights->div_mod_cost;
      return 1;

    default:
      /* We expect a copy assignment with no operator.  */
      gcc_assert (get_gimple_rhs_class (code) == GIMPLE_SINGLE_RHS);
      return 0;
    }
}


/* Estimate number of instructions that will be created by expanding
   the statements in the statement sequence STMTS.
   WEIGHTS contains weights attributed to various constructs.  */

static
int estimate_num_insns_seq (gimple_seq stmts, eni_weights *weights)
{
  int cost;
  gimple_stmt_iterator gsi;

  cost = 0;
  for (gsi = gsi_start (stmts); !gsi_end_p (gsi); gsi_next (&gsi))
    cost += estimate_num_insns (gsi_stmt (gsi), weights);

  return cost;
}


/* Estimate number of instructions that will be created by expanding STMT.
   WEIGHTS contains weights attributed to various constructs.  */

int
estimate_num_insns (gimple stmt, eni_weights *weights)
{
  unsigned cost, i;
  enum gimple_code code = gimple_code (stmt);
  tree lhs;
  tree rhs;

  switch (code)
    {
    case GIMPLE_ASSIGN:
      /* Try to estimate the cost of assignments.  We have three cases to
	 deal with:
	 1) Simple assignments to registers;
	 2) Stores to things that must live in memory.  This includes
	    "normal" stores to scalars, but also assignments of large
	    structures, or constructors of big arrays;

	 Let us look at the first two cases, assuming we have "a = b + C":
	 <GIMPLE_ASSIGN <var_decl "a">
	        <plus_expr <var_decl "b"> <constant C>>
	 If "a" is a GIMPLE register, the assignment to it is free on almost
	 any target, because "a" usually ends up in a real register.  Hence
	 the only cost of this expression comes from the PLUS_EXPR, and we
	 can ignore the GIMPLE_ASSIGN.
	 If "a" is not a GIMPLE register, the assignment to "a" will most
	 likely be a real store, so the cost of the GIMPLE_ASSIGN is the cost
	 of moving something into "a", which we compute using the function
	 estimate_move_cost.  */
      if (gimple_clobber_p (stmt))
	return 0;	/* ={v} {CLOBBER} stmt expands to nothing.  */

      lhs = gimple_assign_lhs (stmt);
      rhs = gimple_assign_rhs1 (stmt);

      if (is_gimple_reg (lhs))
	cost = 0;
      else
	cost = estimate_move_cost (TREE_TYPE (lhs));

      if (!is_gimple_reg (rhs) && !is_gimple_min_invariant (rhs))
	cost += estimate_move_cost (TREE_TYPE (rhs));

      cost += estimate_operator_cost (gimple_assign_rhs_code (stmt), weights,
      				      gimple_assign_rhs1 (stmt),
				      get_gimple_rhs_class (gimple_assign_rhs_code (stmt))
				      == GIMPLE_BINARY_RHS
				      ? gimple_assign_rhs2 (stmt) : NULL);
      break;

    case GIMPLE_COND:
      cost = 1 + estimate_operator_cost (gimple_cond_code (stmt), weights,
      				         gimple_op (stmt, 0),
				         gimple_op (stmt, 1));
      break;

    case GIMPLE_SWITCH:
      /* Take into account cost of the switch + guess 2 conditional jumps for
         each case label.

	 TODO: once the switch expansion logic is sufficiently separated, we can
	 do better job on estimating cost of the switch.  */
      if (weights->time_based)
        cost = floor_log2 (gimple_switch_num_labels (stmt)) * 2;
      else
        cost = gimple_switch_num_labels (stmt) * 2;
      break;

    case GIMPLE_CALL:
      {
	tree decl = gimple_call_fndecl (stmt);
	struct cgraph_node *node = NULL;

	/* Do not special case builtins where we see the body.
	   This just confuse inliner.  */
	if (!decl || !(node = cgraph_get_node (decl)) || node->analyzed)
	  ;
	/* For buitins that are likely expanded to nothing or
	   inlined do not account operand costs.  */
	else if (is_simple_builtin (decl))
	  return 0;
	else if (is_inexpensive_builtin (decl))
	  return weights->target_builtin_call_cost;
	else if (DECL_BUILT_IN_CLASS (decl) == BUILT_IN_NORMAL)
	  {
	    /* We canonicalize x * x to pow (x, 2.0) with -ffast-math, so
	       specialize the cheap expansion we do here.
	       ???  This asks for a more general solution.  */
	    switch (DECL_FUNCTION_CODE (decl))
	      {
		case BUILT_IN_POW:
		case BUILT_IN_POWF:
		case BUILT_IN_POWL:
		  if (TREE_CODE (gimple_call_arg (stmt, 1)) == REAL_CST
		      && REAL_VALUES_EQUAL
			   (TREE_REAL_CST (gimple_call_arg (stmt, 1)), dconst2))
		    return estimate_operator_cost (MULT_EXPR, weights,
						   gimple_call_arg (stmt, 0),
						   gimple_call_arg (stmt, 0));
		  break;

		default:
		  break;
	      }
	  }

	cost = node ? weights->call_cost : weights->indirect_call_cost;
	if (gimple_call_lhs (stmt))
	  cost += estimate_move_cost (TREE_TYPE (gimple_call_lhs (stmt)));
	for (i = 0; i < gimple_call_num_args (stmt); i++)
	  {
	    tree arg = gimple_call_arg (stmt, i);
	    cost += estimate_move_cost (TREE_TYPE (arg));
	  }
	break;
      }

    case GIMPLE_RETURN:
      return weights->return_cost;

    case GIMPLE_GOTO:
    case GIMPLE_LABEL:
    case GIMPLE_NOP:
    case GIMPLE_PHI:
    case GIMPLE_PREDICT:
    case GIMPLE_DEBUG:
      return 0;

    case GIMPLE_ASM:
      return asm_str_count (gimple_asm_string (stmt));

    case GIMPLE_RESX:
      /* This is either going to be an external function call with one
	 argument, or two register copy statements plus a goto.  */
      return 2;

    case GIMPLE_EH_DISPATCH:
      /* ??? This is going to turn into a switch statement.  Ideally
	 we'd have a look at the eh region and estimate the number of
	 edges involved.  */
      return 10;

    case GIMPLE_BIND:
      return estimate_num_insns_seq (gimple_bind_body (stmt), weights);

    case GIMPLE_EH_FILTER:
      return estimate_num_insns_seq (gimple_eh_filter_failure (stmt), weights);

    case GIMPLE_CATCH:
      return estimate_num_insns_seq (gimple_catch_handler (stmt), weights);

    case GIMPLE_TRY:
      return (estimate_num_insns_seq (gimple_try_eval (stmt), weights)
              + estimate_num_insns_seq (gimple_try_cleanup (stmt), weights));

    /* OpenMP directives are generally very expensive.  */

    case GIMPLE_OMP_RETURN:
    case GIMPLE_OMP_SECTIONS_SWITCH:
    case GIMPLE_OMP_ATOMIC_STORE:
    case GIMPLE_OMP_CONTINUE:
      /* ...except these, which are cheap.  */
      return 0;

    case GIMPLE_OMP_ATOMIC_LOAD:
      return weights->omp_cost;

    case GIMPLE_OMP_FOR:
      return (weights->omp_cost
              + estimate_num_insns_seq (gimple_omp_body (stmt), weights)
              + estimate_num_insns_seq (gimple_omp_for_pre_body (stmt), weights));

    case GIMPLE_OMP_PARALLEL:
    case GIMPLE_OMP_TASK:
    case GIMPLE_OMP_CRITICAL:
    case GIMPLE_OMP_MASTER:
    case GIMPLE_OMP_ORDERED:
    case GIMPLE_OMP_SECTION:
    case GIMPLE_OMP_SECTIONS:
    case GIMPLE_OMP_SINGLE:
      return (weights->omp_cost
              + estimate_num_insns_seq (gimple_omp_body (stmt), weights));

    case GIMPLE_TRANSACTION:
      return (weights->tm_cost
	      + estimate_num_insns_seq (gimple_transaction_body (stmt),
					weights));

    default:
      gcc_unreachable ();
    }

  return cost;
}

/* Estimate number of instructions that will be created by expanding
   function FNDECL.  WEIGHTS contains weights attributed to various
   constructs.  */

int
estimate_num_insns_fn (tree fndecl, eni_weights *weights)
{
  struct function *my_function = DECL_STRUCT_FUNCTION (fndecl);
  gimple_stmt_iterator bsi;
  basic_block bb;
  int n = 0;

  gcc_assert (my_function && my_function->cfg);
  FOR_EACH_BB_FN (bb, my_function)
    {
      for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi); gsi_next (&bsi))
	n += estimate_num_insns (gsi_stmt (bsi), weights);
    }

  return n;
}


/* Initializes weights used by estimate_num_insns.  */

void
init_inline_once (void)
{
  eni_size_weights.call_cost = 1;
  eni_size_weights.indirect_call_cost = 3;
  eni_size_weights.target_builtin_call_cost = 1;
  eni_size_weights.div_mod_cost = 1;
  eni_size_weights.omp_cost = 40;
  eni_size_weights.tm_cost = 10;
  eni_size_weights.time_based = false;
  eni_size_weights.return_cost = 1;

  /* Estimating time for call is difficult, since we have no idea what the
     called function does.  In the current uses of eni_time_weights,
     underestimating the cost does less harm than overestimating it, so
     we choose a rather small value here.  */
  eni_time_weights.call_cost = 10;
  eni_time_weights.indirect_call_cost = 15;
  eni_time_weights.target_builtin_call_cost = 1;
  eni_time_weights.div_mod_cost = 10;
  eni_time_weights.omp_cost = 40;
  eni_time_weights.tm_cost = 40;
  eni_time_weights.time_based = true;
  eni_time_weights.return_cost = 2;
}

/* Estimate the number of instructions in a gimple_seq. */

int
count_insns_seq (gimple_seq seq, eni_weights *weights)
{
  gimple_stmt_iterator gsi;
  int n = 0;
  for (gsi = gsi_start (seq); !gsi_end_p (gsi); gsi_next (&gsi))
    n += estimate_num_insns (gsi_stmt (gsi), weights);

  return n;
}


/* Install new lexical TREE_BLOCK underneath 'current_block'.  */

static void
prepend_lexical_block (tree current_block, tree new_block)
{
  BLOCK_CHAIN (new_block) = BLOCK_SUBBLOCKS (current_block);
  BLOCK_SUBBLOCKS (current_block) = new_block;
  BLOCK_SUPERCONTEXT (new_block) = current_block;
}

/* Add local variables from CALLEE to CALLER.  */

static inline void
add_local_variables (struct function *callee, struct function *caller,
		     copy_body_data *id, bool check_var_ann)
{
  tree var;
  unsigned ix;

  FOR_EACH_LOCAL_DECL (callee, ix, var)
    if (TREE_STATIC (var) && !TREE_ASM_WRITTEN (var))
      {
	if (!check_var_ann
	    || (var_ann (var) && add_referenced_var (var)))
	  add_local_decl (caller, var);
      }
    else if (!can_be_nonlocal (var, id))
      {
        tree new_var = remap_decl (var, id);

        /* Remap debug-expressions.  */
	if (TREE_CODE (new_var) == VAR_DECL
	    && DECL_DEBUG_EXPR_IS_FROM (new_var)
	    && new_var != var)
	  {
	    tree tem = DECL_DEBUG_EXPR (var);
	    bool old_regimplify = id->regimplify;
	    id->remapping_type_depth++;
	    walk_tree (&tem, copy_tree_body_r, id, NULL);
	    id->remapping_type_depth--;
	    id->regimplify = old_regimplify;
	    SET_DECL_DEBUG_EXPR (new_var, tem);
	  }
	add_local_decl (caller, new_var);
      }
}

/* If STMT is a GIMPLE_CALL, replace it with its inline expansion.  */

static bool
expand_call_inline (basic_block bb, gimple stmt, copy_body_data *id)
{
  tree use_retvar;
  tree fn;
  struct pointer_map_t *st, *dst;
  tree return_slot;
  tree modify_dest;
  location_t saved_location;
  struct cgraph_edge *cg_edge;
  cgraph_inline_failed_t reason;
  basic_block return_block;
  edge e;
  gimple_stmt_iterator gsi, stmt_gsi;
  bool successfully_inlined = FALSE;
  bool purge_dead_abnormal_edges;

  /* Set input_location here so we get the right instantiation context
     if we call instantiate_decl from inlinable_function_p.  */
  saved_location = input_location;
  if (gimple_has_location (stmt))
    input_location = gimple_location (stmt);

  /* From here on, we're only interested in CALL_EXPRs.  */
  if (gimple_code (stmt) != GIMPLE_CALL)
    goto egress;

  cg_edge = cgraph_edge (id->dst_node, stmt);
  gcc_checking_assert (cg_edge);
  /* First, see if we can figure out what function is being called.
     If we cannot, then there is no hope of inlining the function.  */
  if (cg_edge->indirect_unknown_callee)
    goto egress;
  fn = cg_edge->callee->decl;
  gcc_checking_assert (fn);

  /* If FN is a declaration of a function in a nested scope that was
     globally declared inline, we don't set its DECL_INITIAL.
     However, we can't blindly follow DECL_ABSTRACT_ORIGIN because the
     C++ front-end uses it for cdtors to refer to their internal
     declarations, that are not real functions.  Fortunately those
     don't have trees to be saved, so we can tell by checking their
     gimple_body.  */
  if (!DECL_INITIAL (fn)
      && DECL_ABSTRACT_ORIGIN (fn)
      && gimple_has_body_p (DECL_ABSTRACT_ORIGIN (fn)))
    fn = DECL_ABSTRACT_ORIGIN (fn);

  /* Don't try to inline functions that are not well-suited to inlining.  */
  if (!cgraph_inline_p (cg_edge, &reason))
    {
      /* If this call was originally indirect, we do not want to emit any
	 inlining related warnings or sorry messages because there are no
	 guarantees regarding those.  */
      if (cg_edge->indirect_inlining_edge)
	goto egress;

      if (lookup_attribute ("always_inline", DECL_ATTRIBUTES (fn))
	  /* Avoid warnings during early inline pass. */
	  && cgraph_global_info_ready
	  /* PR 20090218-1_0.c. Body can be provided by another module. */
	  && (reason != CIF_BODY_NOT_AVAILABLE || !flag_generate_lto))
	{
	  error ("inlining failed in call to always_inline %q+F: %s", fn,
		 cgraph_inline_failed_string (reason));
	  error ("called from here");
	}
      else if (warn_inline
	       && DECL_DECLARED_INLINE_P (fn)
	       && !DECL_NO_INLINE_WARNING_P (fn)
	       && !DECL_IN_SYSTEM_HEADER (fn)
	       && reason != CIF_UNSPECIFIED
	       && !lookup_attribute ("noinline", DECL_ATTRIBUTES (fn))
	       /* Do not warn about not inlined recursive calls.  */
	       && !cgraph_edge_recursive_p (cg_edge)
	       /* Avoid warnings during early inline pass. */
	       && cgraph_global_info_ready)
	{
	  warning (OPT_Winline, "inlining failed in call to %q+F: %s",
		   fn, _(cgraph_inline_failed_string (reason)));
	  warning (OPT_Winline, "called from here");
	}
      goto egress;
    }
  fn = cg_edge->callee->decl;

#ifdef ENABLE_CHECKING
  if (cg_edge->callee->decl != id->dst_node->decl)
    verify_cgraph_node (cg_edge->callee);
#endif

  /* We will be inlining this callee.  */
  id->eh_lp_nr = lookup_stmt_eh_lp (stmt);

  /* Update the callers EH personality.  */
  if (DECL_FUNCTION_PERSONALITY (cg_edge->callee->decl))
    DECL_FUNCTION_PERSONALITY (cg_edge->caller->decl)
      = DECL_FUNCTION_PERSONALITY (cg_edge->callee->decl);

  /* Split the block holding the GIMPLE_CALL.  */
  e = split_block (bb, stmt);
  bb = e->src;
  return_block = e->dest;
  remove_edge (e);

  /* split_block splits after the statement; work around this by
     moving the call into the second block manually.  Not pretty,
     but seems easier than doing the CFG manipulation by hand
     when the GIMPLE_CALL is in the last statement of BB.  */
  stmt_gsi = gsi_last_bb (bb);
  gsi_remove (&stmt_gsi, false);

  /* If the GIMPLE_CALL was in the last statement of BB, it may have
     been the source of abnormal edges.  In this case, schedule
     the removal of dead abnormal edges.  */
  gsi = gsi_start_bb (return_block);
  if (gsi_end_p (gsi))
    {
      gsi_insert_after (&gsi, stmt, GSI_NEW_STMT);
      purge_dead_abnormal_edges = true;
    }
  else
    {
      gsi_insert_before (&gsi, stmt, GSI_NEW_STMT);
      purge_dead_abnormal_edges = false;
    }

  stmt_gsi = gsi_start_bb (return_block);

  /* Build a block containing code to initialize the arguments, the
     actual inline expansion of the body, and a label for the return
     statements within the function to jump to.  The type of the
     statement expression is the return type of the function call.  */
  id->block = make_node (BLOCK);
  BLOCK_ABSTRACT_ORIGIN (id->block) = fn;
  BLOCK_SOURCE_LOCATION (id->block) = input_location;
  prepend_lexical_block (gimple_block (stmt), id->block);

  /* Local declarations will be replaced by their equivalents in this
     map.  */
  st = id->decl_map;
  id->decl_map = pointer_map_create ();
  dst = id->debug_map;
  id->debug_map = NULL;

  /* Record the function we are about to inline.  */
  id->src_fn = fn;
  id->src_node = cg_edge->callee;
  id->src_cfun = DECL_STRUCT_FUNCTION (fn);
  id->gimple_call = stmt;

  gcc_assert (!id->src_cfun->after_inlining);

  id->entry_bb = bb;
  if (lookup_attribute ("cold", DECL_ATTRIBUTES (fn)))
    {
      gimple_stmt_iterator si = gsi_last_bb (bb);
      gsi_insert_after (&si, gimple_build_predict (PRED_COLD_FUNCTION,
      						   NOT_TAKEN),
			GSI_NEW_STMT);
    }
  initialize_inlined_parameters (id, stmt, fn, bb);

  if (DECL_INITIAL (fn))
    prepend_lexical_block (id->block, remap_blocks (DECL_INITIAL (fn), id));

  /* Return statements in the function body will be replaced by jumps
     to the RET_LABEL.  */
  gcc_assert (DECL_INITIAL (fn));
  gcc_assert (TREE_CODE (DECL_INITIAL (fn)) == BLOCK);

  /* Find the LHS to which the result of this call is assigned.  */
  return_slot = NULL;
  if (gimple_call_lhs (stmt))
    {
      modify_dest = gimple_call_lhs (stmt);

      /* The function which we are inlining might not return a value,
	 in which case we should issue a warning that the function
	 does not return a value.  In that case the optimizers will
	 see that the variable to which the value is assigned was not
	 initialized.  We do not want to issue a warning about that
	 uninitialized variable.  */
      if (DECL_P (modify_dest))
	TREE_NO_WARNING (modify_dest) = 1;

      if (gimple_call_return_slot_opt_p (stmt))
	{
	  return_slot = modify_dest;
	  modify_dest = NULL;
	}
    }
  else
    modify_dest = NULL;

  /* If we are inlining a call to the C++ operator new, we don't want
     to use type based alias analysis on the return value.  Otherwise
     we may get confused if the compiler sees that the inlined new
     function returns a pointer which was just deleted.  See bug
     33407.  */
  if (DECL_IS_OPERATOR_NEW (fn))
    {
      return_slot = NULL;
      modify_dest = NULL;
    }

  /* Declare the return variable for the function.  */
  use_retvar = declare_return_variable (id, return_slot, modify_dest, bb);

  /* Add local vars in this inlined callee to caller.  */
  add_local_variables (id->src_cfun, cfun, id, true);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Inlining ");
      print_generic_expr (dump_file, id->src_fn, 0);
      fprintf (dump_file, " to ");
      print_generic_expr (dump_file, id->dst_fn, 0);
      fprintf (dump_file, " with frequency %i\n", cg_edge->frequency);
    }

  /* This is it.  Duplicate the callee body.  Assume callee is
     pre-gimplified.  Note that we must not alter the caller
     function in any way before this point, as this CALL_EXPR may be
     a self-referential call; if we're calling ourselves, we need to
     duplicate our body before altering anything.  */
  copy_body (id, bb->count,
  	     cg_edge->frequency * REG_BR_PROB_BASE / CGRAPH_FREQ_BASE,
	     bb, return_block, NULL, NULL);

  /* Reset the escaped solution.  */
  if (cfun->gimple_df)
    pt_solution_reset (&cfun->gimple_df->escaped);

  /* Clean up.  */
  if (id->debug_map)
    {
      pointer_map_destroy (id->debug_map);
      id->debug_map = dst;
    }
  pointer_map_destroy (id->decl_map);
  id->decl_map = st;

  /* Unlink the calls virtual operands before replacing it.  */
  unlink_stmt_vdef (stmt);

  /* If the inlined function returns a result that we care about,
     substitute the GIMPLE_CALL with an assignment of the return
     variable to the LHS of the call.  That is, if STMT was
     'a = foo (...)', substitute the call with 'a = USE_RETVAR'.  */
  if (use_retvar && gimple_call_lhs (stmt))
    {
      gimple old_stmt = stmt;
      stmt = gimple_build_assign (gimple_call_lhs (stmt), use_retvar);
      gsi_replace (&stmt_gsi, stmt, false);
      if (gimple_in_ssa_p (cfun))
	mark_symbols_for_renaming (stmt);
      maybe_clean_or_replace_eh_stmt (old_stmt, stmt);
    }
  else
    {
      /* Handle the case of inlining a function with no return
	 statement, which causes the return value to become undefined.  */
      if (gimple_call_lhs (stmt)
	  && TREE_CODE (gimple_call_lhs (stmt)) == SSA_NAME)
	{
	  tree name = gimple_call_lhs (stmt);
	  tree var = SSA_NAME_VAR (name);
	  tree def = gimple_default_def (cfun, var);

	  if (def)
	    {
	      /* If the variable is used undefined, make this name
		 undefined via a move.  */
	      stmt = gimple_build_assign (gimple_call_lhs (stmt), def);
	      gsi_replace (&stmt_gsi, stmt, true);
	    }
	  else
	    {
	      /* Otherwise make this variable undefined.  */
	      gsi_remove (&stmt_gsi, true);
	      set_default_def (var, name);
	      SSA_NAME_DEF_STMT (name) = gimple_build_nop ();
	    }
	}
      else
        gsi_remove (&stmt_gsi, true);
    }

  if (purge_dead_abnormal_edges)
    {
      gimple_purge_dead_eh_edges (return_block);
      gimple_purge_dead_abnormal_call_edges (return_block);
    }

  /* If the value of the new expression is ignored, that's OK.  We
     don't warn about this for CALL_EXPRs, so we shouldn't warn about
     the equivalent inlined version either.  */
  if (is_gimple_assign (stmt))
    {
      gcc_assert (gimple_assign_single_p (stmt)
		  || CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (stmt)));
      TREE_USED (gimple_assign_rhs1 (stmt)) = 1;
    }

  /* Output the inlining info for this abstract function, since it has been
     inlined.  If we don't do this now, we can lose the information about the
     variables in the function when the blocks get blown away as soon as we
     remove the cgraph node.  */
  (*debug_hooks->outlining_inline_function) (cg_edge->callee->decl);

  /* Update callgraph if needed.  */
  cgraph_remove_node (cg_edge->callee);

  id->block = NULL_TREE;
  successfully_inlined = TRUE;

 egress:
  input_location = saved_location;
  return successfully_inlined;
}

/* Expand call statements reachable from STMT_P.
   We can only have CALL_EXPRs as the "toplevel" tree code or nested
   in a MODIFY_EXPR.  */

static bool
gimple_expand_calls_inline (basic_block bb, copy_body_data *id)
{
  gimple_stmt_iterator gsi;

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple stmt = gsi_stmt (gsi);

      if (is_gimple_call (stmt)
	  && expand_call_inline (bb, stmt, id))
	return true;
    }

  return false;
}


/* Walk all basic blocks created after FIRST and try to fold every statement
   in the STATEMENTS pointer set.  */

static void
fold_marked_statements (int first, struct pointer_set_t *statements)
{
  for (; first < n_basic_blocks; first++)
    if (BASIC_BLOCK (first))
      {
        gimple_stmt_iterator gsi;

	for (gsi = gsi_start_bb (BASIC_BLOCK (first));
	     !gsi_end_p (gsi);
	     gsi_next (&gsi))
	  if (pointer_set_contains (statements, gsi_stmt (gsi)))
	    {
	      gimple old_stmt = gsi_stmt (gsi);
	      tree old_decl = is_gimple_call (old_stmt) ? gimple_call_fndecl (old_stmt) : 0;

	      if (old_decl && DECL_BUILT_IN (old_decl))
		{
		  /* Folding builtins can create multiple instructions,
		     we need to look at all of them.  */
		  gimple_stmt_iterator i2 = gsi;
		  gsi_prev (&i2);
		  if (fold_stmt (&gsi))
		    {
		      gimple new_stmt;
		      /* If a builtin at the end of a bb folded into nothing,
			 the following loop won't work.  */
		      if (gsi_end_p (gsi))
			{
			  cgraph_update_edges_for_call_stmt (old_stmt,
							     old_decl, NULL);
			  break;
			}
		      if (gsi_end_p (i2))
			i2 = gsi_start_bb (BASIC_BLOCK (first));
		      else
			gsi_next (&i2);
		      while (1)
			{
			  new_stmt = gsi_stmt (i2);
			  update_stmt (new_stmt);
			  cgraph_update_edges_for_call_stmt (old_stmt, old_decl,
							     new_stmt);

			  if (new_stmt == gsi_stmt (gsi))
			    {
			      /* It is okay to check only for the very last
				 of these statements.  If it is a throwing
				 statement nothing will change.  If it isn't
				 this can remove EH edges.  If that weren't
				 correct then because some intermediate stmts
				 throw, but not the last one.  That would mean
				 we'd have to split the block, which we can't
				 here and we'd loose anyway.  And as builtins
				 probably never throw, this all
				 is mood anyway.  */
			      if (maybe_clean_or_replace_eh_stmt (old_stmt,
								  new_stmt))
				gimple_purge_dead_eh_edges (BASIC_BLOCK (first));
			      break;
			    }
			  gsi_next (&i2);
			}
		    }
		}
	      else if (fold_stmt (&gsi))
		{
		  /* Re-read the statement from GSI as fold_stmt() may
		     have changed it.  */
		  gimple new_stmt = gsi_stmt (gsi);
		  update_stmt (new_stmt);

		  if (is_gimple_call (old_stmt)
		      || is_gimple_call (new_stmt))
		    cgraph_update_edges_for_call_stmt (old_stmt, old_decl,
						       new_stmt);

		  if (maybe_clean_or_replace_eh_stmt (old_stmt, new_stmt))
		    gimple_purge_dead_eh_edges (BASIC_BLOCK (first));
		}
	    }
      }
}

/* Return true if BB has at least one abnormal outgoing edge.  */

static inline bool
has_abnormal_outgoing_edge_p (basic_block bb)
{
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, bb->succs)
    if (e->flags & EDGE_ABNORMAL)
      return true;

  return false;
}

/* Expand calls to inline functions in the body of FN.  */

unsigned int
optimize_inline_calls (tree fn)
{
  copy_body_data id;
  basic_block bb;
  int last = n_basic_blocks;
  struct gimplify_ctx gctx;
  bool inlined_p = false;

  /* Clear out ID.  */
  memset (&id, 0, sizeof (id));

  id.src_node = id.dst_node = cgraph_get_node (fn);
  gcc_assert (id.dst_node->analyzed);
  id.dst_fn = fn;
  /* Or any functions that aren't finished yet.  */
  if (current_function_decl)
    id.dst_fn = current_function_decl;

  id.copy_decl = copy_decl_maybe_to_var;
  id.transform_call_graph_edges = CB_CGE_DUPLICATE;
  id.transform_new_cfg = false;
  id.transform_return_to_modify = true;
  id.transform_lang_insert_block = NULL;
  id.statements_to_fold = pointer_set_create ();

  push_gimplify_context (&gctx);

  /* We make no attempts to keep dominance info up-to-date.  */
  free_dominance_info (CDI_DOMINATORS);
  free_dominance_info (CDI_POST_DOMINATORS);

  /* Register specific gimple functions.  */
  gimple_register_cfg_hooks ();

  /* Reach the trees by walking over the CFG, and note the
     enclosing basic-blocks in the call edges.  */
  /* We walk the blocks going forward, because inlined function bodies
     will split id->current_basic_block, and the new blocks will
     follow it; we'll trudge through them, processing their CALL_EXPRs
     along the way.  */
  FOR_EACH_BB (bb)
    inlined_p |= gimple_expand_calls_inline (bb, &id);

  pop_gimplify_context (NULL);

#ifdef ENABLE_CHECKING
    {
      struct cgraph_edge *e;

      verify_cgraph_node (id.dst_node);

      /* Double check that we inlined everything we are supposed to inline.  */
      for (e = id.dst_node->callees; e; e = e->next_callee)
	gcc_assert (e->inline_failed);
    }
#endif

  /* Fold queued statements.  */
  fold_marked_statements (last, id.statements_to_fold);
  pointer_set_destroy (id.statements_to_fold);

  gcc_assert (!id.debug_stmts);

  /* If we didn't inline into the function there is nothing to do.  */
  if (!inlined_p)
    return 0;

  /* Renumber the lexical scoping (non-code) blocks consecutively.  */
  number_blocks (fn);

  delete_unreachable_blocks_update_callgraph (&id);
#ifdef ENABLE_CHECKING
  verify_cgraph_node (id.dst_node);
#endif

  /* It would be nice to check SSA/CFG/statement consistency here, but it is
     not possible yet - the IPA passes might make various functions to not
     throw and they don't care to proactively update local EH info.  This is
     done later in fixup_cfg pass that also execute the verification.  */
  return (TODO_update_ssa
	  | TODO_cleanup_cfg
	  | (gimple_in_ssa_p (cfun) ? TODO_remove_unused_locals : 0)
	  | (gimple_in_ssa_p (cfun) ? TODO_update_address_taken : 0)
	  | (profile_status != PROFILE_ABSENT ? TODO_rebuild_frequencies : 0));
}

/* Passed to walk_tree.  Copies the node pointed to, if appropriate.  */

tree
copy_tree_r (tree *tp, int *walk_subtrees, void *data ATTRIBUTE_UNUSED)
{
  enum tree_code code = TREE_CODE (*tp);
  enum tree_code_class cl = TREE_CODE_CLASS (code);

  /* We make copies of most nodes.  */
  if (IS_EXPR_CODE_CLASS (cl)
      || code == TREE_LIST
      || code == TREE_VEC
      || code == TYPE_DECL
      || code == OMP_CLAUSE)
    {
      /* Because the chain gets clobbered when we make a copy, we save it
	 here.  */
      tree chain = NULL_TREE, new_tree;

      if (CODE_CONTAINS_STRUCT (code, TS_COMMON))
	chain = TREE_CHAIN (*tp);

      /* Copy the node.  */
      new_tree = copy_node (*tp);

      /* Propagate mudflap marked-ness.  */
      if (flag_mudflap && mf_marked_p (*tp))
        mf_mark (new_tree);

      *tp = new_tree;

      /* Now, restore the chain, if appropriate.  That will cause
	 walk_tree to walk into the chain as well.  */
      if (code == PARM_DECL
	  || code == TREE_LIST
	  || code == OMP_CLAUSE)
	TREE_CHAIN (*tp) = chain;

      /* For now, we don't update BLOCKs when we make copies.  So, we
	 have to nullify all BIND_EXPRs.  */
      if (TREE_CODE (*tp) == BIND_EXPR)
	BIND_EXPR_BLOCK (*tp) = NULL_TREE;
    }
  else if (code == CONSTRUCTOR)
    {
      /* CONSTRUCTOR nodes need special handling because
         we need to duplicate the vector of elements.  */
      tree new_tree;

      new_tree = copy_node (*tp);

      /* Propagate mudflap marked-ness.  */
      if (flag_mudflap && mf_marked_p (*tp))
        mf_mark (new_tree);

      CONSTRUCTOR_ELTS (new_tree) = VEC_copy (constructor_elt, gc,
					 CONSTRUCTOR_ELTS (*tp));
      *tp = new_tree;
    }
  else if (code == STATEMENT_LIST)
    /* We used to just abort on STATEMENT_LIST, but we can run into them
       with statement-expressions (c++/40975).  */
    copy_statement_list (tp);
  else if (TREE_CODE_CLASS (code) == tcc_type)
    *walk_subtrees = 0;
  else if (TREE_CODE_CLASS (code) == tcc_declaration)
    *walk_subtrees = 0;
  else if (TREE_CODE_CLASS (code) == tcc_constant)
    *walk_subtrees = 0;
  return NULL_TREE;
}

/* The SAVE_EXPR pointed to by TP is being copied.  If ST contains
   information indicating to what new SAVE_EXPR this one should be mapped,
   use that one.  Otherwise, create a new node and enter it in ST.  FN is
   the function into which the copy will be placed.  */

static void
remap_save_expr (tree *tp, void *st_, int *walk_subtrees)
{
  struct pointer_map_t *st = (struct pointer_map_t *) st_;
  tree *n;
  tree t;

  /* See if we already encountered this SAVE_EXPR.  */
  n = (tree *) pointer_map_contains (st, *tp);

  /* If we didn't already remap this SAVE_EXPR, do so now.  */
  if (!n)
    {
      t = copy_node (*tp);

      /* Remember this SAVE_EXPR.  */
      *pointer_map_insert (st, *tp) = t;
      /* Make sure we don't remap an already-remapped SAVE_EXPR.  */
      *pointer_map_insert (st, t) = t;
    }
  else
    {
      /* We've already walked into this SAVE_EXPR; don't do it again.  */
      *walk_subtrees = 0;
      t = *n;
    }

  /* Replace this SAVE_EXPR with the copy.  */
  *tp = t;
}

/* Called via walk_tree.  If *TP points to a DECL_STMT for a local label,
   copies the declaration and enters it in the splay_tree in DATA (which is
   really an `copy_body_data *').  */

static tree
mark_local_for_remap_r (tree *tp, int *walk_subtrees ATTRIBUTE_UNUSED,
			void *data)
{
  copy_body_data *id = (copy_body_data *) data;

  /* Don't walk into types.  */
  if (TYPE_P (*tp))
    *walk_subtrees = 0;

  else if (TREE_CODE (*tp) == LABEL_EXPR)
    {
      tree decl = TREE_OPERAND (*tp, 0);

      /* Copy the decl and remember the copy.  */
      insert_decl_map (id, decl, id->copy_decl (decl, id));
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
  copy_body_data *id = (copy_body_data *) data;
  struct pointer_map_t *st = id->decl_map;
  tree *n;

  /* Only a local declaration (variable or label).  */
  if ((TREE_CODE (*tp) == VAR_DECL && !TREE_STATIC (*tp))
      || TREE_CODE (*tp) == LABEL_DECL)
    {
      /* Lookup the declaration.  */
      n = (tree *) pointer_map_contains (st, *tp);

      /* If it's there, remap it.  */
      if (n)
	*tp = *n;
    }

  else if (TREE_CODE (*tp) == STATEMENT_LIST)
    gcc_unreachable ();
  else if (TREE_CODE (*tp) == BIND_EXPR)
    copy_bind_expr (tp, walk_subtrees, id);
  else if (TREE_CODE (*tp) == SAVE_EXPR
	   || TREE_CODE (*tp) == TARGET_EXPR)
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
  copy_body_data id;

  /* There's nothing to do for NULL_TREE.  */
  if (expr == 0)
    return expr;

  /* Set up ID.  */
  memset (&id, 0, sizeof (id));
  id.src_fn = current_function_decl;
  id.dst_fn = current_function_decl;
  id.decl_map = pointer_map_create ();
  id.debug_map = NULL;

  id.copy_decl = copy_decl_no_change;
  id.transform_call_graph_edges = CB_CGE_DUPLICATE;
  id.transform_new_cfg = false;
  id.transform_return_to_modify = false;
  id.transform_lang_insert_block = NULL;

  /* Walk the tree once to find local labels.  */
  walk_tree_without_duplicates (&expr, mark_local_for_remap_r, &id);

  /* Walk the tree again, copying, remapping, and unsaving.  */
  walk_tree (&expr, unsave_r, &id, NULL);

  /* Clean up.  */
  pointer_map_destroy (id.decl_map);
  if (id.debug_map)
    pointer_map_destroy (id.debug_map);

  return expr;
}

/* Called via walk_gimple_seq.  If *GSIP points to a GIMPLE_LABEL for a local
   label, copies the declaration and enters it in the splay_tree in DATA (which
   is really a 'copy_body_data *'.  */

static tree
mark_local_labels_stmt (gimple_stmt_iterator *gsip,
		        bool *handled_ops_p ATTRIBUTE_UNUSED,
		        struct walk_stmt_info *wi)
{
  copy_body_data *id = (copy_body_data *) wi->info;
  gimple stmt = gsi_stmt (*gsip);

  if (gimple_code (stmt) == GIMPLE_LABEL)
    {
      tree decl = gimple_label_label (stmt);

      /* Copy the decl and remember the copy.  */
      insert_decl_map (id, decl, id->copy_decl (decl, id));
    }

  return NULL_TREE;
}


/* Called via walk_gimple_seq by copy_gimple_seq_and_replace_local.
   Using the splay_tree pointed to by ST (which is really a `splay_tree'),
   remaps all local declarations to appropriate replacements in gimple
   operands. */

static tree
replace_locals_op (tree *tp, int *walk_subtrees, void *data)
{
  struct walk_stmt_info *wi = (struct walk_stmt_info*) data;
  copy_body_data *id = (copy_body_data *) wi->info;
  struct pointer_map_t *st = id->decl_map;
  tree *n;
  tree expr = *tp;

  /* Only a local declaration (variable or label).  */
  if ((TREE_CODE (expr) == VAR_DECL
       && !TREE_STATIC (expr))
      || TREE_CODE (expr) == LABEL_DECL)
    {
      /* Lookup the declaration.  */
      n = (tree *) pointer_map_contains (st, expr);

      /* If it's there, remap it.  */
      if (n)
	*tp = *n;
      *walk_subtrees = 0;
    }
  else if (TREE_CODE (expr) == STATEMENT_LIST
	   || TREE_CODE (expr) == BIND_EXPR
	   || TREE_CODE (expr) == SAVE_EXPR)
    gcc_unreachable ();
  else if (TREE_CODE (expr) == TARGET_EXPR)
    {
      /* Don't mess with a TARGET_EXPR that hasn't been expanded.
         It's OK for this to happen if it was part of a subtree that
         isn't immediately expanded, such as operand 2 of another
         TARGET_EXPR.  */
      if (!TREE_OPERAND (expr, 1))
	{
	  TREE_OPERAND (expr, 1) = TREE_OPERAND (expr, 3);
	  TREE_OPERAND (expr, 3) = NULL_TREE;
	}
    }

  /* Keep iterating.  */
  return NULL_TREE;
}


/* Called via walk_gimple_seq by copy_gimple_seq_and_replace_local.
   Using the splay_tree pointed to by ST (which is really a `splay_tree'),
   remaps all local declarations to appropriate replacements in gimple
   statements. */

static tree
replace_locals_stmt (gimple_stmt_iterator *gsip,
		     bool *handled_ops_p ATTRIBUTE_UNUSED,
		     struct walk_stmt_info *wi)
{
  copy_body_data *id = (copy_body_data *) wi->info;
  gimple stmt = gsi_stmt (*gsip);

  if (gimple_code (stmt) == GIMPLE_BIND)
    {
      tree block = gimple_bind_block (stmt);

      if (block)
	{
	  remap_block (&block, id);
	  gimple_bind_set_block (stmt, block);
	}

      /* This will remap a lot of the same decls again, but this should be
	 harmless.  */
      if (gimple_bind_vars (stmt))
	gimple_bind_set_vars (stmt, remap_decls (gimple_bind_vars (stmt), NULL, id));
    }

  /* Keep iterating.  */
  return NULL_TREE;
}


/* Copies everything in SEQ and replaces variables and labels local to
   current_function_decl.  */

gimple_seq
copy_gimple_seq_and_replace_locals (gimple_seq seq)
{
  copy_body_data id;
  struct walk_stmt_info wi;
  struct pointer_set_t *visited;
  gimple_seq copy;

  /* There's nothing to do for NULL_TREE.  */
  if (seq == NULL)
    return seq;

  /* Set up ID.  */
  memset (&id, 0, sizeof (id));
  id.src_fn = current_function_decl;
  id.dst_fn = current_function_decl;
  id.decl_map = pointer_map_create ();
  id.debug_map = NULL;

  id.copy_decl = copy_decl_no_change;
  id.transform_call_graph_edges = CB_CGE_DUPLICATE;
  id.transform_new_cfg = false;
  id.transform_return_to_modify = false;
  id.transform_lang_insert_block = NULL;

  /* Walk the tree once to find local labels.  */
  memset (&wi, 0, sizeof (wi));
  visited = pointer_set_create ();
  wi.info = &id;
  wi.pset = visited;
  walk_gimple_seq (seq, mark_local_labels_stmt, NULL, &wi);
  pointer_set_destroy (visited);

  copy = gimple_seq_copy (seq);

  /* Walk the copy, remapping decls.  */
  memset (&wi, 0, sizeof (wi));
  wi.info = &id;
  walk_gimple_seq (copy, replace_locals_stmt, replace_locals_op, &wi);

  /* Clean up.  */
  pointer_map_destroy (id.decl_map);
  if (id.debug_map)
    pointer_map_destroy (id.debug_map);

  return copy;
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

DEBUG_FUNCTION bool
debug_find_tree (tree top, tree search)
{
  return walk_tree_without_duplicates (&top, debug_find_tree_1, search) != 0;
}


/* Declare the variables created by the inliner.  Add all the variables in
   VARS to BIND_EXPR.  */

static void
declare_inline_vars (tree block, tree vars)
{
  tree t;
  for (t = vars; t; t = DECL_CHAIN (t))
    {
      DECL_SEEN_IN_BIND_EXPR_P (t) = 1;
      gcc_assert (!TREE_STATIC (t) && !TREE_ASM_WRITTEN (t));
      add_local_decl (cfun, t);
    }

  if (block)
    BLOCK_VARS (block) = chainon (BLOCK_VARS (block), vars);
}

/* Copy NODE (which must be a DECL).  The DECL originally was in the FROM_FN,
   but now it will be in the TO_FN.  PARM_TO_VAR means enable PARM_DECL to
   VAR_DECL translation.  */

static tree
copy_decl_for_dup_finish (copy_body_data *id, tree decl, tree copy)
{
  /* Don't generate debug information for the copy if we wouldn't have
     generated it for the copy either.  */
  DECL_ARTIFICIAL (copy) = DECL_ARTIFICIAL (decl);
  DECL_IGNORED_P (copy) = DECL_IGNORED_P (decl);

  /* Set the DECL_ABSTRACT_ORIGIN so the debugging routines know what
     declaration inspired this copy.  */
  DECL_ABSTRACT_ORIGIN (copy) = DECL_ORIGIN (decl);

  /* The new variable/label has no RTL, yet.  */
  if (CODE_CONTAINS_STRUCT (TREE_CODE (copy), TS_DECL_WRTL)
      && !TREE_STATIC (copy) && !DECL_EXTERNAL (copy))
    SET_DECL_RTL (copy, 0);

  /* These args would always appear unused, if not for this.  */
  TREE_USED (copy) = 1;

  /* Set the context for the new declaration.  */
  if (!DECL_CONTEXT (decl))
    /* Globals stay global.  */
    ;
  else if (DECL_CONTEXT (decl) != id->src_fn)
    /* Things that weren't in the scope of the function we're inlining
       from aren't in the scope we're inlining to, either.  */
    ;
  else if (TREE_STATIC (decl))
    /* Function-scoped static variables should stay in the original
       function.  */
    ;
  else
    /* Ordinary automatic local variables are now in the scope of the
       new function.  */
    DECL_CONTEXT (copy) = id->dst_fn;

  if (TREE_CODE (decl) == VAR_DECL
      /* C++ clones functions during parsing, before
	 referenced_vars.  */
      && gimple_referenced_vars (DECL_STRUCT_FUNCTION (id->src_fn))
      && referenced_var_lookup (DECL_STRUCT_FUNCTION (id->src_fn),
				DECL_UID (decl)))
    add_referenced_var (copy);

  return copy;
}

static tree
copy_decl_to_var (tree decl, copy_body_data *id)
{
  tree copy, type;

  gcc_assert (TREE_CODE (decl) == PARM_DECL
	      || TREE_CODE (decl) == RESULT_DECL);

  type = TREE_TYPE (decl);

  copy = build_decl (DECL_SOURCE_LOCATION (id->dst_fn),
		     VAR_DECL, DECL_NAME (decl), type);
  if (DECL_PT_UID_SET_P (decl))
    SET_DECL_PT_UID (copy, DECL_PT_UID (decl));
  TREE_ADDRESSABLE (copy) = TREE_ADDRESSABLE (decl);
  TREE_READONLY (copy) = TREE_READONLY (decl);
  TREE_THIS_VOLATILE (copy) = TREE_THIS_VOLATILE (decl);
  DECL_GIMPLE_REG_P (copy) = DECL_GIMPLE_REG_P (decl);

  return copy_decl_for_dup_finish (id, decl, copy);
}

/* Like copy_decl_to_var, but create a return slot object instead of a
   pointer variable for return by invisible reference.  */

static tree
copy_result_decl_to_var (tree decl, copy_body_data *id)
{
  tree copy, type;

  gcc_assert (TREE_CODE (decl) == PARM_DECL
	      || TREE_CODE (decl) == RESULT_DECL);

  type = TREE_TYPE (decl);
  if (DECL_BY_REFERENCE (decl))
    type = TREE_TYPE (type);

  copy = build_decl (DECL_SOURCE_LOCATION (id->dst_fn),
		     VAR_DECL, DECL_NAME (decl), type);
  if (DECL_PT_UID_SET_P (decl))
    SET_DECL_PT_UID (copy, DECL_PT_UID (decl));
  TREE_READONLY (copy) = TREE_READONLY (decl);
  TREE_THIS_VOLATILE (copy) = TREE_THIS_VOLATILE (decl);
  if (!DECL_BY_REFERENCE (decl))
    {
      TREE_ADDRESSABLE (copy) = TREE_ADDRESSABLE (decl);
      DECL_GIMPLE_REG_P (copy) = DECL_GIMPLE_REG_P (decl);
    }

  return copy_decl_for_dup_finish (id, decl, copy);
}

tree
copy_decl_no_change (tree decl, copy_body_data *id)
{
  tree copy;

  copy = copy_node (decl);

  /* The COPY is not abstract; it will be generated in DST_FN.  */
  DECL_ABSTRACT (copy) = 0;
  lang_hooks.dup_lang_specific_decl (copy);

  /* TREE_ADDRESSABLE isn't used to indicate that a label's address has
     been taken; it's for internal bookkeeping in expand_goto_internal.  */
  if (TREE_CODE (copy) == LABEL_DECL)
    {
      TREE_ADDRESSABLE (copy) = 0;
      LABEL_DECL_UID (copy) = -1;
    }

  return copy_decl_for_dup_finish (id, decl, copy);
}

static tree
copy_decl_maybe_to_var (tree decl, copy_body_data *id)
{
  if (TREE_CODE (decl) == PARM_DECL || TREE_CODE (decl) == RESULT_DECL)
    return copy_decl_to_var (decl, id);
  else
    return copy_decl_no_change (decl, id);
}

/* Return a copy of the function's argument tree.  */
static tree
copy_arguments_for_versioning (tree orig_parm, copy_body_data * id,
			       bitmap args_to_skip, tree *vars)
{
  tree arg, *parg;
  tree new_parm = NULL;
  int i = 0;

  parg = &new_parm;

  for (arg = orig_parm; arg; arg = DECL_CHAIN (arg), i++)
    if (!args_to_skip || !bitmap_bit_p (args_to_skip, i))
      {
        tree new_tree = remap_decl (arg, id);
	if (TREE_CODE (new_tree) != PARM_DECL)
	  new_tree = id->copy_decl (arg, id);
        lang_hooks.dup_lang_specific_decl (new_tree);
        *parg = new_tree;
	parg = &DECL_CHAIN (new_tree);
      }
    else if (!pointer_map_contains (id->decl_map, arg))
      {
	/* Make an equivalent VAR_DECL.  If the argument was used
	   as temporary variable later in function, the uses will be
	   replaced by local variable.  */
	tree var = copy_decl_to_var (arg, id);
	add_referenced_var (var);
	insert_decl_map (id, arg, var);
        /* Declare this new variable.  */
        DECL_CHAIN (var) = *vars;
        *vars = var;
      }
  return new_parm;
}

/* Return a copy of the function's static chain.  */
static tree
copy_static_chain (tree static_chain, copy_body_data * id)
{
  tree *chain_copy, *pvar;

  chain_copy = &static_chain;
  for (pvar = chain_copy; *pvar; pvar = &DECL_CHAIN (*pvar))
    {
      tree new_tree = remap_decl (*pvar, id);
      lang_hooks.dup_lang_specific_decl (new_tree);
      DECL_CHAIN (new_tree) = DECL_CHAIN (*pvar);
      *pvar = new_tree;
    }
  return static_chain;
}

/* Return true if the function is allowed to be versioned.
   This is a guard for the versioning functionality.  */

bool
tree_versionable_function_p (tree fndecl)
{
  return (!lookup_attribute ("noclone", DECL_ATTRIBUTES (fndecl))
	  && copy_forbidden (DECL_STRUCT_FUNCTION (fndecl), fndecl) == NULL);
}

/* Delete all unreachable basic blocks and update callgraph.
   Doing so is somewhat nontrivial because we need to update all clones and
   remove inline function that become unreachable.  */

static bool
delete_unreachable_blocks_update_callgraph (copy_body_data *id)
{
  bool changed = false;
  basic_block b, next_bb;

  find_unreachable_blocks ();

  /* Delete all unreachable basic blocks.  */

  for (b = ENTRY_BLOCK_PTR->next_bb; b != EXIT_BLOCK_PTR; b = next_bb)
    {
      next_bb = b->next_bb;

      if (!(b->flags & BB_REACHABLE))
	{
          gimple_stmt_iterator bsi;

          for (bsi = gsi_start_bb (b); !gsi_end_p (bsi); gsi_next (&bsi))
	    if (gimple_code (gsi_stmt (bsi)) == GIMPLE_CALL)
	      {
	        struct cgraph_edge *e;
		struct cgraph_node *node;

	        if ((e = cgraph_edge (id->dst_node, gsi_stmt (bsi))) != NULL)
		  {
		    if (!e->inline_failed)
		      cgraph_remove_node_and_inline_clones (e->callee);
		    else
	              cgraph_remove_edge (e);
		  }
		if (id->transform_call_graph_edges == CB_CGE_MOVE_CLONES
		    && id->dst_node->clones)
     		  for (node = id->dst_node->clones; node != id->dst_node;)
		    {
	              if ((e = cgraph_edge (node, gsi_stmt (bsi))) != NULL)
			{
		          if (!e->inline_failed)
		            cgraph_remove_node_and_inline_clones (e->callee);
			  else
	                    cgraph_remove_edge (e);
			}

		      if (node->clones)
			node = node->clones;
		      else if (node->next_sibling_clone)
			node = node->next_sibling_clone;
		      else
			{
			  while (node != id->dst_node && !node->next_sibling_clone)
			    node = node->clone_of;
			  if (node != id->dst_node)
			    node = node->next_sibling_clone;
			}
		    }
	      }
	  delete_basic_block (b);
	  changed = true;
	}
    }

  return changed;
}

/* Update clone info after duplication.  */

static void
update_clone_info (copy_body_data * id)
{
  struct cgraph_node *node;
  if (!id->dst_node->clones)
    return;
  for (node = id->dst_node->clones; node != id->dst_node;)
    {
      /* First update replace maps to match the new body.  */
      if (node->clone.tree_map)
        {
	  unsigned int i;
          for (i = 0; i < VEC_length (ipa_replace_map_p, node->clone.tree_map); i++)
	    {
	      struct ipa_replace_map *replace_info;
	      replace_info = VEC_index (ipa_replace_map_p, node->clone.tree_map, i);
	      walk_tree (&replace_info->old_tree, copy_tree_body_r, id, NULL);
	      walk_tree (&replace_info->new_tree, copy_tree_body_r, id, NULL);
	    }
	}
      if (node->clones)
	node = node->clones;
      else if (node->next_sibling_clone)
	node = node->next_sibling_clone;
      else
	{
	  while (node != id->dst_node && !node->next_sibling_clone)
	    node = node->clone_of;
	  if (node != id->dst_node)
	    node = node->next_sibling_clone;
	}
    }
}

/* Create a copy of a function's tree.
   OLD_DECL and NEW_DECL are FUNCTION_DECL tree nodes
   of the original function and the new copied function
   respectively.  In case we want to replace a DECL
   tree with another tree while duplicating the function's
   body, TREE_MAP represents the mapping between these
   trees. If UPDATE_CLONES is set, the call_stmt fields
   of edges of clones of the function will be updated.  

   If non-NULL ARGS_TO_SKIP determine function parameters to remove
   from new version.
   If non-NULL BLOCK_TO_COPY determine what basic blocks to copy.
   If non_NULL NEW_ENTRY determine new entry BB of the clone.
*/
void
tree_function_versioning (tree old_decl, tree new_decl,
			  VEC(ipa_replace_map_p,gc)* tree_map,
			  bool update_clones, bitmap args_to_skip,
			  bitmap blocks_to_copy, basic_block new_entry)
{
  struct cgraph_node *old_version_node;
  struct cgraph_node *new_version_node;
  copy_body_data id;
  tree p;
  unsigned i;
  struct ipa_replace_map *replace_info;
  basic_block old_entry_block, bb;
  VEC (gimple, heap) *init_stmts = VEC_alloc (gimple, heap, 10);

  tree old_current_function_decl = current_function_decl;
  tree vars = NULL_TREE;

  gcc_assert (TREE_CODE (old_decl) == FUNCTION_DECL
	      && TREE_CODE (new_decl) == FUNCTION_DECL);
  DECL_POSSIBLY_INLINED (old_decl) = 1;

  old_version_node = cgraph_get_node (old_decl);
  gcc_checking_assert (old_version_node);
  new_version_node = cgraph_get_node (new_decl);
  gcc_checking_assert (new_version_node);

  /* Copy over debug args.  */
  if (DECL_HAS_DEBUG_ARGS_P (old_decl))
    {
      VEC(tree, gc) **new_debug_args, **old_debug_args;
      gcc_checking_assert (decl_debug_args_lookup (new_decl) == NULL);
      DECL_HAS_DEBUG_ARGS_P (new_decl) = 0;
      old_debug_args = decl_debug_args_lookup (old_decl);
      if (old_debug_args)
	{
	  new_debug_args = decl_debug_args_insert (new_decl);
	  *new_debug_args = VEC_copy (tree, gc, *old_debug_args);
	}
    }

  /* Output the inlining info for this abstract function, since it has been
     inlined.  If we don't do this now, we can lose the information about the
     variables in the function when the blocks get blown away as soon as we
     remove the cgraph node.  */
  (*debug_hooks->outlining_inline_function) (old_decl);

  DECL_ARTIFICIAL (new_decl) = 1;
  DECL_ABSTRACT_ORIGIN (new_decl) = DECL_ORIGIN (old_decl);
  DECL_FUNCTION_PERSONALITY (new_decl) = DECL_FUNCTION_PERSONALITY (old_decl);

  /* Prepare the data structures for the tree copy.  */
  memset (&id, 0, sizeof (id));

  /* Generate a new name for the new version. */
  id.statements_to_fold = pointer_set_create ();

  id.decl_map = pointer_map_create ();
  id.debug_map = NULL;
  id.src_fn = old_decl;
  id.dst_fn = new_decl;
  id.src_node = old_version_node;
  id.dst_node = new_version_node;
  id.src_cfun = DECL_STRUCT_FUNCTION (old_decl);
  if (id.src_node->ipa_transforms_to_apply)
    {
      VEC(ipa_opt_pass,heap) * old_transforms_to_apply = id.dst_node->ipa_transforms_to_apply;
      unsigned int i;

      id.dst_node->ipa_transforms_to_apply = VEC_copy (ipa_opt_pass, heap,
					               id.src_node->ipa_transforms_to_apply);
      for (i = 0; i < VEC_length (ipa_opt_pass, old_transforms_to_apply); i++)
        VEC_safe_push (ipa_opt_pass, heap, id.dst_node->ipa_transforms_to_apply,
		       VEC_index (ipa_opt_pass,
		       		  old_transforms_to_apply,
				  i));
    }

  id.copy_decl = copy_decl_no_change;
  id.transform_call_graph_edges
    = update_clones ? CB_CGE_MOVE_CLONES : CB_CGE_MOVE;
  id.transform_new_cfg = true;
  id.transform_return_to_modify = false;
  id.transform_lang_insert_block = NULL;

  current_function_decl = new_decl;
  old_entry_block = ENTRY_BLOCK_PTR_FOR_FUNCTION
    (DECL_STRUCT_FUNCTION (old_decl));
  initialize_cfun (new_decl, old_decl,
		   old_entry_block->count);
  DECL_STRUCT_FUNCTION (new_decl)->gimple_df->ipa_pta
    = id.src_cfun->gimple_df->ipa_pta;
  push_cfun (DECL_STRUCT_FUNCTION (new_decl));

  /* Copy the function's static chain.  */
  p = DECL_STRUCT_FUNCTION (old_decl)->static_chain_decl;
  if (p)
    DECL_STRUCT_FUNCTION (new_decl)->static_chain_decl =
      copy_static_chain (DECL_STRUCT_FUNCTION (old_decl)->static_chain_decl,
			 &id);

  /* If there's a tree_map, prepare for substitution.  */
  if (tree_map)
    for (i = 0; i < VEC_length (ipa_replace_map_p, tree_map); i++)
      {
	gimple init;
	replace_info = VEC_index (ipa_replace_map_p, tree_map, i);
	if (replace_info->replace_p)
	  {
	    tree op = replace_info->new_tree;
	    if (!replace_info->old_tree)
	      {
		int i = replace_info->parm_num;
		tree parm;
		for (parm = DECL_ARGUMENTS (old_decl); i; parm = DECL_CHAIN (parm))
		  i --;
		replace_info->old_tree = parm;
	      }
		

	    STRIP_NOPS (op);

	    if (TREE_CODE (op) == VIEW_CONVERT_EXPR)
	      op = TREE_OPERAND (op, 0);

	    if (TREE_CODE (op) == ADDR_EXPR)
	      {
		op = TREE_OPERAND (op, 0);
		while (handled_component_p (op))
		  op = TREE_OPERAND (op, 0);
		if (TREE_CODE (op) == VAR_DECL)
		  add_referenced_var (op);
	      }
	    gcc_assert (TREE_CODE (replace_info->old_tree) == PARM_DECL);
	    init = setup_one_parameter (&id, replace_info->old_tree,
	    			        replace_info->new_tree, id.src_fn,
				        NULL,
				        &vars);
	    if (init)
	      VEC_safe_push (gimple, heap, init_stmts, init);
	  }
      }
  /* Copy the function's arguments.  */
  if (DECL_ARGUMENTS (old_decl) != NULL_TREE)
    DECL_ARGUMENTS (new_decl) =
      copy_arguments_for_versioning (DECL_ARGUMENTS (old_decl), &id,
      				     args_to_skip, &vars);

  DECL_INITIAL (new_decl) = remap_blocks (DECL_INITIAL (id.src_fn), &id);
  BLOCK_SUPERCONTEXT (DECL_INITIAL (new_decl)) = new_decl;

  declare_inline_vars (DECL_INITIAL (new_decl), vars);

  if (!VEC_empty (tree, DECL_STRUCT_FUNCTION (old_decl)->local_decls))
    /* Add local vars.  */
    add_local_variables (DECL_STRUCT_FUNCTION (old_decl), cfun, &id, false);

  if (DECL_RESULT (old_decl) != NULL_TREE)
    {
      tree old_name;
      DECL_RESULT (new_decl) = remap_decl (DECL_RESULT (old_decl), &id);
      lang_hooks.dup_lang_specific_decl (DECL_RESULT (new_decl));
      if (gimple_in_ssa_p (id.src_cfun)
	  && DECL_BY_REFERENCE (DECL_RESULT (old_decl))
	  && (old_name
	      = gimple_default_def (id.src_cfun, DECL_RESULT (old_decl))))
	{
	  tree new_name = make_ssa_name (DECL_RESULT (new_decl), NULL);
	  insert_decl_map (&id, old_name, new_name);
	  SSA_NAME_DEF_STMT (new_name) = gimple_build_nop ();
	  set_default_def (DECL_RESULT (new_decl), new_name);
	}
    }

  /* Copy the Function's body.  */
  copy_body (&id, old_entry_block->count, REG_BR_PROB_BASE,
	     ENTRY_BLOCK_PTR, EXIT_BLOCK_PTR, blocks_to_copy, new_entry);

  /* Renumber the lexical scoping (non-code) blocks consecutively.  */
  number_blocks (new_decl);

  /* We want to create the BB unconditionally, so that the addition of
     debug stmts doesn't affect BB count, which may in the end cause
     codegen differences.  */
  bb = split_edge (single_succ_edge (ENTRY_BLOCK_PTR));
  while (VEC_length (gimple, init_stmts))
    insert_init_stmt (&id, bb, VEC_pop (gimple, init_stmts));
  update_clone_info (&id);

  /* Remap the nonlocal_goto_save_area, if any.  */
  if (cfun->nonlocal_goto_save_area)
    {
      struct walk_stmt_info wi;

      memset (&wi, 0, sizeof (wi));
      wi.info = &id;
      walk_tree (&cfun->nonlocal_goto_save_area, remap_gimple_op_r, &wi, NULL);
    }

  /* Clean up.  */
  pointer_map_destroy (id.decl_map);
  if (id.debug_map)
    pointer_map_destroy (id.debug_map);
  free_dominance_info (CDI_DOMINATORS);
  free_dominance_info (CDI_POST_DOMINATORS);

  fold_marked_statements (0, id.statements_to_fold);
  pointer_set_destroy (id.statements_to_fold);
  fold_cond_expr_cond ();
  delete_unreachable_blocks_update_callgraph (&id);
  if (id.dst_node->analyzed)
    cgraph_rebuild_references ();
  update_ssa (TODO_update_ssa);

  /* After partial cloning we need to rescale frequencies, so they are
     within proper range in the cloned function.  */
  if (new_entry)
    {
      struct cgraph_edge *e;
      rebuild_frequencies ();

      new_version_node->count = ENTRY_BLOCK_PTR->count;
      for (e = new_version_node->callees; e; e = e->next_callee)
	{
	  basic_block bb = gimple_bb (e->call_stmt);
	  e->frequency = compute_call_stmt_bb_frequency (current_function_decl,
							 bb);
	  e->count = bb->count;
	}
      for (e = new_version_node->indirect_calls; e; e = e->next_callee)
	{
	  basic_block bb = gimple_bb (e->call_stmt);
	  e->frequency = compute_call_stmt_bb_frequency (current_function_decl,
							 bb);
	  e->count = bb->count;
	}
    }

  free_dominance_info (CDI_DOMINATORS);
  free_dominance_info (CDI_POST_DOMINATORS);

  gcc_assert (!id.debug_stmts);
  VEC_free (gimple, heap, init_stmts);
  pop_cfun ();
  current_function_decl = old_current_function_decl;
  gcc_assert (!current_function_decl
	      || DECL_STRUCT_FUNCTION (current_function_decl) == cfun);
  return;
}

/* EXP is CALL_EXPR present in a GENERIC expression tree.  Try to integrate
   the callee and return the inlined body on success.  */

tree
maybe_inline_call_in_expr (tree exp)
{
  tree fn = get_callee_fndecl (exp);

  /* We can only try to inline "const" functions.  */
  if (fn && TREE_READONLY (fn) && DECL_SAVED_TREE (fn))
    {
      struct pointer_map_t *decl_map = pointer_map_create ();
      call_expr_arg_iterator iter;
      copy_body_data id;
      tree param, arg, t;

      /* Remap the parameters.  */
      for (param = DECL_ARGUMENTS (fn), arg = first_call_expr_arg (exp, &iter);
	   param;
	   param = DECL_CHAIN (param), arg = next_call_expr_arg (&iter))
	*pointer_map_insert (decl_map, param) = arg;

      memset (&id, 0, sizeof (id));
      id.src_fn = fn;
      id.dst_fn = current_function_decl;
      id.src_cfun = DECL_STRUCT_FUNCTION (fn);
      id.decl_map = decl_map;

      id.copy_decl = copy_decl_no_change;
      id.transform_call_graph_edges = CB_CGE_DUPLICATE;
      id.transform_new_cfg = false;
      id.transform_return_to_modify = true;
      id.transform_lang_insert_block = NULL;

      /* Make sure not to unshare trees behind the front-end's back
	 since front-end specific mechanisms may rely on sharing.  */
      id.regimplify = false;
      id.do_not_unshare = true;

      /* We're not inside any EH region.  */
      id.eh_lp_nr = 0;

      t = copy_tree_body (&id);
      pointer_map_destroy (decl_map);

      /* We can only return something suitable for use in a GENERIC
	 expression tree.  */
      if (TREE_CODE (t) == MODIFY_EXPR)
	return TREE_OPERAND (t, 1);
    }

   return NULL_TREE;
}

/* Duplicate a type, fields and all.  */

tree
build_duplicate_type (tree type)
{
  struct copy_body_data id;

  memset (&id, 0, sizeof (id));
  id.src_fn = current_function_decl;
  id.dst_fn = current_function_decl;
  id.src_cfun = cfun;
  id.decl_map = pointer_map_create ();
  id.debug_map = NULL;
  id.copy_decl = copy_decl_no_change;

  type = remap_type_1 (type, &id);

  pointer_map_destroy (id.decl_map);
  if (id.debug_map)
    pointer_map_destroy (id.debug_map);

  TYPE_CANONICAL (type) = type;

  return type;
}
