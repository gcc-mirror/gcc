/* Inline functions for tree-flow.h
   Copyright (C) 2001, 2003 Free Software Foundation, Inc.
   Contributed by Diego Novillo <dnovillo@redhat.com>

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

#ifndef _TREE_FLOW_INLINE_H
#define _TREE_FLOW_INLINE_H 1

/* Inline functions for manipulating various data structures defined in
   tree-flow.h.  See tree-flow.h for documentation.  */

static inline var_ann_t
var_ann (tree t)
{
#if defined ENABLE_CHECKING
  if (t == NULL_TREE
      || !DECL_P (t)
      || (t->common.ann
	  && t->common.ann->common.type != VAR_ANN))
    abort ();
#endif

  return (var_ann_t) t->common.ann;
}

static inline var_ann_t
get_var_ann (tree var)
{
  var_ann_t ann = var_ann (var);
  return (ann) ? ann : create_var_ann (var);
}

static inline stmt_ann_t
stmt_ann (tree t)
{
#if defined ENABLE_CHECKING
  if (!is_gimple_stmt (t) && !is_essa_node (t))
    abort ();
#endif

  return (stmt_ann_t) t->common.ann;
}

static inline stmt_ann_t
get_stmt_ann (tree stmt)
{
  stmt_ann_t ann = stmt_ann (stmt);
  return (ann) ? ann : create_stmt_ann (stmt);
}


static inline enum tree_ann_type
ann_type (tree_ann ann)
{
  return ann->common.type;
}

static inline basic_block
bb_for_stmt (tree t)
{
  stmt_ann_t ann = stmt_ann (t);
  return ann ? ann->bb : NULL;
}

static inline varray_type
may_aliases (tree var)
{
  var_ann_t ann = var_ann (var);
  return ann ? ann->may_aliases : NULL;
}

static inline bool
has_hidden_use (tree var)
{
  var_ann_t ann = var_ann (var);
  return ann ? ann->has_hidden_use : false;
}

static inline void
set_has_hidden_use (tree var)
{
  var_ann_t ann = var_ann (var);
  if (ann == NULL)
    ann = create_var_ann (var);
  ann->has_hidden_use = 1;
}

static inline int
get_lineno (tree expr)
{
  if (expr == NULL_TREE)
    return -1;

  if (TREE_CODE (expr) == COMPOUND_EXPR)
    expr = TREE_OPERAND (expr, 0);

  if (! EXPR_LOCUS (expr))
    return -1;

  return EXPR_LINENO (expr);
}

static inline const char *
get_filename (tree expr)
{
  if (expr == NULL_TREE)
    return "???";

  if (TREE_CODE (expr) == COMPOUND_EXPR)
    expr = TREE_OPERAND (expr, 0);

  if (EXPR_LOCUS (expr) && EXPR_FILENAME (expr))
    return EXPR_FILENAME (expr);
  else
    return "???";
}

static inline void
modify_stmt (tree t)
{
  stmt_ann_t ann = stmt_ann (t);
  if (ann == NULL)
    ann = create_stmt_ann (t);
  ann->modified = 1;
}

static inline void
unmodify_stmt (tree t)
{
  stmt_ann_t ann = stmt_ann (t);
  if (ann == NULL)
    ann = create_stmt_ann (t);
  ann->modified = 0;
}

static inline bool
stmt_modified_p (tree t)
{
  stmt_ann_t ann = stmt_ann (t);

  /* Note that if the statement doesn't yet have an annotation, we consider it
     modified.  This will force the next call to get_stmt_operands to scan the
     statement.  */
  return ann ? ann->modified : true;
}

static inline def_optype
get_def_ops (stmt_ann_t ann)
{
  return ann ? ann->def_ops : NULL;
}

static inline use_optype
get_use_ops (stmt_ann_t ann)
{
  return ann ? ann->use_ops : NULL;
}

static inline vdef_optype
get_vdef_ops (stmt_ann_t ann)
{
  return ann ? ann->vdef_ops : NULL;
}

static inline vuse_optype
get_vuse_ops (stmt_ann_t ann)
{
  return ann ? ann->vuse_ops : NULL;
}

static inline tree *
get_use_op_ptr (use_optype uses, unsigned int index)
{
#ifdef ENABLE_CHECKING
  if (index >= uses->num_uses)
    abort();
#endif
  return uses->uses[index];
}

static inline tree *
get_def_op_ptr (def_optype defs, unsigned int index)
{
#ifdef ENABLE_CHECKING
  if (index >= defs->num_defs)
    abort();
#endif
  return defs->defs[index];
}

static inline tree *
get_vdef_result_ptr(vdef_optype vdefs, unsigned int index)
{
#ifdef ENABLE_CHECKING
  if (index >= vdefs->num_vdefs)
    abort();
#endif
  return &(vdefs->vdefs[index * 2]);
}

static inline tree *
get_vdef_op_ptr(vdef_optype vdefs, unsigned int index)
{
#ifdef ENABLE_CHECKING
  if (index >= vdefs->num_vdefs)
    abort();
#endif
  return &(vdefs->vdefs[index * 2 + 1]);
}

static inline tree *
get_vuse_op_ptr(vuse_optype vuses, unsigned int index)
{
#ifdef ENABLE_CHECKING
  if (index >= vuses->num_vuses)
    abort();
#endif
  return &(vuses->vuses[index]);
}

static inline void
start_ssa_stmt_operands (tree stmt ATTRIBUTE_UNUSED)
{
#ifdef ENABLE_CHECKING
  verify_start_operands (stmt);
#endif
}

static inline bitmap
addresses_taken (tree stmt)
{
  stmt_ann_t ann = stmt_ann (stmt);
  return ann ? ann->addresses_taken : NULL;
}

static dataflow_t
get_immediate_uses (tree stmt)
{
  stmt_ann_t ann = stmt_ann (stmt);
  return ann ? ann->df : NULL;
}

static inline int
num_immediate_uses (dataflow_t df)
{
  varray_type imm;

  if (!df)
    return 0;

  imm = df->immediate_uses;
  if (!imm)
    return df->uses[1] ? 2 : 1;

  return VARRAY_ACTIVE_SIZE (imm) + 2;
}

static inline tree
immediate_use (dataflow_t df, int num)
{
  if (!df)
    return NULL_TREE;

#ifdef ENABLE_CHECKING
  if (num >= num_immediate_uses (df))
    abort ();
#endif
  if (num < 2)
    return df->uses[num];
  return VARRAY_TREE (df->immediate_uses, num - 2);
}

static inline bb_ann_t
bb_ann (basic_block bb)
{
  return (bb_ann_t)bb->tree_annotations;
}

static inline tree
phi_nodes (basic_block bb)
{
  if (bb->index < 0)
    return NULL;
  return bb_ann (bb)->phi_nodes;
}

/* Set list of phi nodes of a basic block BB to L.  */

static inline void
set_phi_nodes (basic_block bb, tree l)
{
  tree phi;

  bb_ann (bb)->phi_nodes = l;
  for (phi = l; phi; phi = TREE_CHAIN (phi))
    set_bb_for_stmt (phi, bb);
}

/* Return the phi index number for an edge.  */
static inline int
phi_arg_from_edge (tree phi, edge e)
{
  int i;
#if defined ENABLE_CHECKING
  if (!phi || TREE_CODE (phi) != PHI_NODE)
    abort();
#endif

  for (i = 0; i < PHI_NUM_ARGS (phi); i++)
    if (PHI_ARG_EDGE (phi, i) == e)
      return i;

  return -1;
}


/* Return the phi argument number for an edge.  */
static inline struct phi_arg_d *
phi_element_for_edge (tree phi, edge e)
{
  int i;

  i = phi_arg_from_edge (phi, e);
  if (i != -1)
    return &(PHI_ARG_ELT (phi, i));
  else
    return (struct phi_arg_d *)NULL;
}

/*  -----------------------------------------------------------------------  */

static inline bool
is_exec_stmt (tree t)
{
  return (t && !IS_EMPTY_STMT (t) && t != error_mark_node);
}


/* Return true if this stmt can be the target of a control transfer stmt such
   as a goto.  */
static inline bool
is_label_stmt (tree t)
{
  if (t)
    switch (TREE_CODE (t))
      {
	case LABEL_DECL:
	case LABEL_EXPR:
	case CASE_LABEL_EXPR:
	  return true;
	default:
	  return false;
      }
  return false;
}

static inline bool
may_propagate_copy (tree dest, tree orig)
{
  /* FIXME.  GIMPLE is allowing pointer assignments and comparisons of
     pointers that have different alias sets.  This means that these
     pointers will have different memory tags associated to them.
     
     If we allow copy propagation in these cases, statements de-referencing
     the new pointer will now have a reference to a different memory tag
     with potentially incorrect SSA information.

     This was showing up in libjava/java/util/zip/ZipFile.java with code
     like:

     	struct java.io.BufferedInputStream *T.660;
	struct java.io.BufferedInputStream *T.647;
	struct java.io.InputStream *is;
	struct java.io.InputStream *is.662;
	[ ... ]
	T.660 = T.647;
	is = T.660;	<-- This ought to be type-casted
	is.662 = is;

     Also, f/name.c exposed a similar problem with a COND_EXPR predicate
     that was causing DOM to generate and equivalence with two pointers of
     alias-incompatible types:

     	struct _ffename_space *n;
	struct _ffename *ns;
	[ ... ]
	if (n == ns)
	  goto lab;
	...
	lab:
	return n;

     I think that GIMPLE should emit the appropriate type-casts.  For the
     time being, blocking copy-propagation in these cases is the safe thing
     to do.  */
  if (TREE_CODE (dest) == SSA_NAME
      && TREE_CODE (orig) == SSA_NAME
      && POINTER_TYPE_P (TREE_TYPE (dest))
      && POINTER_TYPE_P (TREE_TYPE (orig)))
    {
      tree mt_dest = var_ann (SSA_NAME_VAR (dest))->type_mem_tag;
      tree mt_orig = var_ann (SSA_NAME_VAR (orig))->type_mem_tag;
      if (mt_dest && mt_orig && mt_dest != mt_orig)
	return false;
    }

  /* If the destination is a SSA_NAME for a virtual operand, then we have
     some special cases to handle.  */
  if (TREE_CODE (dest) == SSA_NAME && !is_gimple_reg (dest))
    {
      /* If both operands are SSA_NAMEs referring to virtual operands, then
	 we can always propagate.  */
      if (TREE_CODE (orig) == SSA_NAME)
	{
	  if (!is_gimple_reg (orig))
	    return true;

#ifdef ENABLE_CHECKING
	  /* If we have one real and one virtual operand, then something has
	     gone terribly wrong.  */
	  if (is_gimple_reg (orig))
	    abort ();
#endif
	}

      /* We have a "copy" from something like a constant into a virtual
	 operand.  Reject these.  */
      return false;
    }

  return (!SSA_NAME_OCCURS_IN_ABNORMAL_PHI (dest)
	  && (TREE_CODE (orig) != SSA_NAME
	      || !SSA_NAME_OCCURS_IN_ABNORMAL_PHI (orig))
	  && !DECL_HARD_REGISTER (SSA_NAME_VAR (dest)));
}

static inline void
set_default_def (tree var, tree def)
{
  var_ann_t ann = var_ann (var);
  if (ann == NULL)
    ann = create_var_ann (var);
  ann->default_def = def;
}

static inline tree
default_def (tree var)
{
  var_ann_t ann = var_ann (var);
  return ann ? ann->default_def : NULL_TREE;
}

/* PHI nodes should contain only ssa_names and invariants.  A test
   for ssa_name is definitely simpler; don't let invalid contents
   slip in in the meantime.  */

static inline bool
phi_ssa_name_p (tree t)
{
  if (TREE_CODE (t) == SSA_NAME)
    return true;
#ifdef ENABLE_CHECKING
  if (!is_gimple_min_invariant (t))
    abort ();
#endif
  return false;
}

/*  -----------------------------------------------------------------------  */

static inline block_stmt_iterator
bsi_start (basic_block bb)
{
  block_stmt_iterator bsi;
  if (bb->stmt_list)
    bsi.tsi = tsi_start (bb->stmt_list);
  else
    {
#ifdef ENABLE_CHECKING
      if (bb->index >= 0)
	abort ();
#endif
      bsi.tsi.ptr = NULL;
      bsi.tsi.container = NULL;
    }
  bsi.bb = bb;
  return bsi;
}

static inline block_stmt_iterator
bsi_last (basic_block bb)
{
  block_stmt_iterator bsi;
  if (bb->stmt_list)
    bsi.tsi = tsi_last (bb->stmt_list);
  else
    {
#ifdef ENABLE_CHECKING
      if (bb->index >= 0)
	abort ();
#endif
      bsi.tsi.ptr = NULL;
      bsi.tsi.container = NULL;
    }
  bsi.bb = bb;
  return bsi;
}

static inline bool
bsi_end_p (block_stmt_iterator i)
{
  return tsi_end_p (i.tsi);
}

static inline void
bsi_next (block_stmt_iterator *i)
{
  tsi_next (&i->tsi);
}

static inline void
bsi_prev (block_stmt_iterator *i)
{
  tsi_prev (&i->tsi);
}

static inline tree
bsi_stmt (block_stmt_iterator i)
{
  return tsi_stmt (i.tsi);
}

static inline tree *
bsi_stmt_ptr (block_stmt_iterator i)
{
  return tsi_stmt_ptr (i.tsi);
}

static inline bool
may_be_aliased (tree var)
{
  return (TREE_ADDRESSABLE (var)
          || decl_function_context (var) != current_function_decl);
}

static inline bool
is_call_clobbered (tree var)
{
  return needs_to_live_in_memory (var)
	 || bitmap_bit_p (call_clobbered_vars, var_ann (var)->uid);
}

static inline void
mark_call_clobbered (tree var)
{
  var_ann_t ann = var_ann (var);
  /* Call-clobbered variables need to live in memory.  */
  DECL_NEEDS_TO_LIVE_IN_MEMORY_INTERNAL (var) = 1;
  bitmap_set_bit (call_clobbered_vars, ann->uid);
}

static inline void
mark_non_addressable (tree var)
{
  bitmap_clear_bit (call_clobbered_vars, var_ann (var)->uid);
  DECL_NEEDS_TO_LIVE_IN_MEMORY_INTERNAL (var) = 0;
  TREE_ADDRESSABLE (var) = 0;
}

#endif /* _TREE_FLOW_INLINE_H  */
