/* Inline functions for tree-flow.h
   Copyright (C) 2001, 2003, 2005 Free Software Foundation, Inc.
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

/* Return the variable annotation for T, which must be a _DECL node.
   Return NULL if the variable annotation doesn't already exist.  */
static inline var_ann_t
var_ann (tree t)
{
  gcc_assert (t);
  gcc_assert (DECL_P (t));
  gcc_assert (!t->common.ann || t->common.ann->common.type == VAR_ANN);

  return (var_ann_t) t->common.ann;
}

/* Return the variable annotation for T, which must be a _DECL node.
   Create the variable annotation if it doesn't exist.  */
static inline var_ann_t
get_var_ann (tree var)
{
  var_ann_t ann = var_ann (var);
  return (ann) ? ann : create_var_ann (var);
}

/* Return the statement annotation for T, which must be a statement
   node.  Return NULL if the statement annotation doesn't exist.  */
static inline stmt_ann_t
stmt_ann (tree t)
{
#ifdef ENABLE_CHECKING
  gcc_assert (is_gimple_stmt (t));
#endif
  return (stmt_ann_t) t->common.ann;
}

/* Return the statement annotation for T, which must be a statement
   node.  Create the statement annotation if it doesn't exist.  */
static inline stmt_ann_t
get_stmt_ann (tree stmt)
{
  stmt_ann_t ann = stmt_ann (stmt);
  return (ann) ? ann : create_stmt_ann (stmt);
}


/* Return the annotation type for annotation ANN.  */
static inline enum tree_ann_type
ann_type (tree_ann_t ann)
{
  return ann->common.type;
}

/* Return the basic block for statement T.  */
static inline basic_block
bb_for_stmt (tree t)
{
  stmt_ann_t ann;

  if (TREE_CODE (t) == PHI_NODE)
    return PHI_BB (t);

  ann = stmt_ann (t);
  return ann ? ann->bb : NULL;
}

/* Return the may_aliases varray for variable VAR, or NULL if it has
   no may aliases.  */
static inline varray_type
may_aliases (tree var)
{
  var_ann_t ann = var_ann (var);
  return ann ? ann->may_aliases : NULL;
}

/* Return the line number for EXPR, or return -1 if we have no line
   number information for it.  */
static inline int
get_lineno (tree expr)
{
  if (expr == NULL_TREE)
    return -1;

  if (TREE_CODE (expr) == COMPOUND_EXPR)
    expr = TREE_OPERAND (expr, 0);

  if (! EXPR_HAS_LOCATION (expr))
    return -1;

  return EXPR_LINENO (expr);
}

/* Return the file name for EXPR, or return "???" if we have no
   filename information.  */
static inline const char *
get_filename (tree expr)
{
  const char *filename;
  if (expr == NULL_TREE)
    return "???";

  if (TREE_CODE (expr) == COMPOUND_EXPR)
    expr = TREE_OPERAND (expr, 0);

  if (EXPR_HAS_LOCATION (expr) && (filename = EXPR_FILENAME (expr)))
    return filename;
  else
    return "???";
}

/* Return true if T is a noreturn call.  */
static inline bool
noreturn_call_p (tree t)
{
  tree call = get_call_expr_in (t);
  return call != 0 && (call_expr_flags (call) & ECF_NORETURN) != 0;
}

/* Mark statement T as modified.  */
static inline void
modify_stmt (tree t)
{
  stmt_ann_t ann = stmt_ann (t);
  if (ann == NULL)
    ann = create_stmt_ann (t);
  else if (noreturn_call_p (t))
    VEC_safe_push (tree, modified_noreturn_calls, t);
  ann->modified = 1;
}

/* Mark statement T as unmodified.  */
static inline void
unmodify_stmt (tree t)
{
  stmt_ann_t ann = stmt_ann (t);
  if (ann == NULL)
    ann = create_stmt_ann (t);
  ann->modified = 0;
}

/* Return true if T is marked as modified, false otherwise.  */
static inline bool
stmt_modified_p (tree t)
{
  stmt_ann_t ann = stmt_ann (t);

  /* Note that if the statement doesn't yet have an annotation, we consider it
     modified.  This will force the next call to get_stmt_operands to scan the
     statement.  */
  return ann ? ann->modified : true;
}

/* Return the definitions present in ANN, a statement annotation.
   Return NULL if this annotation contains no definitions.  */
static inline def_optype
get_def_ops (stmt_ann_t ann)
{
  return ann ? ann->operands.def_ops : NULL;
}

/* Return the uses present in ANN, a statement annotation.
   Return NULL if this annotation contains no uses.  */
static inline use_optype
get_use_ops (stmt_ann_t ann)
{
  return ann ? ann->operands.use_ops : NULL;
}

/* Return the virtual may-defs present in ANN, a statement
   annotation.
   Return NULL if this annotation contains no virtual may-defs.  */
static inline v_may_def_optype
get_v_may_def_ops (stmt_ann_t ann)
{
  return ann ? ann->operands.v_may_def_ops : NULL;
}

/* Return the virtual uses present in ANN, a statement annotation.
   Return NULL if this annotation contains no virtual uses.  */
static inline vuse_optype
get_vuse_ops (stmt_ann_t ann)
{
  return ann ? ann->operands.vuse_ops : NULL;
}

/* Return the virtual must-defs present in ANN, a statement
   annotation.  Return NULL if this annotation contains no must-defs.*/
static inline v_must_def_optype
get_v_must_def_ops (stmt_ann_t ann)
{
  return ann ? ann->operands.v_must_def_ops : NULL;
}

/* Return the tree pointer to by USE.  */ 
static inline tree
get_use_from_ptr (use_operand_p use)
{ 
  return *(use.use);
} 

/* Return the tree pointer to by DEF.  */
static inline tree
get_def_from_ptr (def_operand_p def)
{
  return *(def.def);
}

/* Return a pointer to the tree that is at INDEX in the USES array.  */
static inline use_operand_p
get_use_op_ptr (use_optype uses, unsigned int index)
{
  gcc_assert (index < uses->num_uses);
  return uses->uses[index];
}

/* Return a def_operand_p pointer for element INDEX of DEFS.  */
static inline def_operand_p
get_def_op_ptr (def_optype defs, unsigned int index)
{
  gcc_assert (index < defs->num_defs);
  return defs->defs[index];
}


/* Return the def_operand_p that is the V_MAY_DEF_RESULT for the V_MAY_DEF
   at INDEX in the V_MAY_DEFS array.  */
static inline def_operand_p
get_v_may_def_result_ptr(v_may_def_optype v_may_defs, unsigned int index)
{
  def_operand_p op;
  gcc_assert (index < v_may_defs->num_v_may_defs);
  op.def = &(v_may_defs->v_may_defs[index].def);
  return op;
}

/* Return a use_operand_p that is the V_MAY_DEF_OP for the V_MAY_DEF at
   INDEX in the V_MAY_DEFS array.  */
static inline use_operand_p
get_v_may_def_op_ptr(v_may_def_optype v_may_defs, unsigned int index)
{
  use_operand_p op;
  gcc_assert (index < v_may_defs->num_v_may_defs);
  op.use = &(v_may_defs->v_may_defs[index].use);
  return op;
}

/* Return a use_operand_p that is at INDEX in the VUSES array.  */
static inline use_operand_p
get_vuse_op_ptr(vuse_optype vuses, unsigned int index)
{
  use_operand_p op;
  gcc_assert (index < vuses->num_vuses);
  op.use = &(vuses->vuses[index]);
  return op;
}

/* Return a def_operand_p that is the V_MUST_DEF_RESULT for the
   V_MUST_DEF at INDEX in the V_MUST_DEFS array.  */
static inline def_operand_p
get_v_must_def_result_ptr (v_must_def_optype v_must_defs, unsigned int index)
{
  def_operand_p op;
  gcc_assert (index < v_must_defs->num_v_must_defs);
  op.def = &(v_must_defs->v_must_defs[index].def);
  return op;
}

/* Return a use_operand_p that is the V_MUST_DEF_KILL for the 
   V_MUST_DEF at INDEX in the V_MUST_DEFS array.  */
static inline use_operand_p
get_v_must_def_kill_ptr (v_must_def_optype v_must_defs, unsigned int index)
{
  use_operand_p op;
  gcc_assert (index < v_must_defs->num_v_must_defs);
  op.use = &(v_must_defs->v_must_defs[index].use);
  return op;
}

/* Return a def_operand_p pointer for the result of PHI.  */
static inline def_operand_p
get_phi_result_ptr (tree phi)
{
  def_operand_p op;
  op.def = &(PHI_RESULT_TREE (phi));
  return op;
}

/* Return a use_operand_p pointer for argument I of phinode PHI.  */
static inline use_operand_p
get_phi_arg_def_ptr (tree phi, int i)
{
  use_operand_p op;
  op.use = &(PHI_ARG_DEF_TREE (phi, i));
  return op;
}
 
/* Return the bitmap of addresses taken by STMT, or NULL if it takes
   no addresses.  */
static inline bitmap
addresses_taken (tree stmt)
{
  stmt_ann_t ann = stmt_ann (stmt);
  return ann ? ann->addresses_taken : NULL;
}

/* Return the immediate uses of STMT, or NULL if this information is
   not computed.  */
static dataflow_t
get_immediate_uses (tree stmt)
{
  stmt_ann_t ann;

  if (TREE_CODE (stmt) == PHI_NODE)
    return PHI_DF (stmt);

  ann = stmt_ann (stmt);
  return ann ? ann->df : NULL;
}

/* Return the number of immediate uses present in the dataflow
   information at DF.  */
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

/* Return the tree that is at NUM in the immediate use DF array.  */
static inline tree
immediate_use (dataflow_t df, int num)
{
  if (!df)
    return NULL_TREE;

#ifdef ENABLE_CHECKING
  gcc_assert (num < num_immediate_uses (df));
#endif
  if (num < 2)
    return df->uses[num];
  return VARRAY_TREE (df->immediate_uses, num - 2);
}

/* Return the basic_block annotation for BB.  */
static inline bb_ann_t
bb_ann (basic_block bb)
{
  return (bb_ann_t)bb->tree_annotations;
}

/* Return the PHI nodes for basic block BB, or NULL if there are no
   PHI nodes.  */
static inline tree
phi_nodes (basic_block bb)
{
  return bb_ann (bb)->phi_nodes;
}

/* Set list of phi nodes of a basic block BB to L.  */

static inline void
set_phi_nodes (basic_block bb, tree l)
{
  tree phi;

  bb_ann (bb)->phi_nodes = l;
  for (phi = l; phi; phi = PHI_CHAIN (phi))
    set_bb_for_stmt (phi, bb);
}

/* Mark VAR as used, so that it'll be preserved during rtl expansion.  */

static inline void
set_is_used (tree var)
{
  var_ann_t ann = get_var_ann (var);
  ann->used = 1;
}


/*  -----------------------------------------------------------------------  */

/* Return true if T is an executable statement.  */
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

/* Set the default definition for VAR to DEF.  */
static inline void
set_default_def (tree var, tree def)
{
  var_ann_t ann = get_var_ann (var);
  ann->default_def = def;
}

/* Return the default definition for variable VAR, or NULL if none
   exists.  */
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
  gcc_assert (is_gimple_min_invariant (t));
#endif
  return false;
}

/*  -----------------------------------------------------------------------  */

/* Return a block_stmt_iterator that points to beginning of basic
   block BB.  */
static inline block_stmt_iterator
bsi_start (basic_block bb)
{
  block_stmt_iterator bsi;
  if (bb->stmt_list)
    bsi.tsi = tsi_start (bb->stmt_list);
  else
    {
      gcc_assert (bb->index < 0);
      bsi.tsi.ptr = NULL;
      bsi.tsi.container = NULL;
    }
  bsi.bb = bb;
  return bsi;
}

/* Return a block statement iterator that points to the last label in
   block BB.  */

static inline block_stmt_iterator
bsi_after_labels (basic_block bb)
{
  block_stmt_iterator bsi;
  tree_stmt_iterator next;

  bsi.bb = bb;

  if (!bb->stmt_list)
    {
      gcc_assert (bb->index < 0);
      bsi.tsi.ptr = NULL;
      bsi.tsi.container = NULL;
      return bsi;
    }

  bsi.tsi = tsi_start (bb->stmt_list);
  if (tsi_end_p (bsi.tsi))
    return bsi;

  /* Ensure that there are some labels.  The rationale is that we want
     to insert after the bsi that is returned, and these insertions should
     be placed at the start of the basic block.  This would not work if the
     first statement was not label; rather fail here than enable the user
     proceed in wrong way.  */
  gcc_assert (TREE_CODE (tsi_stmt (bsi.tsi)) == LABEL_EXPR);

  next = bsi.tsi;
  tsi_next (&next);

  while (!tsi_end_p (next)
	 && TREE_CODE (tsi_stmt (next)) == LABEL_EXPR)
    {
      bsi.tsi = next;
      tsi_next (&next);
    }

  return bsi;
}

/* Return a block statement iterator that points to the end of basic
   block BB.  */
static inline block_stmt_iterator
bsi_last (basic_block bb)
{
  block_stmt_iterator bsi;
  if (bb->stmt_list)
    bsi.tsi = tsi_last (bb->stmt_list);
  else
    {
      gcc_assert (bb->index < 0);
      bsi.tsi.ptr = NULL;
      bsi.tsi.container = NULL;
    }
  bsi.bb = bb;
  return bsi;
}

/* Return true if block statement iterator I has reached the end of
   the basic block.  */
static inline bool
bsi_end_p (block_stmt_iterator i)
{
  return tsi_end_p (i.tsi);
}

/* Modify block statement iterator I so that it is at the next
   statement in the basic block.  */
static inline void
bsi_next (block_stmt_iterator *i)
{
  tsi_next (&i->tsi);
}

/* Modify block statement iterator I so that it is at the previous
   statement in the basic block.  */
static inline void
bsi_prev (block_stmt_iterator *i)
{
  tsi_prev (&i->tsi);
}

/* Return the statement that block statement iterator I is currently
   at.  */
static inline tree
bsi_stmt (block_stmt_iterator i)
{
  return tsi_stmt (i.tsi);
}

/* Return a pointer to the statement that block statement iterator I
   is currently at.  */
static inline tree *
bsi_stmt_ptr (block_stmt_iterator i)
{
  return tsi_stmt_ptr (i.tsi);
}

/* Returns the loop of the statement STMT.  */

static inline struct loop *
loop_containing_stmt (tree stmt)
{
  basic_block bb = bb_for_stmt (stmt);
  if (!bb)
    return NULL;

  return bb->loop_father;
}

/* Return true if VAR is a clobbered by function calls.  */
static inline bool
is_call_clobbered (tree var)
{
  return is_global_var (var)
	 || bitmap_bit_p (call_clobbered_vars, var_ann (var)->uid);
}

/* Mark variable VAR as being clobbered by function calls.  */
static inline void
mark_call_clobbered (tree var)
{
  var_ann_t ann = var_ann (var);
  /* If VAR is a memory tag, then we need to consider it a global
     variable.  This is because the pointer that VAR represents has
     been found to point to either an arbitrary location or to a known
     location in global memory.  */
  if (ann->mem_tag_kind != NOT_A_TAG)
    DECL_EXTERNAL (var) = 1;
  bitmap_set_bit (call_clobbered_vars, ann->uid);
  ssa_call_clobbered_cache_valid = false;
  ssa_ro_call_cache_valid = false;
}

/* Clear the call-clobbered attribute from variable VAR.  */
static inline void
clear_call_clobbered (tree var)
{
  var_ann_t ann = var_ann (var);
  if (ann->mem_tag_kind != NOT_A_TAG)
    DECL_EXTERNAL (var) = 0;
  bitmap_clear_bit (call_clobbered_vars, ann->uid);
  ssa_call_clobbered_cache_valid = false;
  ssa_ro_call_cache_valid = false;
}

/* Mark variable VAR as being non-addressable.  */
static inline void
mark_non_addressable (tree var)
{
  bitmap_clear_bit (call_clobbered_vars, var_ann (var)->uid);
  TREE_ADDRESSABLE (var) = 0;
  ssa_call_clobbered_cache_valid = false;
  ssa_ro_call_cache_valid = false;
}

/* Return the common annotation for T.  Return NULL if the annotation
   doesn't already exist.  */
static inline tree_ann_t
tree_ann (tree t)
{
  return t->common.ann;
}

/* Return a common annotation for T.  Create the constant annotation if it
   doesn't exist.  */
static inline tree_ann_t
get_tree_ann (tree t)
{
  tree_ann_t ann = tree_ann (t);
  return (ann) ? ann : create_tree_ann (t);
}

/*  -----------------------------------------------------------------------  */

/* The following set of routines are used to iterator over various type of
   SSA operands.  */

/* Return true if PTR is finished iterating.  */
static inline bool
op_iter_done (ssa_op_iter *ptr)
{
  return ptr->done;
}

/* Get the next iterator use value for PTR.  */
static inline use_operand_p
op_iter_next_use (ssa_op_iter *ptr)
{
  if (ptr->use_i < ptr->num_use)
    {
      return USE_OP_PTR (ptr->ops->use_ops, (ptr->use_i)++);
    }
  if (ptr->vuse_i < ptr->num_vuse)
    {
      return VUSE_OP_PTR (ptr->ops->vuse_ops, (ptr->vuse_i)++);
    }
  if (ptr->v_mayu_i < ptr->num_v_mayu)
    {
      return V_MAY_DEF_OP_PTR (ptr->ops->v_may_def_ops,
			       (ptr->v_mayu_i)++);
    }
  if (ptr->v_mustu_i < ptr->num_v_mustu)
    {
      return V_MUST_DEF_KILL_PTR (ptr->ops->v_must_def_ops,
				  (ptr->v_mustu_i)++);
    }
  ptr->done = true;
  return NULL_USE_OPERAND_P;
}

/* Get the next iterator def value for PTR.  */
static inline def_operand_p
op_iter_next_def (ssa_op_iter *ptr)
{
  if (ptr->def_i < ptr->num_def)
    {
      return DEF_OP_PTR (ptr->ops->def_ops, (ptr->def_i)++);
    }
  if (ptr->v_mustd_i < ptr->num_v_mustd)
    {
      return V_MUST_DEF_RESULT_PTR (ptr->ops->v_must_def_ops, 
					(ptr->v_mustd_i)++);
    }
  if (ptr->v_mayd_i < ptr->num_v_mayd)
    {
      return V_MAY_DEF_RESULT_PTR (ptr->ops->v_may_def_ops,
					   (ptr->v_mayd_i)++);
    }
  ptr->done = true;
  return NULL_DEF_OPERAND_P;
}

/* Get the next iterator tree value for PTR.  */
static inline tree
op_iter_next_tree (ssa_op_iter *ptr)
{
  if (ptr->use_i < ptr->num_use)
    {
      return USE_OP (ptr->ops->use_ops, (ptr->use_i)++);
    }
  if (ptr->vuse_i < ptr->num_vuse)
    {
      return VUSE_OP (ptr->ops->vuse_ops, (ptr->vuse_i)++);
    }
  if (ptr->v_mayu_i < ptr->num_v_mayu)
    {
      return V_MAY_DEF_OP (ptr->ops->v_may_def_ops, (ptr->v_mayu_i)++);
    }
  if (ptr->v_mustu_i < ptr->num_v_mustu)
    {
      return V_MUST_DEF_KILL (ptr->ops->v_must_def_ops, (ptr->v_mustu_i)++);
    }
  if (ptr->def_i < ptr->num_def)
    {
      return DEF_OP (ptr->ops->def_ops, (ptr->def_i)++);
    }
  if (ptr->v_mustd_i < ptr->num_v_mustd)
    {
      return V_MUST_DEF_RESULT (ptr->ops->v_must_def_ops, 
					(ptr->v_mustd_i)++);
    }
  if (ptr->v_mayd_i < ptr->num_v_mayd)
    {
      return V_MAY_DEF_RESULT (ptr->ops->v_may_def_ops,
					   (ptr->v_mayd_i)++);
    }
  ptr->done = true;
  return NULL;
}

/* Initialize the iterator PTR to the virtual defs in STMT.  */
static inline void
op_iter_init (ssa_op_iter *ptr, tree stmt, int flags)
{
  stmt_operands_p ops;
  stmt_ann_t ann = get_stmt_ann (stmt);

  ops = &(ann->operands);
  ptr->done = false;
  ptr->ops = ops;
  ptr->num_def = (flags & SSA_OP_DEF) ? NUM_DEFS (ops->def_ops) : 0;
  ptr->num_use = (flags & SSA_OP_USE) ? NUM_USES (ops->use_ops) : 0;
  ptr->num_vuse = (flags & SSA_OP_VUSE) ? NUM_VUSES (ops->vuse_ops) : 0;
  ptr->num_v_mayu = (flags & SSA_OP_VMAYUSE)
		     ?  NUM_V_MAY_DEFS (ops->v_may_def_ops) : 0;
  ptr->num_v_mayd = (flags & SSA_OP_VMAYDEF) 
		     ?  NUM_V_MAY_DEFS (ops->v_may_def_ops) : 0;
  ptr->num_v_mustu = (flags & SSA_OP_VMUSTDEFKILL)
                     ? NUM_V_MUST_DEFS (ops->v_must_def_ops) : 0;
  ptr->num_v_mustd = (flags & SSA_OP_VMUSTDEF) 
		     ? NUM_V_MUST_DEFS (ops->v_must_def_ops) : 0;
  ptr->def_i = 0;
  ptr->use_i = 0;
  ptr->vuse_i = 0;
  ptr->v_mayu_i = 0;
  ptr->v_mayd_i = 0;
  ptr->v_mustu_i = 0;
  ptr->v_mustd_i = 0;
}

/* Initialize iterator PTR to the use operands in STMT based on FLAGS. Return
   the first use.  */
static inline use_operand_p
op_iter_init_use (ssa_op_iter *ptr, tree stmt, int flags)
{
  op_iter_init (ptr, stmt, flags);
  return op_iter_next_use (ptr);
}

/* Initialize iterator PTR to the def operands in STMT based on FLAGS. Return
   the first def.  */
static inline def_operand_p
op_iter_init_def (ssa_op_iter *ptr, tree stmt, int flags)
{
  op_iter_init (ptr, stmt, flags);
  return op_iter_next_def (ptr);
}

/* Initialize iterator PTR to the operands in STMT based on FLAGS. Return
   the first operand as a tree.  */
static inline tree
op_iter_init_tree (ssa_op_iter *ptr, tree stmt, int flags)
{
  op_iter_init (ptr, stmt, flags);
  return op_iter_next_tree (ptr);
}

/* Get the next iterator mustdef value for PTR, returning the mustdef values in
   KILL and DEF.  */
static inline void
op_iter_next_mustdef (use_operand_p *kill, def_operand_p *def, ssa_op_iter *ptr)
{
  if (ptr->v_mustu_i < ptr->num_v_mustu)
    {
      *def = V_MUST_DEF_RESULT_PTR (ptr->ops->v_must_def_ops, ptr->v_mustu_i);
      *kill = V_MUST_DEF_KILL_PTR (ptr->ops->v_must_def_ops, (ptr->v_mustu_i)++);
      return;
    }
  else
    {
      *def = NULL_DEF_OPERAND_P;
      *kill = NULL_USE_OPERAND_P;
    }
  ptr->done = true;
  return;
}
/* Get the next iterator maydef value for PTR, returning the maydef values in
   USE and DEF.  */
static inline void
op_iter_next_maydef (use_operand_p *use, def_operand_p *def, ssa_op_iter *ptr)
{
  if (ptr->v_mayu_i < ptr->num_v_mayu)
    {
      *def = V_MAY_DEF_RESULT_PTR (ptr->ops->v_may_def_ops, ptr->v_mayu_i);
      *use = V_MAY_DEF_OP_PTR (ptr->ops->v_may_def_ops, (ptr->v_mayu_i)++);
      return;
    }
  else
    {
      *def = NULL_DEF_OPERAND_P;
      *use = NULL_USE_OPERAND_P;
    }
  ptr->done = true;
  return;
}

/* Initialize iterator PTR to the operands in STMT.  Return the first operands
   in USE and DEF.  */
static inline void
op_iter_init_maydef (ssa_op_iter *ptr, tree stmt, use_operand_p *use, 
		     def_operand_p *def)
{
  op_iter_init (ptr, stmt, SSA_OP_VMAYUSE);
  op_iter_next_maydef (use, def, ptr);
}

/* Initialize iterator PTR to the operands in STMT.  Return the first operands
   in KILL and DEF.  */
static inline void
op_iter_init_mustdef (ssa_op_iter *ptr, tree stmt, use_operand_p *kill, 
		     def_operand_p *def)
{
  op_iter_init (ptr, stmt, SSA_OP_VMUSTDEFKILL);
  op_iter_next_mustdef (kill, def, ptr);
}
#endif /* _TREE_FLOW_INLINE_H  */
