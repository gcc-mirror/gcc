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
mark_stmt_modified (tree t)
{
  stmt_ann_t ann;
  if (TREE_CODE (t) == PHI_NODE)
    return;

  ann = stmt_ann (t);
  if (ann == NULL)
    ann = create_stmt_ann (t);
  else if (noreturn_call_p (t))
    VEC_safe_push (tree, modified_noreturn_calls, t);
  ann->modified = 1;
}

/* Mark statement T as modified, and update it.  */
static inline void
update_stmt (tree t)
{
  if (TREE_CODE (t) == PHI_NODE)
    return;
  mark_stmt_modified (t);
  update_stmt_operands (t);
}

static inline void
update_stmt_if_modified (tree t)
{
  if (stmt_modified_p (t))
    update_stmt_operands (t);
}

static inline void 
get_stmt_operands (tree stmt ATTRIBUTE_UNUSED)
{
#ifdef ENABLE_CHECKING
  stmt_ann_t ann;
                                                                                
  /* The optimizers cannot handle statements that are nothing but a
     _DECL.  This indicates a bug in the gimplifier.  */
  gcc_assert (!SSA_VAR_P (stmt));
                                                                                
  /* Ignore error statements.  */
  if (TREE_CODE (stmt) == ERROR_MARK)
    return;
                                                                                
  ann = get_stmt_ann (stmt);
  gcc_assert (!ann->modified);

  return;
#endif
}

/* Return true if T is marked as modified, false otherwise.  */
static inline bool
stmt_modified_p (tree t)
{
  stmt_ann_t ann = stmt_ann (t);

  /* Note that if the statement doesn't yet have an annotation, we consider it
     modified.  This will force the next call to update_stmt_operands to scan 
     the statement.  */
  return ann ? ann->modified : true;
}

/* Delink an immediate_uses node from its chain.  */
static inline void
delink_imm_use (ssa_imm_use_t *linknode)
{
  /* Return if this node is not in a list.  */
  if (linknode->prev == NULL)
    return;

  linknode->prev->next = linknode->next;
  linknode->next->prev = linknode->prev;
  linknode->prev = NULL;
  linknode->next = NULL;
}

/* Link ssa_imm_use node LINKNODE into the chain for LIST.  */
static inline void
link_imm_use_to_list (ssa_imm_use_t *linknode, ssa_imm_use_t *list)
{
  /* Link the new node at the head of the list.  If we are in the process of 
     traversing the list, we wont visit any new nodes added to it.  */
  linknode->prev = list;
  linknode->next = list->next;
  list->next->prev = linknode;
  list->next = linknode;
}

/* Link ssa_imm_use node LINKNODE into the chain for DEF.  */
static inline void
link_imm_use (ssa_imm_use_t *linknode, tree def)
{
  ssa_imm_use_t *root;

  if (!def || TREE_CODE (def) != SSA_NAME)
    linknode->prev = NULL;
  else
    {
      root = &(SSA_NAME_IMM_USE_NODE (def));
#ifdef ENABLE_CHECKING
      if (linknode->use)
        gcc_assert (*(linknode->use) == def);
#endif
      link_imm_use_to_list (linknode, root);
    }
}

/* Set the value of a use pointed by USE to VAL.  */
static inline void
set_ssa_use_from_ptr (use_operand_p use, tree val)
{
  delink_imm_use (use);
  *(use->use) = val;
  link_imm_use (use, val);
}

/* Link ssa_imm_use node LINKNODE into the chain for DEF, with use occuring 
   in STMT.  */
static inline void
link_imm_use_stmt (ssa_imm_use_t *linknode, tree def, tree stmt)
{
  if (stmt)
    link_imm_use (linknode, def);
  else
    link_imm_use (linknode, NULL);
  linknode->stmt = stmt;
}

/* Relink a new node in place of an old node in the list.  */
static inline void
relink_imm_use (ssa_imm_use_t *node, ssa_imm_use_t *old)
{
#ifdef ENABLE_CHECKING
  /* The node one had better be in the same list.  */
  if (*(old->use) != *(node->use))
    abort ();
#endif
  node->prev = old->prev;
  node->next = old->next;
  if (old->prev)
    {
      old->prev->next = node;
      old->next->prev = node;
      /* Remove the old node from the list.  */
      old->prev = NULL;
    }

}

/* Relink ssa_imm_use node LINKNODE into the chain for OLD, with use occuring 
   in STMT.  */
static inline void
relink_imm_use_stmt (ssa_imm_use_t *linknode, ssa_imm_use_t *old, tree stmt)
{
  if (stmt)
    relink_imm_use (linknode, old);
  else
    link_imm_use (linknode, NULL);
  linknode->stmt = stmt;
}

/* Finished the traverse of an immediate use list IMM by removing it from 
   the list.  */
static inline void
end_safe_imm_use_traverse (imm_use_iterator *imm)
{
 delink_imm_use (&(imm->iter_node));
}

/* Return true if IMM is at the end of the list.  */
static inline bool
end_safe_imm_use_p (imm_use_iterator *imm)
{
  return (imm->imm_use == imm->end_p);
}

/* Initialize iterator IMM to process the list for VAR.  */
static inline use_operand_p
first_safe_imm_use (imm_use_iterator *imm, tree var)
{
  /* Set up and link the iterator node into the linked list for VAR.  */
  imm->iter_node.use = NULL;
  imm->iter_node.stmt = NULL_TREE;
  imm->end_p = &(SSA_NAME_IMM_USE_NODE (var));
  /* Check if there are 0 elements.  */
  if (imm->end_p->next == imm->end_p)
    {
      imm->imm_use = imm->end_p;
      return NULL_USE_OPERAND_P;
    }

  link_imm_use (&(imm->iter_node), var);
  imm->imm_use = imm->iter_node.next;
  return imm->imm_use;
}

/* Bump IMM to then next use in the list.  */
static inline use_operand_p
next_safe_imm_use (imm_use_iterator *imm)
{
  ssa_imm_use_t *ptr;
  use_operand_p old;

  old = imm->imm_use;
  /* If the next node following the iter_node is still the one refered to by
     imm_use, then the list hasnt changed, go to the next node.  */
  if (imm->iter_node.next == imm->imm_use)
    {
      ptr = &(imm->iter_node);
      /* Remove iternode fromn the list.  */
      delink_imm_use (ptr);
      imm->imm_use = imm->imm_use->next;
      if (! end_safe_imm_use_p (imm))
	{
	  /* This isnt the end, link iternode before the next use.  */
	  ptr->prev = imm->imm_use->prev;
	  ptr->next = imm->imm_use;
	  imm->imm_use->prev->next = ptr;
	  imm->imm_use->prev = ptr;
	}
      else
	return old;
    }
  else
    {
      /* If the 'next' value after the iterator isn't the same as it was, then
	 a node has been deleted, so we sinply proceed to the node following 
	 where the iterator is in the list.  */
      imm->imm_use = imm->iter_node.next;
      if (end_safe_imm_use_p (imm))
        {
	  end_safe_imm_use_traverse (imm);
	  return old;
	}
    }

  return imm->imm_use;
}

/* Return true is IMM has reached the end of the immeidate use list.  */
static inline bool
end_readonly_imm_use_p (imm_use_iterator *imm)
{
  return (imm->imm_use == imm->end_p);
}

/* Initialize iterator IMM to process the list for VAR.  */
static inline use_operand_p
first_readonly_imm_use (imm_use_iterator *imm, tree var)
{
  gcc_assert (TREE_CODE (var) == SSA_NAME);

  imm->end_p = &(SSA_NAME_IMM_USE_NODE (var));
  imm->imm_use = imm->end_p->next;
#ifdef ENABLE_CHECKING
  imm->iter_node.next = imm->imm_use->next;
#endif
  if (end_readonly_imm_use_p (imm))
    return NULL_USE_OPERAND_P;
  return imm->imm_use;
}

/* Bump IMM to then next use in the list.  */
static inline use_operand_p
next_readonly_imm_use (imm_use_iterator *imm)
{
  use_operand_p old = imm->imm_use;

#ifdef ENABLE_CHECKING
  /* If this assertion fails, it indicates the 'next' pointer has changed 
     since we the last bump.  This indicates that the list is being modified
     via stmt changes, or SET_USE, or somesuch thing, and you need to be
     using the SAFE version of the iterator.  */
  gcc_assert (imm->iter_node.next == old->next);
  imm->iter_node.next = old->next->next;
#endif

  imm->imm_use = old->next;
  if (end_readonly_imm_use_p (imm))
    return old;
  return imm->imm_use;
}

/* Return true if VAR has no uses.  */
static inline bool
has_zero_uses (tree var)
{
  ssa_imm_use_t *ptr;
  ptr = &(SSA_NAME_IMM_USE_NODE (var));
  /* A single use means there is no items in the list.  */
  return (ptr == ptr->next);
}

/* Return true if VAR has a single use.  */
static inline bool
has_single_use (tree var)
{
  ssa_imm_use_t *ptr;
  ptr = &(SSA_NAME_IMM_USE_NODE (var));
  /* A single use means there is one item in the list.  */
  return (ptr != ptr->next && ptr == ptr->next->next);
}

/* If VAR has only a single immediate use, return true, and set USE_P and STMT
   to the use pointer and stmt of occurence.  */
static inline bool
single_imm_use (tree var, use_operand_p *use_p, tree *stmt)
{
  ssa_imm_use_t *ptr;

  ptr = &(SSA_NAME_IMM_USE_NODE (var));
  if (ptr != ptr->next && ptr == ptr->next->next)
    {
      *use_p = ptr->next;
      *stmt = ptr->next->stmt;
      return true;
    }
  *use_p = NULL_USE_OPERAND_P;
  *stmt = NULL_TREE;
  return false;
}

/* Return the number of immediate uses of VAR.  */
static inline unsigned int
num_imm_uses (tree var)
{
  ssa_imm_use_t *ptr, *start;
  unsigned int num;

  start = &(SSA_NAME_IMM_USE_NODE (var));
  num = 0;
  for (ptr = start->next; ptr != start; ptr = ptr->next)
     num++;

  return num;
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
  return *(use->use);
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
  return &(uses->uses[index]);
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
  gcc_assert (index < v_may_defs->num_v_may_defs);
  return &(v_may_defs->v_may_defs[index].imm_use);
}

/* Return a use_operand_p that is at INDEX in the VUSES array.  */
static inline use_operand_p
get_vuse_op_ptr(vuse_optype vuses, unsigned int index)
{
  gcc_assert (index < vuses->num_vuses);
  return &(vuses->vuses[index].imm_use);
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
  gcc_assert (index < v_must_defs->num_v_must_defs);
  return &(v_must_defs->v_must_defs[index].imm_use);
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
  return &(PHI_ARG_IMM_USE_NODE (phi,i));
}

/* Delink all immediate_use information for STMT.  */
static inline void
delink_stmt_imm_use (tree stmt)
{
   unsigned int x;
   use_optype uses = STMT_USE_OPS (stmt);
   vuse_optype vuses = STMT_VUSE_OPS (stmt);
   v_may_def_optype v_may_defs = STMT_V_MAY_DEF_OPS (stmt);
   v_must_def_optype v_must_defs = STMT_V_MUST_DEF_OPS (stmt);

   for (x = 0; x < NUM_USES (uses); x++)
     delink_imm_use (&(uses->uses[x]));

   for (x = 0; x < NUM_VUSES (vuses); x++)
     delink_imm_use (&(vuses->vuses[x].imm_use));

   for (x = 0; x < NUM_V_MAY_DEFS (v_may_defs); x++)
     delink_imm_use (&(v_may_defs->v_may_defs[x].imm_use));

   for (x = 0; x < NUM_V_MUST_DEFS (v_must_defs); x++)
     delink_imm_use (&(v_must_defs->v_must_defs[x].imm_use));
}


/* Return the bitmap of addresses taken by STMT, or NULL if it takes
   no addresses.  */
static inline bitmap
addresses_taken (tree stmt)
{
  stmt_ann_t ann = stmt_ann (stmt);
  return ann ? ann->addresses_taken : NULL;
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

/* Return the phi argument which contains the specified use.  */

static inline int
phi_arg_index_from_use (use_operand_p use)
{
  struct phi_arg_d *element, *root;
  int index;
  tree phi;

  /* Since the use is the first thing in a PHI arguemnt element, we can
     calculate its index based on casting it to an argument, and performing
     pointer arithmetic.  */

  phi = USE_STMT (use);
  gcc_assert (TREE_CODE (phi) == PHI_NODE);

  element = (struct phi_arg_d *)use;
  root = &(PHI_ARG_ELT (phi, 0));
  index = element - root;

#ifdef ENABLE_CHECKING
  /* Make sure the calculation doesn't have any leftover bytes.  If it does, 
     then imm_use is liekly not the first element in phi_arg_d.  */
  gcc_assert (
	  (((char *)element - (char *)root) % sizeof (struct phi_arg_d)) == 0);
  gcc_assert (index >= 0 && index < PHI_ARG_CAPACITY (phi));
#endif
 
 return index;
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
  if (ann->mem_tag_kind != NOT_A_TAG && ann->mem_tag_kind != STRUCT_FIELD)
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
  if (ann->mem_tag_kind != NOT_A_TAG && ann->mem_tag_kind != STRUCT_FIELD)
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

/* Return true if REF, a COMPONENT_REF, has an ARRAY_REF somewhere in it.  */

static inline bool
ref_contains_array_ref (tree ref)
{
  while (handled_component_p (ref))
    {
      if (TREE_CODE (ref) == ARRAY_REF)
	return true;
      ref = TREE_OPERAND (ref, 0);
    }
  return false;
}

/* Given a variable VAR, lookup and return a pointer to the list of
   subvariables for it.  */

static inline subvar_t *
lookup_subvars_for_var (tree var)
{
  var_ann_t ann = var_ann (var);
  gcc_assert (ann);
  return &ann->subvars;
}

/* Given a variable VAR, return a linked list of subvariables for VAR, or
   NULL, if there are no subvariables.  */

static inline subvar_t
get_subvars_for_var (tree var)
{
  subvar_t subvars;

  gcc_assert (SSA_VAR_P (var));  
  
  if (TREE_CODE (var) == SSA_NAME)
    subvars = *(lookup_subvars_for_var (SSA_NAME_VAR (var)));
  else
    subvars = *(lookup_subvars_for_var (var));
  return subvars;
}

/* Return true if V is a tree that we can have subvars for.
   Normally, this is any aggregate type, however, due to implementation
   limitations ATM, we exclude array types as well.  */

static inline bool
var_can_have_subvars (tree v)
{
  return (AGGREGATE_TYPE_P (TREE_TYPE (v)) &&
	  TREE_CODE (TREE_TYPE (v)) != ARRAY_TYPE);
}

  
/* Return true if OFFSET and SIZE define a range that overlaps with some
   portion of the range of SV, a subvar.  If there was an exact overlap,
   *EXACT will be set to true upon return. */

static inline bool
overlap_subvar (HOST_WIDE_INT offset, HOST_WIDE_INT size,
		subvar_t sv,  bool *exact)
{
  /* There are three possible cases of overlap.
     1. We can have an exact overlap, like so:   
     |offset, offset + size             |
     |sv->offset, sv->offset + sv->size |
     
     2. We can have offset starting after sv->offset, like so:
     
           |offset, offset + size              |
     |sv->offset, sv->offset + sv->size  |

     3. We can have offset starting before sv->offset, like so:
     
     |offset, offset + size    |
       |sv->offset, sv->offset + sv->size|
  */

  if (exact)
    *exact = false;
  if (offset == sv->offset && size == sv->size)
    {
      if (exact)
	*exact = true;
      return true;
    }
  else if (offset >= sv->offset && offset < (sv->offset + sv->size))
    {
      return true;
    }
  else if (offset < sv->offset && (offset + size > sv->offset))
    {
      return true;
    }
  return false;

}

#endif /* _TREE_FLOW_INLINE_H  */
