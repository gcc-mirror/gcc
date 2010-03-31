/* Loop autoparallelization.
   Copyright (C) 2006, 2007, 2008, 2009 Free Software Foundation, Inc.
   Contributed by Sebastian Pop <pop@cri.ensmp.fr> and
   Zdenek Dvorak <dvorakz@suse.cz>.

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
#include "rtl.h"
#include "tree-flow.h"
#include "cfgloop.h"
#include "ggc.h"
#include "tree-data-ref.h"
#include "diagnostic.h"
#include "tree-pass.h"
#include "tree-scalar-evolution.h"
#include "hashtab.h"
#include "langhooks.h"
#include "tree-vectorizer.h"

/* This pass tries to distribute iterations of loops into several threads.
   The implementation is straightforward -- for each loop we test whether its
   iterations are independent, and if it is the case (and some additional
   conditions regarding profitability and correctness are satisfied), we
   add GIMPLE_OMP_PARALLEL and GIMPLE_OMP_FOR codes and let omp expansion
   machinery do its job.

   The most of the complexity is in bringing the code into shape expected
   by the omp expanders:
   -- for GIMPLE_OMP_FOR, ensuring that the loop has only one induction
      variable and that the exit test is at the start of the loop body
   -- for GIMPLE_OMP_PARALLEL, replacing the references to local addressable
      variables by accesses through pointers, and breaking up ssa chains
      by storing the values incoming to the parallelized loop to a structure
      passed to the new function as an argument (something similar is done
      in omp gimplification, unfortunately only a small part of the code
      can be shared).

   TODO:
   -- if there are several parallelizable loops in a function, it may be
      possible to generate the threads just once (using synchronization to
      ensure that cross-loop dependences are obeyed).
   -- handling of common scalar dependence patterns (accumulation, ...)
   -- handling of non-innermost loops  */

/*
  Reduction handling:
  currently we use vect_is_simple_reduction() to detect reduction patterns.
  The code transformation will be introduced by an example.


parloop
{
  int sum=1;

  for (i = 0; i < N; i++)
   {
    x[i] = i + 3;
    sum+=x[i];
   }
}

gimple-like code:
header_bb:

  # sum_29 = PHI <sum_11(5), 1(3)>
  # i_28 = PHI <i_12(5), 0(3)>
  D.1795_8 = i_28 + 3;
  x[i_28] = D.1795_8;
  sum_11 = D.1795_8 + sum_29;
  i_12 = i_28 + 1;
  if (N_6(D) > i_12)
    goto header_bb;


exit_bb:

  # sum_21 = PHI <sum_11(4)>
  printf (&"%d"[0], sum_21);


after reduction transformation (only relevant parts):

parloop
{

....


  # Storing the initial value given by the user.  #

  .paral_data_store.32.sum.27 = 1;

  #pragma omp parallel num_threads(4)

  #pragma omp for schedule(static)

  # The neutral element corresponding to the particular
  reduction's operation, e.g. 0 for PLUS_EXPR,
  1 for MULT_EXPR, etc. replaces the user's initial value.  #

  # sum.27_29 = PHI <sum.27_11, 0>

  sum.27_11 = D.1827_8 + sum.27_29;

  GIMPLE_OMP_CONTINUE

  # Adding this reduction phi is done at create_phi_for_local_result() #
  # sum.27_56 = PHI <sum.27_11, 0>
  GIMPLE_OMP_RETURN

  # Creating the atomic operation is done at
  create_call_for_reduction_1()  #

  #pragma omp atomic_load
  D.1839_59 = *&.paral_data_load.33_51->reduction.23;
  D.1840_60 = sum.27_56 + D.1839_59;
  #pragma omp atomic_store (D.1840_60);

  GIMPLE_OMP_RETURN

 # collecting the result after the join of the threads is done at
  create_loads_for_reductions().
  The value computed by the threads is loaded from the
  shared struct.  #


  .paral_data_load.33_52 = &.paral_data_store.32;
  sum_37 =  .paral_data_load.33_52->sum.27;
  sum_43 = D.1795_41 + sum_37;

  exit bb:
  # sum_21 = PHI <sum_43, sum_26>
  printf (&"%d"[0], sum_21);

...

}

*/

/* Minimal number of iterations of a loop that should be executed in each
   thread.  */
#define MIN_PER_THREAD 100

/* Element of the hashtable, representing a
   reduction in the current loop.  */
struct reduction_info
{
  gimple reduc_stmt;		/* reduction statement.  */
  gimple reduc_phi;		/* The phi node defining the reduction.  */
  enum tree_code reduction_code;/* code for the reduction operation.  */
  gimple keep_res;		/* The PHI_RESULT of this phi is the resulting value
				   of the reduction variable when existing the loop. */
  tree initial_value;		/* The initial value of the reduction var before entering the loop.  */
  tree field;			/*  the name of the field in the parloop data structure intended for reduction.  */
  tree init;			/* reduction initialization value.  */
  gimple new_phi;		/* (helper field) Newly created phi node whose result
				   will be passed to the atomic operation.  Represents
				   the local result each thread computed for the reduction
				   operation.  */
};

/* Equality and hash functions for hashtab code.  */

static int
reduction_info_eq (const void *aa, const void *bb)
{
  const struct reduction_info *a = (const struct reduction_info *) aa;
  const struct reduction_info *b = (const struct reduction_info *) bb;

  return (a->reduc_phi == b->reduc_phi);
}

static hashval_t
reduction_info_hash (const void *aa)
{
  const struct reduction_info *a = (const struct reduction_info *) aa;

  return htab_hash_pointer (a->reduc_phi);
}

static struct reduction_info *
reduction_phi (htab_t reduction_list, gimple phi)
{
  struct reduction_info tmpred, *red;

  if (htab_elements (reduction_list) == 0)
    return NULL;

  tmpred.reduc_phi = phi;
  red = (struct reduction_info *) htab_find (reduction_list, &tmpred);

  return red;
}

/* Element of hashtable of names to copy.  */

struct name_to_copy_elt
{
  unsigned version;	/* The version of the name to copy.  */
  tree new_name;	/* The new name used in the copy.  */
  tree field;		/* The field of the structure used to pass the
			   value.  */
};

/* Equality and hash functions for hashtab code.  */

static int
name_to_copy_elt_eq (const void *aa, const void *bb)
{
  const struct name_to_copy_elt *a = (const struct name_to_copy_elt *) aa;
  const struct name_to_copy_elt *b = (const struct name_to_copy_elt *) bb;

  return a->version == b->version;
}

static hashval_t
name_to_copy_elt_hash (const void *aa)
{
  const struct name_to_copy_elt *a = (const struct name_to_copy_elt *) aa;

  return (hashval_t) a->version;
}


/* Data dependency analysis. Returns true if the iterations of LOOP
   are independent on each other (that is, if we can execute them
   in parallel).  */

static bool
loop_parallel_p (struct loop *loop)
{
  VEC (ddr_p, heap) * dependence_relations;
  VEC (data_reference_p, heap) *datarefs;
  lambda_trans_matrix trans;
  bool ret = false;

  if (dump_file && (dump_flags & TDF_DETAILS))
  {
    fprintf (dump_file, "Considering loop %d\n", loop->num);
    if (!loop->inner)
      fprintf (dump_file, "loop is innermost\n");
    else
      fprintf (dump_file, "loop NOT innermost\n");
   }

  /* Check for problems with dependences.  If the loop can be reversed,
     the iterations are independent.  */
  datarefs = VEC_alloc (data_reference_p, heap, 10);
  dependence_relations = VEC_alloc (ddr_p, heap, 10 * 10);
  compute_data_dependences_for_loop (loop, true, &datarefs,
				     &dependence_relations);
  if (dump_file && (dump_flags & TDF_DETAILS))
    dump_data_dependence_relations (dump_file, dependence_relations);

  trans = lambda_trans_matrix_new (1, 1);
  LTM_MATRIX (trans)[0][0] = -1;

  if (lambda_transform_legal_p (trans, 1, dependence_relations))
    {
      ret = true;
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "  SUCCESS: may be parallelized\n");
    }
  else if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file,
	     "  FAILED: data dependencies exist across iterations\n");

  free_dependence_relations (dependence_relations);
  free_data_refs (datarefs);

  return ret;
}

/* Return true when LOOP contains basic blocks marked with the
   BB_IRREDUCIBLE_LOOP flag.  */

static inline bool
loop_has_blocks_with_irreducible_flag (struct loop *loop)
{
  unsigned i;
  basic_block *bbs = get_loop_body_in_dom_order (loop);
  bool res = true;

  for (i = 0; i < loop->num_nodes; i++)
    if (bbs[i]->flags & BB_IRREDUCIBLE_LOOP)
      goto end;

  res = false;
 end:
  free (bbs);
  return res;
}

/* Assigns the address of OBJ in TYPE to an ssa name, and returns this name.
   The assignment statement is placed on edge ENTRY.  DECL_ADDRESS maps decls
   to their addresses that can be reused.  The address of OBJ is known to
   be invariant in the whole function.  */

static tree
take_address_of (tree obj, tree type, edge entry, htab_t decl_address)
{
  int uid;
  void **dslot;
  struct int_tree_map ielt, *nielt;
  tree *var_p, name, bvar, addr;
  gimple stmt;
  gimple_seq stmts;

  /* Since the address of OBJ is invariant, the trees may be shared.
     Avoid rewriting unrelated parts of the code.  */
  obj = unshare_expr (obj);
  for (var_p = &obj;
       handled_component_p (*var_p);
       var_p = &TREE_OPERAND (*var_p, 0))
    continue;
  uid = DECL_UID (*var_p);

  ielt.uid = uid;
  dslot = htab_find_slot_with_hash (decl_address, &ielt, uid, INSERT);
  if (!*dslot)
    {
      addr = build_addr (*var_p, current_function_decl);
      bvar = create_tmp_var (TREE_TYPE (addr), get_name (*var_p));
      add_referenced_var (bvar);
      stmt = gimple_build_assign (bvar, addr);
      name = make_ssa_name (bvar, stmt);
      gimple_assign_set_lhs (stmt, name);
      gsi_insert_on_edge_immediate (entry, stmt);

      nielt = XNEW (struct int_tree_map);
      nielt->uid = uid;
      nielt->to = name;
      *dslot = nielt;
    }
  else
    name = ((struct int_tree_map *) *dslot)->to;

  if (var_p != &obj)
    {
      *var_p = build1 (INDIRECT_REF, TREE_TYPE (*var_p), name);
      name = force_gimple_operand (build_addr (obj, current_function_decl),
				   &stmts, true, NULL_TREE);
      if (!gimple_seq_empty_p (stmts))
	gsi_insert_seq_on_edge_immediate (entry, stmts);
    }

  if (TREE_TYPE (name) != type)
    {
      name = force_gimple_operand (fold_convert (type, name), &stmts, true,
				   NULL_TREE);
      if (!gimple_seq_empty_p (stmts))
	gsi_insert_seq_on_edge_immediate (entry, stmts);
    }

  return name;
}

/* Callback for htab_traverse.  Create the initialization statement
   for reduction described in SLOT, and place it at the preheader of
   the loop described in DATA.  */

static int
initialize_reductions (void **slot, void *data)
{
  tree init, c;
  tree bvar, type, arg;
  edge e;

  struct reduction_info *const reduc = (struct reduction_info *) *slot;
  struct loop *loop = (struct loop *) data;

  /* Create initialization in preheader:
     reduction_variable = initialization value of reduction.  */

  /* In the phi node at the header, replace the argument coming
     from the preheader with the reduction initialization value.  */

  /* Create a new variable to initialize the reduction.  */
  type = TREE_TYPE (PHI_RESULT (reduc->reduc_phi));
  bvar = create_tmp_var (type, "reduction");
  add_referenced_var (bvar);

  c = build_omp_clause (gimple_location (reduc->reduc_stmt),
			OMP_CLAUSE_REDUCTION);
  OMP_CLAUSE_REDUCTION_CODE (c) = reduc->reduction_code;
  OMP_CLAUSE_DECL (c) = SSA_NAME_VAR (gimple_assign_lhs (reduc->reduc_stmt));

  init = omp_reduction_init (c, TREE_TYPE (bvar));
  reduc->init = init;

  /* Replace the argument representing the initialization value
     with the initialization value for the reduction (neutral
     element for the particular operation, e.g. 0 for PLUS_EXPR,
     1 for MULT_EXPR, etc).
     Keep the old value in a new variable "reduction_initial",
     that will be taken in consideration after the parallel
     computing is done.  */

  e = loop_preheader_edge (loop);
  arg = PHI_ARG_DEF_FROM_EDGE (reduc->reduc_phi, e);
  /* Create new variable to hold the initial value.  */

  SET_USE (PHI_ARG_DEF_PTR_FROM_EDGE
	   (reduc->reduc_phi, loop_preheader_edge (loop)), init);
  reduc->initial_value = arg;
  return 1;
}

struct elv_data
{
  struct walk_stmt_info info;
  edge entry;
  htab_t decl_address;
  bool changed;
};

/* Eliminates references to local variables in *TP out of the single
   entry single exit region starting at DTA->ENTRY.
   DECL_ADDRESS contains addresses of the references that had their
   address taken already.  If the expression is changed, CHANGED is
   set to true.  Callback for walk_tree.  */

static tree
eliminate_local_variables_1 (tree *tp, int *walk_subtrees, void *data)
{
  struct elv_data *const dta = (struct elv_data *) data;
  tree t = *tp, var, addr, addr_type, type, obj;

  if (DECL_P (t))
    {
      *walk_subtrees = 0;

      if (!SSA_VAR_P (t) || DECL_EXTERNAL (t))
	return NULL_TREE;

      type = TREE_TYPE (t);
      addr_type = build_pointer_type (type);
      addr = take_address_of (t, addr_type, dta->entry, dta->decl_address);
      *tp = build1 (INDIRECT_REF, TREE_TYPE (*tp), addr);

      dta->changed = true;
      return NULL_TREE;
    }

  if (TREE_CODE (t) == ADDR_EXPR)
    {
      /* ADDR_EXPR may appear in two contexts:
	 -- as a gimple operand, when the address taken is a function invariant
	 -- as gimple rhs, when the resulting address in not a function
	    invariant
	 We do not need to do anything special in the latter case (the base of
	 the memory reference whose address is taken may be replaced in the
	 DECL_P case).  The former case is more complicated, as we need to
	 ensure that the new address is still a gimple operand.  Thus, it
	 is not sufficient to replace just the base of the memory reference --
	 we need to move the whole computation of the address out of the
	 loop.  */
      if (!is_gimple_val (t))
	return NULL_TREE;

      *walk_subtrees = 0;
      obj = TREE_OPERAND (t, 0);
      var = get_base_address (obj);
      if (!var || !SSA_VAR_P (var) || DECL_EXTERNAL (var))
	return NULL_TREE;

      addr_type = TREE_TYPE (t);
      addr = take_address_of (obj, addr_type, dta->entry, dta->decl_address);
      *tp = addr;

      dta->changed = true;
      return NULL_TREE;
    }

  if (!EXPR_P (t))
    *walk_subtrees = 0;

  return NULL_TREE;
}

/* Moves the references to local variables in STMT out of the single
   entry single exit region starting at ENTRY.  DECL_ADDRESS contains
   addresses of the references that had their address taken
   already.  */

static void
eliminate_local_variables_stmt (edge entry, gimple stmt,
				htab_t decl_address)
{
  struct elv_data dta;

  memset (&dta.info, '\0', sizeof (dta.info));
  dta.entry = entry;
  dta.decl_address = decl_address;
  dta.changed = false;

  if (gimple_debug_bind_p (stmt))
    walk_tree (gimple_debug_bind_get_value_ptr (stmt),
	       eliminate_local_variables_1, &dta.info, NULL);
  else
    walk_gimple_op (stmt, eliminate_local_variables_1, &dta.info);

  if (dta.changed)
    update_stmt (stmt);
}

/* Eliminates the references to local variables from the single entry
   single exit region between the ENTRY and EXIT edges.

   This includes:
   1) Taking address of a local variable -- these are moved out of the
   region (and temporary variable is created to hold the address if
   necessary).

   2) Dereferencing a local variable -- these are replaced with indirect
   references.  */

static void
eliminate_local_variables (edge entry, edge exit)
{
  basic_block bb;
  VEC (basic_block, heap) *body = VEC_alloc (basic_block, heap, 3);
  unsigned i;
  gimple_stmt_iterator gsi;
  htab_t decl_address = htab_create (10, int_tree_map_hash, int_tree_map_eq,
				     free);
  basic_block entry_bb = entry->src;
  basic_block exit_bb = exit->dest;

  gather_blocks_in_sese_region (entry_bb, exit_bb, &body);

  for (i = 0; VEC_iterate (basic_block, body, i, bb); i++)
    if (bb != entry_bb && bb != exit_bb)
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	eliminate_local_variables_stmt (entry, gsi_stmt (gsi),
					decl_address);

  htab_delete (decl_address);
  VEC_free (basic_block, heap, body);
}

/* Returns true if expression EXPR is not defined between ENTRY and
   EXIT, i.e. if all its operands are defined outside of the region.  */

static bool
expr_invariant_in_region_p (edge entry, edge exit, tree expr)
{
  basic_block entry_bb = entry->src;
  basic_block exit_bb = exit->dest;
  basic_block def_bb;

  if (is_gimple_min_invariant (expr))
    return true;

  if (TREE_CODE (expr) == SSA_NAME)
    {
      def_bb = gimple_bb (SSA_NAME_DEF_STMT (expr));
      if (def_bb
	  && dominated_by_p (CDI_DOMINATORS, def_bb, entry_bb)
	  && !dominated_by_p (CDI_DOMINATORS, def_bb, exit_bb))
	return false;

      return true;
    }

  return false;
}

/* If COPY_NAME_P is true, creates and returns a duplicate of NAME.
   The copies are stored to NAME_COPIES, if NAME was already duplicated,
   its duplicate stored in NAME_COPIES is returned.

   Regardless of COPY_NAME_P, the decl used as a base of the ssa name is also
   duplicated, storing the copies in DECL_COPIES.  */

static tree
separate_decls_in_region_name (tree name,
			       htab_t name_copies, htab_t decl_copies,
			       bool copy_name_p)
{
  tree copy, var, var_copy;
  unsigned idx, uid, nuid;
  struct int_tree_map ielt, *nielt;
  struct name_to_copy_elt elt, *nelt;
  void **slot, **dslot;

  if (TREE_CODE (name) != SSA_NAME)
    return name;

  idx = SSA_NAME_VERSION (name);
  elt.version = idx;
  slot = htab_find_slot_with_hash (name_copies, &elt, idx,
				   copy_name_p ? INSERT : NO_INSERT);
  if (slot && *slot)
    return ((struct name_to_copy_elt *) *slot)->new_name;

  var = SSA_NAME_VAR (name);
  uid = DECL_UID (var);
  ielt.uid = uid;
  dslot = htab_find_slot_with_hash (decl_copies, &ielt, uid, INSERT);
  if (!*dslot)
    {
      var_copy = create_tmp_var (TREE_TYPE (var), get_name (var));
      DECL_GIMPLE_REG_P (var_copy) = DECL_GIMPLE_REG_P (var);
      add_referenced_var (var_copy);
      nielt = XNEW (struct int_tree_map);
      nielt->uid = uid;
      nielt->to = var_copy;
      *dslot = nielt;

      /* Ensure that when we meet this decl next time, we won't duplicate
         it again.  */
      nuid = DECL_UID (var_copy);
      ielt.uid = nuid;
      dslot = htab_find_slot_with_hash (decl_copies, &ielt, nuid, INSERT);
      gcc_assert (!*dslot);
      nielt = XNEW (struct int_tree_map);
      nielt->uid = nuid;
      nielt->to = var_copy;
      *dslot = nielt;
    }
  else
    var_copy = ((struct int_tree_map *) *dslot)->to;

  if (copy_name_p)
    {
      copy = duplicate_ssa_name (name, NULL);
      nelt = XNEW (struct name_to_copy_elt);
      nelt->version = idx;
      nelt->new_name = copy;
      nelt->field = NULL_TREE;
      *slot = nelt;
    }
  else
    {
      gcc_assert (!slot);
      copy = name;
    }

  SSA_NAME_VAR (copy) = var_copy;
  return copy;
}

/* Finds the ssa names used in STMT that are defined outside the
   region between ENTRY and EXIT and replaces such ssa names with
   their duplicates.  The duplicates are stored to NAME_COPIES.  Base
   decls of all ssa names used in STMT (including those defined in
   LOOP) are replaced with the new temporary variables; the
   replacement decls are stored in DECL_COPIES.  */

static void
separate_decls_in_region_stmt (edge entry, edge exit, gimple stmt,
			       htab_t name_copies, htab_t decl_copies)
{
  use_operand_p use;
  def_operand_p def;
  ssa_op_iter oi;
  tree name, copy;
  bool copy_name_p;

  mark_virtual_ops_for_renaming (stmt);

  FOR_EACH_PHI_OR_STMT_DEF (def, stmt, oi, SSA_OP_DEF)
  {
    name = DEF_FROM_PTR (def);
    gcc_assert (TREE_CODE (name) == SSA_NAME);
    copy = separate_decls_in_region_name (name, name_copies, decl_copies,
					  false);
    gcc_assert (copy == name);
  }

  FOR_EACH_PHI_OR_STMT_USE (use, stmt, oi, SSA_OP_USE)
  {
    name = USE_FROM_PTR (use);
    if (TREE_CODE (name) != SSA_NAME)
      continue;

    copy_name_p = expr_invariant_in_region_p (entry, exit, name);
    copy = separate_decls_in_region_name (name, name_copies, decl_copies,
					  copy_name_p);
    SET_USE (use, copy);
  }
}

/* Finds the ssa names used in STMT that are defined outside the
   region between ENTRY and EXIT and replaces such ssa names with
   their duplicates.  The duplicates are stored to NAME_COPIES.  Base
   decls of all ssa names used in STMT (including those defined in
   LOOP) are replaced with the new temporary variables; the
   replacement decls are stored in DECL_COPIES.  */

static bool
separate_decls_in_region_debug_bind (gimple stmt,
				     htab_t name_copies, htab_t decl_copies)
{
  use_operand_p use;
  ssa_op_iter oi;
  tree var, name;
  struct int_tree_map ielt;
  struct name_to_copy_elt elt;
  void **slot, **dslot;

  var = gimple_debug_bind_get_var (stmt);
  if (TREE_CODE (var) == DEBUG_EXPR_DECL)
    return true;
  gcc_assert (DECL_P (var) && SSA_VAR_P (var));
  ielt.uid = DECL_UID (var);
  dslot = htab_find_slot_with_hash (decl_copies, &ielt, ielt.uid, NO_INSERT);
  if (!dslot)
    return true;
  gimple_debug_bind_set_var (stmt, ((struct int_tree_map *) *dslot)->to);

  FOR_EACH_PHI_OR_STMT_USE (use, stmt, oi, SSA_OP_USE)
  {
    name = USE_FROM_PTR (use);
    if (TREE_CODE (name) != SSA_NAME)
      continue;

    elt.version = SSA_NAME_VERSION (name);
    slot = htab_find_slot_with_hash (name_copies, &elt, elt.version, NO_INSERT);
    if (!slot)
      {
	gimple_debug_bind_reset_value (stmt);
	update_stmt (stmt);
	break;
      }

    SET_USE (use, ((struct name_to_copy_elt *) *slot)->new_name);
  }

  return false;
}

/* Callback for htab_traverse.  Adds a field corresponding to the reduction
   specified in SLOT. The type is passed in DATA.  */

static int
add_field_for_reduction (void **slot, void *data)
{

  struct reduction_info *const red = (struct reduction_info *) *slot;
  tree const type = (tree) data;
  tree var = SSA_NAME_VAR (gimple_assign_lhs (red->reduc_stmt));
  tree field = build_decl (gimple_location (red->reduc_stmt),
			   FIELD_DECL, DECL_NAME (var), TREE_TYPE (var));

  insert_field_into_struct (type, field);

  red->field = field;

  return 1;
}

/* Callback for htab_traverse.  Adds a field corresponding to a ssa name
   described in SLOT. The type is passed in DATA.  */

static int
add_field_for_name (void **slot, void *data)
{
  struct name_to_copy_elt *const elt = (struct name_to_copy_elt *) *slot;
  tree type = (tree) data;
  tree name = ssa_name (elt->version);
  tree var = SSA_NAME_VAR (name);
  tree field = build_decl (DECL_SOURCE_LOCATION (var),
			   FIELD_DECL, DECL_NAME (var), TREE_TYPE (var));

  insert_field_into_struct (type, field);
  elt->field = field;

  return 1;
}

/* Callback for htab_traverse.  A local result is the intermediate result
   computed by a single
   thread, or the initial value in case no iteration was executed.
   This function creates a phi node reflecting these values.
   The phi's result will be stored in NEW_PHI field of the
   reduction's data structure.  */

static int
create_phi_for_local_result (void **slot, void *data)
{
  struct reduction_info *const reduc = (struct reduction_info *) *slot;
  const struct loop *const loop = (const struct loop *) data;
  edge e;
  gimple new_phi;
  basic_block store_bb;
  tree local_res;
  source_location locus;

  /* STORE_BB is the block where the phi
     should be stored.  It is the destination of the loop exit.
     (Find the fallthru edge from GIMPLE_OMP_CONTINUE).  */
  store_bb = FALLTHRU_EDGE (loop->latch)->dest;

  /* STORE_BB has two predecessors.  One coming from  the loop
     (the reduction's result is computed at the loop),
     and another coming from a block preceding the loop,
     when no iterations
     are executed (the initial value should be taken).  */
  if (EDGE_PRED (store_bb, 0) == FALLTHRU_EDGE (loop->latch))
    e = EDGE_PRED (store_bb, 1);
  else
    e = EDGE_PRED (store_bb, 0);
  local_res
    = make_ssa_name (SSA_NAME_VAR (gimple_assign_lhs (reduc->reduc_stmt)),
		     NULL);
  locus = gimple_location (reduc->reduc_stmt);
  new_phi = create_phi_node (local_res, store_bb);
  SSA_NAME_DEF_STMT (local_res) = new_phi;
  add_phi_arg (new_phi, reduc->init, e, locus);
  add_phi_arg (new_phi, gimple_assign_lhs (reduc->reduc_stmt),
	       FALLTHRU_EDGE (loop->latch), locus);
  reduc->new_phi = new_phi;

  return 1;
}

struct clsn_data
{
  tree store;
  tree load;

  basic_block store_bb;
  basic_block load_bb;
};

/* Callback for htab_traverse.  Create an atomic instruction for the
   reduction described in SLOT.
   DATA annotates the place in memory the atomic operation relates to,
   and the basic block it needs to be generated in.  */

static int
create_call_for_reduction_1 (void **slot, void *data)
{
  struct reduction_info *const reduc = (struct reduction_info *) *slot;
  struct clsn_data *const clsn_data = (struct clsn_data *) data;
  gimple_stmt_iterator gsi;
  tree type = TREE_TYPE (PHI_RESULT (reduc->reduc_phi));
  tree struct_type = TREE_TYPE (TREE_TYPE (clsn_data->load));
  tree load_struct;
  basic_block bb;
  basic_block new_bb;
  edge e;
  tree t, addr, ref, x;
  tree tmp_load, name;
  gimple load;

  load_struct = fold_build1 (INDIRECT_REF, struct_type, clsn_data->load);
  t = build3 (COMPONENT_REF, type, load_struct, reduc->field, NULL_TREE);

  addr = build_addr (t, current_function_decl);

  /* Create phi node.  */
  bb = clsn_data->load_bb;

  e = split_block (bb, t);
  new_bb = e->dest;

  tmp_load = create_tmp_var (TREE_TYPE (TREE_TYPE (addr)), NULL);
  add_referenced_var (tmp_load);
  tmp_load = make_ssa_name (tmp_load, NULL);
  load = gimple_build_omp_atomic_load (tmp_load, addr);
  SSA_NAME_DEF_STMT (tmp_load) = load;
  gsi = gsi_start_bb (new_bb);
  gsi_insert_after (&gsi, load, GSI_NEW_STMT);

  e = split_block (new_bb, load);
  new_bb = e->dest;
  gsi = gsi_start_bb (new_bb);
  ref = tmp_load;
  x = fold_build2 (reduc->reduction_code,
		   TREE_TYPE (PHI_RESULT (reduc->new_phi)), ref,
		   PHI_RESULT (reduc->new_phi));

  name = force_gimple_operand_gsi (&gsi, x, true, NULL_TREE, true,
				   GSI_CONTINUE_LINKING);

  gsi_insert_after (&gsi, gimple_build_omp_atomic_store (name), GSI_NEW_STMT);
  return 1;
}

/* Create the atomic operation at the join point of the threads.
   REDUCTION_LIST describes the reductions in the LOOP.
   LD_ST_DATA describes the shared data structure where
   shared data is stored in and loaded from.  */
static void
create_call_for_reduction (struct loop *loop, htab_t reduction_list,
			   struct clsn_data *ld_st_data)
{
  htab_traverse (reduction_list, create_phi_for_local_result, loop);
  /* Find the fallthru edge from GIMPLE_OMP_CONTINUE.  */
  ld_st_data->load_bb = FALLTHRU_EDGE (loop->latch)->dest;
  htab_traverse (reduction_list, create_call_for_reduction_1, ld_st_data);
}

/* Callback for htab_traverse.  Loads the final reduction value at the
   join point of all threads, and inserts it in the right place.  */

static int
create_loads_for_reductions (void **slot, void *data)
{
  struct reduction_info *const red = (struct reduction_info *) *slot;
  struct clsn_data *const clsn_data = (struct clsn_data *) data;
  gimple stmt;
  gimple_stmt_iterator gsi;
  tree type = TREE_TYPE (gimple_assign_lhs (red->reduc_stmt));
  tree struct_type = TREE_TYPE (TREE_TYPE (clsn_data->load));
  tree load_struct;
  tree name;
  tree x;

  gsi = gsi_after_labels (clsn_data->load_bb);
  load_struct = fold_build1 (INDIRECT_REF, struct_type, clsn_data->load);
  load_struct = build3 (COMPONENT_REF, type, load_struct, red->field,
			NULL_TREE);

  x = load_struct;
  name = PHI_RESULT (red->keep_res);
  stmt = gimple_build_assign (name, x);
  SSA_NAME_DEF_STMT (name) = stmt;

  gsi_insert_after (&gsi, stmt, GSI_NEW_STMT);

  for (gsi = gsi_start_phis (gimple_bb (red->keep_res));
       !gsi_end_p (gsi); gsi_next (&gsi))
    if (gsi_stmt (gsi) == red->keep_res)
      {
	remove_phi_node (&gsi, false);
	return 1;
      }
  gcc_unreachable ();
}

/* Load the reduction result that was stored in LD_ST_DATA.
   REDUCTION_LIST describes the list of reductions that the
   loads should be generated for.  */
static void
create_final_loads_for_reduction (htab_t reduction_list,
				  struct clsn_data *ld_st_data)
{
  gimple_stmt_iterator gsi;
  tree t;
  gimple stmt;

  gsi = gsi_after_labels (ld_st_data->load_bb);
  t = build_fold_addr_expr (ld_st_data->store);
  stmt = gimple_build_assign (ld_st_data->load, t);

  gsi_insert_before (&gsi, stmt, GSI_NEW_STMT);
  SSA_NAME_DEF_STMT (ld_st_data->load) = stmt;

  htab_traverse (reduction_list, create_loads_for_reductions, ld_st_data);

}

/* Callback for htab_traverse.  Store the neutral value for the
  particular reduction's operation, e.g. 0 for PLUS_EXPR,
  1 for MULT_EXPR, etc. into the reduction field.
  The reduction is specified in SLOT. The store information is
  passed in DATA.  */

static int
create_stores_for_reduction (void **slot, void *data)
{
  struct reduction_info *const red = (struct reduction_info *) *slot;
  struct clsn_data *const clsn_data = (struct clsn_data *) data;
  tree t;
  gimple stmt;
  gimple_stmt_iterator gsi;
  tree type = TREE_TYPE (gimple_assign_lhs (red->reduc_stmt));

  gsi = gsi_last_bb (clsn_data->store_bb);
  t = build3 (COMPONENT_REF, type, clsn_data->store, red->field, NULL_TREE);
  stmt = gimple_build_assign (t, red->initial_value);
  mark_virtual_ops_for_renaming (stmt);
  gsi_insert_after (&gsi, stmt, GSI_NEW_STMT);

  return 1;
}

/* Callback for htab_traverse.  Creates loads to a field of LOAD in LOAD_BB and
   store to a field of STORE in STORE_BB for the ssa name and its duplicate
   specified in SLOT.  */

static int
create_loads_and_stores_for_name (void **slot, void *data)
{
  struct name_to_copy_elt *const elt = (struct name_to_copy_elt *) *slot;
  struct clsn_data *const clsn_data = (struct clsn_data *) data;
  tree t;
  gimple stmt;
  gimple_stmt_iterator gsi;
  tree type = TREE_TYPE (elt->new_name);
  tree struct_type = TREE_TYPE (TREE_TYPE (clsn_data->load));
  tree load_struct;

  gsi = gsi_last_bb (clsn_data->store_bb);
  t = build3 (COMPONENT_REF, type, clsn_data->store, elt->field, NULL_TREE);
  stmt = gimple_build_assign (t, ssa_name (elt->version));
  mark_virtual_ops_for_renaming (stmt);
  gsi_insert_after (&gsi, stmt, GSI_NEW_STMT);

  gsi = gsi_last_bb (clsn_data->load_bb);
  load_struct = fold_build1 (INDIRECT_REF, struct_type, clsn_data->load);
  t = build3 (COMPONENT_REF, type, load_struct, elt->field, NULL_TREE);
  stmt = gimple_build_assign (elt->new_name, t);
  SSA_NAME_DEF_STMT (elt->new_name) = stmt;
  gsi_insert_after (&gsi, stmt, GSI_NEW_STMT);

  return 1;
}

/* Moves all the variables used in LOOP and defined outside of it (including
   the initial values of loop phi nodes, and *PER_THREAD if it is a ssa
   name) to a structure created for this purpose.  The code

   while (1)
     {
       use (a);
       use (b);
     }

   is transformed this way:

   bb0:
   old.a = a;
   old.b = b;

   bb1:
   a' = new->a;
   b' = new->b;
   while (1)
     {
       use (a');
       use (b');
     }

   `old' is stored to *ARG_STRUCT and `new' is stored to NEW_ARG_STRUCT.  The
   pointer `new' is intentionally not initialized (the loop will be split to a
   separate function later, and `new' will be initialized from its arguments).
   LD_ST_DATA holds information about the shared data structure used to pass
   information among the threads.  It is initialized here, and
   gen_parallel_loop will pass it to create_call_for_reduction that
   needs this information.  REDUCTION_LIST describes the reductions
   in LOOP.  */

static void
separate_decls_in_region (edge entry, edge exit, htab_t reduction_list,
			  tree *arg_struct, tree *new_arg_struct,
			  struct clsn_data *ld_st_data)

{
  basic_block bb1 = split_edge (entry);
  basic_block bb0 = single_pred (bb1);
  htab_t name_copies = htab_create (10, name_to_copy_elt_hash,
				    name_to_copy_elt_eq, free);
  htab_t decl_copies = htab_create (10, int_tree_map_hash, int_tree_map_eq,
				    free);
  unsigned i;
  tree type, type_name, nvar;
  gimple_stmt_iterator gsi;
  struct clsn_data clsn_data;
  VEC (basic_block, heap) *body = VEC_alloc (basic_block, heap, 3);
  basic_block bb;
  basic_block entry_bb = bb1;
  basic_block exit_bb = exit->dest;
  bool has_debug_stmt = false;

  entry = single_succ_edge (entry_bb);
  gather_blocks_in_sese_region (entry_bb, exit_bb, &body);

  for (i = 0; VEC_iterate (basic_block, body, i, bb); i++)
    {
      if (bb != entry_bb && bb != exit_bb)
	{
	  for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	    separate_decls_in_region_stmt (entry, exit, gsi_stmt (gsi),
					   name_copies, decl_copies);

	  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	    {
	      gimple stmt = gsi_stmt (gsi);

	      if (is_gimple_debug (stmt))
		has_debug_stmt = true;
	      else
		separate_decls_in_region_stmt (entry, exit, stmt,
					       name_copies, decl_copies);
	    }
	}
    }

  /* Now process debug bind stmts.  We must not create decls while
     processing debug stmts, so we defer their processing so as to
     make sure we will have debug info for as many variables as
     possible (all of those that were dealt with in the loop above),
     and discard those for which we know there's nothing we can
     do.  */
  if (has_debug_stmt)
    for (i = 0; VEC_iterate (basic_block, body, i, bb); i++)
      if (bb != entry_bb && bb != exit_bb)
	{
	  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi);)
	    {
	      gimple stmt = gsi_stmt (gsi);

	      if (gimple_debug_bind_p (stmt))
		{
		  if (separate_decls_in_region_debug_bind (stmt,
							   name_copies,
							   decl_copies))
		    {
		      gsi_remove (&gsi, true);
		      continue;
		    }
		}

	      gsi_next (&gsi);
	    }
	}

  VEC_free (basic_block, heap, body);

  if (htab_elements (name_copies) == 0 && htab_elements (reduction_list) == 0)
    {
      /* It may happen that there is nothing to copy (if there are only
         loop carried and external variables in the loop).  */
      *arg_struct = NULL;
      *new_arg_struct = NULL;
    }
  else
    {
      /* Create the type for the structure to store the ssa names to.  */
      type = lang_hooks.types.make_type (RECORD_TYPE);
      type_name = build_decl (BUILTINS_LOCATION,
			      TYPE_DECL, create_tmp_var_name (".paral_data"),
			      type);
      TYPE_NAME (type) = type_name;

      htab_traverse (name_copies, add_field_for_name, type);
      if (reduction_list && htab_elements (reduction_list) > 0)
	{
	  /* Create the fields for reductions.  */
	  htab_traverse (reduction_list, add_field_for_reduction,
                         type);
	}
      layout_type (type);

      /* Create the loads and stores.  */
      *arg_struct = create_tmp_var (type, ".paral_data_store");
      add_referenced_var (*arg_struct);
      nvar = create_tmp_var (build_pointer_type (type), ".paral_data_load");
      add_referenced_var (nvar);
      *new_arg_struct = make_ssa_name (nvar, NULL);

      ld_st_data->store = *arg_struct;
      ld_st_data->load = *new_arg_struct;
      ld_st_data->store_bb = bb0;
      ld_st_data->load_bb = bb1;

      htab_traverse (name_copies, create_loads_and_stores_for_name,
		     ld_st_data);

      /* Load the calculation from memory (after the join of the threads).  */

      if (reduction_list && htab_elements (reduction_list) > 0)
	{
	  htab_traverse (reduction_list, create_stores_for_reduction,
                        ld_st_data);
	  clsn_data.load = make_ssa_name (nvar, NULL);
	  clsn_data.load_bb = exit->dest;
	  clsn_data.store = ld_st_data->store;
	  create_final_loads_for_reduction (reduction_list, &clsn_data);
	}
    }

  htab_delete (decl_copies);
  htab_delete (name_copies);
}

/* Bitmap containing uids of functions created by parallelization.  We cannot
   allocate it from the default obstack, as it must live across compilation
   of several functions; we make it gc allocated instead.  */

static GTY(()) bitmap parallelized_functions;

/* Returns true if FN was created by create_loop_fn.  */

static bool
parallelized_function_p (tree fn)
{
  if (!parallelized_functions || !DECL_ARTIFICIAL (fn))
    return false;

  return bitmap_bit_p (parallelized_functions, DECL_UID (fn));
}

/* Creates and returns an empty function that will receive the body of
   a parallelized loop.  */

static tree
create_loop_fn (void)
{
  char buf[100];
  char *tname;
  tree decl, type, name, t;
  struct function *act_cfun = cfun;
  static unsigned loopfn_num;

  snprintf (buf, 100, "%s.$loopfn", current_function_name ());
  ASM_FORMAT_PRIVATE_NAME (tname, buf, loopfn_num++);
  clean_symbol_name (tname);
  name = get_identifier (tname);
  type = build_function_type_list (void_type_node, ptr_type_node, NULL_TREE);

  decl = build_decl (BUILTINS_LOCATION,
		     FUNCTION_DECL, name, type);
  if (!parallelized_functions)
    parallelized_functions = BITMAP_GGC_ALLOC ();
  bitmap_set_bit (parallelized_functions, DECL_UID (decl));

  TREE_STATIC (decl) = 1;
  TREE_USED (decl) = 1;
  DECL_ARTIFICIAL (decl) = 1;
  DECL_IGNORED_P (decl) = 0;
  TREE_PUBLIC (decl) = 0;
  DECL_UNINLINABLE (decl) = 1;
  DECL_EXTERNAL (decl) = 0;
  DECL_CONTEXT (decl) = NULL_TREE;
  DECL_INITIAL (decl) = make_node (BLOCK);

  t = build_decl (BUILTINS_LOCATION,
		  RESULT_DECL, NULL_TREE, void_type_node);
  DECL_ARTIFICIAL (t) = 1;
  DECL_IGNORED_P (t) = 1;
  DECL_RESULT (decl) = t;

  t = build_decl (BUILTINS_LOCATION,
		  PARM_DECL, get_identifier (".paral_data_param"),
		  ptr_type_node);
  DECL_ARTIFICIAL (t) = 1;
  DECL_ARG_TYPE (t) = ptr_type_node;
  DECL_CONTEXT (t) = decl;
  TREE_USED (t) = 1;
  DECL_ARGUMENTS (decl) = t;

  allocate_struct_function (decl, false);

  /* The call to allocate_struct_function clobbers CFUN, so we need to restore
     it.  */
  set_cfun (act_cfun);

  return decl;
}

/* Moves the exit condition of LOOP to the beginning of its header, and
   duplicates the part of the last iteration that gets disabled to the
   exit of the loop.  NIT is the number of iterations of the loop
   (used to initialize the variables in the duplicated part).

   TODO: the common case is that latch of the loop is empty and immediately
   follows the loop exit.  In this case, it would be better not to copy the
   body of the loop, but only move the entry of the loop directly before the
   exit check and increase the number of iterations of the loop by one.
   This may need some additional preconditioning in case NIT = ~0.
   REDUCTION_LIST describes the reductions in LOOP.  */

static void
transform_to_exit_first_loop (struct loop *loop, htab_t reduction_list, tree nit)
{
  basic_block *bbs, *nbbs, ex_bb, orig_header;
  unsigned n;
  bool ok;
  edge exit = single_dom_exit (loop), hpred;
  tree control, control_name, res, t;
  gimple phi, nphi, cond_stmt, stmt, cond_nit;
  gimple_stmt_iterator gsi;
  tree nit_1;

  split_block_after_labels (loop->header);
  orig_header = single_succ (loop->header);
  hpred = single_succ_edge (loop->header);

  cond_stmt = last_stmt (exit->src);
  control = gimple_cond_lhs (cond_stmt);
  gcc_assert (gimple_cond_rhs (cond_stmt) == nit);

  /* Make sure that we have phi nodes on exit for all loop header phis
     (create_parallel_loop requires that).  */
  for (gsi = gsi_start_phis (loop->header); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      phi = gsi_stmt (gsi);
      res = PHI_RESULT (phi);
      t = make_ssa_name (SSA_NAME_VAR (res), phi);
      SET_PHI_RESULT (phi, t);
      nphi = create_phi_node (res, orig_header);
      SSA_NAME_DEF_STMT (res) = nphi;
      add_phi_arg (nphi, t, hpred, UNKNOWN_LOCATION);

      if (res == control)
	{
	  gimple_cond_set_lhs (cond_stmt, t);
	  update_stmt (cond_stmt);
	  control = t;
	}
    }
  bbs = get_loop_body_in_dom_order (loop);

  for (n = 0; bbs[n] != loop->latch; n++)
    continue;
  nbbs = XNEWVEC (basic_block, n);
  ok = gimple_duplicate_sese_tail (single_succ_edge (loop->header), exit,
				   bbs + 1, n, nbbs);
  gcc_assert (ok);
  free (bbs);
  ex_bb = nbbs[0];
  free (nbbs);

  /* Other than reductions, the only gimple reg that should be copied
     out of the loop is the control variable.  */

  control_name = NULL_TREE;
  for (gsi = gsi_start_phis (ex_bb); !gsi_end_p (gsi); )
    {
      phi = gsi_stmt (gsi);
      res = PHI_RESULT (phi);
      if (!is_gimple_reg (res))
	{
	  gsi_next (&gsi);
	  continue;
	}

      /* Check if it is a part of reduction.  If it is,
         keep the phi at the reduction's keep_res field.  The
         PHI_RESULT of this phi is the resulting value of the reduction
         variable when exiting the loop.  */

      exit = single_dom_exit (loop);

      if (htab_elements (reduction_list) > 0)
	{
	  struct reduction_info *red;

	  tree val = PHI_ARG_DEF_FROM_EDGE (phi, exit);
	  red = reduction_phi (reduction_list, SSA_NAME_DEF_STMT (val));
	  if (red)
	    {
	      red->keep_res = phi;
	      gsi_next (&gsi);
	      continue;
	    }
	}
      gcc_assert (control_name == NULL_TREE
		  && SSA_NAME_VAR (res) == SSA_NAME_VAR (control));
      control_name = res;
      remove_phi_node (&gsi, false);
    }
  gcc_assert (control_name != NULL_TREE);

  /* Initialize the control variable to number of iterations
     according to the rhs of the exit condition.  */
  gsi = gsi_after_labels (ex_bb);
  cond_nit = last_stmt (exit->src);
  nit_1 =  gimple_cond_rhs (cond_nit);
  nit_1 = force_gimple_operand_gsi (&gsi,
				  fold_convert (TREE_TYPE (control_name), nit_1),
				  false, NULL_TREE, false, GSI_SAME_STMT);
  stmt = gimple_build_assign (control_name, nit_1);
  gsi_insert_before (&gsi, stmt, GSI_NEW_STMT);
  SSA_NAME_DEF_STMT (control_name) = stmt;
}

/* Create the parallel constructs for LOOP as described in gen_parallel_loop.
   LOOP_FN and DATA are the arguments of GIMPLE_OMP_PARALLEL.
   NEW_DATA is the variable that should be initialized from the argument
   of LOOP_FN.  N_THREADS is the requested number of threads.  Returns the
   basic block containing GIMPLE_OMP_PARALLEL tree.  */

static basic_block
create_parallel_loop (struct loop *loop, tree loop_fn, tree data,
		      tree new_data, unsigned n_threads)
{
  gimple_stmt_iterator gsi;
  basic_block bb, paral_bb, for_bb, ex_bb;
  tree t, param;
  gimple stmt, for_stmt, phi, cond_stmt;
  tree cvar, cvar_init, initvar, cvar_next, cvar_base, type;
  edge exit, nexit, guard, end, e;

  /* Prepare the GIMPLE_OMP_PARALLEL statement.  */
  bb = loop_preheader_edge (loop)->src;
  paral_bb = single_pred (bb);
  gsi = gsi_last_bb (paral_bb);

  t = build_omp_clause (BUILTINS_LOCATION, OMP_CLAUSE_NUM_THREADS);
  OMP_CLAUSE_NUM_THREADS_EXPR (t)
    = build_int_cst (integer_type_node, n_threads);
  stmt = gimple_build_omp_parallel (NULL, t, loop_fn, data);

  gsi_insert_after (&gsi, stmt, GSI_NEW_STMT);

  /* Initialize NEW_DATA.  */
  if (data)
    {
      gsi = gsi_after_labels (bb);

      param = make_ssa_name (DECL_ARGUMENTS (loop_fn), NULL);
      stmt = gimple_build_assign (param, build_fold_addr_expr (data));
      gsi_insert_before (&gsi, stmt, GSI_SAME_STMT);
      SSA_NAME_DEF_STMT (param) = stmt;

      stmt = gimple_build_assign (new_data,
				  fold_convert (TREE_TYPE (new_data), param));
      gsi_insert_before (&gsi, stmt, GSI_SAME_STMT);
      SSA_NAME_DEF_STMT (new_data) = stmt;
    }

  /* Emit GIMPLE_OMP_RETURN for GIMPLE_OMP_PARALLEL.  */
  bb = split_loop_exit_edge (single_dom_exit (loop));
  gsi = gsi_last_bb (bb);
  gsi_insert_after (&gsi, gimple_build_omp_return (false), GSI_NEW_STMT);

  /* Extract data for GIMPLE_OMP_FOR.  */
  gcc_assert (loop->header == single_dom_exit (loop)->src);
  cond_stmt = last_stmt (loop->header);

  cvar = gimple_cond_lhs (cond_stmt);
  cvar_base = SSA_NAME_VAR (cvar);
  phi = SSA_NAME_DEF_STMT (cvar);
  cvar_init = PHI_ARG_DEF_FROM_EDGE (phi, loop_preheader_edge (loop));
  initvar = make_ssa_name (cvar_base, NULL);
  SET_USE (PHI_ARG_DEF_PTR_FROM_EDGE (phi, loop_preheader_edge (loop)),
	   initvar);
  cvar_next = PHI_ARG_DEF_FROM_EDGE (phi, loop_latch_edge (loop));

  gsi = gsi_last_bb (loop->latch);
  gcc_assert (gsi_stmt (gsi) == SSA_NAME_DEF_STMT (cvar_next));
  gsi_remove (&gsi, true);

  /* Prepare cfg.  */
  for_bb = split_edge (loop_preheader_edge (loop));
  ex_bb = split_loop_exit_edge (single_dom_exit (loop));
  extract_true_false_edges_from_block (loop->header, &nexit, &exit);
  gcc_assert (exit == single_dom_exit (loop));

  guard = make_edge (for_bb, ex_bb, 0);
  single_succ_edge (loop->latch)->flags = 0;
  end = make_edge (loop->latch, ex_bb, EDGE_FALLTHRU);
  for (gsi = gsi_start_phis (ex_bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      source_location locus;
      tree def;
      phi = gsi_stmt (gsi);
      stmt = SSA_NAME_DEF_STMT (PHI_ARG_DEF_FROM_EDGE (phi, exit));

      def = PHI_ARG_DEF_FROM_EDGE (stmt, loop_preheader_edge (loop));
      locus = gimple_phi_arg_location_from_edge (stmt,
						 loop_preheader_edge (loop));
      add_phi_arg (phi, def, guard, locus);

      def = PHI_ARG_DEF_FROM_EDGE (stmt, loop_latch_edge (loop));
      locus = gimple_phi_arg_location_from_edge (stmt, loop_latch_edge (loop));
      add_phi_arg (phi, def, end, locus);
    }
  e = redirect_edge_and_branch (exit, nexit->dest);
  PENDING_STMT (e) = NULL;

  /* Emit GIMPLE_OMP_FOR.  */
  gimple_cond_set_lhs (cond_stmt, cvar_base);
  type = TREE_TYPE (cvar);
  t = build_omp_clause (BUILTINS_LOCATION, OMP_CLAUSE_SCHEDULE);
  OMP_CLAUSE_SCHEDULE_KIND (t) = OMP_CLAUSE_SCHEDULE_STATIC;

  for_stmt = gimple_build_omp_for (NULL, t, 1, NULL);
  gimple_omp_for_set_index (for_stmt, 0, initvar);
  gimple_omp_for_set_initial (for_stmt, 0, cvar_init);
  gimple_omp_for_set_final (for_stmt, 0, gimple_cond_rhs (cond_stmt));
  gimple_omp_for_set_cond (for_stmt, 0, gimple_cond_code (cond_stmt));
  gimple_omp_for_set_incr (for_stmt, 0, build2 (PLUS_EXPR, type,
						cvar_base,
						build_int_cst (type, 1)));

  gsi = gsi_last_bb (for_bb);
  gsi_insert_after (&gsi, for_stmt, GSI_NEW_STMT);
  SSA_NAME_DEF_STMT (initvar) = for_stmt;

  /* Emit GIMPLE_OMP_CONTINUE.  */
  gsi = gsi_last_bb (loop->latch);
  stmt = gimple_build_omp_continue (cvar_next, cvar);
  gsi_insert_after (&gsi, stmt, GSI_NEW_STMT);
  SSA_NAME_DEF_STMT (cvar_next) = stmt;

  /* Emit GIMPLE_OMP_RETURN for GIMPLE_OMP_FOR.  */
  gsi = gsi_last_bb (ex_bb);
  gsi_insert_after (&gsi, gimple_build_omp_return (true), GSI_NEW_STMT);

  return paral_bb;
}

/* Generates code to execute the iterations of LOOP in N_THREADS
   threads in parallel.

   NITER describes number of iterations of LOOP.
   REDUCTION_LIST describes the reductions existent in the LOOP.  */

static void
gen_parallel_loop (struct loop *loop, htab_t reduction_list,
		   unsigned n_threads, struct tree_niter_desc *niter)
{
  loop_iterator li;
  tree many_iterations_cond, type, nit;
  tree arg_struct, new_arg_struct;
  gimple_seq stmts;
  basic_block parallel_head;
  edge entry, exit;
  struct clsn_data clsn_data;
  unsigned prob;

  /* From

     ---------------------------------------------------------------------
     loop
       {
	 IV = phi (INIT, IV + STEP)
	 BODY1;
	 if (COND)
	   break;
	 BODY2;
       }
     ---------------------------------------------------------------------

     with # of iterations NITER (possibly with MAY_BE_ZERO assumption),
     we generate the following code:

     ---------------------------------------------------------------------

     if (MAY_BE_ZERO
     || NITER < MIN_PER_THREAD * N_THREADS)
     goto original;

     BODY1;
     store all local loop-invariant variables used in body of the loop to DATA.
     GIMPLE_OMP_PARALLEL (OMP_CLAUSE_NUM_THREADS (N_THREADS), LOOPFN, DATA);
     load the variables from DATA.
     GIMPLE_OMP_FOR (IV = INIT; COND; IV += STEP) (OMP_CLAUSE_SCHEDULE (static))
     BODY2;
     BODY1;
     GIMPLE_OMP_CONTINUE;
     GIMPLE_OMP_RETURN         -- GIMPLE_OMP_FOR
     GIMPLE_OMP_RETURN         -- GIMPLE_OMP_PARALLEL
     goto end;

     original:
     loop
       {
	 IV = phi (INIT, IV + STEP)
	 BODY1;
	 if (COND)
	   break;
	 BODY2;
       }

     end:

   */

  /* Create two versions of the loop -- in the old one, we know that the
     number of iterations is large enough, and we will transform it into the
     loop that will be split to loop_fn, the new one will be used for the
     remaining iterations.  */

  type = TREE_TYPE (niter->niter);
  nit = force_gimple_operand (unshare_expr (niter->niter), &stmts, true,
			      NULL_TREE);
  if (stmts)
    gsi_insert_seq_on_edge_immediate (loop_preheader_edge (loop), stmts);

  many_iterations_cond =
    fold_build2 (GE_EXPR, boolean_type_node,
		 nit, build_int_cst (type, MIN_PER_THREAD * n_threads));
  many_iterations_cond
    = fold_build2 (TRUTH_AND_EXPR, boolean_type_node,
		   invert_truthvalue (unshare_expr (niter->may_be_zero)),
		   many_iterations_cond);
  many_iterations_cond
    = force_gimple_operand (many_iterations_cond, &stmts, false, NULL_TREE);
  if (stmts)
    gsi_insert_seq_on_edge_immediate (loop_preheader_edge (loop), stmts);
  if (!is_gimple_condexpr (many_iterations_cond))
    {
      many_iterations_cond
	= force_gimple_operand (many_iterations_cond, &stmts,
				true, NULL_TREE);
      if (stmts)
	gsi_insert_seq_on_edge_immediate (loop_preheader_edge (loop), stmts);
    }

  initialize_original_copy_tables ();

  /* We assume that the loop usually iterates a lot.  */
  prob = 4 * REG_BR_PROB_BASE / 5;
  loop_version (loop, many_iterations_cond, NULL,
		prob, prob, REG_BR_PROB_BASE - prob, true);
  update_ssa (TODO_update_ssa);
  free_original_copy_tables ();

  /* Base all the induction variables in LOOP on a single control one.  */
  canonicalize_loop_ivs (loop, &nit, true);

  /* Ensure that the exit condition is the first statement in the loop.  */
  transform_to_exit_first_loop (loop, reduction_list, nit);

  /* Generate initializations for reductions.  */
  if (htab_elements (reduction_list) > 0)
    htab_traverse (reduction_list, initialize_reductions, loop);

  /* Eliminate the references to local variables from the loop.  */
  gcc_assert (single_exit (loop));
  entry = loop_preheader_edge (loop);
  exit = single_dom_exit (loop);

  eliminate_local_variables (entry, exit);
  /* In the old loop, move all variables non-local to the loop to a structure
     and back, and create separate decls for the variables used in loop.  */
  separate_decls_in_region (entry, exit, reduction_list, &arg_struct,
			    &new_arg_struct, &clsn_data);

  /* Create the parallel constructs.  */
  parallel_head = create_parallel_loop (loop, create_loop_fn (), arg_struct,
					new_arg_struct, n_threads);
  if (htab_elements (reduction_list) > 0)
    create_call_for_reduction (loop, reduction_list, &clsn_data);

  scev_reset ();

  /* Cancel the loop (it is simpler to do it here rather than to teach the
     expander to do it).  */
  cancel_loop_tree (loop);

  /* Free loop bound estimations that could contain references to
     removed statements.  */
  FOR_EACH_LOOP (li, loop, 0)
    free_numbers_of_iterations_estimates_loop (loop);

  /* Expand the parallel constructs.  We do it directly here instead of running
     a separate expand_omp pass, since it is more efficient, and less likely to
     cause troubles with further analyses not being able to deal with the
     OMP trees.  */

  omp_expand_local (parallel_head);
}

/* Returns true when LOOP contains vector phi nodes.  */

static bool
loop_has_vector_phi_nodes (struct loop *loop ATTRIBUTE_UNUSED)
{
  unsigned i;
  basic_block *bbs = get_loop_body_in_dom_order (loop);
  gimple_stmt_iterator gsi;
  bool res = true;

  for (i = 0; i < loop->num_nodes; i++)
    for (gsi = gsi_start_phis (bbs[i]); !gsi_end_p (gsi); gsi_next (&gsi))
      if (TREE_CODE (TREE_TYPE (PHI_RESULT (gsi_stmt (gsi)))) == VECTOR_TYPE)
	goto end;

  res = false;
 end:
  free (bbs);
  return res;
}

/* Create a reduction_info struct, initialize it with REDUC_STMT
   and PHI, insert it to the REDUCTION_LIST.  */

static void
build_new_reduction (htab_t reduction_list, gimple reduc_stmt, gimple phi)
{
  PTR *slot;
  struct reduction_info *new_reduction;

  gcc_assert (reduc_stmt);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file,
	       "Detected reduction. reduction stmt is: \n");
      print_gimple_stmt (dump_file, reduc_stmt, 0, 0);
      fprintf (dump_file, "\n");
    }

  new_reduction = XCNEW (struct reduction_info);

  new_reduction->reduc_stmt = reduc_stmt;
  new_reduction->reduc_phi = phi;
  new_reduction->reduction_code = gimple_assign_rhs_code (reduc_stmt);
  slot = htab_find_slot (reduction_list, new_reduction, INSERT);
  *slot = new_reduction;
}

/* Detect all reductions in the LOOP, insert them into REDUCTION_LIST.  */

static void
gather_scalar_reductions (loop_p loop, htab_t reduction_list)
{
  gimple_stmt_iterator gsi;
  loop_vec_info simple_loop_info;

  vect_dump = NULL;
  simple_loop_info = vect_analyze_loop_form (loop);

  for (gsi = gsi_start_phis (loop->header); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple phi = gsi_stmt (gsi);
      affine_iv iv;
      tree res = PHI_RESULT (phi);
      bool double_reduc;

      if (!is_gimple_reg (res))
	continue;

      if (!simple_iv (loop, loop, res, &iv, true)
	&& simple_loop_info)
	{
           gimple reduc_stmt = vect_is_simple_reduction (simple_loop_info, phi, true, &double_reduc);
	   if (reduc_stmt && !double_reduc)
              build_new_reduction (reduction_list, reduc_stmt, phi);
        }
    }
    destroy_loop_vec_info (simple_loop_info, true);
}

/* Try to initialize NITER for code generation part.  */

static bool
try_get_loop_niter (loop_p loop, struct tree_niter_desc *niter)
{
  edge exit = single_dom_exit (loop);

  gcc_assert (exit);

  /* We need to know # of iterations, and there should be no uses of values
     defined inside loop outside of it, unless the values are invariants of
     the loop.  */
  if (!number_of_iterations_exit (loop, exit, niter, false))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "  FAILED: number of iterations not known\n");
      return false;
    }

  return true;
}

/* Try to initialize REDUCTION_LIST for code generation part.
   REDUCTION_LIST describes the reductions.  */

static bool
try_create_reduction_list (loop_p loop, htab_t reduction_list)
{
  edge exit = single_dom_exit (loop);
  gimple_stmt_iterator gsi;

  gcc_assert (exit);

  gather_scalar_reductions (loop, reduction_list);


  for (gsi = gsi_start_phis (exit->dest); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple phi = gsi_stmt (gsi);
      struct reduction_info *red;
      imm_use_iterator imm_iter;
      use_operand_p use_p;
      gimple reduc_phi;
      tree val = PHI_ARG_DEF_FROM_EDGE (phi, exit);

      if (is_gimple_reg (val))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "phi is ");
	      print_gimple_stmt (dump_file, phi, 0, 0);
	      fprintf (dump_file, "arg of phi to exit:   value ");
	      print_generic_expr (dump_file, val, 0);
	      fprintf (dump_file, " used outside loop\n");
	      fprintf (dump_file,
		       "  checking if it a part of reduction pattern:  \n");
	    }
	  if (htab_elements (reduction_list) == 0)
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file,
			 "  FAILED: it is not a part of reduction.\n");
	      return false;
	    }
	  reduc_phi = NULL;
	  FOR_EACH_IMM_USE_FAST (use_p, imm_iter, val)
	    {
	      if (flow_bb_inside_loop_p (loop, gimple_bb (USE_STMT (use_p))))
		{
		  reduc_phi = USE_STMT (use_p);
		  break;
		}
	    }
	  red = reduction_phi (reduction_list, reduc_phi);
	  if (red == NULL)
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file,
			 "  FAILED: it is not a part of reduction.\n");
	      return false;
	    }
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "reduction phi is  ");
	      print_gimple_stmt (dump_file, red->reduc_phi, 0, 0);
	      fprintf (dump_file, "reduction stmt is  ");
	      print_gimple_stmt (dump_file, red->reduc_stmt, 0, 0);
	    }
	}
    }

  /* The iterations of the loop may communicate only through bivs whose
     iteration space can be distributed efficiently.  */
  for (gsi = gsi_start_phis (loop->header); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple phi = gsi_stmt (gsi);
      tree def = PHI_RESULT (phi);
      affine_iv iv;

      if (is_gimple_reg (def) && !simple_iv (loop, loop, def, &iv, true))
	{
	  struct reduction_info *red;

	  red = reduction_phi (reduction_list, phi);
	  if (red == NULL)
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file,
			 "  FAILED: scalar dependency between iterations\n");
	      return false;
	    }
	}
    }


  return true;
}

/* Detect parallel loops and generate parallel code using libgomp
   primitives.  Returns true if some loop was parallelized, false
   otherwise.  */

bool
parallelize_loops (void)
{
  unsigned n_threads = flag_tree_parallelize_loops;
  bool changed = false;
  struct loop *loop;
  struct tree_niter_desc niter_desc;
  loop_iterator li;
  htab_t reduction_list;
  HOST_WIDE_INT estimated;
  LOC loop_loc;
  
  /* Do not parallelize loops in the functions created by parallelization.  */
  if (parallelized_function_p (cfun->decl))
    return false;
  if (cfun->has_nonlocal_label)
    return false;

  reduction_list = htab_create (10, reduction_info_hash,
				     reduction_info_eq, free);
  init_stmt_vec_info_vec ();

  FOR_EACH_LOOP (li, loop, 0)
    {
      htab_empty (reduction_list);
      if (dump_file && (dump_flags & TDF_DETAILS))
      {
        fprintf (dump_file, "Trying loop %d as candidate\n",loop->num);
	if (loop->inner)
	  fprintf (dump_file, "loop %d is not innermost\n",loop->num);
	else
	  fprintf (dump_file, "loop %d is innermost\n",loop->num);
      }

      /* If we use autopar in graphite pass, we use its marked dependency
      checking results.  */
      if (flag_loop_parallelize_all && !loop->can_be_parallel)
      {
        if (dump_file && (dump_flags & TDF_DETAILS))
	   fprintf (dump_file, "loop is not parallel according to graphite\n");
	continue;
      }

      if (!single_dom_exit (loop))
      {

        if (dump_file && (dump_flags & TDF_DETAILS))
	  fprintf (dump_file, "loop is !single_dom_exit\n");

	continue;
      }

      if (/* And of course, the loop must be parallelizable.  */
	  !can_duplicate_loop_p (loop)
	  || loop_has_blocks_with_irreducible_flag (loop)
	  || (loop_preheader_edge (loop)->src->flags & BB_IRREDUCIBLE_LOOP)
	  /* FIXME: the check for vector phi nodes could be removed.  */
	  || loop_has_vector_phi_nodes (loop))
	continue;
      estimated = estimated_loop_iterations_int (loop, false);
      /* FIXME: Bypass this check as graphite doesn't update the
      count and frequency correctly now.  */
      if (!flag_loop_parallelize_all
	  && ((estimated !=-1 
	     && estimated <= (HOST_WIDE_INT) n_threads * MIN_PER_THREAD)
	      /* Do not bother with loops in cold areas.  */
	      || optimize_loop_nest_for_size_p (loop)))
	continue;

      if (!try_get_loop_niter (loop, &niter_desc))
	continue;

      if (!try_create_reduction_list (loop, reduction_list))
	continue;

      if (!flag_loop_parallelize_all && !loop_parallel_p (loop))
	continue;

      changed = true;
      if (dump_file && (dump_flags & TDF_DETAILS))
      {
	if (loop->inner)
	  fprintf (dump_file, "parallelizing outer loop %d\n",loop->header->index);
	else
	  fprintf (dump_file, "parallelizing inner loop %d\n",loop->header->index);
	loop_loc = find_loop_location (loop);
	if (loop_loc != UNKNOWN_LOC)
	  fprintf (dump_file, "\nloop at %s:%d: ",
		   LOC_FILE (loop_loc), LOC_LINE (loop_loc));
      }
      gen_parallel_loop (loop, reduction_list,
			 n_threads, &niter_desc);
      verify_flow_info ();
      verify_dominators (CDI_DOMINATORS);
      verify_loop_structure ();
      verify_loop_closed_ssa ();
    }

  free_stmt_vec_info_vec ();
  htab_delete (reduction_list);

  /* Parallelization will cause new function calls to be inserted through
     which local variables will escape.  Reset the points-to solutions
     for ESCAPED and CALLUSED.  */
  if (changed)
    {
      pt_solution_reset (&cfun->gimple_df->escaped);
      pt_solution_reset (&cfun->gimple_df->callused);
    }

  return changed;
}

#include "gt-tree-parloops.h"
