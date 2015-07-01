/* Loop autoparallelization.
   Copyright (C) 2006-2015 Free Software Foundation, Inc.
   Contributed by Sebastian Pop <pop@cri.ensmp.fr> 
   Zdenek Dvorak <dvorakz@suse.cz> and Razya Ladelsky <razya@il.ibm.com>.

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
#include "alias.h"
#include "symtab.h"
#include "options.h"
#include "tree.h"
#include "fold-const.h"
#include "predict.h"
#include "tm.h"
#include "hard-reg-set.h"
#include "function.h"
#include "dominance.h"
#include "cfg.h"
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-expr.h"
#include "gimple.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "gimple-walk.h"
#include "stor-layout.h"
#include "tree-nested.h"
#include "gimple-ssa.h"
#include "tree-cfg.h"
#include "tree-phinodes.h"
#include "ssa-iterators.h"
#include "stringpool.h"
#include "tree-ssanames.h"
#include "tree-ssa-loop-ivopts.h"
#include "tree-ssa-loop-manip.h"
#include "tree-ssa-loop-niter.h"
#include "tree-ssa-loop.h"
#include "tree-into-ssa.h"
#include "cfgloop.h"
#include "tree-data-ref.h"
#include "tree-scalar-evolution.h"
#include "gimple-pretty-print.h"
#include "tree-pass.h"
#include "langhooks.h"
#include "tree-vectorizer.h"
#include "tree-hasher.h"
#include "tree-parloops.h"
#include "omp-low.h"
#include "tree-nested.h"
#include "cgraph.h"
#include "tree-ssa.h"

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
   -- handling of common reduction patterns for outer loops.  
    
   More info can also be found at http://gcc.gnu.org/wiki/AutoParInGCC  */
/*
  Reduction handling:
  currently we use vect_force_simple_reduction() to detect reduction patterns.
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
  unsigned reduc_version;	/* SSA_NAME_VERSION of original reduc_phi
				   result.  */
  gphi *keep_res;		/* The PHI_RESULT of this phi is the resulting value
				   of the reduction variable when existing the loop. */
  tree initial_value;		/* The initial value of the reduction var before entering the loop.  */
  tree field;			/*  the name of the field in the parloop data structure intended for reduction.  */
  tree init;			/* reduction initialization value.  */
  gphi *new_phi;		/* (helper field) Newly created phi node whose result
				   will be passed to the atomic operation.  Represents
				   the local result each thread computed for the reduction
				   operation.  */
};

/* Reduction info hashtable helpers.  */

struct reduction_hasher : free_ptr_hash <reduction_info>
{
  static inline hashval_t hash (const reduction_info *);
  static inline bool equal (const reduction_info *, const reduction_info *);
};

/* Equality and hash functions for hashtab code.  */

inline bool
reduction_hasher::equal (const reduction_info *a, const reduction_info *b)
{
  return (a->reduc_phi == b->reduc_phi);
}

inline hashval_t
reduction_hasher::hash (const reduction_info *a)
{
  return a->reduc_version;
}

typedef hash_table<reduction_hasher> reduction_info_table_type;


static struct reduction_info *
reduction_phi (reduction_info_table_type *reduction_list, gimple phi)
{
  struct reduction_info tmpred, *red;

  if (reduction_list->elements () == 0 || phi == NULL)
    return NULL;

  tmpred.reduc_phi = phi;
  tmpred.reduc_version = gimple_uid (phi);
  red = reduction_list->find (&tmpred);

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

/* Name copies hashtable helpers.  */

struct name_to_copy_hasher : free_ptr_hash <name_to_copy_elt>
{
  static inline hashval_t hash (const name_to_copy_elt *);
  static inline bool equal (const name_to_copy_elt *, const name_to_copy_elt *);
};

/* Equality and hash functions for hashtab code.  */

inline bool
name_to_copy_hasher::equal (const name_to_copy_elt *a, const name_to_copy_elt *b)
{
  return a->version == b->version;
}

inline hashval_t
name_to_copy_hasher::hash (const name_to_copy_elt *a)
{
  return (hashval_t) a->version;
}

typedef hash_table<name_to_copy_hasher> name_to_copy_table_type;

/* A transformation matrix, which is a self-contained ROWSIZE x COLSIZE
   matrix.  Rather than use floats, we simply keep a single DENOMINATOR that
   represents the denominator for every element in the matrix.  */
typedef struct lambda_trans_matrix_s
{
  lambda_matrix matrix;
  int rowsize;
  int colsize;
  int denominator;
} *lambda_trans_matrix;
#define LTM_MATRIX(T) ((T)->matrix)
#define LTM_ROWSIZE(T) ((T)->rowsize)
#define LTM_COLSIZE(T) ((T)->colsize)
#define LTM_DENOMINATOR(T) ((T)->denominator)

/* Allocate a new transformation matrix.  */

static lambda_trans_matrix
lambda_trans_matrix_new (int colsize, int rowsize,
			 struct obstack * lambda_obstack)
{
  lambda_trans_matrix ret;

  ret = (lambda_trans_matrix)
    obstack_alloc (lambda_obstack, sizeof (struct lambda_trans_matrix_s));
  LTM_MATRIX (ret) = lambda_matrix_new (rowsize, colsize, lambda_obstack);
  LTM_ROWSIZE (ret) = rowsize;
  LTM_COLSIZE (ret) = colsize;
  LTM_DENOMINATOR (ret) = 1;
  return ret;
}

/* Multiply a vector VEC by a matrix MAT.
   MAT is an M*N matrix, and VEC is a vector with length N.  The result
   is stored in DEST which must be a vector of length M.  */

static void
lambda_matrix_vector_mult (lambda_matrix matrix, int m, int n,
			   lambda_vector vec, lambda_vector dest)
{
  int i, j;

  lambda_vector_clear (dest, m);
  for (i = 0; i < m; i++)
    for (j = 0; j < n; j++)
      dest[i] += matrix[i][j] * vec[j];
}

/* Return true if TRANS is a legal transformation matrix that respects
   the dependence vectors in DISTS and DIRS.  The conservative answer
   is false.

   "Wolfe proves that a unimodular transformation represented by the
   matrix T is legal when applied to a loop nest with a set of
   lexicographically non-negative distance vectors RDG if and only if
   for each vector d in RDG, (T.d >= 0) is lexicographically positive.
   i.e.: if and only if it transforms the lexicographically positive
   distance vectors to lexicographically positive vectors.  Note that
   a unimodular matrix must transform the zero vector (and only it) to
   the zero vector." S.Muchnick.  */

static bool
lambda_transform_legal_p (lambda_trans_matrix trans,
			  int nb_loops,
			  vec<ddr_p> dependence_relations)
{
  unsigned int i, j;
  lambda_vector distres;
  struct data_dependence_relation *ddr;

  gcc_assert (LTM_COLSIZE (trans) == nb_loops
	      && LTM_ROWSIZE (trans) == nb_loops);

  /* When there are no dependences, the transformation is correct.  */
  if (dependence_relations.length () == 0)
    return true;

  ddr = dependence_relations[0];
  if (ddr == NULL)
    return true;

  /* When there is an unknown relation in the dependence_relations, we
     know that it is no worth looking at this loop nest: give up.  */
  if (DDR_ARE_DEPENDENT (ddr) == chrec_dont_know)
    return false;

  distres = lambda_vector_new (nb_loops);

  /* For each distance vector in the dependence graph.  */
  FOR_EACH_VEC_ELT (dependence_relations, i, ddr)
    {
      /* Don't care about relations for which we know that there is no
	 dependence, nor about read-read (aka. output-dependences):
	 these data accesses can happen in any order.  */
      if (DDR_ARE_DEPENDENT (ddr) == chrec_known
	  || (DR_IS_READ (DDR_A (ddr)) && DR_IS_READ (DDR_B (ddr))))
	continue;

      /* Conservatively answer: "this transformation is not valid".  */
      if (DDR_ARE_DEPENDENT (ddr) == chrec_dont_know)
	return false;

      /* If the dependence could not be captured by a distance vector,
	 conservatively answer that the transform is not valid.  */
      if (DDR_NUM_DIST_VECTS (ddr) == 0)
	return false;

      /* Compute trans.dist_vect */
      for (j = 0; j < DDR_NUM_DIST_VECTS (ddr); j++)
	{
	  lambda_matrix_vector_mult (LTM_MATRIX (trans), nb_loops, nb_loops,
				     DDR_DIST_VECT (ddr, j), distres);

	  if (!lambda_vector_lexico_pos (distres, nb_loops))
	    return false;
	}
    }
  return true;
}

/* Data dependency analysis. Returns true if the iterations of LOOP
   are independent on each other (that is, if we can execute them
   in parallel).  */

static bool
loop_parallel_p (struct loop *loop, struct obstack * parloop_obstack)
{
  vec<ddr_p> dependence_relations;
  vec<data_reference_p> datarefs;
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
  auto_vec<loop_p, 3> loop_nest;
  datarefs.create (10);
  dependence_relations.create (100);
  if (! compute_data_dependences_for_loop (loop, true, &loop_nest, &datarefs,
					   &dependence_relations))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "  FAILED: cannot analyze data dependencies\n");
      ret = false;
      goto end;
    }
  if (dump_file && (dump_flags & TDF_DETAILS))
    dump_data_dependence_relations (dump_file, dependence_relations);

  trans = lambda_trans_matrix_new (1, 1, parloop_obstack);
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

 end:
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
   be invariant in the whole function.  Other needed statements are placed
   right before GSI.  */

static tree
take_address_of (tree obj, tree type, edge entry,
		 int_tree_htab_type *decl_address, gimple_stmt_iterator *gsi)
{
  int uid;
  tree *var_p, name, addr;
  gassign *stmt;
  gimple_seq stmts;

  /* Since the address of OBJ is invariant, the trees may be shared.
     Avoid rewriting unrelated parts of the code.  */
  obj = unshare_expr (obj);
  for (var_p = &obj;
       handled_component_p (*var_p);
       var_p = &TREE_OPERAND (*var_p, 0))
    continue;

  /* Canonicalize the access to base on a MEM_REF.  */
  if (DECL_P (*var_p))
    *var_p = build_simple_mem_ref (build_fold_addr_expr (*var_p));

  /* Assign a canonical SSA name to the address of the base decl used
     in the address and share it for all accesses and addresses based
     on it.  */
  uid = DECL_UID (TREE_OPERAND (TREE_OPERAND (*var_p, 0), 0));
  int_tree_map elt;
  elt.uid = uid;
  int_tree_map *slot = decl_address->find_slot (elt, INSERT);
  if (!slot->to)
    {
      if (gsi == NULL)
	return NULL;
      addr = TREE_OPERAND (*var_p, 0);
      const char *obj_name
	= get_name (TREE_OPERAND (TREE_OPERAND (*var_p, 0), 0));
      if (obj_name)
	name = make_temp_ssa_name (TREE_TYPE (addr), NULL, obj_name);
      else
	name = make_ssa_name (TREE_TYPE (addr));
      stmt = gimple_build_assign (name, addr);
      gsi_insert_on_edge_immediate (entry, stmt);

      slot->uid = uid;
      slot->to = name;
    }
  else
    name = slot->to;

  /* Express the address in terms of the canonical SSA name.  */
  TREE_OPERAND (*var_p, 0) = name;
  if (gsi == NULL)
    return build_fold_addr_expr_with_type (obj, type);

  name = force_gimple_operand (build_addr (obj, current_function_decl),
			       &stmts, true, NULL_TREE);
  if (!gimple_seq_empty_p (stmts))
    gsi_insert_seq_before (gsi, stmts, GSI_SAME_STMT);

  if (!useless_type_conversion_p (type, TREE_TYPE (name)))
    {
      name = force_gimple_operand (fold_convert (type, name), &stmts, true,
				   NULL_TREE);
      if (!gimple_seq_empty_p (stmts))
	gsi_insert_seq_before (gsi, stmts, GSI_SAME_STMT);
    }

  return name;
}

/* Callback for htab_traverse.  Create the initialization statement
   for reduction described in SLOT, and place it at the preheader of
   the loop described in DATA.  */

int
initialize_reductions (reduction_info **slot, struct loop *loop)
{
  tree init, c;
  tree bvar, type, arg;
  edge e;

  struct reduction_info *const reduc = *slot;

  /* Create initialization in preheader:
     reduction_variable = initialization value of reduction.  */

  /* In the phi node at the header, replace the argument coming
     from the preheader with the reduction initialization value.  */

  /* Create a new variable to initialize the reduction.  */
  type = TREE_TYPE (PHI_RESULT (reduc->reduc_phi));
  bvar = create_tmp_var (type, "reduction");

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
  int_tree_htab_type *decl_address;
  gimple_stmt_iterator *gsi;
  bool changed;
  bool reset;
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
      addr = take_address_of (t, addr_type, dta->entry, dta->decl_address,
			      dta->gsi);
      if (dta->gsi == NULL && addr == NULL_TREE)
	{
	  dta->reset = true;
	  return NULL_TREE;
	}

      *tp = build_simple_mem_ref (addr);

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
      addr = take_address_of (obj, addr_type, dta->entry, dta->decl_address,
			      dta->gsi);
      if (dta->gsi == NULL && addr == NULL_TREE)
	{
	  dta->reset = true;
	  return NULL_TREE;
	}
      *tp = addr;

      dta->changed = true;
      return NULL_TREE;
    }

  if (!EXPR_P (t))
    *walk_subtrees = 0;

  return NULL_TREE;
}

/* Moves the references to local variables in STMT at *GSI out of the single
   entry single exit region starting at ENTRY.  DECL_ADDRESS contains
   addresses of the references that had their address taken
   already.  */

static void
eliminate_local_variables_stmt (edge entry, gimple_stmt_iterator *gsi,
				int_tree_htab_type *decl_address)
{
  struct elv_data dta;
  gimple stmt = gsi_stmt (*gsi);

  memset (&dta.info, '\0', sizeof (dta.info));
  dta.entry = entry;
  dta.decl_address = decl_address;
  dta.changed = false;
  dta.reset = false;

  if (gimple_debug_bind_p (stmt))
    {
      dta.gsi = NULL;
      walk_tree (gimple_debug_bind_get_value_ptr (stmt),
		 eliminate_local_variables_1, &dta.info, NULL);
      if (dta.reset)
	{
	  gimple_debug_bind_reset_value (stmt);
	  dta.changed = true;
	}
    }
  else if (gimple_clobber_p (stmt))
    {
      stmt = gimple_build_nop ();
      gsi_replace (gsi, stmt, false);
      dta.changed = true;
    }
  else
    {
      dta.gsi = gsi;
      walk_gimple_op (stmt, eliminate_local_variables_1, &dta.info);
    }

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
  auto_vec<basic_block, 3> body;
  unsigned i;
  gimple_stmt_iterator gsi;
  bool has_debug_stmt = false;
  int_tree_htab_type decl_address (10);
  basic_block entry_bb = entry->src;
  basic_block exit_bb = exit->dest;

  gather_blocks_in_sese_region (entry_bb, exit_bb, &body);

  FOR_EACH_VEC_ELT (body, i, bb)
    if (bb != entry_bb && bb != exit_bb)
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	if (is_gimple_debug (gsi_stmt (gsi)))
	  {
	    if (gimple_debug_bind_p (gsi_stmt (gsi)))
	      has_debug_stmt = true;
	  }
	else
	  eliminate_local_variables_stmt (entry, &gsi, &decl_address);

  if (has_debug_stmt)
    FOR_EACH_VEC_ELT (body, i, bb)
      if (bb != entry_bb && bb != exit_bb)
	for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	  if (gimple_debug_bind_p (gsi_stmt (gsi)))
	    eliminate_local_variables_stmt (entry, &gsi, &decl_address);
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
separate_decls_in_region_name (tree name, name_to_copy_table_type *name_copies,
			       int_tree_htab_type *decl_copies,
			       bool copy_name_p)
{
  tree copy, var, var_copy;
  unsigned idx, uid, nuid;
  struct int_tree_map ielt;
  struct name_to_copy_elt elt, *nelt;
  name_to_copy_elt **slot;
  int_tree_map *dslot;

  if (TREE_CODE (name) != SSA_NAME)
    return name;

  idx = SSA_NAME_VERSION (name);
  elt.version = idx;
  slot = name_copies->find_slot_with_hash (&elt, idx,
					   copy_name_p ? INSERT : NO_INSERT);
  if (slot && *slot)
    return (*slot)->new_name;

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

  var = SSA_NAME_VAR (name);
  if (!var)
    return copy;

  uid = DECL_UID (var);
  ielt.uid = uid;
  dslot = decl_copies->find_slot_with_hash (ielt, uid, INSERT);
  if (!dslot->to)
    {
      var_copy = create_tmp_var (TREE_TYPE (var), get_name (var));
      DECL_GIMPLE_REG_P (var_copy) = DECL_GIMPLE_REG_P (var);
      dslot->uid = uid;
      dslot->to = var_copy;

      /* Ensure that when we meet this decl next time, we won't duplicate
         it again.  */
      nuid = DECL_UID (var_copy);
      ielt.uid = nuid;
      dslot = decl_copies->find_slot_with_hash (ielt, nuid, INSERT);
      gcc_assert (!dslot->to);
      dslot->uid = nuid;
      dslot->to = var_copy;
    }
  else
    var_copy = dslot->to;

  replace_ssa_name_symbol (copy, var_copy);
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
			       name_to_copy_table_type *name_copies,
			       int_tree_htab_type *decl_copies)
{
  use_operand_p use;
  def_operand_p def;
  ssa_op_iter oi;
  tree name, copy;
  bool copy_name_p;

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
separate_decls_in_region_debug (gimple stmt,
				name_to_copy_table_type *name_copies,
				int_tree_htab_type *decl_copies)
{
  use_operand_p use;
  ssa_op_iter oi;
  tree var, name;
  struct int_tree_map ielt;
  struct name_to_copy_elt elt;
  name_to_copy_elt **slot;
  int_tree_map *dslot;

  if (gimple_debug_bind_p (stmt))
    var = gimple_debug_bind_get_var (stmt);
  else if (gimple_debug_source_bind_p (stmt))
    var = gimple_debug_source_bind_get_var (stmt);
  else
    return true;
  if (TREE_CODE (var) == DEBUG_EXPR_DECL || TREE_CODE (var) == LABEL_DECL)
    return true;
  gcc_assert (DECL_P (var) && SSA_VAR_P (var));
  ielt.uid = DECL_UID (var);
  dslot = decl_copies->find_slot_with_hash (ielt, ielt.uid, NO_INSERT);
  if (!dslot)
    return true;
  if (gimple_debug_bind_p (stmt))
    gimple_debug_bind_set_var (stmt, dslot->to);
  else if (gimple_debug_source_bind_p (stmt))
    gimple_debug_source_bind_set_var (stmt, dslot->to);

  FOR_EACH_PHI_OR_STMT_USE (use, stmt, oi, SSA_OP_USE)
  {
    name = USE_FROM_PTR (use);
    if (TREE_CODE (name) != SSA_NAME)
      continue;

    elt.version = SSA_NAME_VERSION (name);
    slot = name_copies->find_slot_with_hash (&elt, elt.version, NO_INSERT);
    if (!slot)
      {
	gimple_debug_bind_reset_value (stmt);
	update_stmt (stmt);
	break;
      }

    SET_USE (use, (*slot)->new_name);
  }

  return false;
}

/* Callback for htab_traverse.  Adds a field corresponding to the reduction
   specified in SLOT. The type is passed in DATA.  */

int
add_field_for_reduction (reduction_info **slot, tree type)
{

  struct reduction_info *const red = *slot;
  tree var = gimple_assign_lhs (red->reduc_stmt);
  tree field = build_decl (gimple_location (red->reduc_stmt), FIELD_DECL,
			   SSA_NAME_IDENTIFIER (var), TREE_TYPE (var));

  insert_field_into_struct (type, field);

  red->field = field;

  return 1;
}

/* Callback for htab_traverse.  Adds a field corresponding to a ssa name
   described in SLOT. The type is passed in DATA.  */

int
add_field_for_name (name_to_copy_elt **slot, tree type)
{
  struct name_to_copy_elt *const elt = *slot;
  tree name = ssa_name (elt->version);
  tree field = build_decl (UNKNOWN_LOCATION,
			   FIELD_DECL, SSA_NAME_IDENTIFIER (name),
			   TREE_TYPE (name));

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

int
create_phi_for_local_result (reduction_info **slot, struct loop *loop)
{
  struct reduction_info *const reduc = *slot;
  edge e;
  gphi *new_phi;
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
  local_res = copy_ssa_name (gimple_assign_lhs (reduc->reduc_stmt));
  locus = gimple_location (reduc->reduc_stmt);
  new_phi = create_phi_node (local_res, store_bb);
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

int
create_call_for_reduction_1 (reduction_info **slot, struct clsn_data *clsn_data)
{
  struct reduction_info *const reduc = *slot;
  gimple_stmt_iterator gsi;
  tree type = TREE_TYPE (PHI_RESULT (reduc->reduc_phi));
  tree load_struct;
  basic_block bb;
  basic_block new_bb;
  edge e;
  tree t, addr, ref, x;
  tree tmp_load, name;
  gimple load;

  load_struct = build_simple_mem_ref (clsn_data->load);
  t = build3 (COMPONENT_REF, type, load_struct, reduc->field, NULL_TREE);

  addr = build_addr (t, current_function_decl);

  /* Create phi node.  */
  bb = clsn_data->load_bb;

  gsi = gsi_last_bb (bb);
  e = split_block (bb, gsi_stmt (gsi));
  new_bb = e->dest;

  tmp_load = create_tmp_var (TREE_TYPE (TREE_TYPE (addr)));
  tmp_load = make_ssa_name (tmp_load);
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
create_call_for_reduction (struct loop *loop,
			   reduction_info_table_type *reduction_list,
			   struct clsn_data *ld_st_data)
{
  reduction_list->traverse <struct loop *, create_phi_for_local_result> (loop);
  /* Find the fallthru edge from GIMPLE_OMP_CONTINUE.  */
  ld_st_data->load_bb = FALLTHRU_EDGE (loop->latch)->dest;
  reduction_list
    ->traverse <struct clsn_data *, create_call_for_reduction_1> (ld_st_data);
}

/* Callback for htab_traverse.  Loads the final reduction value at the
   join point of all threads, and inserts it in the right place.  */

int
create_loads_for_reductions (reduction_info **slot, struct clsn_data *clsn_data)
{
  struct reduction_info *const red = *slot;
  gimple stmt;
  gimple_stmt_iterator gsi;
  tree type = TREE_TYPE (gimple_assign_lhs (red->reduc_stmt));
  tree load_struct;
  tree name;
  tree x;

  gsi = gsi_after_labels (clsn_data->load_bb);
  load_struct = build_simple_mem_ref (clsn_data->load);
  load_struct = build3 (COMPONENT_REF, type, load_struct, red->field,
			NULL_TREE);

  x = load_struct;
  name = PHI_RESULT (red->keep_res);
  stmt = gimple_build_assign (name, x);

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
create_final_loads_for_reduction (reduction_info_table_type *reduction_list,
				  struct clsn_data *ld_st_data)
{
  gimple_stmt_iterator gsi;
  tree t;
  gimple stmt;

  gsi = gsi_after_labels (ld_st_data->load_bb);
  t = build_fold_addr_expr (ld_st_data->store);
  stmt = gimple_build_assign (ld_st_data->load, t);

  gsi_insert_before (&gsi, stmt, GSI_NEW_STMT);

  reduction_list
    ->traverse <struct clsn_data *, create_loads_for_reductions> (ld_st_data);

}

/* Callback for htab_traverse.  Store the neutral value for the
  particular reduction's operation, e.g. 0 for PLUS_EXPR,
  1 for MULT_EXPR, etc. into the reduction field.
  The reduction is specified in SLOT. The store information is
  passed in DATA.  */

int
create_stores_for_reduction (reduction_info **slot, struct clsn_data *clsn_data)
{
  struct reduction_info *const red = *slot;
  tree t;
  gimple stmt;
  gimple_stmt_iterator gsi;
  tree type = TREE_TYPE (gimple_assign_lhs (red->reduc_stmt));

  gsi = gsi_last_bb (clsn_data->store_bb);
  t = build3 (COMPONENT_REF, type, clsn_data->store, red->field, NULL_TREE);
  stmt = gimple_build_assign (t, red->initial_value);
  gsi_insert_after (&gsi, stmt, GSI_NEW_STMT);

  return 1;
}

/* Callback for htab_traverse.  Creates loads to a field of LOAD in LOAD_BB and
   store to a field of STORE in STORE_BB for the ssa name and its duplicate
   specified in SLOT.  */

int
create_loads_and_stores_for_name (name_to_copy_elt **slot,
				  struct clsn_data *clsn_data)
{
  struct name_to_copy_elt *const elt = *slot;
  tree t;
  gimple stmt;
  gimple_stmt_iterator gsi;
  tree type = TREE_TYPE (elt->new_name);
  tree load_struct;

  gsi = gsi_last_bb (clsn_data->store_bb);
  t = build3 (COMPONENT_REF, type, clsn_data->store, elt->field, NULL_TREE);
  stmt = gimple_build_assign (t, ssa_name (elt->version));
  gsi_insert_after (&gsi, stmt, GSI_NEW_STMT);

  gsi = gsi_last_bb (clsn_data->load_bb);
  load_struct = build_simple_mem_ref (clsn_data->load);
  t = build3 (COMPONENT_REF, type, load_struct, elt->field, NULL_TREE);
  stmt = gimple_build_assign (elt->new_name, t);
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
separate_decls_in_region (edge entry, edge exit,
			  reduction_info_table_type *reduction_list,
			  tree *arg_struct, tree *new_arg_struct,
			  struct clsn_data *ld_st_data)

{
  basic_block bb1 = split_edge (entry);
  basic_block bb0 = single_pred (bb1);
  name_to_copy_table_type name_copies (10);
  int_tree_htab_type decl_copies (10);
  unsigned i;
  tree type, type_name, nvar;
  gimple_stmt_iterator gsi;
  struct clsn_data clsn_data;
  auto_vec<basic_block, 3> body;
  basic_block bb;
  basic_block entry_bb = bb1;
  basic_block exit_bb = exit->dest;
  bool has_debug_stmt = false;

  entry = single_succ_edge (entry_bb);
  gather_blocks_in_sese_region (entry_bb, exit_bb, &body);

  FOR_EACH_VEC_ELT (body, i, bb)
    {
      if (bb != entry_bb && bb != exit_bb)
	{
	  for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	    separate_decls_in_region_stmt (entry, exit, gsi_stmt (gsi),
					   &name_copies, &decl_copies);

	  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	    {
	      gimple stmt = gsi_stmt (gsi);

	      if (is_gimple_debug (stmt))
		has_debug_stmt = true;
	      else
		separate_decls_in_region_stmt (entry, exit, stmt,
					       &name_copies, &decl_copies);
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
    FOR_EACH_VEC_ELT (body, i, bb)
      if (bb != entry_bb && bb != exit_bb)
	{
	  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi);)
	    {
	      gimple stmt = gsi_stmt (gsi);

	      if (is_gimple_debug (stmt))
		{
		  if (separate_decls_in_region_debug (stmt, &name_copies,
						      &decl_copies))
		    {
		      gsi_remove (&gsi, true);
		      continue;
		    }
		}

	      gsi_next (&gsi);
	    }
	}

  if (name_copies.elements () == 0 && reduction_list->elements () == 0)
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
      type_name = build_decl (UNKNOWN_LOCATION,
			      TYPE_DECL, create_tmp_var_name (".paral_data"),
			      type);
      TYPE_NAME (type) = type_name;

      name_copies.traverse <tree, add_field_for_name> (type);
      if (reduction_list && reduction_list->elements () > 0)
	{
	  /* Create the fields for reductions.  */
	  reduction_list->traverse <tree, add_field_for_reduction> (type);
	}
      layout_type (type);

      /* Create the loads and stores.  */
      *arg_struct = create_tmp_var (type, ".paral_data_store");
      nvar = create_tmp_var (build_pointer_type (type), ".paral_data_load");
      *new_arg_struct = make_ssa_name (nvar);

      ld_st_data->store = *arg_struct;
      ld_st_data->load = *new_arg_struct;
      ld_st_data->store_bb = bb0;
      ld_st_data->load_bb = bb1;

      name_copies
	.traverse <struct clsn_data *, create_loads_and_stores_for_name>
		  (ld_st_data);

      /* Load the calculation from memory (after the join of the threads).  */

      if (reduction_list && reduction_list->elements () > 0)
	{
	  reduction_list
	    ->traverse <struct clsn_data *, create_stores_for_reduction>
	    (ld_st_data);
	  clsn_data.load = make_ssa_name (nvar);
	  clsn_data.load_bb = exit->dest;
	  clsn_data.store = ld_st_data->store;
	  create_final_loads_for_reduction (reduction_list, &clsn_data);
	}
    }
}

/* Returns true if FN was created to run in parallel.  */

bool
parallelized_function_p (tree fndecl)
{
  cgraph_node *node = cgraph_node::get (fndecl);
  gcc_assert (node != NULL);
  return node->parallelized_function;
}

/* Creates and returns an empty function that will receive the body of
   a parallelized loop.  */

static tree
create_loop_fn (location_t loc)
{
  char buf[100];
  char *tname;
  tree decl, type, name, t;
  struct function *act_cfun = cfun;
  static unsigned loopfn_num;

  loc = LOCATION_LOCUS (loc);
  snprintf (buf, 100, "%s.$loopfn", current_function_name ());
  ASM_FORMAT_PRIVATE_NAME (tname, buf, loopfn_num++);
  clean_symbol_name (tname);
  name = get_identifier (tname);
  type = build_function_type_list (void_type_node, ptr_type_node, NULL_TREE);

  decl = build_decl (loc, FUNCTION_DECL, name, type);
  TREE_STATIC (decl) = 1;
  TREE_USED (decl) = 1;
  DECL_ARTIFICIAL (decl) = 1;
  DECL_IGNORED_P (decl) = 0;
  TREE_PUBLIC (decl) = 0;
  DECL_UNINLINABLE (decl) = 1;
  DECL_EXTERNAL (decl) = 0;
  DECL_CONTEXT (decl) = NULL_TREE;
  DECL_INITIAL (decl) = make_node (BLOCK);

  t = build_decl (loc, RESULT_DECL, NULL_TREE, void_type_node);
  DECL_ARTIFICIAL (t) = 1;
  DECL_IGNORED_P (t) = 1;
  DECL_RESULT (decl) = t;

  t = build_decl (loc, PARM_DECL, get_identifier (".paral_data_param"),
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

/* Replace uses of NAME by VAL in block BB.  */

static void
replace_uses_in_bb_by (tree name, tree val, basic_block bb)
{
  gimple use_stmt;
  imm_use_iterator imm_iter;

  FOR_EACH_IMM_USE_STMT (use_stmt, imm_iter, name)
    {
      if (gimple_bb (use_stmt) != bb)
	continue;

      use_operand_p use_p;
      FOR_EACH_IMM_USE_ON_STMT (use_p, imm_iter)
	SET_USE (use_p, val);
    }
}

/* Replace uses of NAME by VAL in blocks BBS.  */

static void
replace_uses_in_bbs_by (tree name, tree val, bitmap bbs)
{
  gimple use_stmt;
  imm_use_iterator imm_iter;

  FOR_EACH_IMM_USE_STMT (use_stmt, imm_iter, name)
    {
      if (!bitmap_bit_p (bbs, gimple_bb (use_stmt)->index))
	continue;

      use_operand_p use_p;
      FOR_EACH_IMM_USE_ON_STMT (use_p, imm_iter)
	SET_USE (use_p, val);
    }
}

/* Do transformation from:

     <bb preheader>:
     ...
     goto <bb header>

     <bb header>:
     ivtmp_a = PHI <ivtmp_init (preheader), ivtmp_b (latch)>
     sum_a = PHI <sum_init (preheader), sum_b (latch)>
     ...
     use (ivtmp_a)
     ...
     sum_b = sum_a + sum_update
     ...
     if (ivtmp_a < n)
       goto <bb latch>;
     else
       goto <bb exit>;

     <bb latch>:
     ivtmp_b = ivtmp_a + 1;
     goto <bb header>

     <bb exit>:
     sum_z = PHI <sum_b (cond[1])>

     [1] Where <bb cond> is single_pred (bb latch); In the simplest case,
	 that's <bb header>.

   to:

     <bb preheader>:
     ...
     goto <bb newheader>

     <bb header>:
     ivtmp_a = PHI <ivtmp_c (latch)>
     sum_a = PHI <sum_c (latch)>
     ...
     use (ivtmp_a)
     ...
     sum_b = sum_a + sum_update
     ...
     goto <bb latch>;

     <bb newheader>:
     ivtmp_c = PHI <ivtmp_init (preheader), ivtmp_b (latch)>
     sum_c = PHI <sum_init (preheader), sum_b (latch)>
     if (ivtmp_c < n + 1)
       goto <bb header>;
     else
       goto <bb exit>;

     <bb latch>:
     ivtmp_b = ivtmp_a + 1;
     goto <bb newheader>

     <bb exit>:
     sum_z = PHI <sum_c (newheader)>


   In unified diff format:

      <bb preheader>:
      ...
-     goto <bb header>
+     goto <bb newheader>

      <bb header>:
-     ivtmp_a = PHI <ivtmp_init (preheader), ivtmp_b (latch)>
-     sum_a = PHI <sum_init (preheader), sum_b (latch)>
+     ivtmp_a = PHI <ivtmp_c (latch)>
+     sum_a = PHI <sum_c (latch)>
      ...
      use (ivtmp_a)
      ...
      sum_b = sum_a + sum_update
      ...
-     if (ivtmp_a < n)
-       goto <bb latch>;
+     goto <bb latch>;
+
+     <bb newheader>:
+     ivtmp_c = PHI <ivtmp_init (preheader), ivtmp_b (latch)>
+     sum_c = PHI <sum_init (preheader), sum_b (latch)>
+     if (ivtmp_c < n + 1)
+       goto <bb header>;
      else
	goto <bb exit>;

      <bb latch>:
      ivtmp_b = ivtmp_a + 1;
-     goto <bb header>
+     goto <bb newheader>

      <bb exit>:
-     sum_z = PHI <sum_b (cond[1])>
+     sum_z = PHI <sum_c (newheader)>

   Note: the example does not show any virtual phis, but these are handled more
   or less as reductions.


   Moves the exit condition of LOOP to the beginning of its header.
   REDUCTION_LIST describes the reductions in LOOP.  BOUND is the new loop
   bound.  */

static void
transform_to_exit_first_loop_alt (struct loop *loop,
				  reduction_info_table_type *reduction_list,
				  tree bound)
{
  basic_block header = loop->header;
  basic_block latch = loop->latch;
  edge exit = single_dom_exit (loop);
  basic_block exit_block = exit->dest;
  gcond *cond_stmt = as_a <gcond *> (last_stmt (exit->src));
  tree control = gimple_cond_lhs (cond_stmt);
  edge e;

  /* Gather the bbs dominated by the exit block.  */
  bitmap exit_dominated = BITMAP_ALLOC (NULL);
  bitmap_set_bit (exit_dominated, exit_block->index);
  vec<basic_block> exit_dominated_vec
    = get_dominated_by (CDI_DOMINATORS, exit_block);

  int i;
  basic_block dom_bb;
  FOR_EACH_VEC_ELT (exit_dominated_vec, i, dom_bb)
    bitmap_set_bit (exit_dominated, dom_bb->index);

  exit_dominated_vec.release ();

  /* Create the new_header block.  */
  basic_block new_header = split_block_before_cond_jump (exit->src);
  edge split_edge = single_pred_edge (new_header);

  /* Redirect entry edge to new_header.  */
  edge entry = loop_preheader_edge (loop);
  e = redirect_edge_and_branch (entry, new_header);
  gcc_assert (e == entry);

  /* Redirect post_inc_edge to new_header.  */
  edge post_inc_edge = single_succ_edge (latch);
  e = redirect_edge_and_branch (post_inc_edge, new_header);
  gcc_assert (e == post_inc_edge);

  /* Redirect post_cond_edge to header.  */
  edge post_cond_edge = single_pred_edge (latch);
  e = redirect_edge_and_branch (post_cond_edge, header);
  gcc_assert (e == post_cond_edge);

  /* Redirect split_edge to latch.  */
  e = redirect_edge_and_branch (split_edge, latch);
  gcc_assert (e == split_edge);

  /* Set the new loop bound.  */
  gimple_cond_set_rhs (cond_stmt, bound);
  update_stmt (cond_stmt);

  /* Repair the ssa.  */
  vec<edge_var_map> *v = redirect_edge_var_map_vector (post_inc_edge);
  edge_var_map *vm;
  gphi_iterator gsi;
  for (gsi = gsi_start_phis (header), i = 0;
       !gsi_end_p (gsi) && v->iterate (i, &vm);
       gsi_next (&gsi), i++)
    {
      gphi *phi = gsi.phi ();
      tree res_a = PHI_RESULT (phi);

      /* Create new phi.  */
      tree res_c = copy_ssa_name (res_a, phi);
      gphi *nphi = create_phi_node (res_c, new_header);

      /* Replace ivtmp_a with ivtmp_c in condition 'if (ivtmp_a < n)'.  */
      replace_uses_in_bb_by (res_a, res_c, new_header);

      /* Replace ivtmp/sum_b with ivtmp/sum_c in header phi.  */
      add_phi_arg (phi, res_c, post_cond_edge, UNKNOWN_LOCATION);

      /* Replace sum_b with sum_c in exit phi.  Loop-closed ssa does not hold
	 for virtuals, so we cannot get away with exit_block only.  */
      tree res_b = redirect_edge_var_map_def (vm);
      replace_uses_in_bbs_by (res_b, res_c, exit_dominated);

      struct reduction_info *red = reduction_phi (reduction_list, phi);
      gcc_assert (virtual_operand_p (res_a)
		  || res_a == control
		  || red != NULL);

      if (red)
	{
	  /* Register the new reduction phi.  */
	  red->reduc_phi = nphi;
	  gimple_set_uid (red->reduc_phi, red->reduc_version);
	}
    }
  gcc_assert (gsi_end_p (gsi) && !v->iterate (i, &vm));
  BITMAP_FREE (exit_dominated);

  /* Set the preheader argument of the new phis to ivtmp/sum_init.  */
  flush_pending_stmts (entry);

  /* Set the latch arguments of the new phis to ivtmp/sum_b.  */
  flush_pending_stmts (post_inc_edge);

  /* Register the reduction exit phis.  */
  for (gphi_iterator gsi = gsi_start_phis (exit_block);
       !gsi_end_p (gsi);
       gsi_next (&gsi))
    {
      gphi *phi = gsi.phi ();
      tree res_z = PHI_RESULT (phi);
      if (virtual_operand_p (res_z))
	continue;

      tree res_c = PHI_ARG_DEF_FROM_EDGE (phi, exit);
      gimple reduc_phi = SSA_NAME_DEF_STMT (res_c);
      struct reduction_info *red = reduction_phi (reduction_list, reduc_phi);
      if (red != NULL)
	red->keep_res = phi;
    }

  /* We're going to cancel the loop at the end of gen_parallel_loop, but until
     then we're still using some fields, so only bother about fields that are
     still used: header and latch.
     The loop has a new header bb, so we update it.  The latch bb stays the
     same.  */
  loop->header = new_header;

  /* Recalculate dominance info.  */
  free_dominance_info (CDI_DOMINATORS);
  calculate_dominance_info (CDI_DOMINATORS);
}

/* Tries to moves the exit condition of LOOP to the beginning of its header
   without duplication of the loop body.  NIT is the number of iterations of the
   loop.  REDUCTION_LIST describes the reductions in LOOP.  Return true if
   transformation is successful.  */

static bool
try_transform_to_exit_first_loop_alt (struct loop *loop,
				      reduction_info_table_type *reduction_list,
				      tree nit)
{
  /* Check whether the latch contains a single statement.  */
  if (!gimple_seq_nondebug_singleton_p (bb_seq (loop->latch)))
    return false;

  /* Check whether the latch contains the loop iv increment.  */
  edge back = single_succ_edge (loop->latch);
  edge exit = single_dom_exit (loop);
  gcond *cond_stmt = as_a <gcond *> (last_stmt (exit->src));
  tree control = gimple_cond_lhs (cond_stmt);
  gphi *phi = as_a <gphi *> (SSA_NAME_DEF_STMT (control));
  tree inc_res = gimple_phi_arg_def (phi, back->dest_idx);
  if (gimple_bb (SSA_NAME_DEF_STMT (inc_res)) != loop->latch)
    return false;

  /* Check whether there's no code between the loop condition and the latch.  */
  if (!single_pred_p (loop->latch)
      || single_pred (loop->latch) != exit->src)
    return false;

  tree alt_bound = NULL_TREE;
  tree nit_type = TREE_TYPE (nit);

  /* Figure out whether nit + 1 overflows.  */
  if (TREE_CODE (nit) == INTEGER_CST)
    {
      if (!tree_int_cst_equal (nit, TYPE_MAXVAL (nit_type)))
	{
	  alt_bound = fold_build2_loc (UNKNOWN_LOCATION, PLUS_EXPR, nit_type,
				       nit, build_one_cst (nit_type));

	  gcc_assert (TREE_CODE (alt_bound) == INTEGER_CST);
	  transform_to_exit_first_loop_alt (loop, reduction_list, alt_bound);
	  return true;
	}
      else
	{
	  /* Todo: Figure out if we can trigger this, if it's worth to handle
	     optimally, and if we can handle it optimally.  */
	  return false;
	}
    }

  gcc_assert (TREE_CODE (nit) == SSA_NAME);

  /* Variable nit is the loop bound as returned by canonicalize_loop_ivs, for an
     iv with base 0 and step 1 that is incremented in the latch, like this:

     <bb header>:
     # iv_1 = PHI <0 (preheader), iv_2 (latch)>
     ...
     if (iv_1 < nit)
       goto <bb latch>;
     else
       goto <bb exit>;

     <bb latch>:
     iv_2 = iv_1 + 1;
     goto <bb header>;

     The range of iv_1 is [0, nit].  The latch edge is taken for
     iv_1 == [0, nit - 1] and the exit edge is taken for iv_1 == nit.  So the
     number of latch executions is equal to nit.

     The function max_loop_iterations gives us the maximum number of latch
     executions, so it gives us the maximum value of nit.  */
  widest_int nit_max;
  if (!max_loop_iterations (loop, &nit_max))
    return false;

  /* Check if nit + 1 overflows.  */
  widest_int type_max = wi::to_widest (TYPE_MAXVAL (nit_type));
  if (!wi::lts_p (nit_max, type_max))
    return false;

  gimple def = SSA_NAME_DEF_STMT (nit);

  /* Try to find nit + 1, in the form of n in an assignment nit = n - 1.  */
  if (def
      && is_gimple_assign (def)
      && gimple_assign_rhs_code (def) == PLUS_EXPR)
    {
      tree op1 = gimple_assign_rhs1 (def);
      tree op2 = gimple_assign_rhs2 (def);
      if (integer_minus_onep (op1))
	alt_bound = op2;
      else if (integer_minus_onep (op2))
	alt_bound = op1;
    }

  if (alt_bound == NULL_TREE)
    return false;

  transform_to_exit_first_loop_alt (loop, reduction_list, alt_bound);
  return true;
}

/* Moves the exit condition of LOOP to the beginning of its header.  NIT is the
   number of iterations of the loop.  REDUCTION_LIST describes the reductions in
   LOOP.  */

static void
transform_to_exit_first_loop (struct loop *loop,
			      reduction_info_table_type *reduction_list,
			      tree nit)
{
  basic_block *bbs, *nbbs, ex_bb, orig_header;
  unsigned n;
  bool ok;
  edge exit = single_dom_exit (loop), hpred;
  tree control, control_name, res, t;
  gphi *phi, *nphi;
  gassign *stmt;
  gcond *cond_stmt, *cond_nit;
  tree nit_1;

  split_block_after_labels (loop->header);
  orig_header = single_succ (loop->header);
  hpred = single_succ_edge (loop->header);

  cond_stmt = as_a <gcond *> (last_stmt (exit->src));
  control = gimple_cond_lhs (cond_stmt);
  gcc_assert (gimple_cond_rhs (cond_stmt) == nit);

  /* Make sure that we have phi nodes on exit for all loop header phis
     (create_parallel_loop requires that).  */
  for (gphi_iterator gsi = gsi_start_phis (loop->header);
       !gsi_end_p (gsi);
       gsi_next (&gsi))
    {
      phi = gsi.phi ();
      res = PHI_RESULT (phi);
      t = copy_ssa_name (res, phi);
      SET_PHI_RESULT (phi, t);
      nphi = create_phi_node (res, orig_header);
      add_phi_arg (nphi, t, hpred, UNKNOWN_LOCATION);

      if (res == control)
	{
	  gimple_cond_set_lhs (cond_stmt, t);
	  update_stmt (cond_stmt);
	  control = t;
	}
    }

  bbs = get_loop_body_in_dom_order (loop);

  for (n = 0; bbs[n] != exit->src; n++)
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
  exit = single_dom_exit (loop);
  control_name = NULL_TREE;
  for (gphi_iterator gsi = gsi_start_phis (ex_bb);
       !gsi_end_p (gsi); )
    {
      phi = gsi.phi ();
      res = PHI_RESULT (phi);
      if (virtual_operand_p (res))
	{
	  gsi_next (&gsi);
	  continue;
	}

      /* Check if it is a part of reduction.  If it is,
         keep the phi at the reduction's keep_res field.  The
         PHI_RESULT of this phi is the resulting value of the reduction
         variable when exiting the loop.  */

      if (reduction_list->elements () > 0)
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
  gimple_stmt_iterator gsi = gsi_after_labels (ex_bb);
  cond_nit = as_a <gcond *> (last_stmt (exit->src));
  nit_1 =  gimple_cond_rhs (cond_nit);
  nit_1 = force_gimple_operand_gsi (&gsi,
				  fold_convert (TREE_TYPE (control_name), nit_1),
				  false, NULL_TREE, false, GSI_SAME_STMT);
  stmt = gimple_build_assign (control_name, nit_1);
  gsi_insert_before (&gsi, stmt, GSI_NEW_STMT);
}

/* Create the parallel constructs for LOOP as described in gen_parallel_loop.
   LOOP_FN and DATA are the arguments of GIMPLE_OMP_PARALLEL.
   NEW_DATA is the variable that should be initialized from the argument
   of LOOP_FN.  N_THREADS is the requested number of threads.  Returns the
   basic block containing GIMPLE_OMP_PARALLEL tree.  */

static basic_block
create_parallel_loop (struct loop *loop, tree loop_fn, tree data,
		      tree new_data, unsigned n_threads, location_t loc)
{
  gimple_stmt_iterator gsi;
  basic_block bb, paral_bb, for_bb, ex_bb;
  tree t, param;
  gomp_parallel *omp_par_stmt;
  gimple omp_return_stmt1, omp_return_stmt2;
  gimple phi;
  gcond *cond_stmt;
  gomp_for *for_stmt;
  gomp_continue *omp_cont_stmt;
  tree cvar, cvar_init, initvar, cvar_next, cvar_base, type;
  edge exit, nexit, guard, end, e;

  /* Prepare the GIMPLE_OMP_PARALLEL statement.  */
  bb = loop_preheader_edge (loop)->src;
  paral_bb = single_pred (bb);
  gsi = gsi_last_bb (paral_bb);

  t = build_omp_clause (loc, OMP_CLAUSE_NUM_THREADS);
  OMP_CLAUSE_NUM_THREADS_EXPR (t)
    = build_int_cst (integer_type_node, n_threads);
  omp_par_stmt = gimple_build_omp_parallel (NULL, t, loop_fn, data);
  gimple_set_location (omp_par_stmt, loc);

  gsi_insert_after (&gsi, omp_par_stmt, GSI_NEW_STMT);

  /* Initialize NEW_DATA.  */
  if (data)
    {
      gassign *assign_stmt;

      gsi = gsi_after_labels (bb);

      param = make_ssa_name (DECL_ARGUMENTS (loop_fn));
      assign_stmt = gimple_build_assign (param, build_fold_addr_expr (data));
      gsi_insert_before (&gsi, assign_stmt, GSI_SAME_STMT);

      assign_stmt = gimple_build_assign (new_data,
				  fold_convert (TREE_TYPE (new_data), param));
      gsi_insert_before (&gsi, assign_stmt, GSI_SAME_STMT);
    }

  /* Emit GIMPLE_OMP_RETURN for GIMPLE_OMP_PARALLEL.  */
  bb = split_loop_exit_edge (single_dom_exit (loop));
  gsi = gsi_last_bb (bb);
  omp_return_stmt1 = gimple_build_omp_return (false);
  gimple_set_location (omp_return_stmt1, loc);
  gsi_insert_after (&gsi, omp_return_stmt1, GSI_NEW_STMT);

  /* Extract data for GIMPLE_OMP_FOR.  */
  gcc_assert (loop->header == single_dom_exit (loop)->src);
  cond_stmt = as_a <gcond *> (last_stmt (loop->header));

  cvar = gimple_cond_lhs (cond_stmt);
  cvar_base = SSA_NAME_VAR (cvar);
  phi = SSA_NAME_DEF_STMT (cvar);
  cvar_init = PHI_ARG_DEF_FROM_EDGE (phi, loop_preheader_edge (loop));
  initvar = copy_ssa_name (cvar);
  SET_USE (PHI_ARG_DEF_PTR_FROM_EDGE (phi, loop_preheader_edge (loop)),
	   initvar);
  cvar_next = PHI_ARG_DEF_FROM_EDGE (phi, loop_latch_edge (loop));

  gsi = gsi_last_nondebug_bb (loop->latch);
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
  for (gphi_iterator gpi = gsi_start_phis (ex_bb);
       !gsi_end_p (gpi); gsi_next (&gpi))
    {
      source_location locus;
      tree def;
      gphi *phi = gpi.phi ();
      gphi *stmt;

      stmt = as_a <gphi *> (
	       SSA_NAME_DEF_STMT (PHI_ARG_DEF_FROM_EDGE (phi, exit)));

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
  t = build_omp_clause (loc, OMP_CLAUSE_SCHEDULE);
  OMP_CLAUSE_SCHEDULE_KIND (t) = OMP_CLAUSE_SCHEDULE_STATIC;

  for_stmt = gimple_build_omp_for (NULL, GF_OMP_FOR_KIND_FOR, t, 1, NULL);
  gimple_set_location (for_stmt, loc);
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
  omp_cont_stmt = gimple_build_omp_continue (cvar_next, cvar);
  gimple_set_location (omp_cont_stmt, loc);
  gsi_insert_after (&gsi, omp_cont_stmt, GSI_NEW_STMT);
  SSA_NAME_DEF_STMT (cvar_next) = omp_cont_stmt;

  /* Emit GIMPLE_OMP_RETURN for GIMPLE_OMP_FOR.  */
  gsi = gsi_last_bb (ex_bb);
  omp_return_stmt2 = gimple_build_omp_return (true);
  gimple_set_location (omp_return_stmt2, loc);
  gsi_insert_after (&gsi, omp_return_stmt2, GSI_NEW_STMT);

  /* After the above dom info is hosed.  Re-compute it.  */
  free_dominance_info (CDI_DOMINATORS);
  calculate_dominance_info (CDI_DOMINATORS);

  return paral_bb;
}

/* Generates code to execute the iterations of LOOP in N_THREADS
   threads in parallel.

   NITER describes number of iterations of LOOP.
   REDUCTION_LIST describes the reductions existent in the LOOP.  */

static void
gen_parallel_loop (struct loop *loop,
		   reduction_info_table_type *reduction_list,
		   unsigned n_threads, struct tree_niter_desc *niter)
{
  tree many_iterations_cond, type, nit;
  tree arg_struct, new_arg_struct;
  gimple_seq stmts;
  edge entry, exit;
  struct clsn_data clsn_data;
  unsigned prob;
  location_t loc;
  gimple cond_stmt;
  unsigned int m_p_thread=2;

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

  /* We should compute a better number-of-iterations value for outer loops.
     That is, if we have
 
    for (i = 0; i < n; ++i)
      for (j = 0; j < m; ++j)
        ...

    we should compute nit = n * m, not nit = n.  
    Also may_be_zero handling would need to be adjusted.  */

  type = TREE_TYPE (niter->niter);
  nit = force_gimple_operand (unshare_expr (niter->niter), &stmts, true,
			      NULL_TREE);
  if (stmts)
    gsi_insert_seq_on_edge_immediate (loop_preheader_edge (loop), stmts);

  if (loop->inner)
    m_p_thread=2;
  else
    m_p_thread=MIN_PER_THREAD;

   many_iterations_cond =
     fold_build2 (GE_EXPR, boolean_type_node,
                nit, build_int_cst (type, m_p_thread * n_threads));

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

  /* Ensure that the exit condition is the first statement in the loop.
     The common case is that latch of the loop is empty (apart from the
     increment) and immediately follows the loop exit test.  Attempt to move the
     entry of the loop directly before the exit check and increase the number of
     iterations of the loop by one.  */
  if (!try_transform_to_exit_first_loop_alt (loop, reduction_list, nit))
    {
      /* Fall back on the method that handles more cases, but duplicates the
	 loop body: move the exit condition of LOOP to the beginning of its
	 header, and duplicate the part of the last iteration that gets disabled
	 to the exit of the loop.  */
      transform_to_exit_first_loop (loop, reduction_list, nit);
    }

  /* Generate initializations for reductions.  */
  if (reduction_list->elements () > 0)
    reduction_list->traverse <struct loop *, initialize_reductions> (loop);

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
  loc = UNKNOWN_LOCATION;
  cond_stmt = last_stmt (loop->header);
  if (cond_stmt)
    loc = gimple_location (cond_stmt);
  create_parallel_loop (loop, create_loop_fn (loc), arg_struct,
			new_arg_struct, n_threads, loc);
  if (reduction_list->elements () > 0)
    create_call_for_reduction (loop, reduction_list, &clsn_data);

  scev_reset ();

  /* Cancel the loop (it is simpler to do it here rather than to teach the
     expander to do it).  */
  cancel_loop_tree (loop);

  /* Free loop bound estimations that could contain references to
     removed statements.  */
  FOR_EACH_LOOP (loop, 0)
    free_numbers_of_iterations_estimates_loop (loop);
}

/* Returns true when LOOP contains vector phi nodes.  */

static bool
loop_has_vector_phi_nodes (struct loop *loop ATTRIBUTE_UNUSED)
{
  unsigned i;
  basic_block *bbs = get_loop_body_in_dom_order (loop);
  gphi_iterator gsi;
  bool res = true;

  for (i = 0; i < loop->num_nodes; i++)
    for (gsi = gsi_start_phis (bbs[i]); !gsi_end_p (gsi); gsi_next (&gsi))
      if (TREE_CODE (TREE_TYPE (PHI_RESULT (gsi.phi ()))) == VECTOR_TYPE)
	goto end;

  res = false;
 end:
  free (bbs);
  return res;
}

/* Create a reduction_info struct, initialize it with REDUC_STMT
   and PHI, insert it to the REDUCTION_LIST.  */

static void
build_new_reduction (reduction_info_table_type *reduction_list,
		     gimple reduc_stmt, gphi *phi)
{
  reduction_info **slot;
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
  new_reduction->reduc_version = SSA_NAME_VERSION (gimple_phi_result (phi));
  new_reduction->reduction_code = gimple_assign_rhs_code (reduc_stmt);
  slot = reduction_list->find_slot (new_reduction, INSERT);
  *slot = new_reduction;
}

/* Callback for htab_traverse.  Sets gimple_uid of reduc_phi stmts.  */

int
set_reduc_phi_uids (reduction_info **slot, void *data ATTRIBUTE_UNUSED)
{
  struct reduction_info *const red = *slot;
  gimple_set_uid (red->reduc_phi, red->reduc_version);
  return 1;
}

/* Detect all reductions in the LOOP, insert them into REDUCTION_LIST.  */

static void
gather_scalar_reductions (loop_p loop, reduction_info_table_type *reduction_list)
{
  gphi_iterator gsi;
  loop_vec_info simple_loop_info;

  simple_loop_info = vect_analyze_loop_form (loop);

  for (gsi = gsi_start_phis (loop->header); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gphi *phi = gsi.phi ();
      affine_iv iv;
      tree res = PHI_RESULT (phi);
      bool double_reduc;

      if (virtual_operand_p (res))
	continue;

      if (!simple_iv (loop, loop, res, &iv, true)
	&& simple_loop_info)
	{
           gimple reduc_stmt = vect_force_simple_reduction (simple_loop_info,
							    phi, true,
							    &double_reduc);
	   if (reduc_stmt && !double_reduc)
              build_new_reduction (reduction_list, reduc_stmt, phi);
        }
    }
  destroy_loop_vec_info (simple_loop_info, true);

  /* As gimple_uid is used by the vectorizer in between vect_analyze_loop_form
     and destroy_loop_vec_info, we can set gimple_uid of reduc_phi stmts
     only now.  */
  reduction_list->traverse <void *, set_reduc_phi_uids> (NULL);
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
try_create_reduction_list (loop_p loop,
			   reduction_info_table_type *reduction_list)
{
  edge exit = single_dom_exit (loop);
  gphi_iterator gsi;

  gcc_assert (exit);

  gather_scalar_reductions (loop, reduction_list);


  for (gsi = gsi_start_phis (exit->dest); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gphi *phi = gsi.phi ();
      struct reduction_info *red;
      imm_use_iterator imm_iter;
      use_operand_p use_p;
      gimple reduc_phi;
      tree val = PHI_ARG_DEF_FROM_EDGE (phi, exit);

      if (!virtual_operand_p (val))
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
	  if (reduction_list->elements () == 0)
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file,
			 "  FAILED: it is not a part of reduction.\n");
	      return false;
	    }
	  reduc_phi = NULL;
	  FOR_EACH_IMM_USE_FAST (use_p, imm_iter, val)
	    {
	      if (!gimple_debug_bind_p (USE_STMT (use_p))
		  && flow_bb_inside_loop_p (loop, gimple_bb (USE_STMT (use_p))))
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
      gphi *phi = gsi.phi ();
      tree def = PHI_RESULT (phi);
      affine_iv iv;

      if (!virtual_operand_p (def) && !simple_iv (loop, loop, def, &iv, true))
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

static bool
parallelize_loops (void)
{
  unsigned n_threads = flag_tree_parallelize_loops;
  bool changed = false;
  struct loop *loop;
  struct tree_niter_desc niter_desc;
  struct obstack parloop_obstack;
  HOST_WIDE_INT estimated;
  source_location loop_loc;

  /* Do not parallelize loops in the functions created by parallelization.  */
  if (parallelized_function_p (cfun->decl))
    return false;
  if (cfun->has_nonlocal_label)
    return false;

  gcc_obstack_init (&parloop_obstack);
  reduction_info_table_type reduction_list (10);
  init_stmt_vec_info_vec ();

  FOR_EACH_LOOP (loop, 0)
    {
      reduction_list.empty ();
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

      estimated = estimated_stmt_executions_int (loop);
      if (estimated == -1)
	estimated = max_stmt_executions_int (loop);
      /* FIXME: Bypass this check as graphite doesn't update the
	 count and frequency correctly now.  */
      if (!flag_loop_parallelize_all
	  && ((estimated != -1
	       && estimated <= (HOST_WIDE_INT) n_threads * MIN_PER_THREAD)
	      /* Do not bother with loops in cold areas.  */
	      || optimize_loop_nest_for_size_p (loop)))
	continue;

      if (!try_get_loop_niter (loop, &niter_desc))
	continue;

      if (!try_create_reduction_list (loop, &reduction_list))
	continue;

      if (!flag_loop_parallelize_all
	  && !loop_parallel_p (loop, &parloop_obstack))
	continue;

      changed = true;
      if (dump_file && (dump_flags & TDF_DETAILS))
      {
	if (loop->inner)
	  fprintf (dump_file, "parallelizing outer loop %d\n",loop->header->index);
	else
	  fprintf (dump_file, "parallelizing inner loop %d\n",loop->header->index);
	loop_loc = find_loop_location (loop);
	if (loop_loc != UNKNOWN_LOCATION)
	  fprintf (dump_file, "\nloop at %s:%d: ",
		   LOCATION_FILE (loop_loc), LOCATION_LINE (loop_loc));
      }
      gen_parallel_loop (loop, &reduction_list,
			 n_threads, &niter_desc);
    }

  free_stmt_vec_info_vec ();
  obstack_free (&parloop_obstack, NULL);

  /* Parallelization will cause new function calls to be inserted through
     which local variables will escape.  Reset the points-to solution
     for ESCAPED.  */
  if (changed)
    pt_solution_reset (&cfun->gimple_df->escaped);

  return changed;
}

/* Parallelization.  */

namespace {

const pass_data pass_data_parallelize_loops =
{
  GIMPLE_PASS, /* type */
  "parloops", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  TV_TREE_PARALLELIZE_LOOPS, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_parallelize_loops : public gimple_opt_pass
{
public:
  pass_parallelize_loops (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_parallelize_loops, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) { return flag_tree_parallelize_loops > 1; }
  virtual unsigned int execute (function *);

}; // class pass_parallelize_loops

unsigned
pass_parallelize_loops::execute (function *fun)
{
  if (number_of_loops (fun) <= 1)
    return 0;

  if (parallelize_loops ())
    {
      fun->curr_properties &= ~(PROP_gimple_eomp);
      return TODO_update_ssa;
    }

  return 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_parallelize_loops (gcc::context *ctxt)
{
  return new pass_parallelize_loops (ctxt);
}
