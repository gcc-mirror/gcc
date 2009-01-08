/* Linear Loop transforms
   Copyright (C) 2003, 2004, 2005, 2007, 2008, 2009
   Free Software Foundation, Inc.
   Contributed by Daniel Berlin <dberlin@dberlin.org>.

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
#include "ggc.h"
#include "tree.h"
#include "target.h"

#include "rtl.h"
#include "basic-block.h"
#include "diagnostic.h"
#include "obstack.h"
#include "tree-flow.h"
#include "tree-dump.h"
#include "timevar.h"
#include "cfgloop.h"
#include "expr.h"
#include "optabs.h"
#include "tree-chrec.h"
#include "tree-data-ref.h"
#include "tree-scalar-evolution.h"
#include "tree-pass.h"
#include "lambda.h"

/* Linear loop transforms include any composition of interchange,
   scaling, skewing, and reversal.  They are used to change the
   iteration order of loop nests in order to optimize data locality of
   traversals, or remove dependences that prevent
   parallelization/vectorization/etc.  

   TODO: Determine reuse vectors/matrix and use it to determine optimal
   transform matrix for locality purposes.
   TODO: Completion of partial transforms.  */

/* Gather statistics for loop interchange.  LOOP is the loop being
   considered. The first loop in the considered loop nest is
   FIRST_LOOP, and consequently, the index of the considered loop is
   obtained by LOOP->DEPTH - FIRST_LOOP->DEPTH
   
   Initializes:
   - DEPENDENCE_STEPS the sum of all the data dependence distances
   carried by loop LOOP,

   - NB_DEPS_NOT_CARRIED_BY_LOOP the number of dependence relations
   for which the loop LOOP is not carrying any dependence,

   - ACCESS_STRIDES the sum of all the strides in LOOP.

   Example: for the following loop,

   | loop_1 runs 1335 times
   |   loop_2 runs 1335 times
   |     A[{{0, +, 1}_1, +, 1335}_2]
   |     B[{{0, +, 1}_1, +, 1335}_2]
   |   endloop_2
   |   A[{0, +, 1336}_1]
   | endloop_1

   gather_interchange_stats (in loop_1) will return 
   DEPENDENCE_STEPS = 3002
   NB_DEPS_NOT_CARRIED_BY_LOOP = 5
   ACCESS_STRIDES = 10694

   gather_interchange_stats (in loop_2) will return 
   DEPENDENCE_STEPS = 3000
   NB_DEPS_NOT_CARRIED_BY_LOOP = 7
   ACCESS_STRIDES = 8010
*/

static void
gather_interchange_stats (VEC (ddr_p, heap) *dependence_relations ATTRIBUTE_UNUSED,
			  VEC (data_reference_p, heap) *datarefs ATTRIBUTE_UNUSED,
			  struct loop *loop ATTRIBUTE_UNUSED,
			  struct loop *first_loop ATTRIBUTE_UNUSED,
			  unsigned int *dependence_steps ATTRIBUTE_UNUSED, 
			  unsigned int *nb_deps_not_carried_by_loop ATTRIBUTE_UNUSED, 
			  double_int *access_strides ATTRIBUTE_UNUSED)
{
  unsigned int i, j;
  struct data_dependence_relation *ddr;
  struct data_reference *dr;

  *dependence_steps = 0;
  *nb_deps_not_carried_by_loop = 0;
  *access_strides = double_int_zero;

  for (i = 0; VEC_iterate (ddr_p, dependence_relations, i, ddr); i++)
    {
      /* If we don't know anything about this dependence, or the distance
	 vector is NULL, or there is no dependence, then there is no reuse of
	 data.  */
      if (DDR_ARE_DEPENDENT (ddr) == chrec_dont_know
	  || DDR_ARE_DEPENDENT (ddr) == chrec_known
	  || DDR_NUM_DIST_VECTS (ddr) == 0)
	continue;

      for (j = 0; j < DDR_NUM_DIST_VECTS (ddr); j++)
	{
	  int dist = DDR_DIST_VECT (ddr, j)[loop_depth (loop) - loop_depth (first_loop)];

	  if (dist == 0)
	    (*nb_deps_not_carried_by_loop) += 1;

	  else if (dist < 0)
	    (*dependence_steps) += -dist;

	  else
	    (*dependence_steps) += dist;
	}
    }

  /* Compute the access strides.  */
  for (i = 0; VEC_iterate (data_reference_p, datarefs, i, dr); i++)
    {
      unsigned int it;
      tree ref = DR_REF (dr);
      gimple stmt = DR_STMT (dr);
      struct loop *stmt_loop = loop_containing_stmt (stmt);
      struct loop *inner_loop = first_loop->inner;

      if (inner_loop != stmt_loop 
	  && !flow_loop_nested_p (inner_loop, stmt_loop))
	continue;

      for (it = 0; it < DR_NUM_DIMENSIONS (dr); 
	   it++, ref = TREE_OPERAND (ref, 0))
	{
	  int num = am_vector_index_for_loop (DR_ACCESS_MATRIX (dr), loop->num);
	  int istride = AM_GET_ACCESS_MATRIX_ELEMENT (DR_ACCESS_MATRIX (dr), it, num);
	  tree array_size = TYPE_SIZE (TREE_TYPE (ref));
	  double_int dstride;

	  if (array_size == NULL_TREE 
	      || TREE_CODE (array_size) != INTEGER_CST)
	    continue;

	  dstride = double_int_mul (tree_to_double_int (array_size), 
				    shwi_to_double_int (istride));
	  (*access_strides) = double_int_add (*access_strides, dstride);
	}
    }
}

/* Attempt to apply interchange transformations to TRANS to maximize the
   spatial and temporal locality of the loop.  
   Returns the new transform matrix.  The smaller the reuse vector
   distances in the inner loops, the fewer the cache misses.
   FIRST_LOOP is the loop->num of the first loop in the analyzed loop
   nest.  */


static lambda_trans_matrix
try_interchange_loops (lambda_trans_matrix trans, 
		       unsigned int depth,		       
		       VEC (ddr_p, heap) *dependence_relations,
		       VEC (data_reference_p, heap) *datarefs,
		       struct loop *first_loop)
{
  bool res;
  struct loop *loop_i;
  struct loop *loop_j;
  unsigned int dependence_steps_i, dependence_steps_j;
  double_int access_strides_i, access_strides_j;
  double_int small, large, nb_iter;
  double_int l1_cache_size, l2_cache_size;
  int cmp;
  unsigned int nb_deps_not_carried_by_i, nb_deps_not_carried_by_j;
  struct data_dependence_relation *ddr;

  if (VEC_length (ddr_p, dependence_relations) == 0)
    return trans;

  /* When there is an unknown relation in the dependence_relations, we
     know that it is no worth looking at this loop nest: give up.  */
  ddr = VEC_index (ddr_p, dependence_relations, 0);
  if (ddr == NULL || DDR_ARE_DEPENDENT (ddr) == chrec_dont_know)
    return trans;

  l1_cache_size = uhwi_to_double_int (L1_CACHE_SIZE * 1024);
  l2_cache_size = uhwi_to_double_int (L2_CACHE_SIZE * 1024);

  /* LOOP_I is always the outer loop.  */
  for (loop_j = first_loop->inner; 
       loop_j; 
       loop_j = loop_j->inner)
    for (loop_i = first_loop; 
	 loop_depth (loop_i) < loop_depth (loop_j); 
	 loop_i = loop_i->inner)
      {
	gather_interchange_stats (dependence_relations, datarefs,
				  loop_i, first_loop,
				  &dependence_steps_i, 
				  &nb_deps_not_carried_by_i,
				  &access_strides_i);
	gather_interchange_stats (dependence_relations, datarefs,
				  loop_j, first_loop,
				  &dependence_steps_j, 
				  &nb_deps_not_carried_by_j, 
				  &access_strides_j);
	
	/* Heuristics for loop interchange profitability:

	   0. Don't transform if the smallest stride is larger than
	      the L2 cache, or if the largest stride multiplied by the
	      number of iterations is smaller than the L1 cache.

	   1. (spatial locality) Inner loops should have smallest
              dependence steps.

	   2. (spatial locality) Inner loops should contain more
	   dependence relations not carried by the loop.

	   3. (temporal locality) Inner loops should have smallest
	      array access strides.
	*/

	cmp = double_int_ucmp (access_strides_i, access_strides_j);
	small = cmp < 0 ? access_strides_i : access_strides_j;
	large = cmp < 0 ? access_strides_j : access_strides_i;

	if (double_int_ucmp (small, l2_cache_size) > 0)
	  continue;

	res = cmp < 0 ?
	  estimated_loop_iterations (loop_j, false, &nb_iter):
	  estimated_loop_iterations (loop_i, false, &nb_iter);
	large = double_int_mul (large, nb_iter);

	if (res && double_int_ucmp (large, l1_cache_size) < 0)
	  continue;

	if (dependence_steps_i < dependence_steps_j 
	    || nb_deps_not_carried_by_i > nb_deps_not_carried_by_j
	    || cmp < 0)
	  {
	    lambda_matrix_row_exchange (LTM_MATRIX (trans),
					loop_depth (loop_i) - loop_depth (first_loop),
					loop_depth (loop_j) - loop_depth (first_loop));
	    /* Validate the resulting matrix.  When the transformation
	       is not valid, reverse to the previous transformation.  */
	    if (!lambda_transform_legal_p (trans, depth, dependence_relations))
	      lambda_matrix_row_exchange (LTM_MATRIX (trans), 
					  loop_depth (loop_i) - loop_depth (first_loop), 
					  loop_depth (loop_j) - loop_depth (first_loop));
	  }
      }

  return trans;
}

/* Return the number of nested loops in LOOP_NEST, or 0 if the loops
   are not perfectly nested.  */

unsigned int
perfect_loop_nest_depth (struct loop *loop_nest)
{
  struct loop *temp;
  unsigned int depth = 1;

  /* If it's not a loop nest, we don't want it.  We also don't handle
     sibling loops properly, which are loops of the following form:

     | for (i = 0; i < 50; i++)
     |   {
     |     for (j = 0; j < 50; j++)
     |       {
     |        ...
     |       }
     |     for (j = 0; j < 50; j++)
     |       {
     |        ...
     |       }
     |   }
  */

  if (!loop_nest->inner || !single_exit (loop_nest))
    return 0;

  for (temp = loop_nest->inner; temp; temp = temp->inner)
    {
      /* If we have a sibling loop or multiple exit edges, jump ship.  */
      if (temp->next || !single_exit (temp))
	return 0;

      depth++;
    }

  return depth;
}

/* Perform a set of linear transforms on loops.  */

void
linear_transform_loops (void)
{
  bool modified = false;
  loop_iterator li;
  VEC(tree,heap) *oldivs = NULL;
  VEC(tree,heap) *invariants = NULL;
  VEC(tree,heap) *lambda_parameters = NULL;
  VEC(gimple,heap) *remove_ivs = VEC_alloc (gimple, heap, 3);
  struct loop *loop_nest;
  gimple oldiv_stmt;
  unsigned i;

  FOR_EACH_LOOP (li, loop_nest, 0)
    {
      unsigned int depth = 0;
      VEC (ddr_p, heap) *dependence_relations;
      VEC (data_reference_p, heap) *datarefs;
      
      lambda_loopnest before, after;
      lambda_trans_matrix trans;
      struct obstack lambda_obstack;
      struct loop *loop;
      VEC(loop_p,heap) *nest;

      depth = perfect_loop_nest_depth (loop_nest);
      if (depth == 0)
	continue;

      nest = VEC_alloc (loop_p, heap, 3);
      for (loop = loop_nest; loop; loop = loop->inner)
	VEC_safe_push (loop_p, heap, nest, loop);

      gcc_obstack_init (&lambda_obstack);
      VEC_truncate (tree, oldivs, 0);
      VEC_truncate (tree, invariants, 0);
      VEC_truncate (tree, lambda_parameters, 0);

      datarefs = VEC_alloc (data_reference_p, heap, 10);
      dependence_relations = VEC_alloc (ddr_p, heap, 10 * 10);
      if (!compute_data_dependences_for_loop (loop_nest, true, &datarefs,
					      &dependence_relations))
	goto free_and_continue;
      
      lambda_collect_parameters (datarefs, &lambda_parameters);
      if (!lambda_compute_access_matrices (datarefs, lambda_parameters, nest))
	goto free_and_continue;

      if (dump_file && (dump_flags & TDF_DETAILS))
	dump_ddrs (dump_file, dependence_relations);

      /* Build the transformation matrix.  */
      trans = lambda_trans_matrix_new (depth, depth);
      lambda_matrix_id (LTM_MATRIX (trans), depth);
      trans = try_interchange_loops (trans, depth, dependence_relations,
				     datarefs, loop_nest);

      if (lambda_trans_matrix_id_p (trans))
	{
	  if (dump_file)
	   fprintf (dump_file, "Won't transform loop. Optimal transform is the identity transform\n");
	  goto free_and_continue;
	}

      /* Check whether the transformation is legal.  */
      if (!lambda_transform_legal_p (trans, depth, dependence_relations))
	{
	  if (dump_file)
	    fprintf (dump_file, "Can't transform loop, transform is illegal:\n");
	  goto free_and_continue;
	}

      before = gcc_loopnest_to_lambda_loopnest (loop_nest, &oldivs,
                                                &invariants, &lambda_obstack);

      if (!before)
	goto free_and_continue;

      if (dump_file)
	{
	  fprintf (dump_file, "Before:\n");
	  print_lambda_loopnest (dump_file, before, 'i');
	}
  
      after = lambda_loopnest_transform (before, trans, &lambda_obstack);

      if (dump_file)
	{
	  fprintf (dump_file, "After:\n");
	  print_lambda_loopnest (dump_file, after, 'u');
	}

      lambda_loopnest_to_gcc_loopnest (loop_nest, oldivs, invariants,
				       &remove_ivs,
                                       after, trans, &lambda_obstack);
      modified = true;

      if (dump_file)
	fprintf (dump_file, "Successfully transformed loop.\n");

    free_and_continue:
      obstack_free (&lambda_obstack, NULL);
      free_dependence_relations (dependence_relations);
      free_data_refs (datarefs);
      VEC_free (loop_p, heap, nest);
    }

  for (i = 0; VEC_iterate (gimple, remove_ivs, i, oldiv_stmt); i++)
    remove_iv (oldiv_stmt);

  VEC_free (tree, heap, oldivs);
  VEC_free (tree, heap, invariants);
  VEC_free (gimple, heap, remove_ivs);
  scev_reset ();

  if (modified)
    rewrite_into_loop_closed_ssa (NULL, TODO_update_ssa_full_phi);
}
