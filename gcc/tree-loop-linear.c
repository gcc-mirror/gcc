/* Linear Loop transforms
   Copyright (C) 2003, 2004 Free Software Foundation, Inc.
   Contributed by Daniel Berlin <dberlin@dberlin.org>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */


#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "errors.h"
#include "ggc.h"
#include "tree.h"
#include "target.h"

#include "rtl.h"
#include "basic-block.h"
#include "diagnostic.h"
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
#include "varray.h"
#include "lambda.h"

/* Linear loop transforms include any composition of interchange,
   scaling, skewing, and reversal.  They are used to change the
   iteration order of loop nests in order to optimize data locality of
   traversals, or remove dependences that prevent
   parallelization/vectorization/etc.  

   TODO: Determine reuse vectors/matrix and use it to determine optimal
   transform matrix for locality purposes.
   TODO: Completion of partial transforms.  */

/* Gather statistics for loop interchange.  Initializes SUM the sum of
   all the data dependence distances carried by loop LOOP_NUMBER.
   NB_DEPS_NOT_CARRIED_BY_LOOP is initialized to the number of
   dependence relations for which the loop LOOP_NUMBER is not carrying
   any dependence.  */

static void
gather_interchange_stats (varray_type dependence_relations, 
			  unsigned int loop_number, 
			  unsigned int *sum, 
			  unsigned int *nb_deps_not_carried_by_loop)
{
  unsigned int i;

  *sum = 0;
  *nb_deps_not_carried_by_loop = 0;
  for (i = 0; i < VARRAY_ACTIVE_SIZE (dependence_relations); i++)
    {
      int dist;
      struct data_dependence_relation *ddr = 
	(struct data_dependence_relation *) 
	VARRAY_GENERIC_PTR (dependence_relations, i);

      if (DDR_ARE_DEPENDENT (ddr) == chrec_dont_know)
	{
	  /* Some constants will need tweaking, but not something that should
	     be user-accessible.  Thus, no --param.  */
	  *sum += 100;
	  continue;
	}

      /* When we know that there is no dependence, we know that there
	 is no reuse of the data.  */
      if (DDR_ARE_DEPENDENT (ddr) == chrec_known)
	{
	  /* Ditto on the no --param here */
	  *sum += 1000;
	  continue;
	}

      dist = DDR_DIST_VECT (ddr)[loop_number];
      if (dist == 0)
	*nb_deps_not_carried_by_loop++;
      else if (dist < 0)
	*sum += -dist;
      else
	*sum += dist;
    }
}

/* Apply to TRANS any loop interchange that minimize inner loop steps.
   DEPTH is the depth of the loop nest, and DEPENDENCE_RELATIONS is an array
   of dependence relations.
   Returns the new transform matrix.  The smaller the reuse vector
   distances in the inner loops, the fewer the cache misses.  */

static lambda_trans_matrix
try_interchange_loops (lambda_trans_matrix trans, 
		       unsigned int depth,		       
		       varray_type dependence_relations)
{
  unsigned int loop_i, loop_j;
  unsigned int steps_i, steps_j;
  unsigned int nb_deps_not_carried_by_i, nb_deps_not_carried_by_j;
  struct data_dependence_relation *ddr;

  /* When there is an unknown relation in the dependence_relations, we
     know that it is no worth looking at this loop nest: give up.  */
  ddr = (struct data_dependence_relation *) 
    VARRAY_GENERIC_PTR (dependence_relations, 0);
  if (ddr == NULL || DDR_ARE_DEPENDENT (ddr) == chrec_dont_know)
    return trans;
  
  /* LOOP_I is always the outer loop.  */
  for (loop_j = 1; loop_j < depth; loop_j++)
    for (loop_i = 0; loop_i < loop_j; loop_i++)
      {
	gather_interchange_stats (dependence_relations, loop_i, &steps_i, 
				  &nb_deps_not_carried_by_i);
	gather_interchange_stats (dependence_relations, loop_j, &steps_j, 
				  &nb_deps_not_carried_by_j);
	
	/* Heuristics for loop interchange profitability:
	   1. Inner loops should have smallest steps.
	   2. Inner loops should contain more dependence relations not
	   carried by the loop.
	*/
	if (steps_i < steps_j 
	    || nb_deps_not_carried_by_i > nb_deps_not_carried_by_j)
	  {
	    lambda_matrix_row_exchange (LTM_MATRIX (trans), loop_i, loop_j);
	
	    /* Validate the resulting matrix.  When the transformation
	       is not valid, reverse to the previous matrix.  
	       
	       FIXME: In this case of transformation it could be
	       faster to verify the validity of the interchange
	       without applying the transform to the matrix.  But for
	       the moment do it cleanly: this is just a prototype.  */
	    if (!lambda_transform_legal_p (trans, depth, dependence_relations))
	      lambda_matrix_row_exchange (LTM_MATRIX (trans), loop_i, loop_j);
	  }
      }
  
  return trans;
}

/* Perform a set of linear transforms on LOOPS.  */

void
linear_transform_loops (struct loops *loops)
{
  unsigned int i;

  for (i = 1; i < loops->num; i++)
    {
      unsigned int depth = 0;
      varray_type datarefs;
      varray_type dependence_relations;
      struct loop *loop_nest = loops->parray[i];
      struct loop *temp;
      VEC (tree) *oldivs;
      VEC (tree) *invariants;
      lambda_loopnest before, after;
      lambda_trans_matrix trans;
      bool problem = false;
      /* If it's not a loop nest, we don't want it.
         We also don't handle sibling loops properly, 
         which are loops of the following form:
         for (i = 0; i < 50; i++)
           {
             for (j = 0; j < 50; j++)
               {
	        ...
               }
           for (j = 0; j < 50; j++)
               {
                ...
               }
           } */
      if (!loop_nest->inner)
	continue;
      for (temp = loop_nest; temp; temp = temp->inner)
	{
	  flow_loop_scan (temp, LOOP_ALL);
	  /* If we have a sibling loop or multiple exit edges, jump ship.  */
	  if (temp->next || temp->num_exits != 1)
	    {
	      problem = true;
	      break;
	    }
	  depth ++;
	}
      if (problem)
	continue;

      /* Analyze data references and dependence relations using scev.  */      
 
      VARRAY_GENERIC_PTR_INIT (datarefs, 10, "datarefs");
      VARRAY_GENERIC_PTR_INIT (dependence_relations, 10,
			       "dependence_relations");
      
  
      compute_data_dependences_for_loop (depth, loop_nest,
					 &datarefs, &dependence_relations);
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  unsigned int j;
	  for (j = 0; j < VARRAY_ACTIVE_SIZE (dependence_relations); j++)
	    {
	      struct data_dependence_relation *ddr = 
		(struct data_dependence_relation *) 
		VARRAY_GENERIC_PTR (dependence_relations, j);

	      if (DDR_ARE_DEPENDENT (ddr) == NULL_TREE)
		{
		  fprintf (dump_file, "DISTANCE_V (");
		  print_lambda_vector (dump_file, DDR_DIST_VECT (ddr), 
				       loops->num);
		  fprintf (dump_file, ")\n");
		  fprintf (dump_file, "DIRECTION_V (");
		  print_lambda_vector (dump_file, DDR_DIR_VECT (ddr), 
				       loops->num);
		  fprintf (dump_file, ")\n");
		}
	    }
	  fprintf (dump_file, "\n\n");
	}
      /* Build the transformation matrix.  */
      trans = lambda_trans_matrix_new (depth, depth);
      lambda_matrix_id (LTM_MATRIX (trans), depth);
      trans = try_interchange_loops (trans, depth, dependence_relations);

      /* Check whether the transformation is legal.  */
      if (!lambda_transform_legal_p (trans, depth, dependence_relations))
	{
	  if (dump_file)
	    fprintf (dump_file, "Can't transform loop, transform is illegal:\n");
	  continue;
	}
      before = gcc_loopnest_to_lambda_loopnest (loop_nest, &oldivs, 
						&invariants);
      if (!before)
	continue;
            
      if (dump_file)
	{
	  fprintf (dump_file, "Before:\n");
	  print_lambda_loopnest (dump_file, before, 'i');
	}
  
      after = lambda_loopnest_transform (before, trans);
      if (dump_file)
	{
	  fprintf (dump_file, "After:\n");
	  print_lambda_loopnest (dump_file, after, 'u');
	}
      lambda_loopnest_to_gcc_loopnest (loop_nest, oldivs, invariants,
				       after, trans);
      oldivs = NULL;
      invariants = NULL;
      free_dependence_relations (dependence_relations);
      free_data_refs (datarefs);
    }
}
