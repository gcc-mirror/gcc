/* Rename SSA copies.
   Copyright (C) 2004 Free Software Foundation, Inc.
   Contributed by Andrew MacLeod <amacleod@redhat.com>

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
#include "tree.h"
#include "flags.h"
#include "basic-block.h"
#include "function.h"
#include "diagnostic.h"
#include "bitmap.h"
#include "tree-flow.h"
#include "tree-gimple.h"
#include "tree-inline.h"
#include "timevar.h"
#include "tree-alias-common.h"
#include "hashtab.h"
#include "tree-dump.h"
#include "tree-ssa-live.h"
#include "tree-pass.h"

extern void rename_ssa_copies (void);

/* The following routines implement the SSA copy renaming phase.

   This optimization looks for copies between 2 SSA_NAMES, either through a
   direct copy, or an implicit one via a PHI node result and its arguments.

   Each copy is examined to determine if it is possible to rename the base
   variable of one of the operands to the same variable as the other operand.
   ie.
   T.3_5 = <blah>
   a_1 = T.3_5

   If this copy couldn't be copy propagated, it could possibly remain in the 
   program throughout the optimization phases.   After SSA->normal, it would 
   become:

   T.3 = <blah>
   a = T.3
   
   Since T.3_5 is distinct from all other SSA versions of T.3, there is no 
   fundamental reason why the base variable needs to be T.3, subject to 
   certain restrictions.  This optimization attempts to determine if we can 
   change the base variable on copies like this, and result in code such as:

   a_5 = <blah>
   a_1 = a_5

   This gives the SSA->normal pass a shot at coalescing a_1 and a_5. If it is 
   possible, the copy goes away completely. If it isn't possible, a new temp
   will be created for a_5, and you will end up with the exact same code:

   a.8 = <blah>
   a = a.8

   The other benefit of performing this optimization relates to what variables
   are chosen in copies.  Gimplification of the program uses temporaries for
   a lot of things. expressions like

   a_1 = <blah>
   <blah2> = a_1

   get turned into 
    
   T.3_5 = <blah>
   a_1 = T.3_5
   <blah2> = a_1

   Copy propagation is done in a forward direction, and if we can propagate
   through the copy, we end up with:

   T.3_5 = <blah>
   <blah2> = T.3_5

   The copy is gone, but so is all reference to the user variable 'a'. By
   performing this optimization, we would see the sequence:

   a_5 = <blah>
   a_1 = a_5
   <blah2> = a_1

   which copy propagation would then turn into:
   
   a_5 = <blah>
   <blah2> = a_5

   and so we still retain the user variable whenever possible.  */


/* Coalesce the partitions in MAP representing VAR1 and VAR2 if it is valid.
   Choose a representative for the partition, and send debug info to DEBUG.  */

static void
copy_rename_partition_coalesce (var_map map, tree var1, tree var2, FILE *debug)
{
  int p1, p2, p3;
  tree root1, root2;
  var_ann_t ann1, ann2, ann3;
  bool gimp1, gimp2;

#ifdef ENABLE_CHECKING
  if (TREE_CODE (var1) != SSA_NAME || TREE_CODE (var2) != SSA_NAME)
    abort ();
#endif

  register_ssa_partition (map, var1, false);
  register_ssa_partition (map, var2, true);

  p1 = partition_find (map->var_partition, SSA_NAME_VERSION (var1));
  p2 = partition_find (map->var_partition, SSA_NAME_VERSION (var2));

  if (debug)
    {
      fprintf (debug, "Try : ");
      print_generic_expr (debug, var1, TDF_SLIM);
      fprintf (debug, "(P%d) & ", p1);
      print_generic_expr (debug, var2, TDF_SLIM);
      fprintf (debug, "(P%d)", p2);
    }

#ifdef ENABLE_CHECKING
  if (p1 == NO_PARTITION || p2 == NO_PARTITION)
    abort ();
#endif

  root1 = SSA_NAME_VAR (partition_to_var (map, p1));
  root2 = SSA_NAME_VAR (partition_to_var (map, p2));

  if (DECL_HARD_REGISTER (root1) || DECL_HARD_REGISTER (root2))
    {
      if (debug)
        {
	  if (DECL_HARD_REGISTER (root1))
	    print_generic_expr (debug, var1, TDF_SLIM);
	  else
	    print_generic_expr (debug, var2, TDF_SLIM);
	  fprintf (debug, " is a hardware register.  No Coalescing.\n");
	}
      return;
    }

  ann1 = var_ann (root1);
  ann2 = var_ann (root2);

  if (p1 == p2)
    {
      if (debug)
	fprintf (debug, " : Already coalesced.\n");
      return;
    }

  /* Partitions already have the same root, simply merge them.  */
  if (root1 == root2)
    {
      p1 = partition_union (map->var_partition, p1, p2);
      if (debug)
	fprintf (debug, " : Same root, coalesced --> P%d.\n", p1);
      return;
    }

  /* Never attempt to coalesce 2 difference parameters.  */
  if (TREE_CODE (root1) == PARM_DECL && TREE_CODE (root2) == PARM_DECL)
    {
      if (debug)
        fprintf (debug, " : 2 different PARM_DECLS. No coalesce.\n");
      return;
    }

  if ((TREE_CODE (root1) == RESULT_DECL) != (TREE_CODE (root2) == RESULT_DECL))
    {
      if (debug)
        fprintf (debug, " : One root a RESULT_DECL. No coalesce.\n");
      return;
    }

  gimp1 = is_gimple_tmp_var (root1);
  gimp2 = is_gimple_tmp_var (root2);

  /* Never attempt to coalesce 2 user variables unless one is an inline 
     variable.  */
  if (!gimp1 && !gimp2)
    {
      if (DECL_FROM_INLINE (root2))
        gimp2 = true;
      else
        if (DECL_FROM_INLINE (root1))
	  gimp1 = true;
	else 
	  {
	    if (debug)
	      fprintf (debug, " : 2 different USER vars. No coalesce.\n");
	    return;
	  }
    }

    
  /* Don't coalesce if there are two different memory tags.  */
  if (ann1->type_mem_tag && ann2->type_mem_tag
      && ann1->type_mem_tag != ann2->type_mem_tag)
    {
      if (debug)
	fprintf (debug, " : 2 memory tags. No coalesce.\n");
      return;
    }

  /* If both values have default defs, we can't coalesce.  If only one has a 
     tag, make sure that variable is the new root partition.  */
  if (default_def (root1))
    {
      if (default_def (root2))
	{
	  if (debug)
	    fprintf (debug, " : 2 default defs. No coalesce.\n");
	  return;
	}
      else
        {
	  gimp2 = true;
	  gimp1 = false;
	}
    }
  else
    if (default_def (root2))
      {
	gimp1 = true;
	gimp2 = false;
      }

  /* Merge the two partitions.  */
  p3 = partition_union (map->var_partition, p1, p2);

  /* Set the root variable of the partition to the better choice, if there is 
     one.  */
  if (!gimp2)
    SSA_NAME_VAR (partition_to_var (map, p3)) = root2;
  else
    if (!gimp1)
      SSA_NAME_VAR (partition_to_var (map, p3)) = root1;

  /* Update the various flag widgitry of the current base representative.  */
  ann3 = var_ann (SSA_NAME_VAR (partition_to_var (map, p3)));
  if (ann1->type_mem_tag)
    ann3->type_mem_tag = ann1->type_mem_tag;
  else
    ann3->type_mem_tag = ann2->type_mem_tag;

  if (debug)
    {
      fprintf (debug, " --> P%d ", p3);
      print_generic_expr (debug, SSA_NAME_VAR (partition_to_var (map, p3)), 
			  TDF_SLIM);
      fprintf (debug, "\n");
    }
}


/* This function will make a pass through the IL, and attempt to coalesce any
   SSA versions which occur in PHI's or copies.  Coalescing is accomplished by
   changing the underlying root variable of all coalesced version.  This will 
   then cause the SSA->normal pass to attempt to coalesce them all to the same 
   variable.  */

void
rename_ssa_copies (void)
{
  var_map map;
  basic_block bb;
  block_stmt_iterator bsi;
  tree phi, stmt, var, part_var;
  unsigned x;
  FILE *debug;

  if (dump_file && (dump_flags & TDF_DETAILS))
    debug = dump_file;
  else
    debug = NULL;

  map = init_var_map (num_ssa_names + 1);

  FOR_EACH_BB (bb)
    {
      /* Scan for real copies.  */
      for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
	{
	  stmt = bsi_stmt (bsi); 
	  if (TREE_CODE (stmt) == MODIFY_EXPR)
	    {
	      tree lhs = TREE_OPERAND (stmt, 0);
	      tree rhs = TREE_OPERAND (stmt, 1);

              if (TREE_CODE (lhs) == SSA_NAME
		  && !has_hidden_use (SSA_NAME_VAR (lhs))
		  && TREE_CODE (rhs) == SSA_NAME)
		copy_rename_partition_coalesce (map, lhs, rhs, debug);
	    }
	}
    }

  FOR_EACH_BB (bb)
    {
      /* Treat PHI nodes as copies between the result and each argument.  */
      for (phi = phi_nodes (bb); phi; phi = TREE_CHAIN (phi))
        {
          int i;
	  tree res = PHI_RESULT (phi);

	  /* Do not process virtual SSA_NAMES or variables which have
	     hidden uses.  */
	  if (!is_gimple_reg (SSA_NAME_VAR (res))
	      || has_hidden_use (SSA_NAME_VAR (res)))
	    continue;

          for (i = 0; i < PHI_NUM_ARGS (phi); i++)
            {
              tree arg = PHI_ARG_DEF (phi, i);
              if (TREE_CODE (arg) == SSA_NAME)
		copy_rename_partition_coalesce (map, res, arg, debug);
            }
        }
    }

  if (debug)
    dump_var_map (debug, map);

  /* Now one more pass to make all elements of a partition share the same
     root variable.  */
  
  for (x = 1; x <= num_ssa_names; x++)
    {
      part_var = partition_to_var (map, x);
      if (!part_var)
        continue;
      var = map->partition_to_var[x];
      if (debug)
        {
	  if (SSA_NAME_VAR (var) != SSA_NAME_VAR (part_var))
	    {
	      fprintf (debug, "Coalesced ");
	      print_generic_expr (debug, var, TDF_SLIM);
	      fprintf (debug, " to ");
	      print_generic_expr (debug, part_var, TDF_SLIM);
	      fprintf (debug, "\n");
	    }
	}
      SSA_NAME_VAR (var) = SSA_NAME_VAR (part_var);
    }

  delete_var_map (map);
}

/* Return true if copy rename is to be performed.  */

static bool
gate_copyrename (void)
{
  return flag_tree_copyrename != 0;
}

struct tree_opt_pass pass_rename_ssa_copies = 
{  
  "copyrename",				/* name */
  gate_copyrename,			/* gate */
  rename_ssa_copies,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_COPY_RENAME,			/* tv_id */
  PROP_cfg | PROP_ssa,			/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */ 
  TODO_dump_func | TODO_verify_ssa      /* todo_flags_finish */
}; 

