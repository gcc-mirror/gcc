/* Rename SSA copies.
   Copyright (C) 2004, 2006, 2007, 2008, 2009, 2010, 2011
   Free Software Foundation, Inc.
   Contributed by Andrew MacLeod <amacleod@redhat.com>

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
#include "tree.h"
#include "gimple.h"
#include "flags.h"
#include "basic-block.h"
#include "function.h"
#include "tree-pretty-print.h"
#include "bitmap.h"
#include "tree-flow.h"
#include "gimple.h"
#include "tree-inline.h"
#include "timevar.h"
#include "hashtab.h"
#include "tree-dump.h"
#include "tree-ssa-live.h"
#include "tree-pass.h"
#include "langhooks.h"

/* The following routines implement the SSA copy renaming phase.

   This optimization looks for copies between 2 SSA_NAMES, either through a
   direct copy, or an implicit one via a PHI node result and its arguments.

   Each copy is examined to determine if it is possible to rename the base
   variable of one of the operands to the same variable as the other operand.
   i.e.
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

static bool
copy_rename_partition_coalesce (var_map map, tree var1, tree var2, FILE *debug)
{
  int p1, p2, p3;
  tree root1, root2;
  tree rep1, rep2;
  bool ign1, ign2, abnorm;

  gcc_assert (TREE_CODE (var1) == SSA_NAME);
  gcc_assert (TREE_CODE (var2) == SSA_NAME);

  register_ssa_partition (map, var1);
  register_ssa_partition (map, var2);

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

  gcc_assert (p1 != NO_PARTITION);
  gcc_assert (p2 != NO_PARTITION);

  rep1 = partition_to_var (map, p1);
  rep2 = partition_to_var (map, p2);
  root1 = SSA_NAME_VAR (rep1);
  root2 = SSA_NAME_VAR (rep2);

  if (p1 == p2)
    {
      if (debug)
	fprintf (debug, " : Already coalesced.\n");
      return false;
    }

  /* Don't coalesce if one of the variables occurs in an abnormal PHI.  */
  abnorm = (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (rep1)
	    || SSA_NAME_OCCURS_IN_ABNORMAL_PHI (rep2));
  if (abnorm)
    {
      if (debug)
	fprintf (debug, " : Abnormal PHI barrier.  No coalesce.\n");
      return false;
    }

  /* Partitions already have the same root, simply merge them.  */
  if (root1 == root2)
    {
      p1 = partition_union (map->var_partition, p1, p2);
      if (debug)
	fprintf (debug, " : Same root, coalesced --> P%d.\n", p1);
      return false;
    }

  /* Never attempt to coalesce 2 different parameters.  */
  if (TREE_CODE (root1) == PARM_DECL && TREE_CODE (root2) == PARM_DECL)
    {
      if (debug)
        fprintf (debug, " : 2 different PARM_DECLS. No coalesce.\n");
      return false;
    }

  if ((TREE_CODE (root1) == RESULT_DECL) != (TREE_CODE (root2) == RESULT_DECL))
    {
      if (debug)
        fprintf (debug, " : One root a RESULT_DECL. No coalesce.\n");
      return false;
    }

  ign1 = TREE_CODE (root1) == VAR_DECL && DECL_IGNORED_P (root1);
  ign2 = TREE_CODE (root2) == VAR_DECL && DECL_IGNORED_P (root2);

  /* Never attempt to coalesce 2 user variables unless one is an inline
     variable.  */
  if (!ign1 && !ign2)
    {
      if (DECL_FROM_INLINE (root2))
        ign2 = true;
      else if (DECL_FROM_INLINE (root1))
	ign1 = true;
      else
	{
	  if (debug)
	    fprintf (debug, " : 2 different USER vars. No coalesce.\n");
	  return false;
	}
    }

  /* If both values have default defs, we can't coalesce.  If only one has a
     tag, make sure that variable is the new root partition.  */
  if (gimple_default_def (cfun, root1))
    {
      if (gimple_default_def (cfun, root2))
	{
	  if (debug)
	    fprintf (debug, " : 2 default defs. No coalesce.\n");
	  return false;
	}
      else
        {
	  ign2 = true;
	  ign1 = false;
	}
    }
  else if (gimple_default_def (cfun, root2))
    {
      ign1 = true;
      ign2 = false;
    }

  /* Don't coalesce if the new chosen root variable would be read-only.
     If both ign1 && ign2, then the root var of the larger partition
     wins, so reject in that case if any of the root vars is TREE_READONLY.
     Otherwise reject only if the root var, on which replace_ssa_name_symbol
     will be called below, is readonly.  */
  if ((TREE_READONLY (root1) && ign2) || (TREE_READONLY (root2) && ign1))
    {
      if (debug)
	fprintf (debug, " : Readonly variable.  No coalesce.\n");
      return false;
    }

  /* Don't coalesce if the two variables aren't type compatible .  */
  if (!types_compatible_p (TREE_TYPE (root1), TREE_TYPE (root2))
      /* There is a disconnect between the middle-end type-system and
         VRP, avoid coalescing enum types with different bounds.  */
      || ((TREE_CODE (TREE_TYPE (root1)) == ENUMERAL_TYPE
	   || TREE_CODE (TREE_TYPE (root2)) == ENUMERAL_TYPE)
	  && TREE_TYPE (root1) != TREE_TYPE (root2)))
    {
      if (debug)
	fprintf (debug, " : Incompatible types.  No coalesce.\n");
      return false;
    }

  /* Merge the two partitions.  */
  p3 = partition_union (map->var_partition, p1, p2);

  /* Set the root variable of the partition to the better choice, if there is
     one.  */
  if (!ign2)
    replace_ssa_name_symbol (partition_to_var (map, p3), root2);
  else if (!ign1)
    replace_ssa_name_symbol (partition_to_var (map, p3), root1);

  if (debug)
    {
      fprintf (debug, " --> P%d ", p3);
      print_generic_expr (debug, SSA_NAME_VAR (partition_to_var (map, p3)),
			  TDF_SLIM);
      fprintf (debug, "\n");
    }
  return true;
}


/* This function will make a pass through the IL, and attempt to coalesce any
   SSA versions which occur in PHI's or copies.  Coalescing is accomplished by
   changing the underlying root variable of all coalesced version.  This will
   then cause the SSA->normal pass to attempt to coalesce them all to the same
   variable.  */

static unsigned int
rename_ssa_copies (void)
{
  var_map map;
  basic_block bb;
  gimple_stmt_iterator gsi;
  tree var, part_var;
  gimple stmt, phi;
  unsigned x;
  FILE *debug;
  bool updated = false;

  if (dump_file && (dump_flags & TDF_DETAILS))
    debug = dump_file;
  else
    debug = NULL;

  map = init_var_map (num_ssa_names);

  FOR_EACH_BB (bb)
    {
      /* Scan for real copies.  */
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  stmt = gsi_stmt (gsi);
	  if (gimple_assign_ssa_name_copy_p (stmt))
	    {
	      tree lhs = gimple_assign_lhs (stmt);
	      tree rhs = gimple_assign_rhs1 (stmt);

	      updated |= copy_rename_partition_coalesce (map, lhs, rhs, debug);
	    }
	}
    }

  FOR_EACH_BB (bb)
    {
      /* Treat PHI nodes as copies between the result and each argument.  */
      for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
        {
          size_t i;
	  tree res;

	  phi = gsi_stmt (gsi);
	  res = gimple_phi_result (phi);

	  /* Do not process virtual SSA_NAMES.  */
	  if (!is_gimple_reg (SSA_NAME_VAR (res)))
	    continue;

          for (i = 0; i < gimple_phi_num_args (phi); i++)
            {
              tree arg = gimple_phi_arg (phi, i)->def;
              if (TREE_CODE (arg) == SSA_NAME)
		updated |= copy_rename_partition_coalesce (map, res, arg, debug);
            }
        }
    }

  if (debug)
    dump_var_map (debug, map);

  /* Now one more pass to make all elements of a partition share the same
     root variable.  */

  for (x = 1; x < num_ssa_names; x++)
    {
      part_var = partition_to_var (map, x);
      if (!part_var)
        continue;
      var = ssa_name (x);
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
      replace_ssa_name_symbol (var, SSA_NAME_VAR (part_var));
    }

  delete_var_map (map);
  return updated ? TODO_remove_unused_locals : 0;
}

/* Return true if copy rename is to be performed.  */

static bool
gate_copyrename (void)
{
  return flag_tree_copyrename != 0;
}

struct gimple_opt_pass pass_rename_ssa_copies =
{
 {
  GIMPLE_PASS,
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
 }
};
