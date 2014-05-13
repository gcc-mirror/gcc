/* Rename SSA copies.
   Copyright (C) 2004-2014 Free Software Foundation, Inc.
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
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "flags.h"
#include "function.h"
#include "tree-pretty-print.h"
#include "bitmap.h"
#include "gimple-ssa.h"
#include "stringpool.h"
#include "tree-ssanames.h"
#include "expr.h"
#include "tree-dfa.h"
#include "tree-inline.h"
#include "hashtab.h"
#include "tree-ssa-live.h"
#include "tree-pass.h"
#include "langhooks.h"

static struct
{
  /* Number of copies coalesced.  */
  int coalesced;
} stats;

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

static void
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

  if (p1 == p2)
    {
      if (debug)
	fprintf (debug, " : Already coalesced.\n");
      return;
    }

  rep1 = partition_to_var (map, p1);
  rep2 = partition_to_var (map, p2);
  root1 = SSA_NAME_VAR (rep1);
  root2 = SSA_NAME_VAR (rep2);
  if (!root1 && !root2)
    return;

  /* Don't coalesce if one of the variables occurs in an abnormal PHI.  */
  abnorm = (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (rep1)
	    || SSA_NAME_OCCURS_IN_ABNORMAL_PHI (rep2));
  if (abnorm)
    {
      if (debug)
	fprintf (debug, " : Abnormal PHI barrier.  No coalesce.\n");
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

  /* Never attempt to coalesce 2 different parameters.  */
  if ((root1 && TREE_CODE (root1) == PARM_DECL)
      && (root2 && TREE_CODE (root2) == PARM_DECL))
    {
      if (debug)
        fprintf (debug, " : 2 different PARM_DECLS. No coalesce.\n");
      return;
    }

  if ((root1 && TREE_CODE (root1) == RESULT_DECL)
      != (root2 && TREE_CODE (root2) == RESULT_DECL))
    {
      if (debug)
        fprintf (debug, " : One root a RESULT_DECL. No coalesce.\n");
      return;
    }

  ign1 = !root1 || (TREE_CODE (root1) == VAR_DECL && DECL_IGNORED_P (root1));
  ign2 = !root2 || (TREE_CODE (root2) == VAR_DECL && DECL_IGNORED_P (root2));

  /* Refrain from coalescing user variables, if requested.  */
  if (!ign1 && !ign2)
    {
      if (flag_ssa_coalesce_vars && DECL_FROM_INLINE (root2))
	ign2 = true;
      else if (flag_ssa_coalesce_vars && DECL_FROM_INLINE (root1))
	ign1 = true;
      else if (flag_ssa_coalesce_vars != 2)
	{
	  if (debug)
	    fprintf (debug, " : 2 different USER vars. No coalesce.\n");
	  return;
	}
      else
	ign2 = true;
    }

  /* If both values have default defs, we can't coalesce.  If only one has a
     tag, make sure that variable is the new root partition.  */
  if (root1 && ssa_default_def (cfun, root1))
    {
      if (root2 && ssa_default_def (cfun, root2))
	{
	  if (debug)
	    fprintf (debug, " : 2 default defs. No coalesce.\n");
	  return;
	}
      else
        {
	  ign2 = true;
	  ign1 = false;
	}
    }
  else if (root2 && ssa_default_def (cfun, root2))
    {
      ign1 = true;
      ign2 = false;
    }

  /* Do not coalesce if we cannot assign a symbol to the partition.  */
  if (!(!ign2 && root2)
      && !(!ign1 && root1))
    {
      if (debug)
	fprintf (debug, " : Choosen variable has no root.  No coalesce.\n");
      return;
    }

  /* Don't coalesce if the new chosen root variable would be read-only.
     If both ign1 && ign2, then the root var of the larger partition
     wins, so reject in that case if any of the root vars is TREE_READONLY.
     Otherwise reject only if the root var, on which replace_ssa_name_symbol
     will be called below, is readonly.  */
  if (((root1 && TREE_READONLY (root1)) && ign2)
      || ((root2 && TREE_READONLY (root2)) && ign1))
    {
      if (debug)
	fprintf (debug, " : Readonly variable.  No coalesce.\n");
      return;
    }

  /* Don't coalesce if the two variables aren't type compatible .  */
  if (!types_compatible_p (TREE_TYPE (var1), TREE_TYPE (var2))
      /* There is a disconnect between the middle-end type-system and
         VRP, avoid coalescing enum types with different bounds.  */
      || ((TREE_CODE (TREE_TYPE (var1)) == ENUMERAL_TYPE
	   || TREE_CODE (TREE_TYPE (var2)) == ENUMERAL_TYPE)
	  && TREE_TYPE (var1) != TREE_TYPE (var2)))
    {
      if (debug)
	fprintf (debug, " : Incompatible types.  No coalesce.\n");
      return;
    }

  /* Merge the two partitions.  */
  p3 = partition_union (map->var_partition, p1, p2);

  /* Set the root variable of the partition to the better choice, if there is
     one.  */
  if (!ign2 && root2)
    replace_ssa_name_symbol (partition_to_var (map, p3), root2);
  else if (!ign1 && root1)
    replace_ssa_name_symbol (partition_to_var (map, p3), root1);
  else
    gcc_unreachable ();

  if (debug)
    {
      fprintf (debug, " --> P%d ", p3);
      print_generic_expr (debug, SSA_NAME_VAR (partition_to_var (map, p3)),
			  TDF_SLIM);
      fprintf (debug, "\n");
    }
}


namespace {

const pass_data pass_data_rename_ssa_copies =
{
  GIMPLE_PASS, /* type */
  "copyrename", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  true, /* has_execute */
  TV_TREE_COPY_RENAME, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_rename_ssa_copies : public gimple_opt_pass
{
public:
  pass_rename_ssa_copies (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_rename_ssa_copies, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass * clone () { return new pass_rename_ssa_copies (m_ctxt); }
  virtual bool gate (function *) { return flag_tree_copyrename != 0; }
  virtual unsigned int execute (function *);

}; // class pass_rename_ssa_copies

/* This function will make a pass through the IL, and attempt to coalesce any
   SSA versions which occur in PHI's or copies.  Coalescing is accomplished by
   changing the underlying root variable of all coalesced version.  This will
   then cause the SSA->normal pass to attempt to coalesce them all to the same
   variable.  */

unsigned int
pass_rename_ssa_copies::execute (function *fun)
{
  var_map map;
  basic_block bb;
  gimple_stmt_iterator gsi;
  tree var, part_var;
  gimple stmt, phi;
  unsigned x;
  FILE *debug;

  memset (&stats, 0, sizeof (stats));

  if (dump_file && (dump_flags & TDF_DETAILS))
    debug = dump_file;
  else
    debug = NULL;

  map = init_var_map (num_ssa_names);

  FOR_EACH_BB_FN (bb, fun)
    {
      /* Scan for real copies.  */
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  stmt = gsi_stmt (gsi);
	  if (gimple_assign_ssa_name_copy_p (stmt))
	    {
	      tree lhs = gimple_assign_lhs (stmt);
	      tree rhs = gimple_assign_rhs1 (stmt);

	      copy_rename_partition_coalesce (map, lhs, rhs, debug);
	    }
	}
    }

  FOR_EACH_BB_FN (bb, fun)
    {
      /* Treat PHI nodes as copies between the result and each argument.  */
      for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
        {
          size_t i;
	  tree res;

	  phi = gsi_stmt (gsi);
	  res = gimple_phi_result (phi);

	  /* Do not process virtual SSA_NAMES.  */
	  if (virtual_operand_p (res))
	    continue;

	  /* Make sure to only use the same partition for an argument
	     as the result but never the other way around.  */
	  if (SSA_NAME_VAR (res)
	      && !DECL_IGNORED_P (SSA_NAME_VAR (res)))
	    for (i = 0; i < gimple_phi_num_args (phi); i++)
	      {
		tree arg = PHI_ARG_DEF (phi, i);
		if (TREE_CODE (arg) == SSA_NAME)
		  copy_rename_partition_coalesce (map, res, arg,
						  debug);
	      }
	  /* Else if all arguments are in the same partition try to merge
	     it with the result.  */
	  else
	    {
	      int all_p_same = -1;
	      int p = -1;
	      for (i = 0; i < gimple_phi_num_args (phi); i++)
		{
		  tree arg = PHI_ARG_DEF (phi, i);
		  if (TREE_CODE (arg) != SSA_NAME)
		    {
		      all_p_same = 0;
		      break;
		    }
		  else if (all_p_same == -1)
		    {
		      p = partition_find (map->var_partition,
					  SSA_NAME_VERSION (arg));
		      all_p_same = 1;
		    }
		  else if (all_p_same == 1
			   && p != partition_find (map->var_partition,
						   SSA_NAME_VERSION (arg)))
		    {
		      all_p_same = 0;
		      break;
		    }
		}
	      if (all_p_same == 1)
		copy_rename_partition_coalesce (map, res,
						PHI_ARG_DEF (phi, 0),
						debug);
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
      if (SSA_NAME_VAR (var) == SSA_NAME_VAR (part_var))
	continue;
      if (debug)
        {
	  fprintf (debug, "Coalesced ");
	  print_generic_expr (debug, var, TDF_SLIM);
	  fprintf (debug, " to ");
	  print_generic_expr (debug, part_var, TDF_SLIM);
	  fprintf (debug, "\n");
	}
      stats.coalesced++;
      replace_ssa_name_symbol (var, SSA_NAME_VAR (part_var));
    }

  statistics_counter_event (fun, "copies coalesced",
			    stats.coalesced);
  delete_var_map (map);
  return 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_rename_ssa_copies (gcc::context *ctxt)
{
  return new pass_rename_ssa_copies (ctxt);
}
