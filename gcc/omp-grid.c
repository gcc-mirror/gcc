/* Lowering and expansion of OpenMP directives for HSA GPU agents.

   Copyright (C) 2013-2020 Free Software Foundation, Inc.

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
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "ssa.h"
#include "cgraph.h"
#include "pretty-print.h"
#include "fold-const.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimple-walk.h"
#include "tree-inline.h"
#include "langhooks.h"
#include "omp-general.h"
#include "omp-low.h"
#include "omp-grid.h"
#include "gimple-pretty-print.h"

/* Return the lastprivate predicate for a given gridified loop described by
   FD).  */

tree
omp_grid_lastprivate_predicate (struct omp_for_data *fd)
{
  /* When dealing with a gridified loop, we need to check up to three collapsed
     iteration variables but they are not actually captured in this fd.
     Fortunately, we can easily rely on HSA builtins to get this
     information.  */

  tree id, size;
  if (gimple_omp_for_kind (fd->for_stmt) == GF_OMP_FOR_KIND_GRID_LOOP
      && gimple_omp_for_grid_intra_group (fd->for_stmt))
    {
      id = builtin_decl_explicit (BUILT_IN_HSA_WORKITEMID);
      size = builtin_decl_explicit (BUILT_IN_HSA_CURRENTWORKGROUPSIZE);
    }
  else
    {
      id = builtin_decl_explicit (BUILT_IN_HSA_WORKITEMABSID);
      size = builtin_decl_explicit (BUILT_IN_HSA_GRIDSIZE);
    }
  tree cond = NULL;
  for (int dim = 0; dim < fd->collapse; dim++)
    {
      tree dim_tree = build_int_cstu (unsigned_type_node, dim);
      tree u1 = build_int_cstu (unsigned_type_node, 1);
      tree c2
	= build2 (EQ_EXPR, boolean_type_node,
		  build2 (PLUS_EXPR, unsigned_type_node,
			  build_call_expr (id, 1, dim_tree), u1),
		  build_call_expr (size, 1, dim_tree));
      if (cond)
	cond = build2 (TRUTH_AND_EXPR, boolean_type_node, cond, c2);
      else
	cond = c2;
    }
  return cond;
}

/* Structure describing the basic properties of the loop we ara analyzing
   whether it can be gridified and when it is gridified.  */

class grid_prop
{
public:
  /* True when we are doing tiling gridification, i.e. when there is a distinct
     distribute loop over groups and a loop construct over work-items.  False
     when distribute and parallel for loops form a combined construct.  */
  bool tiling;
  /* Location of the target construct for optimization information
     messages.  */
  dump_user_location_t target_loc;
  /* The collapse clause of the involved loops.  Collapse value of all of them
     must be the same for gridification to take place.  */
  size_t collapse;
  /* Group sizes, if requested by the user or NULL if not requested.  */
  tree group_sizes[3];
};

#define GRID_MISSED_MSG_PREFIX "Will not turn target construct into a " \
  "gridified HSA kernel because "

/* Return true if STMT is an assignment of a register-type into a local
   VAR_DECL.  If GRID is non-NULL, the assignment additionally must not be to
   any of the trees specifying group sizes there.  */

static bool
grid_safe_assignment_p (gimple *stmt, grid_prop *grid)
{
  gassign *assign = dyn_cast <gassign *> (stmt);
  if (!assign)
    return false;
  if (gimple_clobber_p (assign))
    return true;
  tree lhs = gimple_assign_lhs (assign);
  if (!VAR_P (lhs)
      || !is_gimple_reg_type (TREE_TYPE (lhs))
      || is_global_var (lhs))
    return false;
  if (grid)
    for (unsigned i = 0; i < grid->collapse; i++)
      if (lhs == grid->group_sizes[i])
	return false;
  return true;
}

/* Return true if all statements in SEQ are assignments to local register-type
   variables that do not hold group size information.  */

static bool
grid_seq_only_contains_local_assignments (gimple_seq seq, grid_prop *grid)
{
  if (!seq)
    return true;

  gimple_stmt_iterator gsi;
  for (gsi = gsi_start (seq); !gsi_end_p (gsi); gsi_next (&gsi))
    if (!grid_safe_assignment_p (gsi_stmt (gsi), grid))
      return false;
  return true;
}

/* Scan statements in SEQ and call itself recursively on any bind.  GRID
   describes hitherto discovered properties of the loop that is evaluated for
   possible gridification.  If during whole search only assignments to
   register-type local variables (that do not overwrite group size information)
   and one single OMP statement is encountered, return true, otherwise return
   false.  RET is where we store any OMP statement encountered.  */

static bool
grid_find_single_omp_among_assignments_1 (gimple_seq seq, grid_prop *grid,
					  const char *name, gimple **ret)
{
  gimple_stmt_iterator gsi;
  for (gsi = gsi_start (seq); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);

      if (grid_safe_assignment_p (stmt, grid))
	continue;
      if (gbind *bind = dyn_cast <gbind *> (stmt))
	{
	  gimple_seq bind_body = gimple_bind_body (bind);
	  if (!grid_find_single_omp_among_assignments_1 (bind_body, grid, name,
							 ret))
	      return false;
	}
      else if (is_gimple_omp (stmt))
	{
	  if (*ret)
	    {
	      if (dump_enabled_p ())
		{
		  dump_printf_loc (MSG_MISSED_OPTIMIZATION, grid->target_loc,
				   GRID_MISSED_MSG_PREFIX "%s construct "
				   "contains multiple OpenMP constructs\n",
				   name);
		  dump_printf_loc (MSG_NOTE, *ret,
				   "The first OpenMP construct within "
				   "a parallel\n");
		  dump_printf_loc (MSG_NOTE, stmt,
				   "The second OpenMP construct within "
				   "a parallel\n");
		}
	      return false;
	    }
	  *ret = stmt;
	}
      else
	{
	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_MISSED_OPTIMIZATION, grid->target_loc,
			       GRID_MISSED_MSG_PREFIX "%s construct contains "
			       "a complex statement\n", name);
	      dump_printf_loc (MSG_NOTE, stmt,
			       "This statement cannot be analyzed for "
			       "gridification\n");
	    }
	  return false;
	}
    }
  return true;
}

/* Scan statements in SEQ and make sure that it and any binds in it contain
   only assignments to local register-type variables (that do not overwrite
   group size information) and one OMP construct.  If so, return that
   construct, otherwise return NULL.  GRID describes hitherto discovered
   properties of the loop that is evaluated for possible gridification.  If
   dumping is enabled and function fails, use NAME to dump a note with the
   reason for failure.  */

static gimple *
grid_find_single_omp_among_assignments (gimple_seq seq, grid_prop *grid,
					const char *name)
{
  if (!seq)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, grid->target_loc,
			 GRID_MISSED_MSG_PREFIX "%s construct has empty body\n",
			 name);
      return NULL;
    }

  gimple *ret = NULL;
  if (grid_find_single_omp_among_assignments_1 (seq, grid, name, &ret))
    {
      if (!ret && dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, grid->target_loc,
			 GRID_MISSED_MSG_PREFIX "%s construct does not contain"
			 " any other OpenMP construct\n", name);
      return ret;
    }
  else
    return NULL;
}

/* Walker function looking for statements there is no point gridifying (and for
   noreturn function calls which we cannot do).  Return non-NULL if such a
   function is found.  */

static tree
grid_find_ungridifiable_statement (gimple_stmt_iterator *gsi,
				   bool *handled_ops_p,
				   struct walk_stmt_info *wi)
{
  *handled_ops_p = false;
  gimple *stmt = gsi_stmt (*gsi);
  switch (gimple_code (stmt))
    {
    case GIMPLE_CALL:
      if (gimple_call_noreturn_p (as_a <gcall *> (stmt)))
	{
	  *handled_ops_p = true;
	  wi->info = stmt;
	  return error_mark_node;
	}
      break;

    /* We may reduce the following list if we find a way to implement the
       clauses, but now there is no point trying further.  */
    case GIMPLE_OMP_CRITICAL:
    case GIMPLE_OMP_TASKGROUP:
    case GIMPLE_OMP_TASK:
    case GIMPLE_OMP_SECTION:
    case GIMPLE_OMP_SECTIONS:
    case GIMPLE_OMP_SECTIONS_SWITCH:
    case GIMPLE_OMP_TARGET:
    case GIMPLE_OMP_ORDERED:
      *handled_ops_p = true;
      wi->info = stmt;
      return error_mark_node;
    default:
      break;
    }
  return NULL;
}

/* Examine clauses of omp parallel statement PAR and if any prevents
   gridification, issue a missed-optimization diagnostics and return false,
   otherwise return true.  GRID describes hitherto discovered properties of the
   loop that is evaluated for possible gridification.  */

static bool
grid_parallel_clauses_gridifiable (gomp_parallel *par, dump_user_location_t tloc)
{
  tree clauses = gimple_omp_parallel_clauses (par);
  while (clauses)
    {
      switch (OMP_CLAUSE_CODE (clauses))
	{
	case OMP_CLAUSE_NUM_THREADS:
	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_MISSED_OPTIMIZATION, tloc,
			       GRID_MISSED_MSG_PREFIX "because there is "
			       "a num_threads clause of the parallel "
			       "construct\n");
	      dump_printf_loc (MSG_NOTE, par,
			       "Parallel construct has a num_threads clause\n");
	    }
	  return false;

	case OMP_CLAUSE_REDUCTION:
	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_MISSED_OPTIMIZATION, tloc,
			       GRID_MISSED_MSG_PREFIX "a reduction clause "
			       "is present\n ");
	      dump_printf_loc (MSG_NOTE, par,
			       "Parallel construct has a reduction clause\n");
	    }
	  return false;

	default:
	  break;
	}
      clauses = OMP_CLAUSE_CHAIN (clauses);
    }
  return true;
}

/* Examine clauses and the body of omp loop statement GFOR and if something
   prevents gridification, issue a missed-optimization diagnostics and return
   false, otherwise return true.  GRID describes hitherto discovered properties
   of the loop that is evaluated for possible gridification.  */

static bool
grid_inner_loop_gridifiable_p (gomp_for *gfor, grid_prop *grid)
{
  if (!grid_seq_only_contains_local_assignments (gimple_omp_for_pre_body (gfor),
						 grid))
    {
      if (dump_enabled_p ())
	{
	  dump_printf_loc (MSG_MISSED_OPTIMIZATION, grid->target_loc,
			   GRID_MISSED_MSG_PREFIX "the inner loop "
			   "loop bounds computation contains a complex "
			   "statement\n");
	  dump_printf_loc (MSG_NOTE, gfor,
			   "Loop construct cannot be analyzed for "
			   "gridification\n");
	}
      return false;
    }

  tree clauses = gimple_omp_for_clauses (gfor);
  while (clauses)
    {
      switch (OMP_CLAUSE_CODE (clauses))
	{
	case OMP_CLAUSE_SCHEDULE:
	  if (OMP_CLAUSE_SCHEDULE_KIND (clauses) != OMP_CLAUSE_SCHEDULE_AUTO)
	    {
	      if (dump_enabled_p ())
		{
		  dump_printf_loc (MSG_MISSED_OPTIMIZATION, grid->target_loc,
				   GRID_MISSED_MSG_PREFIX "the inner loop "
				   "has a non-automatic schedule clause\n");
		  dump_printf_loc (MSG_NOTE, gfor,
				   "Loop construct has a non automatic "
				   "schedule clause\n");
		}
	      return false;
	    }
	  break;

	case OMP_CLAUSE_REDUCTION:
	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_MISSED_OPTIMIZATION, grid->target_loc,
			       GRID_MISSED_MSG_PREFIX "a reduction "
			       "clause is present\n ");
	      dump_printf_loc (MSG_NOTE, gfor,
			       "Loop construct has a reduction schedule "
			       "clause\n");
	    }
	  return false;

	default:
	  break;
	}
      clauses = OMP_CLAUSE_CHAIN (clauses);
    }
  struct walk_stmt_info wi;
  memset (&wi, 0, sizeof (wi));
  if (walk_gimple_seq (gimple_omp_body (gfor),
		       grid_find_ungridifiable_statement,
		       NULL, &wi))
    {
      gimple *bad = (gimple *) wi.info;
      if (dump_enabled_p ())
	{
	  if (is_gimple_call (bad))
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, grid->target_loc,
			       GRID_MISSED_MSG_PREFIX "the inner loop contains "
			       "call to a noreturn function\n");
	  else
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, grid->target_loc,
			     GRID_MISSED_MSG_PREFIX "the inner loop contains "
			     "statement %s which cannot be transformed\n",
			     gimple_code_name[(int) gimple_code (bad)]);
	  dump_printf_loc (MSG_NOTE, bad,
			   "This statement cannot be analyzed for "
			   "gridification\n");
	}
      return false;
    }
  return true;
}

/* Given distribute omp construct represented by DIST, which in the original
   source forms a compound construct with a looping construct, return true if it
   can be turned into a gridified HSA kernel.  Otherwise return false.  GRID
   describes hitherto discovered properties of the loop that is evaluated for
   possible gridification.  */

static bool
grid_dist_follows_simple_pattern (gomp_for *dist, grid_prop *grid)
{
  dump_user_location_t tloc = grid->target_loc;
  gimple *stmt = grid_find_single_omp_among_assignments (gimple_omp_body (dist),
							 grid, "distribute");
  gomp_parallel *par;
  if (!stmt
      || !(par = dyn_cast <gomp_parallel *> (stmt))
      || !grid_parallel_clauses_gridifiable (par, tloc))
    return false;

  stmt = grid_find_single_omp_among_assignments (gimple_omp_body (par), grid,
						 "parallel");
  gomp_for *gfor;
  if (!stmt || !(gfor = dyn_cast <gomp_for *> (stmt)))
    return false;

  if (gimple_omp_for_kind (gfor) != GF_OMP_FOR_KIND_FOR)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, tloc,
			 GRID_MISSED_MSG_PREFIX "the inner loop is not "
			 "a simple for loop\n");
      return false;
    }
  gcc_assert (gimple_omp_for_collapse (gfor) == grid->collapse);

  if (!grid_inner_loop_gridifiable_p (gfor, grid))
    return false;

  return true;
}

/* Given an omp loop statement GFOR, return true if it can participate in
   tiling gridification, i.e. in one where the distribute and parallel for
   loops do not form a compound statement.  GRID describes hitherto discovered
   properties of the loop that is evaluated for possible gridification.  */

static bool
grid_gfor_follows_tiling_pattern (gomp_for *gfor, grid_prop *grid)
{
  if (gimple_omp_for_kind (gfor) != GF_OMP_FOR_KIND_FOR)
    {
      if (dump_enabled_p ())
	{
	  dump_printf_loc (MSG_MISSED_OPTIMIZATION, grid->target_loc,
			   GRID_MISSED_MSG_PREFIX "an inner loop is not "
			   "a simple for loop\n");
	  dump_printf_loc (MSG_NOTE, gfor,
			   "This statement is not a simple for loop\n");
	}
      return false;
    }

  if (!grid_inner_loop_gridifiable_p (gfor, grid))
    return false;

  if (gimple_omp_for_collapse (gfor) != grid->collapse)
    {
      if (dump_enabled_p ())
	{
	  dump_printf_loc (MSG_MISSED_OPTIMIZATION, grid->target_loc,
			   GRID_MISSED_MSG_PREFIX "an inner loop does not "
			   "have use the same collapse clause\n");
	  dump_printf_loc (MSG_NOTE, gfor,
			   "Loop construct uses a different collapse clause\n");
	}
      return false;
    }

  struct omp_for_data fd;
  struct omp_for_data_loop *loops
    = (struct omp_for_data_loop *)alloca (grid->collapse
					  * sizeof (struct omp_for_data_loop));
  omp_extract_for_data (gfor, &fd, loops);
  for (unsigned i = 0; i < grid->collapse; i++)
    {
      tree itype, type = TREE_TYPE (fd.loops[i].v);
      if (POINTER_TYPE_P (type))
	itype = signed_type_for (type);
      else
	itype = type;

      tree n1 = fold_convert (itype, fd.loops[i].n1);
      tree n2 = fold_convert (itype, fd.loops[i].n2);
      tree t = build_int_cst (itype,
			      (fd.loops[i].cond_code == LT_EXPR ? -1 : 1));
      t = fold_build2 (PLUS_EXPR, itype, fd.loops[i].step, t);
      t = fold_build2 (PLUS_EXPR, itype, t, n2);
      t = fold_build2 (MINUS_EXPR, itype, t, n1);
      if (TYPE_UNSIGNED (itype) && fd.loops[i].cond_code == GT_EXPR)
	t = fold_build2 (TRUNC_DIV_EXPR, itype,
			 fold_build1 (NEGATE_EXPR, itype, t),
			 fold_build1 (NEGATE_EXPR, itype, fd.loops[i].step));
      else
	t = fold_build2 (TRUNC_DIV_EXPR, itype, t, fd.loops[i].step);

      if (!operand_equal_p (grid->group_sizes[i], t, 0))
	{
	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_MISSED_OPTIMIZATION, grid->target_loc,
			       GRID_MISSED_MSG_PREFIX "the distribute and "
			       "an internal loop do not agree on tile size\n");
	      dump_printf_loc (MSG_NOTE, gfor,
			       "Loop construct does not seem to loop over "
			       "a tile size\n");
	    }
	  return false;
	}
    }
  return true;
}

/* Facing a call to FNDECL in the body of a distribute construct, return true
   if we can handle it or false if it precludes gridification.  */

static bool
grid_call_permissible_in_distribute_p (tree fndecl)
{
  if (DECL_PURE_P (fndecl) || TREE_READONLY (fndecl))
    return true;

  const char *name = IDENTIFIER_POINTER (DECL_NAME (fndecl));
  if (strstr (name, "omp_") != name)
    return false;

  if ((strcmp (name, "omp_get_thread_num") == 0)
      || (strcmp (name, "omp_get_num_threads") == 0)
      || (strcmp (name, "omp_get_num_teams") == 0)
      || (strcmp (name, "omp_get_team_num") == 0)
      || (strcmp (name, "omp_get_level") == 0)
      || (strcmp (name, "omp_get_active_level") == 0)
      || (strcmp (name, "omp_in_parallel") == 0))
    return true;

  return false;
}

/* Facing a call satisfying grid_call_permissible_in_distribute_p in the body
   of a distribute construct that is pointed at by GSI, modify it as necessary
   for gridification.  If the statement itself got removed, return true.  */

static bool
grid_handle_call_in_distribute (gimple_stmt_iterator *gsi)
{
  gimple *stmt = gsi_stmt (*gsi);
  tree fndecl = gimple_call_fndecl (stmt);
  gcc_checking_assert (stmt);
  if (DECL_PURE_P (fndecl) || TREE_READONLY (fndecl))
    return false;

  const char *name = IDENTIFIER_POINTER (DECL_NAME (fndecl));
  if ((strcmp (name, "omp_get_thread_num") == 0)
      || (strcmp (name, "omp_get_level") == 0)
      || (strcmp (name, "omp_get_active_level") == 0)
      || (strcmp (name, "omp_in_parallel") == 0))
    {
      tree lhs = gimple_call_lhs (stmt);
      if (lhs)
	{
	  gassign *assign
	    = gimple_build_assign (lhs, build_zero_cst (TREE_TYPE (lhs)));
	  gsi_insert_before (gsi, assign, GSI_SAME_STMT);
	}
      gsi_remove (gsi, true);
      return true;
    }

  /* The rest of the omp functions can stay as they are, HSA back-end will
     handle them correctly.  */
  gcc_checking_assert ((strcmp (name, "omp_get_num_threads") == 0)
		       || (strcmp (name, "omp_get_num_teams") == 0)
		       || (strcmp (name, "omp_get_team_num") == 0));
  return false;
}

/* Given a sequence of statements within a distribute omp construct or a
   parallel construct, which in the original source does not form a compound
   construct with a looping construct, return true if it does not prevent us
   from turning it into a gridified HSA kernel.  Otherwise return false.  GRID
   describes hitherto discovered properties of the loop that is evaluated for
   possible gridification.  IN_PARALLEL must be true if seq is within a
   parallel construct and flase if it is only within a distribute
   construct.  */

static bool
grid_dist_follows_tiling_pattern (gimple_seq seq, grid_prop *grid,
				  bool in_parallel)
{
  gimple_stmt_iterator gsi;
  for (gsi = gsi_start (seq); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);

      if (grid_safe_assignment_p (stmt, grid)
	  || gimple_code (stmt) == GIMPLE_GOTO
	  || gimple_code (stmt) == GIMPLE_LABEL
	  || gimple_code (stmt) == GIMPLE_COND)
	continue;
      else if (gbind *bind = dyn_cast <gbind *> (stmt))
	{
	  if (!grid_dist_follows_tiling_pattern (gimple_bind_body (bind),
						 grid, in_parallel))
	    return false;
	  continue;
	}
      else if (gtry *try_stmt = dyn_cast <gtry *> (stmt))
	{
	  if (gimple_try_kind (try_stmt) == GIMPLE_TRY_CATCH)
	    {
	      if (dump_enabled_p ())
		{
		  dump_printf_loc (MSG_MISSED_OPTIMIZATION, grid->target_loc,
				   GRID_MISSED_MSG_PREFIX "the distribute "
				   "construct contains a try..catch region\n");
		  dump_printf_loc (MSG_NOTE, try_stmt,
				   "This statement cannot be analyzed for "
				   "tiled gridification\n");
		}
	      return false;
	    }
	  if (!grid_dist_follows_tiling_pattern (gimple_try_eval (try_stmt),
						 grid, in_parallel))
	    return false;
	  if (!grid_dist_follows_tiling_pattern (gimple_try_cleanup (try_stmt),
						 grid, in_parallel))
	    return false;
	  continue;
	}
      else if (is_gimple_call (stmt))
	{
	  tree fndecl = gimple_call_fndecl (stmt);
	  if (fndecl && grid_call_permissible_in_distribute_p (fndecl))
	    continue;

	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_MISSED_OPTIMIZATION, grid->target_loc,
			       GRID_MISSED_MSG_PREFIX "the distribute "
			       "construct contains a call\n");
	      dump_printf_loc (MSG_NOTE, stmt,
			       "This statement cannot be analyzed for "
			       "tiled gridification\n");
	    }
	  return false;
	}
      else if (gomp_parallel *par = dyn_cast <gomp_parallel *> (stmt))
	{
	  if (in_parallel)
	    {
	      if (dump_enabled_p ())
		{
		  dump_printf_loc (MSG_MISSED_OPTIMIZATION, grid->target_loc,
				   GRID_MISSED_MSG_PREFIX "a parallel "
				   "construct contains another parallel "
				   "construct\n");
		  dump_printf_loc (MSG_NOTE, stmt,
				   "This parallel construct is nested in "
				   "another one\n");
		}
	      return false;
	    }
	  if (!grid_parallel_clauses_gridifiable (par, grid->target_loc)
	      || !grid_dist_follows_tiling_pattern (gimple_omp_body (par),
						    grid, true))
	    return false;
	}
      else if (gomp_for *gfor = dyn_cast <gomp_for *> (stmt))
	{
	  if (!in_parallel)
	    {
	      if (dump_enabled_p ())
		{
		  dump_printf_loc (MSG_MISSED_OPTIMIZATION, grid->target_loc,
				   GRID_MISSED_MSG_PREFIX "a loop "
				   "construct is not nested within a parallel "
				   "construct\n");
		  dump_printf_loc (MSG_NOTE, stmt,
				   "This loop construct is not nested in "
				   "a parallel construct\n");
		}
	      return false;
	    }
	  if (!grid_gfor_follows_tiling_pattern (gfor, grid))
	    return false;
	}
      else
	{
	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_MISSED_OPTIMIZATION, grid->target_loc,
			       GRID_MISSED_MSG_PREFIX "the distribute "
			       "construct contains a complex statement\n");
	      dump_printf_loc (MSG_NOTE, stmt,
			       "This statement cannot be analyzed for "
			       "tiled gridification\n");
	    }
	  return false;
	}
    }
    return true;
}

/* If TARGET follows a pattern that can be turned into a gridified HSA kernel,
   return true, otherwise return false.  In the case of success, also fill in
   GRID with information describing the kernel grid.  */

static bool
grid_target_follows_gridifiable_pattern (gomp_target *target, grid_prop *grid)
{
  if (gimple_omp_target_kind (target) != GF_OMP_TARGET_KIND_REGION)
    return false;

  dump_user_location_t tloc = target;
  grid->target_loc = tloc;
  gimple *stmt
    = grid_find_single_omp_among_assignments (gimple_omp_body (target),
					      grid, "target");
  if (!stmt)
    return false;
  gomp_teams *teams = dyn_cast <gomp_teams *> (stmt);
  tree group_size = NULL;
  if (!teams)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, tloc,
			 GRID_MISSED_MSG_PREFIX "it does not have a sole "
			 "teams construct in it.\n");
      return false;
    }

  tree clauses = gimple_omp_teams_clauses (teams);
  while (clauses)
    {
      switch (OMP_CLAUSE_CODE (clauses))
	{
	case OMP_CLAUSE_NUM_TEAMS:
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, tloc,
			     GRID_MISSED_MSG_PREFIX "the teams construct "
			     "contains a num_teams clause\n ");
	  return false;

	case OMP_CLAUSE_REDUCTION:
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, tloc,
			     GRID_MISSED_MSG_PREFIX "a reduction "
			     "clause is present\n ");
	  return false;

	case OMP_CLAUSE_THREAD_LIMIT:
	  if (!integer_zerop (OMP_CLAUSE_OPERAND (clauses, 0)))
	    group_size = OMP_CLAUSE_OPERAND (clauses, 0);
	  break;

	default:
	  break;
	}
      clauses = OMP_CLAUSE_CHAIN (clauses);
    }

  stmt = grid_find_single_omp_among_assignments (gimple_omp_body (teams), grid,
						 "teams");
  if (!stmt)
    return false;
  gomp_for *dist = dyn_cast <gomp_for *> (stmt);
  if (!dist)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, tloc,
			 GRID_MISSED_MSG_PREFIX "the teams construct does not "
			 "have a single distribute construct in it.\n");
      return false;
    }

  gcc_assert (gimple_omp_for_kind (dist) == GF_OMP_FOR_KIND_DISTRIBUTE);

  grid->collapse = gimple_omp_for_collapse (dist);
  if (grid->collapse > 3)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, tloc,
			 GRID_MISSED_MSG_PREFIX "the distribute construct "
			 "contains collapse clause with parameter greater "
			 "than 3\n");
      return false;
    }

  struct omp_for_data fd;
  struct omp_for_data_loop *dist_loops
    = (struct omp_for_data_loop *)alloca (grid->collapse
					  * sizeof (struct omp_for_data_loop));
  omp_extract_for_data (dist, &fd, dist_loops);
  if (fd.chunk_size)
    {
      if (group_size && !operand_equal_p (group_size, fd.chunk_size, 0))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, tloc,
			     GRID_MISSED_MSG_PREFIX "the teams "
			     "thread limit is different from distribute "
			     "schedule chunk\n");
	  return false;
	}
      group_size = fd.chunk_size;
    }
  if (group_size && grid->collapse > 1)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, tloc,
			 GRID_MISSED_MSG_PREFIX "group size cannot be "
			 "set using thread_limit or schedule clauses "
			 "when also using a collapse clause greater than 1\n");
      return false;
    }

  if (gimple_omp_for_combined_p (dist))
    {
      grid->tiling = false;
      grid->group_sizes[0] = group_size;
      for (unsigned i = 1; i < grid->collapse; i++)
	grid->group_sizes[i] = NULL;
      return grid_dist_follows_simple_pattern (dist, grid);
    }
  else
    {
      grid->tiling = true;
      if (group_size)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, tloc,
			     GRID_MISSED_MSG_PREFIX "group size cannot be set "
			     "using thread_limit or schedule clauses when "
			     "distribute and loop constructs do not form "
			     "one combined construct\n");
	  return false;
	}
      for (unsigned i = 0; i < grid->collapse; i++)
	{
	  if (fd.loops[i].cond_code == GT_EXPR)
	    grid->group_sizes[i] = fold_build1 (NEGATE_EXPR,
						TREE_TYPE (fd.loops[i].step),
						fd.loops[i].step);
	  else
	    grid->group_sizes[i] = fd.loops[i].step;
	}
      return grid_dist_follows_tiling_pattern (gimple_omp_body (dist), grid,
					       false);
    }
}

/* Operand walker, used to remap pre-body declarations according to a hash map
   provided in DATA.  */

static tree
grid_remap_prebody_decls (tree *tp, int *walk_subtrees, void *data)
{
  tree t = *tp;

  if (DECL_P (t) || TYPE_P (t))
    *walk_subtrees = 0;
  else
    *walk_subtrees = 1;

  if (VAR_P (t))
    {
      struct walk_stmt_info *wi = (struct walk_stmt_info *) data;
      hash_map<tree, tree> *declmap = (hash_map<tree, tree> *) wi->info;
      tree *repl = declmap->get (t);
      if (repl)
	*tp = *repl;
    }
  return NULL_TREE;
}

/* Identifiers of segments into which a particular variable should be places
   when gridifying.  */

enum grid_var_segment {GRID_SEGMENT_PRIVATE, GRID_SEGMENT_GROUP,
		       GRID_SEGMENT_GLOBAL};

/* Mark VAR so that it is eventually placed into SEGMENT.  Place an artificial
   builtin call into SEQ that will make sure the variable is always considered
   address taken.  */

static void
grid_mark_variable_segment (tree var, enum grid_var_segment segment)
{
  /* Making a non-addressable variables would require that we re-gimplify all
     their uses.  Fortunately, we do not have to do this because if they are
     not addressable, it means they are not used in atomic or parallel
     statements and so relaxed GPU consistency rules mean we can just keep them
     private.  */
  if (!TREE_ADDRESSABLE (var))
    return;

  switch (segment)
    {
    case GRID_SEGMENT_GROUP:
      DECL_ATTRIBUTES (var) = tree_cons (get_identifier ("hsa_group_segment"),
					 NULL, DECL_ATTRIBUTES (var));
      break;
    case GRID_SEGMENT_GLOBAL:
      DECL_ATTRIBUTES (var) = tree_cons (get_identifier ("hsa_global_segment"),
					 NULL, DECL_ATTRIBUTES (var));
      break;
    default:
      gcc_unreachable ();
    }

  if (!TREE_STATIC (var))
    {
      TREE_STATIC (var) = 1;
      const char *prefix = IDENTIFIER_POINTER (DECL_NAME (var));
      SET_DECL_ASSEMBLER_NAME (var, create_tmp_var_name (prefix));
      varpool_node::finalize_decl (var);
    }

}

/* Copy leading register-type assignments to local variables in SRC to just
   before DST, Creating temporaries, adjusting mapping of operands in WI and
   remapping operands as necessary.  Add any new temporaries to TGT_BIND.
   Return the first statement that does not conform to grid_safe_assignment_p
   or NULL.  If VAR_SEGMENT is not GRID_SEGMENT_PRIVATE, also mark all
   variables in traversed bind statements so that they are put into the
   appropriate segment.  */

static gimple *
grid_copy_leading_local_assignments (gimple_seq src, gimple_stmt_iterator *dst,
				     gbind *tgt_bind,
				     enum grid_var_segment var_segment,
				     struct walk_stmt_info *wi)
{
  hash_map<tree, tree> *declmap = (hash_map<tree, tree> *) wi->info;
  gimple_stmt_iterator gsi;
  for (gsi = gsi_start (src); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);
      if (gbind *bind = dyn_cast <gbind *> (stmt))
	{
	  gimple *r = grid_copy_leading_local_assignments
	    (gimple_bind_body (bind), dst, tgt_bind, var_segment, wi);

	  if (var_segment != GRID_SEGMENT_PRIVATE)
	    for (tree var = gimple_bind_vars (bind);
		 var;
		 var = DECL_CHAIN (var))
	      grid_mark_variable_segment (var, var_segment);
	  if (r)
	    return r;
	  else
	    continue;
	}
      if (!grid_safe_assignment_p (stmt, NULL))
	return stmt;
      tree lhs = gimple_assign_lhs (as_a <gassign *> (stmt));
      tree repl = copy_var_decl (lhs, create_tmp_var_name (NULL),
				 TREE_TYPE (lhs));
      DECL_CONTEXT (repl) = current_function_decl;
      gimple_bind_append_vars (tgt_bind, repl);

      declmap->put (lhs, repl);
      gassign *copy = as_a <gassign *> (gimple_copy (stmt));
      walk_gimple_op (copy, grid_remap_prebody_decls, wi);
      gsi_insert_before (dst, copy, GSI_SAME_STMT);
    }
  return NULL;
}

/* Statement walker function to make adjustments to statements within the
   gridifed kernel copy.  */

static tree
grid_process_grid_body (gimple_stmt_iterator *gsi, bool *handled_ops_p,
			struct walk_stmt_info *)
{
  *handled_ops_p = false;
  gimple *stmt = gsi_stmt (*gsi);
  if (gimple_code (stmt) == GIMPLE_OMP_FOR
      && gimple_omp_for_kind (stmt) == GF_OMP_FOR_KIND_SIMD)
  {
    gomp_for *loop = as_a <gomp_for *> (stmt);
    tree clauses = gimple_omp_for_clauses (loop);
    tree cl = omp_find_clause (clauses, OMP_CLAUSE_SAFELEN);
    if (cl)
      OMP_CLAUSE_SAFELEN_EXPR (cl) = integer_one_node;
    else
      {
	tree c = build_omp_clause (UNKNOWN_LOCATION, OMP_CLAUSE_SAFELEN);
	OMP_CLAUSE_SAFELEN_EXPR (c) = integer_one_node;
	OMP_CLAUSE_CHAIN (c) = clauses;
	gimple_omp_for_set_clauses (loop, c);
      }
  }
  return NULL_TREE;
}

/* Given a PARLOOP that is a normal for looping construct but also a part of a
   combined construct with a simd loop, eliminate the simd loop.  */

static void
grid_eliminate_combined_simd_part (gomp_for *parloop)
{
  struct walk_stmt_info wi;

  memset (&wi, 0, sizeof (wi));
  wi.val_only = true;
  enum gf_mask msk = GF_OMP_FOR_KIND_SIMD;
  wi.info = (void *) &msk;
  walk_gimple_seq (gimple_omp_body (parloop), omp_find_combined_for, NULL, &wi);
  gimple *stmt = (gimple *) wi.info;
  /* We expect that the SIMD id the only statement in the parallel loop.  */
  gcc_assert (stmt
	      && gimple_code (stmt) == GIMPLE_OMP_FOR
	      && (gimple_omp_for_kind (stmt) == GF_OMP_FOR_KIND_SIMD)
	      && gimple_omp_for_combined_into_p (stmt)
	      && !gimple_omp_for_combined_p (stmt));
  gomp_for *simd = as_a <gomp_for *> (stmt);

  /* Copy over the iteration properties because the body refers to the index in
     the bottmom-most loop.  */
  unsigned i, collapse = gimple_omp_for_collapse (parloop);
  gcc_checking_assert (collapse == gimple_omp_for_collapse (simd));
  for (i = 0; i < collapse; i++)
    {
      gimple_omp_for_set_index (parloop, i, gimple_omp_for_index (simd, i));
      gimple_omp_for_set_initial (parloop, i, gimple_omp_for_initial (simd, i));
      gimple_omp_for_set_final (parloop, i, gimple_omp_for_final (simd, i));
      gimple_omp_for_set_incr (parloop, i, gimple_omp_for_incr (simd, i));
    }

  tree *tgt= gimple_omp_for_clauses_ptr (parloop);
  while (*tgt)
    tgt = &OMP_CLAUSE_CHAIN (*tgt);

  /* Copy over all clauses, except for linear clauses, which are turned into
     private clauses, and all other simd-specific clauses, which are
     ignored.  */
  tree *pc = gimple_omp_for_clauses_ptr (simd);
  while (*pc)
    {
      tree c = *pc;
      switch (OMP_CLAUSE_CODE (c))
	{
	case OMP_CLAUSE_LINEAR:
	  {
	    tree priv = build_omp_clause (UNKNOWN_LOCATION, OMP_CLAUSE_PRIVATE);
	    OMP_CLAUSE_DECL (priv) = OMP_CLAUSE_DECL (c);
	    OMP_CLAUSE_CHAIN (priv) = NULL;
	    *tgt = priv;
	    tgt = &OMP_CLAUSE_CHAIN (priv);
	    pc = &OMP_CLAUSE_CHAIN (c);
	    break;
	  }

	case OMP_CLAUSE_SAFELEN:
	case OMP_CLAUSE_SIMDLEN:
	case OMP_CLAUSE_ALIGNED:
	  pc = &OMP_CLAUSE_CHAIN (c);
	  break;

	default:
	  *pc = OMP_CLAUSE_CHAIN (c);
	  OMP_CLAUSE_CHAIN (c) = NULL;
	  *tgt = c;
	  tgt = &OMP_CLAUSE_CHAIN (c);
	  break;
	}
    }

  /* Finally, throw away the simd and mark the parallel loop as not
     combined.  */
  gimple_omp_set_body (parloop, gimple_omp_body (simd));
  gimple_omp_for_set_combined_p (parloop, false);
}

/* Statement walker function marking all parallels as grid_phony and loops as
   grid ones representing threads of a particular thread group.  */

static tree
grid_mark_tiling_loops (gimple_stmt_iterator *gsi, bool *handled_ops_p,
			struct walk_stmt_info *wi_in)
{
  *handled_ops_p = false;
  if (gomp_for *loop = dyn_cast <gomp_for *> (gsi_stmt (*gsi)))
    {
      *handled_ops_p = true;
      gimple_omp_for_set_kind (loop, GF_OMP_FOR_KIND_GRID_LOOP);
      gimple_omp_for_set_grid_intra_group (loop, true);
      if (gimple_omp_for_combined_p (loop))
	grid_eliminate_combined_simd_part (loop);

      struct walk_stmt_info body_wi;
      memset (&body_wi, 0, sizeof (body_wi));
      walk_gimple_seq_mod (gimple_omp_body_ptr (loop),
			   grid_process_grid_body, NULL, &body_wi);

      gbind *bind = (gbind *) wi_in->info;
      tree c;
      for (c = gimple_omp_for_clauses (loop); c; c = OMP_CLAUSE_CHAIN (c))
	if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LASTPRIVATE)
	  {
	    push_gimplify_context ();
	    tree ov = OMP_CLAUSE_DECL (c);
	    tree gv = copy_var_decl (ov, create_tmp_var_name (NULL),
				    TREE_TYPE (ov));

	    grid_mark_variable_segment (gv, GRID_SEGMENT_GROUP);
	    DECL_CONTEXT (gv) = current_function_decl;
	    gimple_bind_append_vars (bind, gv);
	    tree x = lang_hooks.decls.omp_clause_assign_op (c, gv, ov);
	    gimplify_and_add (x, &OMP_CLAUSE_LASTPRIVATE_GIMPLE_SEQ (c));
	    x = lang_hooks.decls.omp_clause_copy_ctor (c, ov, gv);
	    gimple_seq l = NULL;
	    gimplify_and_add (x, &l);
	    gsi_insert_seq_after (gsi, l, GSI_SAME_STMT);
	    pop_gimplify_context (bind);
	  }
    }
  return NULL_TREE;
}

/* Statement walker function marking all parallels as grid_phony and loops as
   grid ones representing threads of a particular thread group.  */

static tree
grid_mark_tiling_parallels_and_loops (gimple_stmt_iterator *gsi,
				      bool *handled_ops_p,
				      struct walk_stmt_info *wi_in)
{
  *handled_ops_p = false;
  wi_in->removed_stmt = false;
  gimple *stmt = gsi_stmt (*gsi);
  if (gbind *bind = dyn_cast <gbind *> (stmt))
    {
      for (tree var = gimple_bind_vars (bind); var; var = DECL_CHAIN (var))
	grid_mark_variable_segment (var, GRID_SEGMENT_GROUP);
    }
  else if (gomp_parallel *parallel = dyn_cast <gomp_parallel *> (stmt))
    {
      *handled_ops_p = true;
      gimple_omp_parallel_set_grid_phony (parallel, true);

      gbind *new_bind = gimple_build_bind (NULL, NULL, make_node (BLOCK));
      gimple_bind_set_body (new_bind, gimple_omp_body (parallel));
      gimple_seq s = NULL;
      gimple_seq_add_stmt (&s, new_bind);
      gimple_omp_set_body (parallel, s);

      struct walk_stmt_info wi_par;
      memset (&wi_par, 0, sizeof (wi_par));
      wi_par.info = new_bind;
      walk_gimple_seq_mod (gimple_bind_body_ptr (new_bind),
			   grid_mark_tiling_loops, NULL, &wi_par);
    }
  else if (is_a <gcall *> (stmt))
    wi_in->removed_stmt = grid_handle_call_in_distribute (gsi);
  return NULL_TREE;
}

/* Given freshly copied top level kernel SEQ, identify the individual OMP
   components, mark them as part of kernel, copy assignment leading to them
   just before DST, remapping them using WI and adding new temporaries to
   TGT_BIND, and return the loop that will be used for kernel dispatch.  */

static gomp_for *
grid_process_kernel_body_copy (grid_prop *grid, gimple_seq seq,
			       gimple_stmt_iterator *dst,
			       gbind *tgt_bind, struct walk_stmt_info *wi)
{
  gimple *stmt = grid_copy_leading_local_assignments (seq, dst, tgt_bind,
						      GRID_SEGMENT_GLOBAL, wi);
  gomp_teams *teams = dyn_cast <gomp_teams *> (stmt);
  gcc_assert (teams);
  gimple_omp_teams_set_grid_phony (teams, true);
  stmt = grid_copy_leading_local_assignments (gimple_omp_body (teams), dst,
					      tgt_bind, GRID_SEGMENT_GLOBAL,
					      wi);
  gcc_checking_assert (stmt);
  gomp_for *dist = dyn_cast <gomp_for *> (stmt);
  gcc_assert (dist);
  gimple_seq prebody = gimple_omp_for_pre_body (dist);
  if (prebody)
    grid_copy_leading_local_assignments (prebody, dst, tgt_bind,
					 GRID_SEGMENT_GROUP, wi);

  if (grid->tiling)
    {
      gimple_omp_for_set_kind (dist, GF_OMP_FOR_KIND_GRID_LOOP);
      gimple_omp_for_set_grid_group_iter (dist, true);

      struct walk_stmt_info wi_tiled;
      memset (&wi_tiled, 0, sizeof (wi_tiled));
      walk_gimple_seq_mod (gimple_omp_body_ptr (dist),
			   grid_mark_tiling_parallels_and_loops, NULL,
			   &wi_tiled);
      return dist;
    }
  else
    {
      gimple_omp_for_set_grid_phony (dist, true);
      stmt = grid_copy_leading_local_assignments (gimple_omp_body (dist), dst,
						  tgt_bind,
						  GRID_SEGMENT_PRIVATE, wi);
      gcc_checking_assert (stmt);
      gomp_parallel *parallel = as_a <gomp_parallel *> (stmt);
      gimple_omp_parallel_set_grid_phony (parallel, true);
      stmt = grid_copy_leading_local_assignments (gimple_omp_body (parallel),
						  dst, tgt_bind,
						  GRID_SEGMENT_PRIVATE, wi);
      gomp_for *inner_loop = as_a <gomp_for *> (stmt);
      gimple_omp_for_set_kind (inner_loop, GF_OMP_FOR_KIND_GRID_LOOP);
      prebody = gimple_omp_for_pre_body (inner_loop);
      if (prebody)
	grid_copy_leading_local_assignments (prebody, dst, tgt_bind,
					     GRID_SEGMENT_PRIVATE, wi);

      if (gimple_omp_for_combined_p (inner_loop))
	grid_eliminate_combined_simd_part (inner_loop);
      struct walk_stmt_info body_wi;
      memset (&body_wi, 0, sizeof (body_wi));
      walk_gimple_seq_mod (gimple_omp_body_ptr (inner_loop),
			   grid_process_grid_body, NULL, &body_wi);

      return inner_loop;
    }
}

/* If TARGET points to a GOMP_TARGET which follows a gridifiable pattern,
   create a GPU kernel for it.  GSI must point to the same statement, TGT_BIND
   is the bind into which temporaries inserted before TARGET should be
   added.  */

static void
grid_attempt_target_gridification (gomp_target *target,
				   gimple_stmt_iterator *gsi,
				   gbind *tgt_bind)
{
  /* removed group_size */
  grid_prop grid = {};
  if (!target || !grid_target_follows_gridifiable_pattern (target, &grid))
    return;

  location_t loc = gimple_location (target);
  if (dump_enabled_p ())
    dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, target,
		     "Target construct will be turned into a gridified HSA "
		     "kernel\n");

  /* Copy target body to a GPUKERNEL construct:  */
  gimple_seq kernel_seq = copy_gimple_seq_and_replace_locals
    (gimple_omp_body (target));

  hash_map<tree, tree> *declmap = new hash_map<tree, tree>;
  struct walk_stmt_info wi;
  memset (&wi, 0, sizeof (struct walk_stmt_info));
  wi.info = declmap;

  /* Copy assignments in between OMP statements before target, mark OMP
     statements within copy appropriately.  */
  gomp_for *inner_loop = grid_process_kernel_body_copy (&grid, kernel_seq, gsi,
							tgt_bind, &wi);

  gbind *old_bind
    = as_a <gbind *> (gimple_seq_first (gimple_omp_body (target)));
  gbind *new_bind = as_a <gbind *> (gimple_seq_first (kernel_seq));
  tree new_block = gimple_bind_block (new_bind);
  tree enc_block = BLOCK_SUPERCONTEXT (gimple_bind_block (old_bind));
  BLOCK_CHAIN (new_block) = BLOCK_SUBBLOCKS (enc_block);
  BLOCK_SUBBLOCKS (enc_block) = new_block;
  BLOCK_SUPERCONTEXT (new_block) = enc_block;
  gimple *gpukernel = gimple_build_omp_grid_body (kernel_seq);
  gimple_seq_add_stmt
    (gimple_bind_body_ptr (as_a <gbind *> (gimple_omp_body (target))),
     gpukernel);

  for (size_t i = 0; i < grid.collapse; i++)
    walk_tree (&grid.group_sizes[i], grid_remap_prebody_decls, &wi, NULL);
  push_gimplify_context ();
  for (size_t i = 0; i < grid.collapse; i++)
    {
      tree index_var = gimple_omp_for_index (inner_loop, i);
      tree itype, type = TREE_TYPE (index_var);
      if (POINTER_TYPE_P (type))
	itype = signed_type_for (type);
      else
	itype = type;

      enum tree_code cond_code = gimple_omp_for_cond (inner_loop, i);
      tree n1 = unshare_expr (gimple_omp_for_initial (inner_loop, i));
      walk_tree (&n1, grid_remap_prebody_decls, &wi, NULL);
      tree n2 = unshare_expr (gimple_omp_for_final (inner_loop, i));
      walk_tree (&n2, grid_remap_prebody_decls, &wi, NULL);
      tree step
	= omp_get_for_step_from_incr (loc, gimple_omp_for_incr (inner_loop, i));
      omp_adjust_for_condition (loc, &cond_code, &n2, index_var, step);
      n1 = fold_convert (itype, n1);
      n2 = fold_convert (itype, n2);

      tree cond = fold_build2 (cond_code, boolean_type_node, n1, n2);

      tree t = build_int_cst (itype, (cond_code == LT_EXPR ? -1 : 1));
      t = fold_build2 (PLUS_EXPR, itype, step, t);
      t = fold_build2 (PLUS_EXPR, itype, t, n2);
      t = fold_build2 (MINUS_EXPR, itype, t, n1);
      if (TYPE_UNSIGNED (itype) && cond_code == GT_EXPR)
	t = fold_build2 (TRUNC_DIV_EXPR, itype,
			 fold_build1 (NEGATE_EXPR, itype, t),
			 fold_build1 (NEGATE_EXPR, itype, step));
      else
	t = fold_build2 (TRUNC_DIV_EXPR, itype, t, step);
      t = fold_build3 (COND_EXPR, itype, cond, t, build_zero_cst (itype));
      if (grid.tiling)
	{
	  if (cond_code == GT_EXPR)
	    step = fold_build1 (NEGATE_EXPR, itype, step);
	  t = fold_build2 (MULT_EXPR, itype, t, step);
	}

      tree gs = fold_convert (uint32_type_node, t);
      gimple_seq tmpseq = NULL;
      gimplify_expr (&gs, &tmpseq, NULL, is_gimple_val, fb_rvalue);
      if (!gimple_seq_empty_p (tmpseq))
	gsi_insert_seq_before (gsi, tmpseq, GSI_SAME_STMT);

      tree ws;
      if (grid.group_sizes[i])
	{
	  ws = fold_convert (uint32_type_node, grid.group_sizes[i]);
	  tmpseq = NULL;
	  gimplify_expr (&ws, &tmpseq, NULL, is_gimple_val, fb_rvalue);
	  if (!gimple_seq_empty_p (tmpseq))
	    gsi_insert_seq_before (gsi, tmpseq, GSI_SAME_STMT);
	}
      else
	ws = build_zero_cst (uint32_type_node);

      tree c = build_omp_clause (UNKNOWN_LOCATION, OMP_CLAUSE__GRIDDIM_);
      OMP_CLAUSE__GRIDDIM__DIMENSION (c) = i;
      OMP_CLAUSE__GRIDDIM__SIZE (c) = gs;
      OMP_CLAUSE__GRIDDIM__GROUP (c) = ws;
      OMP_CLAUSE_CHAIN (c) = gimple_omp_target_clauses (target);
      gimple_omp_target_set_clauses (target, c);
    }
  pop_gimplify_context (tgt_bind);
  delete declmap;
  return;
}

/* Walker function doing all the work for create_target_kernels.  */

static tree
grid_gridify_all_targets_stmt (gimple_stmt_iterator *gsi,
				   bool *handled_ops_p,
				   struct walk_stmt_info *incoming)
{
  *handled_ops_p = false;

  gimple *stmt = gsi_stmt (*gsi);
  gomp_target *target = dyn_cast <gomp_target *> (stmt);
  if (target)
    {
      gbind *tgt_bind = (gbind *) incoming->info;
      gcc_checking_assert (tgt_bind);
      grid_attempt_target_gridification (target, gsi, tgt_bind);
      return NULL_TREE;
    }
  gbind *bind = dyn_cast <gbind *> (stmt);
  if (bind)
    {
      *handled_ops_p = true;
      struct walk_stmt_info wi;
      memset (&wi, 0, sizeof (wi));
      wi.info = bind;
      walk_gimple_seq_mod (gimple_bind_body_ptr (bind),
			   grid_gridify_all_targets_stmt, NULL, &wi);
    }
  return NULL_TREE;
}

/* Attempt to gridify all target constructs in BODY_P.  All such targets will
   have their bodies duplicated, with the new copy being put into a
   gimple_omp_grid_body statement.  All kernel-related construct within the
   grid_body will be marked with phony flags or kernel kinds.  Moreover, some
   re-structuring is often needed, such as copying pre-bodies before the target
   construct so that kernel grid sizes can be computed.  */

void
omp_grid_gridify_all_targets (gimple_seq *body_p)
{
  struct walk_stmt_info wi;
  memset (&wi, 0, sizeof (wi));
  walk_gimple_seq_mod (body_p, grid_gridify_all_targets_stmt, NULL, &wi);
}
