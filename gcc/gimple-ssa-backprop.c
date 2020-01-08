/* Back-propagation of usage information to definitions.
   Copyright (C) 2015-2020 Free Software Foundation, Inc.

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

/* This pass propagates information that is common to all uses of an SSA
   name back up through the sequence of statements that generate it,
   simplifying the statements where possible.  Sometimes this can expose
   fully or partially dead code, but the main focus is simplifying
   computations.

   At the moment the pass only handles one piece of information: whether the
   sign of a value matters, and therefore whether sign-changing operations
   can be skipped.  The pass could be extended to more interesting
   information in future, such as which bits of an integer are significant.

   For example, take the function:

     double
     f (double *a, int n, double start)
     {
       double x = fabs (start);
       for (int i = 0; i < n; ++i)
	 x *= a[i];
       return __builtin_cos (x);
     }

   cos(x) == cos(-x), so the sign of the final x doesn't matter.
   That x is the result of a series of multiplications, and if
   the sign of the result of a multiplication doesn't matter,
   the signs of the inputs don't matter either.

   The pass would replace the incoming value of x (i.e. fabs(start))
   with start.  Since there are no other uses of the fabs result,
   the call would get deleted as dead.

   The algorithm is:

   (1) Do a post-order traversal of the blocks in the function, walking
       each block backwards.  For each potentially-simplifiable statement
       that defines an SSA name X, examine all uses of X to see what
       information is actually significant.  Record this as INFO_MAP[X].
       Optimistically ignore for now any back-edge references to
       unprocessed phis.

       (An alternative would be to record each use when we visit its
       statement and take the intersection as we go along.  However,
       this would lead to more SSA names being entered into INFO_MAP
       unnecessarily, only to be taken out again later.  At the moment
       very few SSA names end up with useful information.)

   (2) Iteratively reduce the optimistic result of (1) until we reach
       a maximal fixed point (which at the moment would mean revisiting
       statements at most once).  First push all SSA names that used an
       optimistic assumption about a backedge phi onto a worklist.
       While the worklist is nonempty, pick off an SSA name X and recompute
       INFO_MAP[X].  If the value changes, push all SSA names used in the
       definition of X onto the worklist.

   (3) Iterate over each SSA name X with info in INFO_MAP, in the
       opposite order to (1), i.e. a forward reverse-post-order walk.
       Try to optimize the definition of X using INFO_MAP[X] and fold
       the result.  (This ensures that we fold definitions before uses.)

   (4) Iterate over each SSA name X with info in INFO_MAP, in the same
       order as (1), and delete any statements that are now dead.
       (This ensures that if a sequence of statements is dead,
       we delete the last statement first.)

   Note that this pass does not deal with direct redundancies,
   such as cos(-x)->cos(x).  match.pd handles those cases instead.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "ssa.h"
#include "fold-const.h"
#include "tree-pass.h"
#include "cfganal.h"
#include "gimple-pretty-print.h"
#include "tree-cfg.h"
#include "tree-ssa.h"
#include "tree-ssa-propagate.h"
#include "gimple-fold.h"
#include "alloc-pool.h"
#include "tree-hash-traits.h"
#include "case-cfn-macros.h"

namespace {

/* Information about a group of uses of an SSA name.  */
class usage_info
{
public:
  usage_info () : flag_word (0) {}
  usage_info &operator &= (const usage_info &);
  usage_info operator & (const usage_info &) const;
  bool operator == (const usage_info &) const;
  bool operator != (const usage_info &) const;
  bool is_useful () const;

  static usage_info intersection_identity ();

  union
  {
    struct
    {
      /* True if the uses treat x and -x in the same way.  */
      unsigned int ignore_sign : 1;
    } flags;
    /* All the flag bits as a single int.  */
    unsigned int flag_word;
  };
};

/* Return an X such that X & Y == Y for all Y.  This is the most
   optimistic assumption possible.  */

usage_info
usage_info::intersection_identity ()
{
  usage_info ret;
  ret.flag_word = -1;
  return ret;
}

/* Intersect *THIS with OTHER, so that *THIS describes all uses covered
   by the original *THIS and OTHER.  */

usage_info &
usage_info::operator &= (const usage_info &other)
{
  flag_word &= other.flag_word;
  return *this;
}

/* Return the intersection of *THIS and OTHER, i.e. a structure that
   describes all uses covered by *THIS and OTHER.  */

usage_info
usage_info::operator & (const usage_info &other) const
{
  usage_info info (*this);
  info &= other;
  return info;
}

bool
usage_info::operator == (const usage_info &other) const
{
  return flag_word == other.flag_word;
}

bool
usage_info::operator != (const usage_info &other) const
{
  return !operator == (other);
}

/* Return true if *THIS is not simply the default, safe assumption.  */

bool
usage_info::is_useful () const
{
  return flag_word != 0;
}

/* Start a dump line about SSA name VAR.  */

static void
dump_usage_prefix (FILE *file, tree var)
{
  fprintf (file, "  ");
  print_generic_expr (file, var);
  fprintf (file, ": ");
}

/* Print INFO to FILE.  */

static void
dump_usage_info (FILE *file, tree var, usage_info *info)
{
  if (info->flags.ignore_sign)
    {
      dump_usage_prefix (file, var);
      fprintf (file, "sign bit not important\n");
    }
}

/* Represents one execution of the pass.  */
class backprop
{
public:
  backprop (function *);
  ~backprop ();

  void execute ();

private:
  const usage_info *lookup_operand (tree);

  void push_to_worklist (tree);
  tree pop_from_worklist ();

  void process_builtin_call_use (gcall *, tree, usage_info *);
  void process_assign_use (gassign *, tree, usage_info *);
  void process_phi_use (gphi *, usage_info *);
  void process_use (gimple *, tree, usage_info *);
  bool intersect_uses (tree, usage_info *);
  void reprocess_inputs (gimple *);
  void process_var (tree);
  void process_block (basic_block);

  void prepare_change (tree);
  void complete_change (gimple *);
  void optimize_builtin_call (gcall *, tree, const usage_info *);
  void replace_assign_rhs (gassign *, tree, tree, tree, tree);
  void optimize_assign (gassign *, tree, const usage_info *);
  void optimize_phi (gphi *, tree, const usage_info *);

  typedef hash_map <tree_ssa_name_hash, usage_info *> info_map_type;
  typedef std::pair <tree, usage_info *> var_info_pair;

  /* The function we're optimizing.  */
  function *m_fn;

  /* Pool for allocating usage_info structures.  */
  object_allocator <usage_info> m_info_pool;

  /* Maps an SSA name to a description of all uses of that SSA name.
     All the usage_infos satisfy is_useful.

     We use a hash_map because the map is expected to be sparse
     (i.e. most SSA names won't have useful information attached to them).
     We could move to a directly-indexed array if that situation changes.  */
  info_map_type m_info_map;

  /* Post-ordered list of all potentially-interesting SSA names,
     along with information that describes all uses.  */
  auto_vec <var_info_pair, 128> m_vars;

  /* A bitmap of blocks that we have finished processing in the initial
     post-order walk.  */
  auto_sbitmap m_visited_blocks;

  /* A bitmap of phis that we have finished processing in the initial
     post-order walk, excluding those from blocks mentioned in
     M_VISITED_BLOCKS.  */
  auto_bitmap m_visited_phis;

  /* A worklist of SSA names whose definitions need to be reconsidered.  */
  auto_vec <tree, 64> m_worklist;

  /* The SSA names in M_WORKLIST, identified by their SSA_NAME_VERSION.
     We use a bitmap rather than an sbitmap because most SSA names are
     never added to the worklist.  */
  bitmap m_worklist_names;
};

backprop::backprop (function *fn)
  : m_fn (fn),
    m_info_pool ("usage_info"),
    m_visited_blocks (last_basic_block_for_fn (m_fn)),
    m_worklist_names (BITMAP_ALLOC (NULL))
{
  bitmap_clear (m_visited_blocks);
}

backprop::~backprop ()
{
  BITMAP_FREE (m_worklist_names);
  m_info_pool.release ();
}

/* Return usage information for general operand OP, or null if none.  */

const usage_info *
backprop::lookup_operand (tree op)
{
  if (op && TREE_CODE (op) == SSA_NAME)
    {
      usage_info **slot = m_info_map.get (op);
      if (slot)
	return *slot;
    }
  return NULL;
}

/* Add SSA name VAR to the worklist, if it isn't on the worklist already.  */

void
backprop::push_to_worklist (tree var)
{
  if (!bitmap_set_bit (m_worklist_names, SSA_NAME_VERSION (var)))
    return;
  m_worklist.safe_push (var);
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "[WORKLIST] Pushing ");
      print_generic_expr (dump_file, var);
      fprintf (dump_file, "\n");
    }
}

/* Remove and return the next SSA name from the worklist.  The worklist
   is known to be nonempty.  */

tree
backprop::pop_from_worklist ()
{
  tree var = m_worklist.pop ();
  bitmap_clear_bit (m_worklist_names, SSA_NAME_VERSION (var));
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "[WORKLIST] Popping ");
      print_generic_expr (dump_file, var);
      fprintf (dump_file, "\n");
    }
  return var;
}

/* Make INFO describe all uses of RHS in CALL, which is a call to a
   built-in function.  */

void
backprop::process_builtin_call_use (gcall *call, tree rhs, usage_info *info)
{
  combined_fn fn = gimple_call_combined_fn (call);
  tree lhs = gimple_call_lhs (call);
  switch (fn)
    {
    case CFN_LAST:
      break;

    CASE_CFN_COS:
    CASE_CFN_COSH:
    CASE_CFN_CCOS:
    CASE_CFN_CCOSH:
    CASE_CFN_HYPOT:
      /* The signs of all inputs are ignored.  */
      info->flags.ignore_sign = true;
      break;

    CASE_CFN_COPYSIGN:
    CASE_CFN_COPYSIGN_FN:
      /* The sign of the first input is ignored.  */
      if (rhs != gimple_call_arg (call, 1))
	info->flags.ignore_sign = true;
      break;

    CASE_CFN_POW:
      {
	/* The sign of the first input is ignored as long as the second
	   input is an even real.  */
	tree power = gimple_call_arg (call, 1);
	HOST_WIDE_INT n;
	if (TREE_CODE (power) == REAL_CST
	    && real_isinteger (&TREE_REAL_CST (power), &n)
	    && (n & 1) == 0)
	  info->flags.ignore_sign = true;
	break;
      }

    CASE_CFN_FMA:
    CASE_CFN_FMA_FN:
    case CFN_FMS:
    case CFN_FNMA:
    case CFN_FNMS:
      /* In X * X + Y, where Y is distinct from X, the sign of X doesn't
	 matter.  */
      if (gimple_call_arg (call, 0) == rhs
	  && gimple_call_arg (call, 1) == rhs
	  && gimple_call_arg (call, 2) != rhs)
	info->flags.ignore_sign = true;
      break;

    default:
      if (negate_mathfn_p (fn))
	{
	  /* The sign of the (single) input doesn't matter provided
	     that the sign of the output doesn't matter.  */
	  const usage_info *lhs_info = lookup_operand (lhs);
	  if (lhs_info)
	    info->flags.ignore_sign = lhs_info->flags.ignore_sign;
	}
      break;
    }
}

/* Make INFO describe all uses of RHS in ASSIGN.  */

void
backprop::process_assign_use (gassign *assign, tree rhs, usage_info *info)
{
  tree lhs = gimple_assign_lhs (assign);
  switch (gimple_assign_rhs_code (assign))
    {
    case ABS_EXPR:
    case ABSU_EXPR:
      /* The sign of the input doesn't matter.  */
      info->flags.ignore_sign = true;
      break;

    case COND_EXPR:
      /* For A = B ? C : D, propagate information about all uses of A
	 to C and D.  */
      if (rhs != gimple_assign_rhs1 (assign))
	{
	  const usage_info *lhs_info = lookup_operand (lhs);
	  if (lhs_info)
	    *info = *lhs_info;
	}
      break;

    case MULT_EXPR:
      /* In X * X, the sign of X doesn't matter.  */
      if (gimple_assign_rhs1 (assign) == rhs
	  && gimple_assign_rhs2 (assign) == rhs)
	info->flags.ignore_sign = true;
      /* Fall through.  */

    case NEGATE_EXPR:
    case RDIV_EXPR:
      /* If the sign of the result doesn't matter, the sign of the inputs
	 doesn't matter either.  */
      if (FLOAT_TYPE_P (TREE_TYPE (rhs)))
	{
	  const usage_info *lhs_info = lookup_operand (lhs);
	  if (lhs_info)
	    info->flags.ignore_sign = lhs_info->flags.ignore_sign;
	}
      break;

    default:
      break;
    }
}

/* Make INFO describe the uses of PHI's result.  */

void
backprop::process_phi_use (gphi *phi, usage_info *info)
{
  tree result = gimple_phi_result (phi);
  if (const usage_info *result_info = lookup_operand (result))
    *info = *result_info;
}

/* Make INFO describe all uses of RHS in STMT.  */

void
backprop::process_use (gimple *stmt, tree rhs, usage_info *info)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "[USE] ");
      print_generic_expr (dump_file, rhs);
      fprintf (dump_file, " in ");
      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
    }

  if (gcall *call = dyn_cast <gcall *> (stmt))
    process_builtin_call_use (call, rhs, info);
  else if (gassign *assign = dyn_cast <gassign *> (stmt))
    process_assign_use (assign, rhs, info);
  else if (gphi *phi = dyn_cast <gphi *> (stmt))
    process_phi_use (phi, info);

  if (dump_file && (dump_flags & TDF_DETAILS))
    dump_usage_info (dump_file, rhs, info);
}

/* Make INFO describe all uses of VAR, returning true if the result
   is useful.  If the uses include phis that haven't been processed yet,
   make the most optimistic assumption possible, so that we aim for
   a maximum rather than a minimum fixed point.  */

bool
backprop::intersect_uses (tree var, usage_info *info)
{
  imm_use_iterator iter;
  use_operand_p use_p;
  *info = usage_info::intersection_identity ();
  FOR_EACH_IMM_USE_FAST (use_p, iter, var)
    {
      gimple *stmt = USE_STMT (use_p);
      if (is_gimple_debug (stmt))
	continue;
      gphi *phi = dyn_cast <gphi *> (stmt);
      if (phi
	  && !bitmap_bit_p (m_visited_blocks, gimple_bb (phi)->index)
	  && !bitmap_bit_p (m_visited_phis,
			    SSA_NAME_VERSION (gimple_phi_result (phi))))
	{
	  /* Skip unprocessed phis.  */
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "[BACKEDGE] ");
	      print_generic_expr (dump_file, var);
	      fprintf (dump_file, " in ");
	      print_gimple_stmt (dump_file, phi, 0, TDF_SLIM);
	    }
	}
      else
	{
	  usage_info subinfo;
	  process_use (stmt, var, &subinfo);
	  *info &= subinfo;
	  if (!info->is_useful ())
	    return false;
	}
    }
  return true;
}

/* Queue for reconsideration any input of STMT that has information
   associated with it.  This is used if that information might be
   too optimistic.  */

void
backprop::reprocess_inputs (gimple *stmt)
{
  use_operand_p use_p;
  ssa_op_iter oi;
  FOR_EACH_PHI_OR_STMT_USE (use_p, stmt, oi, SSA_OP_USE)
    {
      tree var = get_use_from_ptr (use_p);
      if (lookup_operand (var))
	push_to_worklist (var);
    }
}

/* Say that we're recording INFO for SSA name VAR, or that we're deleting
   existing information if INFO is null.  INTRO describes the change.  */

static void
dump_var_info (tree var, usage_info *info, const char *intro)
{
  fprintf (dump_file, "[DEF] %s for ", intro);
  print_gimple_stmt (dump_file, SSA_NAME_DEF_STMT (var), 0, TDF_SLIM);
  if (info)
    dump_usage_info (dump_file, var, info);
}

/* Process all uses of VAR and record or update the result in
   M_INFO_MAP and M_VARS.  */

void
backprop::process_var (tree var)
{
  if (has_zero_uses (var))
    return;

  usage_info info;
  intersect_uses (var, &info);

  gimple *stmt = SSA_NAME_DEF_STMT (var);
  if (info.is_useful ())
    {
      bool existed;
      usage_info *&map_info = m_info_map.get_or_insert (var, &existed);
      if (!existed)
	{
	  /* Recording information about VAR for the first time.  */
	  map_info = m_info_pool.allocate ();
	  *map_info = info;
	  m_vars.safe_push (var_info_pair (var, map_info));
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    dump_var_info (var, map_info, "Recording new information");

	  /* If STMT is a phi, reprocess any backedge uses.  This is a
	     no-op for other uses, which won't have any information
	     associated with them.  */
	  if (is_a <gphi *> (stmt))
	    reprocess_inputs (stmt);
	}
      else if (info != *map_info)
	{
	  /* Recording information that is less optimistic than before.  */
	  gcc_checking_assert ((info & *map_info) == info);
	  *map_info = info;
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    dump_var_info (var, map_info, "Updating information");
	  reprocess_inputs (stmt);
	}
    }
  else
    {
      if (usage_info **slot = m_info_map.get (var))
	{
	  /* Removing previously-recorded information.  */
	  **slot = info;
	  m_info_map.remove (var);
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    dump_var_info (var, NULL, "Deleting information");
	  reprocess_inputs (stmt);
	}
      else
	{
	  /* If STMT is a phi, remove any information recorded for
	     its arguments.  */
	  if (is_a <gphi *> (stmt))
	    reprocess_inputs (stmt);
	}
    }
}

/* Process all statements and phis in BB, during the first post-order walk.  */

void
backprop::process_block (basic_block bb)
{
  for (gimple_stmt_iterator gsi = gsi_last_bb (bb); !gsi_end_p (gsi);
       gsi_prev (&gsi))
    {
      tree lhs = gimple_get_lhs (gsi_stmt (gsi));
      if (lhs && TREE_CODE (lhs) == SSA_NAME)
	process_var (lhs);
    }
  for (gphi_iterator gpi = gsi_start_phis (bb); !gsi_end_p (gpi);
       gsi_next (&gpi))
    {
      tree result = gimple_phi_result (gpi.phi ());
      process_var (result);
      bitmap_set_bit (m_visited_phis, SSA_NAME_VERSION (result));
    }
  bitmap_clear (m_visited_phis);
}

/* Delete the definition of VAR, which has no uses.  */

static void
remove_unused_var (tree var)
{
  gimple *stmt = SSA_NAME_DEF_STMT (var);
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Deleting ");
      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
    }
  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
  gsi_remove (&gsi, true);
  release_defs (stmt);
}

/* Note that we're replacing OLD_RHS with NEW_RHS in STMT.  */

static void
note_replacement (gimple *stmt, tree old_rhs, tree new_rhs)
{
  fprintf (dump_file, "Replacing use of ");
  print_generic_expr (dump_file, old_rhs);
  fprintf (dump_file, " with ");
  print_generic_expr (dump_file, new_rhs);
  fprintf (dump_file, " in ");
  print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
}

/* If RHS is an SSA name whose definition just changes the sign of a value,
   return that other value, otherwise return null.  */

static tree
strip_sign_op_1 (tree rhs)
{
  if (TREE_CODE (rhs) != SSA_NAME)
    return NULL_TREE;

  gimple *def_stmt = SSA_NAME_DEF_STMT (rhs);
  if (gassign *assign = dyn_cast <gassign *> (def_stmt))
    switch (gimple_assign_rhs_code (assign))
      {
      case ABS_EXPR:
      case ABSU_EXPR:
      case NEGATE_EXPR:
	return gimple_assign_rhs1 (assign);

      default:
	break;
      }
  else if (gcall *call = dyn_cast <gcall *> (def_stmt))
    switch (gimple_call_combined_fn (call))
      {
      CASE_CFN_COPYSIGN:
      CASE_CFN_COPYSIGN_FN:
	return gimple_call_arg (call, 0);

      default:
	break;
      }

  return NULL_TREE;
}

/* If RHS is an SSA name whose definition just changes the sign of a value,
   strip all such operations and return the ultimate input to them.
   Return null otherwise.

   Although this could in principle lead to quadratic searching,
   in practice a long sequence of sign manipulations should already
   have been folded down.  E.g. --x -> x, abs(-x) -> abs(x).  We search
   for more than one operation in order to catch cases like -abs(x).  */

static tree
strip_sign_op (tree rhs)
{
  tree new_rhs = strip_sign_op_1 (rhs);
  if (!new_rhs)
    return NULL_TREE;
  while (tree next = strip_sign_op_1 (new_rhs))
    new_rhs = next;
  return new_rhs;
}

/* Start a change in the value of VAR that is suitable for all non-debug
   uses of VAR.  We need to make sure that debug statements continue to
   use the original definition of VAR where possible, or are nullified
   otherwise.  */

void
backprop::prepare_change (tree var)
{
  if (MAY_HAVE_DEBUG_BIND_STMTS)
    insert_debug_temp_for_var_def (NULL, var);
  reset_flow_sensitive_info (var);
}

/* STMT has been changed.  Give the fold machinery a chance to simplify
   and canonicalize it (e.g. by ensuring that commutative operands have
   the right order), then record the updates.  */

void
backprop::complete_change (gimple *stmt)
{
  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
  if (fold_stmt (&gsi))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "  which folds to: ");
	  print_gimple_stmt (dump_file, gsi_stmt (gsi), 0, TDF_SLIM);
	}
    }
  update_stmt (gsi_stmt (gsi));
}

/* Optimize CALL, a call to a built-in function with lhs LHS, on the
   basis that INFO describes all uses of LHS.  */

void
backprop::optimize_builtin_call (gcall *call, tree lhs, const usage_info *info)
{
  /* If we have an f such that -f(x) = f(-x), and if the sign of the result
     doesn't matter, strip any sign operations from the input.  */
  if (info->flags.ignore_sign
      && negate_mathfn_p (gimple_call_combined_fn (call)))
    {
      tree new_arg = strip_sign_op (gimple_call_arg (call, 0));
      if (new_arg)
	{
	  prepare_change (lhs);
	  gimple_call_set_arg (call, 0, new_arg);
	  complete_change (call);
	}
    }
}

/* Optimize ASSIGN, an assignment to LHS, by replacing rhs operand N
   with RHS<N>, if RHS<N> is nonnull.  This may change the value of LHS.  */

void
backprop::replace_assign_rhs (gassign *assign, tree lhs, tree rhs1,
			      tree rhs2, tree rhs3)
{
  if (!rhs1 && !rhs2 && !rhs3)
    return;

  prepare_change (lhs);
  if (rhs1)
    gimple_assign_set_rhs1 (assign, rhs1);
  if (rhs2)
    gimple_assign_set_rhs2 (assign, rhs2);
  if (rhs3)
    gimple_assign_set_rhs3 (assign, rhs3);
  complete_change (assign);
}

/* Optimize ASSIGN, an assignment to LHS, on the basis that INFO
   describes all uses of LHS.  */

void
backprop::optimize_assign (gassign *assign, tree lhs, const usage_info *info)
{
  switch (gimple_assign_rhs_code (assign))
    {
    case MULT_EXPR:
    case RDIV_EXPR:
      /* If the sign of the result doesn't matter, strip sign operations
	 from both inputs.  */
      if (info->flags.ignore_sign)
	replace_assign_rhs (assign, lhs,
			    strip_sign_op (gimple_assign_rhs1 (assign)),
			    strip_sign_op (gimple_assign_rhs2 (assign)),
			    NULL_TREE);
      break;

    case COND_EXPR:
      /* If the sign of A ? B : C doesn't matter, strip sign operations
	 from both B and C.  */
      if (info->flags.ignore_sign)
	replace_assign_rhs (assign, lhs,
			    NULL_TREE,
			    strip_sign_op (gimple_assign_rhs2 (assign)),
			    strip_sign_op (gimple_assign_rhs3 (assign)));
      break;

    default:
      break;
    }
}

/* Optimize PHI, which defines VAR, on the basis that INFO describes all
   uses of the result.  */

void
backprop::optimize_phi (gphi *phi, tree var, const usage_info *info)
{
  /* If the sign of the result doesn't matter, try to strip sign operations
     from arguments.  */
  if (info->flags.ignore_sign)
    {
      basic_block bb = gimple_bb (phi);
      use_operand_p use;
      ssa_op_iter oi;
      bool replaced = false;
      FOR_EACH_PHI_ARG (use, phi, oi, SSA_OP_USE)
	{
	  /* Propagating along abnormal edges is delicate, punt for now.  */
	  const int index = PHI_ARG_INDEX_FROM_USE (use);
	  if (EDGE_PRED (bb, index)->flags & EDGE_ABNORMAL)
	    continue;

	  tree new_arg = strip_sign_op (USE_FROM_PTR (use));
	  if (new_arg)
	    {
	      if (!replaced)
		prepare_change (var);
	      if (dump_file && (dump_flags & TDF_DETAILS))
		note_replacement (phi, USE_FROM_PTR (use), new_arg);
	      replace_exp (use, new_arg);
	      replaced = true;
	    }
	}
    }
}

void
backprop::execute ()
{
  /* Phase 1: Traverse the function, making optimistic assumptions
     about any phi whose definition we haven't seen.  */
  int *postorder = XNEWVEC (int, n_basic_blocks_for_fn (m_fn));
  unsigned int postorder_num = post_order_compute (postorder, false, false);
  for (unsigned int i = 0; i < postorder_num; ++i)
    {
      process_block (BASIC_BLOCK_FOR_FN (m_fn, postorder[i]));
      bitmap_set_bit (m_visited_blocks, postorder[i]);
    }
  XDELETEVEC (postorder);

  /* Phase 2: Use the initial (perhaps overly optimistic) information
     to create a maximal fixed point solution.  */
  while (!m_worklist.is_empty ())
    process_var (pop_from_worklist ());

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\n");

  /* Phase 3: Do a reverse post-order walk, using information about
     the uses of SSA names to optimize their definitions.  */
  for (unsigned int i = m_vars.length (); i-- > 0;)
    {
      usage_info *info = m_vars[i].second;
      if (info->is_useful ())
	{
	  tree var = m_vars[i].first;
	  gimple *stmt = SSA_NAME_DEF_STMT (var);
	  if (gcall *call = dyn_cast <gcall *> (stmt))
	    optimize_builtin_call (call, var, info);
	  else if (gassign *assign = dyn_cast <gassign *> (stmt))
	    optimize_assign (assign, var, info);
	  else if (gphi *phi = dyn_cast <gphi *> (stmt))
	    optimize_phi (phi, var, info);
	}
    }

  /* Phase 4: Do a post-order walk, deleting statements that are no
     longer needed.  */
  for (unsigned int i = 0; i < m_vars.length (); ++i)
    {
      tree var = m_vars[i].first;
      if (has_zero_uses (var))
	remove_unused_var (var);
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\n");
}

const pass_data pass_data_backprop =
{
  GIMPLE_PASS, /* type */
  "backprop", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_BACKPROP, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_backprop : public gimple_opt_pass
{
public:
  pass_backprop (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_backprop, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass * clone () { return new pass_backprop (m_ctxt); }
  virtual bool gate (function *) { return flag_ssa_backprop; }
  virtual unsigned int execute (function *);

}; // class pass_backprop

unsigned int
pass_backprop::execute (function *fn)
{
  backprop (fn).execute ();
  return 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_backprop (gcc::context *ctxt)
{
  return new pass_backprop (ctxt);
}
