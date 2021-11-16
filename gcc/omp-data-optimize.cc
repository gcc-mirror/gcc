/* OMP data optimize

   Copyright (C) 2021 Free Software Foundation, Inc.

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

/* This pass tries to optimize OMP data movement.

   The purpose is two-fold: (1) simply avoid redundant data movement, and (2)
   as an enabler for other compiler optimizations.

   Currently, the focus is on OpenACC 'kernels' constructs, but this may be
   done more generally later: other compute constructs, but also structured
   'data' constructs, for example.

   Currently, this implements:
    - Convert "copy/map(tofrom)" to "copyin/map(to)", where the variable is
      known to be dead on exit.
    - Further optimize to "private" where the variable is also known to be
      dead on entry.

   Future improvements may include:
    - Optimize mappings that do not start as "copy/map(tofrom)".
    - Optimize mappings to "copyout/map(from)" where the variable is dead on
      entry, but not exit.
    - Improved data liveness checking.
    - Etc.

   As long as we make sure to not violate user-expected OpenACC semantics, we
   may do "anything".

   The pass runs too early to use the full data flow analysis tools, so this
   uses some simplified rules.  The analysis could certainly be improved.

   A variable is dead on exit if
    1. Nothing reads it between the end of the target region and the end
       of the function.
    2. It is not global, static, external, or otherwise persistent.
    3. It is not addressable (and therefore cannot be aliased).
    4. There are no backward jumps following the target region (and therefore
       there can be no loop around the target region).

   A variable is dead on entry if the first occurrence of the variable within
   the target region is a write.  The algorithm attempts to check all possible
   code paths, but may give up where control flow is too complex. No attempt
   is made to evaluate conditionals, so it is likely that it will miss cases
   where the user might declare private manually.

   Future improvements:
    1. Allow backward jumps (loops) where the target is also after the end of
       the target region.
    2. Detect dead-on-exit variables when there is a write following the
       target region (tricky, in the presence of conditionals).
    3. Ignore reads in the "else" branch of conditionals where the target
       region is in the "then" branch.
    4. Optimize global/static/external variables that are provably dead on
       entry or exit.
   (Most of this can be achieved by unifying the two DF algorithms in this
   file; the one for scanning inside the target regions had to be made more
   capable, with propagation of live state across blocks, but that's more
   effort than I have time right now to do the rework.)
*/

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree-pass.h"
#include "options.h"
#include "tree.h"
#include "function.h"
#include "basic-block.h"
#include "gimple.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimple-walk.h"
#include "gomp-constants.h"
#include "gimple-pretty-print.h"

#define DUMP_LOC(STMT) \
  dump_user_location_t::from_location_t (OMP_CLAUSE_LOCATION (STMT)) 

/* These types track why we could *not* optimize a variable mapping.  The
   main reason for differentiating the different reasons is diagnostics.  */

enum inhibit_kinds {
  INHIBIT_NOT, // "optimize"
  INHIBIT_USE,
  INHIBIT_JMP,
  INHIBIT_BAD
};

struct inhibit_descriptor
{
  enum inhibit_kinds kind;
  gimple *stmt;
};

/* OMP Data Optimize walk state tables.  */
struct ODO_State {
  hash_map<tree, inhibit_descriptor> candidates;
  hash_set<tree> visited_labels;
  bool lhs_scanned;
};

/* These types track whether a variable can be full private, or not.

   These are ORDERED in ascending precedence; when combining two values
   (at a conditional or switch), the higher value is used.   */

enum access_kinds {
  ACCESS_NONE,      /* Variable not accessed.  */
  ACCESS_DEF_FIRST, /* Variable is defined before use.  */
  ACCESS_UNKNOWN,   /* Status is yet to be determined.  */
  ACCESS_UNSUPPORTED, /* Variable is array or reference.  */
  ACCESS_USE_FIRST  /* Variable is used without definition (live on entry).  */
};

struct ODO_BB {
  access_kinds access;
  gimple *foot_stmt;
};

struct ODO_Target_state {
  tree var;

  const void *bb_id;  /* A unique id for the BB (use a convenient pointer).  */
  ODO_BB bb;
  bool lhs_scanned;
  bool can_short_circuit;

  hash_map<const void*,ODO_BB> scanned_bb;
};

/* Classify a newly discovered variable, and add it to the candidate list.  */

static void
omp_data_optimize_add_candidate (const dump_user_location_t &loc, tree var,
				 ODO_State *state)
{
  inhibit_descriptor in;
  in.stmt = NULL;

#if __GNUC__ >= 10
# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Wformat"
#endif
  if (DECL_EXTERNAL (var))
    {
      if (dump_enabled_p () && dump_flags & TDF_DETAILS)
	dump_printf_loc (MSG_NOTE, loc,
			 " -> unsuitable variable: %T is external\n", var);

      in.kind = INHIBIT_BAD;
    }
  else if (TREE_STATIC (var))
    {
      if (dump_enabled_p () && dump_flags & TDF_DETAILS)
	dump_printf_loc (MSG_NOTE, loc,
			 " -> unsuitable variable: %T is static\n", var);

      in.kind = INHIBIT_BAD;
    }
  else if (TREE_ADDRESSABLE (var))
    {
      if (dump_enabled_p () && dump_flags & TDF_DETAILS)
	dump_printf_loc (MSG_NOTE, loc,
			 " -> unsuitable variable: %T is addressable\n",
			 var);

      in.kind = INHIBIT_BAD;
    }
  else
    {
      if (dump_enabled_p () && dump_flags & TDF_DETAILS)
	dump_printf_loc (MSG_NOTE, loc, " -> candidate variable: %T\n",
			 var);

      in.kind = INHIBIT_NOT;
    }
#if __GNUC__ >= 10
# pragma GCC diagnostic pop
#endif

  if (state->candidates.put (var, in))
    gcc_unreachable ();
}

/* Add all the variables in a gimple bind statement to the list of
   optimization candidates.  */

static void
omp_data_optimize_stmt_bind (const gbind *bind, ODO_State *state)
{
  if (dump_enabled_p () && dump_flags & TDF_DETAILS)
    dump_printf_loc (MSG_NOTE, bind, "considering scope\n");

  tree vars = gimple_bind_vars (bind);
  for (tree var = vars; var; var = TREE_CHAIN (var))
    omp_data_optimize_add_candidate (bind, var, state);
}

/* Assess a control flow statement to see if it prevents us from optimizing
   OMP variable mappings.  A conditional jump usually won't, but a loop
   means a much more complicated liveness algorithm than this would be needed
   to reason effectively.  */

static void
omp_data_optimize_stmt_jump (gimple *stmt, ODO_State *state)
{
  /* In the general case, in presence of looping/control flow, we cannot make
     any promises about (non-)uses of 'var's -- so we have to inhibit
     optimization.  */
  if (dump_enabled_p () && dump_flags & TDF_DETAILS)
    dump_printf_loc (MSG_NOTE, stmt, "loop/control encountered: %G\n", stmt);

  bool forward = false;
  switch (gimple_code (stmt))
    {
    case GIMPLE_COND:
      if (state->visited_labels.contains (gimple_cond_true_label
					  (as_a <gcond*> (stmt)))
	  && state->visited_labels.contains (gimple_cond_false_label
					     (as_a <gcond*> (stmt))))
	forward = true;
      break;
    case GIMPLE_GOTO:
      if (state->visited_labels.contains (gimple_goto_dest
					  (as_a <ggoto*> (stmt))))
	forward = true;
      break;
    case GIMPLE_SWITCH:
	{
	  gswitch *sw = as_a <gswitch*> (stmt);
	  forward = true;
	  for (unsigned i = 0; i < gimple_switch_num_labels (sw); i++)
	    if (!state->visited_labels.contains (CASE_LABEL
						 (gimple_switch_label (sw,
								       i))))
	      {
		forward = false;
		break;
	      }
	  break;
	}
    case GIMPLE_ASM:
	{
	  gasm *asm_stmt = as_a <gasm*> (stmt);
	  forward = true;
	  for (unsigned i = 0; i < gimple_asm_nlabels (asm_stmt); i++)
	    if (!state->visited_labels.contains (TREE_VALUE
						 (gimple_asm_label_op
						  (asm_stmt, i))))
	      {
		forward = false;
		break;
	      }
	  break;
	}
    default:
      gcc_unreachable ();
    }
  if (forward)
    {
      if (dump_enabled_p () && dump_flags & TDF_DETAILS)
	dump_printf_loc (MSG_NOTE, stmt,
			 " -> forward jump; candidates remain valid\n");

      return;
    }

  /* If we get here then control flow has invalidated all current optimization
     candidates.  */
  for (hash_map<tree, inhibit_descriptor>::iterator it = state->candidates.begin ();
       it != state->candidates.end ();
       ++it)
    {
      if ((*it).second.kind == INHIBIT_BAD)
	continue;

      if (dump_enabled_p () && dump_flags & TDF_DETAILS)
	dump_printf_loc (MSG_NOTE, stmt, " -> discarding candidate: %T\n",
			 (*it).first);

      /* We're walking backward: this earlier instance ("earlier" in
	 'gimple_seq' forward order) overrides what we may have had before.  */
      (*it).second.kind = INHIBIT_JMP;
      (*it).second.stmt = stmt;
    }
}

/* A helper callback for omp_data_optimize_can_be_private.
   Check if an operand matches the specific one we're looking for, and
   assess the context in which it appears.  */

static tree
omp_data_optimize_scan_target_op (tree *tp, int *walk_subtrees, void *data)
{
  struct walk_stmt_info *wi = (struct walk_stmt_info *) data;
  ODO_Target_state *state = (ODO_Target_state *)wi->info;
  tree op = *tp;

  if (wi->is_lhs && !state->lhs_scanned
      && state->bb.access != ACCESS_USE_FIRST)
    {
      /* We're at the top level of the LHS operand.  Anything we scan inside
	 (array indices etc.) should be treated as RHS.  */
      state->lhs_scanned = 1;

      /* Writes to arrays and references are unhandled, as yet.  */
      tree base = get_base_address (op);
      if (base && base != op && base == state->var)
	{
	  state->bb.access = ACCESS_UNSUPPORTED;
	  *walk_subtrees = 0;
	}
      /* Write to scalar variable.  */
      else if (op == state->var)
	{
	  state->bb.access = ACCESS_DEF_FIRST;
	  *walk_subtrees = 0;
	}
    }
  else if (op == state->var)
    {
      state->bb.access = ACCESS_USE_FIRST;
      *walk_subtrees = 0;
    }
  return NULL;
}

/* A helper callback for omp_data_optimize_can_be_private, this assesses a
   statement inside a target region to see how it affects the data flow of the
   operands.  A set of basic blocks is recorded, each with the observed access
   details for the given variable.  */

static tree
omp_data_optimize_scan_target_stmt (gimple_stmt_iterator *gsi_p,
				    bool *handled_ops_p,
				    struct walk_stmt_info *wi)
{
  ODO_Target_state *state = (ODO_Target_state *) wi->info;
  gimple *stmt = gsi_stmt (*gsi_p);

  /* If an access was found in the previous statement then we're done.  */
  if (state->bb.access != ACCESS_NONE && state->can_short_circuit)
    {
      *handled_ops_p = true;
      return (tree)1;  /* Return non-NULL, otherwise ignored.  */
    }

  /* If the first def/use is already found then don't check more operands.  */
  *handled_ops_p = state->bb.access != ACCESS_NONE;

  switch (gimple_code (stmt))
    {
    /* These will be the last statement in a basic block, and will always
       be followed by a label or the end of scope.  */
    case GIMPLE_COND:
    case GIMPLE_GOTO:
    case GIMPLE_SWITCH:
      if (state->bb.access == ACCESS_NONE)
	state->bb.access = ACCESS_UNKNOWN;
      state->bb.foot_stmt = stmt;
      state->can_short_circuit = false;
      break;

    /* asm goto statements are not necessarily followed by a label.  */
    case GIMPLE_ASM:
      if (gimple_asm_nlabels (as_a <gasm*> (stmt)) > 0)
	{
	  if (state->bb.access == ACCESS_NONE)
	    state->bb.access = ACCESS_UNKNOWN;
	  state->bb.foot_stmt = stmt;
	  state->scanned_bb.put (state->bb_id, state->bb);

	  /* Start a new fake BB using the asm string as a unique id.  */
	  state->bb_id = gimple_asm_string (as_a <gasm*> (stmt));
	  state->bb.access = ACCESS_NONE;
	  state->bb.foot_stmt = NULL;
	  state->can_short_circuit = false;
	}
      break;

    /* A label is the beginning of a new basic block, and possibly the end
       of the previous, in the case of a fall-through.  */
    case GIMPLE_LABEL:
      if (state->bb.foot_stmt == NULL)
	state->bb.foot_stmt = stmt;
      if (state->bb.access == ACCESS_NONE)
	state->bb.access = ACCESS_UNKNOWN;
      state->scanned_bb.put (state->bb_id, state->bb);

      state->bb_id = gimple_label_label (as_a <glabel*> (stmt));
      state->bb.access = ACCESS_NONE;
      state->bb.foot_stmt = NULL;
      break;

    /* These should not occur inside target regions??  */
    case GIMPLE_RETURN:
      gcc_unreachable ();

    default:
      break;
    }

  /* Now walk the operands.  */
  state->lhs_scanned = false;
  return NULL;
}

/* Check every operand under a gimple statement to see if a specific variable
   is dead on entry to an OMP TARGET statement.  If so, then we can make the
   variable mapping PRIVATE.  */

static bool
omp_data_optimize_can_be_private (tree var, gimple *target_stmt)
{
  ODO_Target_state state;
  state.var = var;
  void *root_id = var;  /* Any non-null pointer will do for the unique ID.  */
  state.bb_id = root_id;
  state.bb.access = ACCESS_NONE;
  state.bb.foot_stmt = NULL;
  state.lhs_scanned = false;
  state.can_short_circuit = true;

  struct walk_stmt_info wi;
  memset (&wi, 0, sizeof (wi));
  wi.info = &state;

  /* Walk the target region and build the BB list.  */
  gimple_seq target_body = *gimple_omp_body_ptr (target_stmt);
  walk_gimple_seq (target_body, omp_data_optimize_scan_target_stmt,
		   omp_data_optimize_scan_target_op, &wi);

  /* Calculate the liveness data for the whole region.  */
  if (state.can_short_circuit)
    ; /* state.access has the answer already.  */
  else
    {
      /* There's some control flow to navigate.  */

      /* First enter the final BB into the table.  */
      state.scanned_bb.put (state.bb_id, state.bb);

      /* Propagate the known access findings to the parent BBs.
	 
	 For each BB that does not have a known liveness value, combine
	 the liveness data from its descendent BBs, if known.  Repeat until
	 there are no more changes to make.  */
      bool changed;
      do {
	changed = false;
	for (hash_map<const void*,ODO_BB>::iterator it = state.scanned_bb.begin ();
	     it != state.scanned_bb.end ();
	     ++it)
	  {
	    ODO_BB *bb = &(*it).second;
	    tree label;
	    const void *bb_id1, *bb_id2;
	    ODO_BB *chain_bb1, *chain_bb2;
	    unsigned num_labels;

	    /* The foot statement is NULL, in the exit block.
	       Blocks that already have liveness data are done.  */
	    if (bb->foot_stmt == NULL
		|| bb->access != ACCESS_UNKNOWN)
	      continue;

	    /* If we get here then bb->access == ACCESS_UNKNOWN.  */
	    switch (gimple_code (bb->foot_stmt))
	      {
	      /* If the final statement of a block is the label statement
	         then we have a fall-through.  The liveness data can be simply
		 copied from the next block.  */
	      case GIMPLE_LABEL:
		bb_id1 = gimple_label_label (as_a <glabel*> (bb->foot_stmt));
		chain_bb1 = state.scanned_bb.get (bb_id1);
		if (chain_bb1->access != ACCESS_UNKNOWN)
		  {
		    bb->access = chain_bb1->access;
		    changed = true;
		  }
		break;

	      /* Combine the liveness data from both branches of a conditional
	         statement.  The access values are ordered such that the
		 higher value takes precedence.  */
	      case GIMPLE_COND:
		bb_id1 = gimple_cond_true_label (as_a <gcond*>
						 (bb->foot_stmt));
		bb_id2 = gimple_cond_false_label (as_a <gcond*>
						  (bb->foot_stmt));
		chain_bb1 = state.scanned_bb.get (bb_id1);
		chain_bb2 = state.scanned_bb.get (bb_id2);
		bb->access = (chain_bb1->access > chain_bb2->access
			      ? chain_bb1->access
			      : chain_bb2->access);
		if (bb->access != ACCESS_UNKNOWN)
		  changed = true;
		break;

	      /* Copy the liveness data from the destination block.  */
	      case GIMPLE_GOTO:
		bb_id1 = gimple_goto_dest (as_a <ggoto*> (bb->foot_stmt));
		chain_bb1 = state.scanned_bb.get (bb_id1);
		if (chain_bb1->access != ACCESS_UNKNOWN)
		  {
		    bb->access = chain_bb1->access;
		    changed = true;
		  }
		break;

	      /* Combine the liveness data from all the branches of a switch
		 statement.  The access values are ordered such that the
		 highest value takes precedence.  */
	      case GIMPLE_SWITCH:
		num_labels = gimple_switch_num_labels (as_a <gswitch*>
						       (bb->foot_stmt));
		bb->access = ACCESS_NONE;  /* Lowest precedence value.  */
		for (unsigned i = 0; i < num_labels; i++)
		  {
		    label = gimple_switch_label (as_a <gswitch*>
						 (bb->foot_stmt), i);
		    chain_bb1 = state.scanned_bb.get (CASE_LABEL (label));
		    bb->access = (bb->access > chain_bb1->access
				  ? bb->access
				  : chain_bb1->access);
		  }
		if (bb->access != ACCESS_UNKNOWN)
		  changed = true;
		break;

	      /* Combine the liveness data from all the branches of an asm goto
		 statement.  The access values are ordered such that the
		 highest value takes precedence.  */
	      case GIMPLE_ASM:
		num_labels = gimple_asm_nlabels (as_a <gasm*> (bb->foot_stmt));
		bb->access = ACCESS_NONE;  /* Lowest precedence value.  */
		/* Loop through all the labels and the fall-through block.  */
		for (unsigned i = 0; i < num_labels + 1; i++)
		  {
		    if (i < num_labels)
		      bb_id1 = TREE_VALUE (gimple_asm_label_op
					   (as_a <gasm*> (bb->foot_stmt), i));
		    else
		      /* The fall-through fake-BB uses the string for an ID. */
		      bb_id1 = gimple_asm_string (as_a <gasm*>
						  (bb->foot_stmt));
		    chain_bb1 = state.scanned_bb.get (bb_id1);
		    bb->access = (bb->access > chain_bb1->access
				  ? bb->access
				  : chain_bb1->access);
		  }
		if (bb->access != ACCESS_UNKNOWN)
		  changed = true;
		break;

	      /* No other statement kinds should appear as foot statements.  */
	      default:
		gcc_unreachable ();
	      }
	  }
      } while (changed);

      /* The access status should now be readable from the initial BB,
	 if one could be determined.  */
      state.bb = *state.scanned_bb.get (root_id);
    }

#if __GNUC__ >= 10
# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Wformat"
#endif
  if (dump_enabled_p () && dump_flags & TDF_DETAILS)
    {
      for (hash_map<const void*,ODO_BB>::iterator it = state.scanned_bb.begin ();
	   it != state.scanned_bb.end ();
	   ++it)
	{
	  ODO_BB *bb = &(*it).second;
	  dump_printf_loc (MSG_NOTE, bb->foot_stmt,
			   "%T is %s on entry to block ending here\n", var,
			   (bb->access == ACCESS_NONE
			    || bb->access == ACCESS_DEF_FIRST ? "dead"
			    : bb->access == ACCESS_USE_FIRST ? "live"
			    : bb->access == ACCESS_UNSUPPORTED
			    ? "unknown (unsupported op)"
			    : "unknown (complex control flow)"));
	}
      /* If the answer was found early then then the last BB to be scanned
	 will not have been entered into the table.  */
      if (state.can_short_circuit)
	dump_printf_loc (MSG_NOTE, target_stmt,
			 "%T is %s on entry to target region\n", var,
			 (state.bb.access == ACCESS_NONE
			  || state.bb.access == ACCESS_DEF_FIRST ? "dead"
			  : state.bb.access == ACCESS_USE_FIRST ? "live"
			  : state.bb.access == ACCESS_UNSUPPORTED
			  ? "unknown (unsupported op)"
			  : "unknown (complex control flow)"));
    }

  if (state.bb.access != ACCESS_DEF_FIRST
      && dump_enabled_p () && dump_flags & TDF_DETAILS)
    dump_printf_loc (MSG_NOTE, target_stmt, "%T is not suitable"
		     " for private optimization; %s\n", var,
		     (state.bb.access == ACCESS_USE_FIRST
		      ? "live on entry"
		      : state.bb.access == ACCESS_UNKNOWN
		      ? "complex control flow"
		      : "unknown reason"));
#if __GNUC__ >= 10
# pragma GCC diagnostic pop
#endif

  return state.bb.access == ACCESS_DEF_FIRST;
}

/* Inspect a tree operand, from a gimple walk, and check to see if it is a
   variable use that might mean the variable is not a suitable candidate for
   optimization in a prior target region.

   This algorithm is very basic and can be easily fooled by writes with
   subsequent reads, but it should at least err on the safe side.  */

static void
omp_data_optimize_inspect_op (tree op, ODO_State *state, bool is_lhs,
			      gimple *stmt)
{
  if (is_lhs && !state->lhs_scanned)
    {
      /* We're at the top level of the LHS operand.
         Anything we scan inside should be treated as RHS.  */
      state->lhs_scanned = 1;

      /* Writes to variables are not yet taken into account, beyond not
	 invalidating the optimization, but not everything on the
	 left-hand-side is a write (array indices, etc.), and if one element of
	 an array is written to then we should assume the rest is live.  */
      tree base = get_base_address (op);
      if (base && base == op)
	return;  /* Writes to scalars are not a "use".  */
    }

  if (!DECL_P (op))
    return;

  /* If we get here then we have found a use of a variable.  */
  tree var = op;

  inhibit_descriptor *id = state->candidates.get (var);
  if (id && id->kind != INHIBIT_BAD)
    {
      if (dump_enabled_p () && dump_flags & TDF_DETAILS)
	{
	  if (gimple_code (stmt) == GIMPLE_OMP_TARGET)
	    dump_printf_loc (MSG_NOTE, id->stmt,
			     "encountered variable use in target stmt\n");
	  else
	    dump_printf_loc (MSG_NOTE, id->stmt,
			     "encountered variable use: %G\n", stmt);
	  dump_printf_loc (MSG_NOTE, id->stmt,
			   " -> discarding candidate: %T\n", op);
	}

      /* We're walking backward: this earlier instance ("earlier" in
	 'gimple_seq' forward order) overrides what we may have had before.  */
      id->kind = INHIBIT_USE;
      id->stmt = stmt;
    }
}

/* Optimize the data mappings of a target region, where our backward gimple
   walk has identified that the variable is definitely dead on exit.  */

static void
omp_data_optimize_stmt_target (gimple *stmt, ODO_State *state)
{
  for (tree *pc = gimple_omp_target_clauses_ptr (stmt); *pc;
       pc = &OMP_CLAUSE_CHAIN (*pc))
    {
      if (OMP_CLAUSE_CODE (*pc) != OMP_CLAUSE_MAP)
	continue;

      tree var = OMP_CLAUSE_DECL (*pc);
      if (OMP_CLAUSE_MAP_KIND (*pc) == GOMP_MAP_FORCE_TOFROM
	  || OMP_CLAUSE_MAP_KIND (*pc) == GOMP_MAP_TOFROM)
	{
	/* The dump_printf_loc format code %T does not print
	   the head clause of a clause chain but the whole chain.
	   Print the last considered clause manually. */
        char *c_s_prev = NULL;
        if (dump_enabled_p ())
	  c_s_prev = print_omp_clause_to_str (*pc);

        inhibit_descriptor *id = state->candidates.get (var);
        if (!id) {
          /* The variable was not a parameter or named in any bind, so it
             must be in an external scope, and therefore live-on-exit.  */
#if __GNUC__ >= 10
# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Wformat"
#endif
          if (dump_enabled_p ())
            dump_printf_loc(MSG_MISSED_OPTIMIZATION, DUMP_LOC (*pc),
			    "%qs not optimized: %T is unsuitable"
			    " for privatization\n",
			    c_s_prev, var);
          continue;
#if __GNUC__ >= 10
# pragma GCC diagnostic pop
#endif
	    }

	  switch (id->kind)
	    {
	    case INHIBIT_NOT:  /* Don't inhibit optimization.  */

	      /* Change map type from "tofrom" to "to".  */
	      OMP_CLAUSE_SET_MAP_KIND (*pc, GOMP_MAP_TO);

	      if (dump_enabled_p ())
		{
		  char *c_s_opt = print_omp_clause_to_str (*pc);
		  dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, DUMP_LOC (*pc),
				   "%qs optimized to %qs\n", c_s_prev, c_s_opt);
		  free (c_s_prev);
		  c_s_prev = c_s_opt;
		}

	      /* Variables that are dead-on-entry and dead-on-loop can be
	         further optimized to private.  */
	      if (omp_data_optimize_can_be_private (var, stmt))
		{
		  tree c_f = (build_omp_clause
			      (OMP_CLAUSE_LOCATION (*pc),
			       OMP_CLAUSE_PRIVATE));
		  OMP_CLAUSE_DECL (c_f) = var;
		  OMP_CLAUSE_CHAIN (c_f) = OMP_CLAUSE_CHAIN (*pc);
		  //TODO Copy "implicit" flag from 'var'.
		  *pc = c_f;

		  if (dump_enabled_p ())
		    {
		      char *c_s_opt = print_omp_clause_to_str (*pc);
		      dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, DUMP_LOC (*pc),
				       "%qs further optimized to %qs\n",
				       c_s_prev, c_s_opt);
		      free (c_s_prev);
		      c_s_prev = c_s_opt;
		    }
		}
	      break;

#if __GNUC__ >= 10
# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Wformat"
#endif
	    case INHIBIT_USE:  /* Optimization inhibited by a variable use.  */
	      if (dump_enabled_p ())
		{
		  dump_printf_loc (MSG_MISSED_OPTIMIZATION, DUMP_LOC (*pc),
				   "%qs not optimized: %T used...\n",
				   c_s_prev, var);
		  dump_printf_loc (MSG_MISSED_OPTIMIZATION, id->stmt,
				   "... here\n");
		}
	      break;

	    case INHIBIT_JMP:  /* Optimization inhibited by control flow.  */
	      if (dump_enabled_p ())
		{
		  dump_printf_loc (MSG_MISSED_OPTIMIZATION, DUMP_LOC (*pc),
				   "%qs not optimized: %T disguised by"
				   " looping/control flow...\n", c_s_prev, var);
		  dump_printf_loc (MSG_MISSED_OPTIMIZATION, id->stmt,
				   "... here\n");
		}
	      break;

	    case INHIBIT_BAD:  /* Optimization inhibited by properties.  */
	      if (dump_enabled_p ())
		{
		  dump_printf_loc (MSG_MISSED_OPTIMIZATION, DUMP_LOC (*pc),
				   "%qs not optimized: %T is unsuitable"
				   " for privatization\n", c_s_prev, var);
		}
	      break;
#if __GNUC__ >= 10
# pragma GCC diagnostic pop
#endif

	    default:
	      gcc_unreachable ();
	    }

	  if (dump_enabled_p ())
	    free (c_s_prev);
	}
    }

  /* Variables used by target regions cannot be optimized from earlier
     target regions.  */
  for (tree c = *gimple_omp_target_clauses_ptr (stmt);
       c; c = OMP_CLAUSE_CHAIN (c))
    {
      /* This needs to include all the mapping clauses listed in
	 OMP_TARGET_CLAUSE_MASK in c-parser.c.  */
      if (OMP_CLAUSE_CODE (c) != OMP_CLAUSE_MAP
	  && OMP_CLAUSE_CODE (c) != OMP_CLAUSE_PRIVATE
	  && OMP_CLAUSE_CODE (c) != OMP_CLAUSE_FIRSTPRIVATE)
	continue;

      tree var = OMP_CLAUSE_DECL (c);
      omp_data_optimize_inspect_op (var, state, false, stmt);
    }
}

/* Call back for gimple walk.  Scan the statement for target regions and
   variable uses or control flow that might prevent us optimizing offload
   data copies.  */

static tree
omp_data_optimize_callback_stmt (gimple_stmt_iterator *gsi_p,
				 bool *handled_ops_p,
				 struct walk_stmt_info *wi)
{
  ODO_State *state = (ODO_State *) wi->info;

  *handled_ops_p = false;
  state->lhs_scanned = false;

  gimple *stmt = gsi_stmt (*gsi_p);

  switch (gimple_code (stmt))
    {
    /* A bind introduces a new variable scope that might include optimizable
       variables.  */
    case GIMPLE_BIND:
      omp_data_optimize_stmt_bind (as_a <gbind *> (stmt), state);
      break;

    /* Tracking labels allows us to understand control flow better.  */
    case GIMPLE_LABEL:
      state->visited_labels.add (gimple_label_label (as_a <glabel *> (stmt)));
      break;

    /* Statements that might constitute some looping/control flow pattern
       may inhibit optimization of target mappings.  */
    case GIMPLE_COND:
    case GIMPLE_GOTO:
    case GIMPLE_SWITCH:
    case GIMPLE_ASM:
      omp_data_optimize_stmt_jump (stmt, state);
      break;

    /* A target statement that will have variables for us to optimize.  */
    case GIMPLE_OMP_TARGET:
      /* For now, only look at OpenACC 'kernels' constructs.  */
      if (gimple_omp_target_kind (stmt) == GF_OMP_TARGET_KIND_OACC_KERNELS)
	{
	  omp_data_optimize_stmt_target (stmt, state);

	  /* Don't walk inside the target region; use of private variables
	     inside the private region does not stop them being private!
	     NOTE: we *do* want to walk target statement types that are not
	     (yet) handled by omp_data_optimize_stmt_target as the uses there
	     must not be missed.  */
	  // TODO add tests for mixed kernels/parallels
	  *handled_ops_p = true;
	}
      break;

    default:
      break;
    }

  return NULL;
}

/* Call back for gimple walk.  Scan the operand for variable uses.  */

static tree
omp_data_optimize_callback_op (tree *tp, int *walk_subtrees, void *data)
{
  struct walk_stmt_info *wi = (struct walk_stmt_info *) data;

  omp_data_optimize_inspect_op (*tp, (ODO_State *)wi->info, wi->is_lhs,
				wi->stmt);

  *walk_subtrees = 1;
  return NULL;
}

/* Main pass entry point.  See comments at head of file.  */

static unsigned int
omp_data_optimize (void)
{
  /* Capture the function arguments so that they can be optimized.  */
  ODO_State state;
  for (tree decl = DECL_ARGUMENTS (current_function_decl);
       decl;
       decl = DECL_CHAIN (decl))
    {
      const dump_user_location_t loc = dump_user_location_t::from_function_decl (decl);
      omp_data_optimize_add_candidate (loc, decl, &state);
    }

  /* Scan and optimize the function body, from bottom to top.  */
  struct walk_stmt_info wi;
  memset (&wi, 0, sizeof (wi));
  wi.backward = true;
  wi.info = &state;
  gimple_seq body = gimple_body (current_function_decl);
  walk_gimple_seq (body, omp_data_optimize_callback_stmt,
		   omp_data_optimize_callback_op, &wi);

  return 0;
}


namespace {

const pass_data pass_data_omp_data_optimize =
{
  GIMPLE_PASS, /* type */
  "omp_data_optimize", /* name */
  OPTGROUP_OMP, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_gimple_any, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_omp_data_optimize : public gimple_opt_pass
{
public:
  pass_omp_data_optimize (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_omp_data_optimize, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
  {
    return (flag_openacc
	    && param_openacc_kernels == OPENACC_KERNELS_DECOMPOSE);
  }
  virtual unsigned int execute (function *)
  {
    return omp_data_optimize ();
  }

}; // class pass_omp_data_optimize

} // anon namespace

gimple_opt_pass *
make_pass_omp_data_optimize (gcc::context *ctxt)
{
  return new pass_omp_data_optimize (ctxt);
}
