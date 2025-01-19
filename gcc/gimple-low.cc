/* GIMPLE lowering pass.  Converts High GIMPLE into Low GIMPLE.

   Copyright (C) 2003-2025 Free Software Foundation, Inc.

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
#include "fold-const.h"
#include "tree-nested.h"
#include "calls.h"
#include "gimple-iterator.h"
#include "gimple-low.h"
#include "predict.h"
#include "gimple-predict.h"
#include "gimple-fold.h"
#include "cgraph.h"
#include "tree-ssa.h"
#include "value-range.h"
#include "stringpool.h"
#include "tree-ssanames.h"
#include "tree-inline.h"
#include "gimple-walk.h"
#include "attribs.h"

/* The differences between High GIMPLE and Low GIMPLE are the
   following:

   1- Lexical scopes are removed (i.e., GIMPLE_BIND disappears).

   2- GIMPLE_TRY and GIMPLE_CATCH are converted to abnormal control
      flow and exception regions are built as an on-the-side region
      hierarchy (See tree-eh.cc:lower_eh_constructs).

   3- Multiple identical return statements are grouped into a single
      return and gotos to the unique return site.  */

/* Match a return statement with a label.  During lowering, we identify
   identical return statements and replace duplicates with a jump to
   the corresponding label.  */
struct return_statements_t
{
  tree label;
  greturn *stmt;
};
typedef struct return_statements_t return_statements_t;


struct lower_data
{
  /* Block the current statement belongs to.  */
  tree block;

  /* A vector of label and return statements to be moved to the end
     of the function.  */
  vec<return_statements_t> return_statements;

  /* True if the current statement cannot fall through.  */
  bool cannot_fallthru;
};

static void lower_stmt (gimple_stmt_iterator *, struct lower_data *);
static void lower_gimple_bind (gimple_stmt_iterator *, struct lower_data *);
static void lower_try_catch (gimple_stmt_iterator *, struct lower_data *);
static void lower_gimple_return (gimple_stmt_iterator *, struct lower_data *);
static void lower_builtin_setjmp (gimple_stmt_iterator *);
static void lower_builtin_posix_memalign (gimple_stmt_iterator *);
static void lower_builtin_assume_aligned (gimple_stmt_iterator *);


/* Lower the body of current_function_decl from High GIMPLE into Low
   GIMPLE.  */

static unsigned int
lower_function_body (void)
{
  struct lower_data data;
  gimple_seq body = gimple_body (current_function_decl);
  gimple_seq lowered_body;
  gimple_stmt_iterator i;
  gimple *bind;
  gimple *x;

  /* The gimplifier should've left a body of exactly one statement,
     namely a GIMPLE_BIND.  */
  gcc_assert (gimple_seq_first (body) == gimple_seq_last (body)
	      && gimple_code (gimple_seq_first_stmt (body)) == GIMPLE_BIND);

  memset (&data, 0, sizeof (data));
  data.block = DECL_INITIAL (current_function_decl);
  BLOCK_SUBBLOCKS (data.block) = NULL_TREE;
  BLOCK_CHAIN (data.block) = NULL_TREE;
  TREE_ASM_WRITTEN (data.block) = 1;
  data.return_statements.create (8);

  bind = gimple_seq_first_stmt (body);
  lowered_body = NULL;
  gimple_seq_add_stmt (&lowered_body, bind);
  i = gsi_start (lowered_body);
  lower_gimple_bind (&i, &data);

  i = gsi_last (lowered_body);

  /* If we had begin stmt markers from e.g. PCH, but this compilation
     doesn't want them, lower_stmt will have cleaned them up; we can
     now clear the flag that indicates we had them.  */
  if (!MAY_HAVE_DEBUG_MARKER_STMTS && cfun->debug_nonbind_markers)
    {
      /* This counter needs not be exact, but before lowering it will
	 most certainly be.  */
      gcc_assert (cfun->debug_marker_count == 0);
      cfun->debug_nonbind_markers = false;
    }

  /* If the function falls off the end, we need a null return statement.
     If we've already got one in the return_statements vector, we don't
     need to do anything special.  Otherwise build one by hand.  */
  bool may_fallthru = gimple_seq_may_fallthru (lowered_body);
  if (may_fallthru
      && (data.return_statements.is_empty ()
	  || (gimple_return_retval (data.return_statements.last().stmt)
	      != NULL)))
    {
      x = gimple_build_return (NULL);
      gimple_set_location (x, cfun->function_end_locus);
      gimple_set_block (x, DECL_INITIAL (current_function_decl));
      gsi_insert_after (&i, x, GSI_CONTINUE_LINKING);
      may_fallthru = false;
    }

  /* If we lowered any return statements, emit the representative
     at the end of the function.  */
  while (!data.return_statements.is_empty ())
    {
      return_statements_t t = data.return_statements.pop ();
      x = gimple_build_label (t.label);
      gsi_insert_after (&i, x, GSI_CONTINUE_LINKING);
      gsi_insert_after (&i, t.stmt, GSI_CONTINUE_LINKING);
      if (may_fallthru)
	{
	  /* Remove the line number from the representative return statement.
	     It now fills in for the fallthru too.  Failure to remove this
	     will result in incorrect results for coverage analysis.  */
	  gimple_set_location (t.stmt, UNKNOWN_LOCATION);
	  may_fallthru = false;
	}
    }

  /* Once the old body has been lowered, replace it with the new
     lowered sequence.  */
  gimple_set_body (current_function_decl, lowered_body);

  gcc_assert (data.block == DECL_INITIAL (current_function_decl));
  BLOCK_SUBBLOCKS (data.block)
    = blocks_nreverse (BLOCK_SUBBLOCKS (data.block));

  clear_block_marks (data.block);
  data.return_statements.release ();
  return 0;
}

namespace {

const pass_data pass_data_lower_cf =
{
  GIMPLE_PASS, /* type */
  "lower", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_gimple_any, /* properties_required */
  PROP_gimple_lcf, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_lower_cf : public gimple_opt_pass
{
public:
  pass_lower_cf (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_lower_cf, ctxt)
  {}

  /* opt_pass methods: */
  unsigned int execute (function *) final override
  {
    return lower_function_body ();
  }

}; // class pass_lower_cf

} // anon namespace

gimple_opt_pass *
make_pass_lower_cf (gcc::context *ctxt)
{
  return new pass_lower_cf (ctxt);
}

/* Lower sequence SEQ.  Unlike gimplification the statements are not relowered
   when they are changed -- if this has to be done, the lowering routine must
   do it explicitly.  DATA is passed through the recursion.  */

static void
lower_sequence (gimple_seq *seq, struct lower_data *data)
{
  gimple_stmt_iterator gsi;

  for (gsi = gsi_start (*seq); !gsi_end_p (gsi); )
    lower_stmt (&gsi, data);
}


/* Lower the OpenMP directive statement pointed by GSI.  DATA is
   passed through the recursion.  */

static void
lower_omp_directive (gimple_stmt_iterator *gsi, struct lower_data *data)
{
  gimple *stmt;

  stmt = gsi_stmt (*gsi);

  lower_sequence (gimple_omp_body_ptr (stmt), data);
  gsi_insert_seq_after (gsi, gimple_omp_body (stmt), GSI_CONTINUE_LINKING);
  gimple_omp_set_body (stmt, NULL);
  gsi_next (gsi);
}

/* Create an artificial FUNCTION_DECL for assumption at LOC.  */

static tree
create_assumption_fn (location_t loc)
{
  tree name = clone_function_name_numbered (current_function_decl, "_assume");
  /* Temporarily, until we determine all the arguments.  */
  tree type = build_varargs_function_type_list (boolean_type_node, NULL_TREE);
  tree decl = build_decl (loc, FUNCTION_DECL, name, type);
  TREE_STATIC (decl) = 1;
  TREE_USED (decl) = 1;
  DECL_ARTIFICIAL (decl) = 1;
  DECL_IGNORED_P (decl) = 1;
  DECL_NAMELESS (decl) = 1;
  TREE_PUBLIC (decl) = 0;
  DECL_UNINLINABLE (decl) = 1;
  DECL_EXTERNAL (decl) = 0;
  DECL_CONTEXT (decl) = NULL_TREE;
  DECL_INITIAL (decl) = make_node (BLOCK);
  tree attributes = DECL_ATTRIBUTES (current_function_decl);
  if (lookup_attribute ("noipa", attributes) == NULL)
    {
      attributes = tree_cons (get_identifier ("noipa"), NULL, attributes);
      if (lookup_attribute ("noinline", attributes) == NULL)
	attributes = tree_cons (get_identifier ("noinline"), NULL, attributes);
      if (lookup_attribute ("noclone", attributes) == NULL)
	attributes = tree_cons (get_identifier ("noclone"), NULL, attributes);
      if (lookup_attribute ("no_icf", attributes) == NULL)
	attributes = tree_cons (get_identifier ("no_icf"), NULL, attributes);
    }
  DECL_ATTRIBUTES (decl) = attributes;
  BLOCK_SUPERCONTEXT (DECL_INITIAL (decl)) = decl;
  DECL_FUNCTION_SPECIFIC_OPTIMIZATION (decl)
    = DECL_FUNCTION_SPECIFIC_OPTIMIZATION (current_function_decl);
  DECL_FUNCTION_SPECIFIC_TARGET (decl)
    = DECL_FUNCTION_SPECIFIC_TARGET (current_function_decl);
  tree t = build_decl (DECL_SOURCE_LOCATION (decl),
		       RESULT_DECL, NULL_TREE, boolean_type_node);
  DECL_ARTIFICIAL (t) = 1;
  DECL_IGNORED_P (t) = 1;
  DECL_CONTEXT (t) = decl;
  DECL_RESULT (decl) = t;
  push_struct_function (decl);
  cfun->function_end_locus = loc;
  init_tree_ssa (cfun);
  return decl;
}

struct lower_assumption_data
{
  copy_body_data id;
  tree return_false_label;
  tree guard_copy;
  auto_vec<tree> decls;
};

/* Helper function for lower_assumptions.  Find local vars and labels
   in the assumption sequence and remove debug stmts.  */

static tree
find_assumption_locals_r (gimple_stmt_iterator *gsi_p, bool *,
			  struct walk_stmt_info *wi)
{
  lower_assumption_data *data = (lower_assumption_data *) wi->info;
  gimple *stmt = gsi_stmt (*gsi_p);
  tree lhs = gimple_get_lhs (stmt);
  if (lhs && TREE_CODE (lhs) == SSA_NAME)
    {
      gcc_assert (SSA_NAME_VAR (lhs) == NULL_TREE);
      data->id.decl_map->put (lhs, NULL_TREE);
      data->decls.safe_push (lhs);
    }
  switch (gimple_code (stmt))
    {
    case GIMPLE_BIND:
      for (tree var = gimple_bind_vars (as_a <gbind *> (stmt));
	   var; var = DECL_CHAIN (var))
	if (VAR_P (var)
	    && !DECL_EXTERNAL (var)
	    && DECL_CONTEXT (var) == data->id.src_fn)
	  {
	    data->id.decl_map->put (var, var);
	    data->decls.safe_push (var);
	  }
      break;
    case GIMPLE_LABEL:
      {
	tree label = gimple_label_label (as_a <glabel *> (stmt));
	data->id.decl_map->put (label, label);
	break;
      }
    case GIMPLE_RETURN:
      /* If something in assumption tries to return from parent function,
	 if it would be reached in hypothetical evaluation, it would be UB,
	 so transform such returns into return false;  */
      {
	gimple *g = gimple_build_assign (data->guard_copy, boolean_false_node);
	gsi_insert_before (gsi_p, g, GSI_SAME_STMT);
	gimple_return_set_retval (as_a <greturn *> (stmt), data->guard_copy);
	break;
      }
    case GIMPLE_DEBUG:
      /* As assumptions won't be emitted, debug info stmts in them
	 are useless.  */
      gsi_remove (gsi_p, true);
      wi->removed_stmt = true;
      break;
    default:
      break;
    }
  return NULL_TREE;
}

/* Create a new PARM_DECL that is indentical in all respect to DECL except that
   DECL can be either a VAR_DECL, a PARM_DECL or RESULT_DECL.  The original
   DECL must come from ID->src_fn and the copy will be part of ID->dst_fn.  */

static tree
assumption_copy_decl (tree decl, copy_body_data *id)
{
  tree type = TREE_TYPE (decl);

  if (is_global_var (decl))
    return decl;

  gcc_assert (VAR_P (decl)
	      || TREE_CODE (decl) == PARM_DECL
	      || TREE_CODE (decl) == RESULT_DECL);
  if (TREE_THIS_VOLATILE (decl))
    type = build_pointer_type (type);
  tree copy = build_decl (DECL_SOURCE_LOCATION (decl),
			  PARM_DECL, DECL_NAME (decl), type);
  if (DECL_PT_UID_SET_P (decl))
    SET_DECL_PT_UID (copy, DECL_PT_UID (decl));
  TREE_THIS_VOLATILE (copy) = 0;
  if (TREE_THIS_VOLATILE (decl))
    TREE_READONLY (copy) = 1;
  else
    {
      TREE_ADDRESSABLE (copy) = TREE_ADDRESSABLE (decl);
      TREE_READONLY (copy) = TREE_READONLY (decl);
      DECL_NOT_GIMPLE_REG_P (copy) = DECL_NOT_GIMPLE_REG_P (decl);
      DECL_BY_REFERENCE (copy) = DECL_BY_REFERENCE (decl);
    }
  DECL_ARG_TYPE (copy) = type;
  ((lower_assumption_data *) id)->decls.safe_push (decl);
  return copy_decl_for_dup_finish (id, decl, copy);
}

/* Transform gotos out of the assumption into return false.  */

static tree
adjust_assumption_stmt_r (gimple_stmt_iterator *gsi_p, bool *,
			  struct walk_stmt_info *wi)
{
  lower_assumption_data *data = (lower_assumption_data *) wi->info;
  gimple *stmt = gsi_stmt (*gsi_p);
  tree lab = NULL_TREE;
  unsigned int idx = 0;
  if (gimple_code (stmt) == GIMPLE_GOTO)
    lab = gimple_goto_dest (stmt);
  else if (gimple_code (stmt) == GIMPLE_COND)
    {
     repeat:
      if (idx == 0)
	lab = gimple_cond_true_label (as_a <gcond *> (stmt));
      else
	lab = gimple_cond_false_label (as_a <gcond *> (stmt));
    }
  else if (gimple_code (stmt) == GIMPLE_LABEL)
    {
      tree label = gimple_label_label (as_a <glabel *> (stmt));
      DECL_CONTEXT (label) = current_function_decl;
    }
  if (lab)
    {
      if (!data->id.decl_map->get (lab))
	{
	  if (!data->return_false_label)
	    data->return_false_label
	      = create_artificial_label (UNKNOWN_LOCATION);
	  if (gimple_code (stmt) == GIMPLE_GOTO)
	    gimple_goto_set_dest (as_a <ggoto *> (stmt),
				  data->return_false_label);
	  else if (idx == 0)
	    gimple_cond_set_true_label (as_a <gcond *> (stmt),
					data->return_false_label);
	  else
	    gimple_cond_set_false_label (as_a <gcond *> (stmt),
					 data->return_false_label);
	}
      if (gimple_code (stmt) == GIMPLE_COND && idx == 0)
	{
	  idx = 1;
	  goto repeat;
	}
    }
  return NULL_TREE;
}

/* Adjust trees in the assumption body.  Called through walk_tree.  */

static tree
adjust_assumption_stmt_op (tree *tp, int *, void *datap)
{
  struct walk_stmt_info *wi = (struct walk_stmt_info *) datap;
  lower_assumption_data *data = (lower_assumption_data *) wi->info;
  tree t = *tp;
  tree *newt;
  switch (TREE_CODE (t))
    {
    case SSA_NAME:
      newt = data->id.decl_map->get (t);
      /* There shouldn't be SSA_NAMEs other than ones defined in the
	 assumption's body.  */
      gcc_assert (newt);
      *tp = *newt;
      break;
    case LABEL_DECL:
      newt = data->id.decl_map->get (t);
      if (newt)
	*tp = *newt;
      break;
    case VAR_DECL:
    case PARM_DECL:
    case RESULT_DECL:
      *tp = remap_decl (t, &data->id);
      if (TREE_THIS_VOLATILE (t) && *tp != t)
	{
	  *tp = build_simple_mem_ref (*tp);
	  TREE_THIS_NOTRAP (*tp) = 1;
	}
      break;
    default:
      break;
    }
  return NULL_TREE;
}

/* Lower assumption.
   The gimplifier transformed:
   .ASSUME (cond);
   into:
   [[assume (guard)]]
   {
     guard = cond;
   }
   which we should transform into:
   .ASSUME (&artificial_fn, args...);
   where artificial_fn will look like:
   bool artificial_fn (args...)
   {
     guard = cond;
     return guard;
   }
   with any debug stmts in the block removed and jumps out of
   the block or return stmts replaced with return false;  */

static void
lower_assumption (gimple_stmt_iterator *gsi, struct lower_data *data)
{
  gimple *stmt = gsi_stmt (*gsi);
  tree guard = gimple_assume_guard (stmt);
  gimple *bind = gimple_assume_body (stmt);
  location_t loc = gimple_location (stmt);
  gcc_assert (gimple_code (bind) == GIMPLE_BIND);

  lower_assumption_data lad;
  hash_map<tree, tree> decl_map;
  memset (&lad.id, 0, sizeof (lad.id));
  lad.return_false_label = NULL_TREE;
  lad.id.src_fn = current_function_decl;
  lad.id.dst_fn = create_assumption_fn (loc);
  lad.id.src_cfun = DECL_STRUCT_FUNCTION (lad.id.src_fn);
  lad.id.decl_map = &decl_map;
  lad.id.copy_decl = assumption_copy_decl;
  lad.id.transform_call_graph_edges = CB_CGE_DUPLICATE;
  lad.id.transform_parameter = true;
  lad.id.do_not_unshare = true;
  lad.id.do_not_fold = true;
  cfun->curr_properties = lad.id.src_cfun->curr_properties;
  lad.guard_copy = create_tmp_var (boolean_type_node);
  decl_map.put (lad.guard_copy, lad.guard_copy);
  decl_map.put (guard, lad.guard_copy);
  cfun->assume_function = 1;

  /* Find variables, labels and SSA_NAMEs local to the assume GIMPLE_BIND.  */
  gimple_stmt_iterator gsi2 = gsi_start (*gimple_assume_body_ptr (stmt));
  struct walk_stmt_info wi;
  memset (&wi, 0, sizeof (wi));
  wi.info = (void *) &lad;
  walk_gimple_stmt (&gsi2, find_assumption_locals_r, NULL, &wi);
  unsigned int sz = lad.decls.length ();
  for (unsigned i = 0; i < sz; ++i)
    {
      tree v = lad.decls[i];
      tree newv;
      /* SSA_NAMEs defined in the assume condition should be replaced
	 by new SSA_NAMEs in the artificial function.  */
      if (TREE_CODE (v) == SSA_NAME)
	{
	  newv = make_ssa_name (remap_type (TREE_TYPE (v), &lad.id));
	  decl_map.put (v, newv);
	}
      /* Local vars should have context and type adjusted to the
	 new artificial function.  */
      else if (VAR_P (v))
	{
	  if (is_global_var (v) && !DECL_ASSEMBLER_NAME_SET_P (v))
	    DECL_ASSEMBLER_NAME (v);
	  TREE_TYPE (v) = remap_type (TREE_TYPE (v), &lad.id);
	  DECL_CONTEXT (v) = current_function_decl;
	}
    }
  /* References to other automatic vars should be replaced by
     PARM_DECLs to the artificial function.  */
  memset (&wi, 0, sizeof (wi));
  wi.info = (void *) &lad;
  walk_gimple_stmt (&gsi2, adjust_assumption_stmt_r,
		    adjust_assumption_stmt_op, &wi);

  /* At the start prepend guard = false;  */
  gimple_seq body = NULL;
  gimple *g = gimple_build_assign (lad.guard_copy, boolean_false_node);
  gimple_seq_add_stmt (&body, g);
  gimple_seq_add_stmt (&body, bind);
  /* At the end add return guard;  */
  greturn *gr = gimple_build_return (lad.guard_copy);
  gimple_seq_add_stmt (&body, gr);
  /* If there were any jumps to labels outside of the condition,
     replace them with a jump to
     return_false_label:
     guard = false;
     return guard;  */
  if (lad.return_false_label)
    {
      g = gimple_build_label (lad.return_false_label);
      gimple_seq_add_stmt (&body, g);
      g = gimple_build_assign (lad.guard_copy, boolean_false_node);
      gimple_seq_add_stmt (&body, g);
      gr = gimple_build_return (lad.guard_copy);
      gimple_seq_add_stmt (&body, gr);
    }
  bind = gimple_build_bind (NULL_TREE, body, NULL_TREE);
  body = NULL;
  gimple_seq_add_stmt (&body, bind);
  gimple_set_body (current_function_decl, body);
  pop_cfun ();

  tree parms = NULL_TREE;
  tree parmt = void_list_node;
  auto_vec<tree, 8> vargs;
  vargs.safe_grow (1 + (lad.decls.length () - sz), true);
  /* First argument to IFN_ASSUME will be address of the
     artificial function.  */
  vargs[0] = build_fold_addr_expr (lad.id.dst_fn);
  for (unsigned i = lad.decls.length (); i > sz; --i)
    {
      tree *v = decl_map.get (lad.decls[i - 1]);
      gcc_assert (v && TREE_CODE (*v) == PARM_DECL);
      DECL_CHAIN (*v) = parms;
      parms = *v;
      parmt = tree_cons (NULL_TREE, TREE_TYPE (*v), parmt);
      /* Remaining arguments will be the variables/parameters
	 mentioned in the condition.  */
      vargs[i - sz] = lad.decls[i - 1];
      if (TREE_THIS_VOLATILE (lad.decls[i - 1]))
	{
	  TREE_ADDRESSABLE (lad.decls[i - 1]) = 1;
	  vargs[i - sz] = build_fold_addr_expr (lad.decls[i - 1]);
	}
      /* If they have gimple types, we might need to regimplify
	 them to make the IFN_ASSUME call valid.  */
      if (is_gimple_reg_type (TREE_TYPE (vargs[i - sz]))
	  && !is_gimple_val (vargs[i - sz]))
	{
	  tree t = make_ssa_name (TREE_TYPE (vargs[i - sz]));
	  g = gimple_build_assign (t, vargs[i - sz]);
	  gsi_insert_before (gsi, g, GSI_SAME_STMT);
	  vargs[i - sz] = t;
	}
    }
  DECL_ARGUMENTS (lad.id.dst_fn) = parms;
  TREE_TYPE (lad.id.dst_fn) = build_function_type (boolean_type_node, parmt);

  cgraph_node::add_new_function (lad.id.dst_fn, false);

  for (unsigned i = 0; i < sz; ++i)
    {
      tree v = lad.decls[i];
      if (TREE_CODE (v) == SSA_NAME)
	release_ssa_name (v);
    }

  data->cannot_fallthru = false;
  /* Replace GIMPLE_ASSUME statement with IFN_ASSUME call.  */
  gcall *call = gimple_build_call_internal_vec (IFN_ASSUME, vargs);
  gimple_set_location (call, loc);
  gsi_replace (gsi, call, true);
}

/* Lower statement GSI.  DATA is passed through the recursion.  We try to
   track the fallthruness of statements and get rid of unreachable return
   statements in order to prevent the EH lowering pass from adding useless
   edges that can cause bogus warnings to be issued later; this guess need
   not be 100% accurate, simply be conservative and reset cannot_fallthru
   to false if we don't know.  */

static void
lower_stmt (gimple_stmt_iterator *gsi, struct lower_data *data)
{
  gimple *stmt = gsi_stmt (*gsi);

  gimple_set_block (stmt, data->block);

  switch (gimple_code (stmt))
    {
    case GIMPLE_BIND:
      lower_gimple_bind (gsi, data);
      /* Propagate fallthruness.  */
      return;

    case GIMPLE_COND:
    case GIMPLE_GOTO:
    case GIMPLE_SWITCH:
      data->cannot_fallthru = true;
      gsi_next (gsi);
      return;

    case GIMPLE_RETURN:
      if (data->cannot_fallthru)
	{
	  gsi_remove (gsi, false);
	  /* Propagate fallthruness.  */
	}
      else
	{
	  lower_gimple_return (gsi, data);
	  data->cannot_fallthru = true;
	}
      return;

    case GIMPLE_TRY:
      if (gimple_try_kind (stmt) == GIMPLE_TRY_CATCH)
	lower_try_catch (gsi, data);
      else
	{
	  /* It must be a GIMPLE_TRY_FINALLY.  */
	  bool cannot_fallthru;
	  lower_sequence (gimple_try_eval_ptr (stmt), data);
	  cannot_fallthru = data->cannot_fallthru;

	  /* The finally clause is always executed after the try clause,
	     so if it does not fall through, then the try-finally will not
	     fall through.  Otherwise, if the try clause does not fall
	     through, then when the finally clause falls through it will
	     resume execution wherever the try clause was going.  So the
	     whole try-finally will only fall through if both the try
	     clause and the finally clause fall through.  */
	  data->cannot_fallthru = false;
	  lower_sequence (gimple_try_cleanup_ptr (stmt), data);
	  data->cannot_fallthru |= cannot_fallthru;
	  gsi_next (gsi);
	}
      return;

    case GIMPLE_EH_ELSE:
      {
	geh_else *eh_else_stmt = as_a <geh_else *> (stmt);
	lower_sequence (gimple_eh_else_n_body_ptr (eh_else_stmt), data);
	lower_sequence (gimple_eh_else_e_body_ptr (eh_else_stmt), data);
      }
      break;

    case GIMPLE_DEBUG:
      gcc_checking_assert (cfun->debug_nonbind_markers);
      /* We can't possibly have debug bind stmts before lowering, we
	 first emit them when entering SSA.  */
      gcc_checking_assert (gimple_debug_nonbind_marker_p (stmt));
      /* Propagate fallthruness.  */
      /* If the function (e.g. from PCH) had debug stmts, but they're
	 disabled for this compilation, remove them.  */
      if (!MAY_HAVE_DEBUG_MARKER_STMTS)
	gsi_remove (gsi, true);
      else
	gsi_next (gsi);
      return;

    case GIMPLE_OMP_STRUCTURED_BLOCK:
      /* These are supposed to be removed already in OMP lowering.  */
      gcc_unreachable ();

    case GIMPLE_NOP:
    case GIMPLE_ASM:
    case GIMPLE_ASSIGN:
    case GIMPLE_PREDICT:
    case GIMPLE_LABEL:
    case GIMPLE_EH_MUST_NOT_THROW:
    case GIMPLE_OMP_FOR:
    case GIMPLE_OMP_SCOPE:
    case GIMPLE_OMP_DISPATCH:
    case GIMPLE_OMP_SECTIONS:
    case GIMPLE_OMP_SECTIONS_SWITCH:
    case GIMPLE_OMP_SECTION:
    case GIMPLE_OMP_SINGLE:
    case GIMPLE_OMP_MASTER:
    case GIMPLE_OMP_MASKED:
    case GIMPLE_OMP_TASKGROUP:
    case GIMPLE_OMP_ORDERED:
    case GIMPLE_OMP_SCAN:
    case GIMPLE_OMP_CRITICAL:
    case GIMPLE_OMP_RETURN:
    case GIMPLE_OMP_ATOMIC_LOAD:
    case GIMPLE_OMP_ATOMIC_STORE:
    case GIMPLE_OMP_CONTINUE:
      break;

    case GIMPLE_CALL:
      {
	tree decl = gimple_call_fndecl (stmt);
	unsigned i;

	for (i = 0; i < gimple_call_num_args (stmt); i++)
	  {
	    tree arg = gimple_call_arg (stmt, i);
	    if (EXPR_P (arg))
	      TREE_SET_BLOCK (arg, data->block);
	  }

	if (decl
	    && fndecl_built_in_p (decl, BUILT_IN_NORMAL))
	  {
	    if (DECL_FUNCTION_CODE (decl) == BUILT_IN_SETJMP)
	      {
		lower_builtin_setjmp (gsi);
		data->cannot_fallthru = false;
		return;
	      }
	    else if (DECL_FUNCTION_CODE (decl) == BUILT_IN_POSIX_MEMALIGN
		     && flag_tree_bit_ccp
		     && gimple_builtin_call_types_compatible_p (stmt, decl))
	      {
		lower_builtin_posix_memalign (gsi);
		return;
	      }
	    else if (DECL_FUNCTION_CODE (decl) == BUILT_IN_ASSUME_ALIGNED
		     && !optimize)
	      {
		lower_builtin_assume_aligned (gsi);
		data->cannot_fallthru = false;
		gsi_next (gsi);
		return;
	      }
	  }

	if (decl && (flags_from_decl_or_type (decl) & ECF_NORETURN))
	  {
	    data->cannot_fallthru = true;
	    gsi_next (gsi);
	    return;
	  }

	if (gimple_call_internal_p (stmt, IFN_ASAN_MARK))
	  {
	    tree base = gimple_call_arg (stmt, 1);
	    gcc_checking_assert (TREE_CODE (base) == ADDR_EXPR);
	    tree decl = TREE_OPERAND (base, 0);
	    if (VAR_P (decl) && TREE_STATIC (decl))
	      {
		/* Don't poison a variable with static storage; it might have
		   gotten marked before gimplify_init_constructor promoted it
		   to static.  */
		gsi_remove (gsi, true);
		return;
	      }
	  }

	/* We delay folding of built calls from gimplification to
	   here so the IL is in consistent state for the diagnostic
	   machineries job.  */
	if (gimple_call_builtin_p (stmt))
	  fold_stmt (gsi);
      }
      break;

    case GIMPLE_OMP_PARALLEL:
    case GIMPLE_OMP_TASK:
    case GIMPLE_OMP_TARGET:
    case GIMPLE_OMP_TEAMS:
      data->cannot_fallthru = false;
      lower_omp_directive (gsi, data);
      data->cannot_fallthru = false;
      return;

    case GIMPLE_ASSUME:
      lower_assumption (gsi, data);
      return;

    case GIMPLE_TRANSACTION:
      lower_sequence (gimple_transaction_body_ptr (
			as_a <gtransaction *> (stmt)),
		      data);
      break;

    default:
      gcc_unreachable ();
    }

  data->cannot_fallthru = false;
  gsi_next (gsi);
}

/* Lower a bind_expr TSI.  DATA is passed through the recursion.  */

static void
lower_gimple_bind (gimple_stmt_iterator *gsi, struct lower_data *data)
{
  tree old_block = data->block;
  gbind *stmt = as_a <gbind *> (gsi_stmt (*gsi));
  tree new_block = gimple_bind_block (stmt);

  if (new_block)
    {
      if (new_block == old_block)
	{
	  /* The outermost block of the original function may not be the
	     outermost statement chain of the gimplified function.  So we
	     may see the outermost block just inside the function.  */
	  gcc_assert (new_block == DECL_INITIAL (current_function_decl));
	  new_block = NULL;
	}
      else
	{
	  /* We do not expect to handle duplicate blocks.  */
	  gcc_assert (!TREE_ASM_WRITTEN (new_block));
	  TREE_ASM_WRITTEN (new_block) = 1;

	  /* Block tree may get clobbered by inlining.  Normally this would
	     be fixed in rest_of_decl_compilation using block notes, but
	     since we are not going to emit them, it is up to us.  */
	  BLOCK_CHAIN (new_block) = BLOCK_SUBBLOCKS (old_block);
	  BLOCK_SUBBLOCKS (old_block) = new_block;
	  BLOCK_SUBBLOCKS (new_block) = NULL_TREE;
	  BLOCK_SUPERCONTEXT (new_block) = old_block;

	  data->block = new_block;
	}
    }

  record_vars (gimple_bind_vars (stmt));

  /* Scrap DECL_CHAIN up to BLOCK_VARS to ease GC after we no longer
     need gimple_bind_vars.  */
  tree next;
  /* BLOCK_VARS and gimple_bind_vars share a common sub-chain.  Find
     it by marking all BLOCK_VARS.  */
  if (gimple_bind_block (stmt))
    for (tree t = BLOCK_VARS (gimple_bind_block (stmt)); t; t = DECL_CHAIN (t))
      TREE_VISITED (t) = 1;
  for (tree var = gimple_bind_vars (stmt);
       var && ! TREE_VISITED (var); var = next)
    {
      next = DECL_CHAIN (var);
      DECL_CHAIN (var) = NULL_TREE;
    }
  /* Unmark BLOCK_VARS.  */
  if (gimple_bind_block (stmt))
    for (tree t = BLOCK_VARS (gimple_bind_block (stmt)); t; t = DECL_CHAIN (t))
      TREE_VISITED (t) = 0;

  lower_sequence (gimple_bind_body_ptr (stmt), data);

  if (new_block)
    {
      gcc_assert (data->block == new_block);

      BLOCK_SUBBLOCKS (new_block)
	= blocks_nreverse (BLOCK_SUBBLOCKS (new_block));
      data->block = old_block;
    }

  /* The GIMPLE_BIND no longer carries any useful information -- kill it.  */
  gsi_insert_seq_before (gsi, gimple_bind_body (stmt), GSI_SAME_STMT);
  gsi_remove (gsi, false);
}

/* Same as above, but for a GIMPLE_TRY_CATCH.  */

static void
lower_try_catch (gimple_stmt_iterator *gsi, struct lower_data *data)
{
  bool cannot_fallthru;
  gimple *stmt = gsi_stmt (*gsi);
  gimple_stmt_iterator i;

  /* We don't handle GIMPLE_TRY_FINALLY.  */
  gcc_assert (gimple_try_kind (stmt) == GIMPLE_TRY_CATCH);

  lower_sequence (gimple_try_eval_ptr (stmt), data);
  cannot_fallthru = data->cannot_fallthru;

  i = gsi_start (*gimple_try_cleanup_ptr (stmt));
  switch (gimple_code (gsi_stmt (i)))
    {
    case GIMPLE_CATCH:
      /* We expect to see a sequence of GIMPLE_CATCH stmts, each with a
	 catch expression and a body.  The whole try/catch may fall
	 through iff any of the catch bodies falls through.  */
      for (; !gsi_end_p (i); gsi_next (&i))
	{
	  data->cannot_fallthru = false;
	  lower_sequence (gimple_catch_handler_ptr (
                            as_a <gcatch *> (gsi_stmt (i))),
			  data);
	  if (!data->cannot_fallthru)
	    cannot_fallthru = false;
	}
      break;

    case GIMPLE_EH_FILTER:
      /* The exception filter expression only matters if there is an
	 exception.  If the exception does not match EH_FILTER_TYPES,
	 we will execute EH_FILTER_FAILURE, and we will fall through
	 if that falls through.  If the exception does match
	 EH_FILTER_TYPES, the stack unwinder will continue up the
	 stack, so we will not fall through.  We don't know whether we
	 will throw an exception which matches EH_FILTER_TYPES or not,
	 so we just ignore EH_FILTER_TYPES and assume that we might
	 throw an exception which doesn't match.  */
      data->cannot_fallthru = false;
      lower_sequence (gimple_eh_filter_failure_ptr (gsi_stmt (i)), data);
      if (!data->cannot_fallthru)
	cannot_fallthru = false;
      break;

    case GIMPLE_DEBUG:
      gcc_checking_assert (gimple_debug_begin_stmt_p (stmt));
      break;

    default:
      /* This case represents statements to be executed when an
	 exception occurs.  Those statements are implicitly followed
	 by a GIMPLE_RESX to resume execution after the exception.  So
	 in this case the try/catch never falls through.  */
      data->cannot_fallthru = false;
      lower_sequence (gimple_try_cleanup_ptr (stmt), data);
      break;
    }

  data->cannot_fallthru = cannot_fallthru;
  gsi_next (gsi);
}


/* Try to determine whether a TRY_CATCH expression can fall through.
   This is a subroutine of gimple_stmt_may_fallthru.  */

static bool
gimple_try_catch_may_fallthru (gtry *stmt)
{
  gimple_stmt_iterator i;

  /* We don't handle GIMPLE_TRY_FINALLY.  */
  gcc_assert (gimple_try_kind (stmt) == GIMPLE_TRY_CATCH);

  /* If the TRY block can fall through, the whole TRY_CATCH can
     fall through.  */
  if (gimple_seq_may_fallthru (gimple_try_eval (stmt)))
    return true;

  i = gsi_start (*gimple_try_cleanup_ptr (stmt));
  switch (gimple_code (gsi_stmt (i)))
    {
    case GIMPLE_CATCH:
      /* We expect to see a sequence of GIMPLE_CATCH stmts, each with a
	 catch expression and a body.  The whole try/catch may fall
	 through iff any of the catch bodies falls through.  */
      for (; !gsi_end_p (i); gsi_next (&i))
	{
	  if (gimple_seq_may_fallthru (gimple_catch_handler (
					 as_a <gcatch *> (gsi_stmt (i)))))
	    return true;
	}
      return false;

    case GIMPLE_EH_FILTER:
      /* The exception filter expression only matters if there is an
	 exception.  If the exception does not match EH_FILTER_TYPES,
	 we will execute EH_FILTER_FAILURE, and we will fall through
	 if that falls through.  If the exception does match
	 EH_FILTER_TYPES, the stack unwinder will continue up the
	 stack, so we will not fall through.  We don't know whether we
	 will throw an exception which matches EH_FILTER_TYPES or not,
	 so we just ignore EH_FILTER_TYPES and assume that we might
	 throw an exception which doesn't match.  */
      return gimple_seq_may_fallthru (gimple_eh_filter_failure (gsi_stmt (i)));

    default:
      /* This case represents statements to be executed when an
	 exception occurs.  Those statements are implicitly followed
	 by a GIMPLE_RESX to resume execution after the exception.  So
	 in this case the try/catch never falls through.  */
      return false;
    }
}


/* Try to determine if we can continue executing the statement
   immediately following STMT.  This guess need not be 100% accurate;
   simply be conservative and return true if we don't know.  This is
   used only to avoid stupidly generating extra code. If we're wrong,
   we'll just delete the extra code later.  */

bool
gimple_stmt_may_fallthru (gimple *stmt)
{
  if (!stmt)
    return true;

  switch (gimple_code (stmt))
    {
    case GIMPLE_GOTO:
    case GIMPLE_RETURN:
    case GIMPLE_RESX:
      /* Easy cases.  If the last statement of the seq implies
	 control transfer, then we can't fall through.  */
      return false;

    case GIMPLE_SWITCH:
      /* Switch has already been lowered and represents a branch
	 to a selected label and hence can't fall through.  */
      return false;

    case GIMPLE_COND:
      /* GIMPLE_COND's are already lowered into a two-way branch.  They
	 can't fall through.  */
      return false;

    case GIMPLE_BIND:
      return gimple_seq_may_fallthru (
	       gimple_bind_body (as_a <gbind *> (stmt)));

    case GIMPLE_TRY:
      if (gimple_try_kind (stmt) == GIMPLE_TRY_CATCH)
        return gimple_try_catch_may_fallthru (as_a <gtry *> (stmt));

      /* It must be a GIMPLE_TRY_FINALLY.  */

      /* The finally clause is always executed after the try clause,
	 so if it does not fall through, then the try-finally will not
	 fall through.  Otherwise, if the try clause does not fall
	 through, then when the finally clause falls through it will
	 resume execution wherever the try clause was going.  So the
	 whole try-finally will only fall through if both the try
	 clause and the finally clause fall through.  */
      return (gimple_seq_may_fallthru (gimple_try_eval (stmt))
	      && gimple_seq_may_fallthru (gimple_try_cleanup (stmt)));

    case GIMPLE_EH_ELSE:
      {
	geh_else *eh_else_stmt = as_a <geh_else *> (stmt);
	return (gimple_seq_may_fallthru (gimple_eh_else_n_body (eh_else_stmt))
		|| gimple_seq_may_fallthru (gimple_eh_else_e_body (
					      eh_else_stmt)));
      }

    case GIMPLE_CALL:
      /* Functions that do not return do not fall through.  */
      return !gimple_call_noreturn_p (stmt);

    default:
      return true;
    }
}


/* Same as gimple_stmt_may_fallthru, but for the gimple sequence SEQ.  */

bool
gimple_seq_may_fallthru (gimple_seq seq)
{
  return gimple_stmt_may_fallthru (gimple_seq_last_nondebug_stmt (seq));
}


/* Lower a GIMPLE_RETURN GSI.  DATA is passed through the recursion.  */

static void
lower_gimple_return (gimple_stmt_iterator *gsi, struct lower_data *data)
{
  greturn *stmt = as_a <greturn *> (gsi_stmt (*gsi));
  gimple *t;
  int i;
  return_statements_t tmp_rs;

  /* Match this up with an existing return statement that's been created.  */
  for (i = data->return_statements.length () - 1;
       i >= 0; i--)
    {
      tmp_rs = data->return_statements[i];

      if (gimple_return_retval (stmt) == gimple_return_retval (tmp_rs.stmt))
	{
	  /* Remove the line number from the representative return statement.
	     It now fills in for many such returns.  Failure to remove this
	     will result in incorrect results for coverage analysis.  */
	  gimple_set_location (tmp_rs.stmt, UNKNOWN_LOCATION);

	  goto found;
	}
    }

  /* Not found.  Create a new label and record the return statement.  */
  tmp_rs.label = create_artificial_label (cfun->function_end_locus);
  tmp_rs.stmt = stmt;
  data->return_statements.safe_push (tmp_rs);

  /* Generate a goto statement and remove the return statement.  */
 found:
  /* When not optimizing, make sure user returns are preserved.  */
  if (!optimize && gimple_has_location (stmt))
    DECL_ARTIFICIAL (tmp_rs.label) = 0;
  t = gimple_build_goto (tmp_rs.label);
  /* location includes block.  */
  gimple_set_location (t, gimple_location (stmt));
  gsi_insert_before (gsi, t, GSI_SAME_STMT);
  gsi_remove (gsi, false);
}

/* Lower a __builtin_setjmp GSI.

   __builtin_setjmp is passed a pointer to an array of five words (not
   all will be used on all machines).  It operates similarly to the C
   library function of the same name, but is more efficient.

   It is lowered into 2 other builtins, namely __builtin_setjmp_setup,
   __builtin_setjmp_receiver.

   After full lowering, the body of the function should look like:

    {
      int D.1844;
      int D.2844;

      [...]

      __builtin_setjmp_setup (&buf, &<D1847>);
      D.1844 = 0;
      goto <D1846>;
      <D1847>:;
      __builtin_setjmp_receiver (&<D1847>);
      D.1844 = 1;
      <D1846>:;
      if (D.1844 == 0) goto <D1848>; else goto <D1849>;

      [...]

      __builtin_setjmp_setup (&buf, &<D2847>);
      D.2844 = 0;
      goto <D2846>;
      <D2847>:;
      __builtin_setjmp_receiver (&<D2847>);
      D.2844 = 1;
      <D2846>:;
      if (D.2844 == 0) goto <D2848>; else goto <D2849>;

      [...]

      <D3850>:;
      return;
    }

   During cfg creation an extra per-function (or per-OpenMP region)
   block with ABNORMAL_DISPATCHER internal call will be added, unique
   destination of all the abnormal call edges and the unique source of
   all the abnormal edges to the receivers, thus keeping the complexity
   explosion localized.  */

static void
lower_builtin_setjmp (gimple_stmt_iterator *gsi)
{
  gimple *stmt = gsi_stmt (*gsi);
  location_t loc = gimple_location (stmt);
  tree cont_label = create_artificial_label (loc);
  tree next_label = create_artificial_label (loc);
  tree dest, t, arg;
  gimple *g;

  /* __builtin_setjmp_{setup,receiver} aren't ECF_RETURNS_TWICE and for RTL
     these builtins are modelled as non-local label jumps to the label
     that is passed to these two builtins, so pretend we have a non-local
     label during GIMPLE passes too.  See PR60003.  */
  cfun->has_nonlocal_label = 1;

  /* NEXT_LABEL is the label __builtin_longjmp will jump to.  Its address is
     passed to both __builtin_setjmp_setup and __builtin_setjmp_receiver.  */
  FORCED_LABEL (next_label) = 1;

  tree orig_dest = dest = gimple_call_lhs (stmt);
  if (orig_dest && TREE_CODE (orig_dest) == SSA_NAME)
    dest = create_tmp_reg (TREE_TYPE (orig_dest));

  /* Build '__builtin_setjmp_setup (BUF, NEXT_LABEL)' and insert.  */
  arg = build_addr (next_label);
  t = builtin_decl_implicit (BUILT_IN_SETJMP_SETUP);
  g = gimple_build_call (t, 2, gimple_call_arg (stmt, 0), arg);
  /* location includes block.  */
  gimple_set_location (g, loc);
  gsi_insert_before (gsi, g, GSI_SAME_STMT);

  /* Build 'DEST = 0' and insert.  */
  if (dest)
    {
      g = gimple_build_assign (dest, build_zero_cst (TREE_TYPE (dest)));
      gimple_set_location (g, loc);
      gsi_insert_before (gsi, g, GSI_SAME_STMT);
    }

  /* Build 'goto CONT_LABEL' and insert.  */
  g = gimple_build_goto (cont_label);
  gsi_insert_before (gsi, g, GSI_SAME_STMT);

  /* Build 'NEXT_LABEL:' and insert.  */
  g = gimple_build_label (next_label);
  gsi_insert_before (gsi, g, GSI_SAME_STMT);

  /* Build '__builtin_setjmp_receiver (NEXT_LABEL)' and insert.  */
  arg = build_addr (next_label);
  t = builtin_decl_implicit (BUILT_IN_SETJMP_RECEIVER);
  g = gimple_build_call (t, 1, arg);
  gimple_set_location (g, loc);
  gsi_insert_before (gsi, g, GSI_SAME_STMT);

  /* Build 'DEST = 1' and insert.  */
  if (dest)
    {
      g = gimple_build_assign (dest, fold_convert_loc (loc, TREE_TYPE (dest),
						       integer_one_node));
      gimple_set_location (g, loc);
      gsi_insert_before (gsi, g, GSI_SAME_STMT);
    }

  /* Build 'CONT_LABEL:' and insert.  */
  g = gimple_build_label (cont_label);
  gsi_insert_before (gsi, g, GSI_SAME_STMT);

  /* Build orig_dest = dest if necessary.  */
  if (dest != orig_dest)
    {
      g = gimple_build_assign (orig_dest, dest);
      gsi_insert_before (gsi, g, GSI_SAME_STMT);
    }

  /* Remove the call to __builtin_setjmp.  */
  gsi_remove (gsi, false);
}

/* Lower calls to posix_memalign to
     res = posix_memalign (ptr, align, size);
     if (res == 0)
       *ptr = __builtin_assume_aligned (*ptr, align);
   or to
     void *tem;
     res = posix_memalign (&tem, align, size);
     if (res == 0)
       ptr = __builtin_assume_aligned (tem, align);
   in case the first argument was &ptr.  That way we can get at the
   alignment of the heap pointer in CCP.  */

static void
lower_builtin_posix_memalign (gimple_stmt_iterator *gsi)
{
  gimple *stmt, *call = gsi_stmt (*gsi);
  tree pptr = gimple_call_arg (call, 0);
  tree align = gimple_call_arg (call, 1);
  tree res = gimple_call_lhs (call);
  tree ptr = create_tmp_reg (ptr_type_node);
  if (TREE_CODE (pptr) == ADDR_EXPR)
    {
      tree tem = create_tmp_var (ptr_type_node);
      TREE_ADDRESSABLE (tem) = 1;
      gimple_call_set_arg (call, 0, build_fold_addr_expr (tem));
      stmt = gimple_build_assign (ptr, tem);
    }
  else
    stmt = gimple_build_assign (ptr,
				fold_build2 (MEM_REF, ptr_type_node, pptr,
					     build_int_cst (ptr_type_node, 0)));
  if (res == NULL_TREE)
    {
      res = create_tmp_reg (integer_type_node);
      gimple_call_set_lhs (call, res);
    }
  tree align_label = create_artificial_label (UNKNOWN_LOCATION);
  tree noalign_label = create_artificial_label (UNKNOWN_LOCATION);
  gimple *cond = gimple_build_cond (EQ_EXPR, res, integer_zero_node,
				   align_label, noalign_label);
  gsi_insert_after (gsi, cond, GSI_NEW_STMT);
  gsi_insert_after (gsi, gimple_build_label (align_label), GSI_NEW_STMT);
  gsi_insert_after (gsi, stmt, GSI_NEW_STMT);
  stmt = gimple_build_call (builtin_decl_implicit (BUILT_IN_ASSUME_ALIGNED),
			    2, ptr, align);
  gimple_call_set_lhs (stmt, ptr);
  gsi_insert_after (gsi, stmt, GSI_NEW_STMT);
  stmt = gimple_build_assign (fold_build2 (MEM_REF, ptr_type_node, pptr,
					   build_int_cst (ptr_type_node, 0)),
			      ptr);
  gsi_insert_after (gsi, stmt, GSI_NEW_STMT);
  gsi_insert_after (gsi, gimple_build_label (noalign_label), GSI_NEW_STMT);
}

/* Lower calls to __builtin_assume_aligned when not optimizing.  */

static void
lower_builtin_assume_aligned (gimple_stmt_iterator *gsi)
{
  gcall *call = as_a <gcall *> (gsi_stmt (*gsi));

  tree lhs = gimple_call_lhs (call);
  if (!lhs || !POINTER_TYPE_P (TREE_TYPE (lhs)) || TREE_CODE (lhs) != SSA_NAME)
    return;

  tree align = gimple_call_arg (call, 1);
  tree misalign = (gimple_call_num_args (call) > 2
		   ? gimple_call_arg (call, 2) : NULL_TREE);
  if (!tree_fits_uhwi_p (align)
      || (misalign && !tree_fits_uhwi_p (misalign)))
    return;

  unsigned aligni = TREE_INT_CST_LOW (align);
  unsigned misaligni = misalign ? TREE_INT_CST_LOW (misalign) : 0;
  if (aligni <= 1
      || (aligni & (aligni - 1)) != 0
      || (misaligni & ~(aligni - 1)) != 0)
    return;

  /* For lowering we simply transfer alignment information to the
     result and leave the call otherwise unchanged, it will be elided
     at RTL expansion time.  */
  ptr_info_def *pi = get_ptr_info (lhs);
  set_ptr_info_alignment (pi, aligni, misaligni);
}


/* Record the variables in VARS into function FN.  */

void
record_vars_into (tree vars, tree fn)
{
  for (; vars; vars = DECL_CHAIN (vars))
    {
      tree var = vars;

      /* BIND_EXPRs contains also function/type/constant declarations
         we don't need to care about.  */
      if (!VAR_P (var))
	continue;

      /* Nothing to do in this case.  */
      if (DECL_EXTERNAL (var))
	continue;

      /* Record the variable.  */
      add_local_decl (DECL_STRUCT_FUNCTION (fn), var);
    }
}


/* Record the variables in VARS into current_function_decl.  */

void
record_vars (tree vars)
{
  record_vars_into (vars, current_function_decl);
}
