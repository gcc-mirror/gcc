/* Code coverage instrumentation for fuzzing.
   Copyright (C) 2015-2017 Free Software Foundation, Inc.
   Contributed by Dmitry Vyukov <dvyukov@google.com> and
   Wish Wu <wishwu007@gmail.com>

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
#include "basic-block.h"
#include "options.h"
#include "flags.h"
#include "stmt.h"
#include "gimple-iterator.h"
#include "gimple-builder.h"
#include "tree-cfg.h"
#include "tree-pass.h"
#include "tree-iterator.h"
#include "fold-const.h"
#include "stringpool.h"
#include "attribs.h"
#include "output.h"
#include "cgraph.h"
#include "asan.h"

namespace {

/* Instrument one comparison operation, which compares lhs and rhs.
   Call the instrumentation function with the comparison operand.
   For integral comparisons if exactly one of the comparison operands is
   constant, call __sanitizer_cov_trace_const_cmp* instead of
   __sanitizer_cov_trace_cmp*.  */

static void
instrument_comparison (gimple_stmt_iterator *gsi, tree lhs, tree rhs)
{
  tree type = TREE_TYPE (lhs);
  enum built_in_function fncode = END_BUILTINS;
  tree to_type = NULL_TREE;
  bool c = false;

  if (INTEGRAL_TYPE_P (type))
    {
      c = (is_gimple_min_invariant (lhs)
	   ^ is_gimple_min_invariant (rhs));
      switch (int_size_in_bytes (type))
	{
	case 1:
      	  fncode = c ? BUILT_IN_SANITIZER_COV_TRACE_CONST_CMP1
		     : BUILT_IN_SANITIZER_COV_TRACE_CMP1;
	  to_type = unsigned_char_type_node;
	  break;
	case 2:
      	  fncode = c ? BUILT_IN_SANITIZER_COV_TRACE_CONST_CMP2
		     : BUILT_IN_SANITIZER_COV_TRACE_CMP2;
	  to_type = uint16_type_node;
	  break;
	case 4:
      	  fncode = c ? BUILT_IN_SANITIZER_COV_TRACE_CONST_CMP4
		     : BUILT_IN_SANITIZER_COV_TRACE_CMP4;
	  to_type = uint32_type_node;
	  break;
	default:
      	  fncode = c ? BUILT_IN_SANITIZER_COV_TRACE_CONST_CMP8
		     : BUILT_IN_SANITIZER_COV_TRACE_CMP8;
	  to_type = uint64_type_node;
	  break;
	}
    }
  else if (SCALAR_FLOAT_TYPE_P (type))
    {
      if (TYPE_MODE (type) == TYPE_MODE (float_type_node))
	{
      	  fncode = BUILT_IN_SANITIZER_COV_TRACE_CMPF;
	  to_type = float_type_node;
	}
      else if (TYPE_MODE (type) == TYPE_MODE (double_type_node))
	{
      	  fncode = BUILT_IN_SANITIZER_COV_TRACE_CMPD;
	  to_type = double_type_node;
	}
    }

  if (to_type != NULL_TREE)
    {
      gimple_seq seq = NULL;

      if (!useless_type_conversion_p (to_type, type))
	{
	  if (TREE_CODE (lhs) == INTEGER_CST)
	    lhs = fold_convert (to_type, lhs);
	  else
	    {
	      gimple_seq_add_stmt (&seq, build_type_cast (to_type, lhs));
	      lhs = gimple_assign_lhs (gimple_seq_last_stmt (seq));
	    }

	  if (TREE_CODE (rhs) == INTEGER_CST)
	    rhs = fold_convert (to_type, rhs);
	  else
	    {
	      gimple_seq_add_stmt (&seq, build_type_cast (to_type, rhs));
	      rhs = gimple_assign_lhs (gimple_seq_last_stmt (seq));
	    }
	}

      if (c && !is_gimple_min_invariant (lhs))
	std::swap (lhs, rhs);

      tree fndecl = builtin_decl_implicit (fncode);
      gimple *gcall = gimple_build_call (fndecl, 2, lhs, rhs);
      gimple_seq_add_stmt (&seq, gcall);

      gimple_seq_set_location (seq, gimple_location (gsi_stmt (*gsi)));
      gsi_insert_seq_before (gsi, seq, GSI_SAME_STMT);
    }
}

/* Instrument switch statement.  Call __sanitizer_cov_trace_switch with
   the value of the index and array that contains number of case values,
   the bitsize of the index and the case values converted to uint64_t.  */

static void
instrument_switch (gimple_stmt_iterator *gsi, gimple *stmt, function *fun)
{
  gswitch *switch_stmt = as_a<gswitch *> (stmt);
  tree index = gimple_switch_index (switch_stmt);
  HOST_WIDE_INT size_in_bytes = int_size_in_bytes (TREE_TYPE (index));
  if (size_in_bytes == -1 || size_in_bytes > 8)
    return;

  location_t loc = gimple_location (stmt);
  unsigned i, n = gimple_switch_num_labels (switch_stmt), num = 0;
  for (i = 1; i < n; ++i)
    {
      tree label = gimple_switch_label (switch_stmt, i);

      tree low_case = CASE_LOW (label);
      if (low_case != NULL_TREE)
	num++;

      tree high_case = CASE_HIGH (label);
      if (high_case != NULL_TREE)
	num++;
    }

  tree case_array_type
   = build_array_type (build_type_variant (uint64_type_node, 1, 0),
		       build_index_type (size_int (num + 2 - 1)));

  char name[64];
  static size_t case_array_count = 0;
  ASM_GENERATE_INTERNAL_LABEL (name, "LCASEARRAY", case_array_count++);
  tree case_array_var = build_decl (loc, VAR_DECL, get_identifier (name),
				    case_array_type);
  TREE_STATIC (case_array_var) = 1;
  TREE_PUBLIC (case_array_var) = 0;
  TREE_CONSTANT (case_array_var) = 1;
  TREE_READONLY (case_array_var) = 1;
  DECL_EXTERNAL (case_array_var) = 0;
  DECL_ARTIFICIAL (case_array_var) = 1;
  DECL_IGNORED_P (case_array_var) = 1;

  vec <constructor_elt, va_gc> *v = NULL;
  vec_alloc (v, num + 2);
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE,
			  build_int_cst (uint64_type_node, num));
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE,
			  build_int_cst (uint64_type_node,
					 size_in_bytes * BITS_PER_UNIT));
  for (i = 1; i < n; ++i)
    {
      tree label = gimple_switch_label (switch_stmt, i);

      tree low_case = CASE_LOW (label);
      if (low_case != NULL_TREE)
	CONSTRUCTOR_APPEND_ELT (v, NULL_TREE,
				fold_convert (uint64_type_node, low_case));

      tree high_case = CASE_HIGH (label);
      if (high_case != NULL_TREE)
	CONSTRUCTOR_APPEND_ELT (v, NULL_TREE,
				fold_convert (uint64_type_node, high_case));
    }
  tree ctor = build_constructor (case_array_type, v);
  TREE_STATIC (ctor) = 1;
  TREE_PUBLIC (ctor) = 0;
  TREE_CONSTANT (ctor) = 1;
  TREE_READONLY (ctor) = 1;
  DECL_INITIAL (case_array_var) = ctor;
  varpool_node::finalize_decl (case_array_var);
  add_local_decl (fun, case_array_var);

  gimple_seq seq = NULL;

  if (!useless_type_conversion_p (uint64_type_node, TREE_TYPE (index)))
    {
      if (TREE_CODE (index) == INTEGER_CST)
	index = fold_convert (uint64_type_node, index);
      else
	{
	  gimple_seq_add_stmt (&seq, build_type_cast (uint64_type_node, index));
	  index = gimple_assign_lhs (gimple_seq_last_stmt (seq));
	}
    }

  tree fndecl = builtin_decl_implicit (BUILT_IN_SANITIZER_COV_TRACE_SWITCH);
  gimple *gcall = gimple_build_call (fndecl, 2, index,
				     build_fold_addr_expr (case_array_var));
  gimple_seq_add_stmt (&seq, gcall);

  gimple_seq_set_location (seq, loc);
  gsi_insert_seq_before (gsi, seq, GSI_SAME_STMT);
}

unsigned
sancov_pass (function *fun)
{
  initialize_sanitizer_builtins ();

  /* Insert callback into beginning of every BB. */
  if (flag_sanitize_coverage & SANITIZE_COV_TRACE_PC)
    {
      basic_block bb;
      tree fndecl = builtin_decl_implicit (BUILT_IN_SANITIZER_COV_TRACE_PC);
      FOR_EACH_BB_FN (bb, fun)
	{
	  gimple_stmt_iterator gsi = gsi_start_nondebug_after_labels_bb (bb);
	  if (gsi_end_p (gsi))
	    continue;
	  gimple *stmt = gsi_stmt (gsi);
	  gimple *gcall = gimple_build_call (fndecl, 0);
	  gimple_set_location (gcall, gimple_location (stmt));
	  gsi_insert_before (&gsi, gcall, GSI_SAME_STMT);
	}
    }

  /* Insert callback into every comparison related operation.  */
  if (flag_sanitize_coverage & SANITIZE_COV_TRACE_CMP)
    {
      basic_block bb;
      FOR_EACH_BB_FN (bb, fun)
	{
	  gimple_stmt_iterator gsi;
	  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	    {
	      gimple *stmt = gsi_stmt (gsi);
	      enum tree_code rhs_code;
	      switch (gimple_code (stmt))
		{
		case GIMPLE_ASSIGN:
		  rhs_code = gimple_assign_rhs_code (stmt);
		  if (TREE_CODE_CLASS (rhs_code) == tcc_comparison)
		    instrument_comparison (&gsi,
					   gimple_assign_rhs1 (stmt),
					   gimple_assign_rhs2 (stmt));
		  else if (rhs_code == COND_EXPR
			   && COMPARISON_CLASS_P (gimple_assign_rhs1 (stmt)))
		    {
		      tree cond = gimple_assign_rhs1 (stmt);
		      instrument_comparison (&gsi, TREE_OPERAND (cond, 0),
					     TREE_OPERAND (cond, 1));
		    }
		  break;
		case GIMPLE_COND:
		  instrument_comparison (&gsi,
					 gimple_cond_lhs (stmt),
					 gimple_cond_rhs (stmt));
		  break;

		case GIMPLE_SWITCH:
		  instrument_switch (&gsi, stmt, fun);
		  break;

		default:
		  break;
		}
	    }
	}
    }
  return 0;
}

template <bool O0> class pass_sancov : public gimple_opt_pass
{
public:
  pass_sancov (gcc::context *ctxt) : gimple_opt_pass (data, ctxt) {}

  static const pass_data data;
  opt_pass *
  clone ()
  {
    return new pass_sancov<O0> (m_ctxt);
  }
  virtual bool
  gate (function *)
  {
    return flag_sanitize_coverage && (!O0 || !optimize);
  }
  virtual unsigned int
  execute (function *fun)
  {
    return sancov_pass (fun);
  }
}; // class pass_sancov

template <bool O0>
const pass_data pass_sancov<O0>::data = {
  GIMPLE_PASS,		       /* type */
  O0 ? "sancov_O0" : "sancov", /* name */
  OPTGROUP_NONE,	       /* optinfo_flags */
  TV_NONE,		       /* tv_id */
  (PROP_cfg),		       /* properties_required */
  0,			       /* properties_provided */
  0,			       /* properties_destroyed */
  0,			       /* todo_flags_start */
  TODO_update_ssa,	     /* todo_flags_finish */
};

} // anon namespace

gimple_opt_pass *
make_pass_sancov (gcc::context *ctxt)
{
  return new pass_sancov<false> (ctxt);
}

gimple_opt_pass *
make_pass_sancov_O0 (gcc::context *ctxt)
{
  return new pass_sancov<true> (ctxt);
}
