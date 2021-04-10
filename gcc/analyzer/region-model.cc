/* Classes for modeling the state of memory.
   Copyright (C) 2019-2021 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "function.h"
#include "basic-block.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "diagnostic-core.h"
#include "graphviz.h"
#include "options.h"
#include "cgraph.h"
#include "tree-dfa.h"
#include "stringpool.h"
#include "convert.h"
#include "target.h"
#include "fold-const.h"
#include "tree-pretty-print.h"
#include "diagnostic-color.h"
#include "diagnostic-metadata.h"
#include "tristate.h"
#include "bitmap.h"
#include "selftest.h"
#include "function.h"
#include "json.h"
#include "analyzer/analyzer.h"
#include "analyzer/analyzer-logging.h"
#include "ordered-hash-map.h"
#include "options.h"
#include "cgraph.h"
#include "cfg.h"
#include "digraph.h"
#include "analyzer/supergraph.h"
#include "sbitmap.h"
#include "analyzer/call-string.h"
#include "analyzer/program-point.h"
#include "analyzer/store.h"
#include "analyzer/region-model.h"
#include "analyzer/constraint-manager.h"
#include "diagnostic-event-id.h"
#include "analyzer/sm.h"
#include "diagnostic-event-id.h"
#include "analyzer/sm.h"
#include "analyzer/pending-diagnostic.h"
#include "analyzer/region-model-reachability.h"
#include "analyzer/analyzer-selftests.h"
#include "stor-layout.h"
#include "attribs.h"

#if ENABLE_ANALYZER

namespace ana {

/* Dump T to PP in language-independent form, for debugging/logging/dumping
   purposes.  */

void
dump_tree (pretty_printer *pp, tree t)
{
  dump_generic_node (pp, t, 0, TDF_SLIM, 0);
}

/* Dump T to PP in language-independent form in quotes, for
   debugging/logging/dumping purposes.  */

void
dump_quoted_tree (pretty_printer *pp, tree t)
{
  pp_begin_quote (pp, pp_show_color (pp));
  dump_tree (pp, t);
  pp_end_quote (pp, pp_show_color (pp));
}

/* Equivalent to pp_printf (pp, "%qT", t), to avoid nesting pp_printf
   calls within other pp_printf calls.

   default_tree_printer handles 'T' and some other codes by calling
     dump_generic_node (pp, t, 0, TDF_SLIM, 0);
   dump_generic_node calls pp_printf in various places, leading to
   garbled output.

   Ideally pp_printf could be made to be reentrant, but in the meantime
   this function provides a workaround.  */

void
print_quoted_type (pretty_printer *pp, tree t)
{
  pp_begin_quote (pp, pp_show_color (pp));
  dump_generic_node (pp, t, 0, TDF_SLIM, 0);
  pp_end_quote (pp, pp_show_color (pp));
}

/* class region_model.  */

/* Ctor for region_model: construct an "empty" model.  */

region_model::region_model (region_model_manager *mgr)
: m_mgr (mgr), m_store (), m_current_frame (NULL)
{
  m_constraints = new constraint_manager (mgr);
}

/* region_model's copy ctor.  */

region_model::region_model (const region_model &other)
: m_mgr (other.m_mgr), m_store (other.m_store),
  m_constraints (new constraint_manager (*other.m_constraints)),
  m_current_frame (other.m_current_frame)
{
}

/* region_model's dtor.  */

region_model::~region_model ()
{
  delete m_constraints;
}

/* region_model's assignment operator.  */

region_model &
region_model::operator= (const region_model &other)
{
  /* m_mgr is const.  */
  gcc_assert (m_mgr == other.m_mgr);

  m_store = other.m_store;

  delete m_constraints;
  m_constraints = new constraint_manager (*other.m_constraints);

  m_current_frame = other.m_current_frame;

  return *this;
}

/* Equality operator for region_model.

   Amongst other things this directly compares the stores and the constraint
   managers, so for this to be meaningful both this and OTHER should
   have been canonicalized.  */

bool
region_model::operator== (const region_model &other) const
{
  /* We can only compare instances that use the same manager.  */
  gcc_assert (m_mgr == other.m_mgr);

  if (m_store != other.m_store)
    return false;

  if (*m_constraints != *other.m_constraints)
    return false;

  if (m_current_frame != other.m_current_frame)
    return false;

  gcc_checking_assert (hash () == other.hash ());

  return true;
}

/* Generate a hash value for this region_model.  */

hashval_t
region_model::hash () const
{
  hashval_t result = m_store.hash ();
  result ^= m_constraints->hash ();
  return result;
}

/* Dump a representation of this model to PP, showing the
   stack, the store, and any constraints.
   Use SIMPLE to control how svalues and regions are printed.  */

void
region_model::dump_to_pp (pretty_printer *pp, bool simple,
			  bool multiline) const
{
  /* Dump stack.  */
  pp_printf (pp, "stack depth: %i", get_stack_depth ());
  if (multiline)
    pp_newline (pp);
  else
    pp_string (pp, " {");
  for (const frame_region *iter_frame = m_current_frame; iter_frame;
       iter_frame = iter_frame->get_calling_frame ())
    {
      if (multiline)
	pp_string (pp, "  ");
      else if (iter_frame != m_current_frame)
	pp_string (pp, ", ");
      pp_printf (pp, "frame (index %i): ", iter_frame->get_index ());
      iter_frame->dump_to_pp (pp, simple);
      if (multiline)
	pp_newline (pp);
    }
  if (!multiline)
    pp_string (pp, "}");

  /* Dump store.  */
  if (!multiline)
    pp_string (pp, ", {");
  m_store.dump_to_pp (pp, simple, multiline,
		      m_mgr->get_store_manager ());
  if (!multiline)
    pp_string (pp, "}");

  /* Dump constraints.  */
  pp_string (pp, "constraint_manager:");
  if (multiline)
    pp_newline (pp);
  else
    pp_string (pp, " {");
  m_constraints->dump_to_pp (pp, multiline);
  if (!multiline)
    pp_string (pp, "}");
}

/* Dump a representation of this model to FILE.  */

void
region_model::dump (FILE *fp, bool simple, bool multiline) const
{
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  pp_show_color (&pp) = pp_show_color (global_dc->printer);
  pp.buffer->stream = fp;
  dump_to_pp (&pp, simple, multiline);
  pp_newline (&pp);
  pp_flush (&pp);
}

/* Dump a multiline representation of this model to stderr.  */

DEBUG_FUNCTION void
region_model::dump (bool simple) const
{
  dump (stderr, simple, true);
}

/* Dump a multiline representation of this model to stderr.  */

DEBUG_FUNCTION void
region_model::debug () const
{
  dump (true);
}

/* Canonicalize the store and constraints, to maximize the chance of
   equality between region_model instances.  */

void
region_model::canonicalize ()
{
  m_store.canonicalize (m_mgr->get_store_manager ());
  m_constraints->canonicalize ();
}

/* Return true if this region_model is in canonical form.  */

bool
region_model::canonicalized_p () const
{
  region_model copy (*this);
  copy.canonicalize ();
  return *this == copy;
}

/* See the comment for store::loop_replay_fixup.  */

void
region_model::loop_replay_fixup (const region_model *dst_state)
{
  m_store.loop_replay_fixup (dst_state->get_store (), m_mgr);
}

/* A subclass of pending_diagnostic for complaining about uses of
   poisoned values.  */

class poisoned_value_diagnostic
: public pending_diagnostic_subclass<poisoned_value_diagnostic>
{
public:
  poisoned_value_diagnostic (tree expr, enum poison_kind pkind)
  : m_expr (expr), m_pkind (pkind)
  {}

  const char *get_kind () const FINAL OVERRIDE { return "poisoned_value_diagnostic"; }

  bool operator== (const poisoned_value_diagnostic &other) const
  {
    return m_expr == other.m_expr;
  }

  bool emit (rich_location *rich_loc) FINAL OVERRIDE
  {
    switch (m_pkind)
      {
      default:
	gcc_unreachable ();
      case POISON_KIND_FREED:
	{
	  diagnostic_metadata m;
	  m.add_cwe (416); /* "CWE-416: Use After Free".  */
	  return warning_meta (rich_loc, m,
			       OPT_Wanalyzer_use_after_free,
			       "use after %<free%> of %qE",
			       m_expr);
	}
	break;
      case POISON_KIND_POPPED_STACK:
	{
	  /* TODO: which CWE?  */
	  return warning_at
	    (rich_loc,
	     OPT_Wanalyzer_use_of_pointer_in_stale_stack_frame,
	     "dereferencing pointer %qE to within stale stack frame",
	     m_expr);
	}
	break;
      }
  }

  label_text describe_final_event (const evdesc::final_event &ev) FINAL OVERRIDE
  {
    switch (m_pkind)
      {
      default:
	gcc_unreachable ();
      case POISON_KIND_FREED:
	return ev.formatted_print ("use after %<free%> of %qE here",
				   m_expr);
      case POISON_KIND_POPPED_STACK:
	return ev.formatted_print
	  ("dereferencing pointer %qE to within stale stack frame",
	   m_expr);
      }
  }

private:
  tree m_expr;
  enum poison_kind m_pkind;
};

/* A subclass of pending_diagnostic for complaining about shifts
   by negative counts.  */

class shift_count_negative_diagnostic
: public pending_diagnostic_subclass<shift_count_negative_diagnostic>
{
public:
  shift_count_negative_diagnostic (const gassign *assign, tree count_cst)
  : m_assign (assign), m_count_cst (count_cst)
  {}

  const char *get_kind () const FINAL OVERRIDE
  {
    return "shift_count_negative_diagnostic";
  }

  bool operator== (const shift_count_negative_diagnostic &other) const
  {
    return (m_assign == other.m_assign
	    && same_tree_p (m_count_cst, other.m_count_cst));
  }

  bool emit (rich_location *rich_loc) FINAL OVERRIDE
  {
    return warning_at (rich_loc, OPT_Wanalyzer_shift_count_negative,
		       "shift by negative count (%qE)", m_count_cst);
  }

  label_text describe_final_event (const evdesc::final_event &ev) FINAL OVERRIDE
  {
    return ev.formatted_print ("shift by negative amount here (%qE)", m_count_cst);
  }

private:
  const gassign *m_assign;
  tree m_count_cst;
};

/* A subclass of pending_diagnostic for complaining about shifts
   by counts >= the width of the operand type.  */

class shift_count_overflow_diagnostic
: public pending_diagnostic_subclass<shift_count_overflow_diagnostic>
{
public:
  shift_count_overflow_diagnostic (const gassign *assign,
				   int operand_precision,
				   tree count_cst)
  : m_assign (assign), m_operand_precision (operand_precision),
    m_count_cst (count_cst)
  {}

  const char *get_kind () const FINAL OVERRIDE
  {
    return "shift_count_overflow_diagnostic";
  }

  bool operator== (const shift_count_overflow_diagnostic &other) const
  {
    return (m_assign == other.m_assign
	    && m_operand_precision == other.m_operand_precision
	    && same_tree_p (m_count_cst, other.m_count_cst));
  }

  bool emit (rich_location *rich_loc) FINAL OVERRIDE
  {
    return warning_at (rich_loc, OPT_Wanalyzer_shift_count_overflow,
		       "shift by count (%qE) >= precision of type (%qi)",
		       m_count_cst, m_operand_precision);
  }

  label_text describe_final_event (const evdesc::final_event &ev) FINAL OVERRIDE
  {
    return ev.formatted_print ("shift by count %qE here", m_count_cst);
  }

private:
  const gassign *m_assign;
  int m_operand_precision;
  tree m_count_cst;
};

/* If ASSIGN is a stmt that can be modelled via
     set_value (lhs_reg, SVALUE, CTXT)
   for some SVALUE, get the SVALUE.
   Otherwise return NULL.  */

const svalue *
region_model::get_gassign_result (const gassign *assign,
				   region_model_context *ctxt)
{
  tree lhs = gimple_assign_lhs (assign);
  tree rhs1 = gimple_assign_rhs1 (assign);
  enum tree_code op = gimple_assign_rhs_code (assign);
  switch (op)
    {
    default:
      return NULL;

    case POINTER_PLUS_EXPR:
      {
	/* e.g. "_1 = a_10(D) + 12;" */
	tree ptr = rhs1;
	tree offset = gimple_assign_rhs2 (assign);

	const svalue *ptr_sval = get_rvalue (ptr, ctxt);
	const svalue *offset_sval = get_rvalue (offset, ctxt);
	/* Quoting tree.def, "the second operand [of a POINTER_PLUS_EXPR]
	   is an integer of type sizetype".  */
	offset_sval = m_mgr->get_or_create_cast (size_type_node, offset_sval);

	const svalue *sval_binop
	  = m_mgr->get_or_create_binop (TREE_TYPE (lhs), op,
					ptr_sval, offset_sval);
	return sval_binop;
      }
      break;

    case POINTER_DIFF_EXPR:
      {
	/* e.g. "_1 = p_2(D) - q_3(D);".  */
	tree rhs2 = gimple_assign_rhs2 (assign);
	const svalue *rhs1_sval = get_rvalue (rhs1, ctxt);
	const svalue *rhs2_sval = get_rvalue (rhs2, ctxt);

	// TODO: perhaps fold to zero if they're known to be equal?

	const svalue *sval_binop
	  = m_mgr->get_or_create_binop (TREE_TYPE (lhs), op,
					rhs1_sval, rhs2_sval);
	return sval_binop;
      }
      break;

    /* Assignments of the form
	set_value (lvalue (LHS), rvalue (EXPR))
       for various EXPR.
       We already have the lvalue for the LHS above, as "lhs_reg".  */
    case ADDR_EXPR: /* LHS = &RHS;  */
    case BIT_FIELD_REF:
    case COMPONENT_REF: /* LHS = op0.op1;  */
    case MEM_REF:
    case REAL_CST:
    case COMPLEX_CST:
    case VECTOR_CST:
    case INTEGER_CST:
    case ARRAY_REF:
    case SSA_NAME: /* LHS = VAR; */
    case VAR_DECL: /* LHS = VAR; */
    case PARM_DECL:/* LHS = VAR; */
    case REALPART_EXPR:
    case IMAGPART_EXPR:
      return get_rvalue (rhs1, ctxt);

    case ABS_EXPR:
    case ABSU_EXPR:
    case CONJ_EXPR:
    case BIT_NOT_EXPR:
    case FIX_TRUNC_EXPR:
    case FLOAT_EXPR:
    case NEGATE_EXPR:
    case NOP_EXPR:
    case VIEW_CONVERT_EXPR:
      {
	/* Unary ops.  */
	const svalue *rhs_sval = get_rvalue (rhs1, ctxt);
	const svalue *sval_unaryop
	  = m_mgr->get_or_create_unaryop (TREE_TYPE (lhs), op, rhs_sval);
	return sval_unaryop;
      }

    case EQ_EXPR:
    case GE_EXPR:
    case LE_EXPR:
    case NE_EXPR:
    case GT_EXPR:
    case LT_EXPR:
    case UNORDERED_EXPR:
    case ORDERED_EXPR:
      {
	tree rhs2 = gimple_assign_rhs2 (assign);

	const svalue *rhs1_sval = get_rvalue (rhs1, ctxt);
	const svalue *rhs2_sval = get_rvalue (rhs2, ctxt);

	if (TREE_TYPE (lhs) == boolean_type_node)
	  {
	    /* Consider constraints between svalues.  */
	    tristate t = eval_condition (rhs1_sval, op, rhs2_sval);
	    if (t.is_known ())
	      return m_mgr->get_or_create_constant_svalue
		(t.is_true () ? boolean_true_node : boolean_false_node);
	  }

	/* Otherwise, generate a symbolic binary op.  */
	const svalue *sval_binop
	  = m_mgr->get_or_create_binop (TREE_TYPE (lhs), op,
					rhs1_sval, rhs2_sval);
	return sval_binop;
      }
      break;

    case PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
    case MULT_HIGHPART_EXPR:
    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case TRUNC_MOD_EXPR:
    case CEIL_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case ROUND_MOD_EXPR:
    case RDIV_EXPR:
    case EXACT_DIV_EXPR:
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case BIT_AND_EXPR:
    case MIN_EXPR:
    case MAX_EXPR:
    case COMPLEX_EXPR:
      {
	/* Binary ops.  */
	tree rhs2 = gimple_assign_rhs2 (assign);

	const svalue *rhs1_sval = get_rvalue (rhs1, ctxt);
	const svalue *rhs2_sval = get_rvalue (rhs2, ctxt);

	if (ctxt && (op == LSHIFT_EXPR || op == RSHIFT_EXPR))
	  {
	    /* "INT34-C. Do not shift an expression by a negative number of bits
	       or by greater than or equal to the number of bits that exist in
	       the operand."  */
	    if (const tree rhs2_cst = rhs2_sval->maybe_get_constant ())
	      if (TREE_CODE (rhs2_cst) == INTEGER_CST)
		{
		  if (tree_int_cst_sgn (rhs2_cst) < 0)
		    ctxt->warn (new shift_count_negative_diagnostic
				  (assign, rhs2_cst));
		  else if (compare_tree_int (rhs2_cst,
					     TYPE_PRECISION (TREE_TYPE (rhs1)))
			   >= 0)
		    ctxt->warn (new shift_count_overflow_diagnostic
				  (assign, TYPE_PRECISION (TREE_TYPE (rhs1)),
				   rhs2_cst));
		}
	  }

	const svalue *sval_binop
	  = m_mgr->get_or_create_binop (TREE_TYPE (lhs), op,
					rhs1_sval, rhs2_sval);
	return sval_binop;
      }

    /* Vector expressions.  In theory we could implement these elementwise,
       but for now, simply return unknown values.  */
    case VEC_DUPLICATE_EXPR:
    case VEC_SERIES_EXPR:
    case VEC_COND_EXPR:
    case VEC_PERM_EXPR:
    case VEC_WIDEN_MULT_HI_EXPR:
    case VEC_WIDEN_MULT_LO_EXPR:
    case VEC_WIDEN_MULT_EVEN_EXPR:
    case VEC_WIDEN_MULT_ODD_EXPR:
    case VEC_UNPACK_HI_EXPR:
    case VEC_UNPACK_LO_EXPR:
    case VEC_UNPACK_FLOAT_HI_EXPR:
    case VEC_UNPACK_FLOAT_LO_EXPR:
    case VEC_UNPACK_FIX_TRUNC_HI_EXPR:
    case VEC_UNPACK_FIX_TRUNC_LO_EXPR:
    case VEC_PACK_TRUNC_EXPR:
    case VEC_PACK_SAT_EXPR:
    case VEC_PACK_FIX_TRUNC_EXPR:
    case VEC_PACK_FLOAT_EXPR:
    case VEC_WIDEN_LSHIFT_HI_EXPR:
    case VEC_WIDEN_LSHIFT_LO_EXPR:
      return m_mgr->get_or_create_unknown_svalue (TREE_TYPE (lhs));
    }
}

/* Update this model for the ASSIGN stmt, using CTXT to report any
   diagnostics.  */

void
region_model::on_assignment (const gassign *assign, region_model_context *ctxt)
{
  tree lhs = gimple_assign_lhs (assign);
  tree rhs1 = gimple_assign_rhs1 (assign);

  const region *lhs_reg = get_lvalue (lhs, ctxt);

  /* Most assignments are handled by:
       set_value (lhs_reg, SVALUE, CTXT)
     for some SVALUE.  */
  if (const svalue *sval = get_gassign_result (assign, ctxt))
    {
      set_value (lhs_reg, sval, ctxt);
      return;
    }

  enum tree_code op = gimple_assign_rhs_code (assign);
  switch (op)
    {
    default:
      {
	if (0)
	  sorry_at (assign->location, "unhandled assignment op: %qs",
		    get_tree_code_name (op));
	const svalue *unknown_sval
	  = m_mgr->get_or_create_unknown_svalue (TREE_TYPE (lhs));
	set_value (lhs_reg, unknown_sval, ctxt);
      }
      break;

    case CONSTRUCTOR:
      {
	if (TREE_CLOBBER_P (rhs1))
	  {
	    /* e.g. "x ={v} {CLOBBER};"  */
	    clobber_region (lhs_reg);
	  }
	else
	  {
	    /* Any CONSTRUCTOR that survives to this point is either
	       just a zero-init of everything, or a vector.  */
	    if (!CONSTRUCTOR_NO_CLEARING (rhs1))
	      zero_fill_region (lhs_reg);
	    unsigned ix;
	    tree index;
	    tree val;
	    FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (rhs1), ix, index, val)
	      {
		gcc_assert (TREE_CODE (TREE_TYPE (rhs1)) == VECTOR_TYPE);
		if (!index)
		  index = build_int_cst (integer_type_node, ix);
		gcc_assert (TREE_CODE (index) == INTEGER_CST);
		const svalue *index_sval
		  = m_mgr->get_or_create_constant_svalue (index);
		gcc_assert (index_sval);
		const region *sub_reg
		  = m_mgr->get_element_region (lhs_reg,
					       TREE_TYPE (val),
					       index_sval);
		const svalue *val_sval = get_rvalue (val, ctxt);
		set_value (sub_reg, val_sval, ctxt);
	      }
	  }
      }
      break;

    case STRING_CST:
      {
	/* e.g. "struct s2 x = {{'A', 'B', 'C', 'D'}};".  */
	/* Add a default binding, rather than a direct one, so that array
	   access will "inherit" the individual chars.  */
	const svalue *rhs_sval = get_rvalue (rhs1, ctxt);
	m_store.set_value (m_mgr->get_store_manager(), lhs_reg, rhs_sval,
			   BK_default, ctxt ? ctxt->get_uncertainty () : NULL);
      }
      break;
    }
}

/* Update this model for the CALL stmt, using CTXT to report any
   diagnostics - the first half.

   Updates to the region_model that should be made *before* sm-states
   are updated are done here; other updates to the region_model are done
   in region_model::on_call_post.

   Return true if the function call has unknown side effects (it wasn't
   recognized and we don't have a body for it, or are unable to tell which
   fndecl it is).

   Write true to *OUT_TERMINATE_PATH if this execution path should be
   terminated (e.g. the function call terminates the process).  */

bool
region_model::on_call_pre (const gcall *call, region_model_context *ctxt,
			   bool *out_terminate_path)
{
  bool unknown_side_effects = false;

  if (tree callee_fndecl = get_fndecl_for_call (call, ctxt))
    {
      call_details cd (call, this, ctxt);

      /* The various impl_call_* member functions are implemented
	 in region-model-impl-calls.cc.
	 Having them split out into separate functions makes it easier
	 to put breakpoints on the handling of specific functions.  */

      if (fndecl_built_in_p (callee_fndecl, BUILT_IN_NORMAL)
	  && gimple_builtin_call_types_compatible_p (call, callee_fndecl))
	switch (DECL_UNCHECKED_FUNCTION_CODE (callee_fndecl))
	  {
	  default:
	    if (!DECL_PURE_P (callee_fndecl))
	      unknown_side_effects = true;
	    break;
	  case BUILT_IN_ALLOCA:
	  case BUILT_IN_ALLOCA_WITH_ALIGN:
	    return impl_call_alloca (cd);
	  case BUILT_IN_CALLOC:
	    return impl_call_calloc (cd);
	  case BUILT_IN_EXPECT:
	  case BUILT_IN_EXPECT_WITH_PROBABILITY:
	    return impl_call_builtin_expect (cd);
	  case BUILT_IN_FREE:
	    /* Handle in "on_call_post".  */
	    break;
	  case BUILT_IN_MALLOC:
	    return impl_call_malloc (cd);
	  case BUILT_IN_MEMCPY:
	  case BUILT_IN_MEMCPY_CHK:
	    impl_call_memcpy (cd);
	    return false;
	  case BUILT_IN_MEMSET:
	  case BUILT_IN_MEMSET_CHK:
	    impl_call_memset (cd);
	    return false;
	    break;
	  case BUILT_IN_REALLOC:
	    impl_call_realloc (cd);
	    return false;
	  case BUILT_IN_STRCPY:
	  case BUILT_IN_STRCPY_CHK:
	    impl_call_strcpy (cd);
	    return false;
	  case BUILT_IN_STRLEN:
	    if (impl_call_strlen (cd))
	      return false;
	    break;

	  /* Stdio builtins.  */
	  case BUILT_IN_FPRINTF:
	  case BUILT_IN_FPRINTF_UNLOCKED:
	  case BUILT_IN_PUTC:
	  case BUILT_IN_PUTC_UNLOCKED:
	  case BUILT_IN_FPUTC:
	  case BUILT_IN_FPUTC_UNLOCKED:
	  case BUILT_IN_FPUTS:
	  case BUILT_IN_FPUTS_UNLOCKED:
	  case BUILT_IN_FWRITE:
	  case BUILT_IN_FWRITE_UNLOCKED:
	  case BUILT_IN_PRINTF:
	  case BUILT_IN_PRINTF_UNLOCKED:
	  case BUILT_IN_PUTCHAR:
	  case BUILT_IN_PUTCHAR_UNLOCKED:
	  case BUILT_IN_PUTS:
	  case BUILT_IN_PUTS_UNLOCKED:
	  case BUILT_IN_VFPRINTF:
	  case BUILT_IN_VPRINTF:
	    /* These stdio builtins have external effects that are out
	       of scope for the analyzer: we only want to model the effects
	       on the return value.  */
	    break;
	  }
      else if (gimple_call_internal_p (call))
	switch (gimple_call_internal_fn (call))
	  {
	  default:
	    if (!DECL_PURE_P (callee_fndecl))
	      unknown_side_effects = true;
	    break;
	  case IFN_BUILTIN_EXPECT:
	    return impl_call_builtin_expect (cd);
	  }
      else if (is_named_call_p (callee_fndecl, "malloc", call, 1))
	return impl_call_malloc (cd);
      else if (is_named_call_p (callee_fndecl, "calloc", call, 2))
	return impl_call_calloc (cd);
      else if (is_named_call_p (callee_fndecl, "alloca", call, 1))
	return impl_call_alloca (cd);
      else if (is_named_call_p (callee_fndecl, "realloc", call, 2))
	{
	  impl_call_realloc (cd);
	  return false;
	}
      else if (is_named_call_p (callee_fndecl, "error"))
	{
	  if (impl_call_error (cd, 3, out_terminate_path))
	    return false;
	  else
	    unknown_side_effects = true;
	}
      else if (is_named_call_p (callee_fndecl, "error_at_line"))
	{
	  if (impl_call_error (cd, 5, out_terminate_path))
	    return false;
	  else
	    unknown_side_effects = true;
	}
      else if (is_named_call_p (callee_fndecl, "getchar", call, 0))
	{
	  /* No side-effects (tracking stream state is out-of-scope
	     for the analyzer).  */
	}
      else if (is_named_call_p (callee_fndecl, "memset", call, 3)
	       && POINTER_TYPE_P (cd.get_arg_type (0)))
	{
	  impl_call_memset (cd);
	  return false;
	}
      else if (is_named_call_p (callee_fndecl, "strlen", call, 1)
	       && POINTER_TYPE_P (cd.get_arg_type (0)))
	{
	  if (impl_call_strlen (cd))
	    return false;
	}
      else if (is_named_call_p (callee_fndecl, "operator new", call, 1))
	return impl_call_operator_new (cd);
      else if (is_named_call_p (callee_fndecl, "operator new []", call, 1))
	return impl_call_operator_new (cd);
      else if (is_named_call_p (callee_fndecl, "operator delete", call, 1)
	       || is_named_call_p (callee_fndecl, "operator delete", call, 2)
	       || is_named_call_p (callee_fndecl, "operator delete []", call, 1))
	{
	  /* Handle in "on_call_post".  */
	}
      else if (!fndecl_has_gimple_body_p (callee_fndecl)
	       && !DECL_PURE_P (callee_fndecl)
	       && !fndecl_built_in_p (callee_fndecl))
	unknown_side_effects = true;
    }
  else
    unknown_side_effects = true;

  /* Some of the above cases update the lhs of the call based on the
     return value.  If we get here, it hasn't been done yet, so do that
     now.  */
  if (tree lhs = gimple_call_lhs (call))
    {
      const region *lhs_region = get_lvalue (lhs, ctxt);
      if (TREE_CODE (lhs) == SSA_NAME)
	{
	  const svalue *sval = m_mgr->get_or_create_initial_value (lhs_region);
	  set_value (lhs_region, sval, ctxt);
	}
    }

  return unknown_side_effects;
}

/* Update this model for the CALL stmt, using CTXT to report any
   diagnostics - the second half.

   Updates to the region_model that should be made *after* sm-states
   are updated are done here; other updates to the region_model are done
   in region_model::on_call_pre.

   If UNKNOWN_SIDE_EFFECTS is true, also call handle_unrecognized_call
   to purge state.  */

void
region_model::on_call_post (const gcall *call,
			    bool unknown_side_effects,
			    region_model_context *ctxt)
{
  if (tree callee_fndecl = get_fndecl_for_call (call, ctxt))
    {
      if (is_named_call_p (callee_fndecl, "free", call, 1))
	{
	  call_details cd (call, this, ctxt);
	  impl_call_free (cd);
	  return;
	}
      if (is_named_call_p (callee_fndecl, "operator delete", call, 1)
	  || is_named_call_p (callee_fndecl, "operator delete", call, 2)
	  || is_named_call_p (callee_fndecl, "operator delete []", call, 1))
	{
	  call_details cd (call, this, ctxt);
	  impl_call_operator_delete (cd);
	  return;
	}
      /* Was this fndecl referenced by
	 __attribute__((malloc(FOO)))?  */
      if (lookup_attribute ("*dealloc", DECL_ATTRIBUTES (callee_fndecl)))
	{
	  call_details cd (call, this, ctxt);
	  impl_deallocation_call (cd);
	  return;
	}
    }

  if (unknown_side_effects)
    handle_unrecognized_call (call, ctxt);
}

/* Handle a call CALL to a function with unknown behavior.

   Traverse the regions in this model, determining what regions are
   reachable from pointer arguments to CALL and from global variables,
   recursively.

   Set all reachable regions to new unknown values and purge sm-state
   from their values, and from values that point to them.  */

void
region_model::handle_unrecognized_call (const gcall *call,
					region_model_context *ctxt)
{
  tree fndecl = get_fndecl_for_call (call, ctxt);

  reachable_regions reachable_regs (this);

  /* Determine the reachable regions and their mutability.  */
  {
    /* Add globals and regions that already escaped in previous
       unknown calls.  */
    m_store.for_each_cluster (reachable_regions::init_cluster_cb,
			      &reachable_regs);

    /* Params that are pointers.  */
    tree iter_param_types = NULL_TREE;
    if (fndecl)
      iter_param_types = TYPE_ARG_TYPES (TREE_TYPE (fndecl));
    for (unsigned arg_idx = 0; arg_idx < gimple_call_num_args (call); arg_idx++)
      {
	/* Track expected param type, where available.  */
	tree param_type = NULL_TREE;
	if (iter_param_types)
	  {
	    param_type = TREE_VALUE (iter_param_types);
	    gcc_assert (param_type);
	    iter_param_types = TREE_CHAIN (iter_param_types);
	  }

	tree parm = gimple_call_arg (call, arg_idx);
	const svalue *parm_sval = get_rvalue (parm, ctxt);
	reachable_regs.handle_parm (parm_sval, param_type);
      }
  }

  uncertainty_t *uncertainty = ctxt->get_uncertainty ();

  /* Purge sm-state for the svalues that were reachable,
     both in non-mutable and mutable form.  */
  for (svalue_set::iterator iter
	 = reachable_regs.begin_reachable_svals ();
       iter != reachable_regs.end_reachable_svals (); ++iter)
    {
      const svalue *sval = (*iter);
      ctxt->on_unknown_change (sval, false);
    }
  for (svalue_set::iterator iter
	 = reachable_regs.begin_mutable_svals ();
       iter != reachable_regs.end_mutable_svals (); ++iter)
    {
      const svalue *sval = (*iter);
      ctxt->on_unknown_change (sval, true);
      if (uncertainty)
	uncertainty->on_mutable_sval_at_unknown_call (sval);
    }

  /* Mark any clusters that have escaped.  */
  reachable_regs.mark_escaped_clusters (ctxt);

  /* Update bindings for all clusters that have escaped, whether above,
     or previously.  */
  m_store.on_unknown_fncall (call, m_mgr->get_store_manager ());
}

/* Traverse the regions in this model, determining what regions are
   reachable from the store and populating *OUT.

   If EXTRA_SVAL is non-NULL, treat it as an additional "root"
   for reachability (for handling return values from functions when
   analyzing return of the only function on the stack).

   If UNCERTAINTY is non-NULL, treat any svalues that were recorded
   within it as being maybe-bound as additional "roots" for reachability.

   Find svalues that haven't leaked.    */

void
region_model::get_reachable_svalues (svalue_set *out,
				     const svalue *extra_sval,
				     const uncertainty_t *uncertainty)
{
  reachable_regions reachable_regs (this);

  /* Add globals and regions that already escaped in previous
     unknown calls.  */
  m_store.for_each_cluster (reachable_regions::init_cluster_cb,
			    &reachable_regs);

  if (extra_sval)
    reachable_regs.handle_sval (extra_sval);

  if (uncertainty)
    for (uncertainty_t::iterator iter
	   = uncertainty->begin_maybe_bound_svals ();
	 iter != uncertainty->end_maybe_bound_svals (); ++iter)
      reachable_regs.handle_sval (*iter);

  /* Get regions for locals that have explicitly bound values.  */
  for (store::cluster_map_t::iterator iter = m_store.begin ();
       iter != m_store.end (); ++iter)
    {
      const region *base_reg = (*iter).first;
      if (const region *parent = base_reg->get_parent_region ())
	if (parent->get_kind () == RK_FRAME)
	  reachable_regs.add (base_reg, false);
    }

  /* Populate *OUT based on the values that were reachable.  */
  for (svalue_set::iterator iter
	 = reachable_regs.begin_reachable_svals ();
       iter != reachable_regs.end_reachable_svals (); ++iter)
    out->add (*iter);
}

/* Update this model for the RETURN_STMT, using CTXT to report any
   diagnostics.  */

void
region_model::on_return (const greturn *return_stmt, region_model_context *ctxt)
{
  tree callee = get_current_function ()->decl;
  tree lhs = DECL_RESULT (callee);
  tree rhs = gimple_return_retval (return_stmt);

  if (lhs && rhs)
    copy_region (get_lvalue (lhs, ctxt), get_lvalue (rhs, ctxt), ctxt);
}

/* Update this model for a call and return of setjmp/sigsetjmp at CALL within
   ENODE, using CTXT to report any diagnostics.

   This is for the initial direct invocation of setjmp/sigsetjmp (which returns
   0), as opposed to any second return due to longjmp/sigsetjmp.  */

void
region_model::on_setjmp (const gcall *call, const exploded_node *enode,
			 region_model_context *ctxt)
{
  const svalue *buf_ptr = get_rvalue (gimple_call_arg (call, 0), ctxt);
  const region *buf_reg = deref_rvalue (buf_ptr, gimple_call_arg (call, 0),
					 ctxt);

  /* Create a setjmp_svalue for this call and store it in BUF_REG's
     region.  */
  if (buf_reg)
    {
      setjmp_record r (enode, call);
      const svalue *sval
	= m_mgr->get_or_create_setjmp_svalue (r, buf_reg->get_type ());
      set_value (buf_reg, sval, ctxt);
    }

  /* Direct calls to setjmp return 0.  */
  if (tree lhs = gimple_call_lhs (call))
    {
      tree zero = build_int_cst (TREE_TYPE (lhs), 0);
      const svalue *new_sval = m_mgr->get_or_create_constant_svalue (zero);
      const region *lhs_reg = get_lvalue (lhs, ctxt);
      set_value (lhs_reg, new_sval, ctxt);
    }
}

/* Update this region_model for rewinding from a "longjmp" at LONGJMP_CALL
   to a "setjmp" at SETJMP_CALL where the final stack depth should be
   SETJMP_STACK_DEPTH.  Pop any stack frames.  Leak detection is *not*
   done, and should be done by the caller.  */

void
region_model::on_longjmp (const gcall *longjmp_call, const gcall *setjmp_call,
			   int setjmp_stack_depth, region_model_context *ctxt)
{
  /* Evaluate the val, using the frame of the "longjmp".  */
  tree fake_retval = gimple_call_arg (longjmp_call, 1);
  const svalue *fake_retval_sval = get_rvalue (fake_retval, ctxt);

  /* Pop any frames until we reach the stack depth of the function where
     setjmp was called.  */
  gcc_assert (get_stack_depth () >= setjmp_stack_depth);
  while (get_stack_depth () > setjmp_stack_depth)
    pop_frame (NULL, NULL, ctxt);

  gcc_assert (get_stack_depth () == setjmp_stack_depth);

  /* Assign to LHS of "setjmp" in new_state.  */
  if (tree lhs = gimple_call_lhs (setjmp_call))
    {
      /* Passing 0 as the val to longjmp leads to setjmp returning 1.  */
      tree t_zero = build_int_cst (TREE_TYPE (fake_retval), 0);
      const svalue *zero_sval = m_mgr->get_or_create_constant_svalue (t_zero);
      tristate eq_zero = eval_condition (fake_retval_sval, EQ_EXPR, zero_sval);
      /* If we have 0, use 1.  */
      if (eq_zero.is_true ())
	{
	  tree t_one = build_int_cst (TREE_TYPE (fake_retval), 1);
	  const svalue *one_sval
	    = m_mgr->get_or_create_constant_svalue (t_one);
	  fake_retval_sval = one_sval;
	}
      else
	{
	  /* Otherwise note that the value is nonzero.  */
	  m_constraints->add_constraint (fake_retval_sval, NE_EXPR, zero_sval);
	}

      /* Decorate the return value from setjmp as being unmergeable,
	 so that we don't attempt to merge states with it as zero
	 with states in which it's nonzero, leading to a clean distinction
	 in the exploded_graph betweeen the first return and the second
	 return.  */
      fake_retval_sval = m_mgr->get_or_create_unmergeable (fake_retval_sval);

      const region *lhs_reg = get_lvalue (lhs, ctxt);
      set_value (lhs_reg, fake_retval_sval, ctxt);
    }
}

/* Update this region_model for a phi stmt of the form
     LHS = PHI <...RHS...>.
   where RHS is for the appropriate edge.  */

void
region_model::handle_phi (const gphi *phi,
			  tree lhs, tree rhs,
			  region_model_context *ctxt)
{
  /* For now, don't bother tracking the .MEM SSA names.  */
  if (tree var = SSA_NAME_VAR (lhs))
    if (TREE_CODE (var) == VAR_DECL)
      if (VAR_DECL_IS_VIRTUAL_OPERAND (var))
	return;

  const svalue *rhs_sval = get_rvalue (rhs, ctxt);

  set_value (get_lvalue (lhs, ctxt), rhs_sval, ctxt);

  if (ctxt)
    ctxt->on_phi (phi, rhs);
}

/* Implementation of region_model::get_lvalue; the latter adds type-checking.

   Get the id of the region for PV within this region_model,
   emitting any diagnostics to CTXT.  */

const region *
region_model::get_lvalue_1 (path_var pv, region_model_context *ctxt)
{
  tree expr = pv.m_tree;

  gcc_assert (expr);

  switch (TREE_CODE (expr))
    {
    default:
      return m_mgr->get_region_for_unexpected_tree_code (ctxt, expr,
							 dump_location_t ());

    case ARRAY_REF:
      {
	tree array = TREE_OPERAND (expr, 0);
	tree index = TREE_OPERAND (expr, 1);

	const region *array_reg = get_lvalue (array, ctxt);
	const svalue *index_sval = get_rvalue (index, ctxt);
	return m_mgr->get_element_region (array_reg,
					  TREE_TYPE (TREE_TYPE (array)),
					  index_sval);
      }
      break;

    case MEM_REF:
      {
	tree ptr = TREE_OPERAND (expr, 0);
	tree offset = TREE_OPERAND (expr, 1);
	const svalue *ptr_sval = get_rvalue (ptr, ctxt);
	const svalue *offset_sval = get_rvalue (offset, ctxt);
	const region *star_ptr = deref_rvalue (ptr_sval, ptr, ctxt);
	return m_mgr->get_offset_region (star_ptr,
					 TREE_TYPE (expr),
					 offset_sval);
      }
      break;

    case FUNCTION_DECL:
      return m_mgr->get_region_for_fndecl (expr);

    case LABEL_DECL:
      return m_mgr->get_region_for_label (expr);

    case VAR_DECL:
      /* Handle globals.  */
      if (is_global_var (expr))
	return m_mgr->get_region_for_global (expr);

      /* Fall through.  */

    case SSA_NAME:
    case PARM_DECL:
    case RESULT_DECL:
      {
	gcc_assert (TREE_CODE (expr) == SSA_NAME
		    || TREE_CODE (expr) == PARM_DECL
		    || TREE_CODE (expr) == VAR_DECL
		    || TREE_CODE (expr) == RESULT_DECL);

	int stack_index = pv.m_stack_depth;
	const frame_region *frame = get_frame_at_index (stack_index);
	gcc_assert (frame);
	return frame->get_region_for_local (m_mgr, expr);
      }

    case COMPONENT_REF:
      {
	/* obj.field  */
	tree obj = TREE_OPERAND (expr, 0);
	tree field = TREE_OPERAND (expr, 1);
	const region *obj_reg = get_lvalue (obj, ctxt);
	return m_mgr->get_field_region (obj_reg, field);
      }
      break;

    case STRING_CST:
      return m_mgr->get_region_for_string (expr);
    }
}

/* Assert that SRC_TYPE can be converted to DST_TYPE as a no-op.  */

static void
assert_compat_types (tree src_type, tree dst_type)
{
  if (src_type && dst_type && !VOID_TYPE_P (dst_type))
    {
#if CHECKING_P
      if (!(useless_type_conversion_p (src_type, dst_type)))
	internal_error ("incompatible types: %qT and %qT", src_type, dst_type);
#endif
    }
}

/* Get the region for PV within this region_model,
   emitting any diagnostics to CTXT.  */

const region *
region_model::get_lvalue (path_var pv, region_model_context *ctxt)
{
  if (pv.m_tree == NULL_TREE)
    return NULL;

  const region *result_reg = get_lvalue_1 (pv, ctxt);
  assert_compat_types (result_reg->get_type (), TREE_TYPE (pv.m_tree));
  return result_reg;
}

/* Get the region for EXPR within this region_model (assuming the most
   recent stack frame if it's a local).  */

const region *
region_model::get_lvalue (tree expr, region_model_context *ctxt)
{
  return get_lvalue (path_var (expr, get_stack_depth () - 1), ctxt);
}

/* Implementation of region_model::get_rvalue; the latter adds type-checking.

   Get the value of PV within this region_model,
   emitting any diagnostics to CTXT.  */

const svalue *
region_model::get_rvalue_1 (path_var pv, region_model_context *ctxt)
{
  gcc_assert (pv.m_tree);

  switch (TREE_CODE (pv.m_tree))
    {
    default:
      return m_mgr->get_or_create_unknown_svalue (TREE_TYPE (pv.m_tree));

    case ADDR_EXPR:
      {
	/* "&EXPR".  */
	tree expr = pv.m_tree;
	tree op0 = TREE_OPERAND (expr, 0);
	const region *expr_reg = get_lvalue (op0, ctxt);
	return m_mgr->get_ptr_svalue (TREE_TYPE (expr), expr_reg);
      }
      break;

    case BIT_FIELD_REF:
      return m_mgr->get_or_create_unknown_svalue (TREE_TYPE (pv.m_tree));

    case SSA_NAME:
    case VAR_DECL:
    case PARM_DECL:
    case RESULT_DECL:
    case ARRAY_REF:
      {
	const region *reg = get_lvalue (pv, ctxt);
	return get_store_value (reg);
      }

    case REALPART_EXPR:
    case IMAGPART_EXPR:
    case VIEW_CONVERT_EXPR:
      {
	tree expr = pv.m_tree;
	tree arg = TREE_OPERAND (expr, 0);
	const svalue *arg_sval = get_rvalue (arg, ctxt);
	const svalue *sval_unaryop
	  = m_mgr->get_or_create_unaryop (TREE_TYPE (expr), TREE_CODE (expr),
					  arg_sval);
	return sval_unaryop;
      };

    case INTEGER_CST:
    case REAL_CST:
    case COMPLEX_CST:
    case VECTOR_CST:
    case STRING_CST:
      return m_mgr->get_or_create_constant_svalue (pv.m_tree);

    case POINTER_PLUS_EXPR:
	{
	  tree expr = pv.m_tree;
	  tree ptr = TREE_OPERAND (expr, 0);
	  tree offset = TREE_OPERAND (expr, 1);
	  const svalue *ptr_sval = get_rvalue (ptr, ctxt);
	  const svalue *offset_sval = get_rvalue (offset, ctxt);
	  const svalue *sval_binop
	    = m_mgr->get_or_create_binop (TREE_TYPE (expr), POINTER_PLUS_EXPR,
					  ptr_sval, offset_sval);
	  return sval_binop;
	}

    /* Binary ops.  */
    case PLUS_EXPR:
    case MULT_EXPR:
	{
	  tree expr = pv.m_tree;
	  tree arg0 = TREE_OPERAND (expr, 0);
	  tree arg1 = TREE_OPERAND (expr, 1);
	  const svalue *arg0_sval = get_rvalue (arg0, ctxt);
	  const svalue *arg1_sval = get_rvalue (arg1, ctxt);
	  const svalue *sval_binop
	    = m_mgr->get_or_create_binop (TREE_TYPE (expr), TREE_CODE (expr),
					  arg0_sval, arg1_sval);
	  return sval_binop;
	}

    case COMPONENT_REF:
    case MEM_REF:
      {
	const region *ref_reg = get_lvalue (pv, ctxt);
	return get_store_value (ref_reg);
      }
    }
}

/* Get the value of PV within this region_model,
   emitting any diagnostics to CTXT.  */

const svalue *
region_model::get_rvalue (path_var pv, region_model_context *ctxt)
{
  if (pv.m_tree == NULL_TREE)
    return NULL;

  const svalue *result_sval = get_rvalue_1 (pv, ctxt);

  assert_compat_types (result_sval->get_type (), TREE_TYPE (pv.m_tree));

  return result_sval;
}

/* Get the value of EXPR within this region_model (assuming the most
   recent stack frame if it's a local).  */

const svalue *
region_model::get_rvalue (tree expr, region_model_context *ctxt)
{
  return get_rvalue (path_var (expr, get_stack_depth () - 1), ctxt);
}

/* Return true if this model is on a path with "main" as the entrypoint
   (as opposed to one in which we're merely analyzing a subset of the
   path through the code).  */

bool
region_model::called_from_main_p () const
{
  if (!m_current_frame)
    return false;
  /* Determine if the oldest stack frame in this model is for "main".  */
  const frame_region *frame0 = get_frame_at_index (0);
  gcc_assert (frame0);
  return id_equal (DECL_NAME (frame0->get_function ()->decl), "main");
}

/* Subroutine of region_model::get_store_value for when REG is (or is within)
   a global variable that hasn't been touched since the start of this path
   (or was implicitly touched due to a call to an unknown function).  */

const svalue *
region_model::get_initial_value_for_global (const region *reg) const
{
  /* Get the decl that REG is for (or is within).  */
  const decl_region *base_reg
    = reg->get_base_region ()->dyn_cast_decl_region ();
  gcc_assert (base_reg);
  tree decl = base_reg->get_decl ();

  /* Special-case: to avoid having to explicitly update all previously
     untracked globals when calling an unknown fn, they implicitly have
     an unknown value if an unknown call has occurred, unless this is
     static to-this-TU and hasn't escaped.  Globals that have escaped
     are explicitly tracked, so we shouldn't hit this case for them.  */
  if (m_store.called_unknown_fn_p ()
      && TREE_PUBLIC (decl)
      && !TREE_READONLY (decl))
    return m_mgr->get_or_create_unknown_svalue (reg->get_type ());

  /* If we are on a path from the entrypoint from "main" and we have a
     global decl defined in this TU that hasn't been touched yet, then
     the initial value of REG can be taken from the initialization value
     of the decl.  */
  if (called_from_main_p () || TREE_READONLY (decl))
    {
      /* Attempt to get the initializer value for base_reg.  */
      if (const svalue *base_reg_init
	    = base_reg->get_svalue_for_initializer (m_mgr))
	{
	  if (reg == base_reg)
	    return base_reg_init;
	  else
	    {
	      /* Get the value for REG within base_reg_init.  */
	      binding_cluster c (base_reg);
	      c.bind (m_mgr->get_store_manager (), base_reg, base_reg_init,
		      BK_direct);
	      const svalue *sval
		= c.get_any_binding (m_mgr->get_store_manager (), reg);
	      if (sval)
		{
		  if (reg->get_type ())
		    sval = m_mgr->get_or_create_cast (reg->get_type (),
						      sval);
		  return sval;
		}
	    }
	}
    }

  /* Otherwise, return INIT_VAL(REG).  */
  return m_mgr->get_or_create_initial_value (reg);
}

/* Get a value for REG, looking it up in the store, or otherwise falling
   back to "initial" or "unknown" values.  */

const svalue *
region_model::get_store_value (const region *reg) const
{
  /* Special-case: handle var_decls in the constant pool.  */
  if (const decl_region *decl_reg = reg->dyn_cast_decl_region ())
    if (const svalue *sval = decl_reg->maybe_get_constant_value (m_mgr))
      return sval;

  const svalue *sval
    = m_store.get_any_binding (m_mgr->get_store_manager (), reg);
  if (sval)
    {
      if (reg->get_type ())
	sval = m_mgr->get_or_create_cast (reg->get_type (), sval);
      return sval;
    }

  /* Special-case: read at a constant index within a STRING_CST.  */
  if (const offset_region *offset_reg = reg->dyn_cast_offset_region ())
    if (tree byte_offset_cst
	  = offset_reg->get_byte_offset ()->maybe_get_constant ())
      if (const string_region *str_reg
	  = reg->get_parent_region ()->dyn_cast_string_region ())
	{
	  tree string_cst = str_reg->get_string_cst ();
	  if (const svalue *char_sval
		= m_mgr->maybe_get_char_from_string_cst (string_cst,
							 byte_offset_cst))
	    return m_mgr->get_or_create_cast (reg->get_type (), char_sval);
	}

  /* Special-case: read the initial char of a STRING_CST.  */
  if (const cast_region *cast_reg = reg->dyn_cast_cast_region ())
    if (const string_region *str_reg
	= cast_reg->get_original_region ()->dyn_cast_string_region ())
      {
	tree string_cst = str_reg->get_string_cst ();
	tree byte_offset_cst = build_int_cst (integer_type_node, 0);
	if (const svalue *char_sval
	    = m_mgr->maybe_get_char_from_string_cst (string_cst,
						     byte_offset_cst))
	  return m_mgr->get_or_create_cast (reg->get_type (), char_sval);
      }

  /* Otherwise we implicitly have the initial value of the region
     (if the cluster had been touched, binding_cluster::get_any_binding,
     would have returned UNKNOWN, and we would already have returned
     that above).  */

  /* Handle globals.  */
  if (reg->get_base_region ()->get_parent_region ()->get_kind ()
      == RK_GLOBALS)
    return get_initial_value_for_global (reg);

  return m_mgr->get_or_create_initial_value (reg);
}

/* Return false if REG does not exist, true if it may do.
   This is for detecting regions within the stack that don't exist anymore
   after frames are popped.  */

bool
region_model::region_exists_p (const region *reg) const
{
  /* If within a stack frame, check that the stack frame is live.  */
  if (const frame_region *enclosing_frame = reg->maybe_get_frame_region ())
    {
      /* Check that the current frame is the enclosing frame, or is called
	 by it.  */
      for (const frame_region *iter_frame = get_current_frame (); iter_frame;
	   iter_frame = iter_frame->get_calling_frame ())
	if (iter_frame == enclosing_frame)
	  return true;
      return false;
    }

  return true;
}

/* Get a region for referencing PTR_SVAL, creating a region if need be, and
   potentially generating warnings via CTXT.
   PTR_SVAL must be of pointer type.
   PTR_TREE if non-NULL can be used when emitting diagnostics.  */

const region *
region_model::deref_rvalue (const svalue *ptr_sval, tree ptr_tree,
			    region_model_context *ctxt)
{
  gcc_assert (ptr_sval);
  gcc_assert (POINTER_TYPE_P (ptr_sval->get_type ()));

  /* If we're dereferencing PTR_SVAL, assume that it is non-NULL; add this
     as a constraint.  This suppresses false positives from
     -Wanalyzer-null-dereference for the case where we later have an
     if (PTR_SVAL) that would occur if we considered the false branch
     and transitioned the malloc state machine from start->null.  */
  tree null_ptr_cst = build_int_cst (ptr_sval->get_type (), 0);
  const svalue *null_ptr = m_mgr->get_or_create_constant_svalue (null_ptr_cst);
  m_constraints->add_constraint (ptr_sval, NE_EXPR, null_ptr);

  switch (ptr_sval->get_kind ())
    {
    default:
      break;

    case SK_REGION:
      {
	const region_svalue *region_sval
	  = as_a <const region_svalue *> (ptr_sval);
	return region_sval->get_pointee ();
      }

    case SK_BINOP:
      {
	const binop_svalue *binop_sval
	  = as_a <const binop_svalue *> (ptr_sval);
	switch (binop_sval->get_op ())
	  {
	  case POINTER_PLUS_EXPR:
	    {
	      /* If we have a symbolic value expressing pointer arithmentic,
		 try to convert it to a suitable region.  */
	      const region *parent_region
		= deref_rvalue (binop_sval->get_arg0 (), NULL_TREE, ctxt);
	      const svalue *offset = binop_sval->get_arg1 ();
	      tree type= TREE_TYPE (ptr_sval->get_type ());
	      return m_mgr->get_offset_region (parent_region, type, offset);
	    }
	  default:
	    break;
	  }
      }
      break;

    case SK_POISONED:
      {
	if (ctxt)
	  {
	    tree ptr = get_representative_tree (ptr_sval);
	    /* If we can't get a representative tree for PTR_SVAL
	       (e.g. if it hasn't been bound into the store), then
	       fall back on PTR_TREE, if non-NULL.  */
	    if (!ptr)
	      ptr = ptr_tree;
	    if (ptr)
	      {
		const poisoned_svalue *poisoned_sval
		  = as_a <const poisoned_svalue *> (ptr_sval);
		enum poison_kind pkind = poisoned_sval->get_poison_kind ();
		ctxt->warn (new poisoned_value_diagnostic (ptr, pkind));
	      }
	  }
      }
      break;
    }

  return m_mgr->get_symbolic_region (ptr_sval);
}

/* A subclass of pending_diagnostic for complaining about writes to
   constant regions of memory.  */

class write_to_const_diagnostic
: public pending_diagnostic_subclass<write_to_const_diagnostic>
{
public:
  write_to_const_diagnostic (const region *reg, tree decl)
  : m_reg (reg), m_decl (decl)
  {}

  const char *get_kind () const FINAL OVERRIDE
  {
    return "write_to_const_diagnostic";
  }

  bool operator== (const write_to_const_diagnostic &other) const
  {
    return (m_reg == other.m_reg
	    && m_decl == other.m_decl);
  }

  bool emit (rich_location *rich_loc) FINAL OVERRIDE
  {
    bool warned = warning_at (rich_loc, OPT_Wanalyzer_write_to_const,
			      "write to %<const%> object %qE", m_decl);
    if (warned)
      inform (DECL_SOURCE_LOCATION (m_decl), "declared here");
    return warned;
  }

  label_text describe_final_event (const evdesc::final_event &ev) FINAL OVERRIDE
  {
    return ev.formatted_print ("write to %<const%> object %qE here", m_decl);
  }

private:
  const region *m_reg;
  tree m_decl;
};

/* A subclass of pending_diagnostic for complaining about writes to
   string literals.  */

class write_to_string_literal_diagnostic
: public pending_diagnostic_subclass<write_to_string_literal_diagnostic>
{
public:
  write_to_string_literal_diagnostic (const region *reg)
  : m_reg (reg)
  {}

  const char *get_kind () const FINAL OVERRIDE
  {
    return "write_to_string_literal_diagnostic";
  }

  bool operator== (const write_to_string_literal_diagnostic &other) const
  {
    return m_reg == other.m_reg;
  }

  bool emit (rich_location *rich_loc) FINAL OVERRIDE
  {
    return warning_at (rich_loc, OPT_Wanalyzer_write_to_string_literal,
		       "write to string literal");
    /* Ideally we would show the location of the STRING_CST as well,
       but it is not available at this point.  */
  }

  label_text describe_final_event (const evdesc::final_event &ev) FINAL OVERRIDE
  {
    return ev.formatted_print ("write to string literal here");
  }

private:
  const region *m_reg;
};

/* Use CTXT to warn If DEST_REG is a region that shouldn't be written to.  */

void
region_model::check_for_writable_region (const region* dest_reg,
					 region_model_context *ctxt) const
{
  /* Fail gracefully if CTXT is NULL.  */
  if (!ctxt)
    return;

  const region *base_reg = dest_reg->get_base_region ();
  switch (base_reg->get_kind ())
    {
    default:
      break;
    case RK_DECL:
      {
	const decl_region *decl_reg = as_a <const decl_region *> (base_reg);
	tree decl = decl_reg->get_decl ();
	/* Warn about writes to const globals.
	   Don't warn for writes to const locals, and params in particular,
	   since we would warn in push_frame when setting them up (e.g the
	   "this" param is "T* const").  */
	if (TREE_READONLY (decl)
	    && is_global_var (decl))
	  ctxt->warn (new write_to_const_diagnostic (dest_reg, decl));
      }
      break;
    case RK_STRING:
      ctxt->warn (new write_to_string_literal_diagnostic (dest_reg));
      break;
    }
}

/* Set the value of the region given by LHS_REG to the value given
   by RHS_SVAL.  */

void
region_model::set_value (const region *lhs_reg, const svalue *rhs_sval,
			 region_model_context *ctxt)
{
  gcc_assert (lhs_reg);
  gcc_assert (rhs_sval);

  check_for_writable_region (lhs_reg, ctxt);

  m_store.set_value (m_mgr->get_store_manager(), lhs_reg, rhs_sval,
		     BK_direct, ctxt ? ctxt->get_uncertainty () : NULL);
}

/* Set the value of the region given by LHS to the value given by RHS.  */

void
region_model::set_value (tree lhs, tree rhs, region_model_context *ctxt)
{
  const region *lhs_reg = get_lvalue (lhs, ctxt);
  const svalue *rhs_sval = get_rvalue (rhs, ctxt);
  gcc_assert (lhs_reg);
  gcc_assert (rhs_sval);
  set_value (lhs_reg, rhs_sval, ctxt);
}

/* Remove all bindings overlapping REG within the store.  */

void
region_model::clobber_region (const region *reg)
{
  m_store.clobber_region (m_mgr->get_store_manager(), reg);
}

/* Remove any bindings for REG within the store.  */

void
region_model::purge_region (const region *reg)
{
  m_store.purge_region (m_mgr->get_store_manager(), reg);
}

/* Zero-fill REG.  */

void
region_model::zero_fill_region (const region *reg)
{
  m_store.zero_fill_region (m_mgr->get_store_manager(), reg);
}

/* Mark REG as having unknown content.  */

void
region_model::mark_region_as_unknown (const region *reg,
				      uncertainty_t *uncertainty)
{
  m_store.mark_region_as_unknown (m_mgr->get_store_manager(), reg,
				  uncertainty);
}

/* Determine what is known about the condition "LHS_SVAL OP RHS_SVAL" within
   this model.  */

tristate
region_model::eval_condition (const svalue *lhs,
			       enum tree_code op,
			       const svalue *rhs) const
{
  /* For now, make no attempt to capture constraints on floating-point
     values.  */
  if ((lhs->get_type () && FLOAT_TYPE_P (lhs->get_type ()))
      || (rhs->get_type () && FLOAT_TYPE_P (rhs->get_type ())))
    return tristate::unknown ();

  tristate ts = eval_condition_without_cm (lhs, op, rhs);
  if (ts.is_known ())
    return ts;

  /* Otherwise, try constraints.  */
  return m_constraints->eval_condition (lhs, op, rhs);
}

/* Determine what is known about the condition "LHS_SVAL OP RHS_SVAL" within
   this model, without resorting to the constraint_manager.

   This is exposed so that impl_region_model_context::on_state_leak can
   check for equality part-way through region_model::purge_unused_svalues
   without risking creating new ECs.  */

tristate
region_model::eval_condition_without_cm (const svalue *lhs,
					  enum tree_code op,
					  const svalue *rhs) const
{
  gcc_assert (lhs);
  gcc_assert (rhs);

  /* See what we know based on the values.  */

  /* For now, make no attempt to capture constraints on floating-point
     values.  */
  if ((lhs->get_type () && FLOAT_TYPE_P (lhs->get_type ()))
      || (rhs->get_type () && FLOAT_TYPE_P (rhs->get_type ())))
    return tristate::unknown ();

  /* Unwrap any unmergeable values.  */
  lhs = lhs->unwrap_any_unmergeable ();
  rhs = rhs->unwrap_any_unmergeable ();

  if (lhs == rhs)
    {
      /* If we have the same svalue, then we have equality
	 (apart from NaN-handling).
	 TODO: should this definitely be the case for poisoned values?  */
      /* Poisoned and unknown values are "unknowable".  */
      if (lhs->get_kind () == SK_POISONED
	  || lhs->get_kind () == SK_UNKNOWN)
	return tristate::TS_UNKNOWN;

      switch (op)
	{
	case EQ_EXPR:
	case GE_EXPR:
	case LE_EXPR:
	  return tristate::TS_TRUE;

	case NE_EXPR:
	case GT_EXPR:
	case LT_EXPR:
	  return tristate::TS_FALSE;

	default:
	  /* For other ops, use the logic below.  */
	  break;
	}
    }

  /* If we have a pair of region_svalues, compare them.  */
  if (const region_svalue *lhs_ptr = lhs->dyn_cast_region_svalue ())
    if (const region_svalue *rhs_ptr = rhs->dyn_cast_region_svalue ())
      {
	tristate res = region_svalue::eval_condition (lhs_ptr, op, rhs_ptr);
	if (res.is_known ())
	  return res;
	/* Otherwise, only known through constraints.  */
      }

  /* If we have a pair of constants, compare them.  */
  if (const constant_svalue *cst_lhs = lhs->dyn_cast_constant_svalue ())
    if (const constant_svalue *cst_rhs = rhs->dyn_cast_constant_svalue ())
      return constant_svalue::eval_condition (cst_lhs, op, cst_rhs);

  /* Handle comparison of a region_svalue against zero.  */

  if (const region_svalue *ptr = lhs->dyn_cast_region_svalue ())
    if (const constant_svalue *cst_rhs = rhs->dyn_cast_constant_svalue ())
      if (zerop (cst_rhs->get_constant ()))
	{
	  /* A region_svalue is a non-NULL pointer, except in certain
	     special cases (see the comment for region::non_null_p.  */
	  const region *pointee = ptr->get_pointee ();
	  if (pointee->non_null_p ())
	    {
	      switch (op)
		{
		default:
		  gcc_unreachable ();

		case EQ_EXPR:
		case GE_EXPR:
		case LE_EXPR:
		  return tristate::TS_FALSE;

		case NE_EXPR:
		case GT_EXPR:
		case LT_EXPR:
		  return tristate::TS_TRUE;
		}
	    }
	}

  /* Handle rejection of equality for comparisons of the initial values of
     "external" values (such as params) with the address of locals.  */
  if (const initial_svalue *init_lhs = lhs->dyn_cast_initial_svalue ())
    if (const region_svalue *rhs_ptr = rhs->dyn_cast_region_svalue ())
      {
	tristate res = compare_initial_and_pointer (init_lhs, rhs_ptr);
	if (res.is_known ())
	  return res;
      }
  if (const initial_svalue *init_rhs = rhs->dyn_cast_initial_svalue ())
    if (const region_svalue *lhs_ptr = lhs->dyn_cast_region_svalue ())
      {
	tristate res = compare_initial_and_pointer (init_rhs, lhs_ptr);
	if (res.is_known ())
	  return res;
      }

  if (const widening_svalue *widen_lhs = lhs->dyn_cast_widening_svalue ())
    if (tree rhs_cst = rhs->maybe_get_constant ())
      {
	tristate res = widen_lhs->eval_condition_without_cm (op, rhs_cst);
	if (res.is_known ())
	  return res;
      }

  return tristate::TS_UNKNOWN;
}

/* Subroutine of region_model::eval_condition_without_cm, for rejecting
   equality of INIT_VAL(PARM) with &LOCAL.  */

tristate
region_model::compare_initial_and_pointer (const initial_svalue *init,
					    const region_svalue *ptr) const
{
  const region *pointee = ptr->get_pointee ();

  /* If we have a pointer to something within a stack frame, it can't be the
     initial value of a param.  */
  if (pointee->maybe_get_frame_region ())
    if (init->initial_value_of_param_p ())
      return tristate::TS_FALSE;

  return tristate::TS_UNKNOWN;
}

/* Attempt to add the constraint "LHS OP RHS" to this region_model.
   If it is consistent with existing constraints, add it, and return true.
   Return false if it contradicts existing constraints.
   Use CTXT for reporting any diagnostics associated with the accesses.  */

bool
region_model::add_constraint (tree lhs, enum tree_code op, tree rhs,
			      region_model_context *ctxt)
{
  /* For now, make no attempt to capture constraints on floating-point
     values.  */
  if (FLOAT_TYPE_P (TREE_TYPE (lhs)) || FLOAT_TYPE_P (TREE_TYPE (rhs)))
    return true;

  const svalue *lhs_sval = get_rvalue (lhs, ctxt);
  const svalue *rhs_sval = get_rvalue (rhs, ctxt);

  tristate t_cond = eval_condition (lhs_sval, op, rhs_sval);

  /* If we already have the condition, do nothing.  */
  if (t_cond.is_true ())
    return true;

  /* Reject a constraint that would contradict existing knowledge, as
     unsatisfiable.  */
  if (t_cond.is_false ())
    return false;

  /* Store the constraint.  */
  m_constraints->add_constraint (lhs_sval, op, rhs_sval);

  add_any_constraints_from_ssa_def_stmt (lhs, op, rhs, ctxt);

  /* Notify the context, if any.  This exists so that the state machines
     in a program_state can be notified about the condition, and so can
     set sm-state for e.g. unchecked->checked, both for cfg-edges, and
     when synthesizing constraints as above.  */
  if (ctxt)
    ctxt->on_condition (lhs, op, rhs);

  return true;
}

/* As above, but when returning false, if OUT is non-NULL, write a
   new rejected_constraint to *OUT.  */

bool
region_model::add_constraint (tree lhs, enum tree_code op, tree rhs,
			      region_model_context *ctxt,
			      rejected_constraint **out)
{
  bool sat = add_constraint (lhs, op, rhs, ctxt);
  if (!sat && out)
    *out = new rejected_constraint (*this, lhs, op, rhs);
  return sat;
}

/* Subroutine of region_model::add_constraint for handling optimized
   && and || conditionals.

   If we have an SSA_NAME for a boolean compared against 0,
   look at anything implied by the def stmt and call add_constraint
   for it (which could recurse).

   For example, if we have
      _1 = p_6 == 0B;
      _2 = p_8 == 0B
      _3 = _1 | _2
    and add the constraint
      (_3 == 0),
    then the def stmt for _3 implies that _1 and _2 are both false,
    and hence we can add the constraints:
      p_6 != 0B
      p_8 != 0B.  */

void
region_model::add_any_constraints_from_ssa_def_stmt (tree lhs,
						     enum tree_code op,
						     tree rhs,
						     region_model_context *ctxt)
{
  if (TREE_CODE (lhs) != SSA_NAME)
    return;

  if (!zerop (rhs))
    return;

  if (op != NE_EXPR && op != EQ_EXPR)
    return;

  gimple *def_stmt = SSA_NAME_DEF_STMT (lhs);
  if (const gassign *assign = dyn_cast<gassign *> (def_stmt))
    add_any_constraints_from_gassign (op, rhs, assign, ctxt);
  else if (gcall *call = dyn_cast<gcall *> (def_stmt))
    add_any_constraints_from_gcall (op, rhs, call, ctxt);
}

/* Add any constraints for an SSA_NAME defined by ASSIGN
   where the result OP RHS.  */

void
region_model::add_any_constraints_from_gassign (enum tree_code op,
						tree rhs,
						const gassign *assign,
						region_model_context *ctxt)
{
  /* We have either
     - "LHS != false" (i.e. LHS is true), or
     - "LHS == false" (i.e. LHS is false).  */
  bool is_true = op == NE_EXPR;

  enum tree_code rhs_code = gimple_assign_rhs_code (assign);

  switch (rhs_code)
    {
    default:
      break;

    case NOP_EXPR:
    case VIEW_CONVERT_EXPR:
      {
	add_constraint (gimple_assign_rhs1 (assign), op, rhs, ctxt);
      }
      break;

    case BIT_AND_EXPR:
      {
	if (is_true)
	  {
	    /* ...and "LHS == (rhs1 & rhs2) i.e. "(rhs1 & rhs2)" is true
	       then both rhs1 and rhs2 must be true.  */
	    tree rhs1 = gimple_assign_rhs1 (assign);
	    tree rhs2 = gimple_assign_rhs2 (assign);
	    add_constraint (rhs1, NE_EXPR, boolean_false_node, ctxt);
	    add_constraint (rhs2, NE_EXPR, boolean_false_node, ctxt);
	  }
      }
      break;

    case BIT_IOR_EXPR:
      {
	if (!is_true)
	  {
	    /* ...and "LHS == (rhs1 | rhs2)
	       i.e. "(rhs1 | rhs2)" is false
	       then both rhs1 and rhs2 must be false.  */
	    tree rhs1 = gimple_assign_rhs1 (assign);
	    tree rhs2 = gimple_assign_rhs2 (assign);
	    add_constraint (rhs1, EQ_EXPR, boolean_false_node, ctxt);
	    add_constraint (rhs2, EQ_EXPR, boolean_false_node, ctxt);
	  }
      }
      break;

    case EQ_EXPR:
    case NE_EXPR:
      {
	/* ...and "LHS == (rhs1 OP rhs2)"
	   then rhs1 OP rhs2 must have the same logical value as LHS.  */
	tree rhs1 = gimple_assign_rhs1 (assign);
	tree rhs2 = gimple_assign_rhs2 (assign);
	if (!is_true)
	  rhs_code
	    = invert_tree_comparison (rhs_code, false /* honor_nans */);
	add_constraint (rhs1, rhs_code, rhs2, ctxt);
      }
      break;
    }
}

/* Add any constraints for an SSA_NAME defined by CALL
   where the result OP RHS.  */

void
region_model::add_any_constraints_from_gcall (enum tree_code op,
					      tree rhs,
					      const gcall *call,
					      region_model_context *ctxt)
{
  if (gimple_call_builtin_p (call, BUILT_IN_EXPECT)
      || gimple_call_builtin_p (call, BUILT_IN_EXPECT_WITH_PROBABILITY)
      || gimple_call_internal_p (call, IFN_BUILTIN_EXPECT))
    {
      /* __builtin_expect's return value is its initial argument.  */
      add_constraint (gimple_call_arg (call, 0), op, rhs, ctxt);
    }
}

/* Determine what is known about the condition "LHS OP RHS" within
   this model.
   Use CTXT for reporting any diagnostics associated with the accesses.  */

tristate
region_model::eval_condition (tree lhs,
			      enum tree_code op,
			      tree rhs,
			      region_model_context *ctxt)
{
  /* For now, make no attempt to model constraints on floating-point
     values.  */
  if (FLOAT_TYPE_P (TREE_TYPE (lhs)) || FLOAT_TYPE_P (TREE_TYPE (rhs)))
    return tristate::unknown ();

  return eval_condition (get_rvalue (lhs, ctxt), op, get_rvalue (rhs, ctxt));
}

/* Implementation of region_model::get_representative_path_var.
   Attempt to return a path_var that represents SVAL, or return NULL_TREE.
   Use VISITED to prevent infinite mutual recursion with the overload for
   regions.  */

path_var
region_model::get_representative_path_var_1 (const svalue *sval,
					     svalue_set *visited) const
{
  gcc_assert (sval);

  /* Prevent infinite recursion.  */
  if (visited->contains (sval))
    return path_var (NULL_TREE, 0);
  visited->add (sval);

  /* Handle casts by recursion into get_representative_path_var.  */
  if (const svalue *cast_sval = sval->maybe_undo_cast ())
    {
      path_var result = get_representative_path_var (cast_sval, visited);
      tree orig_type = sval->get_type ();
      /* If necessary, wrap the result in a cast.  */
      if (result.m_tree && orig_type)
	result.m_tree = build1 (NOP_EXPR, orig_type, result.m_tree);
      return result;
    }

  auto_vec<path_var> pvs;
  m_store.get_representative_path_vars (this, visited, sval, &pvs);

  if (tree cst = sval->maybe_get_constant ())
    pvs.safe_push (path_var (cst, 0));

  /* Handle string literals and various other pointers.  */
  if (const region_svalue *ptr_sval = sval->dyn_cast_region_svalue ())
    {
      const region *reg = ptr_sval->get_pointee ();
      if (path_var pv = get_representative_path_var (reg, visited))
	return path_var (build1 (ADDR_EXPR,
				 sval->get_type (),
				 pv.m_tree),
			 pv.m_stack_depth);
    }

  /* If we have a sub_svalue, look for ways to represent the parent.  */
  if (const sub_svalue *sub_sval = sval->dyn_cast_sub_svalue ())
    {
      const svalue *parent_sval = sub_sval->get_parent ();
      const region *subreg = sub_sval->get_subregion ();
      if (path_var parent_pv
	    = get_representative_path_var (parent_sval, visited))
	if (const field_region *field_reg = subreg->dyn_cast_field_region ())
	  return path_var (build3 (COMPONENT_REF,
				   sval->get_type (),
				   parent_pv.m_tree,
				   field_reg->get_field (),
				   NULL_TREE),
			   parent_pv.m_stack_depth);
    }

  if (pvs.length () < 1)
    return path_var (NULL_TREE, 0);

  pvs.qsort (readability_comparator);
  return pvs[0];
}

/* Attempt to return a path_var that represents SVAL, or return NULL_TREE.
   Use VISITED to prevent infinite mutual recursion with the overload for
   regions

   This function defers to get_representative_path_var_1 to do the work;
   it adds verification that get_representative_path_var_1 returned a tree
   of the correct type.  */

path_var
region_model::get_representative_path_var (const svalue *sval,
					   svalue_set *visited) const
{
  if (sval == NULL)
    return path_var (NULL_TREE, 0);

  tree orig_type = sval->get_type ();

  path_var result = get_representative_path_var_1 (sval, visited);

  /* Verify that the result has the same type as SVAL, if any.  */
  if (result.m_tree && orig_type)
    gcc_assert (TREE_TYPE (result.m_tree) == orig_type);

  return result;
}

/* Attempt to return a tree that represents SVAL, or return NULL_TREE.

   Strip off any top-level cast, to avoid messages like
     double-free of '(void *)ptr'
   from analyzer diagnostics.  */

tree
region_model::get_representative_tree (const svalue *sval) const
{
  svalue_set visited;
  tree expr = get_representative_path_var (sval, &visited).m_tree;

  /* Strip off any top-level cast.  */
  if (expr && TREE_CODE (expr) == NOP_EXPR)
    expr = TREE_OPERAND (expr, 0);

  return fixup_tree_for_diagnostic (expr);
}

/* Implementation of region_model::get_representative_path_var.

   Attempt to return a path_var that represents REG, or return
   the NULL path_var.
   For example, a region for a field of a local would be a path_var
   wrapping a COMPONENT_REF.
   Use VISITED to prevent infinite mutual recursion with the overload for
   svalues.  */

path_var
region_model::get_representative_path_var_1 (const region *reg,
					     svalue_set *visited) const
{
  switch (reg->get_kind ())
    {
    default:
      gcc_unreachable ();

    case RK_FRAME:
    case RK_GLOBALS:
    case RK_CODE:
    case RK_HEAP:
    case RK_STACK:
    case RK_ROOT:
       /* Regions that represent memory spaces are not expressible as trees.  */
      return path_var (NULL_TREE, 0);

    case RK_FUNCTION:
      {
	const function_region *function_reg
	  = as_a <const function_region *> (reg);
	return path_var (function_reg->get_fndecl (), 0);
      }
    case RK_LABEL:
      {
	const label_region *label_reg = as_a <const label_region *> (reg);
	return path_var (label_reg->get_label (), 0);
      }

    case RK_SYMBOLIC:
      {
	const symbolic_region *symbolic_reg
	  = as_a <const symbolic_region *> (reg);
	const svalue *pointer = symbolic_reg->get_pointer ();
	path_var pointer_pv = get_representative_path_var (pointer, visited);
	if (!pointer_pv)
	  return path_var (NULL_TREE, 0);
	tree offset = build_int_cst (pointer->get_type (), 0);
	return path_var (build2 (MEM_REF,
				 reg->get_type (),
				 pointer_pv.m_tree,
				 offset),
			 pointer_pv.m_stack_depth);
      }
    case RK_DECL:
      {
	const decl_region *decl_reg = as_a <const decl_region *> (reg);
	return path_var (decl_reg->get_decl (), decl_reg->get_stack_depth ());
      }
    case RK_FIELD:
      {
	const field_region *field_reg = as_a <const field_region *> (reg);
	path_var parent_pv
	  = get_representative_path_var (reg->get_parent_region (), visited);
	if (!parent_pv)
	  return path_var (NULL_TREE, 0);
	return path_var (build3 (COMPONENT_REF,
				 reg->get_type (),
				 parent_pv.m_tree,
				 field_reg->get_field (),
				 NULL_TREE),
			 parent_pv.m_stack_depth);
      }

    case RK_ELEMENT:
      {
	const element_region *element_reg
	  = as_a <const element_region *> (reg);
	path_var parent_pv
	  = get_representative_path_var (reg->get_parent_region (), visited);
	if (!parent_pv)
	  return path_var (NULL_TREE, 0);
	path_var index_pv
	  = get_representative_path_var (element_reg->get_index (), visited);
	if (!index_pv)
	  return path_var (NULL_TREE, 0);
	return path_var (build4 (ARRAY_REF,
				 reg->get_type (),
				 parent_pv.m_tree, index_pv.m_tree,
				 NULL_TREE, NULL_TREE),
			 parent_pv.m_stack_depth);
      }

    case RK_OFFSET:
      {
	const offset_region *offset_reg
	  = as_a <const offset_region *> (reg);
	path_var parent_pv
	  = get_representative_path_var (reg->get_parent_region (), visited);
	if (!parent_pv)
	  return path_var (NULL_TREE, 0);
	path_var offset_pv
	  = get_representative_path_var (offset_reg->get_byte_offset (),
					 visited);
	if (!offset_pv || TREE_CODE (offset_pv.m_tree) != INTEGER_CST)
	  return path_var (NULL_TREE, 0);
	tree addr_parent = build1 (ADDR_EXPR,
				   build_pointer_type (reg->get_type ()),
				   parent_pv.m_tree);
	return path_var (build2 (MEM_REF,
				 reg->get_type (),
				 addr_parent, offset_pv.m_tree),
			 parent_pv.m_stack_depth);
      }

    case RK_CAST:
      {
	path_var parent_pv
	  = get_representative_path_var (reg->get_parent_region (), visited);
	if (!parent_pv)
	  return path_var (NULL_TREE, 0);
	return path_var (build1 (NOP_EXPR,
				 reg->get_type (),
				 parent_pv.m_tree),
			 parent_pv.m_stack_depth);
      }

    case RK_HEAP_ALLOCATED:
    case RK_ALLOCA:
      /* No good way to express heap-allocated/alloca regions as trees.  */
      return path_var (NULL_TREE, 0);

    case RK_STRING:
      {
	const string_region *string_reg = as_a <const string_region *> (reg);
	return path_var (string_reg->get_string_cst (), 0);
      }

    case RK_UNKNOWN:
      return path_var (NULL_TREE, 0);
    }
}

/* Attempt to return a path_var that represents REG, or return
   the NULL path_var.
   For example, a region for a field of a local would be a path_var
   wrapping a COMPONENT_REF.
   Use VISITED to prevent infinite mutual recursion with the overload for
   svalues.

   This function defers to get_representative_path_var_1 to do the work;
   it adds verification that get_representative_path_var_1 returned a tree
   of the correct type.  */

path_var
region_model::get_representative_path_var (const region *reg,
					   svalue_set *visited) const
{
  path_var result = get_representative_path_var_1 (reg, visited);

  /* Verify that the result has the same type as REG, if any.  */
  if (result.m_tree && reg->get_type ())
    gcc_assert (TREE_TYPE (result.m_tree) == reg->get_type ());

  return result;
}

/* Update this model for any phis in SNODE, assuming we came from
   LAST_CFG_SUPEREDGE.  */

void
region_model::update_for_phis (const supernode *snode,
			       const cfg_superedge *last_cfg_superedge,
			       region_model_context *ctxt)
{
  gcc_assert (last_cfg_superedge);

  for (gphi_iterator gpi = const_cast<supernode *>(snode)->start_phis ();
       !gsi_end_p (gpi); gsi_next (&gpi))
    {
      gphi *phi = gpi.phi ();

      tree src = last_cfg_superedge->get_phi_arg (phi);
      tree lhs = gimple_phi_result (phi);

      /* Update next_state based on phi.  */
      handle_phi (phi, lhs, src, ctxt);
    }
}

/* Attempt to update this model for taking EDGE (where the last statement
   was LAST_STMT), returning true if the edge can be taken, false
   otherwise.
   When returning false, if OUT is non-NULL, write a new rejected_constraint
   to it.

   For CFG superedges where LAST_STMT is a conditional or a switch
   statement, attempt to add the relevant conditions for EDGE to this
   model, returning true if they are feasible, or false if they are
   impossible.

   For call superedges, push frame information and store arguments
   into parameters.

   For return superedges, pop frame information and store return
   values into any lhs.

   Rejection of call/return superedges happens elsewhere, in
   program_point::on_edge (i.e. based on program point, rather
   than program state).  */

bool
region_model::maybe_update_for_edge (const superedge &edge,
				     const gimple *last_stmt,
				     region_model_context *ctxt,
				     rejected_constraint **out)
{
  /* Handle frame updates for interprocedural edges.  */
  switch (edge.m_kind)
    {
    default:
      break;

    case SUPEREDGE_CALL:
      {
	const call_superedge *call_edge = as_a <const call_superedge *> (&edge);
	update_for_call_superedge (*call_edge, ctxt);
      }
      break;

    case SUPEREDGE_RETURN:
      {
	const return_superedge *return_edge
	  = as_a <const return_superedge *> (&edge);
	update_for_return_superedge (*return_edge, ctxt);
      }
      break;

    case SUPEREDGE_INTRAPROCEDURAL_CALL:
      {
	const callgraph_superedge *cg_sedge
	  = as_a <const callgraph_superedge *> (&edge);
	update_for_call_summary (*cg_sedge, ctxt);
      }
      break;
    }

  if (last_stmt == NULL)
    return true;

  /* Apply any constraints for conditionals/switch statements.  */

  if (const gcond *cond_stmt = dyn_cast <const gcond *> (last_stmt))
    {
      const cfg_superedge *cfg_sedge = as_a <const cfg_superedge *> (&edge);
      return apply_constraints_for_gcond (*cfg_sedge, cond_stmt, ctxt, out);
    }

  if (const gswitch *switch_stmt = dyn_cast <const gswitch *> (last_stmt))
    {
      const switch_cfg_superedge *switch_sedge
	= as_a <const switch_cfg_superedge *> (&edge);
      return apply_constraints_for_gswitch (*switch_sedge, switch_stmt,
					    ctxt, out);
    }

  /* Apply any constraints due to an exception being thrown.  */
  if (const cfg_superedge *cfg_sedge = dyn_cast <const cfg_superedge *> (&edge))
    if (cfg_sedge->get_flags () & EDGE_EH)
      return apply_constraints_for_exception (last_stmt, ctxt, out);

  return true;
}

/* Push a new frame_region on to the stack region.
   Populate the frame_region with child regions for the function call's
   parameters, using values from the arguments at the callsite in the
   caller's frame.  */

void
region_model::update_for_call_superedge (const call_superedge &call_edge,
					 region_model_context *ctxt)
{
  /* Build a vec of argument svalues, using the current top
     frame for resolving tree expressions.  */
  const gcall *call_stmt = call_edge.get_call_stmt ();
  auto_vec<const svalue *> arg_svals (gimple_call_num_args (call_stmt));

  for (unsigned i = 0; i < gimple_call_num_args (call_stmt); i++)
    {
      tree arg = gimple_call_arg (call_stmt, i);
      arg_svals.quick_push (get_rvalue (arg, ctxt));
    }

  push_frame (call_edge.get_callee_function (), &arg_svals, ctxt);
}

/* Pop the top-most frame_region from the stack, and copy the return
   region's values (if any) into the region for the lvalue of the LHS of
   the call (if any).  */
void
region_model::update_for_return_superedge (const return_superedge &return_edge,
					   region_model_context *ctxt)
{
  /* Get the region for the result of the call, within the caller frame.  */
  const region *result_dst_reg = NULL;
  const gcall *call_stmt = return_edge.get_call_stmt ();
  tree lhs = gimple_call_lhs (call_stmt);
  if (lhs)
    {
      /* Normally we access the top-level frame, which is:
	   path_var (expr, get_stack_depth () - 1)
	 whereas here we need the caller frame, hence "- 2" here.  */
      gcc_assert (get_stack_depth () >= 2);
      result_dst_reg = get_lvalue (path_var (lhs, get_stack_depth () - 2),
				   ctxt);
    }

  pop_frame (result_dst_reg, NULL, ctxt);
}

/* Update this region_model with a summary of the effect of calling
   and returning from CG_SEDGE.

   TODO: Currently this is extremely simplistic: we merely set the
   return value to "unknown".  A proper implementation would e.g. update
   sm-state, and presumably be reworked to support multiple outcomes.  */

void
region_model::update_for_call_summary (const callgraph_superedge &cg_sedge,
				       region_model_context *ctxt)
{
  /* For now, set any return value to "unknown".  */
  const gcall *call_stmt = cg_sedge.get_call_stmt ();
  tree lhs = gimple_call_lhs (call_stmt);
  if (lhs)
    mark_region_as_unknown (get_lvalue (lhs, ctxt),
			    ctxt ? ctxt->get_uncertainty () : NULL);

  // TODO: actually implement some kind of summary here
}

/* Given a true or false edge guarded by conditional statement COND_STMT,
   determine appropriate constraints for the edge to be taken.

   If they are feasible, add the constraints and return true.

   Return false if the constraints contradict existing knowledge
   (and so the edge should not be taken).
   When returning false, if OUT is non-NULL, write a new rejected_constraint
   to it.  */

bool
region_model::apply_constraints_for_gcond (const cfg_superedge &sedge,
					   const gcond *cond_stmt,
					   region_model_context *ctxt,
					   rejected_constraint **out)
{
  ::edge cfg_edge = sedge.get_cfg_edge ();
  gcc_assert (cfg_edge != NULL);
  gcc_assert (cfg_edge->flags & (EDGE_TRUE_VALUE | EDGE_FALSE_VALUE));

  enum tree_code op = gimple_cond_code (cond_stmt);
  tree lhs = gimple_cond_lhs (cond_stmt);
  tree rhs = gimple_cond_rhs (cond_stmt);
  if (cfg_edge->flags & EDGE_FALSE_VALUE)
    op = invert_tree_comparison (op, false /* honor_nans */);
  return add_constraint (lhs, op, rhs, ctxt, out);
}

/* Given an EDGE guarded by SWITCH_STMT, determine appropriate constraints
   for the edge to be taken.

   If they are feasible, add the constraints and return true.

   Return false if the constraints contradict existing knowledge
   (and so the edge should not be taken).
   When returning false, if OUT is non-NULL, write a new rejected_constraint
   to it.  */

bool
region_model::apply_constraints_for_gswitch (const switch_cfg_superedge &edge,
					     const gswitch *switch_stmt,
					     region_model_context *ctxt,
					     rejected_constraint **out)
{
  tree index  = gimple_switch_index (switch_stmt);
  tree case_label = edge.get_case_label ();
  gcc_assert (TREE_CODE (case_label) == CASE_LABEL_EXPR);
  tree lower_bound = CASE_LOW (case_label);
  tree upper_bound = CASE_HIGH (case_label);
  if (lower_bound)
    {
      if (upper_bound)
	{
	  /* Range.  */
	  if (!add_constraint (index, GE_EXPR, lower_bound, ctxt, out))
	    return false;
	  return add_constraint (index, LE_EXPR, upper_bound, ctxt, out);
	}
      else
	/* Single-value.  */
	return add_constraint (index, EQ_EXPR, lower_bound, ctxt, out);
    }
  else
    {
      /* The default case.
	 Add exclusions based on the other cases.  */
      for (unsigned other_idx = 1;
	   other_idx < gimple_switch_num_labels (switch_stmt);
	   other_idx++)
	{
	  tree other_label = gimple_switch_label (switch_stmt,
						  other_idx);
	  tree other_lower_bound = CASE_LOW (other_label);
	  tree other_upper_bound = CASE_HIGH (other_label);
	  gcc_assert (other_lower_bound);
	  if (other_upper_bound)
	    {
	      /* Exclude this range-valued case.
		 For now, we just exclude the boundary values.
		 TODO: exclude the values within the region.  */
	      if (!add_constraint (index, NE_EXPR, other_lower_bound,
				   ctxt, out))
		return false;
	      if (!add_constraint (index, NE_EXPR, other_upper_bound,
				   ctxt, out))
		return false;
	    }
	  else
	    /* Exclude this single-valued case.  */
	    if (!add_constraint (index, NE_EXPR, other_lower_bound, ctxt, out))
	      return false;
	}
      return true;
    }
}

/* Apply any constraints due to an exception being thrown at LAST_STMT.

   If they are feasible, add the constraints and return true.

   Return false if the constraints contradict existing knowledge
   (and so the edge should not be taken).
   When returning false, if OUT is non-NULL, write a new rejected_constraint
   to it.  */

bool
region_model::apply_constraints_for_exception (const gimple *last_stmt,
					       region_model_context *ctxt,
					       rejected_constraint **out)
{
  gcc_assert (last_stmt);
  if (const gcall *call = dyn_cast <const gcall *> (last_stmt))
    if (tree callee_fndecl = get_fndecl_for_call (call, ctxt))
      if (is_named_call_p (callee_fndecl, "operator new", call, 1)
	  || is_named_call_p (callee_fndecl, "operator new []", call, 1))
	{
	  /* We have an exception thrown from operator new.
	     Add a constraint that the result was NULL, to avoid a false
	     leak report due to the result being lost when following
	     the EH edge.  */
	  if (tree lhs = gimple_call_lhs (call))
	    return add_constraint (lhs, EQ_EXPR, null_pointer_node, ctxt, out);
	  return true;
	}
  return true;
}

/* For use with push_frame when handling a top-level call within the analysis.
   PARAM has a defined but unknown initial value.
   Anything it points to has escaped, since the calling context "knows"
   the pointer, and thus calls to unknown functions could read/write into
   the region.  */

void
region_model::on_top_level_param (tree param,
				   region_model_context *ctxt)
{
  if (POINTER_TYPE_P (TREE_TYPE (param)))
    {
      const region *param_reg = get_lvalue (param, ctxt);
      const svalue *init_ptr_sval
	= m_mgr->get_or_create_initial_value (param_reg);
      const region *pointee_reg = m_mgr->get_symbolic_region (init_ptr_sval);
      m_store.mark_as_escaped (pointee_reg);
    }
}

/* Update this region_model to reflect pushing a frame onto the stack
   for a call to FUN.

   If ARG_SVALS is non-NULL, use it to populate the parameters
   in the new frame.
   Otherwise, the params have their initial_svalues.

   Return the frame_region for the new frame.  */

const region *
region_model::push_frame (function *fun, const vec<const svalue *> *arg_svals,
			  region_model_context *ctxt)
{
  m_current_frame = m_mgr->get_frame_region (m_current_frame, fun);
  if (arg_svals)
    {
      /* Arguments supplied from a caller frame.  */
      tree fndecl = fun->decl;
      unsigned idx = 0;
      for (tree iter_parm = DECL_ARGUMENTS (fndecl); iter_parm;
	   iter_parm = DECL_CHAIN (iter_parm), ++idx)
	{
	  /* If there's a mismatching declaration, the call stmt might
	     not have enough args.  Handle this case by leaving the
	     rest of the params as uninitialized.  */
	  if (idx >= arg_svals->length ())
	    break;
	  tree parm_lval = iter_parm;
	  if (tree parm_default_ssa = ssa_default_def (fun, iter_parm))
	    parm_lval = parm_default_ssa;
	  const region *parm_reg = get_lvalue (parm_lval, ctxt);
	  const svalue *arg_sval = (*arg_svals)[idx];
	  set_value (parm_reg, arg_sval, ctxt);
	}
    }
  else
    {
      /* Otherwise we have a top-level call within the analysis.  The params
	 have defined but unknown initial values.
	 Anything they point to has escaped.  */
      tree fndecl = fun->decl;
      for (tree iter_parm = DECL_ARGUMENTS (fndecl); iter_parm;
	   iter_parm = DECL_CHAIN (iter_parm))
	{
	  if (tree parm_default_ssa = ssa_default_def (fun, iter_parm))
	    on_top_level_param (parm_default_ssa, ctxt);
	  else
	    on_top_level_param (iter_parm, ctxt);
	}
    }

  return m_current_frame;
}

/* Get the function of the top-most frame in this region_model's stack.
   There must be such a frame.  */

function *
region_model::get_current_function () const
{
  const frame_region *frame = get_current_frame ();
  gcc_assert (frame);
  return frame->get_function ();
}

/* Pop the topmost frame_region from this region_model's stack;

   If RESULT_DST_REG is non-null, copy any return value from the frame
   into RESULT_DST_REG's region.
   If OUT_RESULT is non-null, copy any return value from the frame
   into *OUT_RESULT.

   Purge the frame region and all its descendent regions.
   Convert any pointers that point into such regions into
   POISON_KIND_POPPED_STACK svalues.  */

void
region_model::pop_frame (const region *result_dst_reg,
			 const svalue **out_result,
			 region_model_context *ctxt)
{
  gcc_assert (m_current_frame);

  /* Evaluate the result, within the callee frame.  */
  const frame_region *frame_reg = m_current_frame;
  tree fndecl = m_current_frame->get_function ()->decl;
  tree result = DECL_RESULT (fndecl);
  if (result && TREE_TYPE (result) != void_type_node)
    {
      if (result_dst_reg)
	{
	  /* Copy the result to RESULT_DST_REG.  */
	  copy_region (result_dst_reg,
		       get_lvalue (result, ctxt),
		       ctxt);
	}
      if (out_result)
	*out_result = get_rvalue (result, ctxt);
    }

  /* Pop the frame.  */
  m_current_frame = m_current_frame->get_calling_frame ();

  unbind_region_and_descendents (frame_reg,POISON_KIND_POPPED_STACK);
}

/* Get the number of frames in this region_model's stack.  */

int
region_model::get_stack_depth () const
{
  const frame_region *frame = get_current_frame ();
  if (frame)
    return frame->get_stack_depth ();
  else
    return 0;
}

/* Get the frame_region with the given index within the stack.
   The frame_region must exist.  */

const frame_region *
region_model::get_frame_at_index (int index) const
{
  const frame_region *frame = get_current_frame ();
  gcc_assert (frame);
  gcc_assert (index >= 0);
  gcc_assert (index <= frame->get_index ());
  while (index != frame->get_index ())
    {
      frame = frame->get_calling_frame ();
      gcc_assert (frame);
    }
  return frame;
}

/* Unbind svalues for any regions in REG and below.
   Find any pointers to such regions; convert them to
   poisoned values of kind PKIND.  */

void
region_model::unbind_region_and_descendents (const region *reg,
					     enum poison_kind pkind)
{
  /* Gather a set of base regions to be unbound.  */
  hash_set<const region *> base_regs;
  for (store::cluster_map_t::iterator iter = m_store.begin ();
       iter != m_store.end (); ++iter)
    {
      const region *iter_base_reg = (*iter).first;
      if (iter_base_reg->descendent_of_p (reg))
	base_regs.add (iter_base_reg);
    }
  for (hash_set<const region *>::iterator iter = base_regs.begin ();
       iter != base_regs.end (); ++iter)
    m_store.purge_cluster (*iter);

  /* Find any pointers to REG or its descendents; convert to poisoned.  */
  poison_any_pointers_to_descendents (reg, pkind);
}

/* Implementation of BindingVisitor.
   Update the bound svalues for regions below REG to use poisoned
   values instead.  */

struct bad_pointer_finder
{
  bad_pointer_finder (const region *reg, enum poison_kind pkind,
		      region_model_manager *mgr)
  : m_reg (reg), m_pkind (pkind), m_mgr (mgr), m_count (0)
  {}

  void on_binding (const binding_key *, const svalue *&sval)
  {
    if (const region_svalue *ptr_sval = sval->dyn_cast_region_svalue ())
      {
	const region *ptr_dst = ptr_sval->get_pointee ();
	/* Poison ptrs to descendents of REG, but not to REG itself,
	   otherwise double-free detection doesn't work (since sm-state
	   for "free" is stored on the original ptr svalue).  */
	if (ptr_dst->descendent_of_p (m_reg)
	    && ptr_dst != m_reg)
	  {
	    sval = m_mgr->get_or_create_poisoned_svalue (m_pkind,
							 sval->get_type ());
	    ++m_count;
	  }
      }
  }

  const region *m_reg;
  enum poison_kind m_pkind;
  region_model_manager *const m_mgr;
  int m_count;
};

/* Find any pointers to REG or its descendents; convert them to
   poisoned values of kind PKIND.
   Return the number of pointers that were poisoned.  */

int
region_model::poison_any_pointers_to_descendents (const region *reg,
						   enum poison_kind pkind)
{
  bad_pointer_finder bv (reg, pkind, m_mgr);
  m_store.for_each_binding (bv);
  return bv.m_count;
}

/* Attempt to merge THIS with OTHER_MODEL, writing the result
   to OUT_MODEL.  Use POINT to distinguish values created as a
   result of merging.  */

bool
region_model::can_merge_with_p (const region_model &other_model,
				const program_point &point,
				region_model *out_model) const
{
  gcc_assert (out_model);
  gcc_assert (m_mgr == other_model.m_mgr);
  gcc_assert (m_mgr == out_model->m_mgr);

  if (m_current_frame != other_model.m_current_frame)
    return false;
  out_model->m_current_frame = m_current_frame;

  model_merger m (this, &other_model, point, out_model);

  if (!store::can_merge_p (&m_store, &other_model.m_store,
			   &out_model->m_store, m_mgr->get_store_manager (),
			   &m))
    return false;

  /* Merge constraints.  */
  constraint_manager::merge (*m_constraints,
			      *other_model.m_constraints,
			      out_model->m_constraints);

  return true;
}

/* Attempt to get the fndecl used at CALL, if known, or NULL_TREE
   otherwise.  */

tree
region_model::get_fndecl_for_call (const gcall *call,
				   region_model_context *ctxt)
{
  tree fn_ptr = gimple_call_fn (call);
  if (fn_ptr == NULL_TREE)
    return NULL_TREE;
  const svalue *fn_ptr_sval = get_rvalue (fn_ptr, ctxt);
  if (const region_svalue *fn_ptr_ptr
	= fn_ptr_sval->dyn_cast_region_svalue ())
    {
      const region *reg = fn_ptr_ptr->get_pointee ();
      if (const function_region *fn_reg = reg->dyn_cast_function_region ())
	{
	  tree fn_decl = fn_reg->get_fndecl ();
	  cgraph_node *node = cgraph_node::get (fn_decl);
	  if (!node)
	    return NULL_TREE;
	  const cgraph_node *ultimate_node = node->ultimate_alias_target ();
	  if (ultimate_node)
	    return ultimate_node->decl;
	}
    }

  return NULL_TREE;
}

/* Would be much simpler to use a lambda here, if it were supported.  */

struct append_ssa_names_cb_data
{
  const region_model *model;
  auto_vec<const decl_region *> *out;
};

/* Populate *OUT with all decl_regions for SSA names in the current
   frame that have clusters within the store.  */

void
region_model::
get_ssa_name_regions_for_current_frame (auto_vec<const decl_region *> *out)
  const
{
  append_ssa_names_cb_data data;
  data.model = this;
  data.out = out;
  m_store.for_each_cluster (append_ssa_names_cb, &data);
}

/* Implementation detail of get_ssa_name_regions_for_current_frame.  */

void
region_model::append_ssa_names_cb (const region *base_reg,
				   append_ssa_names_cb_data *cb_data)
{
  if (base_reg->get_parent_region () != cb_data->model->m_current_frame)
    return;
  if (const decl_region *decl_reg = base_reg->dyn_cast_decl_region ())
    {
      if (TREE_CODE (decl_reg->get_decl ()) == SSA_NAME)
	cb_data->out->safe_push (decl_reg);
    }
}

/* Return a new region describing a heap-allocated block of memory.  */

const region *
region_model::create_region_for_heap_alloc (const svalue *size_in_bytes)
{
  const region *reg = m_mgr->create_region_for_heap_alloc ();
  record_dynamic_extents (reg, size_in_bytes);
  return reg;
}

/* Return a new region describing a block of memory allocated within the
   current frame.  */

const region *
region_model::create_region_for_alloca (const svalue *size_in_bytes)
{
  const region *reg = m_mgr->create_region_for_alloca (m_current_frame);
  record_dynamic_extents (reg, size_in_bytes);
  return reg;
}

/* Placeholder hook for recording that the size of REG is SIZE_IN_BYTES.
   Currently does nothing.  */

void
region_model::
record_dynamic_extents (const region *reg ATTRIBUTE_UNUSED,
			const svalue *size_in_bytes ATTRIBUTE_UNUSED)
{
}

/* struct model_merger.  */

/* Dump a multiline representation of this merger to PP.  */

void
model_merger::dump_to_pp (pretty_printer *pp, bool simple) const
{
  pp_string (pp, "model A:");
  pp_newline (pp);
  m_model_a->dump_to_pp (pp, simple, true);
  pp_newline (pp);

  pp_string (pp, "model B:");
  pp_newline (pp);
  m_model_b->dump_to_pp (pp, simple, true);
  pp_newline (pp);

  pp_string (pp, "merged model:");
  pp_newline (pp);
  m_merged_model->dump_to_pp (pp, simple, true);
  pp_newline (pp);
}

/* Dump a multiline representation of this merger to FILE.  */

void
model_merger::dump (FILE *fp, bool simple) const
{
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  pp_show_color (&pp) = pp_show_color (global_dc->printer);
  pp.buffer->stream = fp;
  dump_to_pp (&pp, simple);
  pp_flush (&pp);
}

/* Dump a multiline representation of this merger to stderr.  */

DEBUG_FUNCTION void
model_merger::dump (bool simple) const
{
  dump (stderr, simple);
}

} // namespace ana

/* Dump RMODEL fully to stderr (i.e. without summarization).  */

DEBUG_FUNCTION void
debug (const region_model &rmodel)
{
  rmodel.dump (false);
}

/* struct rejected_constraint.  */

void
rejected_constraint::dump_to_pp (pretty_printer *pp) const
{
  region_model m (m_model);
  const svalue *lhs_sval = m.get_rvalue (m_lhs, NULL);
  const svalue *rhs_sval = m.get_rvalue (m_rhs, NULL);
  lhs_sval->dump_to_pp (pp, true);
  pp_printf (pp, " %s ", op_symbol_code (m_op));
  rhs_sval->dump_to_pp (pp, true);
}

/* class engine.  */

/* Dump the managed objects by class to LOGGER, and the per-class totals.  */

void
engine::log_stats (logger *logger) const
{
  m_mgr.log_stats (logger, true);
}

namespace ana {

#if CHECKING_P

namespace selftest {

/* Build a constant tree of the given type from STR.  */

static tree
build_real_cst_from_string (tree type, const char *str)
{
  REAL_VALUE_TYPE real;
  real_from_string (&real, str);
  return build_real (type, real);
}

/* Append various "interesting" constants to OUT (e.g. NaN).  */

static void
append_interesting_constants (auto_vec<tree> *out)
{
  out->safe_push (build_int_cst (integer_type_node, 0));
  out->safe_push (build_int_cst (integer_type_node, 42));
  out->safe_push (build_int_cst (unsigned_type_node, 0));
  out->safe_push (build_int_cst (unsigned_type_node, 42));
  out->safe_push (build_real_cst_from_string (float_type_node, "QNaN"));
  out->safe_push (build_real_cst_from_string (float_type_node, "-QNaN"));
  out->safe_push (build_real_cst_from_string (float_type_node, "SNaN"));
  out->safe_push (build_real_cst_from_string (float_type_node, "-SNaN"));
  out->safe_push (build_real_cst_from_string (float_type_node, "0.0"));
  out->safe_push (build_real_cst_from_string (float_type_node, "-0.0"));
  out->safe_push (build_real_cst_from_string (float_type_node, "Inf"));
  out->safe_push (build_real_cst_from_string (float_type_node, "-Inf"));
}

/* Verify that tree_cmp is a well-behaved comparator for qsort, even
   if the underlying constants aren't comparable.  */

static void
test_tree_cmp_on_constants ()
{
  auto_vec<tree> csts;
  append_interesting_constants (&csts);

  /* Try sorting every triple. */
  const unsigned num = csts.length ();
  for (unsigned i = 0; i < num; i++)
    for (unsigned j = 0; j < num; j++)
      for (unsigned k = 0; k < num; k++)
	{
	  auto_vec<tree> v (3);
	  v.quick_push (csts[i]);
	  v.quick_push (csts[j]);
	  v.quick_push (csts[k]);
	  v.qsort (tree_cmp);
	}
}

/* Implementation detail of the ASSERT_CONDITION_* macros.  */

void
assert_condition (const location &loc,
		  region_model &model,
		  const svalue *lhs, tree_code op, const svalue *rhs,
		  tristate expected)
{
  tristate actual = model.eval_condition (lhs, op, rhs);
  ASSERT_EQ_AT (loc, actual, expected);
}

/* Implementation detail of the ASSERT_CONDITION_* macros.  */

void
assert_condition (const location &loc,
		  region_model &model,
		  tree lhs, tree_code op, tree rhs,
		  tristate expected)
{
  tristate actual = model.eval_condition (lhs, op, rhs, NULL);
  ASSERT_EQ_AT (loc, actual, expected);
}

/* Implementation detail of ASSERT_DUMP_TREE_EQ.  */

static void
assert_dump_tree_eq (const location &loc, tree t, const char *expected)
{
  auto_fix_quotes sentinel;
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  dump_tree (&pp, t);
  ASSERT_STREQ_AT (loc, pp_formatted_text (&pp), expected);
}

/* Assert that dump_tree (T) is EXPECTED.  */

#define ASSERT_DUMP_TREE_EQ(T, EXPECTED) \
  SELFTEST_BEGIN_STMT							\
  assert_dump_tree_eq ((SELFTEST_LOCATION), (T), (EXPECTED)); \
  SELFTEST_END_STMT

/* Implementation detail of ASSERT_DUMP_EQ.  */

static void
assert_dump_eq (const location &loc,
		const region_model &model,
		bool summarize,
		const char *expected)
{
  auto_fix_quotes sentinel;
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;

  model.dump_to_pp (&pp, summarize, true);
  ASSERT_STREQ_AT (loc, pp_formatted_text (&pp), expected);
}

/* Assert that MODEL.dump_to_pp (SUMMARIZE) is EXPECTED.  */

#define ASSERT_DUMP_EQ(MODEL, SUMMARIZE, EXPECTED) \
  SELFTEST_BEGIN_STMT							\
  assert_dump_eq ((SELFTEST_LOCATION), (MODEL), (SUMMARIZE), (EXPECTED)); \
  SELFTEST_END_STMT

/* Smoketest for region_model::dump_to_pp.  */

static void
test_dump ()
{
  region_model_manager mgr;
  region_model model (&mgr);

  ASSERT_DUMP_EQ (model, false,
		  "stack depth: 0\n"
		  "m_called_unknown_fn: FALSE\n"
		  "constraint_manager:\n"
		  "  equiv classes:\n"
		  "  constraints:\n");
  ASSERT_DUMP_EQ (model, true,
		  "stack depth: 0\n"
		  "m_called_unknown_fn: FALSE\n"
		  "constraint_manager:\n"
		  "  equiv classes:\n"
		  "  constraints:\n");
}

/* Helper function for selftests.  Create a struct or union type named NAME,
   with the fields given by the FIELD_DECLS in FIELDS.
   If IS_STRUCT is true create a RECORD_TYPE (aka a struct), otherwise
   create a UNION_TYPE.  */

static tree
make_test_compound_type (const char *name, bool is_struct,
			 const auto_vec<tree> *fields)
{
  tree t = make_node (is_struct ? RECORD_TYPE : UNION_TYPE);
  TYPE_NAME (t) = get_identifier (name);
  TYPE_SIZE (t) = 0;

  tree fieldlist = NULL;
  int i;
  tree field;
  FOR_EACH_VEC_ELT (*fields, i, field)
    {
      gcc_assert (TREE_CODE (field) == FIELD_DECL);
      DECL_CONTEXT (field) = t;
      fieldlist = chainon (field, fieldlist);
    }
  fieldlist = nreverse (fieldlist);
  TYPE_FIELDS (t) = fieldlist;

  layout_type (t);
  return t;
}

/* Selftest fixture for creating the type "struct coord {int x; int y; };".  */

struct coord_test
{
  coord_test ()
  {
    auto_vec<tree> fields;
    m_x_field = build_decl (UNKNOWN_LOCATION, FIELD_DECL,
			       get_identifier ("x"), integer_type_node);
    fields.safe_push (m_x_field);
    m_y_field = build_decl (UNKNOWN_LOCATION, FIELD_DECL,
			       get_identifier ("y"), integer_type_node);
    fields.safe_push (m_y_field);
    m_coord_type = make_test_compound_type ("coord", true, &fields);
  }

  tree m_x_field;
  tree m_y_field;
  tree m_coord_type;
};

/* Verify usage of a struct.  */

static void
test_struct ()
{
  coord_test ct;

  tree c = build_global_decl ("c", ct.m_coord_type);
  tree c_x = build3 (COMPONENT_REF, TREE_TYPE (ct.m_x_field),
		     c, ct.m_x_field, NULL_TREE);
  tree c_y = build3 (COMPONENT_REF, TREE_TYPE (ct.m_y_field),
		     c, ct.m_y_field, NULL_TREE);

  tree int_17 = build_int_cst (integer_type_node, 17);
  tree int_m3 = build_int_cst (integer_type_node, -3);

  region_model_manager mgr;
  region_model model (&mgr);
  model.set_value (c_x, int_17, NULL);
  model.set_value (c_y, int_m3, NULL);

  /* Verify get_offset for "c.x".  */
  {
    const region *c_x_reg = model.get_lvalue (c_x, NULL);
    region_offset offset = c_x_reg->get_offset ();
    ASSERT_EQ (offset.get_base_region (), model.get_lvalue (c, NULL));
    ASSERT_EQ (offset.get_bit_offset (), 0);
  }

  /* Verify get_offset for "c.y".  */
  {
    const region *c_y_reg = model.get_lvalue (c_y, NULL);
    region_offset offset = c_y_reg->get_offset ();
    ASSERT_EQ (offset.get_base_region (), model.get_lvalue (c, NULL));
    ASSERT_EQ (offset.get_bit_offset (), INT_TYPE_SIZE);
  }
}

/* Verify usage of an array element.  */

static void
test_array_1 ()
{
  tree tlen = size_int (10);
  tree arr_type = build_array_type (char_type_node, build_index_type (tlen));

  tree a = build_global_decl ("a", arr_type);

  region_model_manager mgr;
  region_model model (&mgr);
  tree int_0 = build_int_cst (integer_type_node, 0);
  tree a_0 = build4 (ARRAY_REF, char_type_node,
		     a, int_0, NULL_TREE, NULL_TREE);
  tree char_A = build_int_cst (char_type_node, 'A');
  model.set_value (a_0, char_A, NULL);
}

/* Verify that region_model::get_representative_tree works as expected.  */

static void
test_get_representative_tree ()
{
  region_model_manager mgr;

  /* STRING_CST.  */
  {
    tree string_cst = build_string (4, "foo");
    region_model m (&mgr);
    const svalue *str_sval = m.get_rvalue (string_cst, NULL);
    tree rep = m.get_representative_tree (str_sval);
    ASSERT_EQ (rep, string_cst);
  }

  /* String literal.  */
  {
    tree string_cst_ptr = build_string_literal (4, "foo");
    region_model m (&mgr);
    const svalue *str_sval = m.get_rvalue (string_cst_ptr, NULL);
    tree rep = m.get_representative_tree (str_sval);
    ASSERT_DUMP_TREE_EQ (rep, "&\"foo\"[0]");
  }

  /* Value of an element within an array.  */
  {
    tree tlen = size_int (10);
    tree arr_type = build_array_type (char_type_node, build_index_type (tlen));
    tree a = build_global_decl ("a", arr_type);
    placeholder_svalue test_sval (char_type_node, "test value");

    /* Value of a[3].  */
    {
      test_region_model_context ctxt;
      region_model model (&mgr);
      tree int_3 = build_int_cst (integer_type_node, 3);
      tree a_3 = build4 (ARRAY_REF, char_type_node,
			 a, int_3, NULL_TREE, NULL_TREE);
      const region *a_3_reg = model.get_lvalue (a_3, &ctxt);
      model.set_value (a_3_reg, &test_sval, &ctxt);
      tree rep = model.get_representative_tree (&test_sval);
      ASSERT_DUMP_TREE_EQ (rep, "a[3]");
    }

    /* Value of a[0].  */
    {
      test_region_model_context ctxt;
      region_model model (&mgr);
      tree idx = build_int_cst (integer_type_node, 0);
      tree a_0 = build4 (ARRAY_REF, char_type_node,
			 a, idx, NULL_TREE, NULL_TREE);
      const region *a_0_reg = model.get_lvalue (a_0, &ctxt);
      model.set_value (a_0_reg, &test_sval, &ctxt);
      tree rep = model.get_representative_tree (&test_sval);
      ASSERT_DUMP_TREE_EQ (rep, "a[0]");
    }
  }

  /* Value of a field within a struct.  */
  {
    coord_test ct;

    tree c = build_global_decl ("c", ct.m_coord_type);
    tree c_x = build3 (COMPONENT_REF, TREE_TYPE (ct.m_x_field),
		       c, ct.m_x_field, NULL_TREE);
    tree c_y = build3 (COMPONENT_REF, TREE_TYPE (ct.m_y_field),
		       c, ct.m_y_field, NULL_TREE);

    test_region_model_context ctxt;

    /* Value of initial field.  */
    {
      region_model m (&mgr);
      const region *c_x_reg = m.get_lvalue (c_x, &ctxt);
      placeholder_svalue test_sval_x (integer_type_node, "test x val");
      m.set_value (c_x_reg, &test_sval_x, &ctxt);
      tree rep = m.get_representative_tree (&test_sval_x);
      ASSERT_DUMP_TREE_EQ (rep, "c.x");
    }

    /* Value of non-initial field.  */
    {
      region_model m (&mgr);
      const region *c_y_reg = m.get_lvalue (c_y, &ctxt);
      placeholder_svalue test_sval_y (integer_type_node, "test y val");
      m.set_value (c_y_reg, &test_sval_y, &ctxt);
      tree rep = m.get_representative_tree (&test_sval_y);
      ASSERT_DUMP_TREE_EQ (rep, "c.y");
    }
  }
}

/* Verify that calling region_model::get_rvalue repeatedly on the same
   tree constant retrieves the same svalue *.  */

static void
test_unique_constants ()
{
  tree int_0 = build_int_cst (integer_type_node, 0);
  tree int_42 = build_int_cst (integer_type_node, 42);

  test_region_model_context ctxt;
  region_model_manager mgr;
  region_model model (&mgr);
  ASSERT_EQ (model.get_rvalue (int_0, &ctxt), model.get_rvalue (int_0, &ctxt));
  ASSERT_EQ (model.get_rvalue (int_42, &ctxt),
	     model.get_rvalue (int_42, &ctxt));
  ASSERT_NE (model.get_rvalue (int_0, &ctxt), model.get_rvalue (int_42, &ctxt));
  ASSERT_EQ (ctxt.get_num_diagnostics (), 0);

  /* A "(const int)42" will be a different tree from "(int)42)"...  */
  tree const_int_type_node
    = build_qualified_type (integer_type_node, TYPE_QUAL_CONST);
  tree const_int_42 = build_int_cst (const_int_type_node, 42);
  ASSERT_NE (int_42, const_int_42);
  /* It should have a different const_svalue.  */
  const svalue *int_42_sval = model.get_rvalue (int_42, &ctxt);
  const svalue *const_int_42_sval = model.get_rvalue (const_int_42, &ctxt);
  ASSERT_NE (int_42_sval, const_int_42_sval);
  /* But they should compare as equal.  */
  ASSERT_CONDITION_TRUE (model, int_42_sval, EQ_EXPR, const_int_42_sval);
  ASSERT_CONDITION_FALSE (model, int_42_sval, NE_EXPR, const_int_42_sval);
}

/* Verify that each type gets its own singleton unknown_svalue within a
   region_model_manager, and that NULL_TREE gets its own singleton.  */

static void
test_unique_unknowns ()
{
  region_model_manager mgr;
  const svalue *unknown_int
    = mgr.get_or_create_unknown_svalue (integer_type_node);
  /* Repeated calls with the same type should get the same "unknown"
     svalue.  */
  const svalue *unknown_int_2
    = mgr.get_or_create_unknown_svalue (integer_type_node);
  ASSERT_EQ (unknown_int, unknown_int_2);

  /* Different types (or the NULL type) should have different
     unknown_svalues.  */
  const svalue *unknown_NULL_type = mgr.get_or_create_unknown_svalue (NULL);
  ASSERT_NE (unknown_NULL_type, unknown_int);

  /* Repeated calls with NULL for the type should get the same "unknown"
     svalue.  */
  const svalue *unknown_NULL_type_2 = mgr.get_or_create_unknown_svalue (NULL);
  ASSERT_EQ (unknown_NULL_type, unknown_NULL_type_2);
}

/* Verify that initial_svalue are handled as expected.  */

static void
test_initial_svalue_folding ()
{
  region_model_manager mgr;
  tree x = build_global_decl ("x", integer_type_node);
  tree y = build_global_decl ("y", integer_type_node);

  test_region_model_context ctxt;
  region_model model (&mgr);
  const svalue *x_init = model.get_rvalue (x, &ctxt);
  const svalue *y_init = model.get_rvalue (y, &ctxt);
  ASSERT_NE (x_init, y_init);
  const region *x_reg = model.get_lvalue (x, &ctxt);
  ASSERT_EQ (x_init, mgr.get_or_create_initial_value (x_reg));

}

/* Verify that unary ops are folded as expected.  */

static void
test_unaryop_svalue_folding ()
{
  region_model_manager mgr;
  tree x = build_global_decl ("x", integer_type_node);
  tree y = build_global_decl ("y", integer_type_node);

  test_region_model_context ctxt;
  region_model model (&mgr);
  const svalue *x_init = model.get_rvalue (x, &ctxt);
  const svalue *y_init = model.get_rvalue (y, &ctxt);
  const region *x_reg = model.get_lvalue (x, &ctxt);
  ASSERT_EQ (x_init, mgr.get_or_create_initial_value (x_reg));

  /* "(int)x" -> "x".  */
  ASSERT_EQ (x_init, mgr.get_or_create_cast (integer_type_node, x_init));

  /* "(void *)x" -> something other than "x".  */
  ASSERT_NE (x_init, mgr.get_or_create_cast (ptr_type_node, x_init));

  /* "!(x == y)" -> "x != y".  */
  ASSERT_EQ (mgr.get_or_create_unaryop
	       (boolean_type_node, TRUTH_NOT_EXPR,
		mgr.get_or_create_binop (boolean_type_node, EQ_EXPR,
					 x_init, y_init)),
	     mgr.get_or_create_binop (boolean_type_node, NE_EXPR,
				      x_init, y_init));
  /* "!(x > y)" -> "x <= y".  */
  ASSERT_EQ (mgr.get_or_create_unaryop
	       (boolean_type_node, TRUTH_NOT_EXPR,
		mgr.get_or_create_binop (boolean_type_node, GT_EXPR,
					 x_init, y_init)),
	     mgr.get_or_create_binop (boolean_type_node, LE_EXPR,
				      x_init, y_init));
}

/* Verify that binops on constant svalues are folded.  */

static void
test_binop_svalue_folding ()
{
#define NUM_CSTS 10
  tree cst_int[NUM_CSTS];
  region_model_manager mgr;
  const svalue *cst_sval[NUM_CSTS];
  for (int i = 0; i < NUM_CSTS; i++)
    {
      cst_int[i] = build_int_cst (integer_type_node, i);
      cst_sval[i] = mgr.get_or_create_constant_svalue (cst_int[i]);
      ASSERT_EQ (cst_sval[i]->get_kind (), SK_CONSTANT);
      ASSERT_EQ (cst_sval[i]->maybe_get_constant (), cst_int[i]);
    }

  for (int i = 0; i < NUM_CSTS; i++)
    for (int j = 0; j < NUM_CSTS; j++)
      {
	if (i != j)
	  ASSERT_NE (cst_sval[i], cst_sval[j]);
	if (i + j < NUM_CSTS)
	  {
	    const svalue *sum
	      = mgr.get_or_create_binop (integer_type_node, PLUS_EXPR,
					 cst_sval[i], cst_sval[j]);
	    ASSERT_EQ (sum, cst_sval[i + j]);
	  }
	if (i - j >= 0)
	  {
	    const svalue *difference
	      = mgr.get_or_create_binop (integer_type_node, MINUS_EXPR,
					 cst_sval[i], cst_sval[j]);
	    ASSERT_EQ (difference, cst_sval[i - j]);
	  }
	if (i * j < NUM_CSTS)
	  {
	    const svalue *product
	      = mgr.get_or_create_binop (integer_type_node, MULT_EXPR,
					 cst_sval[i], cst_sval[j]);
	    ASSERT_EQ (product, cst_sval[i * j]);
	  }
	const svalue *eq = mgr.get_or_create_binop (integer_type_node, EQ_EXPR,
					       cst_sval[i], cst_sval[j]);
	ASSERT_EQ (eq, i == j ? cst_sval[1] : cst_sval [0]);
	const svalue *neq = mgr.get_or_create_binop (integer_type_node, NE_EXPR,
						cst_sval[i], cst_sval[j]);
	ASSERT_EQ (neq, i != j ? cst_sval[1] : cst_sval [0]);
	// etc
      }

  tree x = build_global_decl ("x", integer_type_node);

  test_region_model_context ctxt;
  region_model model (&mgr);
  const svalue *x_init = model.get_rvalue (x, &ctxt);

  /* PLUS_EXPR folding.  */
  const svalue *x_init_plus_zero
    = mgr.get_or_create_binop (integer_type_node, PLUS_EXPR,
			       x_init, cst_sval[0]);
  ASSERT_EQ (x_init_plus_zero, x_init);
  const svalue *zero_plus_x_init
    = mgr.get_or_create_binop (integer_type_node, PLUS_EXPR,
			       cst_sval[0], x_init);
  ASSERT_EQ (zero_plus_x_init, x_init);

  /* MULT_EXPR folding.  */
  const svalue *x_init_times_zero
    = mgr.get_or_create_binop (integer_type_node, MULT_EXPR,
			       x_init, cst_sval[0]);
  ASSERT_EQ (x_init_times_zero, cst_sval[0]);
  const svalue *zero_times_x_init
    = mgr.get_or_create_binop (integer_type_node, MULT_EXPR,
			       cst_sval[0], x_init);
  ASSERT_EQ (zero_times_x_init, cst_sval[0]);

  const svalue *x_init_times_one
    = mgr.get_or_create_binop (integer_type_node, MULT_EXPR,
			       x_init, cst_sval[1]);
  ASSERT_EQ (x_init_times_one, x_init);
  const svalue *one_times_x_init
    = mgr.get_or_create_binop (integer_type_node, MULT_EXPR,
			       cst_sval[1], x_init);
  ASSERT_EQ (one_times_x_init, x_init);

  // etc
  // TODO: do we want to use the match-and-simplify DSL for this?

  /* Verify that binops put any constants on the RHS.  */
  const svalue *four_times_x_init
    = mgr.get_or_create_binop (integer_type_node, MULT_EXPR,
			       cst_sval[4], x_init);
  const svalue *x_init_times_four
    = mgr.get_or_create_binop (integer_type_node, MULT_EXPR,
			       x_init, cst_sval[4]);
  ASSERT_EQ (four_times_x_init, x_init_times_four);
  const binop_svalue *binop = four_times_x_init->dyn_cast_binop_svalue ();
  ASSERT_EQ (binop->get_op (), MULT_EXPR);
  ASSERT_EQ (binop->get_arg0 (), x_init);
  ASSERT_EQ (binop->get_arg1 (), cst_sval[4]);

  /* Verify that ((x + 1) + 1) == (x + 2).  */
  const svalue *x_init_plus_one
    = mgr.get_or_create_binop (integer_type_node, PLUS_EXPR,
			       x_init, cst_sval[1]);
  const svalue *x_init_plus_two
    = mgr.get_or_create_binop (integer_type_node, PLUS_EXPR,
			       x_init, cst_sval[2]);
  const svalue *x_init_plus_one_plus_one
    = mgr.get_or_create_binop (integer_type_node, PLUS_EXPR,
			       x_init_plus_one, cst_sval[1]);
  ASSERT_EQ (x_init_plus_one_plus_one, x_init_plus_two);
}

/* Verify that sub_svalues are folded as expected.  */

static void
test_sub_svalue_folding ()
{
  coord_test ct;
  tree c = build_global_decl ("c", ct.m_coord_type);
  tree c_x = build3 (COMPONENT_REF, TREE_TYPE (ct.m_x_field),
		     c, ct.m_x_field, NULL_TREE);

  region_model_manager mgr;
  region_model model (&mgr);
  test_region_model_context ctxt;
  const region *c_x_reg = model.get_lvalue (c_x, &ctxt);

  /* Verify that sub_svalue of "unknown" simply
     yields an unknown.  */

  const svalue *unknown = mgr.get_or_create_unknown_svalue (ct.m_coord_type);
  const svalue *sub = mgr.get_or_create_sub_svalue (TREE_TYPE (ct.m_x_field),
						      unknown, c_x_reg);
  ASSERT_EQ (sub->get_kind (), SK_UNKNOWN);
  ASSERT_EQ (sub->get_type (), TREE_TYPE (ct.m_x_field));
}

/* Test that region::descendent_of_p works as expected.  */

static void
test_descendent_of_p ()
{
  region_model_manager mgr;
  const region *stack = mgr.get_stack_region ();
  const region *heap = mgr.get_heap_region ();
  const region *code = mgr.get_code_region ();
  const region *globals = mgr.get_globals_region ();

  /* descendent_of_p should return true when used on the region itself.  */
  ASSERT_TRUE (stack->descendent_of_p (stack));
  ASSERT_FALSE (stack->descendent_of_p (heap));
  ASSERT_FALSE (stack->descendent_of_p (code));
  ASSERT_FALSE (stack->descendent_of_p (globals));

  tree x = build_global_decl ("x", integer_type_node);
  const region *x_reg = mgr.get_region_for_global (x);
  ASSERT_TRUE (x_reg->descendent_of_p (globals));

  /* A cast_region should be a descendent of the original region.  */
  const region *cast_reg = mgr.get_cast_region (x_reg, ptr_type_node);
  ASSERT_TRUE (cast_reg->descendent_of_p (x_reg));
}

/* Verify that simple assignments work as expected.  */

static void
test_assignment ()
{
  tree int_0 = build_int_cst (integer_type_node, 0);
  tree x = build_global_decl ("x", integer_type_node);
  tree y = build_global_decl ("y", integer_type_node);

  /* "x == 0", then use of y, then "y = 0;".  */
  region_model_manager mgr;
  region_model model (&mgr);
  ADD_SAT_CONSTRAINT (model, x, EQ_EXPR, int_0);
  ASSERT_CONDITION_UNKNOWN (model, y, EQ_EXPR, int_0);
  model.set_value (model.get_lvalue (y, NULL),
		   model.get_rvalue (int_0, NULL),
		   NULL);
  ASSERT_CONDITION_TRUE (model, y, EQ_EXPR, int_0);
  ASSERT_CONDITION_TRUE (model, y, EQ_EXPR, x);
}

/* Verify that compound assignments work as expected.  */

static void
test_compound_assignment ()
{
  coord_test ct;

  tree c = build_global_decl ("c", ct.m_coord_type);
  tree c_x = build3 (COMPONENT_REF, TREE_TYPE (ct.m_x_field),
		     c, ct.m_x_field, NULL_TREE);
  tree c_y = build3 (COMPONENT_REF, TREE_TYPE (ct.m_y_field),
		     c, ct.m_y_field, NULL_TREE);
  tree d = build_global_decl ("d", ct.m_coord_type);
  tree d_x = build3 (COMPONENT_REF, TREE_TYPE (ct.m_x_field),
		     d, ct.m_x_field, NULL_TREE);
  tree d_y = build3 (COMPONENT_REF, TREE_TYPE (ct.m_y_field),
		     d, ct.m_y_field, NULL_TREE);

  tree int_17 = build_int_cst (integer_type_node, 17);
  tree int_m3 = build_int_cst (integer_type_node, -3);

  region_model_manager mgr;
  region_model model (&mgr);
  model.set_value (c_x, int_17, NULL);
  model.set_value (c_y, int_m3, NULL);

  /* Copy c to d.  */
  model.copy_region (model.get_lvalue (d, NULL), model.get_lvalue (c, NULL),
		     NULL);
  /* Check that the fields have the same svalues.  */
  ASSERT_EQ (model.get_rvalue (c_x, NULL), model.get_rvalue (d_x, NULL));
  ASSERT_EQ (model.get_rvalue (c_y, NULL), model.get_rvalue (d_y, NULL));
}

/* Verify the details of pushing and popping stack frames.  */

static void
test_stack_frames ()
{
  tree int_42 = build_int_cst (integer_type_node, 42);
  tree int_10 = build_int_cst (integer_type_node, 10);
  tree int_5 = build_int_cst (integer_type_node, 5);
  tree int_0 = build_int_cst (integer_type_node, 0);

  auto_vec <tree> param_types;
  tree parent_fndecl = make_fndecl (integer_type_node,
				    "parent_fn",
				    param_types);
  allocate_struct_function (parent_fndecl, true);

  tree child_fndecl = make_fndecl (integer_type_node,
				   "child_fn",
				   param_types);
  allocate_struct_function (child_fndecl, true);

  /* "a" and "b" in the parent frame.  */
  tree a = build_decl (UNKNOWN_LOCATION, PARM_DECL,
		       get_identifier ("a"),
		       integer_type_node);
  tree b = build_decl (UNKNOWN_LOCATION, PARM_DECL,
		       get_identifier ("b"),
		       integer_type_node);
  /* "x" and "y" in a child frame.  */
  tree x = build_decl (UNKNOWN_LOCATION, PARM_DECL,
		       get_identifier ("x"),
		       integer_type_node);
  tree y = build_decl (UNKNOWN_LOCATION, PARM_DECL,
		       get_identifier ("y"),
		       integer_type_node);

  /* "p" global.  */
  tree p = build_global_decl ("p", ptr_type_node);

  /* "q" global.  */
  tree q = build_global_decl ("q", ptr_type_node);

  region_model_manager mgr;
  test_region_model_context ctxt;
  region_model model (&mgr);

  /* Push stack frame for "parent_fn".  */
  const region *parent_frame_reg
    = model.push_frame (DECL_STRUCT_FUNCTION (parent_fndecl),
			NULL, &ctxt);
  ASSERT_EQ (model.get_current_frame (), parent_frame_reg);
  ASSERT_TRUE (model.region_exists_p (parent_frame_reg));
  const region *a_in_parent_reg = model.get_lvalue (a, &ctxt);
  model.set_value (a_in_parent_reg,
		   model.get_rvalue (int_42, &ctxt),
		   &ctxt);
  ASSERT_EQ (a_in_parent_reg->maybe_get_frame_region (), parent_frame_reg);

  model.add_constraint (b, LT_EXPR, int_10, &ctxt);
  ASSERT_EQ (model.eval_condition (b, LT_EXPR, int_10, &ctxt),
	     tristate (tristate::TS_TRUE));

  /* Push stack frame for "child_fn".  */
  const region *child_frame_reg
    = model.push_frame (DECL_STRUCT_FUNCTION (child_fndecl), NULL, &ctxt);
  ASSERT_EQ (model.get_current_frame (), child_frame_reg);
  ASSERT_TRUE (model.region_exists_p (child_frame_reg));
  const region *x_in_child_reg = model.get_lvalue (x, &ctxt);
  model.set_value (x_in_child_reg,
		   model.get_rvalue (int_0, &ctxt),
		   &ctxt);
  ASSERT_EQ (x_in_child_reg->maybe_get_frame_region (), child_frame_reg);

  model.add_constraint (y, NE_EXPR, int_5, &ctxt);
  ASSERT_EQ (model.eval_condition (y, NE_EXPR, int_5, &ctxt),
	     tristate (tristate::TS_TRUE));

  /* Point a global pointer at a local in the child frame:  p = &x.  */
  const region *p_in_globals_reg = model.get_lvalue (p, &ctxt);
  model.set_value (p_in_globals_reg,
		   mgr.get_ptr_svalue (ptr_type_node, x_in_child_reg),
		   &ctxt);
  ASSERT_EQ (p_in_globals_reg->maybe_get_frame_region (), NULL);

  /* Point another global pointer at p: q = &p.  */
  const region *q_in_globals_reg = model.get_lvalue (q, &ctxt);
  model.set_value (q_in_globals_reg,
		   mgr.get_ptr_svalue (ptr_type_node, p_in_globals_reg),
		   &ctxt);

  /* Test region::descendent_of_p.  */
  ASSERT_TRUE (child_frame_reg->descendent_of_p (child_frame_reg));
  ASSERT_TRUE (x_in_child_reg->descendent_of_p (child_frame_reg));
  ASSERT_FALSE (a_in_parent_reg->descendent_of_p (child_frame_reg));

  /* Pop the "child_fn" frame from the stack.  */
  model.pop_frame (NULL, NULL, &ctxt);
  ASSERT_FALSE (model.region_exists_p (child_frame_reg));
  ASSERT_TRUE (model.region_exists_p (parent_frame_reg));

  /* Verify that p (which was pointing at the local "x" in the popped
     frame) has been poisoned.  */
  const svalue *new_p_sval = model.get_rvalue (p, &ctxt);
  ASSERT_EQ (new_p_sval->get_kind (), SK_POISONED);
  ASSERT_EQ (new_p_sval->dyn_cast_poisoned_svalue ()->get_poison_kind (),
	     POISON_KIND_POPPED_STACK);

  /* Verify that q still points to p, in spite of the region
     renumbering.  */
  const svalue *new_q_sval = model.get_rvalue (q, &ctxt);
  ASSERT_EQ (new_q_sval->get_kind (), SK_REGION);
  ASSERT_EQ (new_q_sval->dyn_cast_region_svalue ()->get_pointee (),
	     model.get_lvalue (p, &ctxt));

  /* Verify that top of stack has been updated.  */
  ASSERT_EQ (model.get_current_frame (), parent_frame_reg);

  /* Verify locals in parent frame.  */
  /* Verify "a" still has its value.  */
  const svalue *new_a_sval = model.get_rvalue (a, &ctxt);
  ASSERT_EQ (new_a_sval->get_kind (), SK_CONSTANT);
  ASSERT_EQ (new_a_sval->dyn_cast_constant_svalue ()->get_constant (),
	     int_42);
  /* Verify "b" still has its constraint.  */
  ASSERT_EQ (model.eval_condition (b, LT_EXPR, int_10, &ctxt),
	     tristate (tristate::TS_TRUE));
}

/* Verify that get_representative_path_var works as expected, that
   we can map from regions to parms and back within a recursive call
   stack.  */

static void
test_get_representative_path_var ()
{
  auto_vec <tree> param_types;
  tree fndecl = make_fndecl (integer_type_node,
			     "factorial",
			     param_types);
  allocate_struct_function (fndecl, true);

  /* Parm "n".  */
  tree n = build_decl (UNKNOWN_LOCATION, PARM_DECL,
		       get_identifier ("n"),
		       integer_type_node);

  region_model_manager mgr;
  test_region_model_context ctxt;
  region_model model (&mgr);

  /* Push 5 stack frames for "factorial", each with a param  */
  auto_vec<const region *> parm_regs;
  auto_vec<const svalue *> parm_svals;
  for (int depth = 0; depth < 5; depth++)
    {
      const region *frame_n_reg
	= model.push_frame (DECL_STRUCT_FUNCTION (fndecl), NULL, &ctxt);
      const region *parm_n_reg = model.get_lvalue (path_var (n, depth), &ctxt);
      parm_regs.safe_push (parm_n_reg);

      ASSERT_EQ (parm_n_reg->get_parent_region (), frame_n_reg);
      const svalue *sval_n = mgr.get_or_create_initial_value (parm_n_reg);
      parm_svals.safe_push (sval_n);
    }

  /* Verify that we can recognize that the regions are the parms,
     at every depth.  */
  for (int depth = 0; depth < 5; depth++)
    {
      {
	svalue_set visited;
	ASSERT_EQ (model.get_representative_path_var (parm_regs[depth],
						      &visited),
		   path_var (n, depth + 1));
      }
      /* ...and that we can lookup lvalues for locals for all frames,
	 not just the top.  */
      ASSERT_EQ (model.get_lvalue (path_var (n, depth), NULL),
		 parm_regs[depth]);
      /* ...and that we can locate the svalues.  */
      {
	svalue_set visited;
	ASSERT_EQ (model.get_representative_path_var (parm_svals[depth],
						      &visited),
		   path_var (n, depth + 1));
      }
    }
}

/* Ensure that region_model::operator== works as expected.  */

static void
test_equality_1 ()
{
  tree int_42 = build_int_cst (integer_type_node, 42);
  tree int_17 = build_int_cst (integer_type_node, 17);

/* Verify that "empty" region_model instances are equal to each other.  */
  region_model_manager mgr;
  region_model model0 (&mgr);
  region_model model1 (&mgr);
  ASSERT_EQ (model0, model1);

  /* Verify that setting state in model1 makes the models non-equal.  */
  tree x = build_global_decl ("x", integer_type_node);
  model0.set_value (x, int_42, NULL);
  ASSERT_EQ (model0.get_rvalue (x, NULL)->maybe_get_constant (), int_42);
  ASSERT_NE (model0, model1);

  /* Verify the copy-ctor.  */
  region_model model2 (model0);
  ASSERT_EQ (model0, model2);
  ASSERT_EQ (model2.get_rvalue (x, NULL)->maybe_get_constant (), int_42);
  ASSERT_NE (model1, model2);

  /* Verify that models obtained from copy-ctor are independently editable
     w/o affecting the original model.  */
  model2.set_value (x, int_17, NULL);
  ASSERT_NE (model0, model2);
  ASSERT_EQ (model2.get_rvalue (x, NULL)->maybe_get_constant (), int_17);
  ASSERT_EQ (model0.get_rvalue (x, NULL)->maybe_get_constant (), int_42);
}

/* Verify that region models for
      x = 42; y = 113;
   and
      y = 113; x = 42;
   are equal.  */

static void
test_canonicalization_2 ()
{
  tree int_42 = build_int_cst (integer_type_node, 42);
  tree int_113 = build_int_cst (integer_type_node, 113);
  tree x = build_global_decl ("x", integer_type_node);
  tree y = build_global_decl ("y", integer_type_node);

  region_model_manager mgr;
  region_model model0 (&mgr);
  model0.set_value (model0.get_lvalue (x, NULL),
		    model0.get_rvalue (int_42, NULL),
		    NULL);
  model0.set_value (model0.get_lvalue (y, NULL),
		    model0.get_rvalue (int_113, NULL),
		    NULL);

  region_model model1 (&mgr);
  model1.set_value (model1.get_lvalue (y, NULL),
		    model1.get_rvalue (int_113, NULL),
		    NULL);
  model1.set_value (model1.get_lvalue (x, NULL),
		    model1.get_rvalue (int_42, NULL),
		    NULL);

  ASSERT_EQ (model0, model1);
}

/* Verify that constraints for
     x > 3 && y > 42
   and
     y > 42 && x > 3
   are equal after canonicalization.  */

static void
test_canonicalization_3 ()
{
  tree int_3 = build_int_cst (integer_type_node, 3);
  tree int_42 = build_int_cst (integer_type_node, 42);
  tree x = build_global_decl ("x", integer_type_node);
  tree y = build_global_decl ("y", integer_type_node);

  region_model_manager mgr;
  region_model model0 (&mgr);
  model0.add_constraint (x, GT_EXPR, int_3, NULL);
  model0.add_constraint (y, GT_EXPR, int_42, NULL);

  region_model model1 (&mgr);
  model1.add_constraint (y, GT_EXPR, int_42, NULL);
  model1.add_constraint (x, GT_EXPR, int_3, NULL);

  model0.canonicalize ();
  model1.canonicalize ();
  ASSERT_EQ (model0, model1);
}

/* Verify that we can canonicalize a model containing NaN and other real
   constants.  */

static void
test_canonicalization_4 ()
{
  auto_vec<tree> csts;
  append_interesting_constants (&csts);

  region_model_manager mgr;
  region_model model (&mgr);

  unsigned i;
  tree cst;
  FOR_EACH_VEC_ELT (csts, i, cst)
    model.get_rvalue (cst, NULL);

  model.canonicalize ();
}

/* Assert that if we have two region_model instances
   with values VAL_A and VAL_B for EXPR that they are
   mergable.  Write the merged model to *OUT_MERGED_MODEL,
   and the merged svalue ptr to *OUT_MERGED_SVALUE.
   If VAL_A or VAL_B are NULL_TREE, don't populate EXPR
   for that region_model.  */

static void
assert_region_models_merge (tree expr, tree val_a, tree val_b,
			     region_model *out_merged_model,
			     const svalue **out_merged_svalue)
{
  program_point point (program_point::origin ());
  test_region_model_context ctxt;
  region_model_manager *mgr = out_merged_model->get_manager ();
  region_model model0 (mgr);
  region_model model1 (mgr);
  if (val_a)
    model0.set_value (model0.get_lvalue (expr, &ctxt),
		      model0.get_rvalue (val_a, &ctxt),
		      &ctxt);
  if (val_b)
    model1.set_value (model1.get_lvalue (expr, &ctxt),
		      model1.get_rvalue (val_b, &ctxt),
		      &ctxt);

  /* They should be mergeable.  */
  ASSERT_TRUE (model0.can_merge_with_p (model1, point, out_merged_model));
  *out_merged_svalue = out_merged_model->get_rvalue (expr, &ctxt);
}

/* Verify that we can merge region_model instances.  */

static void
test_state_merging ()
{
  tree int_42 = build_int_cst (integer_type_node, 42);
  tree int_113 = build_int_cst (integer_type_node, 113);
  tree x = build_global_decl ("x", integer_type_node);
  tree y = build_global_decl ("y", integer_type_node);
  tree z = build_global_decl ("z", integer_type_node);
  tree p = build_global_decl ("p", ptr_type_node);

  tree addr_of_y = build1 (ADDR_EXPR, ptr_type_node, y);
  tree addr_of_z = build1 (ADDR_EXPR, ptr_type_node, z);

  auto_vec <tree> param_types;
  tree test_fndecl = make_fndecl (integer_type_node, "test_fn", param_types);
  allocate_struct_function (test_fndecl, true);

  /* Param "a".  */
  tree a = build_decl (UNKNOWN_LOCATION, PARM_DECL,
		       get_identifier ("a"),
		       integer_type_node);
  tree addr_of_a = build1 (ADDR_EXPR, ptr_type_node, a);

  /* Param "q", a pointer.  */
  tree q = build_decl (UNKNOWN_LOCATION, PARM_DECL,
		       get_identifier ("q"),
		       ptr_type_node);

  program_point point (program_point::origin ());
  region_model_manager mgr;

  {
    region_model model0 (&mgr);
    region_model model1 (&mgr);
    region_model merged (&mgr);
    /* Verify empty models can be merged.  */
    ASSERT_TRUE (model0.can_merge_with_p (model1, point, &merged));
    ASSERT_EQ (model0, merged);
  }

  /* Verify that we can merge two contradictory constraints on the
     value for a global.  */
  /* TODO: verify that the merged model doesn't have a value for
     the global  */
  {
    region_model model0 (&mgr);
    region_model model1 (&mgr);
    region_model merged (&mgr);
    test_region_model_context ctxt;
    model0.add_constraint (x, EQ_EXPR, int_42, &ctxt);
    model1.add_constraint (x, EQ_EXPR, int_113, &ctxt);
    ASSERT_TRUE (model0.can_merge_with_p (model1, point, &merged));
    ASSERT_NE (model0, merged);
    ASSERT_NE (model1, merged);
  }

  /* Verify handling of a PARM_DECL.  */
  {
    test_region_model_context ctxt;
    region_model model0 (&mgr);
    region_model model1 (&mgr);
    ASSERT_EQ (model0.get_stack_depth (), 0);
    model0.push_frame (DECL_STRUCT_FUNCTION (test_fndecl), NULL, &ctxt);
    ASSERT_EQ (model0.get_stack_depth (), 1);
    model1.push_frame (DECL_STRUCT_FUNCTION (test_fndecl), NULL, &ctxt);

    placeholder_svalue test_sval (integer_type_node, "test sval");
    model0.set_value (model0.get_lvalue (a, &ctxt), &test_sval, &ctxt);
    model1.set_value (model1.get_lvalue (a, &ctxt), &test_sval, &ctxt);
    ASSERT_EQ (model0, model1);

    /* They should be mergeable, and the result should be the same.  */
    region_model merged (&mgr);
    ASSERT_TRUE (model0.can_merge_with_p (model1, point, &merged));
    ASSERT_EQ (model0, merged);
    /* In particular, "a" should have the placeholder value.  */
    ASSERT_EQ (merged.get_rvalue (a, &ctxt), &test_sval);
  }

  /* Verify handling of a global.  */
  {
    test_region_model_context ctxt;
    region_model model0 (&mgr);
    region_model model1 (&mgr);

    placeholder_svalue test_sval (integer_type_node, "test sval");
    model0.set_value (model0.get_lvalue (x, &ctxt), &test_sval, &ctxt);
    model1.set_value (model1.get_lvalue (x, &ctxt), &test_sval, &ctxt);
    ASSERT_EQ (model0, model1);

    /* They should be mergeable, and the result should be the same.  */
    region_model merged (&mgr);
    ASSERT_TRUE (model0.can_merge_with_p (model1, point, &merged));
    ASSERT_EQ (model0, merged);
    /* In particular, "x" should have the placeholder value.  */
    ASSERT_EQ (merged.get_rvalue (x, &ctxt), &test_sval);
  }

  /* Use global-handling to verify various combinations of values.  */

  /* Two equal constant values.  */
  {
    region_model merged (&mgr);
    const svalue *merged_x_sval;
    assert_region_models_merge (x, int_42, int_42, &merged, &merged_x_sval);

    /* In particular, there should be a constant value for "x".  */
    ASSERT_EQ (merged_x_sval->get_kind (), SK_CONSTANT);
    ASSERT_EQ (merged_x_sval->dyn_cast_constant_svalue ()->get_constant (),
	       int_42);
  }

  /* Two non-equal constant values.  */
  {
    region_model merged (&mgr);
    const svalue *merged_x_sval;
    assert_region_models_merge (x, int_42, int_113, &merged, &merged_x_sval);

    /* In particular, there should be a "widening" value for "x".  */
    ASSERT_EQ (merged_x_sval->get_kind (), SK_WIDENING);
  }

  /* Initial and constant.  */
  {
    region_model merged (&mgr);
    const svalue *merged_x_sval;
    assert_region_models_merge (x, NULL_TREE, int_113, &merged, &merged_x_sval);

    /* In particular, there should be an unknown value for "x".  */
    ASSERT_EQ (merged_x_sval->get_kind (), SK_UNKNOWN);
  }

  /* Constant and initial.  */
  {
    region_model merged (&mgr);
    const svalue *merged_x_sval;
    assert_region_models_merge (x, int_42, NULL_TREE, &merged, &merged_x_sval);

    /* In particular, there should be an unknown value for "x".  */
    ASSERT_EQ (merged_x_sval->get_kind (), SK_UNKNOWN);
  }

  /* Unknown and constant.  */
  // TODO

  /* Pointers: NULL and NULL.  */
  // TODO

  /* Pointers: NULL and non-NULL.  */
  // TODO

  /* Pointers: non-NULL and non-NULL: ptr to a local.  */
  {
    region_model model0 (&mgr);
    model0.push_frame (DECL_STRUCT_FUNCTION (test_fndecl), NULL, NULL);
    model0.set_value (model0.get_lvalue (p, NULL),
		      model0.get_rvalue (addr_of_a, NULL), NULL);

    region_model model1 (model0);
    ASSERT_EQ (model0, model1);

    /* They should be mergeable, and the result should be the same.  */
    region_model merged (&mgr);
    ASSERT_TRUE (model0.can_merge_with_p (model1, point, &merged));
    ASSERT_EQ (model0, merged);
  }

  /* Pointers: non-NULL and non-NULL: ptr to a global.  */
  {
    region_model merged (&mgr);
    /* p == &y in both input models.  */
    const svalue *merged_p_sval;
    assert_region_models_merge (p, addr_of_y, addr_of_y, &merged,
				&merged_p_sval);

    /* We should get p == &y in the merged model.  */
    ASSERT_EQ (merged_p_sval->get_kind (), SK_REGION);
    const region_svalue *merged_p_ptr
      = merged_p_sval->dyn_cast_region_svalue ();
    const region *merged_p_star_reg = merged_p_ptr->get_pointee ();
    ASSERT_EQ (merged_p_star_reg, merged.get_lvalue (y, NULL));
  }

  /* Pointers: non-NULL ptrs to different globals: should be unknown.  */
  {
    region_model merged (&mgr);
    /* x == &y vs x == &z in the input models; these are actually casts
       of the ptrs to "int".  */
    const svalue *merged_x_sval;
    // TODO:
    assert_region_models_merge (x, addr_of_y, addr_of_z, &merged,
				&merged_x_sval);

    /* We should get x == unknown in the merged model.  */
    ASSERT_EQ (merged_x_sval->get_kind (), SK_UNKNOWN);
  }

  /* Pointers: non-NULL and non-NULL: ptr to a heap region.  */
  {
    test_region_model_context ctxt;
    region_model model0 (&mgr);
    tree size = build_int_cst (integer_type_node, 1024);
    const svalue *size_sval = mgr.get_or_create_constant_svalue (size);
    const region *new_reg = model0.create_region_for_heap_alloc (size_sval);
    const svalue *ptr_sval = mgr.get_ptr_svalue (ptr_type_node, new_reg);
    model0.set_value (model0.get_lvalue (p, &ctxt),
		      ptr_sval, &ctxt);

    region_model model1 (model0);

    ASSERT_EQ (model0, model1);

    region_model merged (&mgr);
    ASSERT_TRUE (model0.can_merge_with_p (model1, point, &merged));

    /* The merged model ought to be identical.  */
    ASSERT_EQ (model0, merged);
  }

  /* Two regions sharing the same placeholder svalue should continue sharing
     it after self-merger.  */
  {
    test_region_model_context ctxt;
    region_model model0 (&mgr);
    placeholder_svalue placeholder_sval (integer_type_node, "test");
    model0.set_value (model0.get_lvalue (x, &ctxt),
		      &placeholder_sval, &ctxt);
    model0.set_value (model0.get_lvalue (y, &ctxt), &placeholder_sval, &ctxt);
    region_model model1 (model0);

    /* They should be mergeable, and the result should be the same.  */
    region_model merged (&mgr);
    ASSERT_TRUE (model0.can_merge_with_p (model1, point, &merged));
    ASSERT_EQ (model0, merged);

    /* In particular, we should have x == y.  */
    ASSERT_EQ (merged.eval_condition (x, EQ_EXPR, y, &ctxt),
	       tristate (tristate::TS_TRUE));
  }

  {
    region_model model0 (&mgr);
    region_model model1 (&mgr);
    test_region_model_context ctxt;
    model0.add_constraint (x, EQ_EXPR, int_42, &ctxt);
    model1.add_constraint (x, NE_EXPR, int_42, &ctxt);
    region_model merged (&mgr);
    ASSERT_TRUE (model0.can_merge_with_p (model1, point, &merged));
  }

  {
    region_model model0 (&mgr);
    region_model model1 (&mgr);
    test_region_model_context ctxt;
    model0.add_constraint (x, EQ_EXPR, int_42, &ctxt);
    model1.add_constraint (x, NE_EXPR, int_42, &ctxt);
    model1.add_constraint (x, EQ_EXPR, int_113, &ctxt);
    region_model merged (&mgr);
    ASSERT_TRUE (model0.can_merge_with_p (model1, point, &merged));
  }

  // TODO: what can't we merge? need at least one such test

  /* TODO: various things
     - heap regions
     - value merging:
       - every combination, but in particular
	   - pairs of regions
   */

  /* Views.  */
  {
    test_region_model_context ctxt;
    region_model model0 (&mgr);

    const region *x_reg = model0.get_lvalue (x, &ctxt);
    const region *x_as_ptr = mgr.get_cast_region (x_reg, ptr_type_node);
    model0.set_value (x_as_ptr, model0.get_rvalue (addr_of_y, &ctxt), &ctxt);

    region_model model1 (model0);
    ASSERT_EQ (model1, model0);

    /* They should be mergeable, and the result should be the same.  */
    region_model merged (&mgr);
    ASSERT_TRUE (model0.can_merge_with_p (model1, point, &merged));
  }

  /* Verify that we can merge a model in which a local in an older stack
     frame points to a local in a more recent stack frame.  */
  {
    region_model model0 (&mgr);
    model0.push_frame (DECL_STRUCT_FUNCTION (test_fndecl), NULL, NULL);
    const region *q_in_first_frame = model0.get_lvalue (q, NULL);

    /* Push a second frame.  */
    const region *reg_2nd_frame
      = model0.push_frame (DECL_STRUCT_FUNCTION (test_fndecl), NULL, NULL);

    /* Have a pointer in the older frame point to a local in the
       more recent frame.  */
    const svalue *sval_ptr = model0.get_rvalue (addr_of_a, NULL);
    model0.set_value (q_in_first_frame, sval_ptr, NULL);

    /* Verify that it's pointing at the newer frame.  */
    const region *reg_pointee
      = sval_ptr->dyn_cast_region_svalue ()->get_pointee ();
    ASSERT_EQ (reg_pointee->get_parent_region (), reg_2nd_frame);

    model0.canonicalize ();

    region_model model1 (model0);
    ASSERT_EQ (model0, model1);

    /* They should be mergeable, and the result should be the same
       (after canonicalization, at least).  */
    region_model merged (&mgr);
    ASSERT_TRUE (model0.can_merge_with_p (model1, point, &merged));
    merged.canonicalize ();
    ASSERT_EQ (model0, merged);
  }

  /* Verify that we can merge a model in which a local points to a global.  */
  {
    region_model model0 (&mgr);
    model0.push_frame (DECL_STRUCT_FUNCTION (test_fndecl), NULL, NULL);
    model0.set_value (model0.get_lvalue (q, NULL),
		      model0.get_rvalue (addr_of_y, NULL), NULL);

    region_model model1 (model0);
    ASSERT_EQ (model0, model1);

    /* They should be mergeable, and the result should be the same
       (after canonicalization, at least).  */
    region_model merged (&mgr);
    ASSERT_TRUE (model0.can_merge_with_p (model1, point, &merged));
    ASSERT_EQ (model0, merged);
  }
}

/* Verify that constraints are correctly merged when merging region_model
   instances.  */

static void
test_constraint_merging ()
{
  tree int_0 = build_int_cst (integer_type_node, 0);
  tree int_5 = build_int_cst (integer_type_node, 5);
  tree x = build_global_decl ("x", integer_type_node);
  tree y = build_global_decl ("y", integer_type_node);
  tree z = build_global_decl ("z", integer_type_node);
  tree n = build_global_decl ("n", integer_type_node);

  region_model_manager mgr;
  test_region_model_context ctxt;

  /* model0: 0 <= (x == y) < n.  */
  region_model model0 (&mgr);
  model0.add_constraint (x, EQ_EXPR, y, &ctxt);
  model0.add_constraint (x, GE_EXPR, int_0, NULL);
  model0.add_constraint (x, LT_EXPR, n, NULL);

  /* model1: z != 5 && (0 <= x < n).  */
  region_model model1 (&mgr);
  model1.add_constraint (z, NE_EXPR, int_5, NULL);
  model1.add_constraint (x, GE_EXPR, int_0, NULL);
  model1.add_constraint (x, LT_EXPR, n, NULL);

  /* They should be mergeable; the merged constraints should
     be: (0 <= x < n).  */
  program_point point (program_point::origin ());
  region_model merged (&mgr);
  ASSERT_TRUE (model0.can_merge_with_p (model1, point, &merged));

  ASSERT_EQ (merged.eval_condition (x, GE_EXPR, int_0, &ctxt),
	     tristate (tristate::TS_TRUE));
  ASSERT_EQ (merged.eval_condition (x, LT_EXPR, n, &ctxt),
	     tristate (tristate::TS_TRUE));

  ASSERT_EQ (merged.eval_condition (z, NE_EXPR, int_5, &ctxt),
	     tristate (tristate::TS_UNKNOWN));
  ASSERT_EQ (merged.eval_condition (x, LT_EXPR, y, &ctxt),
	     tristate (tristate::TS_UNKNOWN));
}

/* Verify that widening_svalue::eval_condition_without_cm works as
   expected.  */

static void
test_widening_constraints ()
{
  program_point point (program_point::origin ());
  tree int_0 = build_int_cst (integer_type_node, 0);
  tree int_m1 = build_int_cst (integer_type_node, -1);
  tree int_1 = build_int_cst (integer_type_node, 1);
  tree int_256 = build_int_cst (integer_type_node, 256);
  region_model_manager mgr;
  test_region_model_context ctxt;
  const svalue *int_0_sval = mgr.get_or_create_constant_svalue (int_0);
  const svalue *int_1_sval = mgr.get_or_create_constant_svalue (int_1);
  const svalue *w_zero_then_one_sval
    = mgr.get_or_create_widening_svalue (integer_type_node, point,
					  int_0_sval, int_1_sval);
  const widening_svalue *w_zero_then_one
    = w_zero_then_one_sval->dyn_cast_widening_svalue ();
  ASSERT_EQ (w_zero_then_one->get_direction (),
	     widening_svalue::DIR_ASCENDING);
  ASSERT_EQ (w_zero_then_one->eval_condition_without_cm (LT_EXPR, int_m1),
	     tristate::TS_FALSE);
  ASSERT_EQ (w_zero_then_one->eval_condition_without_cm (LT_EXPR, int_0),
	     tristate::TS_FALSE);
  ASSERT_EQ (w_zero_then_one->eval_condition_without_cm (LT_EXPR, int_1),
	     tristate::TS_UNKNOWN);
  ASSERT_EQ (w_zero_then_one->eval_condition_without_cm (LT_EXPR, int_256),
	     tristate::TS_UNKNOWN);

  ASSERT_EQ (w_zero_then_one->eval_condition_without_cm (LE_EXPR, int_m1),
	     tristate::TS_FALSE);
  ASSERT_EQ (w_zero_then_one->eval_condition_without_cm (LE_EXPR, int_0),
	     tristate::TS_UNKNOWN);
  ASSERT_EQ (w_zero_then_one->eval_condition_without_cm (LE_EXPR, int_1),
	     tristate::TS_UNKNOWN);
  ASSERT_EQ (w_zero_then_one->eval_condition_without_cm (LE_EXPR, int_256),
	     tristate::TS_UNKNOWN);

  ASSERT_EQ (w_zero_then_one->eval_condition_without_cm (GT_EXPR, int_m1),
	     tristate::TS_TRUE);
  ASSERT_EQ (w_zero_then_one->eval_condition_without_cm (GT_EXPR, int_0),
	     tristate::TS_UNKNOWN);
  ASSERT_EQ (w_zero_then_one->eval_condition_without_cm (GT_EXPR, int_1),
	     tristate::TS_UNKNOWN);
  ASSERT_EQ (w_zero_then_one->eval_condition_without_cm (GT_EXPR, int_256),
	     tristate::TS_UNKNOWN);

  ASSERT_EQ (w_zero_then_one->eval_condition_without_cm (GE_EXPR, int_m1),
	     tristate::TS_TRUE);
  ASSERT_EQ (w_zero_then_one->eval_condition_without_cm (GE_EXPR, int_0),
	     tristate::TS_TRUE);
  ASSERT_EQ (w_zero_then_one->eval_condition_without_cm (GE_EXPR, int_1),
	     tristate::TS_UNKNOWN);
  ASSERT_EQ (w_zero_then_one->eval_condition_without_cm (GE_EXPR, int_256),
	     tristate::TS_UNKNOWN);

  ASSERT_EQ (w_zero_then_one->eval_condition_without_cm (EQ_EXPR, int_m1),
	     tristate::TS_FALSE);
  ASSERT_EQ (w_zero_then_one->eval_condition_without_cm (EQ_EXPR, int_0),
	     tristate::TS_UNKNOWN);
  ASSERT_EQ (w_zero_then_one->eval_condition_without_cm (EQ_EXPR, int_1),
	     tristate::TS_UNKNOWN);
  ASSERT_EQ (w_zero_then_one->eval_condition_without_cm (EQ_EXPR, int_256),
	     tristate::TS_UNKNOWN);

  ASSERT_EQ (w_zero_then_one->eval_condition_without_cm (NE_EXPR, int_m1),
	     tristate::TS_TRUE);
  ASSERT_EQ (w_zero_then_one->eval_condition_without_cm (NE_EXPR, int_0),
	     tristate::TS_UNKNOWN);
  ASSERT_EQ (w_zero_then_one->eval_condition_without_cm (NE_EXPR, int_1),
	     tristate::TS_UNKNOWN);
  ASSERT_EQ (w_zero_then_one->eval_condition_without_cm (NE_EXPR, int_256),
	     tristate::TS_UNKNOWN);
}

/* Verify merging constraints for states simulating successive iterations
   of a loop.
   Simulate:
     for (i = 0; i < 256; i++)
       [...body...]
   i.e. this gimple:.
     i_15 = 0;
     goto <bb 4>;

   <bb 4> :
     i_11 = PHI <i_15(2), i_23(3)>
     if (i_11 <= 255)
       goto <bb 3>;
     else
       goto [AFTER LOOP]

   <bb 3> :
     [LOOP BODY]
     i_23 = i_11 + 1;

   and thus these ops (and resultant states):
     i_11 = PHI()
       {i_11: 0}
     add_constraint (i_11 <= 255) [for the true edge]
       {i_11: 0}  [constraint was a no-op]
     i_23 = i_11 + 1;
       {i_22: 1}
     i_11 = PHI()
       {i_11: WIDENED (at phi, 0, 1)}
     add_constraint (i_11 <= 255) [for the true edge]
       {i_11: WIDENED (at phi, 0, 1); WIDENED <= 255}
     i_23 = i_11 + 1;
       {i_23: (WIDENED (at phi, 0, 1) + 1); WIDENED <= 255}
     i_11 = PHI(); merge with state at phi above
       {i_11: WIDENED (at phi, 0, 1); WIDENED <= 256}
         [changing meaning of "WIDENED" here]
     if (i_11 <= 255)
        T: {i_11: WIDENED (at phi, 0, 1); WIDENED <= 255}; cache hit
        F: {i_11: 256}
 */

static void
test_iteration_1 ()
{
  program_point point (program_point::origin ());

  tree int_0 = build_int_cst (integer_type_node, 0);
  tree int_1 = build_int_cst (integer_type_node, 1);
  tree int_256 = build_int_cst (integer_type_node, 256);
  tree int_257 = build_int_cst (integer_type_node, 257);
  tree i = build_global_decl ("i", integer_type_node);

  region_model_manager mgr;
  test_region_model_context ctxt;

  /* model0: i: 0.  */
  region_model model0 (&mgr);
  model0.set_value (i, int_0, &ctxt);

  /* model1: i: 1.  */
  region_model model1 (&mgr);
  model1.set_value (i, int_1, &ctxt);

  /* Should merge "i" to a widened value.  */
  region_model model2 (&mgr);
  ASSERT_TRUE (model1.can_merge_with_p (model0, point, &model2));
  const svalue *merged_i = model2.get_rvalue (i, &ctxt);
  ASSERT_EQ (merged_i->get_kind (), SK_WIDENING);
  const widening_svalue *w = merged_i->dyn_cast_widening_svalue ();
  ASSERT_EQ (w->get_direction (), widening_svalue::DIR_ASCENDING);

  /* Add constraint: i < 256  */
  model2.add_constraint (i, LT_EXPR, int_256, &ctxt);
  ASSERT_EQ (model2.eval_condition (i, LT_EXPR, int_256, &ctxt),
	     tristate (tristate::TS_TRUE));
  ASSERT_EQ (model2.eval_condition (i, GE_EXPR, int_0, &ctxt),
	     tristate (tristate::TS_TRUE));

  /* Try merging with the initial state.  */
  region_model model3 (&mgr);
  ASSERT_TRUE (model2.can_merge_with_p (model0, point, &model3));
  /* Merging the merged value with the initial value should be idempotent,
     so that the analysis converges.  */
  ASSERT_EQ (model3.get_rvalue (i, &ctxt), merged_i);
  /* Merger of 0 and a widening value with constraint < CST
     should retain the constraint, even though it was implicit
     for the 0 case.  */
  ASSERT_EQ (model3.eval_condition (i, LT_EXPR, int_256, &ctxt),
	     tristate (tristate::TS_TRUE));
  /* ...and we should have equality: the analysis should have converged.  */
  ASSERT_EQ (model3, model2);

  /* "i_23 = i_11 + 1;"  */
  region_model model4 (model3);
  ASSERT_EQ (model4, model2);
  model4.set_value (i, build2 (PLUS_EXPR, integer_type_node, i, int_1), &ctxt);
  const svalue *plus_one = model4.get_rvalue (i, &ctxt);
  ASSERT_EQ (plus_one->get_kind (), SK_BINOP);

  /* Try merging with the "i: 1" state.  */
  region_model model5 (&mgr);
  ASSERT_TRUE (model4.can_merge_with_p (model1, point, &model5));
  ASSERT_EQ (model5.get_rvalue (i, &ctxt), plus_one);
  ASSERT_EQ (model5, model4);

  /* "i_11 = PHI();" merge with state at phi above.
     For i, we should have a merger of WIDENING with WIDENING + 1,
     and this should be WIDENING again.  */
  region_model model6 (&mgr);
  ASSERT_TRUE (model5.can_merge_with_p (model2, point, &model6));
  const svalue *merged_widening = model6.get_rvalue (i, &ctxt);
  ASSERT_EQ (merged_widening->get_kind (), SK_WIDENING);

  ASSERT_CONDITION_TRUE (model6, i, LT_EXPR, int_257);
}

/* Verify that if we mark a pointer to a malloc-ed region as non-NULL,
   all cast pointers to that region are also known to be non-NULL.  */

static void
test_malloc_constraints ()
{
  region_model_manager mgr;
  region_model model (&mgr);
  tree p = build_global_decl ("p", ptr_type_node);
  tree char_star = build_pointer_type (char_type_node);
  tree q = build_global_decl ("q", char_star);
  tree null_ptr = build_int_cst (ptr_type_node, 0);

  const svalue *size_in_bytes
    = mgr.get_or_create_unknown_svalue (integer_type_node);
  const region *reg = model.create_region_for_heap_alloc (size_in_bytes);
  const svalue *sval = mgr.get_ptr_svalue (ptr_type_node, reg);
  model.set_value (model.get_lvalue (p, NULL), sval, NULL);
  model.set_value (q, p, NULL);

  ASSERT_CONDITION_UNKNOWN (model, p, NE_EXPR, null_ptr);
  ASSERT_CONDITION_UNKNOWN (model, p, EQ_EXPR, null_ptr);
  ASSERT_CONDITION_UNKNOWN (model, q, NE_EXPR, null_ptr);
  ASSERT_CONDITION_UNKNOWN (model, q, EQ_EXPR, null_ptr);

  model.add_constraint (p, NE_EXPR, null_ptr, NULL);

  ASSERT_CONDITION_TRUE (model, p, NE_EXPR, null_ptr);
  ASSERT_CONDITION_FALSE (model, p, EQ_EXPR, null_ptr);
  ASSERT_CONDITION_TRUE (model, q, NE_EXPR, null_ptr);
  ASSERT_CONDITION_FALSE (model, q, EQ_EXPR, null_ptr);
}

/* Smoketest of getting and setting the value of a variable.  */

static void
test_var ()
{
  /* "int i;"  */
  tree i = build_global_decl ("i", integer_type_node);

  tree int_17 = build_int_cst (integer_type_node, 17);
  tree int_m3 = build_int_cst (integer_type_node, -3);

  region_model_manager mgr;
  region_model model (&mgr);

  const region *i_reg = model.get_lvalue (i, NULL);
  ASSERT_EQ (i_reg->get_kind (), RK_DECL);

  /* Reading "i" should give a symbolic "initial value".  */
  const svalue *sval_init = model.get_rvalue (i, NULL);
  ASSERT_EQ (sval_init->get_kind (), SK_INITIAL);
  ASSERT_EQ (sval_init->dyn_cast_initial_svalue ()->get_region (), i_reg);
  /* ..and doing it again should give the same "initial value".  */
  ASSERT_EQ (model.get_rvalue (i, NULL), sval_init);

  /* "i = 17;".  */
  model.set_value (i, int_17, NULL);
  ASSERT_EQ (model.get_rvalue (i, NULL),
	     model.get_rvalue (int_17, NULL));

  /* "i = -3;".  */
  model.set_value (i, int_m3, NULL);
  ASSERT_EQ (model.get_rvalue (i, NULL),
	     model.get_rvalue (int_m3, NULL));

  /* Verify get_offset for "i".  */
  {
    region_offset offset = i_reg->get_offset ();
    ASSERT_EQ (offset.get_base_region (), i_reg);
    ASSERT_EQ (offset.get_bit_offset (), 0);
  }
}

static void
test_array_2 ()
{
  /* "int arr[10];"  */
  tree tlen = size_int (10);
  tree arr_type
    = build_array_type (integer_type_node, build_index_type (tlen));
  tree arr = build_global_decl ("arr", arr_type);

  /* "int i;"  */
  tree i = build_global_decl ("i", integer_type_node);

  tree int_0 = build_int_cst (integer_type_node, 0);
  tree int_1 = build_int_cst (integer_type_node, 1);

  tree arr_0 = build4 (ARRAY_REF, integer_type_node,
		       arr, int_0, NULL_TREE, NULL_TREE);
  tree arr_1 = build4 (ARRAY_REF, integer_type_node,
		       arr, int_1, NULL_TREE, NULL_TREE);
  tree arr_i = build4 (ARRAY_REF, integer_type_node,
		       arr, i, NULL_TREE, NULL_TREE);

  tree int_17 = build_int_cst (integer_type_node, 17);
  tree int_42 = build_int_cst (integer_type_node, 42);
  tree int_m3 = build_int_cst (integer_type_node, -3);

  region_model_manager mgr;
  region_model model (&mgr);
  /* "arr[0] = 17;".  */
  model.set_value (arr_0, int_17, NULL);
  /* "arr[1] = -3;".  */
  model.set_value (arr_1, int_m3, NULL);

  ASSERT_EQ (model.get_rvalue (arr_0, NULL), model.get_rvalue (int_17, NULL));
  ASSERT_EQ (model.get_rvalue (arr_1, NULL), model.get_rvalue (int_m3, NULL));

  /* Overwrite a pre-existing binding: "arr[1] = 42;".  */
  model.set_value (arr_1, int_42, NULL);
  ASSERT_EQ (model.get_rvalue (arr_1, NULL), model.get_rvalue (int_42, NULL));

  /* Verify get_offset for "arr[0]".  */
  {
    const region *arr_0_reg = model.get_lvalue (arr_0, NULL);
    region_offset offset = arr_0_reg->get_offset ();
    ASSERT_EQ (offset.get_base_region (), model.get_lvalue (arr, NULL));
    ASSERT_EQ (offset.get_bit_offset (), 0);
  }

  /* Verify get_offset for "arr[1]".  */
  {
    const region *arr_1_reg = model.get_lvalue (arr_1, NULL);
    region_offset offset = arr_1_reg->get_offset ();
    ASSERT_EQ (offset.get_base_region (), model.get_lvalue (arr, NULL));
    ASSERT_EQ (offset.get_bit_offset (), INT_TYPE_SIZE);
  }

  /* "arr[i] = i;" - this should remove the earlier bindings.  */
  model.set_value (arr_i, i, NULL);
  ASSERT_EQ (model.get_rvalue (arr_i, NULL), model.get_rvalue (i, NULL));
  ASSERT_EQ (model.get_rvalue (arr_0, NULL)->get_kind (), SK_UNKNOWN);

  /* "arr[0] = 17;" - this should remove the arr[i] binding.  */
  model.set_value (arr_0, int_17, NULL);
  ASSERT_EQ (model.get_rvalue (arr_0, NULL), model.get_rvalue (int_17, NULL));
  ASSERT_EQ (model.get_rvalue (arr_i, NULL)->get_kind (), SK_UNKNOWN);
}

/* Smoketest of dereferencing a pointer via MEM_REF.  */

static void
test_mem_ref ()
{
  /*
    x = 17;
    p = &x;
    *p;
   */
  tree x = build_global_decl ("x", integer_type_node);
  tree int_star = build_pointer_type (integer_type_node);
  tree p = build_global_decl ("p", int_star);

  tree int_17 = build_int_cst (integer_type_node, 17);
  tree addr_of_x = build1 (ADDR_EXPR, int_star, x);
  tree offset_0 = build_int_cst (integer_type_node, 0);
  tree star_p = build2 (MEM_REF, integer_type_node, p, offset_0);

  region_model_manager mgr;
  region_model model (&mgr);

  /* "x = 17;".  */
  model.set_value (x, int_17, NULL);

  /* "p = &x;".  */
  model.set_value (p, addr_of_x, NULL);

  const svalue *sval = model.get_rvalue (star_p, NULL);
  ASSERT_EQ (sval->maybe_get_constant (), int_17);
}

/* Test for a POINTER_PLUS_EXPR followed by a MEM_REF.
   Analogous to this code:
     void test_6 (int a[10])
     {
       __analyzer_eval (a[3] == 42); [should be UNKNOWN]
       a[3] = 42;
       __analyzer_eval (a[3] == 42); [should be TRUE]
     }
   from data-model-1.c, which looks like this at the gimple level:
       # __analyzer_eval (a[3] == 42); [should be UNKNOWN]
       int *_1 = a_10(D) + 12;   # POINTER_PLUS_EXPR
       int _2 = *_1;             # MEM_REF
       _Bool _3 = _2 == 42;
       int _4 = (int) _3;
       __analyzer_eval (_4);

       # a[3] = 42;
       int *_5 = a_10(D) + 12;   # POINTER_PLUS_EXPR
       *_5 = 42;                 # MEM_REF

       # __analyzer_eval (a[3] == 42); [should be TRUE]
       int *_6 = a_10(D) + 12;   # POINTER_PLUS_EXPR
       int _7 = *_6;             # MEM_REF
       _Bool _8 = _7 == 42;
       int _9 = (int) _8;
       __analyzer_eval (_9);  */

static void
test_POINTER_PLUS_EXPR_then_MEM_REF ()
{
  tree int_star = build_pointer_type (integer_type_node);
  tree a = build_global_decl ("a", int_star);
  tree offset_12 = build_int_cst (size_type_node, 12);
  tree pointer_plus_expr = build2 (POINTER_PLUS_EXPR, int_star, a, offset_12);
  tree offset_0 = build_int_cst (integer_type_node, 0);
  tree mem_ref = build2 (MEM_REF, integer_type_node,
			 pointer_plus_expr, offset_0);
  region_model_manager mgr;
  region_model m (&mgr);

  tree int_42 = build_int_cst (integer_type_node, 42);
  m.set_value (mem_ref, int_42, NULL);
  ASSERT_EQ (m.get_rvalue (mem_ref, NULL)->maybe_get_constant (), int_42);
}

/* Verify that malloc works.  */

static void
test_malloc ()
{
  tree int_star = build_pointer_type (integer_type_node);
  tree p = build_global_decl ("p", int_star);
  tree n = build_global_decl ("n", integer_type_node);
  tree n_times_4 = build2 (MULT_EXPR, size_type_node,
			   n, build_int_cst (size_type_node, 4));

  region_model_manager mgr;
  test_region_model_context ctxt;
  region_model model (&mgr);

  /* "p = malloc (n * 4);".  */
  const svalue *size_sval = model.get_rvalue (n_times_4, &ctxt);
  const region *reg = model.create_region_for_heap_alloc (size_sval);
  const svalue *ptr = mgr.get_ptr_svalue (int_star, reg);
  model.set_value (model.get_lvalue (p, &ctxt), ptr, &ctxt);
  // TODO: verify dynamic extents
}

/* Verify that alloca works.  */

static void
test_alloca ()
{
  auto_vec <tree> param_types;
  tree fndecl = make_fndecl (integer_type_node,
			     "test_fn",
			     param_types);
  allocate_struct_function (fndecl, true);


  tree int_star = build_pointer_type (integer_type_node);
  tree p = build_global_decl ("p", int_star);
  tree n = build_global_decl ("n", integer_type_node);
  tree n_times_4 = build2 (MULT_EXPR, size_type_node,
			   n, build_int_cst (size_type_node, 4));

  region_model_manager mgr;
  test_region_model_context ctxt;
  region_model model (&mgr);

  /* Push stack frame.  */
  const region *frame_reg
    = model.push_frame (DECL_STRUCT_FUNCTION (fndecl),
			NULL, &ctxt);
  /* "p = alloca (n * 4);".  */
  const svalue *size_sval = model.get_rvalue (n_times_4, &ctxt);
  const region *reg = model.create_region_for_alloca (size_sval);
  ASSERT_EQ (reg->get_parent_region (), frame_reg);
  const svalue *ptr = mgr.get_ptr_svalue (int_star, reg);
  model.set_value (model.get_lvalue (p, &ctxt), ptr, &ctxt);
  // TODO: verify dynamic extents

  /* Verify that the pointers to the alloca region are replaced by
     poisoned values when the frame is popped.  */
  model.pop_frame (NULL, NULL, &ctxt);
  ASSERT_EQ (model.get_rvalue (p, &ctxt)->get_kind (), SK_POISONED);
}

/* Verify that svalue::involves_p works.  */

static void
test_involves_p ()
{
  region_model_manager mgr;
  tree int_star = build_pointer_type (integer_type_node);
  tree p = build_global_decl ("p", int_star);
  tree q = build_global_decl ("q", int_star);

  test_region_model_context ctxt;
  region_model model (&mgr);
  const svalue *p_init = model.get_rvalue (p, &ctxt);
  const svalue *q_init = model.get_rvalue (q, &ctxt);

  ASSERT_TRUE (p_init->involves_p (p_init));
  ASSERT_FALSE (p_init->involves_p (q_init));

  const region *star_p_reg = mgr.get_symbolic_region (p_init);
  const region *star_q_reg = mgr.get_symbolic_region (q_init);

  const svalue *init_star_p = mgr.get_or_create_initial_value (star_p_reg);
  const svalue *init_star_q = mgr.get_or_create_initial_value (star_q_reg);

  ASSERT_TRUE (init_star_p->involves_p (p_init));
  ASSERT_FALSE (p_init->involves_p (init_star_p));
  ASSERT_FALSE (init_star_p->involves_p (q_init));
  ASSERT_TRUE (init_star_q->involves_p (q_init));
  ASSERT_FALSE (init_star_q->involves_p (p_init));
}

/* Run all of the selftests within this file.  */

void
analyzer_region_model_cc_tests ()
{
  test_tree_cmp_on_constants ();
  test_dump ();
  test_struct ();
  test_array_1 ();
  test_get_representative_tree ();
  test_unique_constants ();
  test_unique_unknowns ();
  test_initial_svalue_folding ();
  test_unaryop_svalue_folding ();
  test_binop_svalue_folding ();
  test_sub_svalue_folding ();
  test_descendent_of_p ();
  test_assignment ();
  test_compound_assignment ();
  test_stack_frames ();
  test_get_representative_path_var ();
  test_equality_1 ();
  test_canonicalization_2 ();
  test_canonicalization_3 ();
  test_canonicalization_4 ();
  test_state_merging ();
  test_constraint_merging ();
  test_widening_constraints ();
  test_iteration_1 ();
  test_malloc_constraints ();
  test_var ();
  test_array_2 ();
  test_mem_ref ();
  test_POINTER_PLUS_EXPR_then_MEM_REF ();
  test_malloc ();
  test_alloca ();
  test_involves_p ();
}

} // namespace selftest

#endif /* CHECKING_P */

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
