/* Classes for modeling the state of memory.
   Copyright (C) 2019-2023 Free Software Foundation, Inc.
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
#define INCLUDE_MEMORY
#include "system.h"
#include "coretypes.h"
#include "make-unique.h"
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
#include "bitmap.h"
#include "selftest.h"
#include "analyzer/analyzer.h"
#include "analyzer/analyzer-logging.h"
#include "ordered-hash-map.h"
#include "options.h"
#include "cgraph.h"
#include "cfg.h"
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
#include "analyzer/program-state.h"
#include "analyzer/call-summary.h"
#include "stor-layout.h"
#include "attribs.h"
#include "tree-object-size.h"
#include "gimple-ssa.h"
#include "tree-phinodes.h"
#include "tree-ssa-operands.h"
#include "ssa-iterators.h"
#include "calls.h"
#include "is-a.h"
#include "gcc-rich-location.h"
#include "analyzer/checker-event.h"
#include "analyzer/checker-path.h"
#include "analyzer/feasible-graph.h"

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

/* class region_to_value_map.  */

/* Assignment operator for region_to_value_map.  */

region_to_value_map &
region_to_value_map::operator= (const region_to_value_map &other)
{
  m_hash_map.empty ();
  for (auto iter : other.m_hash_map)
    {
      const region *reg = iter.first;
      const svalue *sval = iter.second;
      m_hash_map.put (reg, sval);
    }
  return *this;
}

/* Equality operator for region_to_value_map.  */

bool
region_to_value_map::operator== (const region_to_value_map &other) const
{
  if (m_hash_map.elements () != other.m_hash_map.elements ())
    return false;

  for (auto iter : *this)
    {
      const region *reg = iter.first;
      const svalue *sval = iter.second;
      const svalue * const *other_slot = other.get (reg);
      if (other_slot == NULL)
	return false;
      if (sval != *other_slot)
	return false;
    }

  return true;
}

/* Dump this object to PP.  */

void
region_to_value_map::dump_to_pp (pretty_printer *pp, bool simple,
				 bool multiline) const
{
  auto_vec<const region *> regs;
  for (iterator iter = begin (); iter != end (); ++iter)
    regs.safe_push ((*iter).first);
  regs.qsort (region::cmp_ptr_ptr);
  if (multiline)
    pp_newline (pp);
  else
    pp_string (pp, " {");
  unsigned i;
  const region *reg;
  FOR_EACH_VEC_ELT (regs, i, reg)
    {
      if (multiline)
	pp_string (pp, "  ");
      else if (i > 0)
	pp_string (pp, ", ");
      reg->dump_to_pp (pp, simple);
      pp_string (pp, ": ");
      const svalue *sval = *get (reg);
      sval->dump_to_pp (pp, true);
      if (multiline)
	pp_newline (pp);
    }
  if (!multiline)
    pp_string (pp, "}");
}

/* Dump this object to stderr.  */

DEBUG_FUNCTION void
region_to_value_map::dump (bool simple) const
{
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  pp_show_color (&pp) = pp_show_color (global_dc->printer);
  pp.buffer->stream = stderr;
  dump_to_pp (&pp, simple, true);
  pp_newline (&pp);
  pp_flush (&pp);
}


/* Attempt to merge THIS with OTHER, writing the result
   to OUT.

   For now, write (region, value) mappings that are in common between THIS
   and OTHER to OUT, effectively taking the intersection.

   Reject merger of different values.  */

bool
region_to_value_map::can_merge_with_p (const region_to_value_map &other,
				       region_to_value_map *out) const
{
  for (auto iter : *this)
    {
      const region *iter_reg = iter.first;
      const svalue *iter_sval = iter.second;
      const svalue * const * other_slot = other.get (iter_reg);
      if (other_slot)
	{
	  if (iter_sval == *other_slot)
	    out->put (iter_reg, iter_sval);
	  else
	    return false;
	}
    }
  return true;
}

/* Purge any state involving SVAL.  */

void
region_to_value_map::purge_state_involving (const svalue *sval)
{
  auto_vec<const region *> to_purge;
  for (auto iter : *this)
    {
      const region *iter_reg = iter.first;
      const svalue *iter_sval = iter.second;
      if (iter_reg->involves_p (sval) || iter_sval->involves_p (sval))
	to_purge.safe_push (iter_reg);
    }
  for (auto iter : to_purge)
    m_hash_map.remove (iter);
}

/* class region_model.  */

/* Ctor for region_model: construct an "empty" model.  */

region_model::region_model (region_model_manager *mgr)
: m_mgr (mgr), m_store (), m_current_frame (NULL),
  m_dynamic_extents ()
{
  m_constraints = new constraint_manager (mgr);
}

/* region_model's copy ctor.  */

region_model::region_model (const region_model &other)
: m_mgr (other.m_mgr), m_store (other.m_store),
  m_constraints (new constraint_manager (*other.m_constraints)),
  m_current_frame (other.m_current_frame),
  m_dynamic_extents (other.m_dynamic_extents)
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

  m_dynamic_extents = other.m_dynamic_extents;

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

  if (m_dynamic_extents != other.m_dynamic_extents)
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

  /* Dump sizes of dynamic regions, if any are known.  */
  if (!m_dynamic_extents.is_empty ())
    {
      pp_string (pp, "dynamic_extents:");
      m_dynamic_extents.dump_to_pp (pp, simple, multiline);
    }
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

/* Assert that this object is valid.  */

void
region_model::validate () const
{
  m_store.validate ();
}

/* Canonicalize the store and constraints, to maximize the chance of
   equality between region_model instances.  */

void
region_model::canonicalize ()
{
  m_store.canonicalize (m_mgr->get_store_manager ());
  m_constraints->canonicalize ();

  if (!m_dynamic_extents.is_empty ())
    {
      /* Purge any dynamic extents for regions that aren't referenced.
	 Normally these are eliminated when leaks are detected, but we
	 can also gain stray heap_allocated_regions that aren't seen
	 by the leak-detection code.  This happens when
	 region_model::on_call_pre provides a default result for a
	 function with both attributes "malloc" and "alloc_size" that
	 also has a known_function implementation.
	 Purge dynamic extent information for such regions.  */
      auto_bitmap referenced_base_region_ids;
      get_referenced_base_regions (referenced_base_region_ids);
      auto_vec<const region *> purgable_dyn_extents;
      for (auto iter : m_dynamic_extents)
	{
	  const region *reg = iter.first;
	  if (!bitmap_bit_p (referenced_base_region_ids, reg->get_id ()))
	    purgable_dyn_extents.safe_push (reg);
	}
      for (auto reg : purgable_dyn_extents)
	m_dynamic_extents.remove (reg);
    }
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
  poisoned_value_diagnostic (tree expr, enum poison_kind pkind,
			     const region *src_region,
			     tree check_expr)
  : m_expr (expr), m_pkind (pkind),
    m_src_region (src_region),
    m_check_expr (check_expr)
  {}

  const char *get_kind () const final override { return "poisoned_value_diagnostic"; }

  bool use_of_uninit_p () const final override
  {
    return m_pkind == POISON_KIND_UNINIT;
  }

  bool operator== (const poisoned_value_diagnostic &other) const
  {
    return (m_expr == other.m_expr
	    && m_pkind == other.m_pkind
	    && m_src_region == other.m_src_region);
  }

  int get_controlling_option () const final override
  {
    switch (m_pkind)
      {
      default:
	gcc_unreachable ();
      case POISON_KIND_UNINIT:
	return OPT_Wanalyzer_use_of_uninitialized_value;
      case POISON_KIND_FREED:
	return OPT_Wanalyzer_use_after_free;
      case POISON_KIND_POPPED_STACK:
	return OPT_Wanalyzer_use_of_pointer_in_stale_stack_frame;
      }
  }

  bool terminate_path_p () const final override { return true; }

  bool emit (rich_location *rich_loc, logger *) final override
  {
    switch (m_pkind)
      {
      default:
	gcc_unreachable ();
      case POISON_KIND_UNINIT:
	{
	  diagnostic_metadata m;
	  m.add_cwe (457); /* "CWE-457: Use of Uninitialized Variable".  */
	  return warning_meta (rich_loc, m, get_controlling_option (),
			       "use of uninitialized value %qE",
			       m_expr);
	}
	break;
      case POISON_KIND_FREED:
	{
	  diagnostic_metadata m;
	  m.add_cwe (416); /* "CWE-416: Use After Free".  */
	  return warning_meta (rich_loc, m, get_controlling_option (),
			       "use after %<free%> of %qE",
			       m_expr);
	}
	break;
      case POISON_KIND_POPPED_STACK:
	{
	  /* TODO: which CWE?  */
	  return warning_at
	    (rich_loc, get_controlling_option (),
	     "dereferencing pointer %qE to within stale stack frame",
	     m_expr);
	}
	break;
      }
  }

  label_text describe_final_event (const evdesc::final_event &ev) final override
  {
    switch (m_pkind)
      {
      default:
	gcc_unreachable ();
      case POISON_KIND_UNINIT:
	return ev.formatted_print ("use of uninitialized value %qE here",
				   m_expr);
      case POISON_KIND_FREED:
	return ev.formatted_print ("use after %<free%> of %qE here",
				   m_expr);
      case POISON_KIND_POPPED_STACK:
	return ev.formatted_print
	  ("dereferencing pointer %qE to within stale stack frame",
	   m_expr);
      }
  }

  void mark_interesting_stuff (interesting_t *interest) final override
  {
    if (m_src_region)
      interest->add_region_creation (m_src_region);
  }

  /* Attempt to suppress false positives.
     Reject paths where the value of the underlying region isn't poisoned.
     This can happen due to state merging when exploring the exploded graph,
     where the more precise analysis during feasibility analysis finds that
     the region is in fact valid.
     To do this we need to get the value from the fgraph.  Unfortunately
     we can't simply query the state of m_src_region (from the enode),
     since it might be a different region in the fnode state (e.g. with
     heap-allocated regions, the numbering could be different).
     Hence we access m_check_expr, if available.  */

  bool check_valid_fpath_p (const feasible_node &fnode,
			    const gimple *emission_stmt)
    const final override
  {
    if (!m_check_expr)
      return true;

    /* We've reached the enode, but not necessarily the right function_point.
       Try to get the state at the correct stmt.  */
    region_model emission_model (fnode.get_model ().get_manager());
    if (!fnode.get_state_at_stmt (emission_stmt, &emission_model))
      /* Couldn't get state; accept this diagnostic.  */
      return true;

    const svalue *fsval = emission_model.get_rvalue (m_check_expr, NULL);
    /* Check to see if the expr is also poisoned in FNODE (and in the
       same way).  */
    const poisoned_svalue * fspval = fsval->dyn_cast_poisoned_svalue ();
    if (!fspval)
      return false;
    if (fspval->get_poison_kind () != m_pkind)
      return false;
    return true;
  }

private:
  tree m_expr;
  enum poison_kind m_pkind;
  const region *m_src_region;
  tree m_check_expr;
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

  const char *get_kind () const final override
  {
    return "shift_count_negative_diagnostic";
  }

  bool operator== (const shift_count_negative_diagnostic &other) const
  {
    return (m_assign == other.m_assign
	    && same_tree_p (m_count_cst, other.m_count_cst));
  }

  int get_controlling_option () const final override
  {
    return OPT_Wanalyzer_shift_count_negative;
  }

  bool emit (rich_location *rich_loc, logger *) final override
  {
    return warning_at (rich_loc, get_controlling_option (),
		       "shift by negative count (%qE)", m_count_cst);
  }

  label_text describe_final_event (const evdesc::final_event &ev) final override
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

  const char *get_kind () const final override
  {
    return "shift_count_overflow_diagnostic";
  }

  bool operator== (const shift_count_overflow_diagnostic &other) const
  {
    return (m_assign == other.m_assign
	    && m_operand_precision == other.m_operand_precision
	    && same_tree_p (m_count_cst, other.m_count_cst));
  }

  int get_controlling_option () const final override
  {
    return OPT_Wanalyzer_shift_count_overflow;
  }

  bool emit (rich_location *rich_loc, logger *) final override
  {
    return warning_at (rich_loc, get_controlling_option (),
		       "shift by count (%qE) >= precision of type (%qi)",
		       m_count_cst, m_operand_precision);
  }

  label_text describe_final_event (const evdesc::final_event &ev) final override
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
	      if (TREE_CODE (rhs2_cst) == INTEGER_CST
		  && INTEGRAL_TYPE_P (TREE_TYPE (rhs1)))
		{
		  if (tree_int_cst_sgn (rhs2_cst) < 0)
		    ctxt->warn
		      (make_unique<shift_count_negative_diagnostic>
			 (assign, rhs2_cst));
		  else if (compare_tree_int (rhs2_cst,
					     TYPE_PRECISION (TREE_TYPE (rhs1)))
			   >= 0)
		    ctxt->warn
		      (make_unique<shift_count_overflow_diagnostic>
			 (assign,
			  int (TYPE_PRECISION (TREE_TYPE (rhs1))),
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

/* Workaround for discarding certain false positives from
   -Wanalyzer-use-of-uninitialized-value
   of the form:
     ((A OR-IF B) OR-IF C)
   and:
     ((A AND-IF B) AND-IF C)
   where evaluating B is redundant, but could involve simple accesses of
   uninitialized locals.

   When optimization is turned on the FE can immediately fold compound
   conditionals.  Specifically, c_parser_condition parses this condition:
     ((A OR-IF B) OR-IF C)
   and calls c_fully_fold on the condition.
   Within c_fully_fold, fold_truth_andor is called, which bails when
   optimization is off, but if any optimization is turned on can convert the
     ((A OR-IF B) OR-IF C)
   into:
     ((A OR B) OR_IF C)
   for sufficiently simple B
   i.e. the inner OR-IF becomes an OR.
   At gimplification time the inner OR becomes BIT_IOR_EXPR (in gimplify_expr),
   giving this for the inner condition:
      tmp = A | B;
      if (tmp)
   thus effectively synthesizing a redundant access of B when optimization
   is turned on, when compared to:
      if (A) goto L1; else goto L4;
  L1: if (B) goto L2; else goto L4;
  L2: if (C) goto L3; else goto L4;
   for the unoptimized case.

   Return true if CTXT appears to be  handling such a short-circuitable stmt,
   such as the def-stmt for B for the:
      tmp = A | B;
   case above, for the case where A is true and thus B would have been
   short-circuited without optimization, using MODEL for the value of A.  */

static bool
within_short_circuited_stmt_p (const region_model *model,
			       const gassign *assign_stmt)
{
  /* We must have an assignment to a temporary of _Bool type.  */
  tree lhs = gimple_assign_lhs (assign_stmt);
  if (TREE_TYPE (lhs) != boolean_type_node)
    return false;
  if (TREE_CODE (lhs) != SSA_NAME)
    return false;
  if (SSA_NAME_VAR (lhs) != NULL_TREE)
    return false;

  /* The temporary bool must be used exactly once: as the second arg of
     a BIT_IOR_EXPR or BIT_AND_EXPR.  */
  use_operand_p use_op;
  gimple *use_stmt;
  if (!single_imm_use (lhs, &use_op, &use_stmt))
    return false;
  const gassign *use_assign = dyn_cast <const gassign *> (use_stmt);
  if (!use_assign)
    return false;
  enum tree_code op = gimple_assign_rhs_code (use_assign);
  if (!(op == BIT_IOR_EXPR ||op == BIT_AND_EXPR))
    return false;
  if (!(gimple_assign_rhs1 (use_assign) != lhs
	&& gimple_assign_rhs2 (use_assign) == lhs))
    return false;

  /* The first arg of the bitwise stmt must have a known value in MODEL
     that implies that the value of the second arg doesn't matter, i.e.
     1 for bitwise or, 0 for bitwise and.  */
  tree other_arg = gimple_assign_rhs1 (use_assign);
  /* Use a NULL ctxt here to avoid generating warnings.  */
  const svalue *other_arg_sval = model->get_rvalue (other_arg, NULL);
  tree other_arg_cst = other_arg_sval->maybe_get_constant ();
  if (!other_arg_cst)
    return false;
  switch (op)
    {
    default:
      gcc_unreachable ();
    case BIT_IOR_EXPR:
      if (zerop (other_arg_cst))
	return false;
      break;
    case BIT_AND_EXPR:
      if (!zerop (other_arg_cst))
	return false;
      break;
    }

  /* All tests passed.  We appear to be in a stmt that generates a boolean
     temporary with a value that won't matter.  */
  return true;
}

/* Workaround for discarding certain false positives from
   -Wanalyzer-use-of-uninitialized-value
   seen with -ftrivial-auto-var-init=.

   -ftrivial-auto-var-init= will generate calls to IFN_DEFERRED_INIT.

   If the address of the var is taken, gimplification will give us
   something like:

     _1 = .DEFERRED_INIT (4, 2, &"len"[0]);
     len = _1;

   The result of DEFERRED_INIT will be an uninit value; we don't
   want to emit a false positive for "len = _1;"

   Return true if ASSIGN_STMT is such a stmt.  */

static bool
due_to_ifn_deferred_init_p (const gassign *assign_stmt)

{
  /* We must have an assignment to a decl from an SSA name that's the
     result of a IFN_DEFERRED_INIT call.  */
  if (gimple_assign_rhs_code (assign_stmt) != SSA_NAME)
    return false;
  tree lhs = gimple_assign_lhs (assign_stmt);
  if (TREE_CODE (lhs) != VAR_DECL)
    return false;
  tree rhs = gimple_assign_rhs1 (assign_stmt);
  if (TREE_CODE (rhs) != SSA_NAME)
    return false;
  const gimple *def_stmt = SSA_NAME_DEF_STMT (rhs);
  const gcall *call = dyn_cast <const gcall *> (def_stmt);
  if (!call)
    return false;
  if (gimple_call_internal_p (call)
      && gimple_call_internal_fn (call) == IFN_DEFERRED_INIT)
    return true;
  return false;
}

/* Check for SVAL being poisoned, adding a warning to CTXT.
   Return SVAL, or, if a warning is added, another value, to avoid
   repeatedly complaining about the same poisoned value in followup code.
   SRC_REGION is a hint about where SVAL came from, and can be NULL.  */

const svalue *
region_model::check_for_poison (const svalue *sval,
				tree expr,
				const region *src_region,
				region_model_context *ctxt) const
{
  if (!ctxt)
    return sval;

  if (const poisoned_svalue *poisoned_sval = sval->dyn_cast_poisoned_svalue ())
    {
      enum poison_kind pkind = poisoned_sval->get_poison_kind ();

      /* Ignore uninitialized uses of empty types; there's nothing
	 to initialize.  */
      if (pkind == POISON_KIND_UNINIT
	  && sval->get_type ()
	  && is_empty_type (sval->get_type ()))
	return sval;

      if (pkind == POISON_KIND_UNINIT)
	if (const gimple *curr_stmt = ctxt->get_stmt ())
	  if (const gassign *assign_stmt
		= dyn_cast <const gassign *> (curr_stmt))
	    {
	      /* Special case to avoid certain false positives.  */
	      if (within_short_circuited_stmt_p (this, assign_stmt))
		return sval;

	      /* Special case to avoid false positive on
		 -ftrivial-auto-var-init=.  */
	      if (due_to_ifn_deferred_init_p (assign_stmt))
		return sval;
	  }

      /* If we have an SSA name for a temporary, we don't want to print
	 '<unknown>'.
	 Poisoned values are shared by type, and so we can't reconstruct
	 the tree other than via the def stmts, using
	 fixup_tree_for_diagnostic.  */
      tree diag_arg = fixup_tree_for_diagnostic (expr);
      if (src_region == NULL && pkind == POISON_KIND_UNINIT)
	src_region = get_region_for_poisoned_expr (expr);

      /* Can we reliably get the poisoned value from "expr"?
	 This is for use by poisoned_value_diagnostic::check_valid_fpath_p.
	 Unfortunately, we might not have a reliable value for EXPR.
	 Hence we only query its value now, and only use it if we get the
	 poisoned value back again.  */
      tree check_expr = expr;
      const svalue *foo_sval = get_rvalue (expr, NULL);
      if (foo_sval == sval)
	check_expr = expr;
      else
	check_expr = NULL;
      if (ctxt->warn (make_unique<poisoned_value_diagnostic> (diag_arg,
							      pkind,
							      src_region,
							      check_expr)))
	{
	  /* We only want to report use of a poisoned value at the first
	     place it gets used; return an unknown value to avoid generating
	     a chain of followup warnings.  */
	  sval = m_mgr->get_or_create_unknown_svalue (sval->get_type ());
	}

      return sval;
    }

  return sval;
}

/* Attempt to get a region for describing EXPR, the source of region of
   a poisoned_svalue for use in a poisoned_value_diagnostic.
   Return NULL if there is no good region to use.  */

const region *
region_model::get_region_for_poisoned_expr (tree expr) const
{
  if (TREE_CODE (expr) == SSA_NAME)
    {
      tree decl = SSA_NAME_VAR (expr);
      if (decl && DECL_P (decl))
	expr = decl;
      else
	return NULL;
    }
  return get_lvalue (expr, NULL);
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
      tree expr = get_diagnostic_tree_for_gassign (assign);
      check_for_poison (sval, expr, NULL, ctxt);
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
	const svalue *rhs_sval = get_rvalue (rhs1, ctxt);
	m_store.set_value (m_mgr->get_store_manager(), lhs_reg, rhs_sval,
			   ctxt ? ctxt->get_uncertainty () : NULL);
      }
      break;
    }
}

/* Handle the pre-sm-state part of STMT, modifying this object in-place.
   Write true to *OUT_UNKNOWN_SIDE_EFFECTS if the stmt has unknown
   side effects.  */

void
region_model::on_stmt_pre (const gimple *stmt,
			   bool *out_unknown_side_effects,
			   region_model_context *ctxt)
{
  switch (gimple_code (stmt))
    {
    default:
      /* No-op for now.  */
      break;

    case GIMPLE_ASSIGN:
      {
	const gassign *assign = as_a <const gassign *> (stmt);
	on_assignment (assign, ctxt);
      }
      break;

    case GIMPLE_ASM:
      {
	const gasm *asm_stmt = as_a <const gasm *> (stmt);
	on_asm_stmt (asm_stmt, ctxt);
      }
      break;

    case GIMPLE_CALL:
      {
	/* Track whether we have a gcall to a function that's not recognized by
	   anything, for which we don't have a function body, or for which we
	   don't know the fndecl.  */
	const gcall *call = as_a <const gcall *> (stmt);
	*out_unknown_side_effects = on_call_pre (call, ctxt);
      }
      break;

    case GIMPLE_RETURN:
      {
	const greturn *return_ = as_a <const greturn *> (stmt);
	on_return (return_, ctxt);
      }
      break;
    }
}

/* Ensure that all arguments at the call described by CD are checked
   for poisoned values, by calling get_rvalue on each argument.  */

void
region_model::check_call_args (const call_details &cd) const
{
  for (unsigned arg_idx = 0; arg_idx < cd.num_args (); arg_idx++)
    cd.get_arg_svalue (arg_idx);
}

/* Return true if CD is known to be a call to a function with
   __attribute__((const)).  */

static bool
const_fn_p (const call_details &cd)
{
  tree fndecl = cd.get_fndecl_for_call ();
  if (!fndecl)
    return false;
  gcc_assert (DECL_P (fndecl));
  return TREE_READONLY (fndecl);
}

/* If this CD is known to be a call to a function with
   __attribute__((const)), attempt to get a const_fn_result_svalue
   based on the arguments, or return NULL otherwise.  */

static const svalue *
maybe_get_const_fn_result (const call_details &cd)
{
  if (!const_fn_p (cd))
    return NULL;

  unsigned num_args = cd.num_args ();
  if (num_args > const_fn_result_svalue::MAX_INPUTS)
    /* Too many arguments.  */
    return NULL;

  auto_vec<const svalue *> inputs (num_args);
  for (unsigned arg_idx = 0; arg_idx < num_args; arg_idx++)
    {
      const svalue *arg_sval = cd.get_arg_svalue (arg_idx);
      if (!arg_sval->can_have_associated_state_p ())
	return NULL;
      inputs.quick_push (arg_sval);
    }

  region_model_manager *mgr = cd.get_manager ();
  const svalue *sval
    = mgr->get_or_create_const_fn_result_svalue (cd.get_lhs_type (),
						 cd.get_fndecl_for_call (),
						 inputs);
  return sval;
}

/* Update this model for an outcome of a call that returns a specific
   integer constant.
   If UNMERGEABLE, then make the result unmergeable, e.g. to prevent
   the state-merger code from merging success and failure outcomes.  */

void
region_model::update_for_int_cst_return (const call_details &cd,
					 int retval,
					 bool unmergeable)
{
  if (!cd.get_lhs_type ())
    return;
  if (TREE_CODE (cd.get_lhs_type ()) != INTEGER_TYPE)
    return;
  const svalue *result
    = m_mgr->get_or_create_int_cst (cd.get_lhs_type (), retval);
  if (unmergeable)
    result = m_mgr->get_or_create_unmergeable (result);
  set_value (cd.get_lhs_region (), result, cd.get_ctxt ());
}

/* Update this model for an outcome of a call that returns zero.
   If UNMERGEABLE, then make the result unmergeable, e.g. to prevent
   the state-merger code from merging success and failure outcomes.  */

void
region_model::update_for_zero_return (const call_details &cd,
				      bool unmergeable)
{
  update_for_int_cst_return (cd, 0, unmergeable);
}

/* Update this model for an outcome of a call that returns non-zero.  */

void
region_model::update_for_nonzero_return (const call_details &cd)
{
  if (!cd.get_lhs_type ())
    return;
  if (TREE_CODE (cd.get_lhs_type ()) != INTEGER_TYPE)
    return;
  const svalue *zero
    = m_mgr->get_or_create_int_cst (cd.get_lhs_type (), 0);
  const svalue *result
    = get_store_value (cd.get_lhs_region (), cd.get_ctxt ());
  add_constraint (result, NE_EXPR, zero, cd.get_ctxt ());
}

/* Subroutine of region_model::maybe_get_copy_bounds.
   The Linux kernel commonly uses
     min_t([unsigned] long, VAR, sizeof(T));
   to set an upper bound on the size of a copy_to_user.
   Attempt to simplify such sizes by trying to get the upper bound as a
   constant.
   Return the simplified svalue if possible, or NULL otherwise.  */

static const svalue *
maybe_simplify_upper_bound (const svalue *num_bytes_sval,
			    region_model_manager *mgr)
{
  tree type = num_bytes_sval->get_type ();
  while (const svalue *raw = num_bytes_sval->maybe_undo_cast ())
    num_bytes_sval = raw;
  if (const binop_svalue *binop_sval = num_bytes_sval->dyn_cast_binop_svalue ())
    if (binop_sval->get_op () == MIN_EXPR)
      if (binop_sval->get_arg1 ()->get_kind () == SK_CONSTANT)
	{
	  return mgr->get_or_create_cast (type, binop_sval->get_arg1 ());
	  /* TODO: we might want to also capture the constraint
	     when recording the diagnostic, or note that we're using
	     the upper bound.  */
	}
  return NULL;
}

/* Attempt to get an upper bound for the size of a copy when simulating a
   copy function.

   NUM_BYTES_SVAL is the symbolic value for the size of the copy.
   Use it if it's constant, otherwise try to simplify it.  Failing
   that, use the size of SRC_REG if constant.

   Return a symbolic value for an upper limit on the number of bytes
   copied, or NULL if no such value could be determined.  */

const svalue *
region_model::maybe_get_copy_bounds (const region *src_reg,
				     const svalue *num_bytes_sval)
{
  if (num_bytes_sval->maybe_get_constant ())
    return num_bytes_sval;

  if (const svalue *simplified
      = maybe_simplify_upper_bound (num_bytes_sval, m_mgr))
    num_bytes_sval = simplified;

  if (num_bytes_sval->maybe_get_constant ())
    return num_bytes_sval;

  /* For now, try just guessing the size as the capacity of the
     base region of the src.
     This is a hack; we might get too large a value.  */
  const region *src_base_reg = src_reg->get_base_region ();
  num_bytes_sval = get_capacity (src_base_reg);

  if (num_bytes_sval->maybe_get_constant ())
    return num_bytes_sval;

  /* Non-constant: give up. */
  return NULL;
}

/* Get any known_function for FNDECL for call CD.

   The call must match all assumptions made by the known_function (such as
   e.g. "argument 1's type must be a pointer type").

   Return NULL if no known_function is found, or it does not match the
   assumption(s).  */

const known_function *
region_model::get_known_function (tree fndecl, const call_details &cd) const
{
  known_function_manager *known_fn_mgr = m_mgr->get_known_function_manager ();
  return known_fn_mgr->get_match (fndecl, cd);
}

/* Get any known_function for IFN, or NULL.  */

const known_function *
region_model::get_known_function (enum internal_fn ifn) const
{
  known_function_manager *known_fn_mgr = m_mgr->get_known_function_manager ();
  return known_fn_mgr->get_internal_fn (ifn);
}

/* Look for attribute "alloc_size" on the called function and, if found,
   return a symbolic value of type size_type_node for the allocation size
   based on the call's parameters.
   Otherwise, return null.  */

static const svalue *
get_result_size_in_bytes (const call_details &cd)
{
  const tree attr = cd.lookup_function_attribute ("alloc_size");
  if (!attr)
    return nullptr;

  const tree atval_1 = TREE_VALUE (attr);
  if (!atval_1)
    return nullptr;

  unsigned argidx1 = TREE_INT_CST_LOW (TREE_VALUE (atval_1)) - 1;
  if (cd.num_args () <= argidx1)
    return nullptr;

  const svalue *sval_arg1 = cd.get_arg_svalue (argidx1);

  if (const tree atval_2 = TREE_CHAIN (atval_1))
    {
      /* Two arguments.  */
      unsigned argidx2 = TREE_INT_CST_LOW (TREE_VALUE (atval_2)) - 1;
      if (cd.num_args () <= argidx2)
	return nullptr;
      const svalue *sval_arg2 = cd.get_arg_svalue (argidx2);
      /* TODO: ideally we shouldn't need this cast here;
	 see PR analyzer/110902.  */
      return cd.get_manager ()->get_or_create_cast
	(size_type_node,
	 cd.get_manager ()->get_or_create_binop (size_type_node,
						 MULT_EXPR,
						 sval_arg1, sval_arg2));
    }
  else
    /* Single argument.  */
    return cd.get_manager ()->get_or_create_cast (size_type_node, sval_arg1);
}

/* Update this model for the CALL stmt, using CTXT to report any
   diagnostics - the first half.

   Updates to the region_model that should be made *before* sm-states
   are updated are done here; other updates to the region_model are done
   in region_model::on_call_post.

   Return true if the function call has unknown side effects (it wasn't
   recognized and we don't have a body for it, or are unable to tell which
   fndecl it is).  */

bool
region_model::on_call_pre (const gcall *call, region_model_context *ctxt)
{
  call_details cd (call, this, ctxt);

  /* Special-case for IFN_DEFERRED_INIT.
     We want to report uninitialized variables with -fanalyzer (treating
     -ftrivial-auto-var-init= as purely a mitigation feature).
     Handle IFN_DEFERRED_INIT by treating it as no-op: don't touch the
     lhs of the call, so that it is still uninitialized from the point of
     view of the analyzer.  */
  if (gimple_call_internal_p (call)
      && gimple_call_internal_fn (call) == IFN_DEFERRED_INIT)
    return false; /* No side effects.  */

  /* Get svalues for all of the arguments at the callsite, to ensure that we
     complain about any uninitialized arguments.  This might lead to
     duplicates if any of the handling below also looks up the svalues,
     but the deduplication code should deal with that.  */
  if (ctxt)
    check_call_args (cd);

  tree callee_fndecl = get_fndecl_for_call (call, ctxt);

  /* Some of the cases below update the lhs of the call based on the
     return value, but not all.  Provide a default value, which may
     get overwritten below.  */
  if (tree lhs = gimple_call_lhs (call))
    {
      const region *lhs_region = get_lvalue (lhs, ctxt);
      const svalue *sval = maybe_get_const_fn_result (cd);
      if (!sval)
	{
	  if (callee_fndecl
	      && lookup_attribute ("malloc", DECL_ATTRIBUTES (callee_fndecl)))
	    {
	      const region *new_reg
		= get_or_create_region_for_heap_alloc (NULL, ctxt);
	      mark_region_as_unknown (new_reg, NULL);
	      sval = m_mgr->get_ptr_svalue (cd.get_lhs_type (), new_reg);
	    }
	  else
	    /* For the common case of functions without __attribute__((const)),
	       use a conjured value, and purge any prior state involving that
	       value (in case this is in a loop).  */
	    sval = m_mgr->get_or_create_conjured_svalue (TREE_TYPE (lhs), call,
							 lhs_region,
							 conjured_purge (this,
									 ctxt));
	  if (const svalue *size_in_bytes = get_result_size_in_bytes (cd))
	    {
	      const region *reg = deref_rvalue (sval, NULL_TREE, ctxt, false);
	      set_dynamic_extents (reg, size_in_bytes, ctxt);
	    }
	}
      set_value (lhs_region, sval, ctxt);
    }

  if (gimple_call_internal_p (call))
    if (const known_function *kf
	  = get_known_function (gimple_call_internal_fn (call)))
      {
	kf->impl_call_pre (cd);
	return false; /* No further side effects.  */
      }

  if (!callee_fndecl)
    return true; /* Unknown side effects.  */

  if (const known_function *kf = get_known_function (callee_fndecl, cd))
    {
      kf->impl_call_pre (cd);
      return false; /* No further side effects.  */
    }

  const int callee_fndecl_flags = flags_from_decl_or_type (callee_fndecl);
  if (callee_fndecl_flags & (ECF_CONST | ECF_PURE))
    return false; /* No side effects.  */

  if (fndecl_built_in_p (callee_fndecl))
    return true; /* Unknown side effects.  */

  if (!fndecl_has_gimple_body_p (callee_fndecl))
    return true; /* Unknown side effects.  */

  return false; /* No side effects.  */
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
      call_details cd (call, this, ctxt);
      if (const known_function *kf = get_known_function (callee_fndecl, cd))
	{
	  kf->impl_call_post (cd);
	  return;
	}
      /* Was this fndecl referenced by
	 __attribute__((malloc(FOO)))?  */
      if (lookup_attribute ("*dealloc", DECL_ATTRIBUTES (callee_fndecl)))
	{
	  impl_deallocation_call (cd);
	  return;
	}
    }

  if (unknown_side_effects)
    handle_unrecognized_call (call, ctxt);
}

/* Purge state involving SVAL from this region_model, using CTXT
   (if non-NULL) to purge other state in a program_state.

   For example, if we're at the def-stmt of an SSA name, then we need to
   purge any state for svalues that involve that SSA name.  This avoids
   false positives in loops, since a symbolic value referring to the
   SSA name will be referring to the previous value of that SSA name.

   For example, in:
     while ((e = hashmap_iter_next(&iter))) {
       struct oid2strbuf *e_strbuf = (struct oid2strbuf *)e;
       free (e_strbuf->value);
     }
   at the def-stmt of e_8:
     e_8 = hashmap_iter_next (&iter);
   we should purge the "freed" state of:
     INIT_VAL(CAST_REG(‘struct oid2strbuf’, (*INIT_VAL(e_8))).value)
   which is the "e_strbuf->value" value from the previous iteration,
   or we will erroneously report a double-free - the "e_8" within it
   refers to the previous value.  */

void
region_model::purge_state_involving (const svalue *sval,
				     region_model_context *ctxt)
{
  if (!sval->can_have_associated_state_p ())
    return;
  m_store.purge_state_involving (sval, m_mgr);
  m_constraints->purge_state_involving (sval);
  m_dynamic_extents.purge_state_involving (sval);
  if (ctxt)
    ctxt->purge_state_involving (sval);
}

/* A pending_note subclass for adding a note about an
   __attribute__((access, ...)) to a diagnostic.  */

class reason_attr_access : public pending_note_subclass<reason_attr_access>
{
public:
  reason_attr_access (tree callee_fndecl, const attr_access &access)
  : m_callee_fndecl (callee_fndecl),
    m_ptr_argno (access.ptrarg),
    m_access_str (TREE_STRING_POINTER (access.to_external_string ()))
  {
  }

  const char *get_kind () const final override { return "reason_attr_access"; }

  void emit () const final override
  {
    inform (DECL_SOURCE_LOCATION (m_callee_fndecl),
	    "parameter %i of %qD marked with attribute %qs",
	    m_ptr_argno + 1, m_callee_fndecl, m_access_str);
  }

  bool operator== (const reason_attr_access &other) const
  {
    return (m_callee_fndecl == other.m_callee_fndecl
	    && m_ptr_argno == other.m_ptr_argno
	    && !strcmp (m_access_str, other.m_access_str));
  }

private:
  tree m_callee_fndecl;
  unsigned m_ptr_argno;
  const char *m_access_str;
};

/* Check CALL a call to external function CALLEE_FNDECL based on
   any __attribute__ ((access, ....) on the latter, complaining to
   CTXT about any issues.

   Currently we merely call check_region_for_write on any regions
   pointed to by arguments marked with a "write_only" or "read_write"
   attribute.  */

void
region_model::
check_external_function_for_access_attr (const gcall *call,
					 tree callee_fndecl,
					 region_model_context *ctxt) const
{
  gcc_assert (call);
  gcc_assert (callee_fndecl);
  gcc_assert (ctxt);

  tree fntype = TREE_TYPE (callee_fndecl);
  if (!fntype)
    return;

  if (!TYPE_ATTRIBUTES (fntype))
    return;

  /* Initialize a map of attribute access specifications for arguments
     to the function call.  */
  rdwr_map rdwr_idx;
  init_attr_rdwr_indices (&rdwr_idx, TYPE_ATTRIBUTES (fntype));

  unsigned argno = 0;

  for (tree iter = TYPE_ARG_TYPES (fntype); iter;
       iter = TREE_CHAIN (iter), ++argno)
    {
      const attr_access* access = rdwr_idx.get (argno);
      if (!access)
	continue;

      /* Ignore any duplicate entry in the map for the size argument.  */
      if (access->ptrarg != argno)
	continue;

      if (access->mode == access_write_only
	  || access->mode == access_read_write)
	{
	  /* Subclass of decorated_region_model_context that
	     adds a note about the attr access to any saved diagnostics.  */
	  class annotating_ctxt : public note_adding_context
	  {
	  public:
	    annotating_ctxt (tree callee_fndecl,
			     const attr_access &access,
			     region_model_context *ctxt)
	    : note_adding_context (ctxt),
	      m_callee_fndecl (callee_fndecl),
	      m_access (access)
	    {
	    }
	    std::unique_ptr<pending_note> make_note () final override
	    {
	      return make_unique<reason_attr_access>
		(m_callee_fndecl, m_access);
	    }
	  private:
	    tree m_callee_fndecl;
	    const attr_access &m_access;
	  };

	  /* Use this ctxt below so that any diagnostics get the
	     note added to them.  */
	  annotating_ctxt my_ctxt (callee_fndecl, *access, ctxt);

	  tree ptr_tree = gimple_call_arg (call, access->ptrarg);
	  const svalue *ptr_sval = get_rvalue (ptr_tree, &my_ctxt);
	  const region *reg = deref_rvalue (ptr_sval, ptr_tree, &my_ctxt);
	  check_region_for_write (reg, nullptr, &my_ctxt);
	  /* We don't use the size arg for now.  */
	}
    }
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

  if (fndecl && ctxt)
    check_external_function_for_access_attr (call, fndecl, ctxt);

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

  uncertainty_t *uncertainty = ctxt ? ctxt->get_uncertainty () : NULL;

  /* Purge sm-state for the svalues that were reachable,
     both in non-mutable and mutable form.  */
  for (svalue_set::iterator iter
	 = reachable_regs.begin_reachable_svals ();
       iter != reachable_regs.end_reachable_svals (); ++iter)
    {
      const svalue *sval = (*iter);
      if (ctxt)
	ctxt->on_unknown_change (sval, false);
    }
  for (svalue_set::iterator iter
	 = reachable_regs.begin_mutable_svals ();
       iter != reachable_regs.end_mutable_svals (); ++iter)
    {
      const svalue *sval = (*iter);
      if (ctxt)
	ctxt->on_unknown_change (sval, true);
      if (uncertainty)
	uncertainty->on_mutable_sval_at_unknown_call (sval);
    }

  /* Mark any clusters that have escaped.  */
  reachable_regs.mark_escaped_clusters (ctxt);

  /* Update bindings for all clusters that have escaped, whether above,
     or previously.  */
  m_store.on_unknown_fncall (call, m_mgr->get_store_manager (),
			     conjured_purge (this, ctxt));

  /* Purge dynamic extents from any regions that have escaped mutably:
     realloc could have been called on them.  */
  for (hash_set<const region *>::iterator
	 iter = reachable_regs.begin_mutable_base_regs ();
       iter != reachable_regs.end_mutable_base_regs ();
       ++iter)
    {
      const region *base_reg = (*iter);
      unset_dynamic_extents (base_reg);
    }
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
    {
      const svalue *sval = get_rvalue (rhs, ctxt);
      const region *ret_reg = get_lvalue (lhs, ctxt);
      set_value (ret_reg, sval, ctxt);
    }
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
      const svalue *new_sval
	= m_mgr->get_or_create_int_cst (TREE_TYPE (lhs), 0);
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
    pop_frame (NULL, NULL, ctxt, false);

  gcc_assert (get_stack_depth () == setjmp_stack_depth);

  /* Assign to LHS of "setjmp" in new_state.  */
  if (tree lhs = gimple_call_lhs (setjmp_call))
    {
      /* Passing 0 as the val to longjmp leads to setjmp returning 1.  */
      const svalue *zero_sval
	= m_mgr->get_or_create_int_cst (TREE_TYPE (fake_retval), 0);
      tristate eq_zero = eval_condition (fake_retval_sval, EQ_EXPR, zero_sval);
      /* If we have 0, use 1.  */
      if (eq_zero.is_true ())
	{
	  const svalue *one_sval
	    = m_mgr->get_or_create_int_cst (TREE_TYPE (fake_retval), 1);
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
   where RHS is for the appropriate edge.
   Get state from OLD_STATE so that all of the phi stmts for a basic block
   are effectively handled simultaneously.  */

void
region_model::handle_phi (const gphi *phi,
			  tree lhs, tree rhs,
			  const region_model &old_state,
			  region_model_context *ctxt)
{
  /* For now, don't bother tracking the .MEM SSA names.  */
  if (tree var = SSA_NAME_VAR (lhs))
    if (TREE_CODE (var) == VAR_DECL)
      if (VAR_DECL_IS_VIRTUAL_OPERAND (var))
	return;

  const svalue *src_sval = old_state.get_rvalue (rhs, ctxt);
  const region *dst_reg = old_state.get_lvalue (lhs, ctxt);

  set_value (dst_reg, src_sval, ctxt);

  if (ctxt)
    ctxt->on_phi (phi, rhs);
}

/* Implementation of region_model::get_lvalue; the latter adds type-checking.

   Get the id of the region for PV within this region_model,
   emitting any diagnostics to CTXT.  */

const region *
region_model::get_lvalue_1 (path_var pv, region_model_context *ctxt) const
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

    case BIT_FIELD_REF:
      {
	tree inner_expr = TREE_OPERAND (expr, 0);
	const region *inner_reg = get_lvalue (inner_expr, ctxt);
	tree num_bits = TREE_OPERAND (expr, 1);
	tree first_bit_offset = TREE_OPERAND (expr, 2);
	gcc_assert (TREE_CODE (num_bits) == INTEGER_CST);
	gcc_assert (TREE_CODE (first_bit_offset) == INTEGER_CST);
	bit_range bits (TREE_INT_CST_LOW (first_bit_offset),
			TREE_INT_CST_LOW (num_bits));
	return m_mgr->get_bit_range (inner_reg, TREE_TYPE (expr), bits);
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
		    || VAR_P (expr)
		    || TREE_CODE (expr) == RESULT_DECL);

	int stack_index = pv.m_stack_depth;
	const frame_region *frame = get_frame_at_index (stack_index);
	gcc_assert (frame);
	return frame->get_region_for_local (m_mgr, expr, ctxt);
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

/* Return true if SRC_TYPE can be converted to DST_TYPE as a no-op.  */

bool
compat_types_p (tree src_type, tree dst_type)
{
  if (src_type && dst_type && !VOID_TYPE_P (dst_type))
    if (!(useless_type_conversion_p (src_type, dst_type)))
      return false;
  return true;
}

/* Get the region for PV within this region_model,
   emitting any diagnostics to CTXT.  */

const region *
region_model::get_lvalue (path_var pv, region_model_context *ctxt) const
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
region_model::get_lvalue (tree expr, region_model_context *ctxt) const
{
  return get_lvalue (path_var (expr, get_stack_depth () - 1), ctxt);
}

/* Implementation of region_model::get_rvalue; the latter adds type-checking.

   Get the value of PV within this region_model,
   emitting any diagnostics to CTXT.  */

const svalue *
region_model::get_rvalue_1 (path_var pv, region_model_context *ctxt) const
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
      {
	tree expr = pv.m_tree;
	tree op0 = TREE_OPERAND (expr, 0);
	const region *reg = get_lvalue (op0, ctxt);
	tree num_bits = TREE_OPERAND (expr, 1);
	tree first_bit_offset = TREE_OPERAND (expr, 2);
	gcc_assert (TREE_CODE (num_bits) == INTEGER_CST);
	gcc_assert (TREE_CODE (first_bit_offset) == INTEGER_CST);
	bit_range bits (TREE_INT_CST_LOW (first_bit_offset),
			TREE_INT_CST_LOW (num_bits));
	return get_rvalue_for_bits (TREE_TYPE (expr), reg, bits, ctxt);
      }

    case VAR_DECL:
      if (DECL_HARD_REGISTER (pv.m_tree))
	{
	  /* If it has a hard register, it doesn't have a memory region
	     and can't be referred to as an lvalue.  */
	  return m_mgr->get_or_create_unknown_svalue (TREE_TYPE (pv.m_tree));
	}
      /* Fall through. */
    case PARM_DECL:
    case SSA_NAME:
    case RESULT_DECL:
    case ARRAY_REF:
      {
	const region *reg = get_lvalue (pv, ctxt);
	return get_store_value (reg, ctxt);
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
    case BIT_AND_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
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
	return get_store_value (ref_reg, ctxt);
      }
    case OBJ_TYPE_REF:
      {
        tree expr = OBJ_TYPE_REF_EXPR (pv.m_tree);
        return get_rvalue (expr, ctxt);
      }
    }
}

/* Get the value of PV within this region_model,
   emitting any diagnostics to CTXT.  */

const svalue *
region_model::get_rvalue (path_var pv, region_model_context *ctxt) const
{
  if (pv.m_tree == NULL_TREE)
    return NULL;

  const svalue *result_sval = get_rvalue_1 (pv, ctxt);

  assert_compat_types (result_sval->get_type (), TREE_TYPE (pv.m_tree));

  result_sval = check_for_poison (result_sval, pv.m_tree, NULL, ctxt);

  return result_sval;
}

/* Get the value of EXPR within this region_model (assuming the most
   recent stack frame if it's a local).  */

const svalue *
region_model::get_rvalue (tree expr, region_model_context *ctxt) const
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
    return reg->get_initial_value_at_main (m_mgr);

  /* Otherwise, return INIT_VAL(REG).  */
  return m_mgr->get_or_create_initial_value (reg);
}

/* Get a value for REG, looking it up in the store, or otherwise falling
   back to "initial" or "unknown" values.
   Use CTXT to report any warnings associated with reading from REG. */

const svalue *
region_model::get_store_value (const region *reg,
			       region_model_context *ctxt) const
{
  /* Getting the value of an empty region gives an unknown_svalue.  */
  if (reg->empty_p ())
    return m_mgr->get_or_create_unknown_svalue (reg->get_type ());

  bool check_poisoned = true;
  if (check_region_for_read (reg, ctxt))
    check_poisoned = false;

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

  return m_mgr->get_or_create_initial_value (reg, check_poisoned);
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
			    region_model_context *ctxt,
			    bool add_nonnull_constraint) const
{
  gcc_assert (ptr_sval);
  gcc_assert (POINTER_TYPE_P (ptr_sval->get_type ()));

  /* If we're dereferencing PTR_SVAL, assume that it is non-NULL; add this
     as a constraint.  This suppresses false positives from
     -Wanalyzer-null-dereference for the case where we later have an
     if (PTR_SVAL) that would occur if we considered the false branch
     and transitioned the malloc state machine from start->null.  */
  if (add_nonnull_constraint)
    {
      tree null_ptr_cst = build_int_cst (ptr_sval->get_type (), 0);
      const svalue *null_ptr
	= m_mgr->get_or_create_constant_svalue (null_ptr_cst);
      m_constraints->add_constraint (ptr_sval, NE_EXPR, null_ptr);
    }

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
		ctxt->warn (::make_unique<poisoned_value_diagnostic>
			      (ptr, pkind, nullptr, nullptr));
	      }
	  }
      }
      break;
    }

  return m_mgr->get_symbolic_region (ptr_sval);
}

/* Attempt to get BITS within any value of REG, as TYPE.
   In particular, extract values from compound_svalues for the case
   where there's a concrete binding at BITS.
   Return an unknown svalue if we can't handle the given case.
   Use CTXT to report any warnings associated with reading from REG.  */

const svalue *
region_model::get_rvalue_for_bits (tree type,
				   const region *reg,
				   const bit_range &bits,
				   region_model_context *ctxt) const
{
  const svalue *sval = get_store_value (reg, ctxt);
  return m_mgr->get_or_create_bits_within (type, bits, sval);
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

  const char *get_kind () const final override
  {
    return "write_to_const_diagnostic";
  }

  bool operator== (const write_to_const_diagnostic &other) const
  {
    return (m_reg == other.m_reg
	    && m_decl == other.m_decl);
  }

  int get_controlling_option () const final override
  {
    return OPT_Wanalyzer_write_to_const;
  }

  bool emit (rich_location *rich_loc, logger *) final override
  {
    auto_diagnostic_group d;
    bool warned;
    switch (m_reg->get_kind ())
      {
      default:
	warned = warning_at (rich_loc, get_controlling_option (),
			     "write to %<const%> object %qE", m_decl);
	break;
      case RK_FUNCTION:
	warned = warning_at (rich_loc, get_controlling_option (),
			     "write to function %qE", m_decl);
	break;
      case RK_LABEL:
	warned = warning_at (rich_loc, get_controlling_option (),
			     "write to label %qE", m_decl);
	break;
      }
    if (warned)
      inform (DECL_SOURCE_LOCATION (m_decl), "declared here");
    return warned;
  }

  label_text describe_final_event (const evdesc::final_event &ev) final override
  {
    switch (m_reg->get_kind ())
      {
      default:
	return ev.formatted_print ("write to %<const%> object %qE here", m_decl);
      case RK_FUNCTION:
	return ev.formatted_print ("write to function %qE here", m_decl);
      case RK_LABEL:
	return ev.formatted_print ("write to label %qE here", m_decl);
      }
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

  const char *get_kind () const final override
  {
    return "write_to_string_literal_diagnostic";
  }

  bool operator== (const write_to_string_literal_diagnostic &other) const
  {
    return m_reg == other.m_reg;
  }

  int get_controlling_option () const final override
  {
    return OPT_Wanalyzer_write_to_string_literal;
  }

  bool emit (rich_location *rich_loc, logger *) final override
  {
    return warning_at (rich_loc, get_controlling_option (),
		       "write to string literal");
    /* Ideally we would show the location of the STRING_CST as well,
       but it is not available at this point.  */
  }

  label_text describe_final_event (const evdesc::final_event &ev) final override
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
    case RK_FUNCTION:
      {
	const function_region *func_reg = as_a <const function_region *> (base_reg);
	tree fndecl = func_reg->get_fndecl ();
	ctxt->warn (make_unique<write_to_const_diagnostic>
		      (func_reg, fndecl));
      }
      break;
    case RK_LABEL:
      {
	const label_region *label_reg = as_a <const label_region *> (base_reg);
	tree label = label_reg->get_label ();
	ctxt->warn (make_unique<write_to_const_diagnostic>
		      (label_reg, label));
      }
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
	  ctxt->warn (make_unique<write_to_const_diagnostic> (dest_reg, decl));
      }
      break;
    case RK_STRING:
      ctxt->warn (make_unique<write_to_string_literal_diagnostic> (dest_reg));
      break;
    }
}

/* Get the capacity of REG in bytes.  */

const svalue *
region_model::get_capacity (const region *reg) const
{
  switch (reg->get_kind ())
    {
    default:
      break;
    case RK_DECL:
      {
	const decl_region *decl_reg = as_a <const decl_region *> (reg);
	tree decl = decl_reg->get_decl ();
	if (TREE_CODE (decl) == SSA_NAME)
	  {
	    tree type = TREE_TYPE (decl);
	    tree size = TYPE_SIZE (type);
	    return get_rvalue (size, NULL);
	  }
	else
	  {
	    tree size = decl_init_size (decl, false);
	    if (size)
	      return get_rvalue (size, NULL);
	  }
      }
      break;
    case RK_SIZED:
      /* Look through sized regions to get at the capacity
	 of the underlying regions.  */
      return get_capacity (reg->get_parent_region ());
    case RK_STRING:
      {
	/* "Capacity" here means "size".  */
	const string_region *string_reg = as_a <const string_region *> (reg);
	tree string_cst = string_reg->get_string_cst ();
	return m_mgr->get_or_create_int_cst (size_type_node,
					     TREE_STRING_LENGTH (string_cst));
      }
      break;
    }

  if (const svalue *recorded = get_dynamic_extents (reg))
    return recorded;

  return m_mgr->get_or_create_unknown_svalue (sizetype);
}

/* Return the string size, including the 0-terminator, if SVAL is a
   constant_svalue holding a string.  Otherwise, return an unknown_svalue.  */

const svalue *
region_model::get_string_size (const svalue *sval) const
{
  tree cst = sval->maybe_get_constant ();
  if (!cst || TREE_CODE (cst) != STRING_CST)
    return m_mgr->get_or_create_unknown_svalue (size_type_node);

  tree out = build_int_cst (size_type_node, TREE_STRING_LENGTH (cst));
  return m_mgr->get_or_create_constant_svalue (out);
}

/* Return the string size, including the 0-terminator, if REG is a
   string_region.  Otherwise, return an unknown_svalue.  */

const svalue *
region_model::get_string_size (const region *reg) const
{
  const string_region *str_reg = dyn_cast <const string_region *> (reg);
  if (!str_reg)
    return m_mgr->get_or_create_unknown_svalue (size_type_node);

  tree cst = str_reg->get_string_cst ();
  tree out = build_int_cst (size_type_node, TREE_STRING_LENGTH (cst));
  return m_mgr->get_or_create_constant_svalue (out);
}

/* If CTXT is non-NULL, use it to warn about any problems accessing REG,
   using DIR to determine if this access is a read or write.
   Return TRUE if an OOB access was detected.
   If SVAL_HINT is non-NULL, use it as a hint in diagnostics
   about the value that would be written to REG.  */

bool
region_model::check_region_access (const region *reg,
				   enum access_direction dir,
				   const svalue *sval_hint,
				   region_model_context *ctxt) const
{
  /* Fail gracefully if CTXT is NULL.  */
  if (!ctxt)
    return false;

  bool oob_access_detected = false;
  check_region_for_taint (reg, dir, ctxt);
  if (!check_region_bounds (reg, dir, sval_hint, ctxt))
    oob_access_detected = true;

  switch (dir)
    {
    default:
      gcc_unreachable ();
    case DIR_READ:
      /* Currently a no-op.  */
      break;
    case DIR_WRITE:
      check_for_writable_region (reg, ctxt);
      break;
    }
  return oob_access_detected;
}

/* If CTXT is non-NULL, use it to warn about any problems writing to REG.  */

void
region_model::check_region_for_write (const region *dest_reg,
				      const svalue *sval_hint,
				      region_model_context *ctxt) const
{
  check_region_access (dest_reg, DIR_WRITE, sval_hint, ctxt);
}

/* If CTXT is non-NULL, use it to warn about any problems reading from REG.
  Returns TRUE if an OOB read was detected.  */

bool
region_model::check_region_for_read (const region *src_reg,
				     region_model_context *ctxt) const
{
  return check_region_access (src_reg, DIR_READ, NULL, ctxt);
}

/* Concrete subclass for casts of pointers that lead to trailing bytes.  */

class dubious_allocation_size
: public pending_diagnostic_subclass<dubious_allocation_size>
{
public:
  dubious_allocation_size (const region *lhs, const region *rhs,
			   const gimple *stmt)
  : m_lhs (lhs), m_rhs (rhs), m_expr (NULL_TREE), m_stmt (stmt),
    m_has_allocation_event (false)
  {}

  dubious_allocation_size (const region *lhs, const region *rhs,
			   tree expr, const gimple *stmt)
  : m_lhs (lhs), m_rhs (rhs), m_expr (expr), m_stmt (stmt),
    m_has_allocation_event (false)
  {}

  const char *get_kind () const final override
  {
    return "dubious_allocation_size";
  }

  bool operator== (const dubious_allocation_size &other) const
  {
    return (m_stmt == other.m_stmt
	    && pending_diagnostic::same_tree_p (m_expr, other.m_expr));
  }

  int get_controlling_option () const final override
  {
    return OPT_Wanalyzer_allocation_size;
  }

  bool emit (rich_location *rich_loc, logger *) final override
  {
    diagnostic_metadata m;
    m.add_cwe (131);

    return warning_meta (rich_loc, m, get_controlling_option (),
			 "allocated buffer size is not a multiple"
			 " of the pointee's size");
  }

  label_text describe_final_event (const evdesc::final_event &ev) final
  override
  {
    tree pointee_type = TREE_TYPE (m_lhs->get_type ());
    if (m_has_allocation_event)
      return ev.formatted_print ("assigned to %qT here;"
				 " %<sizeof (%T)%> is %qE",
				 m_lhs->get_type (), pointee_type,
				 size_in_bytes (pointee_type));
    /* Fallback: Typically, we should always see an allocation_event
       before.  */
    if (m_expr)
      {
	if (TREE_CODE (m_expr) == INTEGER_CST)
	  return ev.formatted_print ("allocated %E bytes and assigned to"
				    " %qT here; %<sizeof (%T)%> is %qE",
				    m_expr, m_lhs->get_type (), pointee_type,
				    size_in_bytes (pointee_type));
	else
	  return ev.formatted_print ("allocated %qE bytes and assigned to"
				    " %qT here; %<sizeof (%T)%> is %qE",
				    m_expr, m_lhs->get_type (), pointee_type,
				    size_in_bytes (pointee_type));
      }

    return ev.formatted_print ("allocated and assigned to %qT here;"
			       " %<sizeof (%T)%> is %qE",
			       m_lhs->get_type (), pointee_type,
			       size_in_bytes (pointee_type));
  }

  void
  add_region_creation_events (const region *,
			      tree capacity,
			      const event_loc_info &loc_info,
			      checker_path &emission_path) final override
  {
    emission_path.add_event
      (make_unique<region_creation_event_allocation_size> (capacity, loc_info));

    m_has_allocation_event = true;
  }

  void mark_interesting_stuff (interesting_t *interest) final override
  {
    interest->add_region_creation (m_rhs);
  }

private:
  const region *m_lhs;
  const region *m_rhs;
  const tree m_expr;
  const gimple *m_stmt;
  bool m_has_allocation_event;
};

/* Return true on dubious allocation sizes for constant sizes.  */

static bool
capacity_compatible_with_type (tree cst, tree pointee_size_tree,
			       bool is_struct)
{
  gcc_assert (TREE_CODE (cst) == INTEGER_CST);
  gcc_assert (TREE_CODE (pointee_size_tree) == INTEGER_CST);

  unsigned HOST_WIDE_INT pointee_size = TREE_INT_CST_LOW (pointee_size_tree);
  unsigned HOST_WIDE_INT alloc_size = TREE_INT_CST_LOW (cst);

  if (is_struct)
    return alloc_size == 0 || alloc_size >= pointee_size;
  return alloc_size % pointee_size == 0;
}

static bool
capacity_compatible_with_type (tree cst, tree pointee_size_tree)
{
  return capacity_compatible_with_type (cst, pointee_size_tree, false);
}

/* Checks whether SVAL could be a multiple of SIZE_CST.

   It works by visiting all svalues inside SVAL until it reaches
   atomic nodes.  From those, it goes back up again and adds each
   node that is not a multiple of SIZE_CST to the RESULT_SET.  */

class size_visitor : public visitor
{
public:
  size_visitor (tree size_cst, const svalue *root_sval, constraint_manager *cm)
  : m_size_cst (size_cst), m_root_sval (root_sval), m_cm (cm)
  {
    m_root_sval->accept (this);
  }

  bool is_dubious_capacity ()
  {
    return result_set.contains (m_root_sval);
  }

  void visit_constant_svalue (const constant_svalue *sval) final override
  {
    check_constant (sval->get_constant (), sval);
  }

  void visit_unaryop_svalue (const unaryop_svalue *sval) final override
  {
    if (CONVERT_EXPR_CODE_P (sval->get_op ())
	  && result_set.contains (sval->get_arg ()))
      result_set.add (sval);
  }

  void visit_binop_svalue (const binop_svalue *sval) final override
  {
    const svalue *arg0 = sval->get_arg0 ();
    const svalue *arg1 = sval->get_arg1 ();

    switch (sval->get_op ())
      {
	case MULT_EXPR:
	  if (result_set.contains (arg0) && result_set.contains (arg1))
	    result_set.add (sval);
	  break;
	case PLUS_EXPR:
	case MINUS_EXPR:
	  if (result_set.contains (arg0) || result_set.contains (arg1))
	    result_set.add (sval);
	  break;
	default:
	  break;
      }
  }

  void visit_unmergeable_svalue (const unmergeable_svalue *sval) final override
  {
    if (result_set.contains (sval->get_arg ()))
      result_set.add (sval);
  }

  void visit_widening_svalue (const widening_svalue *sval) final override
  {
    const svalue *base = sval->get_base_svalue ();
    const svalue *iter = sval->get_iter_svalue ();

    if (result_set.contains (base) || result_set.contains (iter))
      result_set.add (sval);
  }

  void visit_initial_svalue (const initial_svalue *sval) final override
  {
    equiv_class_id id = equiv_class_id::null ();
    if (m_cm->get_equiv_class_by_svalue (sval, &id))
      {
	if (tree cst = id.get_obj (*m_cm).get_any_constant ())
	  check_constant (cst, sval);
      }
    else if (!m_cm->sval_constrained_p (sval))
      {
	result_set.add (sval);
      }
  }

  void visit_conjured_svalue (const conjured_svalue *sval) final override
  {
    equiv_class_id id = equiv_class_id::null ();
    if (m_cm->get_equiv_class_by_svalue (sval, &id))
      if (tree cst = id.get_obj (*m_cm).get_any_constant ())
	check_constant (cst, sval);
  }

private:
  void check_constant (tree cst, const svalue *sval)
  {
    switch (TREE_CODE (cst))
      {
      default:
	/* Assume all unhandled operands are compatible.  */
	break;
      case INTEGER_CST:
	if (!capacity_compatible_with_type (cst, m_size_cst))
	  result_set.add (sval);
	break;
      }
  }

  tree m_size_cst;
  const svalue *m_root_sval;
  constraint_manager *m_cm;
  svalue_set result_set; /* Used as a mapping of svalue*->bool.  */
};

/* Return true if a struct or union either uses the inheritance pattern,
   where the first field is a base struct, or the flexible array member
   pattern, where the last field is an array without a specified size.  */

static bool
struct_or_union_with_inheritance_p (tree struc)
{
  tree iter = TYPE_FIELDS (struc);
  if (iter == NULL_TREE)
	  return false;
  if (RECORD_OR_UNION_TYPE_P (TREE_TYPE (iter)))
	  return true;

  tree last_field;
  while (iter != NULL_TREE)
    {
      last_field = iter;
      iter = DECL_CHAIN (iter);
    }

  if (last_field != NULL_TREE
      && TREE_CODE (TREE_TYPE (last_field)) == ARRAY_TYPE)
	  return true;

  return false;
}

/* Return true if the lhs and rhs of an assignment have different types.  */

static bool
is_any_cast_p (const gimple *stmt)
{
  if (const gassign *assign = dyn_cast <const gassign *> (stmt))
    return gimple_assign_cast_p (assign)
	   || !pending_diagnostic::same_tree_p (
		  TREE_TYPE (gimple_assign_lhs (assign)),
		  TREE_TYPE (gimple_assign_rhs1 (assign)));
  else if (const gcall *call = dyn_cast <const gcall *> (stmt))
    {
      tree lhs = gimple_call_lhs (call);
      return lhs != NULL_TREE && !pending_diagnostic::same_tree_p (
				    TREE_TYPE (gimple_call_lhs (call)),
				    gimple_call_return_type (call));
    }

  return false;
}

/* On pointer assignments, check whether the buffer size of
   RHS_SVAL is compatible with the type of the LHS_REG.
   Use a non-null CTXT to report allocation size warnings.  */

void
region_model::check_region_size (const region *lhs_reg, const svalue *rhs_sval,
				 region_model_context *ctxt) const
{
  if (!ctxt || ctxt->get_stmt () == NULL)
    return;
  /* Only report warnings on assignments that actually change the type.  */
  if (!is_any_cast_p (ctxt->get_stmt ()))
    return;

  tree pointer_type = lhs_reg->get_type ();
  if (pointer_type == NULL_TREE || !POINTER_TYPE_P (pointer_type))
    return;

  tree pointee_type = TREE_TYPE (pointer_type);
  /* Make sure that the type on the left-hand size actually has a size.  */
  if (pointee_type == NULL_TREE || VOID_TYPE_P (pointee_type)
      || TYPE_SIZE_UNIT (pointee_type) == NULL_TREE)
    return;

  /* Bail out early on pointers to structs where we can
     not deduce whether the buffer size is compatible.  */
  bool is_struct = RECORD_OR_UNION_TYPE_P (pointee_type);
  if (is_struct && struct_or_union_with_inheritance_p (pointee_type))
    return;

  tree pointee_size_tree = size_in_bytes (pointee_type);
  /* We give up if the type size is not known at compile-time or the
     type size is always compatible regardless of the buffer size.  */
  if (TREE_CODE (pointee_size_tree) != INTEGER_CST
      || integer_zerop (pointee_size_tree)
      || integer_onep (pointee_size_tree))
    return;

  const region *rhs_reg = deref_rvalue (rhs_sval, NULL_TREE, ctxt, false);
  const svalue *capacity = get_capacity (rhs_reg);
  switch (capacity->get_kind ())
    {
    case svalue_kind::SK_CONSTANT:
      {
	const constant_svalue *cst_cap_sval
	  = as_a <const constant_svalue *> (capacity);
	tree cst_cap = cst_cap_sval->get_constant ();
	if (TREE_CODE (cst_cap) == INTEGER_CST
	    && !capacity_compatible_with_type (cst_cap, pointee_size_tree,
					       is_struct))
	  ctxt->warn (make_unique <dubious_allocation_size> (lhs_reg, rhs_reg,
							     cst_cap,
							     ctxt->get_stmt ()));
      }
      break;
    default:
      {
	if (!is_struct)
	  {
	    size_visitor v (pointee_size_tree, capacity, m_constraints);
	    if (v.is_dubious_capacity ())
	      {
		tree expr = get_representative_tree (capacity);
		ctxt->warn (make_unique <dubious_allocation_size> (lhs_reg,
								   rhs_reg,
								   expr,
								   ctxt->get_stmt ()));
	      }
	  }
      break;
      }
    }
}

/* Set the value of the region given by LHS_REG to the value given
   by RHS_SVAL.
   Use CTXT to report any warnings associated with writing to LHS_REG.  */

void
region_model::set_value (const region *lhs_reg, const svalue *rhs_sval,
			 region_model_context *ctxt)
{
  gcc_assert (lhs_reg);
  gcc_assert (rhs_sval);

  /* Setting the value of an empty region is a no-op.  */
  if (lhs_reg->empty_p ())
    return;

  check_region_size (lhs_reg, rhs_sval, ctxt);

  check_region_for_write (lhs_reg, rhs_sval, ctxt);

  m_store.set_value (m_mgr->get_store_manager(), lhs_reg, rhs_sval,
		     ctxt ? ctxt->get_uncertainty () : NULL);
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

/* Fill REG with SVAL.  */

void
region_model::fill_region (const region *reg, const svalue *sval)
{
  m_store.fill_region (m_mgr->get_store_manager(), reg, sval);
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
  svalue_set maybe_live_values;
  m_store.mark_region_as_unknown (m_mgr->get_store_manager(), reg,
				  uncertainty, &maybe_live_values);
  m_store.on_maybe_live_values (maybe_live_values);
}

/* Determine what is known about the condition "LHS_SVAL OP RHS_SVAL" within
   this model.  */

tristate
region_model::eval_condition (const svalue *lhs,
			       enum tree_code op,
			       const svalue *rhs) const
{
  gcc_assert (lhs);
  gcc_assert (rhs);

  /* For now, make no attempt to capture constraints on floating-point
     values.  */
  if ((lhs->get_type () && FLOAT_TYPE_P (lhs->get_type ()))
      || (rhs->get_type () && FLOAT_TYPE_P (rhs->get_type ())))
    return tristate::unknown ();

  /* See what we know based on the values.  */

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

  if (const constant_svalue *cst_lhs = lhs->dyn_cast_constant_svalue ())
    {
      /* If we have a pair of constants, compare them.  */
      if (const constant_svalue *cst_rhs = rhs->dyn_cast_constant_svalue ())
	return constant_svalue::eval_condition (cst_lhs, op, cst_rhs);
      else
	{
	  /* When we have one constant, put it on the RHS.  */
	  std::swap (lhs, rhs);
	  op = swap_tree_comparison (op);
	}
    }
  gcc_assert (lhs->get_kind () != SK_CONSTANT);

  /* Handle comparison against zero.  */
  if (const constant_svalue *cst_rhs = rhs->dyn_cast_constant_svalue ())
    if (zerop (cst_rhs->get_constant ()))
      {
	if (const region_svalue *ptr = lhs->dyn_cast_region_svalue ())
	  {
	    /* A region_svalue is a non-NULL pointer, except in certain
	       special cases (see the comment for region::non_null_p).  */
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
	else if (const binop_svalue *binop = lhs->dyn_cast_binop_svalue ())
	  {
	    /* Treat offsets from a non-NULL pointer as being non-NULL.  This
	       isn't strictly true, in that eventually ptr++ will wrap
	       around and be NULL, but it won't occur in practise and thus
	       can be used to suppress effectively false positives that we
	       shouldn't warn for.  */
	    if (binop->get_op () == POINTER_PLUS_EXPR)
	      {
		tristate lhs_ts = eval_condition (binop->get_arg0 (), op, rhs);
		if (lhs_ts.is_known ())
		  return lhs_ts;
	      }
	  }
	else if (const unaryop_svalue *unaryop
		   = lhs->dyn_cast_unaryop_svalue ())
	  {
	    if (unaryop->get_op () == NEGATE_EXPR)
	      {
		/* e.g. "-X <= 0" is equivalent to X >= 0".  */
		tristate lhs_ts = eval_condition (unaryop->get_arg (),
						  swap_tree_comparison (op),
						  rhs);
		if (lhs_ts.is_known ())
		  return lhs_ts;
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

  /* Handle comparisons between two svalues with more than one operand.  */
  if (const binop_svalue *binop = lhs->dyn_cast_binop_svalue ())
    {
      switch (op)
	{
	default:
	  break;
	case EQ_EXPR:
	  {
	    /* TODO: binops can be equal even if they are not structurally
		     equal in case of commutative operators.  */
	    tristate res = structural_equality (lhs, rhs);
	    if (res.is_true ())
	      return res;
	  }
	  break;
	case LE_EXPR:
	  {
	    tristate res = structural_equality (lhs, rhs);
	    if (res.is_true ())
	      return res;
	  }
	  break;
	case GE_EXPR:
	  {
	    tristate res = structural_equality (lhs, rhs);
	    if (res.is_true ())
	      return res;
	    res = symbolic_greater_than (binop, rhs);
	    if (res.is_true ())
	      return res;
	  }
	  break;
	case GT_EXPR:
	  {
	    tristate res = symbolic_greater_than (binop, rhs);
	    if (res.is_true ())
	      return res;
	  }
	  break;
	}
    }

  /* Otherwise, try constraints.
     Cast to const to ensure we don't change the constraint_manager as we
     do this (e.g. by creating equivalence classes).  */
  const constraint_manager *constraints = m_constraints;
  return constraints->eval_condition (lhs, op, rhs);
}

/* Subroutine of region_model::eval_condition, for rejecting
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

/* Return true if SVAL is definitely positive.  */

static bool
is_positive_svalue (const svalue *sval)
{
  if (tree cst = sval->maybe_get_constant ())
    return !zerop (cst) && get_range_pos_neg (cst) == 1;
  tree type = sval->get_type ();
  if (!type)
    return false;
  /* Consider a binary operation size_t + int.  The analyzer wraps the int in
     an unaryop_svalue, converting it to a size_t, but in the dynamic execution
     the result is smaller than the first operand.  Thus, we have to look if
     the argument of the unaryop_svalue is also positive.  */
  if (const unaryop_svalue *un_op = dyn_cast <const unaryop_svalue *> (sval))
    return CONVERT_EXPR_CODE_P (un_op->get_op ()) && TYPE_UNSIGNED (type)
	   && is_positive_svalue (un_op->get_arg ());
  return TYPE_UNSIGNED (type);
}

/* Return true if A is definitely larger than B.

   Limitation: does not account for integer overflows and does not try to
	       return false, so it can not be used negated.  */

tristate
region_model::symbolic_greater_than (const binop_svalue *bin_a,
				     const svalue *b) const
{
  if (bin_a->get_op () == PLUS_EXPR || bin_a->get_op () == MULT_EXPR)
    {
      /* Eliminate the right-hand side of both svalues.  */
      if (const binop_svalue *bin_b = dyn_cast <const binop_svalue *> (b))
	if (bin_a->get_op () == bin_b->get_op ()
	    && eval_condition (bin_a->get_arg1 (),
			       GT_EXPR,
			       bin_b->get_arg1 ()).is_true ()
	    && eval_condition (bin_a->get_arg0 (),
			       GE_EXPR,
			       bin_b->get_arg0 ()).is_true ())
	  return tristate (tristate::TS_TRUE);

      /* Otherwise, try to remove a positive offset or factor from BIN_A.  */
      if (is_positive_svalue (bin_a->get_arg1 ())
	  && eval_condition (bin_a->get_arg0 (),
			     GE_EXPR, b).is_true ())
	  return tristate (tristate::TS_TRUE);
    }
  return tristate::unknown ();
}

/* Return true if A and B are equal structurally.

   Structural equality means that A and B are equal if the svalues A and B have
   the same nodes at the same positions in the tree and the leafs are equal.
   Equality for conjured_svalues and initial_svalues is determined by comparing
   the pointers while constants are compared by value.  That behavior is useful
   to check for binaryop_svlaues that evaluate to the same concrete value but
   might use one operand with a different type but the same constant value.

   For example,
     binop_svalue (mult_expr,
       initial_svalue (‘size_t’, decl_region (..., 'some_var')),
       constant_svalue (‘size_t’, 4))
   and
     binop_svalue (mult_expr,
       initial_svalue (‘size_t’, decl_region (..., 'some_var'),
       constant_svalue (‘sizetype’, 4))
   are structurally equal.  A concrete C code example, where this occurs, can
   be found in test7 of out-of-bounds-5.c.  */

tristate
region_model::structural_equality (const svalue *a, const svalue *b) const
{
  /* If A and B are referentially equal, they are also structurally equal.  */
  if (a == b)
    return tristate (tristate::TS_TRUE);

  switch (a->get_kind ())
    {
    default:
      return tristate::unknown ();
    /* SK_CONJURED and SK_INITIAL are already handled
       by the referential equality above.  */
    case SK_CONSTANT:
      {
	tree a_cst = a->maybe_get_constant ();
	tree b_cst = b->maybe_get_constant ();
	if (a_cst && b_cst)
	  return tristate (tree_int_cst_equal (a_cst, b_cst));
      }
      return tristate (tristate::TS_FALSE);
    case SK_UNARYOP:
      {
	const unaryop_svalue *un_a = as_a <const unaryop_svalue *> (a);
	if (const unaryop_svalue *un_b = dyn_cast <const unaryop_svalue *> (b))
	  return tristate (pending_diagnostic::same_tree_p (un_a->get_type (),
							    un_b->get_type ())
			   && un_a->get_op () == un_b->get_op ()
			   && structural_equality (un_a->get_arg (),
						   un_b->get_arg ()));
      }
      return tristate (tristate::TS_FALSE);
    case SK_BINOP:
      {
	const binop_svalue *bin_a = as_a <const binop_svalue *> (a);
	if (const binop_svalue *bin_b = dyn_cast <const binop_svalue *> (b))
	  return tristate (bin_a->get_op () == bin_b->get_op ()
			   && structural_equality (bin_a->get_arg0 (),
						   bin_b->get_arg0 ())
			   && structural_equality (bin_a->get_arg1 (),
						   bin_b->get_arg1 ()));
      }
      return tristate (tristate::TS_FALSE);
    }
}

/* Handle various constraints of the form:
     LHS: ((bool)INNER_LHS INNER_OP INNER_RHS))
     OP : == or !=
     RHS: zero
   and (with a cast):
     LHS: CAST([long]int, ((bool)INNER_LHS INNER_OP INNER_RHS))
     OP : == or !=
     RHS: zero
   by adding constraints for INNER_LHS INNEROP INNER_RHS.

   Return true if this function can fully handle the constraint; if
   so, add the implied constraint(s) and write true to *OUT if they
   are consistent with existing constraints, or write false to *OUT
   if they contradicts existing constraints.

   Return false for cases that this function doeesn't know how to handle.

   For example, if we're checking a stored conditional, we'll have
   something like:
     LHS: CAST(long int, (&HEAP_ALLOCATED_REGION(8)!=(int *)0B))
     OP : NE_EXPR
     RHS: zero
   which this function can turn into an add_constraint of:
     (&HEAP_ALLOCATED_REGION(8) != (int *)0B)

   Similarly, optimized && and || conditionals lead to e.g.
     if (p && q)
   becoming gimple like this:
     _1 = p_6 == 0B;
     _2 = q_8 == 0B
     _3 = _1 | _2
   On the "_3 is false" branch we can have constraints of the form:
     ((&HEAP_ALLOCATED_REGION(8)!=(int *)0B)
      | (&HEAP_ALLOCATED_REGION(10)!=(int *)0B))
     == 0
   which implies that both _1 and _2 are false,
   which this function can turn into a pair of add_constraints of
     (&HEAP_ALLOCATED_REGION(8)!=(int *)0B)
   and:
     (&HEAP_ALLOCATED_REGION(10)!=(int *)0B).  */

bool
region_model::add_constraints_from_binop (const svalue *outer_lhs,
					  enum tree_code outer_op,
					  const svalue *outer_rhs,
					  bool *out,
					  region_model_context *ctxt)
{
  while (const svalue *cast = outer_lhs->maybe_undo_cast ())
    outer_lhs = cast;
  const binop_svalue *binop_sval = outer_lhs->dyn_cast_binop_svalue ();
  if (!binop_sval)
    return false;
  if (!outer_rhs->all_zeroes_p ())
    return false;

  const svalue *inner_lhs = binop_sval->get_arg0 ();
  enum tree_code inner_op = binop_sval->get_op ();
  const svalue *inner_rhs = binop_sval->get_arg1 ();

  if (outer_op != NE_EXPR && outer_op != EQ_EXPR)
    return false;

  /* We have either
     - "OUTER_LHS != false" (i.e. OUTER is true), or
     - "OUTER_LHS == false" (i.e. OUTER is false).  */
  bool is_true = outer_op == NE_EXPR;

  switch (inner_op)
    {
    default:
      return false;

    case EQ_EXPR:
    case NE_EXPR:
      {
	/* ...and "(inner_lhs OP inner_rhs) == 0"
	   then (inner_lhs OP inner_rhs) must have the same
	   logical value as LHS.  */
	if (!is_true)
	  inner_op = invert_tree_comparison (inner_op, false /* honor_nans */);
	*out = add_constraint (inner_lhs, inner_op, inner_rhs, ctxt);
	return true;
      }
      break;

    case BIT_AND_EXPR:
      if (is_true)
	{
	  /* ...and "(inner_lhs & inner_rhs) != 0"
	     then both inner_lhs and inner_rhs must be true.  */
	  const svalue *false_sval
	    = m_mgr->get_or_create_constant_svalue (boolean_false_node);
	  bool sat1 = add_constraint (inner_lhs, NE_EXPR, false_sval, ctxt);
	  bool sat2 = add_constraint (inner_rhs, NE_EXPR, false_sval, ctxt);
	  *out = sat1 && sat2;
	  return true;
	}
      return false;

    case BIT_IOR_EXPR:
      if (!is_true)
	{
	  /* ...and "(inner_lhs | inner_rhs) == 0"
	     i.e. "(inner_lhs | inner_rhs)" is false
	     then both inner_lhs and inner_rhs must be false.  */
	  const svalue *false_sval
	    = m_mgr->get_or_create_constant_svalue (boolean_false_node);
	  bool sat1 = add_constraint (inner_lhs, EQ_EXPR, false_sval, ctxt);
	  bool sat2 = add_constraint (inner_rhs, EQ_EXPR, false_sval, ctxt);
	  *out = sat1 && sat2;
	  return true;
	}
      return false;
    }
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

  return add_constraint (lhs_sval, op, rhs_sval, ctxt);
}

/* Attempt to add the constraint "LHS OP RHS" to this region_model.
   If it is consistent with existing constraints, add it, and return true.
   Return false if it contradicts existing constraints.
   Use CTXT for reporting any diagnostics associated with the accesses.  */

bool
region_model::add_constraint (const svalue *lhs,
			      enum tree_code op,
			      const svalue *rhs,
			      region_model_context *ctxt)
{
  tristate t_cond = eval_condition (lhs, op, rhs);

  /* If we already have the condition, do nothing.  */
  if (t_cond.is_true ())
    return true;

  /* Reject a constraint that would contradict existing knowledge, as
     unsatisfiable.  */
  if (t_cond.is_false ())
    return false;

  bool out;
  if (add_constraints_from_binop (lhs, op, rhs, &out, ctxt))
    return out;

  /* Attempt to store the constraint.  */
  if (!m_constraints->add_constraint (lhs, op, rhs))
    return false;

  /* Notify the context, if any.  This exists so that the state machines
     in a program_state can be notified about the condition, and so can
     set sm-state for e.g. unchecked->checked, both for cfg-edges, and
     when synthesizing constraints as above.  */
  if (ctxt)
    ctxt->on_condition (lhs, op, rhs);

  /* If we have &REGION == NULL, then drop dynamic extents for REGION (for
     the case where REGION is heap-allocated and thus could be NULL).  */
  if (tree rhs_cst = rhs->maybe_get_constant ())
    if (op == EQ_EXPR && zerop (rhs_cst))
      if (const region_svalue *region_sval = lhs->dyn_cast_region_svalue ())
	unset_dynamic_extents (region_sval->get_pointee ());

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
    *out = new rejected_op_constraint (*this, lhs, op, rhs);
  return sat;
}

/* Determine what is known about the condition "LHS OP RHS" within
   this model.
   Use CTXT for reporting any diagnostics associated with the accesses.  */

tristate
region_model::eval_condition (tree lhs,
			      enum tree_code op,
			      tree rhs,
			      region_model_context *ctxt) const
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
    {
      if (sval->get_kind () == SK_CONSTANT)
	return path_var (sval->maybe_get_constant (), 0);
      else
	return path_var (NULL_TREE, 0);
    }
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

  /* Handle binops.  */
  if (const binop_svalue *binop_sval = sval->dyn_cast_binop_svalue ())
    if (path_var lhs_pv
	= get_representative_path_var (binop_sval->get_arg0 (), visited))
      if (path_var rhs_pv
	  = get_representative_path_var (binop_sval->get_arg1 (), visited))
	return path_var (build2 (binop_sval->get_op (),
				 sval->get_type (),
				 lhs_pv.m_tree, rhs_pv.m_tree),
			 lhs_pv.m_stack_depth);

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

tree
region_model::get_representative_tree (const region *reg) const
{
  svalue_set visited;
  tree expr = get_representative_path_var (reg, &visited).m_tree;

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
    case RK_THREAD_LOCAL:
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

    case RK_SIZED:
      return path_var (NULL_TREE, 0);

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

    case RK_VAR_ARG:
    case RK_ERRNO:
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

  /* Copy this state and pass it to handle_phi so that all of the phi stmts
     are effectively handled simultaneously.  */
  const region_model old_state (*this);

  for (gphi_iterator gpi = const_cast<supernode *>(snode)->start_phis ();
       !gsi_end_p (gpi); gsi_next (&gpi))
    {
      gphi *phi = gpi.phi ();

      tree src = last_cfg_superedge->get_phi_arg (phi);
      tree lhs = gimple_phi_result (phi);

      /* Update next_state based on phi and old_state.  */
      handle_phi (phi, lhs, src, old_state, ctxt);
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
      /* This is a no-op for call summaries; we should already
	 have handled the effect of the call summary at the call stmt.  */
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
region_model::update_for_gcall (const gcall *call_stmt,
				region_model_context *ctxt,
				function *callee)
{
  /* Build a vec of argument svalues, using the current top
     frame for resolving tree expressions.  */
  auto_vec<const svalue *> arg_svals (gimple_call_num_args (call_stmt));

  for (unsigned i = 0; i < gimple_call_num_args (call_stmt); i++)
    {
      tree arg = gimple_call_arg (call_stmt, i);
      arg_svals.quick_push (get_rvalue (arg, ctxt));
    }

  if(!callee)
  {
    /* Get the function * from the gcall.  */
    tree fn_decl = get_fndecl_for_call (call_stmt,ctxt);
    callee = DECL_STRUCT_FUNCTION (fn_decl);
  }

  push_frame (callee, &arg_svals, ctxt);
}

/* Pop the top-most frame_region from the stack, and copy the return
   region's values (if any) into the region for the lvalue of the LHS of
   the call (if any).  */

void
region_model::update_for_return_gcall (const gcall *call_stmt,
				       region_model_context *ctxt)
{
  /* Get the lvalue for the result of the call, passing it to pop_frame,
     so that pop_frame can determine the region with respect to the
     *caller* frame.  */
  tree lhs = gimple_call_lhs (call_stmt);
  pop_frame (lhs, NULL, ctxt);
}

/* Extract calling information from the superedge and update the model for the 
   call  */

void
region_model::update_for_call_superedge (const call_superedge &call_edge,
					 region_model_context *ctxt)
{
  const gcall *call_stmt = call_edge.get_call_stmt ();
  update_for_gcall (call_stmt, ctxt, call_edge.get_callee_function ());
}

/* Extract calling information from the return superedge and update the model 
   for the returning call */

void
region_model::update_for_return_superedge (const return_superedge &return_edge,
					   region_model_context *ctxt)
{
  const gcall *call_stmt = return_edge.get_call_stmt ();
  update_for_return_gcall (call_stmt, ctxt);
}

/* Attempt to to use R to replay SUMMARY into this object.
   Return true if it is possible.  */

bool
region_model::replay_call_summary (call_summary_replay &r,
				   const region_model &summary)
{
  gcc_assert (summary.get_stack_depth () == 1);

  m_store.replay_call_summary (r, summary.m_store);

  if (!m_constraints->replay_call_summary (r, *summary.m_constraints))
    return false;

  for (auto kv : summary.m_dynamic_extents)
    {
      const region *summary_reg = kv.first;
      const region *caller_reg = r.convert_region_from_summary (summary_reg);
      if (!caller_reg)
	continue;
      const svalue *summary_sval = kv.second;
      const svalue *caller_sval = r.convert_svalue_from_summary (summary_sval);
      if (!caller_sval)
	continue;
      m_dynamic_extents.put (caller_reg, caller_sval);
    }

  return true;
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

/* Return true iff SWITCH_STMT has a non-default label that contains
   INT_CST.  */

static bool
has_nondefault_case_for_value_p (const gswitch *switch_stmt, tree int_cst)
{
  /* We expect the initial label to be the default; skip it.  */
  gcc_assert (CASE_LOW (gimple_switch_label (switch_stmt, 0)) == NULL);
  unsigned min_idx = 1;
  unsigned max_idx = gimple_switch_num_labels (switch_stmt) - 1;

  /* Binary search: try to find the label containing INT_CST.
     This requires the cases to be sorted by CASE_LOW (done by the
     gimplifier).  */
  while (max_idx >= min_idx)
    {
      unsigned case_idx = (min_idx + max_idx) / 2;
      tree label =  gimple_switch_label (switch_stmt, case_idx);
      tree low = CASE_LOW (label);
      gcc_assert (low);
      tree high = CASE_HIGH (label);
      if (!high)
	high = low;
      if (tree_int_cst_compare (int_cst, low) < 0)
	{
	  /* INT_CST is below the range of this label.  */
	  gcc_assert (case_idx > 0);
	  max_idx = case_idx - 1;
	}
      else if (tree_int_cst_compare (int_cst, high) > 0)
	{
	  /* INT_CST is above the range of this case.  */
	  min_idx = case_idx + 1;
	}
      else
	/* This case contains INT_CST.  */
	return true;
    }
  /* Not found.  */
  return false;
}

/* Return true iff SWITCH_STMT (which must be on an enum value)
   has nondefault cases handling all values in the enum.  */

static bool
has_nondefault_cases_for_all_enum_values_p (const gswitch *switch_stmt)
{
  gcc_assert (switch_stmt);
  tree type = TREE_TYPE (gimple_switch_index (switch_stmt));
  gcc_assert (TREE_CODE (type) == ENUMERAL_TYPE);

  for (tree enum_val_iter = TYPE_VALUES (type);
       enum_val_iter;
       enum_val_iter = TREE_CHAIN (enum_val_iter))
    {
      tree enum_val = TREE_VALUE (enum_val_iter);
      gcc_assert (TREE_CODE (enum_val) == CONST_DECL);
      gcc_assert (TREE_CODE (DECL_INITIAL (enum_val)) == INTEGER_CST);
      if (!has_nondefault_case_for_value_p (switch_stmt,
					    DECL_INITIAL (enum_val)))
	return false;
    }
  return true;
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
  const svalue *index_sval = get_rvalue (index, ctxt);

  /* If we're switching based on an enum type, assume that the user is only
     working with values from the enum.  Hence if this is an
     implicitly-created "default", assume it doesn't get followed.
     This fixes numerous "uninitialized" false positives where we otherwise
     consider jumping past the initialization cases.  */

  if (/* Don't check during feasibility-checking (when ctxt is NULL).  */
      ctxt
      /* Must be an enum value.  */
      && index_sval->get_type ()
      && TREE_CODE (TREE_TYPE (index)) == ENUMERAL_TYPE
      && TREE_CODE (index_sval->get_type ()) == ENUMERAL_TYPE
      /* If we have a constant, then we can check it directly.  */
      && index_sval->get_kind () != SK_CONSTANT
      && edge.implicitly_created_default_p ()
      && has_nondefault_cases_for_all_enum_values_p (switch_stmt)
      /* Don't do this if there's a chance that the index is
	 attacker-controlled.  */
      && !ctxt->possibly_tainted_p (index_sval))
    {
      if (out)
	*out = new rejected_default_case (*this);
      return false;
    }

  bounded_ranges_manager *ranges_mgr = get_range_manager ();
  const bounded_ranges *all_cases_ranges
    = ranges_mgr->get_or_create_ranges_for_switch (&edge, switch_stmt);
  bool sat = m_constraints->add_bounded_ranges (index_sval, all_cases_ranges);
  if (!sat && out)
    *out = new rejected_ranges_constraint (*this, index, all_cases_ranges);
  if (sat && ctxt && !all_cases_ranges->empty_p ())
    ctxt->on_bounded_ranges (*index_sval, *all_cases_ranges);
  return sat;
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
   the region.
   If NONNULL is true, then assume that PARAM must be non-NULL.  */

void
region_model::on_top_level_param (tree param,
				  bool nonnull,
				  region_model_context *ctxt)
{
  if (POINTER_TYPE_P (TREE_TYPE (param)))
    {
      const region *param_reg = get_lvalue (param, ctxt);
      const svalue *init_ptr_sval
	= m_mgr->get_or_create_initial_value (param_reg);
      const region *pointee_reg = m_mgr->get_symbolic_region (init_ptr_sval);
      m_store.mark_as_escaped (pointee_reg);
      if (nonnull)
	{
	  const svalue *null_ptr_sval
	    = m_mgr->get_or_create_null_ptr (TREE_TYPE (param));
	  add_constraint (init_ptr_sval, NE_EXPR, null_ptr_sval, ctxt);
	}
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

      /* Handle any variadic args.  */
      unsigned va_arg_idx = 0;
      for (; idx < arg_svals->length (); idx++, va_arg_idx++)
	{
	  const svalue *arg_sval = (*arg_svals)[idx];
	  const region *var_arg_reg
	    = m_mgr->get_var_arg_region (m_current_frame,
					 va_arg_idx);
	  set_value (var_arg_reg, arg_sval, ctxt);
	}
    }
  else
    {
      /* Otherwise we have a top-level call within the analysis.  The params
	 have defined but unknown initial values.
	 Anything they point to has escaped.  */
      tree fndecl = fun->decl;

      /* Handle "__attribute__((nonnull))".   */
      tree fntype = TREE_TYPE (fndecl);
      bitmap nonnull_args = get_nonnull_args (fntype);

      unsigned parm_idx = 0;
      for (tree iter_parm = DECL_ARGUMENTS (fndecl); iter_parm;
	   iter_parm = DECL_CHAIN (iter_parm))
	{
	  bool non_null = (nonnull_args
			   ? (bitmap_empty_p (nonnull_args)
			      || bitmap_bit_p (nonnull_args, parm_idx))
			   : false);
	  if (tree parm_default_ssa = ssa_default_def (fun, iter_parm))
	    on_top_level_param (parm_default_ssa, non_null, ctxt);
	  else
	    on_top_level_param (iter_parm, non_null, ctxt);
	  parm_idx++;
	}

      BITMAP_FREE (nonnull_args);
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

   If RESULT_LVALUE is non-null, copy any return value from the frame
   into the corresponding region (evaluated with respect to the *caller*
   frame, rather than the called frame).
   If OUT_RESULT is non-null, copy any return value from the frame
   into *OUT_RESULT.

   If EVAL_RETURN_SVALUE is false, then don't evaluate the return value.
   This is for use when unwinding frames e.g. due to longjmp, to suppress
   erroneously reporting uninitialized return values.

   Purge the frame region and all its descendent regions.
   Convert any pointers that point into such regions into
   POISON_KIND_POPPED_STACK svalues.  */

void
region_model::pop_frame (tree result_lvalue,
			 const svalue **out_result,
			 region_model_context *ctxt,
			 bool eval_return_svalue)
{
  gcc_assert (m_current_frame);

  const frame_region *frame_reg = m_current_frame;

  /* Notify state machines.  */
  if (ctxt)
    ctxt->on_pop_frame (frame_reg);

  /* Evaluate the result, within the callee frame.  */
  tree fndecl = m_current_frame->get_function ()->decl;
  tree result = DECL_RESULT (fndecl);
  const svalue *retval = NULL;
  if (result
      && TREE_TYPE (result) != void_type_node
      && eval_return_svalue)
    {
      retval = get_rvalue (result, ctxt);
      if (out_result)
	*out_result = retval;
    }

  /* Pop the frame.  */
  m_current_frame = m_current_frame->get_calling_frame ();

  if (result_lvalue && retval)
    {
      gcc_assert (eval_return_svalue);

      /* Compute result_dst_reg using RESULT_LVALUE *after* popping
	 the frame, but before poisoning pointers into the old frame.  */
      const region *result_dst_reg = get_lvalue (result_lvalue, ctxt);
      set_value (result_dst_reg, retval, ctxt);
    }

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
   poisoned values of kind PKIND.
   Also purge any dynamic extents.  */

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

  /* Purge dynamic extents of any base regions in REG and below
     (e.g. VLAs and alloca stack regions).  */
  for (auto iter : m_dynamic_extents)
    {
      const region *iter_reg = iter.first;
      if (iter_reg->descendent_of_p (reg))
	unset_dynamic_extents (iter_reg);
    }
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
				region_model *out_model,
				const extrinsic_state *ext_state,
				const program_state *state_a,
				const program_state *state_b) const
{
  gcc_assert (out_model);
  gcc_assert (m_mgr == other_model.m_mgr);
  gcc_assert (m_mgr == out_model->m_mgr);

  if (m_current_frame != other_model.m_current_frame)
    return false;
  out_model->m_current_frame = m_current_frame;

  model_merger m (this, &other_model, point, out_model,
		  ext_state, state_a, state_b);

  if (!store::can_merge_p (&m_store, &other_model.m_store,
			   &out_model->m_store, m_mgr->get_store_manager (),
			   &m))
    return false;

  if (!m_dynamic_extents.can_merge_with_p (other_model.m_dynamic_extents,
					   &out_model->m_dynamic_extents))
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

struct append_regions_cb_data
{
  const region_model *model;
  auto_vec<const decl_region *> *out;
};

/* Populate *OUT with all decl_regions in the current
   frame that have clusters within the store.  */

void
region_model::
get_regions_for_current_frame (auto_vec<const decl_region *> *out) const
{
  append_regions_cb_data data;
  data.model = this;
  data.out = out;
  m_store.for_each_cluster (append_regions_cb, &data);
}

/* Implementation detail of get_regions_for_current_frame.  */

void
region_model::append_regions_cb (const region *base_reg,
				 append_regions_cb_data *cb_data)
{
  if (base_reg->get_parent_region () != cb_data->model->m_current_frame)
    return;
  if (const decl_region *decl_reg = base_reg->dyn_cast_decl_region ())
    cb_data->out->safe_push (decl_reg);
}


/* Abstract class for diagnostics related to the use of
   floating-point arithmetic where precision is needed.  */

class imprecise_floating_point_arithmetic : public pending_diagnostic
{
public:
  int get_controlling_option () const final override
  {
    return OPT_Wanalyzer_imprecise_fp_arithmetic;
  }
};

/* Concrete diagnostic to complain about uses of floating-point arithmetic
   in the size argument of malloc etc.  */

class float_as_size_arg : public imprecise_floating_point_arithmetic
{
public:
  float_as_size_arg (tree arg) : m_arg (arg)
  {}

  const char *get_kind () const final override
  {
    return "float_as_size_arg_diagnostic";
  }

  bool subclass_equal_p (const pending_diagnostic &other) const final override
  {
    return same_tree_p (m_arg, ((const float_as_size_arg &) other).m_arg);
  }

  bool emit (rich_location *rich_loc, logger *) final override
  {
    diagnostic_metadata m;
    bool warned = warning_meta (rich_loc, m, get_controlling_option (),
				"use of floating-point arithmetic here might"
				" yield unexpected results");
    if (warned)
      inform (rich_loc->get_loc (), "only use operands of an integer type"
				    " inside the size argument");
    return warned;
  }

  label_text describe_final_event (const evdesc::final_event &ev) final
  override
  {
    if (m_arg)
      return ev.formatted_print ("operand %qE is of type %qT",
				 m_arg, TREE_TYPE (m_arg));
    return ev.formatted_print ("at least one operand of the size argument is"
			       " of a floating-point type");
  }

private:
  tree m_arg;
};

/* Visitor to find uses of floating-point variables/constants in an svalue.  */

class contains_floating_point_visitor : public visitor
{
public:
  contains_floating_point_visitor (const svalue *root_sval) : m_result (NULL)
  {
    root_sval->accept (this);
  }

  const svalue *get_svalue_to_report ()
  {
    return m_result;
  }

  void visit_constant_svalue (const constant_svalue *sval) final override
  {
    /* At the point the analyzer runs, constant integer operands in a floating
       point expression are already implictly converted to floating-points.
       Thus, we do prefer to report non-constants such that the diagnostic
       always reports a floating-point operand.  */
    tree type = sval->get_type ();
    if (type && FLOAT_TYPE_P (type) && !m_result)
      m_result = sval;
  }

  void visit_conjured_svalue (const conjured_svalue *sval) final override
  {
    tree type = sval->get_type ();
    if (type && FLOAT_TYPE_P (type))
      m_result = sval;
  }

  void visit_initial_svalue (const initial_svalue *sval) final override
  {
    tree type = sval->get_type ();
    if (type && FLOAT_TYPE_P (type))
      m_result = sval;
  }

private:
  /* Non-null if at least one floating-point operand was found.  */
  const svalue *m_result;
};

/* May complain about uses of floating-point operands in SIZE_IN_BYTES.  */

void
region_model::check_dynamic_size_for_floats (const svalue *size_in_bytes,
					     region_model_context *ctxt) const
{
  gcc_assert (ctxt);

  contains_floating_point_visitor v (size_in_bytes);
  if (const svalue *float_sval = v.get_svalue_to_report ())
	{
	  tree diag_arg = get_representative_tree (float_sval);
	  ctxt->warn (make_unique<float_as_size_arg> (diag_arg));
	}
}

/* Return a region describing a heap-allocated block of memory.
   Use CTXT to complain about tainted sizes.

   Reuse an existing heap_allocated_region if it's not being referenced by
   this region_model; otherwise create a new one.  */

const region *
region_model::get_or_create_region_for_heap_alloc (const svalue *size_in_bytes,
						   region_model_context *ctxt)
{
  /* Determine which regions are referenced in this region_model, so that
     we can reuse an existing heap_allocated_region if it's not in use on
     this path.  */
  auto_bitmap base_regs_in_use;
  get_referenced_base_regions (base_regs_in_use);

  /* Don't reuse regions that are marked as TOUCHED.  */
  for (store::cluster_map_t::iterator iter = m_store.begin ();
       iter != m_store.end (); ++iter)
    if ((*iter).second->touched_p ())
      {
	const region *base_reg = (*iter).first;
	bitmap_set_bit (base_regs_in_use, base_reg->get_id ());
      }

  const region *reg
    = m_mgr->get_or_create_region_for_heap_alloc (base_regs_in_use);
  if (size_in_bytes)
    if (compat_types_p (size_in_bytes->get_type (), size_type_node))
      set_dynamic_extents (reg, size_in_bytes, ctxt);
  return reg;
}

/* Populate OUT_IDS with the set of IDs of those base regions which are
   reachable in this region_model.  */

void
region_model::get_referenced_base_regions (auto_bitmap &out_ids) const
{
  reachable_regions reachable_regs (const_cast<region_model *> (this));
  m_store.for_each_cluster (reachable_regions::init_cluster_cb,
			    &reachable_regs);
  /* Get regions for locals that have explicitly bound values.  */
  for (store::cluster_map_t::iterator iter = m_store.begin ();
       iter != m_store.end (); ++iter)
    {
      const region *base_reg = (*iter).first;
      if (const region *parent = base_reg->get_parent_region ())
	if (parent->get_kind () == RK_FRAME)
	  reachable_regs.add (base_reg, false);
    }

  bitmap_clear (out_ids);
  for (auto iter_reg : reachable_regs)
    bitmap_set_bit (out_ids, iter_reg->get_id ());
}

/* Return a new region describing a block of memory allocated within the
   current frame.
   Use CTXT to complain about tainted sizes.  */

const region *
region_model::create_region_for_alloca (const svalue *size_in_bytes,
					region_model_context *ctxt)
{
  const region *reg = m_mgr->create_region_for_alloca (m_current_frame);
  if (compat_types_p (size_in_bytes->get_type (), size_type_node))
    set_dynamic_extents (reg, size_in_bytes, ctxt);
  return reg;
}

/* Record that the size of REG is SIZE_IN_BYTES.
   Use CTXT to complain about tainted sizes.  */

void
region_model::set_dynamic_extents (const region *reg,
				   const svalue *size_in_bytes,
				   region_model_context *ctxt)
{
  assert_compat_types (size_in_bytes->get_type (), size_type_node);
  if (ctxt)
    {
      check_dynamic_size_for_taint (reg->get_memory_space (), size_in_bytes,
				    ctxt);
      check_dynamic_size_for_floats (size_in_bytes, ctxt);
    }
  m_dynamic_extents.put (reg, size_in_bytes);
}

/* Get the recording of REG in bytes, or NULL if no dynamic size was
   recorded.  */

const svalue *
region_model::get_dynamic_extents (const region *reg) const
{
  if (const svalue * const *slot = m_dynamic_extents.get (reg))
    return *slot;
  return NULL;
}

/* Unset any recorded dynamic size of REG.  */

void
region_model::unset_dynamic_extents (const region *reg)
{
  m_dynamic_extents.remove (reg);
}

/* Information of the layout of a RECORD_TYPE, capturing it as a vector
   of items, where each item is either a field or padding.  */

class record_layout
{
public:
  /* An item within a record; either a field, or padding after a field.  */
  struct item
  {
  public:
    item (const bit_range &br,
	  tree field,
	  bool is_padding)
    : m_bit_range (br),
      m_field (field),
      m_is_padding (is_padding)
    {
    }

    bit_offset_t get_start_bit_offset () const
    {
      return m_bit_range.get_start_bit_offset ();
    }
    bit_offset_t get_next_bit_offset () const
    {
      return m_bit_range.get_next_bit_offset ();
    }

    bool contains_p (bit_offset_t offset) const
    {
      return m_bit_range.contains_p (offset);
    }

    void dump_to_pp (pretty_printer *pp) const
    {
      if (m_is_padding)
	pp_printf (pp, "padding after %qD", m_field);
      else
	pp_printf (pp, "%qD", m_field);
      pp_string (pp, ", ");
      m_bit_range.dump_to_pp (pp);
    }

    bit_range m_bit_range;
    tree m_field;
    bool m_is_padding;
  };

  record_layout (tree record_type)
  {
    gcc_assert (TREE_CODE (record_type) == RECORD_TYPE);

    for (tree iter = TYPE_FIELDS (record_type); iter != NULL_TREE;
	 iter = DECL_CHAIN (iter))
      {
	if (TREE_CODE (iter) == FIELD_DECL)
	  {
	    int iter_field_offset = int_bit_position (iter);
	    bit_size_t size_in_bits;
	    if (!int_size_in_bits (TREE_TYPE (iter), &size_in_bits))
	      size_in_bits = 0;

	    maybe_pad_to (iter_field_offset);

	    /* Add field.  */
	    m_items.safe_push (item (bit_range (iter_field_offset,
						size_in_bits),
				     iter, false));
	  }
      }

    /* Add any trailing padding.  */
    bit_size_t size_in_bits;
    if (int_size_in_bits (record_type, &size_in_bits))
      maybe_pad_to (size_in_bits);
  }

  void dump_to_pp (pretty_printer *pp) const
  {
    unsigned i;
    item *it;
    FOR_EACH_VEC_ELT (m_items, i, it)
      {
	it->dump_to_pp (pp);
	pp_newline (pp);
      }
  }

  DEBUG_FUNCTION void dump () const
  {
    pretty_printer pp;
    pp_format_decoder (&pp) = default_tree_printer;
    pp.buffer->stream = stderr;
    dump_to_pp (&pp);
    pp_flush (&pp);
  }

  const record_layout::item *get_item_at (bit_offset_t offset) const
  {
    unsigned i;
    item *it;
    FOR_EACH_VEC_ELT (m_items, i, it)
      if (it->contains_p (offset))
	return it;
    return NULL;
  }

private:
  /* Subroutine of ctor.  Add padding item to NEXT_OFFSET if necessary.  */

  void maybe_pad_to (bit_offset_t next_offset)
  {
    if (m_items.length () > 0)
      {
	const item &last_item = m_items[m_items.length () - 1];
	bit_offset_t offset_after_last_item
	  = last_item.get_next_bit_offset ();
	if (next_offset > offset_after_last_item)
	  {
	    bit_size_t padding_size
	      = next_offset - offset_after_last_item;
	    m_items.safe_push (item (bit_range (offset_after_last_item,
						padding_size),
				     last_item.m_field, true));
	  }
      }
  }

  auto_vec<item> m_items;
};

/* A subclass of pending_diagnostic for complaining about uninitialized data
   being copied across a trust boundary to an untrusted output
   (e.g. copy_to_user infoleaks in the Linux kernel).  */

class exposure_through_uninit_copy
  : public pending_diagnostic_subclass<exposure_through_uninit_copy>
{
public:
  exposure_through_uninit_copy (const region *src_region,
				const region *dest_region,
				const svalue *copied_sval)
  : m_src_region (src_region),
    m_dest_region (dest_region),
    m_copied_sval (copied_sval)
  {
    gcc_assert (m_copied_sval->get_kind () == SK_POISONED
		|| m_copied_sval->get_kind () == SK_COMPOUND);
  }

  const char *get_kind () const final override
  {
    return "exposure_through_uninit_copy";
  }

  bool operator== (const exposure_through_uninit_copy &other) const
  {
    return (m_src_region == other.m_src_region
	    && m_dest_region == other.m_dest_region
	    && m_copied_sval == other.m_copied_sval);
  }

  int get_controlling_option () const final override
  {
    return OPT_Wanalyzer_exposure_through_uninit_copy;
  }

  bool emit (rich_location *rich_loc, logger *) final override
  {
    diagnostic_metadata m;
    /* CWE-200: Exposure of Sensitive Information to an Unauthorized Actor.  */
    m.add_cwe (200);
    enum memory_space mem_space = get_src_memory_space ();
    bool warned;
    switch (mem_space)
      {
      default:
	warned = warning_meta
	  (rich_loc, m, get_controlling_option (),
	   "potential exposure of sensitive information"
	   " by copying uninitialized data across trust boundary");
	break;
      case MEMSPACE_STACK:
	warned = warning_meta
	  (rich_loc, m, get_controlling_option (),
	   "potential exposure of sensitive information"
	   " by copying uninitialized data from stack across trust boundary");
	break;
      case MEMSPACE_HEAP:
	warned = warning_meta
	  (rich_loc, m, get_controlling_option (),
	   "potential exposure of sensitive information"
	   " by copying uninitialized data from heap across trust boundary");
	break;
      }
    if (warned)
      {
	location_t loc = rich_loc->get_loc ();
	inform_number_of_uninit_bits (loc);
	complain_about_uninit_ranges (loc);

	if (mem_space == MEMSPACE_STACK)
	  maybe_emit_fixit_hint ();
      }
    return warned;
  }

  label_text describe_final_event (const evdesc::final_event &) final override
  {
    enum memory_space mem_space = get_src_memory_space ();
    switch (mem_space)
      {
      default:
	return label_text::borrow ("uninitialized data copied here");

      case MEMSPACE_STACK:
	return label_text::borrow ("uninitialized data copied from stack here");

      case MEMSPACE_HEAP:
	return label_text::borrow ("uninitialized data copied from heap here");
      }
  }

  void mark_interesting_stuff (interesting_t *interest) final override
  {
    if (m_src_region)
      interest->add_region_creation (m_src_region);
  }

private:
  enum memory_space get_src_memory_space () const
  {
    return m_src_region ? m_src_region->get_memory_space () : MEMSPACE_UNKNOWN;
  }

  bit_size_t calc_num_uninit_bits () const
  {
    switch (m_copied_sval->get_kind ())
      {
      default:
	gcc_unreachable ();
	break;
      case SK_POISONED:
	{
	  const poisoned_svalue *poisoned_sval
	    = as_a <const poisoned_svalue *> (m_copied_sval);
	  gcc_assert (poisoned_sval->get_poison_kind () == POISON_KIND_UNINIT);

	  /* Give up if don't have type information.  */
	  if (m_copied_sval->get_type () == NULL_TREE)
	    return 0;

	  bit_size_t size_in_bits;
	  if (int_size_in_bits (m_copied_sval->get_type (), &size_in_bits))
	    return size_in_bits;

	  /* Give up if we can't get the size of the type.  */
	  return 0;
	}
	break;
      case SK_COMPOUND:
	{
	  const compound_svalue *compound_sval
	    = as_a <const compound_svalue *> (m_copied_sval);
	  bit_size_t result = 0;
	  /* Find keys for uninit svals.  */
	  for (auto iter : *compound_sval)
	    {
	      const svalue *sval = iter.second;
	      if (const poisoned_svalue *psval
		  = sval->dyn_cast_poisoned_svalue ())
		if (psval->get_poison_kind () == POISON_KIND_UNINIT)
		  {
		    const binding_key *key = iter.first;
		    const concrete_binding *ckey
		      = key->dyn_cast_concrete_binding ();
		    gcc_assert (ckey);
		    result += ckey->get_size_in_bits ();
		  }
	    }
	  return result;
	}
      }
  }

  void inform_number_of_uninit_bits (location_t loc) const
  {
    bit_size_t num_uninit_bits = calc_num_uninit_bits ();
    if (num_uninit_bits <= 0)
      return;
    if (num_uninit_bits % BITS_PER_UNIT == 0)
      {
	/* Express in bytes.  */
	byte_size_t num_uninit_bytes = num_uninit_bits / BITS_PER_UNIT;
	if (num_uninit_bytes == 1)
	  inform (loc, "1 byte is uninitialized");
	else
	  inform (loc,
		  "%wu bytes are uninitialized", num_uninit_bytes.to_uhwi ());
      }
    else
      {
	/* Express in bits.  */
	if (num_uninit_bits == 1)
	  inform (loc, "1 bit is uninitialized");
	else
	  inform (loc,
		  "%wu bits are uninitialized", num_uninit_bits.to_uhwi ());
      }
  }

  void complain_about_uninit_ranges (location_t loc) const
  {
    if (const compound_svalue *compound_sval
	= m_copied_sval->dyn_cast_compound_svalue ())
      {
	/* Find keys for uninit svals.  */
	auto_vec<const concrete_binding *> uninit_keys;
	for (auto iter : *compound_sval)
	  {
	    const svalue *sval = iter.second;
	    if (const poisoned_svalue *psval
		= sval->dyn_cast_poisoned_svalue ())
	      if (psval->get_poison_kind () == POISON_KIND_UNINIT)
		{
		  const binding_key *key = iter.first;
		  const concrete_binding *ckey
		    = key->dyn_cast_concrete_binding ();
		  gcc_assert (ckey);
		  uninit_keys.safe_push (ckey);
		}
	  }
	/* Complain about them in sorted order.  */
	uninit_keys.qsort (concrete_binding::cmp_ptr_ptr);

	std::unique_ptr<record_layout> layout;

	tree type = m_copied_sval->get_type ();
	if (type && TREE_CODE (type) == RECORD_TYPE)
	  {
	    // (std::make_unique is C++14)
	    layout = std::unique_ptr<record_layout> (new record_layout (type));

	    if (0)
	      layout->dump ();
	  }

	unsigned i;
	const concrete_binding *ckey;
	FOR_EACH_VEC_ELT (uninit_keys, i, ckey)
	  {
	    bit_offset_t start_bit = ckey->get_start_bit_offset ();
	    bit_offset_t next_bit = ckey->get_next_bit_offset ();
	    complain_about_uninit_range (loc, start_bit, next_bit,
					 layout.get ());
	  }
      }
  }

  void complain_about_uninit_range (location_t loc,
				    bit_offset_t start_bit,
				    bit_offset_t next_bit,
				    const record_layout *layout) const
  {
    if (layout)
      {
	while (start_bit < next_bit)
	  {
	    if (const record_layout::item *item
		  = layout->get_item_at (start_bit))
	      {
		gcc_assert (start_bit >= item->get_start_bit_offset ());
		gcc_assert (start_bit < item->get_next_bit_offset ());
		if (item->get_start_bit_offset () == start_bit
		    && item->get_next_bit_offset () <= next_bit)
		  complain_about_fully_uninit_item (*item);
		else
		  complain_about_partially_uninit_item (*item);
		start_bit = item->get_next_bit_offset ();
		continue;
	      }
	    else
	      break;
	  }
      }

    if (start_bit >= next_bit)
      return;

    if (start_bit % 8 == 0 && next_bit % 8 == 0)
      {
	/* Express in bytes.  */
	byte_offset_t start_byte = start_bit / 8;
	byte_offset_t last_byte = (next_bit / 8) - 1;
	if (last_byte == start_byte)
	  inform (loc,
		  "byte %wu is uninitialized",
		  start_byte.to_uhwi ());
	else
	  inform (loc,
		  "bytes %wu - %wu are uninitialized",
		  start_byte.to_uhwi (),
		  last_byte.to_uhwi ());
      }
    else
      {
	/* Express in bits.  */
	bit_offset_t last_bit = next_bit - 1;
	if (last_bit == start_bit)
	  inform (loc,
		  "bit %wu is uninitialized",
		  start_bit.to_uhwi ());
	else
	  inform (loc,
		  "bits %wu - %wu are uninitialized",
		  start_bit.to_uhwi (),
		  last_bit.to_uhwi ());
      }
  }

  static void
  complain_about_fully_uninit_item (const record_layout::item &item)
  {
    tree field = item.m_field;
    bit_size_t num_bits = item.m_bit_range.m_size_in_bits;
    if (item.m_is_padding)
      {
	if (num_bits % 8 == 0)
	  {
	    /* Express in bytes.  */
	    byte_size_t num_bytes = num_bits / BITS_PER_UNIT;
	    if (num_bytes == 1)
	      inform (DECL_SOURCE_LOCATION (field),
		      "padding after field %qD is uninitialized (1 byte)",
		      field);
	    else
	      inform (DECL_SOURCE_LOCATION (field),
		      "padding after field %qD is uninitialized (%wu bytes)",
		      field, num_bytes.to_uhwi ());
	  }
	else
	  {
	    /* Express in bits.  */
	    if (num_bits == 1)
	      inform (DECL_SOURCE_LOCATION (field),
		      "padding after field %qD is uninitialized (1 bit)",
		      field);
	    else
	      inform (DECL_SOURCE_LOCATION (field),
		      "padding after field %qD is uninitialized (%wu bits)",
		      field, num_bits.to_uhwi ());
	  }
      }
    else
      {
	if (num_bits % 8 == 0)
	  {
	    /* Express in bytes.  */
	    byte_size_t num_bytes = num_bits / BITS_PER_UNIT;
	    if (num_bytes == 1)
	      inform (DECL_SOURCE_LOCATION (field),
		      "field %qD is uninitialized (1 byte)", field);
	    else
	      inform (DECL_SOURCE_LOCATION (field),
		      "field %qD is uninitialized (%wu bytes)",
		      field, num_bytes.to_uhwi ());
	  }
	else
	  {
	    /* Express in bits.  */
	    if (num_bits == 1)
	      inform (DECL_SOURCE_LOCATION (field),
		      "field %qD is uninitialized (1 bit)", field);
	    else
	      inform (DECL_SOURCE_LOCATION (field),
		      "field %qD is uninitialized (%wu bits)",
		      field, num_bits.to_uhwi ());
	  }
      }
  }

  static void
  complain_about_partially_uninit_item (const record_layout::item &item)
  {
    tree field = item.m_field;
    if (item.m_is_padding)
      inform (DECL_SOURCE_LOCATION (field),
	      "padding after field %qD is partially uninitialized",
	      field);
    else
      inform (DECL_SOURCE_LOCATION (field),
	      "field %qD is partially uninitialized",
	      field);
    /* TODO: ideally we'd describe what parts are uninitialized.  */
  }

  void maybe_emit_fixit_hint () const
  {
    if (tree decl = m_src_region->maybe_get_decl ())
      {
	gcc_rich_location hint_richloc (DECL_SOURCE_LOCATION (decl));
	hint_richloc.add_fixit_insert_after (" = {0}");
	inform (&hint_richloc,
		"suggest forcing zero-initialization by"
		" providing a %<{0}%> initializer");
      }
  }

private:
  const region *m_src_region;
  const region *m_dest_region;
  const svalue *m_copied_sval;
};

/* Return true if any part of SVAL is uninitialized.  */

static bool
contains_uninit_p (const svalue *sval)
{
  struct uninit_finder : public visitor
  {
  public:
    uninit_finder () : m_found_uninit (false) {}
    void visit_poisoned_svalue (const poisoned_svalue *sval)
    {
      if (sval->get_poison_kind () == POISON_KIND_UNINIT)
	m_found_uninit = true;
    }
    bool m_found_uninit;
  };

  uninit_finder v;
  sval->accept (&v);

  return v.m_found_uninit;
}

/* Function for use by plugins when simulating writing data through a
   pointer to an "untrusted" region DST_REG (and thus crossing a security
   boundary), such as copying data to user space in an OS kernel.

   Check that COPIED_SVAL is fully initialized.  If not, complain about
   an infoleak to CTXT.

   SRC_REG can be NULL; if non-NULL it is used as a hint in the diagnostic
   as to where COPIED_SVAL came from.  */

void
region_model::maybe_complain_about_infoleak (const region *dst_reg,
					     const svalue *copied_sval,
					     const region *src_reg,
					     region_model_context *ctxt)
{
  /* Check for exposure.  */
  if (contains_uninit_p (copied_sval))
    ctxt->warn (make_unique<exposure_through_uninit_copy> (src_reg,
							   dst_reg,
							   copied_sval));
}

/* Set errno to a positive symbolic int, as if some error has occurred.  */

void
region_model::set_errno (const call_details &cd)
{
  const region *errno_reg = m_mgr->get_errno_region ();
  conjured_purge p (this, cd.get_ctxt ());
  const svalue *new_errno_sval
    = m_mgr->get_or_create_conjured_svalue (integer_type_node,
					    cd.get_call_stmt (),
					    errno_reg, p);
  const svalue *zero
    = m_mgr->get_or_create_int_cst (integer_type_node, 0);
  add_constraint (new_errno_sval, GT_EXPR, zero, cd.get_ctxt ());
  set_value (errno_reg, new_errno_sval, cd.get_ctxt ());
}

/* class noop_region_model_context : public region_model_context.  */

void
noop_region_model_context::add_note (std::unique_ptr<pending_note>)
{
}

void
noop_region_model_context::bifurcate (std::unique_ptr<custom_edge_info>)
{
}

void
noop_region_model_context::terminate_path ()
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

/* Return true if it's OK to merge SVAL with other svalues.  */

bool
model_merger::mergeable_svalue_p (const svalue *sval) const
{
  if (m_ext_state)
    {
      /* Reject merging svalues that have non-purgable sm-state,
	 to avoid falsely reporting memory leaks by merging them
	 with something else.  For example, given a local var "p",
	 reject the merger of a:
	   store_a mapping "p" to a malloc-ed ptr
	 with:
	   store_b mapping "p" to a NULL ptr.  */
      if (m_state_a)
	if (!m_state_a->can_purge_p (*m_ext_state, sval))
	  return false;
      if (m_state_b)
	if (!m_state_b->can_purge_p (*m_ext_state, sval))
	  return false;
    }
  return true;
}

} // namespace ana

/* Dump RMODEL fully to stderr (i.e. without summarization).  */

DEBUG_FUNCTION void
debug (const region_model &rmodel)
{
  rmodel.dump (false);
}

/* class rejected_op_constraint : public rejected_constraint.  */

void
rejected_op_constraint::dump_to_pp (pretty_printer *pp) const
{
  region_model m (m_model);
  const svalue *lhs_sval = m.get_rvalue (m_lhs, NULL);
  const svalue *rhs_sval = m.get_rvalue (m_rhs, NULL);
  lhs_sval->dump_to_pp (pp, true);
  pp_printf (pp, " %s ", op_symbol_code (m_op));
  rhs_sval->dump_to_pp (pp, true);
}

/* class rejected_default_case : public rejected_constraint.  */

void
rejected_default_case::dump_to_pp (pretty_printer *pp) const
{
  pp_string (pp, "implicit default for enum");
}

/* class rejected_ranges_constraint : public rejected_constraint.  */

void
rejected_ranges_constraint::dump_to_pp (pretty_printer *pp) const
{
  region_model m (m_model);
  const svalue *sval = m.get_rvalue (m_expr, NULL);
  sval->dump_to_pp (pp, true);
  pp_string (pp, " in ");
  m_ranges->dump_to_pp (pp, true);
}

/* class engine.  */

/* engine's ctor.  */

engine::engine (const supergraph *sg, logger *logger)
: m_sg (sg), m_mgr (logger)
{
}

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
    region_offset offset = c_x_reg->get_offset (&mgr);
    ASSERT_EQ (offset.get_base_region (), model.get_lvalue (c, NULL));
    ASSERT_EQ (offset.get_bit_offset (), 0);
  }

  /* Verify get_offset for "c.y".  */
  {
    const region *c_y_reg = model.get_lvalue (c_y, NULL);
    region_offset offset = c_y_reg->get_offset (&mgr);
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
    placeholder_svalue test_sval (mgr.alloc_symbol_id (),
				  char_type_node, "test value");

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
      placeholder_svalue test_sval_x (mgr.alloc_symbol_id (),
				      integer_type_node, "test x val");
      m.set_value (c_x_reg, &test_sval_x, &ctxt);
      tree rep = m.get_representative_tree (&test_sval_x);
      ASSERT_DUMP_TREE_EQ (rep, "c.x");
    }

    /* Value of non-initial field.  */
    {
      region_model m (&mgr);
      const region *c_y_reg = m.get_lvalue (c_y, &ctxt);
      placeholder_svalue test_sval_y (mgr.alloc_symbol_id (),
				      integer_type_node, "test y val");
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

  /* Verify various binops on booleans.  */
  {
    const svalue *sval_true = mgr.get_or_create_int_cst (boolean_type_node, 1);
    const svalue *sval_false = mgr.get_or_create_int_cst (boolean_type_node, 0);
    const svalue *sval_unknown
      = mgr.get_or_create_unknown_svalue (boolean_type_node);
    const placeholder_svalue sval_placeholder (mgr.alloc_symbol_id (),
					       boolean_type_node, "v");
    for (auto op : {BIT_IOR_EXPR, TRUTH_OR_EXPR})
      {
	ASSERT_EQ (mgr.get_or_create_binop (boolean_type_node, op,
					    sval_true, sval_unknown),
		   sval_true);
	ASSERT_EQ (mgr.get_or_create_binop (boolean_type_node, op,
					    sval_false, sval_unknown),
		   sval_unknown);
	ASSERT_EQ (mgr.get_or_create_binop (boolean_type_node, op,
					    sval_false, &sval_placeholder),
		   &sval_placeholder);
      }
    for (auto op : {BIT_AND_EXPR, TRUTH_AND_EXPR})
      {
	ASSERT_EQ (mgr.get_or_create_binop (boolean_type_node, op,
					    sval_false, sval_unknown),
		   sval_false);
	ASSERT_EQ (mgr.get_or_create_binop (boolean_type_node, op,
					    sval_true, sval_unknown),
		   sval_unknown);
	ASSERT_EQ (mgr.get_or_create_binop (boolean_type_node, op,
					    sval_true, &sval_placeholder),
		   &sval_placeholder);
      }
  }
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

/* Get BIT within VAL as a symbolic value within MGR.  */

static const svalue *
get_bit (region_model_manager *mgr,
	 bit_offset_t bit,
	 unsigned HOST_WIDE_INT val)
{
  const svalue *inner_svalue
    = mgr->get_or_create_int_cst (unsigned_type_node, val);
  return mgr->get_or_create_bits_within (boolean_type_node,
					 bit_range (bit, 1),
					 inner_svalue);
}

/* Verify that bits_within_svalues are folded as expected.  */

static void
test_bits_within_svalue_folding ()
{
  region_model_manager mgr;

  const svalue *zero = mgr.get_or_create_int_cst (boolean_type_node, 0);
  const svalue *one = mgr.get_or_create_int_cst (boolean_type_node, 1);

  {
    const unsigned val = 0x0000;
    for (unsigned bit = 0; bit < 16; bit++)
      ASSERT_EQ (get_bit (&mgr, bit, val), zero);
  }

  {
    const unsigned val = 0x0001;
    ASSERT_EQ (get_bit (&mgr, 0, val), one);
    for (unsigned bit = 1; bit < 16; bit++)
      ASSERT_EQ (get_bit (&mgr, bit, val), zero);
  }

  {
    const unsigned val = 0x8000;
    for (unsigned bit = 0; bit < 15; bit++)
      ASSERT_EQ (get_bit (&mgr, bit, val), zero);
    ASSERT_EQ (get_bit (&mgr, 15, val), one);
  }

  {
    const unsigned val = 0xFFFF;
    for (unsigned bit = 0; bit < 16; bit++)
      ASSERT_EQ (get_bit (&mgr, bit, val), one);
  }
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

/* Verify that bit_range_region works as expected.  */

static void
test_bit_range_regions ()
{
  tree x = build_global_decl ("x", integer_type_node);
  region_model_manager mgr;
  const region *x_reg = mgr.get_region_for_global (x);
  const region *byte0
    = mgr.get_bit_range (x_reg, char_type_node, bit_range (0, 8));
  const region *byte1
    = mgr.get_bit_range (x_reg, char_type_node, bit_range (8, 8));
  ASSERT_TRUE (byte0->descendent_of_p (x_reg));
  ASSERT_TRUE (byte1->descendent_of_p (x_reg));
  ASSERT_NE (byte0, byte1);
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
  const svalue *sval = model.get_rvalue (c, NULL);
  model.set_value (model.get_lvalue (d, NULL), sval, NULL);

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
  DECL_CONTEXT (a) = parent_fndecl;
  tree b = build_decl (UNKNOWN_LOCATION, PARM_DECL,
		       get_identifier ("b"),
		       integer_type_node);
  DECL_CONTEXT (b) = parent_fndecl;
  /* "x" and "y" in a child frame.  */
  tree x = build_decl (UNKNOWN_LOCATION, PARM_DECL,
		       get_identifier ("x"),
		       integer_type_node);
  DECL_CONTEXT (x) = child_fndecl;
  tree y = build_decl (UNKNOWN_LOCATION, PARM_DECL,
		       get_identifier ("y"),
		       integer_type_node);
  DECL_CONTEXT (y) = child_fndecl;

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
  const svalue *new_p_sval = model.get_rvalue (p, NULL);
  ASSERT_EQ (new_p_sval->get_kind (), SK_POISONED);
  ASSERT_EQ (new_p_sval->dyn_cast_poisoned_svalue ()->get_poison_kind (),
	     POISON_KIND_POPPED_STACK);

  /* Verify that q still points to p, in spite of the region
     renumbering.  */
  const svalue *new_q_sval = model.get_rvalue (q, &ctxt);
  ASSERT_EQ (new_q_sval->get_kind (), SK_REGION);
  ASSERT_EQ (new_q_sval->maybe_get_region (),
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
  DECL_CONTEXT (n) = fndecl;

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

  for (tree cst : csts)
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
  region_model_manager *mgr = out_merged_model->get_manager ();
  program_point point (program_point::origin (*mgr));
  test_region_model_context ctxt;
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
  DECL_CONTEXT (a) = test_fndecl;
  tree addr_of_a = build1 (ADDR_EXPR, ptr_type_node, a);

  /* Param "q", a pointer.  */
  tree q = build_decl (UNKNOWN_LOCATION, PARM_DECL,
		       get_identifier ("q"),
		       ptr_type_node);
  DECL_CONTEXT (q) = test_fndecl;

  region_model_manager mgr;
  program_point point (program_point::origin (mgr));

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

    placeholder_svalue test_sval (mgr.alloc_symbol_id (),
				  integer_type_node, "test sval");
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

    placeholder_svalue test_sval (mgr.alloc_symbol_id (),
				  integer_type_node, "test sval");
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
    tree size = build_int_cst (size_type_node, 1024);
    const svalue *size_sval = mgr.get_or_create_constant_svalue (size);
    const region *new_reg
      = model0.get_or_create_region_for_heap_alloc (size_sval, &ctxt);
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
    placeholder_svalue placeholder_sval (mgr.alloc_symbol_id (),
					 integer_type_node, "test");
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
    const region *reg_pointee = sval_ptr->maybe_get_region ();
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
  program_point point (program_point::origin (mgr));
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
  region_model_manager mgr;
  function_point point (program_point::origin (mgr).get_function_point ());
  tree int_0 = build_int_cst (integer_type_node, 0);
  tree int_m1 = build_int_cst (integer_type_node, -1);
  tree int_1 = build_int_cst (integer_type_node, 1);
  tree int_256 = build_int_cst (integer_type_node, 256);
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
  region_model_manager mgr;
  program_point point (program_point::origin (mgr));

  tree int_0 = build_int_cst (integer_type_node, 0);
  tree int_1 = build_int_cst (integer_type_node, 1);
  tree int_256 = build_int_cst (integer_type_node, 256);
  tree int_257 = build_int_cst (integer_type_node, 257);
  tree i = build_global_decl ("i", integer_type_node);

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
    = mgr.get_or_create_unknown_svalue (size_type_node);
  const region *reg
    = model.get_or_create_region_for_heap_alloc (size_in_bytes, NULL);
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
    region_offset offset = i_reg->get_offset (&mgr);
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
    region_offset offset = arr_0_reg->get_offset (&mgr);
    ASSERT_EQ (offset.get_base_region (), model.get_lvalue (arr, NULL));
    ASSERT_EQ (offset.get_bit_offset (), 0);
  }

  /* Verify get_offset for "arr[1]".  */
  {
    const region *arr_1_reg = model.get_lvalue (arr_1, NULL);
    region_offset offset = arr_1_reg->get_offset (&mgr);
    ASSERT_EQ (offset.get_base_region (), model.get_lvalue (arr, NULL));
    ASSERT_EQ (offset.get_bit_offset (), INT_TYPE_SIZE);
  }

  /* Verify get_offset for "arr[i]".  */
  {
    const region *arr_i_reg = model.get_lvalue (arr_i, NULL);
    region_offset offset = arr_i_reg->get_offset (&mgr);
    ASSERT_EQ (offset.get_base_region (), model.get_lvalue (arr, NULL));
    ASSERT_EQ (offset.get_symbolic_byte_offset ()->get_kind (), SK_BINOP);
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
  const region *reg
    = model.get_or_create_region_for_heap_alloc (size_sval, &ctxt);
  const svalue *ptr = mgr.get_ptr_svalue (int_star, reg);
  model.set_value (model.get_lvalue (p, &ctxt), ptr, &ctxt);
  ASSERT_EQ (model.get_capacity (reg), size_sval);
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
  const region *reg = model.create_region_for_alloca (size_sval, &ctxt);
  ASSERT_EQ (reg->get_parent_region (), frame_reg);
  const svalue *ptr = mgr.get_ptr_svalue (int_star, reg);
  model.set_value (model.get_lvalue (p, &ctxt), ptr, &ctxt);
  ASSERT_EQ (model.get_capacity (reg), size_sval);

  /* Verify that the pointers to the alloca region are replaced by
     poisoned values when the frame is popped.  */
  model.pop_frame (NULL, NULL, &ctxt);
  ASSERT_EQ (model.get_rvalue (p, NULL)->get_kind (), SK_POISONED);
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
  test_bits_within_svalue_folding ();
  test_descendent_of_p ();
  test_bit_range_regions ();
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
