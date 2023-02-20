/* Tracking equivalence classes and constraints at a point on an execution path.
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
#include "tree.h"
#include "function.h"
#include "basic-block.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "fold-const.h"
#include "selftest.h"
#include "diagnostic-core.h"
#include "graphviz.h"
#include "analyzer/analyzer.h"
#include "ordered-hash-map.h"
#include "options.h"
#include "cgraph.h"
#include "cfg.h"
#include "digraph.h"
#include "analyzer/supergraph.h"
#include "sbitmap.h"
#include "bitmap.h"
#include "analyzer/analyzer-logging.h"
#include "analyzer/call-string.h"
#include "analyzer/program-point.h"
#include "analyzer/store.h"
#include "analyzer/region-model.h"
#include "analyzer/constraint-manager.h"
#include "analyzer/call-summary.h"
#include "analyzer/analyzer-selftests.h"
#include "tree-pretty-print.h"

#if ENABLE_ANALYZER

namespace ana {

static tristate
compare_constants (tree lhs_const, enum tree_code op, tree rhs_const)
{
  tree comparison
    = fold_binary (op, boolean_type_node, lhs_const, rhs_const);
  if (comparison == boolean_true_node)
    return tristate (tristate::TS_TRUE);
  if (comparison == boolean_false_node)
    return tristate (tristate::TS_FALSE);
  return tristate (tristate::TS_UNKNOWN);
}

/* Return true iff CST is below the maximum value for its type.  */

static bool
can_plus_one_p (tree cst)
{
  gcc_assert (CONSTANT_CLASS_P (cst));
  return tree_int_cst_lt (cst, TYPE_MAX_VALUE (TREE_TYPE (cst)));
}

/* Return (CST + 1).  */

static tree
plus_one (tree cst)
{
  gcc_assert (CONSTANT_CLASS_P (cst));
  gcc_assert (can_plus_one_p (cst));
  tree result = fold_build2 (PLUS_EXPR, TREE_TYPE (cst),
			     cst, integer_one_node);
  gcc_assert (CONSTANT_CLASS_P (result));
  return result;
}

/* Return true iff CST is above the minimum value for its type.  */

static bool
can_minus_one_p (tree cst)
{
  gcc_assert (CONSTANT_CLASS_P (cst));
  return tree_int_cst_lt (TYPE_MIN_VALUE (TREE_TYPE (cst)), cst);
}

/* Return (CST - 1).  */

static tree
minus_one (tree cst)
{
  gcc_assert (CONSTANT_CLASS_P (cst));
  gcc_assert (can_minus_one_p (cst));
  tree result = fold_build2 (MINUS_EXPR, TREE_TYPE (cst),
			     cst, integer_one_node);
  gcc_assert (CONSTANT_CLASS_P (result));
  return result;
}

/* struct bound.  */

/* Ensure that this bound is closed by converting an open bound to a
   closed one.  */

void
bound::ensure_closed (enum bound_kind bound_kind)
{
  if (!m_closed)
    {
      /* Offset by 1 in the appropriate direction.
	 For example, convert 3 < x into 4 <= x,
	 and convert x < 5 into x <= 4.  */
      gcc_assert (CONSTANT_CLASS_P (m_constant));
      m_constant = fold_build2 (bound_kind == BK_UPPER ? MINUS_EXPR : PLUS_EXPR,
				TREE_TYPE (m_constant),
				m_constant, integer_one_node);
      gcc_assert (CONSTANT_CLASS_P (m_constant));
      m_closed = true;
    }
}

/* Get "<=" vs "<" for this bound.  */

const char *
bound::get_relation_as_str () const
{
  if (m_closed)
    return "<=";
  else
    return "<";
}

/* struct range.  */

/* Dump this range to PP, which must support %E for tree.  */

void
range::dump_to_pp (pretty_printer *pp) const
{
  if (m_lower_bound.m_constant)
    {
      if (m_upper_bound.m_constant)
	pp_printf (pp, "%qE %s x %s %qE",
		   m_lower_bound.m_constant,
		   m_lower_bound.get_relation_as_str (),
		   m_upper_bound.get_relation_as_str (),
		   m_upper_bound.m_constant);
      else
	pp_printf (pp, "%qE %s x",
		   m_lower_bound.m_constant,
		   m_lower_bound.get_relation_as_str ());
    }
  else
    {
      if (m_upper_bound.m_constant)
	pp_printf (pp, "x %s %qE",
		   m_upper_bound.get_relation_as_str (),
		   m_upper_bound.m_constant);
      else
	pp_string (pp, "x");
    }
}

/* Dump this range to stderr.  */

DEBUG_FUNCTION void
range::dump () const
{
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  pp_show_color (&pp) = pp_show_color (global_dc->printer);
  pp.buffer->stream = stderr;
  dump_to_pp (&pp);
  pp_newline (&pp);
  pp_flush (&pp);
}

/* Determine if there is only one possible value for this range.
   If so, return the constant; otherwise, return NULL_TREE.  */

tree
range::constrained_to_single_element ()
{
  if (m_lower_bound.m_constant == NULL_TREE
      || m_upper_bound.m_constant == NULL_TREE)
    return NULL_TREE;

  if (!INTEGRAL_TYPE_P (TREE_TYPE (m_lower_bound.m_constant)))
    return NULL_TREE;
  if (!INTEGRAL_TYPE_P (TREE_TYPE (m_upper_bound.m_constant)))
    return NULL_TREE;

  /* Convert any open bounds to closed bounds.  */
  m_lower_bound.ensure_closed (BK_LOWER);
  m_upper_bound.ensure_closed (BK_UPPER);

  // Are they equal?
  tree comparison = fold_binary (EQ_EXPR, boolean_type_node,
				 m_lower_bound.m_constant,
				 m_upper_bound.m_constant);
  if (comparison == boolean_true_node)
    return m_lower_bound.m_constant;
  else
    return NULL_TREE;
}

/* Eval the condition "X OP RHS_CONST" for X within the range.  */

tristate
range::eval_condition (enum tree_code op, tree rhs_const) const
{
  range copy (*this);
  if (tree single_element = copy.constrained_to_single_element ())
    return compare_constants (single_element, op, rhs_const);

  switch (op)
    {
    case EQ_EXPR:
      if (below_lower_bound (rhs_const))
	return tristate (tristate::TS_FALSE);
      if (above_upper_bound (rhs_const))
	return tristate (tristate::TS_FALSE);
      break;

    case LT_EXPR:
    case LE_EXPR:
      /* Qn: "X </<= RHS_CONST".  */
      /* If RHS_CONST > upper bound, then it's true.
	 If RHS_CONST < lower bound, then it's false.
	 Otherwise unknown.  */
      if (above_upper_bound (rhs_const))
	return tristate (tristate::TS_TRUE);
      if (below_lower_bound (rhs_const))
	return tristate (tristate::TS_FALSE);
      break;

    case NE_EXPR:
      /* Qn: "X != RHS_CONST".  */
      /* If RHS_CONST < lower bound, then it's true.
	 If RHS_CONST > upper bound, then it's false.
	 Otherwise unknown.  */
      if (below_lower_bound (rhs_const))
	return tristate (tristate::TS_TRUE);
      if (above_upper_bound (rhs_const))
	return tristate (tristate::TS_TRUE);
      break;

    case GE_EXPR:
    case GT_EXPR:
      /* Qn: "X >=/> RHS_CONST".  */
      if (above_upper_bound (rhs_const))
	return tristate (tristate::TS_FALSE);
      if (below_lower_bound (rhs_const))
	return tristate (tristate::TS_TRUE);
      break;

    default:
      gcc_unreachable ();
      break;
    }
  return tristate (tristate::TS_UNKNOWN);
}

/* Return true if RHS_CONST is below the lower bound of this range.  */

bool
range::below_lower_bound (tree rhs_const) const
{
  if (!m_lower_bound.m_constant)
    return false;

  return compare_constants (rhs_const,
			    m_lower_bound.m_closed ? LT_EXPR : LE_EXPR,
			    m_lower_bound.m_constant).is_true ();
}

/* Return true if RHS_CONST is above the upper bound of this range.  */

bool
range::above_upper_bound (tree rhs_const) const
{
  if (!m_upper_bound.m_constant)
    return false;

  return compare_constants (rhs_const,
			    m_upper_bound.m_closed ? GT_EXPR : GE_EXPR,
			    m_upper_bound.m_constant).is_true ();
}

/* Attempt to add B to the bound of the given kind of this range.
   Return true if feasible; false if infeasible.  */

bool
range::add_bound (bound b, enum bound_kind bound_kind)
{
  b.ensure_closed (bound_kind);

  switch (bound_kind)
    {
    default:
      gcc_unreachable ();
    case BK_LOWER:
      /* Discard redundant bounds.  */
      if (m_lower_bound.m_constant)
	{
	  m_lower_bound.ensure_closed (BK_LOWER);
	  if (tree_int_cst_le (b.m_constant,
			       m_lower_bound.m_constant))
	    return true;
	}
      if (m_upper_bound.m_constant)
	{
	  m_upper_bound.ensure_closed (BK_UPPER);
	  /* Reject B <= V <= UPPER when B > UPPER.  */
	  if (!tree_int_cst_le (b.m_constant,
				m_upper_bound.m_constant))
	    return false;
	}
      m_lower_bound = b;
      break;

    case BK_UPPER:
      /* Discard redundant bounds.  */
      if (m_upper_bound.m_constant)
	{
	  m_upper_bound.ensure_closed (BK_UPPER);
	  if (!tree_int_cst_lt (b.m_constant,
				m_upper_bound.m_constant))
	    return true;
	}
      if (m_lower_bound.m_constant)
	{
	  m_lower_bound.ensure_closed (BK_LOWER);
	  /* Reject LOWER <= V <= B when LOWER > B.  */
	  if (!tree_int_cst_le (m_lower_bound.m_constant,
				b.m_constant))
	    return false;
	}
      m_upper_bound = b;
      break;
    }

  return true;
}

/* Attempt to add (RANGE OP RHS_CONST) as a bound to this range.
   Return true if feasible; false if infeasible.  */

bool
range::add_bound (enum tree_code op, tree rhs_const)
{
  switch (op)
    {
    default:
      return true;
    case LT_EXPR:
      /* "V < RHS_CONST"  */
      return add_bound (bound (rhs_const, false), BK_UPPER);
    case LE_EXPR:
      /* "V <= RHS_CONST"  */
      return add_bound (bound (rhs_const, true), BK_UPPER);
    case GE_EXPR:
      /* "V >= RHS_CONST"  */
      return add_bound (bound (rhs_const, true), BK_LOWER);
    case GT_EXPR:
      /* "V > RHS_CONST"  */
      return add_bound (bound (rhs_const, false), BK_LOWER);
    }
}

/* struct bounded_range.  */

bounded_range::bounded_range (const_tree lower, const_tree upper)
: m_lower (const_cast<tree> (lower)),
  m_upper (const_cast<tree> (upper))
{
  if (lower && upper)
    {
      gcc_assert (TREE_CODE (m_lower) == INTEGER_CST);
      gcc_assert (TREE_CODE (m_upper) == INTEGER_CST);
      /* We should have lower <= upper.  */
      gcc_assert (!tree_int_cst_lt (m_upper, m_lower));
    }
  else
    {
      /* Purely for pending on-stack values, for
	 writing back to.  */
      gcc_assert (m_lower == NULL_TREE);
      gcc_assert (m_lower == NULL_TREE);
    }
}

static void
dump_cst (pretty_printer *pp, tree cst, bool show_types)
{
  gcc_assert (cst);
  if (show_types)
    {
      pp_character (pp, '(');
      dump_generic_node (pp, TREE_TYPE (cst), 0, (dump_flags_t)0, false);
      pp_character (pp, ')');
    }
  dump_generic_node (pp, cst, 0, (dump_flags_t)0, false);
}

/* Dump this object to PP.  */

void
bounded_range::dump_to_pp (pretty_printer *pp, bool show_types) const
{
  if (singleton_p ())
    dump_cst (pp, m_lower, show_types);
  else
    {
      pp_character (pp, '[');
      dump_cst (pp, m_lower, show_types);
      pp_string (pp, ", ");
      dump_cst (pp, m_upper, show_types);
      pp_character (pp, ']');
    }
}

/* Dump this object to stderr.  */

void
bounded_range::dump (bool show_types) const
{
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  pp_show_color (&pp) = pp_show_color (global_dc->printer);
  pp.buffer->stream = stderr;
  dump_to_pp (&pp, show_types);
  pp_newline (&pp);
  pp_flush (&pp);
}

json::object *
bounded_range::to_json () const
{
  json::object *range_obj = new json::object ();
  set_json_attr (range_obj, "lower", m_lower);
  set_json_attr (range_obj, "upper", m_upper);
  return range_obj;
}

/* Subroutine of bounded_range::to_json.  */

void
bounded_range::set_json_attr (json::object *obj, const char *name, tree value)
{
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  pp_printf (&pp, "%E", value);
  obj->set (name, new json::string (pp_formatted_text (&pp)));
}


/* Return true iff CST is within this range.  */

bool
bounded_range::contains_p (tree cst) const
{
  /* Reject if below lower bound.  */
  if (tree_int_cst_lt (cst, m_lower))
    return false;
  /* Reject if above lower bound.  */
  if (tree_int_cst_lt (m_upper, cst))
    return false;
  return true;
}

/* If this range intersects OTHER, return true, writing
   the intersection to *OUT if OUT is non-NULL.
   Return false if they do not intersect.  */

bool
bounded_range::intersects_p (const bounded_range &other,
			     bounded_range *out) const
{
  const tree max_lower
    = (tree_int_cst_le (m_lower, other.m_lower)
       ? other.m_lower : m_lower);
  gcc_assert (TREE_CODE (max_lower) == INTEGER_CST);
  const tree min_upper
    = (tree_int_cst_le (m_upper, other.m_upper)
       ? m_upper : other.m_upper);
  gcc_assert (TREE_CODE (min_upper) == INTEGER_CST);

  if (tree_int_cst_le (max_lower, min_upper))
    {
      if (out)
	*out = bounded_range (max_lower, min_upper);
      return true;
    }
  else
    return false;
}

bool
bounded_range::operator== (const bounded_range &other) const
{
  return (TREE_TYPE (m_lower) == TREE_TYPE (other.m_lower)
	  && TREE_TYPE (m_upper) == TREE_TYPE (other.m_upper)
	  && tree_int_cst_equal (m_lower, other.m_lower)
	  && tree_int_cst_equal (m_upper, other.m_upper));
}

int
bounded_range::cmp (const bounded_range &br1, const bounded_range &br2)
{
  if (int cmp_lower = tree_int_cst_compare (br1.m_lower,
					    br2.m_lower))
    return cmp_lower;
  return tree_int_cst_compare (br1.m_upper, br2.m_upper);
}

/* struct bounded_ranges.  */

/* Construct a bounded_ranges instance from a single range.  */

bounded_ranges::bounded_ranges (const bounded_range &range)
: m_ranges (1)
{
  m_ranges.quick_push (range);
  canonicalize ();
  validate ();
}

/* Construct a bounded_ranges instance from multiple ranges.  */

bounded_ranges::bounded_ranges (const vec<bounded_range> &ranges)
: m_ranges (ranges.length ())
{
  m_ranges.safe_splice (ranges);
  canonicalize ();
  validate ();
}

/* Construct a bounded_ranges instance for values of LHS for which
   (LHS OP RHS_CONST) is true (e.g. "(LHS > 3)".  */

bounded_ranges::bounded_ranges (enum tree_code op, tree rhs_const)
: m_ranges ()
{
  gcc_assert (TREE_CODE (rhs_const) == INTEGER_CST);
  tree type = TREE_TYPE (rhs_const);
  switch (op)
    {
    default:
      gcc_unreachable ();
    case EQ_EXPR:
      m_ranges.safe_push (bounded_range (rhs_const, rhs_const));
      break;

    case GE_EXPR:
      m_ranges.safe_push (bounded_range (rhs_const, TYPE_MAX_VALUE (type)));
      break;

    case LE_EXPR:
      m_ranges.safe_push (bounded_range (TYPE_MIN_VALUE (type), rhs_const));
      break;

    case NE_EXPR:
      if (tree_int_cst_lt (TYPE_MIN_VALUE (type), rhs_const))
	m_ranges.safe_push (bounded_range (TYPE_MIN_VALUE (type),
					   minus_one (rhs_const)));
      if (tree_int_cst_lt (rhs_const, TYPE_MAX_VALUE (type)))
	m_ranges.safe_push (bounded_range (plus_one (rhs_const),
					   TYPE_MAX_VALUE (type)));
      break;
    case GT_EXPR:
      if (tree_int_cst_lt (rhs_const, TYPE_MAX_VALUE (type)))
	m_ranges.safe_push (bounded_range (plus_one (rhs_const),
					   TYPE_MAX_VALUE (type)));
      break;
    case LT_EXPR:
      if (tree_int_cst_lt (TYPE_MIN_VALUE (type), rhs_const))
	m_ranges.safe_push (bounded_range (TYPE_MIN_VALUE (type),
					   minus_one (rhs_const)));
      break;
    }
  canonicalize ();
  validate ();
}

/* Subroutine of ctors for fixing up m_ranges.
   Also, initialize m_hash.  */

void
bounded_ranges::canonicalize ()
{
  /* Sort the ranges.  */
  m_ranges.qsort ([](const void *p1, const void *p2) -> int
		  {
		    const bounded_range &br1 = *(const bounded_range *)p1;
		    const bounded_range &br2 = *(const bounded_range *)p2;
		    return bounded_range::cmp (br1, br2);
		  });

  /* Merge ranges that are touching or overlapping.  */
  for (unsigned i = 1; i < m_ranges.length (); )
    {
      bounded_range *prev = &m_ranges[i - 1];
      const bounded_range *next = &m_ranges[i];
      if (prev->intersects_p (*next, NULL)
	  || (can_plus_one_p (prev->m_upper)
	      && tree_int_cst_equal (plus_one (prev->m_upper),
				     next->m_lower)))
	{
	  prev->m_upper = next->m_upper;
	  m_ranges.ordered_remove (i);
	}
      else
	i++;
    }

  /* Initialize m_hash.  */
  inchash::hash hstate (0);
  for (const auto &iter : m_ranges)
    {
      inchash::add_expr (iter.m_lower, hstate);
      inchash::add_expr (iter.m_upper, hstate);
    }
  m_hash = hstate.end ();
}

/* Assert that this object is valid.  */

void
bounded_ranges::validate () const
{
  /* Skip this in a release build.  */
#if !CHECKING_P
  return;
#endif

  for (unsigned i = 1; i < m_ranges.length (); i++)
    {
      const bounded_range &prev = m_ranges[i - 1];
      const bounded_range &next = m_ranges[i];

      /* Give up if we somehow have incompatible different types.  */
      if (!types_compatible_p (TREE_TYPE (prev.m_upper),
			       TREE_TYPE (next.m_lower)))
	continue;

      /* Verify sorted.  */
      gcc_assert (tree_int_cst_lt (prev.m_upper, next.m_lower));

      gcc_assert (can_plus_one_p (prev.m_upper));
      /* otherwise there's no room for "next".  */

      /* Verify no ranges touch each other.  */
      gcc_assert (tree_int_cst_lt (plus_one (prev.m_upper), next.m_lower));
    }
}

/* bounded_ranges equality operator.  */

bool
bounded_ranges::operator== (const bounded_ranges &other) const
{
  if (m_ranges.length () != other.m_ranges.length ())
    return false;
  for (unsigned i = 0; i < m_ranges.length (); i++)
    {
      if (m_ranges[i] != other.m_ranges[i])
	return false;
    }
  return true;
}

/* Dump this object to PP.  */

void
bounded_ranges::dump_to_pp (pretty_printer *pp, bool show_types) const
{
  pp_character (pp, '{');
  for (unsigned i = 0; i < m_ranges.length (); ++i)
    {
      if (i > 0)
	pp_string (pp, ", ");
      m_ranges[i].dump_to_pp (pp, show_types);
    }
  pp_character (pp, '}');
}

/* Dump this object to stderr.  */

DEBUG_FUNCTION void
bounded_ranges::dump (bool show_types) const
{
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  pp_show_color (&pp) = pp_show_color (global_dc->printer);
  pp.buffer->stream = stderr;
  dump_to_pp (&pp, show_types);
  pp_newline (&pp);
  pp_flush (&pp);
}

json::value *
bounded_ranges::to_json () const
{
  json::array *arr_obj = new json::array ();

  for (unsigned i = 0; i < m_ranges.length (); ++i)
    arr_obj->append (m_ranges[i].to_json ());

  return arr_obj;
}

/* Determine whether (X OP RHS_CONST) is known to be true or false
   for all X in the ranges expressed by this object.  */

tristate
bounded_ranges::eval_condition (enum tree_code op,
				tree rhs_const,
				bounded_ranges_manager *mgr) const
{
  /* Convert (X OP RHS_CONST) to a bounded_ranges instance and find
     the intersection of that with this object.  */
  bounded_ranges other (op, rhs_const);
  const bounded_ranges *intersection
    = mgr->get_or_create_intersection (this, &other);

  if (intersection->m_ranges.length () > 0)
    {
      /* We can use pointer equality to check for equality,
	 due to instance consolidation.  */
      if (intersection == this)
	return tristate (tristate::TS_TRUE);
      else
	return tristate (tristate::TS_UNKNOWN);
    }
  else
    /* No intersection.  */
    return tristate (tristate::TS_FALSE);
}

/* Return true if CST is within any of the ranges.  */

bool
bounded_ranges::contain_p (tree cst) const
{
  gcc_assert (TREE_CODE (cst) == INTEGER_CST);
  for (const auto &iter : m_ranges)
    {
      /* TODO: should we optimize this based on sorting?  */
      if (iter.contains_p (cst))
	return true;
    }
  return false;
}

int
bounded_ranges::cmp (const bounded_ranges *a, const bounded_ranges *b)
{
  if (int cmp_length = ((int)a->m_ranges.length ()
			- (int)b->m_ranges.length ()))
    return cmp_length;
  for (unsigned i = 0; i < a->m_ranges.length (); i++)
    {
      if (int cmp_range = bounded_range::cmp (a->m_ranges[i], b->m_ranges[i]))
	return cmp_range;
    }
  /* They are equal.  They ought to have been consolidated, so we should
     have two pointers to the same object.  */
  gcc_assert (a == b);
  return 0;
}

/* class bounded_ranges_manager.  */

/* bounded_ranges_manager's dtor.  */

bounded_ranges_manager::~bounded_ranges_manager ()
{
  /* Delete the managed objects.  */
  for (const auto &iter : m_map)
    delete iter.second;
}

/* Get the bounded_ranges instance for the empty set,  creating it if
   necessary.  */

const bounded_ranges *
bounded_ranges_manager::get_or_create_empty ()
{
  auto_vec<bounded_range> empty_vec;

  return consolidate (new bounded_ranges (empty_vec));
}

/* Get the bounded_ranges instance for {CST}, creating it if necessary.  */

const bounded_ranges *
bounded_ranges_manager::get_or_create_point (const_tree cst)
{
  gcc_assert (TREE_CODE (cst) == INTEGER_CST);

  return get_or_create_range (cst, cst);
}

/* Get the bounded_ranges instance for {[LOWER_BOUND..UPPER_BOUND]},
   creating it if necessary.  */

const bounded_ranges *
bounded_ranges_manager::get_or_create_range (const_tree lower_bound,
					     const_tree upper_bound)
{
  gcc_assert (TREE_CODE (lower_bound) == INTEGER_CST);
  gcc_assert (TREE_CODE (upper_bound) == INTEGER_CST);

  return consolidate
    (new bounded_ranges (bounded_range (lower_bound, upper_bound)));
}

/* Get the bounded_ranges instance for the union of OTHERS,
   creating it if necessary.  */

const bounded_ranges *
bounded_ranges_manager::
get_or_create_union (const vec <const bounded_ranges *> &others)
{
  auto_vec<bounded_range> ranges;
  for (const auto &r : others)
    ranges.safe_splice (r->m_ranges);
  return consolidate (new bounded_ranges (ranges));
}

/* Get the bounded_ranges instance for the intersection of A and B,
   creating it if necessary.  */

const bounded_ranges *
bounded_ranges_manager::get_or_create_intersection (const bounded_ranges *a,
						    const bounded_ranges *b)
{
  auto_vec<bounded_range> ranges;
  unsigned a_idx = 0;
  unsigned b_idx = 0;
  while (a_idx < a->m_ranges.length ()
	 && b_idx < b->m_ranges.length ())
    {
      const bounded_range &r_a = a->m_ranges[a_idx];
      const bounded_range &r_b = b->m_ranges[b_idx];

      bounded_range intersection (NULL_TREE, NULL_TREE);
      if (r_a.intersects_p (r_b, &intersection))
	{
	  ranges.safe_push (intersection);
	}
      if (tree_int_cst_lt (r_a.m_lower, r_b.m_lower))
	{
	  a_idx++;
	}
      else
	{
	  if (tree_int_cst_lt (r_a.m_upper, r_b.m_upper))
	    a_idx++;
	  else
	    b_idx++;
	}
    }

  return consolidate (new bounded_ranges (ranges));
}

/* Get the bounded_ranges instance for the inverse of OTHER relative
   to TYPE, creating it if necessary.
   This is for use when handling "default" in switch statements, where
   OTHER represents all the other cases.  */

const bounded_ranges *
bounded_ranges_manager::get_or_create_inverse (const bounded_ranges *other,
					       tree type)
{
  tree min_val = TYPE_MIN_VALUE (type);
  tree max_val = TYPE_MAX_VALUE (type);
  if (other->m_ranges.length () == 0)
    return get_or_create_range (min_val, max_val);
  auto_vec<bounded_range> ranges;
  tree first_lb = other->m_ranges[0].m_lower;
  if (tree_int_cst_lt (min_val, first_lb)
      && can_minus_one_p (first_lb))
    ranges.safe_push (bounded_range (min_val,
				     minus_one (first_lb)));
  for (unsigned i = 1; i < other->m_ranges.length (); i++)
    {
      tree prev_ub = other->m_ranges[i - 1].m_upper;
      tree iter_lb = other->m_ranges[i].m_lower;
      gcc_assert (tree_int_cst_lt (prev_ub, iter_lb));
      if (can_plus_one_p (prev_ub) && can_minus_one_p (iter_lb))
	ranges.safe_push (bounded_range (plus_one (prev_ub),
					 minus_one (iter_lb)));
    }
  tree last_ub
    = other->m_ranges[other->m_ranges.length () - 1].m_upper;
  if (tree_int_cst_lt (last_ub, max_val)
      && can_plus_one_p (last_ub))
    ranges.safe_push (bounded_range (plus_one (last_ub), max_val));

  return consolidate (new bounded_ranges (ranges));
}

/* If an object equal to INST is already present, delete INST and
   return the existing object.
   Otherwise add INST and return it.  */

const bounded_ranges *
bounded_ranges_manager::consolidate (bounded_ranges *inst)
{
  if (bounded_ranges **slot = m_map.get (inst))
    {
      delete inst;
      return *slot;
    }
  m_map.put (inst, inst);
  return inst;
}

/* Get the bounded_ranges instance for EDGE of SWITCH_STMT,
   creating it if necessary, and caching it by edge.  */

const bounded_ranges *
bounded_ranges_manager::
get_or_create_ranges_for_switch (const switch_cfg_superedge *edge,
				 const gswitch *switch_stmt)
{
  /* Look in per-edge cache.  */
  if (const bounded_ranges ** slot = m_edge_cache.get (edge))
    return *slot;

  /* Not yet in cache.  */
  const bounded_ranges *all_cases_ranges
    = create_ranges_for_switch (*edge, switch_stmt);
  m_edge_cache.put (edge, all_cases_ranges);
  return all_cases_ranges;
}

/* Get the bounded_ranges instance for EDGE of SWITCH_STMT,
   creating it if necessary, for edges for which the per-edge
   cache has not yet been populated.  */

const bounded_ranges *
bounded_ranges_manager::
create_ranges_for_switch (const switch_cfg_superedge &edge,
			  const gswitch *switch_stmt)
{
  /* Get the ranges for each case label.  */
  auto_vec <const bounded_ranges *> case_ranges_vec
    (gimple_switch_num_labels (switch_stmt));

  for (tree case_label : edge.get_case_labels ())
    {
      /* Get the ranges for this case label.  */
      const bounded_ranges *case_ranges
	= make_case_label_ranges (switch_stmt, case_label);
      case_ranges_vec.quick_push (case_ranges);
    }

  /* Combine all the ranges for each case label into a single collection
     of ranges.  */
  const bounded_ranges *all_cases_ranges
    = get_or_create_union (case_ranges_vec);
  return all_cases_ranges;
}

/* Get the bounded_ranges instance for CASE_LABEL within
   SWITCH_STMT.  */

const bounded_ranges *
bounded_ranges_manager::
make_case_label_ranges (const gswitch *switch_stmt,
			tree case_label)
{
  gcc_assert (TREE_CODE (case_label) == CASE_LABEL_EXPR);
  tree lower_bound = CASE_LOW (case_label);
  tree upper_bound = CASE_HIGH (case_label);
  if (lower_bound)
    {
      if (upper_bound)
	/* Range.  */
	return get_or_create_range (lower_bound, upper_bound);
      else
	/* Single-value.  */
	return get_or_create_point (lower_bound);
    }
  else
    {
      /* The default case.
	 Add exclusions based on the other cases.  */
      auto_vec <const bounded_ranges *> other_case_ranges
	(gimple_switch_num_labels (switch_stmt));
      for (unsigned other_idx = 1;
	   other_idx < gimple_switch_num_labels (switch_stmt);
	   other_idx++)
	{
	  tree other_label = gimple_switch_label (switch_stmt,
						  other_idx);
	  const bounded_ranges *other_ranges
	    = make_case_label_ranges (switch_stmt, other_label);
	  other_case_ranges.quick_push (other_ranges);
	}
      const bounded_ranges *other_cases_ranges
	= get_or_create_union (other_case_ranges);
      tree type = TREE_TYPE (gimple_switch_index (switch_stmt));
      return get_or_create_inverse (other_cases_ranges, type);
    }
}

/* Dump the number of objects of each class that were managed by this
   manager to LOGGER.
   If SHOW_OBJS is true, also dump the objects themselves.  */

void
bounded_ranges_manager::log_stats (logger *logger, bool show_objs) const
{
  LOG_SCOPE (logger);
  logger->log ("  # %s: %li", "ranges", (long)m_map.elements ());
  if (!show_objs)
    return;

  auto_vec<const bounded_ranges *> vec_objs (m_map.elements ());
  for (const auto &iter : m_map)
    vec_objs.quick_push (iter.second);
  vec_objs.qsort
    ([](const void *p1, const void *p2) -> int
     {
       const bounded_ranges *br1 = *(const bounded_ranges * const *)p1;
       const bounded_ranges *br2 = *(const bounded_ranges * const *)p2;
       return bounded_ranges::cmp (br1, br2);
     });

  for (const auto &iter : vec_objs)
    {
      logger->start_log_line ();
      pretty_printer *pp = logger->get_printer ();
      pp_string (pp, "    ");
      iter->dump_to_pp (pp, true);
      logger->end_log_line ();
    }
}

/* class equiv_class.  */

/* equiv_class's default ctor.  */

equiv_class::equiv_class ()
: m_constant (NULL_TREE), m_cst_sval (NULL), m_vars ()
{
}

/* equiv_class's copy ctor.  */

equiv_class::equiv_class (const equiv_class &other)
: m_constant (other.m_constant), m_cst_sval (other.m_cst_sval),
  m_vars (other.m_vars.length ())
{
  for (const svalue *sval : other.m_vars)
    m_vars.quick_push (sval);
}

/* Print an all-on-one-line representation of this equiv_class to PP,
   which must support %E for trees.  */

void
equiv_class::print (pretty_printer *pp) const
{
  pp_character (pp, '{');
  int i;
  const svalue *sval;
  FOR_EACH_VEC_ELT (m_vars, i, sval)
    {
      if (i > 0)
	pp_string (pp, " == ");
      sval->dump_to_pp (pp, true);
    }
  if (m_constant)
    {
      if (i > 0)
	pp_string (pp, " == ");
      pp_printf (pp, "[m_constant]%qE", m_constant);
    }
  pp_character (pp, '}');
}

/* Return a new json::object of the form
   {"svals" : [str],
    "constant" : optional str}.  */

json::object *
equiv_class::to_json () const
{
  json::object *ec_obj = new json::object ();

  json::array *sval_arr = new json::array ();
  for (const svalue *sval : m_vars)
    sval_arr->append (sval->to_json ());
  ec_obj->set ("svals", sval_arr);

  if (m_constant)
    {
      pretty_printer pp;
      pp_format_decoder (&pp) = default_tree_printer;
      pp_printf (&pp, "%qE", m_constant);
      ec_obj->set ("constant", new json::string (pp_formatted_text (&pp)));
    }

  return ec_obj;
}

/* Generate a hash value for this equiv_class.
   This relies on the ordering of m_vars, and so this object needs to
   have been canonicalized for this to be meaningful.  */

hashval_t
equiv_class::hash () const
{
  inchash::hash hstate;

  inchash::add_expr (m_constant, hstate);
  for (const svalue * sval : m_vars)
    hstate.add_ptr (sval);
  return hstate.end ();
}

/* Equality operator for equiv_class.
   This relies on the ordering of m_vars, and so this object
   and OTHER need to have been canonicalized for this to be
   meaningful.  */

bool
equiv_class::operator== (const equiv_class &other)
{
  if (m_constant != other.m_constant)
    return false; // TODO: use tree equality here?

  /* FIXME: should we compare m_cst_sval?  */

  if (m_vars.length () != other.m_vars.length ())
    return false;

  int i;
  const svalue *sval;
  FOR_EACH_VEC_ELT (m_vars, i, sval)
    if (sval != other.m_vars[i])
      return false;

  return true;
}

/* Add SID to this equiv_class, using CM to check if it's a constant.  */

void
equiv_class::add (const svalue *sval)
{
  gcc_assert (sval);
  if (tree cst = sval->maybe_get_constant ())
    {
      gcc_assert (CONSTANT_CLASS_P (cst));
      /* FIXME: should we canonicalize which svalue is the constant
	 when there are multiple equal constants?  */
      m_constant = cst;
      m_cst_sval = sval;
    }
  m_vars.safe_push (sval);
}

/* Remove SID from this equivalence class.
   Return true if SID was the last var in the equivalence class (suggesting
   a possible leak).  */

bool
equiv_class::del (const svalue *sval)
{
  gcc_assert (sval);
  gcc_assert (sval != m_cst_sval);

  int i;
  const svalue *iv;
  FOR_EACH_VEC_ELT (m_vars, i, iv)
    {
      if (iv == sval)
	{
	  m_vars[i] = m_vars[m_vars.length () - 1];
	  m_vars.pop ();
	  return m_vars.length () == 0;
	}
    }

  /* SVAL must be in the class.  */
  gcc_unreachable ();
  return false;
}

/* Get a representative member of this class, for handling cases
   where the IDs can change mid-traversal.  */

const svalue *
equiv_class::get_representative () const
{
  gcc_assert (m_vars.length () > 0);
  return m_vars[0];
}

/* Sort the svalues within this equiv_class.  */

void
equiv_class::canonicalize ()
{
  m_vars.qsort (svalue::cmp_ptr_ptr);
}

/* Return true if this EC contains a variable, false if it merely
   contains constants.
   Subroutine of constraint_manager::canonicalize, for removing
   redundant ECs.  */

bool
equiv_class::contains_non_constant_p () const
{
  if (m_constant)
    {
      for (auto iter : m_vars)
	if (iter->maybe_get_constant ())
	  continue;
	else
	  /* We have {non-constant == constant}.  */
	  return true;
      /* We only have constants.  */
      return false;
    }
  else
    /* Return true if we have {non-constant == non-constant}.  */
    return m_vars.length () > 1;
}

/* Get a debug string for C_OP.  */

const char *
constraint_op_code (enum constraint_op c_op)
{
  switch (c_op)
    {
    default:
      gcc_unreachable ();
    case CONSTRAINT_NE: return "!=";
    case CONSTRAINT_LT: return "<";
    case CONSTRAINT_LE: return "<=";
    }
}

/* Convert C_OP to an enum tree_code.  */

enum tree_code
constraint_tree_code (enum constraint_op c_op)
{
  switch (c_op)
    {
    default:
      gcc_unreachable ();
    case CONSTRAINT_NE: return NE_EXPR;
    case CONSTRAINT_LT: return LT_EXPR;
    case CONSTRAINT_LE: return LE_EXPR;
    }
}

/* Given "lhs C_OP rhs", determine "lhs T_OP rhs".

   For example, given "x < y", then "x > y" is false.  */

static tristate
eval_constraint_op_for_op (enum constraint_op c_op, enum tree_code t_op)
{
  switch (c_op)
    {
    default:
      gcc_unreachable ();
    case CONSTRAINT_NE:
      if (t_op == EQ_EXPR)
	return tristate (tristate::TS_FALSE);
      if (t_op == NE_EXPR)
	return tristate (tristate::TS_TRUE);
      break;
    case CONSTRAINT_LT:
      if (t_op == LT_EXPR || t_op == LE_EXPR || t_op == NE_EXPR)
	return tristate (tristate::TS_TRUE);
      if (t_op == EQ_EXPR || t_op == GT_EXPR || t_op == GE_EXPR)
	return tristate (tristate::TS_FALSE);
      break;
    case CONSTRAINT_LE:
      if (t_op == LE_EXPR)
	return tristate (tristate::TS_TRUE);
      if (t_op == GT_EXPR)
	return tristate (tristate::TS_FALSE);
      break;
    }
  return tristate (tristate::TS_UNKNOWN);
}

/* class constraint.  */

/* Print this constraint to PP (which must support %E for trees),
   using CM to look up equiv_class instances from ids.  */

void
constraint::print (pretty_printer *pp, const constraint_manager &cm) const
{
  m_lhs.print (pp);
  pp_string (pp, ": ");
  m_lhs.get_obj (cm).print (pp);
  pp_string (pp, " ");
  pp_string (pp, constraint_op_code (m_op));
  pp_string (pp, " ");
  m_rhs.print (pp);
  pp_string (pp, ": ");
  m_rhs.get_obj (cm).print (pp);
}

/* Return a new json::object of the form
   {"lhs" : int, the EC index
    "op"  : str,
    "rhs" : int, the EC index}.  */

json::object *
constraint::to_json () const
{
  json::object *con_obj = new json::object ();

  con_obj->set ("lhs", new json::integer_number (m_lhs.as_int ()));
  con_obj->set ("op", new json::string (constraint_op_code (m_op)));
  con_obj->set ("rhs", new json::integer_number (m_rhs.as_int ()));

  return con_obj;
}

/* Generate a hash value for this constraint.  */

hashval_t
constraint::hash () const
{
  inchash::hash hstate;
  hstate.add_int (m_lhs.m_idx);
  hstate.add_int (m_op);
  hstate.add_int (m_rhs.m_idx);
  return hstate.end ();
}

/* Equality operator for constraints.  */

bool
constraint::operator== (const constraint &other) const
{
  if (m_lhs != other.m_lhs)
    return false;
  if (m_op != other.m_op)
    return false;
  if (m_rhs != other.m_rhs)
    return false;
  return true;
}

/* Return true if this constraint is implied by OTHER.  */

bool
constraint::implied_by (const constraint &other,
			 const constraint_manager &cm) const
{
  if (m_lhs == other.m_lhs)
    if (tree rhs_const = m_rhs.get_obj (cm).get_any_constant ())
      if (tree other_rhs_const = other.m_rhs.get_obj (cm).get_any_constant ())
	if (m_lhs.get_obj (cm).get_any_constant () == NULL_TREE)
	  if (m_op == other.m_op)
	    switch (m_op)
	      {
	      default:
		break;
	      case CONSTRAINT_LE:
	      case CONSTRAINT_LT:
		if (compare_constants (rhs_const,
				       GE_EXPR,
				       other_rhs_const).is_true ())
		  return true;
		break;
	      }
  return false;
}

/* class bounded_ranges_constraint.  */

void
bounded_ranges_constraint::print (pretty_printer *pp,
				  const constraint_manager &cm) const
{
  m_ec_id.print (pp);
  pp_string (pp, ": ");
  m_ec_id.get_obj (cm).print (pp);
  pp_string (pp, ": ");
  m_ranges->dump_to_pp (pp, true);
}

json::object *
bounded_ranges_constraint::to_json () const
{
  json::object *con_obj = new json::object ();

  con_obj->set ("ec", new json::integer_number (m_ec_id.as_int ()));
  con_obj->set ("ranges", m_ranges->to_json ());

  return con_obj;
}

bool
bounded_ranges_constraint::
operator== (const bounded_ranges_constraint &other) const
{
  if (m_ec_id != other.m_ec_id)
    return false;

  /* We can compare by pointer, since the bounded_ranges_manager
     consolidates instances.  */
  return m_ranges == other.m_ranges;
}

void
bounded_ranges_constraint::add_to_hash (inchash::hash *hstate) const
{
  hstate->add_int (m_ec_id.m_idx);
  hstate->merge_hash (m_ranges->get_hash ());
}

/* class equiv_class_id.  */

/* Get the underlying equiv_class for this ID from CM.  */

const equiv_class &
equiv_class_id::get_obj (const constraint_manager &cm) const
{
  return cm.get_equiv_class_by_index (m_idx);
}

/* Access the underlying equiv_class for this ID from CM.  */

equiv_class &
equiv_class_id::get_obj (constraint_manager &cm) const
{
  return cm.get_equiv_class_by_index (m_idx);
}

/* Print this equiv_class_id to PP.  */

void
equiv_class_id::print (pretty_printer *pp) const
{
  if (null_p ())
    pp_printf (pp, "null");
  else
    pp_printf (pp, "ec%i", m_idx);
}

/* class constraint_manager.  */

/* constraint_manager's copy ctor.  */

constraint_manager::constraint_manager (const constraint_manager &other)
: m_equiv_classes (other.m_equiv_classes.length ()),
  m_constraints (other.m_constraints.length ()),
  m_bounded_ranges_constraints (other.m_bounded_ranges_constraints.length ()),
  m_mgr (other.m_mgr)
{
  int i;
  equiv_class *ec;
  FOR_EACH_VEC_ELT (other.m_equiv_classes, i, ec)
    m_equiv_classes.quick_push (new equiv_class (*ec));
  constraint *c;
  FOR_EACH_VEC_ELT (other.m_constraints, i, c)
    m_constraints.quick_push (*c);
  for (const auto &iter : other.m_bounded_ranges_constraints)
    m_bounded_ranges_constraints.quick_push (iter);
}

/* constraint_manager's assignment operator.  */

constraint_manager&
constraint_manager::operator= (const constraint_manager &other)
{
  gcc_assert (m_equiv_classes.length () == 0);
  gcc_assert (m_constraints.length () == 0);
  gcc_assert (m_bounded_ranges_constraints.length () == 0);

  int i;
  equiv_class *ec;
  m_equiv_classes.reserve (other.m_equiv_classes.length ());
  FOR_EACH_VEC_ELT (other.m_equiv_classes, i, ec)
    m_equiv_classes.quick_push (new equiv_class (*ec));
  constraint *c;
  m_constraints.reserve (other.m_constraints.length ());
  FOR_EACH_VEC_ELT (other.m_constraints, i, c)
    m_constraints.quick_push (*c);
  for (const auto &iter : other.m_bounded_ranges_constraints)
    m_bounded_ranges_constraints.quick_push (iter);

  return *this;
}

/* Generate a hash value for this constraint_manager.  */

hashval_t
constraint_manager::hash () const
{
  inchash::hash hstate;
  int i;
  equiv_class *ec;
  constraint *c;

  FOR_EACH_VEC_ELT (m_equiv_classes, i, ec)
    hstate.merge_hash (ec->hash ());
  FOR_EACH_VEC_ELT (m_constraints, i, c)
    hstate.merge_hash (c->hash ());
  for (const auto &iter : m_bounded_ranges_constraints)
    iter.add_to_hash (&hstate);
  return hstate.end ();
}

/* Equality operator for constraint_manager.  */

bool
constraint_manager::operator== (const constraint_manager &other) const
{
  if (m_equiv_classes.length () != other.m_equiv_classes.length ())
    return false;
  if (m_constraints.length () != other.m_constraints.length ())
    return false;
  if (m_bounded_ranges_constraints.length ()
      != other.m_bounded_ranges_constraints.length ())
    return false;

  int i;
  equiv_class *ec;

  FOR_EACH_VEC_ELT (m_equiv_classes, i, ec)
    if (!(*ec == *other.m_equiv_classes[i]))
      return false;

  constraint *c;

  FOR_EACH_VEC_ELT (m_constraints, i, c)
    if (!(*c == other.m_constraints[i]))
      return false;

  for (unsigned i = 0; i < m_bounded_ranges_constraints.length (); i++)
    {
      if (m_bounded_ranges_constraints[i]
	  != other.m_bounded_ranges_constraints[i])
	return false;
    }

  return true;
}

/* Print this constraint_manager to PP (which must support %E for trees).  */

void
constraint_manager::print (pretty_printer *pp) const
{
  pp_string (pp, "{");
  int i;
  equiv_class *ec;
  FOR_EACH_VEC_ELT (m_equiv_classes, i, ec)
    {
      if (i > 0)
	pp_string (pp, ", ");
      equiv_class_id (i).print (pp);
      pp_string (pp, ": ");
      ec->print (pp);
    }
  pp_string (pp, "  |  ");
  constraint *c;
  FOR_EACH_VEC_ELT (m_constraints, i, c)
    {
      if (i > 0)
	pp_string (pp, " && ");
      c->print (pp, *this);
    }
  if (m_bounded_ranges_constraints.length ())
    {
      pp_string (pp, "  |  ");
      i = 0;
      for (const auto &iter : m_bounded_ranges_constraints)
	{
	  if (i > 0)
	    pp_string (pp, " && ");
	  iter.print (pp, *this);
	  i++;
	}
    }
  pp_printf (pp, "}");
}

/* Dump a representation of this constraint_manager to PP
   (which must support %E for trees).  */

void
constraint_manager::dump_to_pp (pretty_printer *pp, bool multiline) const
{
  if (multiline)
    pp_string (pp, "  ");
  pp_string (pp, "equiv classes:");
  if (multiline)
    pp_newline (pp);
  else
    pp_string (pp, " {");
  int i;
  equiv_class *ec;
  FOR_EACH_VEC_ELT (m_equiv_classes, i, ec)
    {
      if (multiline)
	pp_string (pp, "    ");
      else if (i > 0)
	pp_string (pp, ", ");
      equiv_class_id (i).print (pp);
      pp_string (pp, ": ");
      ec->print (pp);
      if (multiline)
	pp_newline (pp);
    }
  if (multiline)
    pp_string (pp, "  ");
  else
    pp_string (pp, "}");
  pp_string (pp, "constraints:");
  if (multiline)
    pp_newline (pp);
  else
    pp_string (pp, "{");
  constraint *c;
  FOR_EACH_VEC_ELT (m_constraints, i, c)
    {
      if (multiline)
	pp_string (pp, "    ");
      pp_printf (pp, "%i: ", i);
      c->print (pp, *this);
      if (multiline)
	pp_newline (pp);
    }
  if (!multiline)
    pp_string (pp, "}");
  if (m_bounded_ranges_constraints.length ())
    {
      if (multiline)
	pp_string (pp, "  ");
      pp_string (pp, "ranges:");
      if (multiline)
	pp_newline (pp);
      else
	pp_string (pp, "{");
      i = 0;
      for (const auto &iter : m_bounded_ranges_constraints)
	{
	  if (multiline)
	    pp_string (pp, "    ");
	  else if (i > 0)
	    pp_string (pp, " && ");
	  iter.print (pp, *this);
	  if (multiline)
	    pp_newline (pp);
	  i++;
	}
      if (!multiline)
	pp_string (pp, "}");
      }
}

/* Dump a multiline representation of this constraint_manager to FP.  */

void
constraint_manager::dump (FILE *fp) const
{
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  pp_show_color (&pp) = pp_show_color (global_dc->printer);
  pp.buffer->stream = fp;
  dump_to_pp (&pp, true);
  pp_flush (&pp);
}

/* Dump a multiline representation of this constraint_manager to stderr.  */

DEBUG_FUNCTION void
constraint_manager::dump () const
{
  dump (stderr);
}

/* Dump a multiline representation of CM to stderr.  */

DEBUG_FUNCTION void
debug (const constraint_manager &cm)
{
  cm.dump ();
}

/* Return a new json::object of the form
   {"ecs" : array of objects, one per equiv_class
    "constraints" : array of objects, one per constraint}.  */

json::object *
constraint_manager::to_json () const
{
  json::object *cm_obj = new json::object ();

  /* Equivalence classes.  */
  {
    json::array *ec_arr = new json::array ();
    for (const equiv_class *ec : m_equiv_classes)
      ec_arr->append (ec->to_json ());
    cm_obj->set ("ecs", ec_arr);
  }

  /* Constraints.  */
  {
    json::array *con_arr = new json::array ();
    for (const constraint &c : m_constraints)
      con_arr->append (c.to_json ());
    cm_obj->set ("constraints", con_arr);
  }

  /* m_bounded_ranges_constraints.  */
  {
    json::array *con_arr = new json::array ();
    for (const auto &c : m_bounded_ranges_constraints)
      con_arr->append (c.to_json ());
    cm_obj->set ("bounded_ranges_constraints", con_arr);
  }

  return cm_obj;
}

/* Attempt to add the constraint LHS OP RHS to this constraint_manager.
   Return true if the constraint could be added (or is already true).
   Return false if the constraint contradicts existing knowledge.  */

bool
constraint_manager::add_constraint (const svalue *lhs,
				     enum tree_code op,
				     const svalue *rhs)
{
  lhs = lhs->unwrap_any_unmergeable ();
  rhs = rhs->unwrap_any_unmergeable ();

  /* Nothing can be known about unknown/poisoned values.  */
  if (!lhs->can_have_associated_state_p ()
      || !rhs->can_have_associated_state_p ())
    /* Not a contradiction.  */
    return true;

  /* Check the conditions on svalues.  */
  {
    tristate t_cond = eval_condition (lhs, op, rhs);

    /* If we already have the condition, do nothing.  */
    if (t_cond.is_true ())
      return true;

    /* Reject a constraint that would contradict existing knowledge, as
       unsatisfiable.  */
    if (t_cond.is_false ())
      return false;
  }

  equiv_class_id lhs_ec_id = get_or_add_equiv_class (lhs);
  equiv_class_id rhs_ec_id = get_or_add_equiv_class (rhs);

  /* Check the stronger conditions on ECs.  */
  {
    tristate t = eval_condition (lhs_ec_id, op, rhs_ec_id);

    /* Discard constraints that are already known.  */
    if (t.is_true ())
      return true;

    /* Reject unsatisfiable constraints.  */
    if (t.is_false ())
      return false;
  }

  /* If adding
       (SVAL + OFFSET) > CST,
     then that can imply:
       SVAL > (CST - OFFSET).  */
  if (const binop_svalue *lhs_binop = lhs->dyn_cast_binop_svalue ())
    if (tree rhs_cst = rhs->maybe_get_constant ())
      if (tree offset = lhs_binop->get_arg1 ()->maybe_get_constant ())
	if ((op == GT_EXPR || op == LT_EXPR
	     || op == GE_EXPR || op == LE_EXPR)
	    && lhs_binop->get_op () == PLUS_EXPR)
	  {
	    tree offset_of_cst = fold_build2 (MINUS_EXPR, TREE_TYPE (rhs_cst),
					      rhs_cst, offset);
	    const svalue *implied_lhs = lhs_binop->get_arg0 ();
	    enum tree_code implied_op = op;
	    const svalue *implied_rhs
	      = m_mgr->get_or_create_constant_svalue (offset_of_cst);
	    if (!add_constraint (implied_lhs, implied_op, implied_rhs))
	      return false;
	    /* The above add_constraint could lead to EC merger, so we need
	       to refresh the EC IDs.  */
	    lhs_ec_id = get_or_add_equiv_class (lhs);
	    rhs_ec_id = get_or_add_equiv_class (rhs);
	  }

  add_unknown_constraint (lhs_ec_id, op, rhs_ec_id);
  return true;
}

/* Attempt to add the constraint LHS_EC_ID OP RHS_EC_ID to this
   constraint_manager.
   Return true if the constraint could be added (or is already true).
   Return false if the constraint contradicts existing knowledge.  */

bool
constraint_manager::add_constraint (equiv_class_id lhs_ec_id,
				     enum tree_code op,
				     equiv_class_id rhs_ec_id)
{
  tristate t = eval_condition (lhs_ec_id, op, rhs_ec_id);

  /* Discard constraints that are already known.  */
  if (t.is_true ())
    return true;

  /* Reject unsatisfiable constraints.  */
  if (t.is_false ())
    return false;

  add_unknown_constraint (lhs_ec_id, op, rhs_ec_id);
  return true;
}

/* Add the constraint LHS_EC_ID OP RHS_EC_ID to this constraint_manager,
   where the constraint has already been checked for being "unknown".  */

void
constraint_manager::add_unknown_constraint (equiv_class_id lhs_ec_id,
					     enum tree_code op,
					     equiv_class_id rhs_ec_id)
{
  gcc_assert (lhs_ec_id != rhs_ec_id);

  /* For now, simply accumulate constraints, without attempting any further
     optimization.  */
  switch (op)
    {
    case EQ_EXPR:
      {
	/* Merge rhs_ec into lhs_ec.  */
	equiv_class &lhs_ec_obj = lhs_ec_id.get_obj (*this);
	const equiv_class &rhs_ec_obj = rhs_ec_id.get_obj (*this);

	int i;
	const svalue *sval;
	FOR_EACH_VEC_ELT (rhs_ec_obj.m_vars, i, sval)
	  lhs_ec_obj.add (sval);

	if (rhs_ec_obj.m_constant)
	  {
	    lhs_ec_obj.m_constant = rhs_ec_obj.m_constant;
	    lhs_ec_obj.m_cst_sval = rhs_ec_obj.m_cst_sval;
	  }

	/* Drop rhs equivalence class, overwriting it with the
	   final ec (which might be the same one).  */
	equiv_class_id final_ec_id = m_equiv_classes.length () - 1;
	equiv_class *old_ec = m_equiv_classes[rhs_ec_id.m_idx];
	equiv_class *final_ec = m_equiv_classes.pop ();
	if (final_ec != old_ec)
	  m_equiv_classes[rhs_ec_id.m_idx] = final_ec;
	delete old_ec;
	if (lhs_ec_id == final_ec_id)
	  lhs_ec_id = rhs_ec_id;

	/* Update the constraints.  */
	constraint *c;
	FOR_EACH_VEC_ELT (m_constraints, i, c)
	  {
	    /* Update references to the rhs_ec so that
	       they refer to the lhs_ec.  */
	    if (c->m_lhs == rhs_ec_id)
	      c->m_lhs = lhs_ec_id;
	    if (c->m_rhs == rhs_ec_id)
	      c->m_rhs = lhs_ec_id;

	    /* Renumber all constraints that refer to the final rhs_ec
	       to the old rhs_ec, where the old final_ec now lives.  */
	    if (c->m_lhs == final_ec_id)
	      c->m_lhs = rhs_ec_id;
	    if (c->m_rhs == final_ec_id)
	      c->m_rhs = rhs_ec_id;
	  }
	bounded_ranges_constraint *brc;
	FOR_EACH_VEC_ELT (m_bounded_ranges_constraints, i, brc)
	  {
	    if (brc->m_ec_id == rhs_ec_id)
	      brc->m_ec_id = lhs_ec_id;
	    if (brc->m_ec_id == final_ec_id)
	      brc->m_ec_id = rhs_ec_id;
	  }

	/* We may now have self-comparisons due to the merger; these
	   constraints should be removed.  */
	unsigned read_index, write_index;
	VEC_ORDERED_REMOVE_IF (m_constraints, read_index, write_index, c,
			       (c->m_lhs == c->m_rhs));
      }
      break;
    case GE_EXPR:
      add_constraint_internal (rhs_ec_id, CONSTRAINT_LE, lhs_ec_id);
      break;
    case LE_EXPR:
      add_constraint_internal (lhs_ec_id, CONSTRAINT_LE, rhs_ec_id);
      break;
    case NE_EXPR:
      add_constraint_internal (lhs_ec_id, CONSTRAINT_NE, rhs_ec_id);
      break;
    case GT_EXPR:
      add_constraint_internal (rhs_ec_id, CONSTRAINT_LT, lhs_ec_id);
      break;
    case LT_EXPR:
      add_constraint_internal (lhs_ec_id, CONSTRAINT_LT, rhs_ec_id);
      break;
    default:
      /* do nothing.  */
      break;
    }
  validate ();
}

/* Subroutine of constraint_manager::add_constraint, for handling all
   operations other than equality (for which equiv classes are merged).  */

void
constraint_manager::add_constraint_internal (equiv_class_id lhs_id,
					     enum constraint_op c_op,
					     equiv_class_id rhs_id)
{
  if (m_constraints.length () >= (unsigned)param_analyzer_max_constraints)
    return;

  constraint new_c (lhs_id, c_op, rhs_id);

  /* Remove existing constraints that would be implied by the
     new constraint.  */
  unsigned read_index, write_index;
  constraint *c;
  VEC_ORDERED_REMOVE_IF (m_constraints, read_index, write_index, c,
			 (c->implied_by (new_c, *this)));

  /* Add the constraint.  */
  m_constraints.safe_push (new_c);

  /* We don't yet update m_bounded_ranges_constraints here yet.  */

  if (!flag_analyzer_transitivity)
    return;

  if (c_op != CONSTRAINT_NE)
    {
      /* The following can potentially add EQ_EXPR facts, which could lead
	 to ECs being merged, which would change the meaning of the EC IDs.
	 Hence we need to do this via representatives.  */
      const svalue *lhs = lhs_id.get_obj (*this).get_representative ();
      const svalue *rhs = rhs_id.get_obj (*this).get_representative ();

      /* We have LHS </<= RHS */

      /* Handle transitivity of ordering by adding additional constraints
	 based on what we already knew.

	 So if we have already have:
	   (a < b)
	   (c < d)
	 Then adding:
	   (b < c)
	 will also add:
	   (a < c)
	   (b < d)
	 We need to recurse to ensure we also add:
	   (a < d).
	 We call the checked add_constraint to avoid adding constraints
	 that are already present.  Doing so also ensures termination
	 in the case of cycles.

	 We also check for single-element ranges, adding EQ_EXPR facts
	 where we discover them.  For example 3 < x < 5 implies
	 that x == 4 (if x is an integer).  */
      for (unsigned i = 0; i < m_constraints.length (); i++)
	{
	  const constraint *other = &m_constraints[i];
	  if (other->is_ordering_p ())
	    {
	      /* Refresh the EC IDs, in case any mergers have happened.  */
	      lhs_id = get_or_add_equiv_class (lhs);
	      rhs_id = get_or_add_equiv_class (rhs);

	      tree lhs_const = lhs_id.get_obj (*this).m_constant;
	      tree rhs_const = rhs_id.get_obj (*this).m_constant;
	      tree other_lhs_const
		= other->m_lhs.get_obj (*this).m_constant;
	      tree other_rhs_const
		= other->m_rhs.get_obj (*this).m_constant;

	      /* We have "LHS </<= RHS" and "other.lhs </<= other.rhs".  */

	      /* If we have LHS </<= RHS and RHS </<= LHS, then we have a
		 cycle.  */
	      if (rhs_id == other->m_lhs
		  && other->m_rhs == lhs_id)
		{
		  /* We must have equality for this to be possible.  */
		  gcc_assert (c_op == CONSTRAINT_LE
			      && other->m_op == CONSTRAINT_LE);
		  add_constraint (lhs_id, EQ_EXPR, rhs_id);
		  /* Adding an equality will merge the two ECs and potentially
		     reorganize the constraints.  Stop iterating.  */
		  return;
		}
	      /* Otherwise, check for transitivity.  */
	      if (rhs_id == other->m_lhs)
		{
		  /* With RHS == other.lhs, we have:
		     "LHS </<= (RHS, other.lhs) </<= other.rhs"
		     and thus this implies "LHS </<= other.rhs".  */

		  /* Do we have a tightly-constrained range?  */
		  if (lhs_const
		      && !rhs_const
		      && other_rhs_const)
		    {
		      range r (bound (lhs_const, c_op == CONSTRAINT_LE),
			       bound (other_rhs_const,
				      other->m_op == CONSTRAINT_LE));
		      if (tree constant = r.constrained_to_single_element ())
			{
			  const svalue *cst_sval
			    = m_mgr->get_or_create_constant_svalue (constant);
			  add_constraint
			    (rhs_id, EQ_EXPR,
			     get_or_add_equiv_class (cst_sval));
			  return;
			}
		    }

		  /* Otherwise, add the constraint implied by transitivity.  */
		  enum tree_code new_op
		    = ((c_op == CONSTRAINT_LE && other->m_op == CONSTRAINT_LE)
		       ? LE_EXPR : LT_EXPR);
		  add_constraint (lhs_id, new_op, other->m_rhs);
		}
	      else if (other->m_rhs == lhs_id)
		{
		  /* With other.rhs == LHS, we have:
		     "other.lhs </<= (other.rhs, LHS) </<= RHS"
		     and thus this implies "other.lhs </<= RHS".  */

		  /* Do we have a tightly-constrained range?  */
		  if (other_lhs_const
		      && !lhs_const
		      && rhs_const)
		    {
		      range r (bound (other_lhs_const,
				      other->m_op == CONSTRAINT_LE),
			       bound (rhs_const,
				      c_op == CONSTRAINT_LE));
		      if (tree constant = r.constrained_to_single_element ())
			{
			  const svalue *cst_sval
			    = m_mgr->get_or_create_constant_svalue (constant);
			  add_constraint
			    (lhs_id, EQ_EXPR,
			     get_or_add_equiv_class (cst_sval));
			  return;
			}
		    }

		  /* Otherwise, add the constraint implied by transitivity.  */
		  enum tree_code new_op
		    = ((c_op == CONSTRAINT_LE && other->m_op == CONSTRAINT_LE)
		       ? LE_EXPR : LT_EXPR);
		  add_constraint (other->m_lhs, new_op, rhs_id);
		}
	    }
	}
    }
}

/* Attempt to add the constraint that SVAL is within RANGES to this
   constraint_manager.

   Return true if the constraint was successfully added (or is already
   known to be true).
   Return false if the constraint contradicts existing knowledge.  */

bool
constraint_manager::add_bounded_ranges (const svalue *sval,
					const bounded_ranges *ranges)
{
  /* If RANGES is just a singleton, convert this to adding the constraint:
     "SVAL == {the singleton}".  */
  if (ranges->get_count () == 1
      && ranges->get_range (0).singleton_p ())
    {
      tree range_cst = ranges->get_range (0).m_lower;
      const svalue *range_sval
	= m_mgr->get_or_create_constant_svalue (range_cst);
      return add_constraint (sval, EQ_EXPR, range_sval);
    }

  sval = sval->unwrap_any_unmergeable ();

  /* Nothing can be known about unknown/poisoned values.  */
  if (!sval->can_have_associated_state_p ())
    /* Not a contradiction.  */
    return true;

  /* If SVAL is a constant, then we can look at RANGES directly.  */
  if (tree cst = sval->maybe_get_constant ())
    {
      /* If the ranges contain CST, then it's a successful no-op;
	 otherwise it's a contradiction.  */
      return ranges->contain_p (cst);
    }

  equiv_class_id ec_id = get_or_add_equiv_class (sval);

  /* If the EC has a constant, it's either true or false.  */
  const equiv_class &ec = ec_id.get_obj (*this);
  if (tree ec_cst = ec.get_any_constant ())
    {
      if (ranges->contain_p (ec_cst))
	/* We already have SVAL == EC_CST, within RANGES, so
	   we can discard RANGES and succeed.  */
	return true;
      else
	/* We already have SVAL == EC_CST, not within RANGES, so
	   we can reject RANGES as a contradiction.  */
	return false;
    }

  /* We have at most one per ec_id.  */
  /* Iterate through each range in RANGES.  */
  for (auto iter : m_bounded_ranges_constraints)
    {
      if (iter.m_ec_id == ec_id)
	{
	  /* Update with intersection, or fail if empty.  */
	  bounded_ranges_manager *mgr = get_range_manager ();
	  const bounded_ranges *intersection
	    = mgr->get_or_create_intersection (iter.m_ranges, ranges);
	  if (intersection->empty_p ())
	    {
	      /* No intersection; fail.  */
	      return false;
	    }
	  else
	    {
	      /* Update with intersection; succeed.  */
	      iter.m_ranges = intersection;
	      validate ();
	      return true;
	    }
	}
    }
  m_bounded_ranges_constraints.safe_push
    (bounded_ranges_constraint (ec_id, ranges));

  validate ();

  return true;
}

/* Look for SVAL within the equivalence classes of this constraint_manager;
   if found, return true, writing the id to *OUT if OUT is non-NULL,
   otherwise return false.  */

bool
constraint_manager::get_equiv_class_by_svalue (const svalue *sval,
					       equiv_class_id *out) const
{
  /* TODO: should we have a map, rather than these searches?  */
  int i;
  equiv_class *ec;
  FOR_EACH_VEC_ELT (m_equiv_classes, i, ec)
    {
      int j;
      const svalue *iv;
      FOR_EACH_VEC_ELT (ec->m_vars, j, iv)
	if (iv == sval)
	  {
	    if (out)
	      *out = equiv_class_id (i);
	    return true;
	  }
    }
  return false;
}

/* Ensure that SVAL has an equivalence class within this constraint_manager;
   return the ID of the class.  */

equiv_class_id
constraint_manager::get_or_add_equiv_class (const svalue *sval)
{
  equiv_class_id result (-1);

  gcc_assert (sval->can_have_associated_state_p ());

  /* Convert all NULL pointers to (void *) to avoid state explosions
     involving all of the various (foo *)NULL vs (bar *)NULL.  */
  if (sval->get_type ())
    if (POINTER_TYPE_P (sval->get_type ()))
      if (tree cst = sval->maybe_get_constant ())
	if (zerop (cst))
	  sval = m_mgr->get_or_create_constant_svalue (null_pointer_node);

  /* Try svalue match.  */
  if (get_equiv_class_by_svalue (sval, &result))
    return result;

  /* Try equality of constants.  */
  if (tree cst = sval->maybe_get_constant ())
    {
      int i;
      equiv_class *ec;
      FOR_EACH_VEC_ELT (m_equiv_classes, i, ec)
	if (ec->m_constant
	    && types_compatible_p (TREE_TYPE (cst),
				   TREE_TYPE (ec->m_constant)))
	  {
	    tree eq = fold_binary (EQ_EXPR, boolean_type_node,
				   cst, ec->m_constant);
	    if (eq == boolean_true_node)
	      {
		ec->add (sval);
		return equiv_class_id (i);
	      }
	  }
    }


  /* Not found.  */
  equiv_class *new_ec = new equiv_class ();
  new_ec->add (sval);
  m_equiv_classes.safe_push (new_ec);

  equiv_class_id new_id (m_equiv_classes.length () - 1);

  return new_id;
}

/* Evaluate the condition LHS_EC OP RHS_EC.  */

tristate
constraint_manager::eval_condition (equiv_class_id lhs_ec,
				    enum tree_code op,
				    equiv_class_id rhs_ec) const
{
  if (lhs_ec == rhs_ec)
    {
      switch (op)
	{
	case EQ_EXPR:
	case GE_EXPR:
	case LE_EXPR:
	  return tristate (tristate::TS_TRUE);

	case NE_EXPR:
	case GT_EXPR:
	case LT_EXPR:
	  return tristate (tristate::TS_FALSE);
	default:
	  break;
	}
    }

  tree lhs_const = lhs_ec.get_obj (*this).get_any_constant ();
  tree rhs_const = rhs_ec.get_obj (*this).get_any_constant ();
  if (lhs_const && rhs_const)
    {
      tristate result_for_constants
	= compare_constants (lhs_const, op, rhs_const);
      if (result_for_constants.is_known ())
	return result_for_constants;
    }

  enum tree_code swapped_op = swap_tree_comparison (op);

  int i;
  constraint *c;
  FOR_EACH_VEC_ELT (m_constraints, i, c)
    {
      if (c->m_lhs == lhs_ec
	  && c->m_rhs == rhs_ec)
	{
	  tristate result_for_constraint
	    = eval_constraint_op_for_op (c->m_op, op);
	  if (result_for_constraint.is_known ())
	    return result_for_constraint;
	}
      /* Swapped operands.  */
      if (c->m_lhs == rhs_ec
	  && c->m_rhs == lhs_ec)
	{
	  tristate result_for_constraint
	    = eval_constraint_op_for_op (c->m_op, swapped_op);
	  if (result_for_constraint.is_known ())
	    return result_for_constraint;
	}
    }

  /* We don't use m_bounded_ranges_constraints here yet.  */

  return tristate (tristate::TS_UNKNOWN);
}

range
constraint_manager::get_ec_bounds (equiv_class_id ec_id) const
{
  range result;

  int i;
  constraint *c;
  FOR_EACH_VEC_ELT (m_constraints, i, c)
    {
      if (c->m_lhs == ec_id)
	{
	  if (tree other_cst = c->m_rhs.get_obj (*this).get_any_constant ())
	    switch (c->m_op)
	      {
	      default:
		gcc_unreachable ();
	      case CONSTRAINT_NE:
		continue;

	      case CONSTRAINT_LT:
		/* We have "EC_ID < OTHER_CST".  */
		result.add_bound (bound (other_cst, false), BK_UPPER);
		break;

	      case CONSTRAINT_LE:
		/* We have "EC_ID <= OTHER_CST".  */
		result.add_bound (bound (other_cst, true), BK_UPPER);
		break;
	      }
	}
      if (c->m_rhs == ec_id)
	{
	  if (tree other_cst = c->m_lhs.get_obj (*this).get_any_constant ())
	    switch (c->m_op)
	      {
	      default:
		gcc_unreachable ();
	      case CONSTRAINT_NE:
		continue;

	      case CONSTRAINT_LT:
		/* We have "OTHER_CST < EC_ID"
		   i.e. "EC_ID > OTHER_CST".  */
		result.add_bound (bound (other_cst, false), BK_LOWER);
		break;

	      case CONSTRAINT_LE:
		/* We have "OTHER_CST <= EC_ID"
		   i.e. "EC_ID >= OTHER_CST".  */
		result.add_bound (bound (other_cst, true), BK_LOWER);
		break;
	      }
	}
    }

  return result;
}

/* Evaluate the condition LHS_EC OP RHS_CONST, avoiding the creation
   of equiv_class instances.  */

tristate
constraint_manager::eval_condition (equiv_class_id lhs_ec,
				    enum tree_code op,
				    tree rhs_const) const
{
  gcc_assert (!lhs_ec.null_p ());
  gcc_assert (CONSTANT_CLASS_P (rhs_const));

  if (tree lhs_const = lhs_ec.get_obj (*this).get_any_constant ())
    return compare_constants (lhs_const, op, rhs_const);

  /* Check for known inequalities of the form
       (LHS_EC != OTHER_CST) or (OTHER_CST != LHS_EC).
     If RHS_CONST == OTHER_CST, then we also know that LHS_EC != OTHER_CST.
     For example, we might have the constraint
       ptr != (void *)0
     so we want the condition
       ptr == (foo *)0
     to be false.  */
  int i;
  constraint *c;
  FOR_EACH_VEC_ELT (m_constraints, i, c)
    {
      if (c->m_op == CONSTRAINT_NE)
	{
	  if (c->m_lhs == lhs_ec)
	    {
	      if (tree other_cst = c->m_rhs.get_obj (*this).get_any_constant ())
		if (compare_constants
		      (rhs_const, EQ_EXPR, other_cst).is_true ())
		  {
		    switch (op)
		      {
		      case EQ_EXPR:
			return tristate (tristate::TS_FALSE);
		      case NE_EXPR:
			return tristate (tristate::TS_TRUE);
		      default:
			break;
		      }
		  }
	    }
	  if (c->m_rhs == lhs_ec)
	    {
	      if (tree other_cst = c->m_lhs.get_obj (*this).get_any_constant ())
		if (compare_constants
		      (rhs_const, EQ_EXPR, other_cst).is_true ())
		  {
		    switch (op)
		      {
		      case EQ_EXPR:
			return tristate (tristate::TS_FALSE);
		      case NE_EXPR:
			return tristate (tristate::TS_TRUE);
		      default:
			break;
		      }
		  }
	    }
	}
    }

  bounded_ranges_manager *mgr = get_range_manager ();
  for (const auto &iter : m_bounded_ranges_constraints)
    if (iter.m_ec_id == lhs_ec)
      return iter.m_ranges->eval_condition (op, rhs_const, mgr);

  /* Look at existing bounds on LHS_EC.  */
  range lhs_bounds = get_ec_bounds (lhs_ec);
  tristate result = lhs_bounds.eval_condition (op, rhs_const);
  if (result.is_known ())
    return result;

  /* Also reject if range::add_bound fails.  */
  if (!lhs_bounds.add_bound (op, rhs_const))
    return tristate (false);

  return tristate::unknown ();
}

/* Return true iff "LHS == RHS" is known to be impossible due to
   derived conditions.

   Look for an EC containing an EC_VAL of the form (LHS OP CST).
   If found, see if (LHS OP CST) == EC_VAL is false.
   If so, we know this condition is false.

   For example, if we already know that
     (X & CST_MASK) == Y
   and we're evaluating X == Z, we can test to see if
     (Z & CST_MASK) == EC_VAL
   and thus if:
     (Z & CST_MASK) == Y
   and reject this if we know that's false.  */

bool
constraint_manager::impossible_derived_conditions_p (const svalue *lhs,
						     const svalue *rhs) const
{
  int i;
  equiv_class *ec;
  FOR_EACH_VEC_ELT (m_equiv_classes, i, ec)
    {
      for (const svalue *ec_sval : ec->m_vars)
	switch (ec_sval->get_kind ())
	  {
	  default:
	    break;
	  case SK_BINOP:
	    {
	      const binop_svalue *iter_binop
		= as_a <const binop_svalue *> (ec_sval);
	      if (lhs == iter_binop->get_arg0 ()
		  && iter_binop->get_type ())
		if (iter_binop->get_arg1 ()->get_kind () == SK_CONSTANT)
		  {
		    /* Try evalating EC_SVAL with LHS
		       as the value of EC_SVAL's lhs, and see if it's
		       consistent with existing knowledge.  */
		    const svalue *subst_bin_op
		      = m_mgr->get_or_create_binop
		      (iter_binop->get_type (),
		       iter_binop->get_op (),
		       rhs,
		       iter_binop->get_arg1 ());
		    tristate t = eval_condition (subst_bin_op,
						 EQ_EXPR,
						 ec_sval);
		    if (t.is_false ())
		      return true; /* Impossible.  */
		  }
	    }
	    break;
	  }
    }
  /* Not known to be impossible.  */
  return false;
}


/* Evaluate the condition LHS OP RHS, without modifying this
   constraint_manager (avoiding the creation of equiv_class instances).  */

tristate
constraint_manager::eval_condition (const svalue *lhs,
				    enum tree_code op,
				    const svalue *rhs) const
{
  lhs = lhs->unwrap_any_unmergeable ();
  rhs = rhs->unwrap_any_unmergeable ();

  /* Nothing can be known about unknown or poisoned values.  */
  if (lhs->get_kind () == SK_UNKNOWN
      || lhs->get_kind () == SK_POISONED
      || rhs->get_kind () == SK_UNKNOWN
      || rhs->get_kind () == SK_POISONED)
    return tristate (tristate::TS_UNKNOWN);

  if (lhs == rhs
      && !(FLOAT_TYPE_P (lhs->get_type ())
	   || FLOAT_TYPE_P (rhs->get_type ())))
    {
      switch (op)
	{
	case EQ_EXPR:
	case GE_EXPR:
	case LE_EXPR:
	  return tristate (tristate::TS_TRUE);

	case NE_EXPR:
	case GT_EXPR:
	case LT_EXPR:
	  return tristate (tristate::TS_FALSE);
	default:
	  break;
	}
    }

  equiv_class_id lhs_ec (-1);
  equiv_class_id rhs_ec (-1);
  get_equiv_class_by_svalue (lhs, &lhs_ec);
  get_equiv_class_by_svalue (rhs, &rhs_ec);
  if (!lhs_ec.null_p () && !rhs_ec.null_p ())
    {
      tristate result_for_ecs
	= eval_condition (lhs_ec, op, rhs_ec);
      if (result_for_ecs.is_known ())
	return result_for_ecs;
    }

  if (op == EQ_EXPR
      && impossible_derived_conditions_p (lhs, rhs))
    return false;

  /* If at least one is not in an EC, we have no constraints
     comparing LHS and RHS yet.
     They might still be comparable if one (or both) is a constant.

     Alternatively, we can also get here if we had ECs but they weren't
     comparable.  Again, constant comparisons might give an answer.  */
  tree lhs_const = lhs->maybe_get_constant ();
  tree rhs_const = rhs->maybe_get_constant ();
  if (lhs_const && rhs_const)
    {
      tristate result_for_constants
	= compare_constants (lhs_const, op, rhs_const);
      if (result_for_constants.is_known ())
	return result_for_constants;
    }

  if (!lhs_ec.null_p ())
    {
      if (rhs_const)
	return eval_condition (lhs_ec, op, rhs_const);
    }
  if (!rhs_ec.null_p ())
    {
      if (lhs_const)
	{
	  enum tree_code swapped_op = swap_tree_comparison (op);
	  return eval_condition (rhs_ec, swapped_op, lhs_const);
	}
    }

  return tristate (tristate::TS_UNKNOWN);
}

/* Delete any information about svalues identified by P.
   Such instances are removed from equivalence classes, and any
   redundant ECs and constraints are also removed.
   Accumulate stats into STATS.  */

template <typename PurgeCriteria>
void
constraint_manager::purge (const PurgeCriteria &p, purge_stats *stats)
{
  /* Delete any svalues identified by P within the various equivalence
     classes.  */
  for (unsigned ec_idx = 0; ec_idx < m_equiv_classes.length (); )
    {
      equiv_class *ec = m_equiv_classes[ec_idx];

      int i;
      const svalue *sval;
      bool delete_ec = false;
      FOR_EACH_VEC_ELT (ec->m_vars, i, sval)
	{
	  if (sval == ec->m_cst_sval)
	    continue;
	  if (p.should_purge_p (sval))
	    {
	      if (ec->del (sval))
		if (!ec->m_constant)
		  delete_ec = true;
	    }
	}

      if (delete_ec)
	{
	  delete ec;
	  m_equiv_classes.ordered_remove (ec_idx);
	  if (stats)
	    stats->m_num_equiv_classes++;

	  /* Update the constraints, potentially removing some.  */
	  for (unsigned con_idx = 0; con_idx < m_constraints.length (); )
	    {
	      constraint *c = &m_constraints[con_idx];

	      /* Remove constraints that refer to the deleted EC.  */
	      if (c->m_lhs == ec_idx
		  || c->m_rhs == ec_idx)
		{
		  m_constraints.ordered_remove (con_idx);
		  if (stats)
		    stats->m_num_constraints++;
		}
	      else
		{
		  /* Renumber constraints that refer to ECs that have
		     had their idx changed.  */
		  c->m_lhs.update_for_removal (ec_idx);
		  c->m_rhs.update_for_removal (ec_idx);

		  con_idx++;
		}
	    }

	  /* Update bounded_ranges_constraint instances.  */
	  for (unsigned r_idx = 0;
	       r_idx < m_bounded_ranges_constraints.length (); )
	    {
	      bounded_ranges_constraint *brc
		= &m_bounded_ranges_constraints[r_idx];

	      /* Remove if it refers to the deleted EC.  */
	      if (brc->m_ec_id == ec_idx)
		{
		  m_bounded_ranges_constraints.ordered_remove (r_idx);
		  if (stats)
		    stats->m_num_bounded_ranges_constraints++;
		}
	      else
		{
		  /* Renumber any EC ids that refer to ECs that have
		     had their idx changed.  */
		  brc->m_ec_id.update_for_removal (ec_idx);
		  r_idx++;
		}
	    }
	}
      else
	ec_idx++;
    }

  /* Now delete any constraints that are purely between constants.  */
  for (unsigned con_idx = 0; con_idx < m_constraints.length (); )
    {
      constraint *c = &m_constraints[con_idx];
      if (m_equiv_classes[c->m_lhs.m_idx]->m_vars.length () == 0
	  && m_equiv_classes[c->m_rhs.m_idx]->m_vars.length () == 0)
	{
	  m_constraints.ordered_remove (con_idx);
	  if (stats)
	    stats->m_num_constraints++;
	}
      else
	{
	  con_idx++;
	}
    }

  /* Finally, delete any ECs that purely contain constants and aren't
     referenced by any constraints.  */
  for (unsigned ec_idx = 0; ec_idx < m_equiv_classes.length (); )
    {
      equiv_class *ec = m_equiv_classes[ec_idx];
      if (ec->m_vars.length () == 0)
	{
	  equiv_class_id ec_id (ec_idx);
	  bool has_constraint = false;
	  for (unsigned con_idx = 0; con_idx < m_constraints.length ();
	       con_idx++)
	    {
	      constraint *c = &m_constraints[con_idx];
	      if (c->m_lhs == ec_id
		  || c->m_rhs == ec_id)
		{
		  has_constraint = true;
		  break;
		}
	    }
	  if (!has_constraint)
	    {
	      delete ec;
	      m_equiv_classes.ordered_remove (ec_idx);
	      if (stats)
		stats->m_num_equiv_classes++;

	      /* Renumber constraints that refer to ECs that have
		 had their idx changed.  */
	      for (unsigned con_idx = 0; con_idx < m_constraints.length ();
		   con_idx++)
		{
		  constraint *c = &m_constraints[con_idx];
		  c->m_lhs.update_for_removal (ec_idx);
		  c->m_rhs.update_for_removal (ec_idx);
		}

	      /* Likewise for m_bounded_ranges_constraints.  */
	      for (unsigned r_idx = 0;
		   r_idx < m_bounded_ranges_constraints.length ();
		   r_idx++)
		{
		  bounded_ranges_constraint *brc
		    = &m_bounded_ranges_constraints[r_idx];
		  brc->m_ec_id.update_for_removal (ec_idx);
		}

	      continue;
	    }
	}
      ec_idx++;
    }

  validate ();
}

/* Implementation of PurgeCriteria: purge svalues that are not live
   with respect to LIVE_SVALUES and MODEL.  */

class dead_svalue_purger
{
public:
  dead_svalue_purger (const svalue_set &live_svalues,
		      const region_model *model)
  : m_live_svalues (live_svalues), m_model (model)
  {
  }

  bool should_purge_p (const svalue *sval) const
  {
    return !sval->live_p (&m_live_svalues, m_model);
  }

private:
  const svalue_set &m_live_svalues;
  const region_model *m_model;
};

/* Purge dead svalues from equivalence classes and update constraints
   accordingly.  */

void
constraint_manager::
on_liveness_change (const svalue_set &live_svalues,
		    const region_model *model)
{
  dead_svalue_purger p (live_svalues, model);
  purge (p, NULL);
}

class svalue_purger
{
public:
  svalue_purger (const svalue *sval) : m_sval (sval) {}

  bool should_purge_p (const svalue *sval) const
  {
    return sval->involves_p (m_sval);
  }

private:
  const svalue *m_sval;
};

/* Purge any state involving SVAL.  */

void
constraint_manager::purge_state_involving (const svalue *sval)
{
  svalue_purger p (sval);
  purge (p, NULL);
}

/* Comparator for use by constraint_manager::canonicalize.
   Sort a pair of equiv_class instances, using the representative
   svalue as a sort key.  */

static int
equiv_class_cmp (const void *p1, const void *p2)
{
  const equiv_class *ec1 = *(const equiv_class * const *)p1;
  const equiv_class *ec2 = *(const equiv_class * const *)p2;

  const svalue *rep1 = ec1->get_representative ();
  const svalue *rep2 = ec2->get_representative ();

  gcc_assert (rep1);
  gcc_assert (rep2);

  return svalue::cmp_ptr (rep1, rep2);
}

/* Comparator for use by constraint_manager::canonicalize.
   Sort a pair of constraint instances.  */

static int
constraint_cmp (const void *p1, const void *p2)
{
  const constraint *c1 = (const constraint *)p1;
  const constraint *c2 = (const constraint *)p2;
  int lhs_cmp = c1->m_lhs.as_int () - c2->m_lhs.as_int ();
  if (lhs_cmp)
    return lhs_cmp;
  int rhs_cmp = c1->m_rhs.as_int () - c2->m_rhs.as_int ();
  if (rhs_cmp)
    return rhs_cmp;
  return c1->m_op - c2->m_op;
}

/* Purge redundant equivalence classes and constraints, and reorder them
   within this constraint_manager into a canonical order, to increase the
   chances of finding equality with another instance.  */

void
constraint_manager::canonicalize ()
{
  /* First, sort svalues within the ECs.  */
  unsigned i;
  equiv_class *ec;
  FOR_EACH_VEC_ELT (m_equiv_classes, i, ec)
    ec->canonicalize ();

  /* TODO: remove constraints where both sides have a constant, and are
     thus implicit.  But does this break transitivity?  */

  /* We will be purging and reordering ECs.
     We will need to remap the equiv_class_ids in the constraints,
     so we need to store the original index of each EC.
     Build a lookup table, mapping from the representative svalue
     to the original equiv_class_id of that svalue.  */
  hash_map<const svalue *, equiv_class_id> original_ec_id;
  const unsigned orig_num_equiv_classes = m_equiv_classes.length ();
  FOR_EACH_VEC_ELT (m_equiv_classes, i, ec)
    {
      const svalue *rep = ec->get_representative ();
      gcc_assert (rep);
      original_ec_id.put (rep, i);
    }

  /* Find ECs used by constraints.  */
  hash_set<const equiv_class *> used_ecs;
  constraint *c;
  FOR_EACH_VEC_ELT (m_constraints, i, c)
    {
      used_ecs.add (m_equiv_classes[c->m_lhs.as_int ()]);
      used_ecs.add (m_equiv_classes[c->m_rhs.as_int ()]);
    }

  for (const auto &iter : m_bounded_ranges_constraints)
    used_ecs.add (m_equiv_classes[iter.m_ec_id.as_int ()]);

  /* Purge unused ECs: those that aren't used by constraints and
     that effectively have only one svalue (either in m_constant
     or in m_vars).  */
  {
    /* "unordered remove if" from a vec.  */
    unsigned i = 0;
    while (i < m_equiv_classes.length ())
      {
	equiv_class *ec = m_equiv_classes[i];
	if (!used_ecs.contains (ec)
	    && !ec->contains_non_constant_p ())
	  {
	    m_equiv_classes.unordered_remove (i);
	    delete ec;
	  }
	else
	  i++;
      }
  }

  /* Next, sort the surviving ECs into a canonical order.  */
  m_equiv_classes.qsort (equiv_class_cmp);

  /* Populate ec_id_map based on the old vs new EC ids.  */
  one_way_id_map<equiv_class_id> ec_id_map (orig_num_equiv_classes);
  FOR_EACH_VEC_ELT (m_equiv_classes, i, ec)
    {
      const svalue *rep = ec->get_representative ();
      gcc_assert (rep);
      ec_id_map.put (*original_ec_id.get (rep), i);
    }

  /* Use ec_id_map to update the EC ids within the constraints.  */
  FOR_EACH_VEC_ELT (m_constraints, i, c)
    {
      ec_id_map.update (&c->m_lhs);
      ec_id_map.update (&c->m_rhs);
    }

  for (auto &iter : m_bounded_ranges_constraints)
    ec_id_map.update (&iter.m_ec_id);

  /* Finally, sort the constraints. */
  m_constraints.qsort (constraint_cmp);
}

/* Concrete subclass of fact_visitor for use by constraint_manager::merge.
   For every fact in CM_A, see if it is also true in *CM_B.  Add such
   facts to *OUT.  */

class merger_fact_visitor : public fact_visitor
{
public:
  merger_fact_visitor (const constraint_manager *cm_b,
		       constraint_manager *out)
  : m_cm_b (cm_b), m_out (out)
  {}

  void on_fact (const svalue *lhs, enum tree_code code, const svalue *rhs)
    final override
  {
    /* Special-case for widening.  */
    if (lhs->get_kind () == SK_WIDENING)
      if (!m_cm_b->get_equiv_class_by_svalue (lhs, NULL))
	{
	  /* LHS isn't constrained within m_cm_b.  */
	  bool sat = m_out->add_constraint (lhs, code, rhs);
	  gcc_assert (sat);
	  return;
	}

    if (m_cm_b->eval_condition (lhs, code, rhs).is_true ())
      {
	bool sat = m_out->add_constraint (lhs, code, rhs);
	if (!sat)
	  {
	    /* If -fanalyzer-transitivity is off, we can encounter cases
	       where at least one of the two constraint_managers being merged
	       is infeasible, but we only discover that infeasibility
	       during merging (PR analyzer/96650).
	       Silently drop such constraints.  */
	    gcc_assert (!flag_analyzer_transitivity);
	  }
      }
  }

  void on_ranges (const svalue *lhs_sval,
		  const bounded_ranges *ranges) final override
  {
    for (const auto &iter : m_cm_b->m_bounded_ranges_constraints)
      {
	const equiv_class &ec_rhs = iter.m_ec_id.get_obj (*m_cm_b);
	for (unsigned i = 0; i < ec_rhs.m_vars.length (); i++)
	  {
	    const svalue *rhs_sval = ec_rhs.m_vars[i];
	    if (lhs_sval == rhs_sval)
	      {
		/* Union of the two ranges.  */
		auto_vec <const bounded_ranges *> pair (2);
		pair.quick_push (ranges);
		pair.quick_push (iter.m_ranges);
		bounded_ranges_manager *ranges_mgr
		  = m_cm_b->get_range_manager ();
		const bounded_ranges *union_
		  = ranges_mgr->get_or_create_union (pair);
		bool sat = m_out->add_bounded_ranges (lhs_sval, union_);
		gcc_assert (sat);
	      }
	  }
      }
  }

private:
  const constraint_manager *m_cm_b;
  constraint_manager *m_out;
};

/* Use MERGER to merge CM_A and CM_B into *OUT.
   If one thinks of a constraint_manager as a subset of N-dimensional
   space, this takes the union of the points of CM_A and CM_B, and
   expresses that into *OUT.  Alternatively, it can be thought of
   as the intersection of the constraints.  */

void
constraint_manager::merge (const constraint_manager &cm_a,
			   const constraint_manager &cm_b,
			   constraint_manager *out)
{
  /* Merge the equivalence classes and constraints.
     The easiest way to do this seems to be to enumerate all of the facts
     in cm_a, see which are also true in cm_b,
     and add those to *OUT.  */
  merger_fact_visitor v (&cm_b, out);
  cm_a.for_each_fact (&v);
}

/* Call VISITOR's on_fact vfunc repeatedly to express the various
   equivalence classes and constraints.
   This is used by constraint_manager::merge to find the common
   facts between two input constraint_managers.  */

void
constraint_manager::for_each_fact (fact_visitor *visitor) const
{
  /* First, call EQ_EXPR within the various equivalence classes.  */
  unsigned ec_idx;
  equiv_class *ec;
  FOR_EACH_VEC_ELT (m_equiv_classes, ec_idx, ec)
    {
      if (ec->m_cst_sval)
	{
	  unsigned i;
	  const svalue *sval;
	  FOR_EACH_VEC_ELT (ec->m_vars, i, sval)
	    visitor->on_fact (ec->m_cst_sval, EQ_EXPR, sval);
	}
      for (unsigned i = 0; i < ec->m_vars.length (); i++)
	for (unsigned j = i + 1; j < ec->m_vars.length (); j++)
	  visitor->on_fact (ec->m_vars[i], EQ_EXPR, ec->m_vars[j]);
    }

  /* Now, express the various constraints.  */
  unsigned con_idx;
  constraint *c;
  FOR_EACH_VEC_ELT (m_constraints, con_idx, c)
    {
      const equiv_class &ec_lhs = c->m_lhs.get_obj (*this);
      const equiv_class &ec_rhs = c->m_rhs.get_obj (*this);
      enum tree_code code = constraint_tree_code (c->m_op);

      if (ec_lhs.m_cst_sval)
	{
	  for (unsigned j = 0; j < ec_rhs.m_vars.length (); j++)
	    {
	      visitor->on_fact (ec_lhs.m_cst_sval, code, ec_rhs.m_vars[j]);
	    }
	}
      for (unsigned i = 0; i < ec_lhs.m_vars.length (); i++)
	{
	  if (ec_rhs.m_cst_sval)
	    visitor->on_fact (ec_lhs.m_vars[i], code, ec_rhs.m_cst_sval);
	  for (unsigned j = 0; j < ec_rhs.m_vars.length (); j++)
	    visitor->on_fact (ec_lhs.m_vars[i], code, ec_rhs.m_vars[j]);
	}
    }

  for (const auto &iter : m_bounded_ranges_constraints)
    {
      const equiv_class &ec_lhs = iter.m_ec_id.get_obj (*this);
      for (unsigned i = 0; i < ec_lhs.m_vars.length (); i++)
	{
	  const svalue *lhs_sval = ec_lhs.m_vars[i];
	  visitor->on_ranges (lhs_sval, iter.m_ranges);
	}
    }
}

/* Subclass of fact_visitor for use by
   constraint_manager::replay_call_summary.  */

class replay_fact_visitor : public fact_visitor
{
public:
  replay_fact_visitor (call_summary_replay &r,
		       constraint_manager *out)
  : m_r (r), m_out (out), m_feasible (true)
  {}

  bool feasible_p () const { return m_feasible; }

  void on_fact (const svalue *lhs, enum tree_code code, const svalue *rhs)
    final override
  {
    const svalue *caller_lhs = m_r.convert_svalue_from_summary (lhs);
    if (!caller_lhs)
      return;
    const svalue *caller_rhs = m_r.convert_svalue_from_summary (rhs);
    if (!caller_rhs)
      return;
    if (!m_out->add_constraint (caller_lhs, code, caller_rhs))
      m_feasible = false;
  }

  void on_ranges (const svalue *lhs_sval,
		  const bounded_ranges *ranges) final override
  {
    const svalue *caller_lhs = m_r.convert_svalue_from_summary (lhs_sval);
    if (!caller_lhs)
      return;
    if (!m_out->add_bounded_ranges (caller_lhs, ranges))
      m_feasible = false;
  }

private:
  call_summary_replay &m_r;
  constraint_manager *m_out;
  bool m_feasible;
};

/* Attempt to use R to replay the constraints from SUMMARY into this object.
   Return true if it is feasible.  */

bool
constraint_manager::replay_call_summary (call_summary_replay &r,
					 const constraint_manager &summary)
{
  replay_fact_visitor v (r, this);
  summary.for_each_fact (&v);
  return v.feasible_p ();
}

/* Assert that this object is valid.  */

void
constraint_manager::validate () const
{
  /* Skip this in a release build.  */
#if !CHECKING_P
  return;
#endif

  int i;
  equiv_class *ec;
  FOR_EACH_VEC_ELT (m_equiv_classes, i, ec)
    {
      gcc_assert (ec);

      int j;
      const svalue *sval;
      FOR_EACH_VEC_ELT (ec->m_vars, j, sval)
	gcc_assert (sval);
      if (ec->m_constant)
	{
	  gcc_assert (CONSTANT_CLASS_P (ec->m_constant));
	  gcc_assert (ec->m_cst_sval);
	}
#if 0
      else
	gcc_assert (ec->m_vars.length () > 0);
#endif
    }

  constraint *c;
  FOR_EACH_VEC_ELT (m_constraints, i, c)
    {
      gcc_assert (!c->m_lhs.null_p ());
      gcc_assert (c->m_lhs.as_int () < (int)m_equiv_classes.length ());
      gcc_assert (!c->m_rhs.null_p ());
      gcc_assert (c->m_rhs.as_int () < (int)m_equiv_classes.length ());
    }

  for (const auto &iter : m_bounded_ranges_constraints)
    {
      gcc_assert (!iter.m_ec_id.null_p ());
      gcc_assert (iter.m_ec_id.as_int () < (int)m_equiv_classes.length ());
    }
}

bounded_ranges_manager *
constraint_manager::get_range_manager () const
{
  return m_mgr->get_range_manager ();
}

#if CHECKING_P

namespace selftest {

/* Various constraint_manager selftests.
   These have to be written in terms of a region_model, since
   the latter is responsible for managing svalue instances.  */

/* Verify that range::add_bound works as expected.  */

static void
test_range ()
{
  tree int_0 = build_int_cst (integer_type_node, 0);
  tree int_1 = build_int_cst (integer_type_node, 1);
  tree int_2 = build_int_cst (integer_type_node, 2);
  tree int_5 = build_int_cst (integer_type_node, 5);

  {
    range r;
    ASSERT_FALSE (r.constrained_to_single_element ());

    /* (r >= 1).  */
    ASSERT_TRUE (r.add_bound (GE_EXPR, int_1));

    /* Redundant.  */
    ASSERT_TRUE (r.add_bound (GE_EXPR, int_0));
    ASSERT_TRUE (r.add_bound (GT_EXPR, int_0));

    ASSERT_FALSE (r.constrained_to_single_element ());

    /* Contradiction.  */
    ASSERT_FALSE (r.add_bound (LT_EXPR, int_1));

    /* (r < 5).  */
    ASSERT_TRUE (r.add_bound (LT_EXPR, int_5));
    ASSERT_FALSE (r.constrained_to_single_element ());

    /* Contradiction.  */
    ASSERT_FALSE (r.add_bound (GE_EXPR, int_5));

    /* (r < 2).  */
    ASSERT_TRUE (r.add_bound (LT_EXPR, int_2));
    ASSERT_TRUE (r.constrained_to_single_element ());

    /* Redundant.  */
    ASSERT_TRUE (r.add_bound (LE_EXPR, int_1));
    ASSERT_TRUE (r.constrained_to_single_element ());
  }
}

/* Verify that setting and getting simple conditions within a region_model
   work (thus exercising the underlying constraint_manager).  */

static void
test_constraint_conditions ()
{
  tree int_42 = build_int_cst (integer_type_node, 42);
  tree int_0 = build_int_cst (integer_type_node, 0);

  tree x = build_global_decl ("x", integer_type_node);
  tree y = build_global_decl ("y", integer_type_node);
  tree z = build_global_decl ("z", integer_type_node);

  /* Self-comparisons.  */
  {
    region_model_manager mgr;
    region_model model (&mgr);
    ASSERT_CONDITION_TRUE (model, x, EQ_EXPR, x);
    ASSERT_CONDITION_TRUE (model, x, LE_EXPR, x);
    ASSERT_CONDITION_TRUE (model, x, GE_EXPR, x);
    ASSERT_CONDITION_FALSE (model, x, NE_EXPR, x);
    ASSERT_CONDITION_FALSE (model, x, LT_EXPR, x);
    ASSERT_CONDITION_FALSE (model, x, GT_EXPR, x);
  }

  /* Adding self-equality shouldn't add equiv classes.  */
  {
    region_model_manager mgr;
    region_model model (&mgr);
    ADD_SAT_CONSTRAINT (model, x, EQ_EXPR, x);
    ADD_SAT_CONSTRAINT (model, int_42, EQ_EXPR, int_42);
    /* ...even when done directly via svalues: */
    const svalue *sval_int_42 = model.get_rvalue (int_42, NULL);
    bool sat = model.get_constraints ()->add_constraint (sval_int_42,
							  EQ_EXPR,
							  sval_int_42);
    ASSERT_TRUE (sat);
    ASSERT_EQ (model.get_constraints ()->m_equiv_classes.length (), 0);
  }

  /* x == y.  */
  {
    region_model_manager mgr;
    region_model model (&mgr);
    ASSERT_CONDITION_UNKNOWN (model, x, EQ_EXPR, y);

    ADD_SAT_CONSTRAINT (model, x, EQ_EXPR, y);

    ASSERT_CONDITION_TRUE (model, x, EQ_EXPR, y);
    ASSERT_CONDITION_TRUE (model, x, LE_EXPR, y);
    ASSERT_CONDITION_TRUE (model, x, GE_EXPR, y);
    ASSERT_CONDITION_FALSE (model, x, NE_EXPR, y);
    ASSERT_CONDITION_FALSE (model, x, LT_EXPR, y);
    ASSERT_CONDITION_FALSE (model, x, GT_EXPR, y);

    /* Swapped operands.  */
    ASSERT_CONDITION_TRUE (model, y, EQ_EXPR, x);
    ASSERT_CONDITION_TRUE (model, y, LE_EXPR, x);
    ASSERT_CONDITION_TRUE (model, y, GE_EXPR, x);
    ASSERT_CONDITION_FALSE (model, y, NE_EXPR, x);
    ASSERT_CONDITION_FALSE (model, y, LT_EXPR, x);
    ASSERT_CONDITION_FALSE (model, y, GT_EXPR, x);

    /* Comparison with other var.  */
    ASSERT_CONDITION_UNKNOWN (model, x, EQ_EXPR, z);
    ASSERT_CONDITION_UNKNOWN (model, x, LE_EXPR, z);
    ASSERT_CONDITION_UNKNOWN (model, x, GE_EXPR, z);
    ASSERT_CONDITION_UNKNOWN (model, x, NE_EXPR, z);
    ASSERT_CONDITION_UNKNOWN (model, x, LT_EXPR, z);
    ASSERT_CONDITION_UNKNOWN (model, x, GT_EXPR, z);
  }

  /* x == y, then y == z  */
  {
    region_model_manager mgr;
    region_model model (&mgr);
    ASSERT_CONDITION_UNKNOWN (model, x, EQ_EXPR, y);

    ADD_SAT_CONSTRAINT (model, x, EQ_EXPR, y);
    ADD_SAT_CONSTRAINT (model, y, EQ_EXPR, z);

    ASSERT_CONDITION_TRUE (model, x, EQ_EXPR, z);
    ASSERT_CONDITION_TRUE (model, x, LE_EXPR, z);
    ASSERT_CONDITION_TRUE (model, x, GE_EXPR, z);
    ASSERT_CONDITION_FALSE (model, x, NE_EXPR, z);
    ASSERT_CONDITION_FALSE (model, x, LT_EXPR, z);
    ASSERT_CONDITION_FALSE (model, x, GT_EXPR, z);
  }

  /* x != y.  */
  {
    region_model_manager mgr;
    region_model model (&mgr);

    ADD_SAT_CONSTRAINT (model, x, NE_EXPR, y);

    ASSERT_CONDITION_TRUE (model, x, NE_EXPR, y);
    ASSERT_CONDITION_FALSE (model, x, EQ_EXPR, y);
    ASSERT_CONDITION_UNKNOWN (model, x, LE_EXPR, y);
    ASSERT_CONDITION_UNKNOWN (model, x, GE_EXPR, y);
    ASSERT_CONDITION_UNKNOWN (model, x, LT_EXPR, y);
    ASSERT_CONDITION_UNKNOWN (model, x, GT_EXPR, y);

    /* Swapped operands.  */
    ASSERT_CONDITION_TRUE (model, y, NE_EXPR, x);
    ASSERT_CONDITION_FALSE (model, y, EQ_EXPR, x);
    ASSERT_CONDITION_UNKNOWN (model, y, LE_EXPR, x);
    ASSERT_CONDITION_UNKNOWN (model, y, GE_EXPR, x);
    ASSERT_CONDITION_UNKNOWN (model, y, LT_EXPR, x);
    ASSERT_CONDITION_UNKNOWN (model, y, GT_EXPR, x);

    /* Comparison with other var.  */
    ASSERT_CONDITION_UNKNOWN (model, x, EQ_EXPR, z);
    ASSERT_CONDITION_UNKNOWN (model, x, LE_EXPR, z);
    ASSERT_CONDITION_UNKNOWN (model, x, GE_EXPR, z);
    ASSERT_CONDITION_UNKNOWN (model, x, NE_EXPR, z);
    ASSERT_CONDITION_UNKNOWN (model, x, LT_EXPR, z);
    ASSERT_CONDITION_UNKNOWN (model, x, GT_EXPR, z);
  }

  /* x < y.  */
  {
    region_model_manager mgr;
    region_model model (&mgr);

    ADD_SAT_CONSTRAINT (model, x, LT_EXPR, y);

    ASSERT_CONDITION_TRUE (model, x, LT_EXPR, y);
    ASSERT_CONDITION_TRUE (model, x, LE_EXPR, y);
    ASSERT_CONDITION_TRUE (model, x, NE_EXPR, y);
    ASSERT_CONDITION_FALSE (model, x, EQ_EXPR, y);
    ASSERT_CONDITION_FALSE (model, x, GT_EXPR, y);
    ASSERT_CONDITION_FALSE (model, x, GE_EXPR, y);

    /* Swapped operands.  */
    ASSERT_CONDITION_FALSE (model, y, LT_EXPR, x);
    ASSERT_CONDITION_FALSE (model, y, LE_EXPR, x);
    ASSERT_CONDITION_TRUE (model, y, NE_EXPR, x);
    ASSERT_CONDITION_FALSE (model, y, EQ_EXPR, x);
    ASSERT_CONDITION_TRUE (model, y, GT_EXPR, x);
    ASSERT_CONDITION_TRUE (model, y, GE_EXPR, x);
  }

  /* x <= y.  */
  {
    region_model_manager mgr;
    region_model model (&mgr);

    ADD_SAT_CONSTRAINT (model, x, LE_EXPR, y);

    ASSERT_CONDITION_UNKNOWN (model, x, LT_EXPR, y);
    ASSERT_CONDITION_TRUE (model, x, LE_EXPR, y);
    ASSERT_CONDITION_UNKNOWN (model, x, NE_EXPR, y);
    ASSERT_CONDITION_UNKNOWN (model, x, EQ_EXPR, y);
    ASSERT_CONDITION_FALSE (model, x, GT_EXPR, y);
    ASSERT_CONDITION_UNKNOWN (model, x, GE_EXPR, y);

    /* Swapped operands.  */
    ASSERT_CONDITION_FALSE (model, y, LT_EXPR, x);
    ASSERT_CONDITION_UNKNOWN (model, y, LE_EXPR, x);
    ASSERT_CONDITION_UNKNOWN (model, y, NE_EXPR, x);
    ASSERT_CONDITION_UNKNOWN (model, y, EQ_EXPR, x);
    ASSERT_CONDITION_UNKNOWN (model, y, GT_EXPR, x);
    ASSERT_CONDITION_TRUE (model, y, GE_EXPR, x);
  }

  /* x > y.  */
  {
    region_model_manager mgr;
    region_model model (&mgr);

    ADD_SAT_CONSTRAINT (model, x, GT_EXPR, y);

    ASSERT_CONDITION_TRUE (model, x, GT_EXPR, y);
    ASSERT_CONDITION_TRUE (model, x, GE_EXPR, y);
    ASSERT_CONDITION_TRUE (model, x, NE_EXPR, y);
    ASSERT_CONDITION_FALSE (model, x, EQ_EXPR, y);
    ASSERT_CONDITION_FALSE (model, x, LT_EXPR, y);
    ASSERT_CONDITION_FALSE (model, x, LE_EXPR, y);

    /* Swapped operands.  */
    ASSERT_CONDITION_FALSE (model, y, GT_EXPR, x);
    ASSERT_CONDITION_FALSE (model, y, GE_EXPR, x);
    ASSERT_CONDITION_TRUE (model, y, NE_EXPR, x);
    ASSERT_CONDITION_FALSE (model, y, EQ_EXPR, x);
    ASSERT_CONDITION_TRUE (model, y, LT_EXPR, x);
    ASSERT_CONDITION_TRUE (model, y, LE_EXPR, x);
  }

  /* x >= y.  */
  {
    region_model_manager mgr;
    region_model model (&mgr);

    ADD_SAT_CONSTRAINT (model, x, GE_EXPR, y);

    ASSERT_CONDITION_UNKNOWN (model, x, GT_EXPR, y);
    ASSERT_CONDITION_TRUE (model, x, GE_EXPR, y);
    ASSERT_CONDITION_UNKNOWN (model, x, NE_EXPR, y);
    ASSERT_CONDITION_UNKNOWN (model, x, EQ_EXPR, y);
    ASSERT_CONDITION_FALSE (model, x, LT_EXPR, y);
    ASSERT_CONDITION_UNKNOWN (model, x, LE_EXPR, y);

    /* Swapped operands.  */
    ASSERT_CONDITION_FALSE (model, y, GT_EXPR, x);
    ASSERT_CONDITION_UNKNOWN (model, y, GE_EXPR, x);
    ASSERT_CONDITION_UNKNOWN (model, y, NE_EXPR, x);
    ASSERT_CONDITION_UNKNOWN (model, y, EQ_EXPR, x);
    ASSERT_CONDITION_UNKNOWN (model, y, LT_EXPR, x);
    ASSERT_CONDITION_TRUE (model, y, LE_EXPR, x);
  }

  // TODO: implied orderings

  /* Constants.  */
  {
    region_model_manager mgr;
    region_model model (&mgr);
    ASSERT_CONDITION_FALSE (model, int_0, EQ_EXPR, int_42);
    ASSERT_CONDITION_TRUE (model, int_0, NE_EXPR, int_42);
    ASSERT_CONDITION_TRUE (model, int_0, LT_EXPR, int_42);
    ASSERT_CONDITION_TRUE (model, int_0, LE_EXPR, int_42);
    ASSERT_CONDITION_FALSE (model, int_0, GT_EXPR, int_42);
    ASSERT_CONDITION_FALSE (model, int_0, GE_EXPR, int_42);
  }

  /* x == 0, y == 42.  */
  {
    region_model_manager mgr;
    region_model model (&mgr);
    ADD_SAT_CONSTRAINT (model, x, EQ_EXPR, int_0);
    ADD_SAT_CONSTRAINT (model, y, EQ_EXPR, int_42);

    ASSERT_CONDITION_TRUE (model, x, NE_EXPR, y);
    ASSERT_CONDITION_FALSE (model, x, EQ_EXPR, y);
    ASSERT_CONDITION_TRUE (model, x, LE_EXPR, y);
    ASSERT_CONDITION_FALSE (model, x, GE_EXPR, y);
    ASSERT_CONDITION_TRUE (model, x, LT_EXPR, y);
    ASSERT_CONDITION_FALSE (model, x, GT_EXPR, y);
  }

  /* Unsatisfiable combinations.  */

  /* x == y && x != y.  */
  {
    region_model_manager mgr;
    region_model model (&mgr);
    ADD_SAT_CONSTRAINT (model, x, EQ_EXPR, y);
    ADD_UNSAT_CONSTRAINT (model, x, NE_EXPR, y);
  }

  /* x == 0 then x == 42.  */
  {
    region_model_manager mgr;
    region_model model (&mgr);
    ADD_SAT_CONSTRAINT (model, x, EQ_EXPR, int_0);
    ADD_UNSAT_CONSTRAINT (model, x, EQ_EXPR, int_42);
  }

  /* x == 0 then x != 0.  */
  {
    region_model_manager mgr;
    region_model model (&mgr);
    ADD_SAT_CONSTRAINT (model, x, EQ_EXPR, int_0);
    ADD_UNSAT_CONSTRAINT (model, x, NE_EXPR, int_0);
  }

  /* x == 0 then x > 0.  */
  {
    region_model_manager mgr;
    region_model model (&mgr);
    ADD_SAT_CONSTRAINT (model, x, EQ_EXPR, int_0);
    ADD_UNSAT_CONSTRAINT (model, x, GT_EXPR, int_0);
  }

  /* x != y && x == y.  */
  {
    region_model_manager mgr;
    region_model model (&mgr);
    ADD_SAT_CONSTRAINT (model, x, NE_EXPR, y);
    ADD_UNSAT_CONSTRAINT (model, x, EQ_EXPR, y);
  }

  /* x <= y && x > y.  */
  {
    region_model_manager mgr;
    region_model model (&mgr);
    ADD_SAT_CONSTRAINT (model, x, LE_EXPR, y);
    ADD_UNSAT_CONSTRAINT (model, x, GT_EXPR, y);
  }

  // etc
}

/* Test transitivity of conditions.  */

static void
test_transitivity ()
{
  tree a = build_global_decl ("a", integer_type_node);
  tree b = build_global_decl ("b", integer_type_node);
  tree c = build_global_decl ("c", integer_type_node);
  tree d = build_global_decl ("d", integer_type_node);

  /* a == b, then c == d, then c == b.  */
  {
    region_model_manager mgr;
    region_model model (&mgr);
    ASSERT_CONDITION_UNKNOWN (model, a, EQ_EXPR, b);
    ASSERT_CONDITION_UNKNOWN (model, b, EQ_EXPR, c);
    ASSERT_CONDITION_UNKNOWN (model, c, EQ_EXPR, d);
    ASSERT_CONDITION_UNKNOWN (model, a, EQ_EXPR, d);

    ADD_SAT_CONSTRAINT (model, a, EQ_EXPR, b);
    ASSERT_CONDITION_TRUE (model, a, EQ_EXPR, b);

    ADD_SAT_CONSTRAINT (model, c, EQ_EXPR, d);
    ASSERT_CONDITION_TRUE (model, c, EQ_EXPR, d);
    ASSERT_CONDITION_UNKNOWN (model, a, EQ_EXPR, d);

    ADD_SAT_CONSTRAINT (model, c, EQ_EXPR, b);
    ASSERT_CONDITION_TRUE (model, c, EQ_EXPR, b);
    ASSERT_CONDITION_TRUE (model, a, EQ_EXPR, d);
  }

  /* Transitivity: "a < b", "b < c" should imply "a < c".  */
  {
    region_model_manager mgr;
    region_model model (&mgr);
    ADD_SAT_CONSTRAINT (model, a, LT_EXPR, b);
    ADD_SAT_CONSTRAINT (model, b, LT_EXPR, c);

    ASSERT_CONDITION_TRUE (model, a, LT_EXPR, c);
    ASSERT_CONDITION_FALSE (model, a, EQ_EXPR, c);
  }

  /* Transitivity: "a <= b", "b < c" should imply "a < c".  */
  {
    region_model_manager mgr;
    region_model model (&mgr);
    ADD_SAT_CONSTRAINT (model, a, LE_EXPR, b);
    ADD_SAT_CONSTRAINT (model, b, LT_EXPR, c);

    ASSERT_CONDITION_TRUE (model, a, LT_EXPR, c);
    ASSERT_CONDITION_FALSE (model, a, EQ_EXPR, c);
  }

  /* Transitivity: "a <= b", "b <= c" should imply "a <= c".  */
  {
    region_model_manager mgr;
    region_model model (&mgr);
    ADD_SAT_CONSTRAINT (model, a, LE_EXPR, b);
    ADD_SAT_CONSTRAINT (model, b, LE_EXPR, c);

    ASSERT_CONDITION_TRUE (model, a, LE_EXPR, c);
    ASSERT_CONDITION_UNKNOWN (model, a, EQ_EXPR, c);
  }

  /* Transitivity: "a > b", "b > c" should imply "a > c".  */
  {
    region_model_manager mgr;
    region_model model (&mgr);
    ADD_SAT_CONSTRAINT (model, a, GT_EXPR, b);
    ADD_SAT_CONSTRAINT (model, b, GT_EXPR, c);

    ASSERT_CONDITION_TRUE (model, a, GT_EXPR, c);
    ASSERT_CONDITION_FALSE (model, a, EQ_EXPR, c);
  }

  /* Transitivity: "a >= b", "b > c" should imply " a > c".  */
  {
    region_model_manager mgr;
    region_model model (&mgr);
    ADD_SAT_CONSTRAINT (model, a, GE_EXPR, b);
    ADD_SAT_CONSTRAINT (model, b, GT_EXPR, c);

    ASSERT_CONDITION_TRUE (model, a, GT_EXPR, c);
    ASSERT_CONDITION_FALSE (model, a, EQ_EXPR, c);
  }

  /* Transitivity: "a >= b", "b >= c" should imply "a >= c".  */
  {
    region_model_manager mgr;
    region_model model (&mgr);
    ADD_SAT_CONSTRAINT (model, a, GE_EXPR, b);
    ADD_SAT_CONSTRAINT (model, b, GE_EXPR, c);

    ASSERT_CONDITION_TRUE (model, a, GE_EXPR, c);
    ASSERT_CONDITION_UNKNOWN (model, a, EQ_EXPR, c);
  }

  /* Transitivity: "(a < b)", "(c < d)", "(b < c)" should
     imply the easy cases:
       (a < c)
       (b < d)
     but also that:
       (a < d).  */
  {
    region_model_manager mgr;
    region_model model (&mgr);
    ADD_SAT_CONSTRAINT (model, a, LT_EXPR, b);
    ADD_SAT_CONSTRAINT (model, c, LT_EXPR, d);
    ADD_SAT_CONSTRAINT (model, b, LT_EXPR, c);

    ASSERT_CONDITION_TRUE (model, a, LT_EXPR, c);
    ASSERT_CONDITION_TRUE (model, b, LT_EXPR, d);
    ASSERT_CONDITION_TRUE (model, a, LT_EXPR, d);
  }

  /* Transitivity: "a >= b", "b >= a" should imply that a == b.  */
  {
    region_model_manager mgr;
    region_model model (&mgr);
    ADD_SAT_CONSTRAINT (model, a, GE_EXPR, b);
    ADD_SAT_CONSTRAINT (model, b, GE_EXPR, a);

    // TODO:
    ASSERT_CONDITION_TRUE (model, a, EQ_EXPR, b);

    /* The ECs for a and b should have merged, and any constraints removed.  */
    ASSERT_EQ (model.get_constraints ()->m_equiv_classes.length (), 1);
    ASSERT_EQ (model.get_constraints ()->m_constraints.length (), 0);
  }

  /* Transitivity: "a >= b", "b > a" should be impossible.  */
  {
    region_model_manager mgr;
    region_model model (&mgr);
    ADD_SAT_CONSTRAINT (model, a, GE_EXPR, b);
    ADD_UNSAT_CONSTRAINT (model, b, GT_EXPR, a);
  }

  /* Transitivity: "a >= b", "b >= c", "c >= a" should imply
     that a == b == c.  */
  {
    region_model_manager mgr;
    region_model model (&mgr);
    ADD_SAT_CONSTRAINT (model, a, GE_EXPR, b);
    ADD_SAT_CONSTRAINT (model, b, GE_EXPR, c);
    ADD_SAT_CONSTRAINT (model, c, GE_EXPR, a);

    ASSERT_CONDITION_TRUE (model, a, EQ_EXPR, c);
  }

  /* Transitivity: "a > b", "b > c", "c > a"
     should be impossible.  */
  {
    region_model_manager mgr;
    region_model model (&mgr);
    ADD_SAT_CONSTRAINT (model, a, GT_EXPR, b);
    ADD_SAT_CONSTRAINT (model, b, GT_EXPR, c);
    ADD_UNSAT_CONSTRAINT (model, c, GT_EXPR, a);
  }

}

/* Test various conditionals involving constants where the results
   ought to be implied based on the values of the constants.  */

static void
test_constant_comparisons ()
{
  tree int_1 = build_int_cst (integer_type_node, 1);
  tree int_3 = build_int_cst (integer_type_node, 3);
  tree int_4 = build_int_cst (integer_type_node, 4);
  tree int_5 = build_int_cst (integer_type_node, 5);

  tree int_1023 = build_int_cst (integer_type_node, 1023);
  tree int_1024 = build_int_cst (integer_type_node, 1024);

  tree a = build_global_decl ("a", integer_type_node);
  tree b = build_global_decl ("b", integer_type_node);

  tree a_plus_one = build2 (PLUS_EXPR, integer_type_node, a, int_1);

  /* Given a >= 1024, then a <= 1023 should be impossible.  */
  {
    region_model_manager mgr;
    region_model model (&mgr);
    ADD_SAT_CONSTRAINT (model, a, GE_EXPR, int_1024);
    ADD_UNSAT_CONSTRAINT (model, a, LE_EXPR, int_1023);
  }

  /* a > 4.  */
  {
    region_model_manager mgr;
    region_model model (&mgr);
    ADD_SAT_CONSTRAINT (model, a, GT_EXPR, int_4);
    ASSERT_CONDITION_TRUE (model, a, GT_EXPR, int_4);
    ASSERT_CONDITION_TRUE (model, a, NE_EXPR, int_3);
    ASSERT_CONDITION_UNKNOWN (model, a, NE_EXPR, int_5);
  }

  /* a <= 4.  */
  {
    region_model_manager mgr;
    region_model model (&mgr);
    ADD_SAT_CONSTRAINT (model, a, LE_EXPR, int_4);
    ASSERT_CONDITION_FALSE (model, a, GT_EXPR, int_4);
    ASSERT_CONDITION_FALSE (model, a, GT_EXPR, int_5);
    ASSERT_CONDITION_UNKNOWN (model, a, NE_EXPR, int_3);
  }

  /* If "a > b" and "a == 3", then "b == 4" ought to be unsatisfiable.  */
  {
    region_model_manager mgr;
    region_model model (&mgr);
    ADD_SAT_CONSTRAINT (model, a, GT_EXPR, b);
    ADD_SAT_CONSTRAINT (model, a, EQ_EXPR, int_3);
    ADD_UNSAT_CONSTRAINT (model, b, EQ_EXPR, int_4);
  }

  /* Various tests of int ranges where there is only one possible candidate.  */
  {
    /* If "a <= 4" && "a > 3", then "a == 4",
       assuming a is of integral type.  */
    {
      region_model_manager mgr;
      region_model model (&mgr);
      ADD_SAT_CONSTRAINT (model, a, LE_EXPR, int_4);
      ADD_SAT_CONSTRAINT (model, a, GT_EXPR, int_3);
      ASSERT_CONDITION_TRUE (model, a, EQ_EXPR, int_4);
    }

    /* If "a > 3" && "a <= 4", then "a == 4",
       assuming a is of integral type.  */
    {
      region_model_manager mgr;
      region_model model (&mgr);
      ADD_SAT_CONSTRAINT (model, a, GT_EXPR, int_3);
      ADD_SAT_CONSTRAINT (model, a, LE_EXPR, int_4);
      ASSERT_CONDITION_TRUE (model, a, EQ_EXPR, int_4);
    }
    /* If "a > 3" && "a < 5", then "a == 4",
       assuming a is of integral type.  */
    {
      region_model_manager mgr;
      region_model model (&mgr);
      ADD_SAT_CONSTRAINT (model, a, GT_EXPR, int_3);
      ADD_SAT_CONSTRAINT (model, a, LT_EXPR, int_5);
      ASSERT_CONDITION_TRUE (model, a, EQ_EXPR, int_4);
    }
    /* If "a >= 4" && "a < 5", then "a == 4",
       assuming a is of integral type.  */
    {
      region_model_manager mgr;
      region_model model (&mgr);
      ADD_SAT_CONSTRAINT (model, a, GE_EXPR, int_4);
      ADD_SAT_CONSTRAINT (model, a, LT_EXPR, int_5);
      ASSERT_CONDITION_TRUE (model, a, EQ_EXPR, int_4);
    }
    /* If "a >= 4" && "a <= 4", then "a == 4".  */
    {
      region_model_manager mgr;
      region_model model (&mgr);
      ADD_SAT_CONSTRAINT (model, a, GE_EXPR, int_4);
      ADD_SAT_CONSTRAINT (model, a, LE_EXPR, int_4);
      ASSERT_CONDITION_TRUE (model, a, EQ_EXPR, int_4);
    }
  }

  /* As above, but for floating-point:
     if "f > 3" && "f <= 4" we don't know that f == 4.  */
  {
    tree f = build_global_decl ("f", double_type_node);
    tree float_3 = build_real_from_int_cst (double_type_node, int_3);
    tree float_4 = build_real_from_int_cst (double_type_node, int_4);

    region_model_manager mgr;
    region_model model (&mgr);
    ADD_SAT_CONSTRAINT (model, f, GT_EXPR, float_3);
    ADD_SAT_CONSTRAINT (model, f, LE_EXPR, float_4);
    ASSERT_CONDITION_UNKNOWN (model, f, EQ_EXPR, float_4);
    ASSERT_CONDITION_UNKNOWN (model, f, EQ_EXPR, int_4);
  }

  /* "a > 3 && a <= 3" should be impossible.  */
  {
    region_model_manager mgr;
    region_model model (&mgr);
    ADD_SAT_CONSTRAINT (model, a, GT_EXPR, int_3);
    ADD_UNSAT_CONSTRAINT (model, a, LE_EXPR, int_3);
  }

  /* "(a + 1) > 3 && a < 3" should be impossible.  */
  {
    region_model_manager mgr;
    {
      region_model model (&mgr);
      ADD_SAT_CONSTRAINT (model, a_plus_one, GT_EXPR, int_3);
      ADD_UNSAT_CONSTRAINT (model, a, LT_EXPR, int_3);
    }
    {
      region_model model (&mgr);
      ADD_SAT_CONSTRAINT (model, a, LT_EXPR, int_3);
      ADD_UNSAT_CONSTRAINT (model, a_plus_one, GT_EXPR, int_3);
    }
  }

  /* "3 < a < 4" should be impossible for integer a.  */
  {
    region_model_manager mgr;
    {
      region_model model (&mgr);
      ADD_SAT_CONSTRAINT (model, int_3, LT_EXPR, a);
      ADD_UNSAT_CONSTRAINT (model, a, LT_EXPR, int_4);
    }
    {
      region_model model (&mgr);
      ADD_SAT_CONSTRAINT (model, int_1, LT_EXPR, a);
      ADD_SAT_CONSTRAINT (model, int_3, LT_EXPR, a);
      ADD_SAT_CONSTRAINT (model, a, LT_EXPR, int_5);
      ADD_UNSAT_CONSTRAINT (model, a, LT_EXPR, int_4);
    }
    {
      region_model model (&mgr);
      ADD_SAT_CONSTRAINT (model, int_1, LT_EXPR, a);
      ADD_SAT_CONSTRAINT (model, a, LT_EXPR, int_5);
      ADD_SAT_CONSTRAINT (model, int_3, LT_EXPR, a);
      ADD_UNSAT_CONSTRAINT (model, a, LT_EXPR, int_4);
    }
    {
      region_model model (&mgr);
      ADD_SAT_CONSTRAINT (model, a, LT_EXPR, int_4);
      ADD_UNSAT_CONSTRAINT (model, int_3, LT_EXPR, a);
    }
    {
      region_model model (&mgr);
      ADD_SAT_CONSTRAINT (model, a, GT_EXPR, int_3);
      ADD_UNSAT_CONSTRAINT (model, int_4, GT_EXPR, a);
    }
    {
      region_model model (&mgr);
      ADD_SAT_CONSTRAINT (model, int_4, GT_EXPR, a);
      ADD_UNSAT_CONSTRAINT (model, a, GT_EXPR, int_3);
    }
  }
}

/* Verify various lower-level implementation details about
   constraint_manager.  */

static void
test_constraint_impl ()
{
  tree int_42 = build_int_cst (integer_type_node, 42);
  tree int_0 = build_int_cst (integer_type_node, 0);

  tree x = build_global_decl ("x", integer_type_node);
  tree y = build_global_decl ("y", integer_type_node);
  tree z = build_global_decl ("z", integer_type_node);

  /* x == y.  */
  {
    region_model_manager mgr;
    region_model model (&mgr);

    ADD_SAT_CONSTRAINT (model, x, EQ_EXPR, y);

    /* Assert various things about the insides of model.  */
    constraint_manager *cm = model.get_constraints ();
    ASSERT_EQ (cm->m_constraints.length (), 0);
    ASSERT_EQ (cm->m_equiv_classes.length (), 1);
  }

  /* y <= z; x == y.  */
  {
    region_model_manager mgr;
    region_model model (&mgr);
    ASSERT_CONDITION_UNKNOWN (model, x, EQ_EXPR, y);
    ASSERT_CONDITION_UNKNOWN (model, x, GE_EXPR, z);

    ADD_SAT_CONSTRAINT (model, y, GE_EXPR, z);
    ASSERT_CONDITION_TRUE (model, y, GE_EXPR, z);
    ASSERT_CONDITION_UNKNOWN (model, x, GE_EXPR, z);

    ADD_SAT_CONSTRAINT (model, x, EQ_EXPR, y);

    /* Assert various things about the insides of model.  */
    constraint_manager *cm = model.get_constraints ();
    ASSERT_EQ (cm->m_constraints.length (), 1);
    ASSERT_EQ (cm->m_equiv_classes.length (), 2);

    /* Ensure that we merged the constraints.  */
    ASSERT_CONDITION_TRUE (model, x, GE_EXPR, z);
  }

  /* y <= z; y == x.  */
  {
    region_model_manager mgr;
    region_model model (&mgr);
    ASSERT_CONDITION_UNKNOWN (model, x, EQ_EXPR, y);
    ASSERT_CONDITION_UNKNOWN (model, x, GE_EXPR, z);

    ADD_SAT_CONSTRAINT (model, y, GE_EXPR, z);
    ASSERT_CONDITION_TRUE (model, y, GE_EXPR, z);
    ASSERT_CONDITION_UNKNOWN (model, x, GE_EXPR, z);

    ADD_SAT_CONSTRAINT (model, y, EQ_EXPR, x);

    /* Assert various things about the insides of model.  */
    constraint_manager *cm = model.get_constraints ();
    ASSERT_EQ (cm->m_constraints.length (), 1);
    ASSERT_EQ (cm->m_equiv_classes.length (), 2);

    /* Ensure that we merged the constraints.  */
    ASSERT_CONDITION_TRUE (model, x, GE_EXPR, z);
  }

  /* x == 0, then x != 42.  */
  {
    region_model_manager mgr;
    region_model model (&mgr);

    ADD_SAT_CONSTRAINT (model, x, EQ_EXPR, int_0);
    ADD_SAT_CONSTRAINT (model, x, NE_EXPR, int_42);

    /* Assert various things about the insides of model.  */
    constraint_manager *cm = model.get_constraints ();
    ASSERT_EQ (cm->m_constraints.length (), 0);
    ASSERT_EQ (cm->m_equiv_classes.length (), 1);
  }

  // TODO: selftest for merging ecs "in the middle"
  // where a non-final one gets overwritten

  // TODO: selftest where there are pre-existing constraints
}

/* Check that operator== and hashing works as expected for the
   various types.  */

static void
test_equality ()
{
  tree x = build_global_decl ("x", integer_type_node);
  tree y = build_global_decl ("y", integer_type_node);

  {
    region_model_manager mgr;
    region_model model0 (&mgr);
    region_model model1 (&mgr);

    constraint_manager *cm0 = model0.get_constraints ();
    constraint_manager *cm1 = model1.get_constraints ();

    ASSERT_EQ (cm0->hash (), cm1->hash ());
    ASSERT_EQ (*cm0, *cm1);

    ASSERT_EQ (model0.hash (), model1.hash ());
    ASSERT_EQ (model0, model1);

    ADD_SAT_CONSTRAINT (model1, x, EQ_EXPR, y);
    ASSERT_NE (cm0->hash (), cm1->hash ());
    ASSERT_NE (*cm0, *cm1);

    ASSERT_NE (model0.hash (), model1.hash ());
    ASSERT_NE (model0, model1);

    region_model model2 (&mgr);
    constraint_manager *cm2 = model2.get_constraints ();
    /* Make the same change to cm2.  */
    ADD_SAT_CONSTRAINT (model2, x, EQ_EXPR, y);
    ASSERT_EQ (cm1->hash (), cm2->hash ());
    ASSERT_EQ (*cm1, *cm2);

    ASSERT_EQ (model1.hash (), model2.hash ());
    ASSERT_EQ (model1, model2);
  }
}

/* Verify tracking inequality of a variable against many constants.  */

static void
test_many_constants ()
{
  region_model_manager mgr;
  program_point point (program_point::origin (mgr));
  tree a = build_global_decl ("a", integer_type_node);

  region_model model (&mgr);
  auto_vec<tree> constants;
  for (int i = 0; i < 20; i++)
    {
      tree constant = build_int_cst (integer_type_node, i);
      constants.safe_push (constant);
      ADD_SAT_CONSTRAINT (model, a, NE_EXPR, constant);

      /* Merge, and check the result.  */
      region_model other (model);

      region_model merged (&mgr);
      ASSERT_TRUE (model.can_merge_with_p (other, point, &merged));
      model.canonicalize ();
      merged.canonicalize ();
      ASSERT_EQ (model, merged);

      for (int j = 0; j <= i; j++)
	ASSERT_CONDITION_TRUE (model, a, NE_EXPR, constants[j]);
    }
}

/* Verify that purging state relating to a variable doesn't leave stray
   equivalence classes (after canonicalization).  */

static void
test_purging (void)
{
  tree int_0 = build_int_cst (integer_type_node, 0);
  tree a = build_global_decl ("a", integer_type_node);
  tree b = build_global_decl ("b", integer_type_node);

  /*  "a != 0".  */
  {
    region_model_manager mgr;
    region_model model (&mgr);
    ADD_SAT_CONSTRAINT (model, a, NE_EXPR, int_0);
    ASSERT_EQ (model.get_constraints ()->m_equiv_classes.length (), 2);
    ASSERT_EQ (model.get_constraints ()->m_constraints.length (), 1);

    /* Purge state for "a".  */
    const svalue *sval_a = model.get_rvalue (a, NULL);
    model.purge_state_involving (sval_a, NULL);
    model.canonicalize ();
    /* We should have an empty constraint_manager.  */
    ASSERT_EQ (model.get_constraints ()->m_equiv_classes.length (), 0);
    ASSERT_EQ (model.get_constraints ()->m_constraints.length (), 0);
  }

  /*  "a != 0" && "b != 0".  */
  {
    region_model_manager mgr;
    region_model model (&mgr);
    ADD_SAT_CONSTRAINT (model, a, NE_EXPR, int_0);
    ADD_SAT_CONSTRAINT (model, b, NE_EXPR, int_0);
    ASSERT_EQ (model.get_constraints ()->m_equiv_classes.length (), 3);
    ASSERT_EQ (model.get_constraints ()->m_constraints.length (), 2);

    /* Purge state for "a".  */
    const svalue *sval_a = model.get_rvalue (a, NULL);
    model.purge_state_involving (sval_a, NULL);
    model.canonicalize ();
    /* We should just have the constraint/ECs involving b != 0.  */
    ASSERT_EQ (model.get_constraints ()->m_equiv_classes.length (), 2);
    ASSERT_EQ (model.get_constraints ()->m_constraints.length (), 1);
    ASSERT_CONDITION_TRUE (model, b, NE_EXPR, int_0);
  }

  /*  "a != 0" && "b == 0".  */
  {
    region_model_manager mgr;
    region_model model (&mgr);
    ADD_SAT_CONSTRAINT (model, a, NE_EXPR, int_0);
    ADD_SAT_CONSTRAINT (model, b, EQ_EXPR, int_0);
    ASSERT_EQ (model.get_constraints ()->m_equiv_classes.length (), 2);
    ASSERT_EQ (model.get_constraints ()->m_constraints.length (), 1);

    /* Purge state for "a".  */
    const svalue *sval_a = model.get_rvalue (a, NULL);
    model.purge_state_involving (sval_a, NULL);
    model.canonicalize ();
    /* We should just have the EC involving b == 0.  */
    ASSERT_EQ (model.get_constraints ()->m_equiv_classes.length (), 1);
    ASSERT_EQ (model.get_constraints ()->m_constraints.length (), 0);
    ASSERT_CONDITION_TRUE (model, b, EQ_EXPR, int_0);
  }

  /*  "a == 0".  */
  {
    region_model_manager mgr;
    region_model model (&mgr);
    ADD_SAT_CONSTRAINT (model, a, EQ_EXPR, int_0);
    ASSERT_EQ (model.get_constraints ()->m_equiv_classes.length (), 1);
    ASSERT_EQ (model.get_constraints ()->m_constraints.length (), 0);

    /* Purge state for "a".  */
    const svalue *sval_a = model.get_rvalue (a, NULL);
    model.purge_state_involving (sval_a, NULL);
    model.canonicalize ();
    /* We should have an empty constraint_manager.  */
    ASSERT_EQ (model.get_constraints ()->m_equiv_classes.length (), 0);
    ASSERT_EQ (model.get_constraints ()->m_constraints.length (), 0);
  }

  /*  "a == 0" && "b != 0".  */
  {
    region_model_manager mgr;
    region_model model (&mgr);
    ADD_SAT_CONSTRAINT (model, a, EQ_EXPR, int_0);
    ADD_SAT_CONSTRAINT (model, b, NE_EXPR, int_0);
    ASSERT_EQ (model.get_constraints ()->m_equiv_classes.length (), 2);
    ASSERT_EQ (model.get_constraints ()->m_constraints.length (), 1);

    /* Purge state for "a".  */
    const svalue *sval_a = model.get_rvalue (a, NULL);
    model.purge_state_involving (sval_a, NULL);
    model.canonicalize ();
    /* We should just have the constraint/ECs involving b != 0.  */
    ASSERT_EQ (model.get_constraints ()->m_equiv_classes.length (), 2);
    ASSERT_EQ (model.get_constraints ()->m_constraints.length (), 1);
    ASSERT_CONDITION_TRUE (model, b, NE_EXPR, int_0);
  }

  /*  "a == 0" && "b == 0".  */
  {
    region_model_manager mgr;
    region_model model (&mgr);
    ADD_SAT_CONSTRAINT (model, a, EQ_EXPR, int_0);
    ADD_SAT_CONSTRAINT (model, b, EQ_EXPR, int_0);
    ASSERT_EQ (model.get_constraints ()->m_equiv_classes.length (), 1);
    ASSERT_EQ (model.get_constraints ()->m_constraints.length (), 0);

    /* Purge state for "a".  */
    const svalue *sval_a = model.get_rvalue (a, NULL);
    model.purge_state_involving (sval_a, NULL);
    model.canonicalize ();
    /* We should just have the EC involving b == 0.  */
    ASSERT_EQ (model.get_constraints ()->m_equiv_classes.length (), 1);
    ASSERT_EQ (model.get_constraints ()->m_constraints.length (), 0);
    ASSERT_CONDITION_TRUE (model, b, EQ_EXPR, int_0);
  }
}

/* Implementation detail of ASSERT_DUMP_BOUNDED_RANGES_EQ.  */

static void
assert_dump_bounded_range_eq (const location &loc,
			      const bounded_range &range,
			      const char *expected)
{
  auto_fix_quotes sentinel;
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  range.dump_to_pp (&pp, false);
  ASSERT_STREQ_AT (loc, pp_formatted_text (&pp), expected);
}

/* Assert that BR.dump (false) is EXPECTED.  */

#define ASSERT_DUMP_BOUNDED_RANGE_EQ(BR, EXPECTED) \
  SELFTEST_BEGIN_STMT							\
  assert_dump_bounded_range_eq ((SELFTEST_LOCATION), (BR), (EXPECTED)); \
  SELFTEST_END_STMT

/* Verify that bounded_range works as expected.  */

static void
test_bounded_range ()
{
  tree u8_0 = build_int_cst (unsigned_char_type_node, 0);
  tree u8_1 = build_int_cst (unsigned_char_type_node, 1);
  tree u8_64 = build_int_cst (unsigned_char_type_node, 64);
  tree u8_128 = build_int_cst (unsigned_char_type_node, 128);
  tree u8_255 = build_int_cst (unsigned_char_type_node, 255);

  tree s8_0 = build_int_cst (signed_char_type_node, 0);
  tree s8_1 = build_int_cst (signed_char_type_node, 1);
  tree s8_2 = build_int_cst (signed_char_type_node, 2);

  bounded_range br_u8_0 (u8_0, u8_0);
  ASSERT_DUMP_BOUNDED_RANGE_EQ (br_u8_0, "0");
  ASSERT_TRUE (br_u8_0.contains_p (u8_0));
  ASSERT_FALSE (br_u8_0.contains_p (u8_1));
  ASSERT_TRUE (br_u8_0.contains_p (s8_0));
  ASSERT_FALSE (br_u8_0.contains_p (s8_1));

  bounded_range br_u8_0_1 (u8_0, u8_1);
  ASSERT_DUMP_BOUNDED_RANGE_EQ (br_u8_0_1, "[0, 1]");

  bounded_range tmp (NULL_TREE, NULL_TREE);
  ASSERT_TRUE (br_u8_0.intersects_p (br_u8_0_1, &tmp));
  ASSERT_DUMP_BOUNDED_RANGE_EQ (tmp, "0");

  bounded_range br_u8_64_128 (u8_64, u8_128);
  ASSERT_DUMP_BOUNDED_RANGE_EQ (br_u8_64_128, "[64, 128]");

  ASSERT_FALSE (br_u8_0.intersects_p (br_u8_64_128, NULL));
  ASSERT_FALSE (br_u8_64_128.intersects_p (br_u8_0, NULL));

  bounded_range br_u8_128_255 (u8_128, u8_255);
  ASSERT_DUMP_BOUNDED_RANGE_EQ (br_u8_128_255, "[128, 255]");
  ASSERT_TRUE (br_u8_128_255.intersects_p (br_u8_64_128, &tmp));
  ASSERT_DUMP_BOUNDED_RANGE_EQ (tmp, "128");

  bounded_range br_s8_2 (s8_2, s8_2);
  ASSERT_DUMP_BOUNDED_RANGE_EQ (br_s8_2, "2");
  bounded_range br_s8_2_u8_255 (s8_2, u8_255);
  ASSERT_DUMP_BOUNDED_RANGE_EQ (br_s8_2_u8_255, "[2, 255]");
}

/* Implementation detail of ASSERT_DUMP_BOUNDED_RANGES_EQ.  */

static void
assert_dump_bounded_ranges_eq (const location &loc,
			       const bounded_ranges *ranges,
			       const char *expected)
{
  auto_fix_quotes sentinel;
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  ranges->dump_to_pp (&pp, false);
  ASSERT_STREQ_AT (loc, pp_formatted_text (&pp), expected);
}

/* Implementation detail of ASSERT_DUMP_BOUNDED_RANGES_EQ.  */

static void
assert_dump_bounded_ranges_eq (const location &loc,
			       const bounded_ranges &ranges,
			       const char *expected)
{
  auto_fix_quotes sentinel;
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  ranges.dump_to_pp (&pp, false);
  ASSERT_STREQ_AT (loc, pp_formatted_text (&pp), expected);
}

/* Assert that BRS.dump (false) is EXPECTED.  */

#define ASSERT_DUMP_BOUNDED_RANGES_EQ(BRS, EXPECTED) \
  SELFTEST_BEGIN_STMT							\
  assert_dump_bounded_ranges_eq ((SELFTEST_LOCATION), (BRS), (EXPECTED)); \
  SELFTEST_END_STMT

/* Verify that the bounded_ranges class works as expected.  */

static void
test_bounded_ranges ()
{
  bounded_ranges_manager mgr;

  tree ch0 = build_int_cst (unsigned_char_type_node, 0);
  tree ch1 = build_int_cst (unsigned_char_type_node, 1);
  tree ch2 = build_int_cst (unsigned_char_type_node, 2);
  tree ch3 = build_int_cst (unsigned_char_type_node, 3);
  tree ch128 = build_int_cst (unsigned_char_type_node, 128);
  tree ch129 = build_int_cst (unsigned_char_type_node, 129);
  tree ch254 = build_int_cst (unsigned_char_type_node, 254);
  tree ch255 = build_int_cst (unsigned_char_type_node, 255);

  const bounded_ranges *empty = mgr.get_or_create_empty ();
  ASSERT_DUMP_BOUNDED_RANGES_EQ (empty, "{}");

  const bounded_ranges *point0 = mgr.get_or_create_point (ch0);
  ASSERT_DUMP_BOUNDED_RANGES_EQ (point0, "{0}");

  const bounded_ranges *point1 = mgr.get_or_create_point (ch1);
  ASSERT_DUMP_BOUNDED_RANGES_EQ (point1, "{1}");

  const bounded_ranges *point2 = mgr.get_or_create_point (ch2);
  ASSERT_DUMP_BOUNDED_RANGES_EQ (point2, "{2}");

  const bounded_ranges *range0_128 = mgr.get_or_create_range (ch0, ch128);
  ASSERT_DUMP_BOUNDED_RANGES_EQ (range0_128, "{[0, 128]}");

  const bounded_ranges *range0_255 = mgr.get_or_create_range (ch0, ch255);
  ASSERT_DUMP_BOUNDED_RANGES_EQ (range0_255, "{[0, 255]}");

  ASSERT_FALSE (empty->contain_p (ch0));
  ASSERT_FALSE (empty->contain_p (ch1));
  ASSERT_FALSE (empty->contain_p (ch255));

  ASSERT_TRUE (point0->contain_p (ch0));
  ASSERT_FALSE (point0->contain_p (ch1));
  ASSERT_FALSE (point0->contain_p (ch255));

  ASSERT_FALSE (point1->contain_p (ch0));
  ASSERT_TRUE (point1->contain_p (ch1));
  ASSERT_FALSE (point0->contain_p (ch255));

  ASSERT_TRUE (range0_128->contain_p (ch0));
  ASSERT_TRUE (range0_128->contain_p (ch1));
  ASSERT_TRUE (range0_128->contain_p (ch128));
  ASSERT_FALSE (range0_128->contain_p (ch129));
  ASSERT_FALSE (range0_128->contain_p (ch254));
  ASSERT_FALSE (range0_128->contain_p (ch255));

  const bounded_ranges *inv0_128
    = mgr.get_or_create_inverse (range0_128, unsigned_char_type_node);
  ASSERT_DUMP_BOUNDED_RANGES_EQ (inv0_128, "{[129, 255]}");

  const bounded_ranges *range128_129 = mgr.get_or_create_range (ch128, ch129);
  ASSERT_DUMP_BOUNDED_RANGES_EQ (range128_129, "{[128, 129]}");

  const bounded_ranges *inv128_129
    = mgr.get_or_create_inverse (range128_129, unsigned_char_type_node);
  ASSERT_DUMP_BOUNDED_RANGES_EQ (inv128_129, "{[0, 127], [130, 255]}");

  /* Intersection.  */
  {
    /* Intersection of disjoint ranges should be empty set.  */
    const bounded_ranges *intersect0_1
      = mgr.get_or_create_intersection (point0, point1);
    ASSERT_DUMP_BOUNDED_RANGES_EQ (intersect0_1, "{}");
  }

  /* Various tests of "union of ranges".  */
  {
    {
      /* Touching points should be merged into a range.  */
      auto_vec <const bounded_ranges *> v;
      v.safe_push (point0);
      v.safe_push (point1);
      const bounded_ranges *union_0_and_1 = mgr.get_or_create_union (v);
      ASSERT_DUMP_BOUNDED_RANGES_EQ (union_0_and_1, "{[0, 1]}");
    }

    {
      /* Overlapping and out-of-order.  */
      auto_vec <const bounded_ranges *> v;
      v.safe_push (inv0_128); // {[129, 255]}
      v.safe_push (range128_129);
      const bounded_ranges *union_129_255_and_128_129
	= mgr.get_or_create_union (v);
      ASSERT_DUMP_BOUNDED_RANGES_EQ (union_129_255_and_128_129, "{[128, 255]}");
    }

    {
      /* Union of R and inverse(R) should be full range of type.  */
      auto_vec <const bounded_ranges *> v;
      v.safe_push (range128_129);
      v.safe_push (inv128_129);
      const bounded_ranges *union_ = mgr.get_or_create_union (v);
      ASSERT_DUMP_BOUNDED_RANGES_EQ (union_, "{[0, 255]}");
    }

    /* Union with an endpoint.  */
    {
      const bounded_ranges *range2_to_255
	= mgr.get_or_create_range (ch2, ch255);
      ASSERT_DUMP_BOUNDED_RANGES_EQ (range2_to_255, "{[2, 255]}");
      auto_vec <const bounded_ranges *> v;
      v.safe_push (point0);
      v.safe_push (point2);
      v.safe_push (range2_to_255);
      const bounded_ranges *union_ = mgr.get_or_create_union (v);
      ASSERT_DUMP_BOUNDED_RANGES_EQ (union_, "{0, [2, 255]}");
    }

    /* Construct from vector of bounded_range.  */
    {
      auto_vec<bounded_range> v;
      v.safe_push (bounded_range (ch2, ch2));
      v.safe_push (bounded_range (ch0, ch0));
      v.safe_push (bounded_range (ch2, ch255));
      bounded_ranges br (v);
      ASSERT_DUMP_BOUNDED_RANGES_EQ (&br, "{0, [2, 255]}");
    }
  }

  /* Various tests of "inverse".  */
  {
    {
      const bounded_ranges *range_1_to_3 = mgr.get_or_create_range (ch1, ch3);
      ASSERT_DUMP_BOUNDED_RANGES_EQ (range_1_to_3, "{[1, 3]}");
      const bounded_ranges *inv
	= mgr.get_or_create_inverse (range_1_to_3, unsigned_char_type_node);
      ASSERT_DUMP_BOUNDED_RANGES_EQ (inv, "{0, [4, 255]}");
    }
    {
      const bounded_ranges *range_1_to_255
	= mgr.get_or_create_range (ch1, ch255);
      ASSERT_DUMP_BOUNDED_RANGES_EQ (range_1_to_255, "{[1, 255]}");
      const bounded_ranges *inv
	= mgr.get_or_create_inverse (range_1_to_255, unsigned_char_type_node);
      ASSERT_DUMP_BOUNDED_RANGES_EQ (inv, "{0}");
    }
    {
      const bounded_ranges *range_0_to_254
	= mgr.get_or_create_range (ch0, ch254);
      ASSERT_DUMP_BOUNDED_RANGES_EQ (range_0_to_254, "{[0, 254]}");
      const bounded_ranges *inv
	= mgr.get_or_create_inverse (range_0_to_254, unsigned_char_type_node);
      ASSERT_DUMP_BOUNDED_RANGES_EQ (inv, "{255}");
    }
  }

  /* "case 'a'-'z': case 'A-Z':" vs "default:", for ASCII.  */
  {
    tree ch65 = build_int_cst (unsigned_char_type_node, 65);
    tree ch90 = build_int_cst (unsigned_char_type_node, 90);

    tree ch97 = build_int_cst (unsigned_char_type_node, 97);
    tree ch122 = build_int_cst (unsigned_char_type_node, 122);

    const bounded_ranges *A_to_Z = mgr.get_or_create_range (ch65, ch90);
    ASSERT_DUMP_BOUNDED_RANGES_EQ (A_to_Z, "{[65, 90]}");
    const bounded_ranges *a_to_z = mgr.get_or_create_range (ch97, ch122);
    ASSERT_DUMP_BOUNDED_RANGES_EQ (a_to_z, "{[97, 122]}");
    auto_vec <const bounded_ranges *> v;
    v.safe_push (A_to_Z);
    v.safe_push (a_to_z);
    const bounded_ranges *label_ranges = mgr.get_or_create_union (v);
    ASSERT_DUMP_BOUNDED_RANGES_EQ (label_ranges, "{[65, 90], [97, 122]}");
    const bounded_ranges *default_ranges
      = mgr.get_or_create_inverse (label_ranges, unsigned_char_type_node);
    ASSERT_DUMP_BOUNDED_RANGES_EQ (default_ranges,
				   "{[0, 64], [91, 96], [123, 255]}");
  }

  /* Verify ranges from ops.  */
  ASSERT_DUMP_BOUNDED_RANGES_EQ (bounded_ranges (EQ_EXPR, ch128),
				 "{128}");
  ASSERT_DUMP_BOUNDED_RANGES_EQ (bounded_ranges (NE_EXPR, ch128),
				 "{[0, 127], [129, 255]}");
  ASSERT_DUMP_BOUNDED_RANGES_EQ (bounded_ranges (LT_EXPR, ch128),
				 "{[0, 127]}");
  ASSERT_DUMP_BOUNDED_RANGES_EQ (bounded_ranges (LE_EXPR, ch128),
				 "{[0, 128]}");
  ASSERT_DUMP_BOUNDED_RANGES_EQ (bounded_ranges (GE_EXPR, ch128),
				 "{[128, 255]}");
  ASSERT_DUMP_BOUNDED_RANGES_EQ (bounded_ranges (GT_EXPR, ch128),
				 "{[129, 255]}");
  /* Ops at endpoints of type ranges.  */
  ASSERT_DUMP_BOUNDED_RANGES_EQ (bounded_ranges (LE_EXPR, ch0),
				 "{0}");
  ASSERT_DUMP_BOUNDED_RANGES_EQ (bounded_ranges (LT_EXPR, ch0),
				 "{}");
  ASSERT_DUMP_BOUNDED_RANGES_EQ (bounded_ranges (NE_EXPR, ch0),
				 "{[1, 255]}");
  ASSERT_DUMP_BOUNDED_RANGES_EQ (bounded_ranges (GE_EXPR, ch255),
				 "{255}");
  ASSERT_DUMP_BOUNDED_RANGES_EQ (bounded_ranges (GT_EXPR, ch255),
				 "{}");
  ASSERT_DUMP_BOUNDED_RANGES_EQ (bounded_ranges (NE_EXPR, ch255),
				 "{[0, 254]}");

  /* Verify that instances are consolidated by mgr.  */
  ASSERT_EQ (mgr.get_or_create_point (ch0),
	     mgr.get_or_create_point (ch0));
  ASSERT_NE (mgr.get_or_create_point (ch0),
	     mgr.get_or_create_point (ch1));
}

/* Verify that we can handle sufficiently simple bitmasking operations.  */

static void
test_bits (void)
{
  region_model_manager mgr;

  tree int_0 = build_int_cst (integer_type_node, 0);
  tree int_0x80 = build_int_cst (integer_type_node, 0x80);
  tree int_0xff = build_int_cst (integer_type_node, 0xff);
  tree x = build_global_decl ("x", integer_type_node);

  tree x_bit_and_0x80 = build2 (BIT_AND_EXPR, integer_type_node, x, int_0x80);
  tree x_bit_and_0xff = build2 (BIT_AND_EXPR, integer_type_node, x, int_0xff);

  /* "x & 0x80 == 0x80".  */
  {
    region_model model (&mgr);
    ADD_SAT_CONSTRAINT (model, x_bit_and_0x80, EQ_EXPR, int_0x80);
    ASSERT_CONDITION_FALSE (model, x, EQ_EXPR, int_0);
    ASSERT_CONDITION_UNKNOWN (model, x, EQ_EXPR, int_0x80);
  }

  /* "x & 0x80 != 0x80".  */
  {
    region_model model (&mgr);
    ADD_SAT_CONSTRAINT (model, x_bit_and_0x80, NE_EXPR, int_0x80);
    ASSERT_CONDITION_UNKNOWN (model, x, EQ_EXPR, int_0);
    ASSERT_CONDITION_FALSE (model, x, EQ_EXPR, int_0x80);
  }

  /* "x & 0x80 == 0".  */
  {
    region_model model (&mgr);

    ADD_SAT_CONSTRAINT (model, x_bit_and_0x80, EQ_EXPR, int_0);
    ASSERT_CONDITION_UNKNOWN (model, x, EQ_EXPR, int_0);
    ASSERT_CONDITION_FALSE (model, x, EQ_EXPR, int_0x80);
  }

  /* "x & 0x80 != 0".  */
  {
    region_model model (&mgr);
    ADD_SAT_CONSTRAINT (model, x_bit_and_0x80, NE_EXPR, int_0);
    ASSERT_CONDITION_FALSE (model, x, EQ_EXPR, int_0);
    ASSERT_CONDITION_UNKNOWN (model, x, EQ_EXPR, int_0x80);
  }

  /* More that one bit in the mask.  */

  /* "x & 0xff == 0x80".  */
  {
    region_model model (&mgr);
    ADD_SAT_CONSTRAINT (model, x_bit_and_0xff, EQ_EXPR, int_0x80);
    ASSERT_CONDITION_FALSE (model, x, EQ_EXPR, int_0);
    ASSERT_CONDITION_UNKNOWN (model, x, EQ_EXPR, int_0x80);
    ASSERT_CONDITION_FALSE (model, x, EQ_EXPR, int_0xff);
  }

  /* "x & 0xff != 0x80".  */
  {
    region_model model (&mgr);
    ADD_SAT_CONSTRAINT (model, x_bit_and_0xff, NE_EXPR, int_0x80);
    ASSERT_CONDITION_UNKNOWN (model, x, EQ_EXPR, int_0);
    ASSERT_CONDITION_FALSE (model, x, EQ_EXPR, int_0x80);
    ASSERT_CONDITION_UNKNOWN (model, x, EQ_EXPR, int_0xff);
  }

  /* "x & 0xff == 0".  */
  {
    region_model model (&mgr);

    ADD_SAT_CONSTRAINT (model, x_bit_and_0xff, EQ_EXPR, int_0);
    ASSERT_CONDITION_UNKNOWN (model, x, EQ_EXPR, int_0);
    ASSERT_CONDITION_FALSE (model, x, EQ_EXPR, int_0x80);
    ASSERT_CONDITION_FALSE (model, x, EQ_EXPR, int_0xff);
  }

  /* "x & 0xff != 0".  */
  {
    region_model model (&mgr);
    ADD_SAT_CONSTRAINT (model, x_bit_and_0xff, NE_EXPR, int_0);
    ASSERT_CONDITION_FALSE (model, x, EQ_EXPR, int_0);
    ASSERT_CONDITION_UNKNOWN (model, x, EQ_EXPR, int_0x80);
    ASSERT_CONDITION_UNKNOWN (model, x, EQ_EXPR, int_0xff);
  }
}

/* Run the selftests in this file, temporarily overriding
   flag_analyzer_transitivity with TRANSITIVITY.  */

static void
run_constraint_manager_tests (bool transitivity)
{
  int saved_flag_analyzer_transitivity = flag_analyzer_transitivity;
  flag_analyzer_transitivity = transitivity;

  test_range ();
  test_constraint_conditions ();
  if (flag_analyzer_transitivity)
    {
      /* These selftests assume transitivity.  */
      test_transitivity ();
    }
  test_constant_comparisons ();
  test_constraint_impl ();
  test_equality ();
  test_many_constants ();
  test_purging ();
  test_bounded_range ();
  test_bounded_ranges ();
  test_bits ();

  flag_analyzer_transitivity = saved_flag_analyzer_transitivity;
}

/* Run all of the selftests within this file.  */

void
analyzer_constraint_manager_cc_tests ()
{
  /* Run the tests twice: with and without transitivity.  */
  run_constraint_manager_tests (true);
  run_constraint_manager_tests (false);
}

} // namespace selftest

#endif /* CHECKING_P */

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
