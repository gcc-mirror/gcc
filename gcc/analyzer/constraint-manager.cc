/* Tracking equivalence classes and constraints at a point on an execution path.
   Copyright (C) 2019-2020 Free Software Foundation, Inc.
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
#include "fold-const.h"
#include "selftest.h"
#include "diagnostic-core.h"
#include "graphviz.h"
#include "function.h"
#include "analyzer/analyzer.h"
#include "ordered-hash-map.h"
#include "options.h"
#include "cgraph.h"
#include "cfg.h"
#include "digraph.h"
#include "analyzer/supergraph.h"
#include "sbitmap.h"
#include "bitmap.h"
#include "tristate.h"
#include "analyzer/region-model.h"
#include "analyzer/constraint-manager.h"
#include "analyzer/analyzer-selftests.h"

#if ENABLE_ANALYZER

namespace ana {

/* One of the end-points of a range.  */

struct bound
{
  bound () : m_constant (NULL_TREE), m_closed (false) {}
  bound (tree constant, bool closed)
  : m_constant (constant), m_closed (closed) {}

  void ensure_closed (bool is_upper);

  const char * get_relation_as_str () const;

  tree m_constant;
  bool m_closed;
};

/* A range of values, used for determining if a value has been
   constrained to just one possible constant value.  */

struct range
{
  range () : m_lower_bound (), m_upper_bound () {}
  range (const bound &lower, const bound &upper)
  : m_lower_bound (lower), m_upper_bound (upper) {}

  void dump (pretty_printer *pp) const;

  bool constrained_to_single_element (tree *out);

  bound m_lower_bound;
  bound m_upper_bound;
};

/* struct bound.  */

/* Ensure that this bound is closed by converting an open bound to a
   closed one.  */

void
bound::ensure_closed (bool is_upper)
{
  if (!m_closed)
    {
      /* Offset by 1 in the appropriate direction.
	 For example, convert 3 < x into 4 <= x,
	 and convert x < 5 into x <= 4.  */
      gcc_assert (CONSTANT_CLASS_P (m_constant));
      m_constant = fold_build2 (is_upper ? MINUS_EXPR : PLUS_EXPR,
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
range::dump (pretty_printer *pp) const
{
  pp_printf (pp, "%qE %s x %s %qE",
	     m_lower_bound.m_constant,
	     m_lower_bound.get_relation_as_str (),
	     m_upper_bound.get_relation_as_str (),
	     m_upper_bound.m_constant);
}

/* Determine if there is only one possible value for this range.
   If so, return true and write the constant to *OUT.
   Otherwise, return false.  */

bool
range::constrained_to_single_element (tree *out)
{
  if (!INTEGRAL_TYPE_P (TREE_TYPE (m_lower_bound.m_constant)))
    return false;
  if (!INTEGRAL_TYPE_P (TREE_TYPE (m_upper_bound.m_constant)))
    return false;

  /* Convert any open bounds to closed bounds.  */
  m_lower_bound.ensure_closed (false);
  m_upper_bound.ensure_closed (true);

  // Are they equal?
  tree comparison = fold_binary (EQ_EXPR, boolean_type_node,
				 m_lower_bound.m_constant,
				 m_upper_bound.m_constant);
  if (comparison == boolean_true_node)
    {
      *out = m_lower_bound.m_constant;
      return true;
    }
  else
    return false;
}

/* class equiv_class.  */

/* equiv_class's default ctor.  */

equiv_class::equiv_class ()
: m_constant (NULL_TREE), m_cst_sid (svalue_id::null ()),
  m_vars ()
{
}

/* equiv_class's copy ctor.  */

equiv_class::equiv_class (const equiv_class &other)
: m_constant (other.m_constant), m_cst_sid (other.m_cst_sid),
  m_vars (other.m_vars.length ())
{
  int i;
  svalue_id *sid;
  FOR_EACH_VEC_ELT (other.m_vars, i, sid)
    m_vars.quick_push (*sid);
}

/* Print an all-on-one-line representation of this equiv_class to PP,
   which must support %E for trees.  */

void
equiv_class::print (pretty_printer *pp) const
{
  pp_character (pp, '{');
  int i;
  svalue_id *sid;
  FOR_EACH_VEC_ELT (m_vars, i, sid)
    {
      if (i > 0)
	pp_string (pp, " == ");
      sid->print (pp);
    }
  if (m_constant)
    {
      if (i > 0)
	pp_string (pp, " == ");
      pp_printf (pp, "%qE", m_constant);
    }
  pp_character (pp, '}');
}

/* Generate a hash value for this equiv_class.  */

hashval_t
equiv_class::hash () const
{
  inchash::hash hstate;
  int i;
  svalue_id *sid;

  inchash::add_expr (m_constant, hstate);
  FOR_EACH_VEC_ELT (m_vars, i, sid)
    inchash::add (*sid, hstate);
  return hstate.end ();
}

/* Equality operator for equiv_class.  */

bool
equiv_class::operator== (const equiv_class &other)
{
  if (m_constant != other.m_constant)
    return false; // TODO: use tree equality here?

  /* FIXME: should we compare m_cst_sid?  */

  if (m_vars.length () != other.m_vars.length ())
    return false;

  int i;
  svalue_id *sid;
  FOR_EACH_VEC_ELT (m_vars, i, sid)
    if (! (*sid == other.m_vars[i]))
      return false;

  return true;
}

/* Add SID to this equiv_class, using CM to check if it's a constant.  */

void
equiv_class::add (svalue_id sid, const constraint_manager &cm)
{
  gcc_assert (!sid.null_p ());
  if (tree cst = cm.maybe_get_constant (sid))
    {
      gcc_assert (CONSTANT_CLASS_P (cst));
      /* FIXME: should we canonicalize which svalue is the constant
	 when there are multiple equal constants?  */
      m_constant = cst;
      m_cst_sid = sid;
    }
  m_vars.safe_push (sid);
}

/* Remove SID from this equivalence class.
   Return true if SID was the last var in the equivalence class (suggesting
   a possible leak).  */

bool
equiv_class::del (svalue_id sid)
{
  gcc_assert (!sid.null_p ());
  gcc_assert (sid != m_cst_sid);

  int i;
  svalue_id *iv;
  FOR_EACH_VEC_ELT (m_vars, i, iv)
    {
      if (*iv == sid)
	{
	  m_vars[i] = m_vars[m_vars.length () - 1];
	  m_vars.pop ();
	  return m_vars.length () == 0;
	}
    }

  /* SID must be in the class.  */
  gcc_unreachable ();
  return false;
}

/* Get a representative member of this class, for handling cases
   where the IDs can change mid-traversal.  */

svalue_id
equiv_class::get_representative () const
{
  if (!m_cst_sid.null_p ())
    return m_cst_sid;
  else
    {
      gcc_assert (m_vars.length () > 0);
      return m_vars[0];
    }
}

/* Remap all svalue_ids within this equiv_class using MAP.  */

void
equiv_class::remap_svalue_ids (const svalue_id_map &map)
{
  int i;
  svalue_id *iv;
  FOR_EACH_VEC_ELT (m_vars, i, iv)
    map.update (iv);
  map.update (&m_cst_sid);
}

/* Comparator for use by equiv_class::canonicalize.  */

static int
svalue_id_cmp_by_id (const void *p1, const void *p2)
{
  const svalue_id *sid1 = (const svalue_id *)p1;
  const svalue_id *sid2 = (const svalue_id *)p2;
  return sid1->as_int () - sid2->as_int ();
}

/* Sort the svalues_ids within this equiv_class.  */

void
equiv_class::canonicalize ()
{
  m_vars.qsort (svalue_id_cmp_by_id);
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
  m_constraints (other.m_constraints.length ())
{
  int i;
  equiv_class *ec;
  FOR_EACH_VEC_ELT (other.m_equiv_classes, i, ec)
    m_equiv_classes.quick_push (new equiv_class (*ec));
  constraint *c;
  FOR_EACH_VEC_ELT (other.m_constraints, i, c)
    m_constraints.quick_push (*c);
}

/* constraint_manager's assignment operator.  */

constraint_manager&
constraint_manager::operator= (const constraint_manager &other)
{
  gcc_assert (m_equiv_classes.length () == 0);
  gcc_assert (m_constraints.length () == 0);

  int i;
  equiv_class *ec;
  m_equiv_classes.reserve (other.m_equiv_classes.length ());
  FOR_EACH_VEC_ELT (other.m_equiv_classes, i, ec)
    m_equiv_classes.quick_push (new equiv_class (*ec));
  constraint *c;
  m_constraints.reserve (other.m_constraints.length ());
  FOR_EACH_VEC_ELT (other.m_constraints, i, c)
    m_constraints.quick_push (*c);

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

  int i;
  equiv_class *ec;

  FOR_EACH_VEC_ELT (m_equiv_classes, i, ec)
    if (!(*ec == *other.m_equiv_classes[i]))
      return false;

  constraint *c;

  FOR_EACH_VEC_ELT (m_constraints, i, c)
    if (!(*c == other.m_constraints[i]))
      return false;

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
  pp_printf (pp, "}");
}

/* Dump a multiline representation of this constraint_manager to PP
   (which must support %E for trees).  */

void
constraint_manager::dump_to_pp (pretty_printer *pp) const
{
  // TODO
  pp_string (pp, "  equiv classes:");
  pp_newline (pp);
  int i;
  equiv_class *ec;
  FOR_EACH_VEC_ELT (m_equiv_classes, i, ec)
    {
      pp_string (pp, "    ");
      equiv_class_id (i).print (pp);
      pp_string (pp, ": ");
      ec->print (pp);
      pp_newline (pp);
    }
  pp_string (pp, "  constraints:");
  pp_newline (pp);
  constraint *c;
  FOR_EACH_VEC_ELT (m_constraints, i, c)
    {
      pp_printf (pp, "    %i: ", i);
      c->print (pp, *this);
      pp_newline (pp);
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
  dump_to_pp (&pp);
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

/* Attempt to add the constraint LHS OP RHS to this constraint_manager.
   Return true if the constraint could be added (or is already true).
   Return false if the constraint contradicts existing knowledge.  */

bool
constraint_manager::add_constraint (svalue_id lhs,
				    enum tree_code op,
				    svalue_id rhs)
{
  equiv_class_id lhs_ec_id = get_or_add_equiv_class (lhs);
  equiv_class_id rhs_ec_id = get_or_add_equiv_class (rhs);
  return add_constraint (lhs_ec_id, op,rhs_ec_id);
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
	svalue_id *sid;
	FOR_EACH_VEC_ELT (rhs_ec_obj.m_vars, i, sid)
	  lhs_ec_obj.add (*sid, *this);

	if (rhs_ec_obj.m_constant)
	  {
	    lhs_ec_obj.m_constant = rhs_ec_obj.m_constant;
	    lhs_ec_obj.m_cst_sid = rhs_ec_obj.m_cst_sid;
	  }

	/* Drop rhs equivalence class, overwriting it with the
	   final ec (which might be the same one).  */
	equiv_class_id final_ec_id = m_equiv_classes.length () - 1;
	equiv_class *old_ec = m_equiv_classes[rhs_ec_id.m_idx];
	equiv_class *final_ec = m_equiv_classes.pop ();
	if (final_ec != old_ec)
	  m_equiv_classes[rhs_ec_id.m_idx] = final_ec;
	delete old_ec;

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
  return true;
}

/* Subroutine of constraint_manager::add_constraint, for handling all
   operations other than equality (for which equiv classes are merged).  */

void
constraint_manager::add_constraint_internal (equiv_class_id lhs_id,
					     enum constraint_op c_op,
					     equiv_class_id rhs_id)
{
  /* Add the constraint.  */
  m_constraints.safe_push (constraint (lhs_id, c_op, rhs_id));

  if (!flag_analyzer_transitivity)
    return;

  if (c_op != CONSTRAINT_NE)
    {
      /* The following can potentially add EQ_EXPR facts, which could lead
	 to ECs being merged, which would change the meaning of the EC IDs.
	 Hence we need to do this via representatives.  */
      svalue_id lhs = lhs_id.get_obj (*this).get_representative ();
      svalue_id rhs = rhs_id.get_obj (*this).get_representative ();

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
		      tree constant;
		      if (r.constrained_to_single_element (&constant))
			{
			  svalue_id cst_sid = get_sid_for_constant (constant);
			  add_constraint
			    (rhs_id, EQ_EXPR,
			     get_or_add_equiv_class (cst_sid));
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
		      tree constant;
		      if (r.constrained_to_single_element (&constant))
			{
			  svalue_id cst_sid = get_sid_for_constant (constant);
			  add_constraint
			    (lhs_id, EQ_EXPR,
			     get_or_add_equiv_class (cst_sid));
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

/* Look for SID within the equivalence classes of this constraint_manager;
   if found, write the id to *OUT and return true, otherwise return false.  */

bool
constraint_manager::get_equiv_class_by_sid (svalue_id sid, equiv_class_id *out) const
{
  /* TODO: should we have a map, rather than these searches?  */
  int i;
  equiv_class *ec;
  FOR_EACH_VEC_ELT (m_equiv_classes, i, ec)
    {
      int j;
      svalue_id *iv;
      FOR_EACH_VEC_ELT (ec->m_vars, j, iv)
	if (*iv == sid)
	  {
	    *out = equiv_class_id (i);
	    return true;
	  }
    }
  return false;
}

/* Ensure that SID has an equivalence class within this constraint_manager;
   return the ID of the class.  */

equiv_class_id
constraint_manager::get_or_add_equiv_class (svalue_id sid)
{
  equiv_class_id result (-1);

  /* Try svalue_id match.  */
  if (get_equiv_class_by_sid (sid, &result))
    return result;

  /* Try equality of constants.  */
  if (tree cst = maybe_get_constant (sid))
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
		ec->add (sid, *this);
		return equiv_class_id (i);
	      }
	  }
    }


  /* Not found.  */
  equiv_class *new_ec = new equiv_class ();
  new_ec->add (sid, *this);
  m_equiv_classes.safe_push (new_ec);

  equiv_class_id new_id (m_equiv_classes.length () - 1);

  if (maybe_get_constant (sid))
    {
      /* If we have a new EC for a constant, add constraints comparing this
	 to other constants we may have (so that we accumulate the transitive
	 closure of all constraints on constants as the constants are
	 added).  */
      for (equiv_class_id other_id (0); other_id.m_idx < new_id.m_idx;
	   other_id.m_idx++)
	{
	  const equiv_class &other_ec = other_id.get_obj (*this);
	  if (other_ec.m_constant
	      && types_compatible_p (TREE_TYPE (new_ec->m_constant),
				     TREE_TYPE (other_ec.m_constant)))
	    {
	      /* If we have two ECs, both with constants, the constants must be
		 non-equal (or they would be in the same EC).
		 Determine the direction of the inequality, and record that
		 fact.  */
	      tree lt
		= fold_binary (LT_EXPR, boolean_type_node,
			       new_ec->m_constant, other_ec.m_constant);
	      if (lt == boolean_true_node)
		add_constraint_internal (new_id, CONSTRAINT_LT, other_id);
	      else if (lt == boolean_false_node)
		add_constraint_internal (other_id, CONSTRAINT_LT, new_id);
	      /* Refresh new_id, in case ECs were merged.  SID should always
		 be present by now, so this should never lead to a
		 recursion.  */
	      new_id = get_or_add_equiv_class (sid);
	    }
	}
    }

  return new_id;
}

/* Evaluate the condition LHS_EC OP RHS_EC.  */

tristate
constraint_manager::eval_condition (equiv_class_id lhs_ec,
				    enum tree_code op,
				    equiv_class_id rhs_ec)
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
      tree comparison
	= fold_binary (op, boolean_type_node, lhs_const, rhs_const);
      if (comparison == boolean_true_node)
	return tristate (tristate::TS_TRUE);
      if (comparison == boolean_false_node)
	return tristate (tristate::TS_FALSE);
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

  return tristate (tristate::TS_UNKNOWN);
}

/* Evaluate the condition LHS OP RHS, creating equiv_class instances for
   LHS and RHS if they aren't already in equiv_classes.  */

tristate
constraint_manager::eval_condition (svalue_id lhs,
				    enum tree_code op,
				    svalue_id rhs)
{
  return eval_condition (get_or_add_equiv_class (lhs),
			 op,
			 get_or_add_equiv_class (rhs));
}

/* Delete any information about svalue_id instances identified by P.
   Such instances are removed from equivalence classes, and any
   redundant ECs and constraints are also removed.
   Accumulate stats into STATS.  */

void
constraint_manager::purge (const purge_criteria &p, purge_stats *stats)
{
  /* Delete any svalue_ids identified by P within the various equivalence
     classes.  */
  for (unsigned ec_idx = 0; ec_idx < m_equiv_classes.length (); )
    {
      equiv_class *ec = m_equiv_classes[ec_idx];

      int i;
      svalue_id *pv;
      bool delete_ec = false;
      FOR_EACH_VEC_ELT (ec->m_vars, i, pv)
	{
	  if (*pv == ec->m_cst_sid)
	    continue;
	  if (p.should_purge_p (*pv))
	    {
	      if (ec->del (*pv))
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
	      continue;
	    }
	}
      ec_idx++;
    }

  validate ();
}

/* Remap all svalue_ids within this constraint_manager using MAP.  */

void
constraint_manager::remap_svalue_ids (const svalue_id_map &map)
{
  int i;
  equiv_class *ec;
  FOR_EACH_VEC_ELT (m_equiv_classes, i, ec)
    ec->remap_svalue_ids (map);
}

/* Comparator for use by constraint_manager::canonicalize.
   Sort a pair of equiv_class instances, using the representative
   svalue_id as a sort key.  */

static int
equiv_class_cmp (const void *p1, const void *p2)
{
  const equiv_class *ec1 = *(const equiv_class * const *)p1;
  const equiv_class *ec2 = *(const equiv_class * const *)p2;

  svalue_id rep1 = ec1->get_representative ();
  svalue_id rep2 = ec2->get_representative ();

  return rep1.as_int () - rep2.as_int ();
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

/* Reorder the equivalence classes and constraints within this
   constraint_manager into a canonical order, to increase the
   chances of finding equality with another instance.  */

void
constraint_manager::canonicalize (unsigned num_svalue_ids)
{
  /* First, sort svalue_ids within the ECs.  */
  unsigned i;
  equiv_class *ec;
  FOR_EACH_VEC_ELT (m_equiv_classes, i, ec)
    ec->canonicalize ();

  /* Next, sort the ECs into a canonical order.  */

  /* We will need to remap the equiv_class_ids in the constraints,
     so we need to store the original index of each EC.
     Build a lookup table, mapping from representative svalue_id
     to the original equiv_class_id of that svalue_id.  */
  auto_vec<equiv_class_id> original_ec_id (num_svalue_ids);
  for (i = 0; i < num_svalue_ids; i++)
    original_ec_id.quick_push (equiv_class_id::null ());
  FOR_EACH_VEC_ELT (m_equiv_classes, i, ec)
    {
      svalue_id rep = ec->get_representative ();
      gcc_assert (!rep.null_p ());
      original_ec_id[rep.as_int ()] = i;
    }

  /* Sort the equivalence classes.  */
  m_equiv_classes.qsort (equiv_class_cmp);

  /* Populate ec_id_map based on the old vs new EC ids.  */
  one_way_id_map<equiv_class_id> ec_id_map (m_equiv_classes.length ());
  FOR_EACH_VEC_ELT (m_equiv_classes, i, ec)
    {
      svalue_id rep = ec->get_representative ();
      ec_id_map.put (original_ec_id[rep.as_int ()], i);
    }

  /* Update the EC ids within the constraints.  */
  constraint *c;
  FOR_EACH_VEC_ELT (m_constraints, i, c)
    {
      ec_id_map.update (&c->m_lhs);
      ec_id_map.update (&c->m_rhs);
    }

  /* Finally, sort the constraints. */
  m_constraints.qsort (constraint_cmp);
}

/* A concrete subclass of constraint_manager for use when
   merging two constraint_manager into a third constraint_manager,
   each of which has its own region_model.
   Calls are delegated to the constraint_manager for the merged model,
   and thus affect its region_model.  */

class cleaned_constraint_manager : public constraint_manager
{
public:
  cleaned_constraint_manager (constraint_manager *merged) : m_merged (merged) {}

  constraint_manager *clone (region_model *) const FINAL OVERRIDE
  {
    gcc_unreachable ();
  }
  tree maybe_get_constant (svalue_id sid) const FINAL OVERRIDE
  {
    return m_merged->maybe_get_constant (sid);
  }
  svalue_id get_sid_for_constant (tree cst) const FINAL OVERRIDE
  {
    return m_merged->get_sid_for_constant (cst);
  }
  virtual int get_num_svalues () const FINAL OVERRIDE
  {
    return m_merged->get_num_svalues ();
  }
private:
  constraint_manager *m_merged;
};

/* Concrete subclass of fact_visitor for use by constraint_manager::merge.
   For every fact in CM_A, see if it is also true in *CM_B.  Add such
   facts to *OUT.  */

class merger_fact_visitor : public fact_visitor
{
public:
  merger_fact_visitor (constraint_manager *cm_b,
		       constraint_manager *out)
  : m_cm_b (cm_b), m_out (out)
  {}

  void on_fact (svalue_id lhs, enum tree_code code, svalue_id rhs)
    FINAL OVERRIDE
  {
    if (m_cm_b->eval_condition (lhs, code, rhs).is_true ())
      {
	bool sat = m_out->add_constraint (lhs, code, rhs);
	gcc_assert (sat);
      }
  }

private:
  constraint_manager *m_cm_b;
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
			   constraint_manager *out,
			   const model_merger &merger)
{
  gcc_assert (merger.m_sid_mapping);

  /* Map svalue_ids in each equiv class from both sources
     to the merged region_model, dropping ids that don't survive merger,
     and potentially creating svalues in *OUT for constants.  */
  cleaned_constraint_manager cleaned_cm_a (out);
  const one_way_svalue_id_map &map_a_to_m
    = merger.m_sid_mapping->m_map_from_a_to_m;
  clean_merger_input (cm_a, map_a_to_m, &cleaned_cm_a);

  cleaned_constraint_manager cleaned_cm_b (out);
  const one_way_svalue_id_map &map_b_to_m
    = merger.m_sid_mapping->m_map_from_b_to_m;
  clean_merger_input (cm_b, map_b_to_m, &cleaned_cm_b);

  /* At this point, the two cleaned CMs have ECs and constraints referring
     to svalues in the merged region model, but both of them have separate
     ECs.  */

  /* Merge the equivalence classes and constraints.
     The easiest way to do this seems to be to enumerate all of the facts
     in cleaned_cm_a, see which are also true in cleaned_cm_b,
     and add those to *OUT.  */
  merger_fact_visitor v (&cleaned_cm_b, out);
  cleaned_cm_a.for_each_fact (&v);
}

/* A subroutine of constraint_manager::merge.
   Use MAP_SID_TO_M to map equivalence classes and constraints from
   SM_IN to *OUT.  Purge any non-constant svalue_id that don't appear
   in the result of MAP_SID_TO_M, purging any ECs and their constraints
   that become empty as a result.  Potentially create svalues in
   the merged region_model for constants that weren't already in use there.  */

void
constraint_manager::
clean_merger_input (const constraint_manager &cm_in,
		    const one_way_svalue_id_map &map_sid_to_m,
		    constraint_manager *out)
{
  one_way_id_map<equiv_class_id> map_ec_to_m
    (cm_in.m_equiv_classes.length ());
  unsigned ec_idx;
  equiv_class *ec;
  FOR_EACH_VEC_ELT (cm_in.m_equiv_classes, ec_idx, ec)
    {
      equiv_class cleaned_ec;
      if (tree cst = ec->get_any_constant ())
	{
	  cleaned_ec.m_constant = cst;
	  /* Lazily create the constant in the out region_model.  */
	  cleaned_ec.m_cst_sid = out->get_sid_for_constant (cst);
	}
      unsigned var_idx;
      svalue_id *var_in_sid;
      FOR_EACH_VEC_ELT (ec->m_vars, var_idx, var_in_sid)
	{
	  svalue_id var_m_sid = map_sid_to_m.get_dst_for_src (*var_in_sid);
	  if (!var_m_sid.null_p ())
	    cleaned_ec.m_vars.safe_push (var_m_sid);
	}
      if (cleaned_ec.get_any_constant () || !cleaned_ec.m_vars.is_empty ())
	{
	  map_ec_to_m.put (ec_idx, out->m_equiv_classes.length ());
	  out->m_equiv_classes.safe_push (new equiv_class (cleaned_ec));
	}
    }

  /* Write out to *OUT any constraints for which both sides survived
     cleaning, using the new EC IDs.  */
  unsigned con_idx;
  constraint *c;
  FOR_EACH_VEC_ELT (cm_in.m_constraints, con_idx, c)
    {
      equiv_class_id new_lhs = map_ec_to_m.get_dst_for_src (c->m_lhs);
      if (new_lhs.null_p ())
	continue;
      equiv_class_id new_rhs = map_ec_to_m.get_dst_for_src (c->m_rhs);
      if (new_rhs.null_p ())
	continue;
      out->m_constraints.safe_push (constraint (new_lhs,
						c->m_op,
						new_rhs));
    }
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
      if (!ec->m_cst_sid.null_p ())
	{
	  unsigned i;
	  svalue_id *sid;
	  FOR_EACH_VEC_ELT (ec->m_vars, i, sid)
	    visitor->on_fact (ec->m_cst_sid, EQ_EXPR, *sid);
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

      if (!ec_lhs.m_cst_sid.null_p ())
	{
	  for (unsigned j = 0; j < ec_rhs.m_vars.length (); j++)
	    {
	      visitor->on_fact (ec_lhs.m_cst_sid, code, ec_rhs.m_vars[j]);
	    }
	}
      for (unsigned i = 0; i < ec_lhs.m_vars.length (); i++)
	{
	  if (!ec_rhs.m_cst_sid.null_p ())
	    visitor->on_fact (ec_lhs.m_vars[i], code, ec_rhs.m_cst_sid);
	  for (unsigned j = 0; j < ec_rhs.m_vars.length (); j++)
	    visitor->on_fact (ec_lhs.m_vars[i], code, ec_rhs.m_vars[j]);
	}
    }
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
      svalue_id *sid;
      FOR_EACH_VEC_ELT (ec->m_vars, j, sid)
	{
	  gcc_assert (!sid->null_p ());
	  gcc_assert (sid->as_int () < get_num_svalues ());
	}
      if (ec->m_constant)
	{
	  gcc_assert (CONSTANT_CLASS_P (ec->m_constant));
	  gcc_assert (!ec->m_cst_sid.null_p ());
	  gcc_assert (ec->m_cst_sid.as_int () < get_num_svalues ());
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
      gcc_assert (c->m_lhs.as_int () <= (int)m_equiv_classes.length ());
      gcc_assert (!c->m_rhs.null_p ());
      gcc_assert (c->m_rhs.as_int () <= (int)m_equiv_classes.length ());
    }
}

#if CHECKING_P

namespace selftest {

/* Various constraint_manager selftests.
   These have to be written in terms of a region_model, since
   the latter is responsible for managing svalue and svalue_id
   instances.  */

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
    region_model model;
    ASSERT_CONDITION_TRUE (model, x, EQ_EXPR, x);
    ASSERT_CONDITION_TRUE (model, x, LE_EXPR, x);
    ASSERT_CONDITION_TRUE (model, x, GE_EXPR, x);
    ASSERT_CONDITION_FALSE (model, x, NE_EXPR, x);
    ASSERT_CONDITION_FALSE (model, x, LT_EXPR, x);
    ASSERT_CONDITION_FALSE (model, x, GT_EXPR, x);
  }

  /* x == y.  */
  {
    region_model model;
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
    region_model model;
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
    region_model model;

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
    region_model model;

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
    region_model model;

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
    region_model model;

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
    region_model model;

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
    region_model model;
    ASSERT_CONDITION_FALSE (model, int_0, EQ_EXPR, int_42);
    ASSERT_CONDITION_TRUE (model, int_0, NE_EXPR, int_42);
    ASSERT_CONDITION_TRUE (model, int_0, LT_EXPR, int_42);
    ASSERT_CONDITION_TRUE (model, int_0, LE_EXPR, int_42);
    ASSERT_CONDITION_FALSE (model, int_0, GT_EXPR, int_42);
    ASSERT_CONDITION_FALSE (model, int_0, GE_EXPR, int_42);
  }

  /* x == 0, y == 42.  */
  {
    region_model model;
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
    region_model model;
    ADD_SAT_CONSTRAINT (model, x, EQ_EXPR, y);
    ADD_UNSAT_CONSTRAINT (model, x, NE_EXPR, y);
  }

  /* x == 0 then x == 42.  */
  {
    region_model model;
    ADD_SAT_CONSTRAINT (model, x, EQ_EXPR, int_0);
    ADD_UNSAT_CONSTRAINT (model, x, EQ_EXPR, int_42);
  }

  /* x == 0 then x != 0.  */
  {
    region_model model;
    ADD_SAT_CONSTRAINT (model, x, EQ_EXPR, int_0);
    ADD_UNSAT_CONSTRAINT (model, x, NE_EXPR, int_0);
  }

  /* x == 0 then x > 0.  */
  {
    region_model model;
    ADD_SAT_CONSTRAINT (model, x, EQ_EXPR, int_0);
    ADD_UNSAT_CONSTRAINT (model, x, GT_EXPR, int_0);
  }

  /* x != y && x == y.  */
  {
    region_model model;
    ADD_SAT_CONSTRAINT (model, x, NE_EXPR, y);
    ADD_UNSAT_CONSTRAINT (model, x, EQ_EXPR, y);
  }

  /* x <= y && x > y.  */
  {
    region_model model;
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
    region_model model;
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
    region_model model;
    ADD_SAT_CONSTRAINT (model, a, LT_EXPR, b);
    ADD_SAT_CONSTRAINT (model, b, LT_EXPR, c);

    ASSERT_CONDITION_TRUE (model, a, LT_EXPR, c);
    ASSERT_CONDITION_FALSE (model, a, EQ_EXPR, c);
  }

  /* Transitivity: "a <= b", "b < c" should imply "a < c".  */
  {
    region_model model;
    ADD_SAT_CONSTRAINT (model, a, LE_EXPR, b);
    ADD_SAT_CONSTRAINT (model, b, LT_EXPR, c);

    ASSERT_CONDITION_TRUE (model, a, LT_EXPR, c);
    ASSERT_CONDITION_FALSE (model, a, EQ_EXPR, c);
  }

  /* Transitivity: "a <= b", "b <= c" should imply "a <= c".  */
  {
    region_model model;
    ADD_SAT_CONSTRAINT (model, a, LE_EXPR, b);
    ADD_SAT_CONSTRAINT (model, b, LE_EXPR, c);

    ASSERT_CONDITION_TRUE (model, a, LE_EXPR, c);
    ASSERT_CONDITION_UNKNOWN (model, a, EQ_EXPR, c);
  }

  /* Transitivity: "a > b", "b > c" should imply "a > c".  */
  {
    region_model model;
    ADD_SAT_CONSTRAINT (model, a, GT_EXPR, b);
    ADD_SAT_CONSTRAINT (model, b, GT_EXPR, c);

    ASSERT_CONDITION_TRUE (model, a, GT_EXPR, c);
    ASSERT_CONDITION_FALSE (model, a, EQ_EXPR, c);
  }

  /* Transitivity: "a >= b", "b > c" should imply " a > c".  */
  {
    region_model model;
    ADD_SAT_CONSTRAINT (model, a, GE_EXPR, b);
    ADD_SAT_CONSTRAINT (model, b, GT_EXPR, c);

    ASSERT_CONDITION_TRUE (model, a, GT_EXPR, c);
    ASSERT_CONDITION_FALSE (model, a, EQ_EXPR, c);
  }

  /* Transitivity: "a >= b", "b >= c" should imply "a >= c".  */
  {
    region_model model;
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
    region_model model;
    ADD_SAT_CONSTRAINT (model, a, LT_EXPR, b);
    ADD_SAT_CONSTRAINT (model, c, LT_EXPR, d);
    ADD_SAT_CONSTRAINT (model, b, LT_EXPR, c);

    ASSERT_CONDITION_TRUE (model, a, LT_EXPR, c);
    ASSERT_CONDITION_TRUE (model, b, LT_EXPR, d);
    ASSERT_CONDITION_TRUE (model, a, LT_EXPR, d);
  }

  /* Transitivity: "a >= b", "b >= a" should imply that a == b.  */
  {
    region_model model;
    ADD_SAT_CONSTRAINT (model, a, GE_EXPR, b);
    ADD_SAT_CONSTRAINT (model, b, GE_EXPR, a);

    // TODO:
    ASSERT_CONDITION_TRUE (model, a, EQ_EXPR, b);
  }

  /* Transitivity: "a >= b", "b > a" should be impossible.  */
  {
    region_model model;
    ADD_SAT_CONSTRAINT (model, a, GE_EXPR, b);
    ADD_UNSAT_CONSTRAINT (model, b, GT_EXPR, a);
  }

  /* Transitivity: "a >= b", "b >= c", "c >= a" should imply
     that a == b == c.  */
  {
    region_model model;
    ADD_SAT_CONSTRAINT (model, a, GE_EXPR, b);
    ADD_SAT_CONSTRAINT (model, b, GE_EXPR, c);
    ADD_SAT_CONSTRAINT (model, c, GE_EXPR, a);

    ASSERT_CONDITION_TRUE (model, a, EQ_EXPR, c);
  }

  /* Transitivity: "a > b", "b > c", "c > a"
     should be impossible.  */
  {
    region_model model;
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
  tree int_3 = build_int_cst (integer_type_node, 3);
  tree int_4 = build_int_cst (integer_type_node, 4);
  tree int_5 = build_int_cst (integer_type_node, 5);

  tree int_1023 = build_int_cst (integer_type_node, 1023);
  tree int_1024 = build_int_cst (integer_type_node, 1024);

  tree a = build_global_decl ("a", integer_type_node);
  tree b = build_global_decl ("b", integer_type_node);

  /* Given a >= 1024, then a <= 1023 should be impossible.  */
  {
    region_model model;
    ADD_SAT_CONSTRAINT (model, a, GE_EXPR, int_1024);
    ADD_UNSAT_CONSTRAINT (model, a, LE_EXPR, int_1023);
  }

  /* a > 4.  */
  {
    region_model model;
    ADD_SAT_CONSTRAINT (model, a, GT_EXPR, int_4);
    ASSERT_CONDITION_TRUE (model, a, GT_EXPR, int_4);
    ASSERT_CONDITION_TRUE (model, a, NE_EXPR, int_3);
    ASSERT_CONDITION_UNKNOWN (model, a, NE_EXPR, int_5);
  }

  /* a <= 4.  */
  {
    region_model model;
    ADD_SAT_CONSTRAINT (model, a, LE_EXPR, int_4);
    ASSERT_CONDITION_FALSE (model, a, GT_EXPR, int_4);
    ASSERT_CONDITION_FALSE (model, a, GT_EXPR, int_5);
    ASSERT_CONDITION_UNKNOWN (model, a, NE_EXPR, int_3);
  }

  /* If "a > b" and "a == 3", then "b == 4" ought to be unsatisfiable.  */
  {
    region_model model;
    ADD_SAT_CONSTRAINT (model, a, GT_EXPR, b);
    ADD_SAT_CONSTRAINT (model, a, EQ_EXPR, int_3);
    ADD_UNSAT_CONSTRAINT (model, b, EQ_EXPR, int_4);
  }

  /* Various tests of int ranges where there is only one possible candidate.  */
  {
    /* If "a <= 4" && "a > 3", then "a == 4",
       assuming a is of integral type.  */
    {
      region_model model;
      ADD_SAT_CONSTRAINT (model, a, LE_EXPR, int_4);
      ADD_SAT_CONSTRAINT (model, a, GT_EXPR, int_3);
      ASSERT_CONDITION_TRUE (model, a, EQ_EXPR, int_4);
    }

    /* If "a > 3" && "a <= 4", then "a == 4",
       assuming a is of integral type.  */
    {
      region_model model;
      ADD_SAT_CONSTRAINT (model, a, GT_EXPR, int_3);
      ADD_SAT_CONSTRAINT (model, a, LE_EXPR, int_4);
      ASSERT_CONDITION_TRUE (model, a, EQ_EXPR, int_4);
    }
    /* If "a > 3" && "a < 5", then "a == 4",
       assuming a is of integral type.  */
    {
      region_model model;
      ADD_SAT_CONSTRAINT (model, a, GT_EXPR, int_3);
      ADD_SAT_CONSTRAINT (model, a, LT_EXPR, int_5);
      ASSERT_CONDITION_TRUE (model, a, EQ_EXPR, int_4);
    }
    /* If "a >= 4" && "a < 5", then "a == 4",
       assuming a is of integral type.  */
    {
      region_model model;
      ADD_SAT_CONSTRAINT (model, a, GE_EXPR, int_4);
      ADD_SAT_CONSTRAINT (model, a, LT_EXPR, int_5);
      ASSERT_CONDITION_TRUE (model, a, EQ_EXPR, int_4);
    }
    /* If "a >= 4" && "a <= 4", then "a == 4".  */
    {
      region_model model;
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

    region_model model;
    ADD_SAT_CONSTRAINT (model, f, GT_EXPR, float_3);
    ADD_SAT_CONSTRAINT (model, f, LE_EXPR, float_4);
    ASSERT_CONDITION_UNKNOWN (model, f, EQ_EXPR, float_4);
    ASSERT_CONDITION_UNKNOWN (model, f, EQ_EXPR, int_4);
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
    region_model model;

    ADD_SAT_CONSTRAINT (model, x, EQ_EXPR, y);

    /* Assert various things about the insides of model.  */
    constraint_manager *cm = model.get_constraints ();
    ASSERT_EQ (cm->m_constraints.length (), 0);
    ASSERT_EQ (cm->m_equiv_classes.length (), 1);
  }

  /* y <= z; x == y.  */
  {
    region_model model;
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
    region_model model;
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
    region_model model;

    ADD_SAT_CONSTRAINT (model, x, EQ_EXPR, int_0);
    ADD_SAT_CONSTRAINT (model, x, NE_EXPR, int_42);

    /* Assert various things about the insides of model.  */
    constraint_manager *cm = model.get_constraints ();
    ASSERT_EQ (cm->m_constraints.length (), 1);
    ASSERT_EQ (cm->m_equiv_classes.length (), 2);
    ASSERT_EQ (cm->m_constraints[0].m_lhs,
	       cm->get_or_add_equiv_class (model.get_rvalue (int_0, NULL)));
    ASSERT_EQ (cm->m_constraints[0].m_rhs,
	       cm->get_or_add_equiv_class (model.get_rvalue (int_42, NULL)));
    ASSERT_EQ (cm->m_constraints[0].m_op, CONSTRAINT_LT);
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
    region_model model0;
    region_model model1;

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

    region_model model2;
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
  tree a = build_global_decl ("a", integer_type_node);

  region_model model;
  auto_vec<tree> constants;
  for (int i = 0; i < 20; i++)
    {
      tree constant = build_int_cst (integer_type_node, i);
      constants.safe_push (constant);
      ADD_SAT_CONSTRAINT (model, a, NE_EXPR, constant);

      /* Merge, and check the result.  */
      region_model other (model);

      region_model merged;
      ASSERT_TRUE (model.can_merge_with_p (other, &merged));
      model.canonicalize (NULL);
      merged.canonicalize (NULL);
      ASSERT_EQ (model, merged);

      for (int j = 0; j <= i; j++)
	ASSERT_CONDITION_TRUE (model, a, NE_EXPR, constants[j]);
    }
}

/* Run the selftests in this file, temporarily overriding
   flag_analyzer_transitivity with TRANSITIVITY.  */

static void
run_constraint_manager_tests (bool transitivity)
{
  int saved_flag_analyzer_transitivity = flag_analyzer_transitivity;
  flag_analyzer_transitivity = transitivity;

  test_constraint_conditions ();
  if (flag_analyzer_transitivity)
    {
      /* These selftests assume transitivity.  */
      test_transitivity ();
      test_constant_comparisons ();
    }
  test_constraint_impl ();
  test_equality ();
  test_many_constants ();

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
