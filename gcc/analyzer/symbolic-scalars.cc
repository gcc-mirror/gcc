/* Unit-safe symbolic scalar values (e.g. "n * 32 bytes")
   Copyright (C) 2019-2026 Free Software Foundation, Inc.
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

#include "analyzer/common.h"

#include "selftest.h"

#include "diagnostics/markup.h"

#include "analyzer/region-model-manager.h"
#include "analyzer/symbolic-scalars.h"
#include "analyzer/region-model.h"

#if ENABLE_ANALYZER

namespace ana {
namespace symbolic {

// template<typename ConcreteValueType> class scalar : public symbolic::node

template<typename ConcreteValueType>
bool
scalar<ConcreteValueType>::is_zero_p () const
{
  if (auto concrete = dyn_cast_concrete_scalar ())
    return concrete->get_concrete ().is_zero_p ();
  return false;
}

template
bool
scalar<integer_with_unit<units::bit>>::is_zero_p () const;

template
bool
scalar<integer_with_unit<units::byte>>::is_zero_p () const;

template
bool
scalar<integer>::is_zero_p () const;

template<typename ConcreteValueType>
bool
scalar<ConcreteValueType>::is_one_p () const
{
  if (auto concrete = dyn_cast_concrete_scalar ())
    return concrete->get_concrete ().is_one_p ();
  return false;
}

template
bool
scalar<integer_with_unit<units::bit>>::is_one_p () const;

template
bool
scalar<integer_with_unit<units::byte>>::is_one_p () const;

template
bool
scalar<integer>::is_one_p () const;

template<typename ConcreteValueType>
bool
scalar<ConcreteValueType>::is_unknown_p () const
{
  if (auto *from_sval = dyn_cast_scalar_from_svalue ())
    return from_sval->get_svalue ().get_kind () == SK_UNKNOWN;
  return false;
}

template<typename ConcreteValueType>
void
scalar<ConcreteValueType>::print_dump_widget_label (pretty_printer *pp) const
{
  start_any_dump_color (pp);
  print_dump_widget_label_for_scalar (pp);
  end_any_dump_color (pp);
}

template<>
std::unique_ptr<json::object>
scalar<integer>::make_json_obj (const char *type_name) const
{
  return value_node::make_json_obj (type_name);
}

template<>
std::unique_ptr<json::object>
scalar<integer_with_unit<units::bit>>::make_json_obj (const char *type_name) const
{
  auto obj = value_node::make_json_obj (type_name);
  obj->set_string ("unit", "bit");
  return obj;
}

template<>
std::unique_ptr<json::object>
scalar<integer_with_unit<units::byte>>::make_json_obj (const char *type_name) const
{
  auto obj = value_node::make_json_obj (type_name);
  obj->set_string ("unit", "byte");
  return obj;
}

/* A total ordering on scalars, which attempts to do the right
   thing where the scalars are directly comparable (using
   get_partial_ordering), with fallbacks for other cases.  */

template<typename ConcreteValueType>
bool
scalar<ConcreteValueType>::
get_total_ordering (const scalar<ConcreteValueType> &a,
		    enum tree_code op,
		    const scalar<ConcreteValueType> &b,
		    const region_model *model)
{
  tristate partial = get_partial_ordering (a, op, b, model);
  if (partial.is_known ())
    return partial.is_true ();

  ConcreteValueType conc_a;
  if (concrete_from_symbolic (a, &conc_a))
    {
      ConcreteValueType conc_b;
      if (concrete_from_symbolic (b, &conc_b))
	{
	  /* Concrete vs concrete: should have already been handled
	     by get_partial_ordering.  */
	  gcc_unreachable ();
	}
      else
	{
	  /* Concrete vs symbolic: put all concrete before all symbolic.  */
	  switch (op)
	    {
	    default:
	      gcc_unreachable ();
	    case NE_EXPR:
	    case LT_EXPR:
	    case LE_EXPR:
	      return true;
	    case EQ_EXPR:
	    case GT_EXPR:
	    case GE_EXPR:
	      return false;
	    }
	}
    }
  else
    {
      ConcreteValueType conc_b;
      if (concrete_from_symbolic (b, &conc_b))
	{
	  /* Symbolic vs concrete: put all symbolic after all concrete.  */
	  switch (op)
	    {
	    default:
	      gcc_unreachable ();
	    case EQ_EXPR:
	    case LT_EXPR:
	    case LE_EXPR:
	      return false;
	    case NE_EXPR:
	    case GT_EXPR:
	    case GE_EXPR:
	      return true;
	    }
	}
      else
	{
	  /* Symbolic vs symbolic with unknown ordering.
	     Fall back to ID order.  */
	  int cmp_ids = symbolic::node::cmp_ids (&a, &b);
	  switch (op)
	    {
	    default:
	      gcc_unreachable ();
	    case EQ_EXPR:
	      return cmp_ids == 0;
	    case NE_EXPR:
	      return cmp_ids != 0;
	    case LT_EXPR:
	      return cmp_ids < 0;
	    case LE_EXPR:
	      return cmp_ids <= 0;
	    case GT_EXPR:
	      return cmp_ids > 0;
	    case GE_EXPR:
	      return cmp_ids >= 0;
	    }
	}
    }
}

template<typename ConcreteValueType>
tristate
scalar<ConcreteValueType>::
get_partial_ordering (const scalar<ConcreteValueType> &a,
		      enum tree_code op,
		      const scalar<ConcreteValueType> &b,
		      const region_model *model)
{
  ConcreteValueType conc_a;
  if (concrete_from_symbolic (a, &conc_a))
    {
      ConcreteValueType conc_b;
      if (concrete_from_symbolic (b, &conc_b))
	{
	  /* Concrete vs concrete.  */
	  switch (op)
	    {
	    default:
	      gcc_unreachable ();
	    case EQ_EXPR:
	      return conc_a == conc_b;
	    case NE_EXPR:
	      return conc_a != conc_b;
	    case LT_EXPR:
	      return conc_a < conc_b;
	    case LE_EXPR:
	      return conc_a <= conc_b;
	    case GT_EXPR:
	      return conc_a > conc_b;
	    case GE_EXPR:
	      return conc_a >= conc_b;
	    }
	}
      else
	{
	  /* Concrete vs symbolic: can't compare.  */
	  return tristate::unknown ();
	}
    }
  else
    {
      ConcreteValueType conc_b;
      if (concrete_from_symbolic (b, &conc_b))
	{
	  /* Concrete vs symbolic: can't compare.  */
	  return tristate::unknown ();
	}
      else
	{
	  /* Symbolic vs symbolic.  */

	  /* Check for identity.  */
	  if (&a == &b)
	    switch (op)
	      {
	      default:
		gcc_unreachable ();
	      case NE_EXPR:
	      case LT_EXPR:
	      case GT_EXPR:
		return false;

	      case EQ_EXPR:
	      case LE_EXPR:
	      case GE_EXPR:
		return true;
	      }

	  /* Try taking their difference to see if it simplifies
	     to a concrete value.  */;
	  auto &diff = a - b;
	  ConcreteValueType conc_diff;
	  if (concrete_from_symbolic (diff, &conc_diff))
	    {
	      switch (op)
		{
		default:
		  gcc_unreachable ();
		case EQ_EXPR:
		  return conc_diff.get_integer () == 0;
		case NE_EXPR:
		  return conc_diff.get_integer () != 0;
		case LT_EXPR:
		  return conc_diff.get_integer () < 0;
		case LE_EXPR:
		  return conc_diff.get_integer () <= 0;
		case GT_EXPR:
		  return conc_diff.get_integer () > 0;
		case GE_EXPR:
		  return conc_diff.get_integer () >= 0;
		}
	    }

	  if (auto *bits_from_bytes_a = a.dyn_cast_bits_from_bytes_scalar ())
	    if (auto *bits_from_bytes_b = b.dyn_cast_bits_from_bytes_scalar ())
	      return symbolic_byte_size::get_partial_ordering
		(bits_from_bytes_a->get_bytes (),
		 op,
		 bits_from_bytes_b->get_bytes (),
		 model);

	  if (auto *scalar_from_sval_a = a.dyn_cast_scalar_from_svalue ())
	    if (auto *scalar_from_sval_b = b.dyn_cast_scalar_from_svalue ())
	      {
		// TODO: would be nice not to need a region_model here
		auto &node_mgr = a.get_mgr ().get_node_manager ();
		const region_model placeholder_model (node_mgr);
		const region_model *tmp_model = model;
		if (!tmp_model)
		  tmp_model = &placeholder_model;
		auto ts_sval = tmp_model->eval_condition
		  (&scalar_from_sval_a->get_svalue (),
		   op,
		   &scalar_from_sval_b->get_svalue ());
		if (ts_sval.is_known ())
		  return ts_sval;
	      }

	  return tristate::unknown ();
	}
    }
}

template
bool
symbolic_bit_size::get_total_ordering (const symbolic_bit_size &a,
				       enum tree_code op,
				       const symbolic_bit_size &b,
				       const region_model *model);

template
bool
symbolic_byte_size::get_total_ordering (const symbolic_byte_size &a,
					enum tree_code op,
					const symbolic_byte_size &b,
					const region_model *model);

// template<typename ConcreteValueType> class concrete_scalar : public scalar<ConcreteValueType>

template<typename ConcreteValueType>
concrete_scalar<ConcreteValueType>::
concrete_scalar (unsigned id,
		 scalar_manager<ConcreteValueType> &mgr,
		 const ConcreteValueType &value)
: scalar<ConcreteValueType> (complexity (1, 1), id, mgr,
			     scalar<ConcreteValueType>::kind::concrete),
  m_value (value)
{
}

template<>
const integer &
concrete_scalar<concrete_bit_size>::get_integer () const
{
  return m_value.get_integer ();
}

template<>
const integer &
concrete_scalar<concrete_byte_size>::get_integer () const
{
  return m_value.get_integer ();
}

template<>
const integer &
concrete_scalar<integer>::get_integer () const
{
  return m_value;
}

template<typename ConcreteValueType>
void
concrete_scalar<ConcreteValueType>::
print_dump_widget_label_for_scalar (pretty_printer *pp) const
{
  pp_printf (pp, "concrete_scalar<%s>: ", ConcreteValueType::dump_type_name);
  m_value.dump_to_pp (pp);
}

template<typename ConcreteValueType>
void
concrete_scalar<ConcreteValueType>::
add_dump_widget_children (text_art::tree_widget &,
			  node_dump_state &) const
{
    // no-op
}

template<>
void
concrete_scalar<integer>::accept_node_visitor (symbolic::node_visitor &v) const
{
  v.visit_scalar_unitless (*this);
}

template<>
void
concrete_scalar<integer_with_unit<units::bit>>::accept_node_visitor (symbolic::node_visitor &v) const
{
  v.visit_scalar_bit (*this);
}

template<>
void
concrete_scalar<integer_with_unit<units::byte>>::
accept_node_visitor (symbolic::node_visitor &v) const
{
  v.visit_scalar_byte (*this);
}

template<typename ConcreteValueType>
std::unique_ptr<json::object>
concrete_scalar<ConcreteValueType>::to_json () const
{
  auto obj = scalar<ConcreteValueType>::make_json_obj ("concrete_scalar");
  obj->set ("value", m_value.to_json ());
  return obj;
}

template<typename ConcreteValueType>
void
concrete_scalar<ConcreteValueType>::
dump_inline_to_pp (pretty_printer *pp, enum units::role role) const
{
  m_value.dump_to_pp (pp, role);
}

template<>
void
concrete_scalar<integer>::
dump_inline_to_pp (pretty_printer *pp, enum units::role) const
{
  m_value.dump_to_pp (pp);
}

template<typename ConcreteValueType>
std::unique_ptr<diagnostics::markup::node>
concrete_scalar<ConcreteValueType>::make_markup_node (markup_context &) const
{
  pretty_printer pp;
  m_value.dump_to_pp (&pp);
  return diagnostics::markup::make_text_node (pp_formatted_text (&pp));
}

// template<typename ConcreteValueType> class sum : public scalar<ConcreteValueType>

template<typename ConcreteValueType>
bool
sum<ConcreteValueType>::memoization_key::
operator< (const memoization_key &other) const
{
  if (m_addends.size () != other.m_addends.size ())
    return m_addends.size () < other.m_addends.size ();
  for (size_t i = 0 ; i < m_addends.size (); ++i)
    if (int c = symbolic::node::cmp (*m_addends[i],
				     *other.m_addends[i]))
      return c < 0;
  return false;
}

template<typename ConcreteValueType>
void
sum<ConcreteValueType>::folding_helper::
add_addend (const scalar<ConcreteValueType> &addend,
	    scalar_manager<integer> &unitless_mgr)
{
  /* Accumulate the sum of any concrete terms.  */
  if (auto concrete = addend.dyn_cast_concrete_scalar ())
    m_sum_of_concrete_values += concrete->get_concrete ();
  else if (auto sum = addend.dyn_cast_sum ())
    {
      /* Flatten sums of sums.  */
      for (auto symbolic : sum->m_symbolic_addends)
	add_addend (*symbolic, unitless_mgr);
      if (auto concrete = sum->m_concrete_addend)
	m_sum_of_concrete_values += concrete->get_concrete ();
    }
  else if (auto product = addend.dyn_cast_product ())
    add_to_coefficient_for_addend (&product->get_lhs (),
				   &product->get_rhs ());
  else
    add_to_coefficient_for_addend (&addend,
				   &unitless_mgr.get_concrete (1));
}

/* Update MAP by adding COEFFICIENT_CHANGE to the coefficient for TERM
   (if any).  */

template <typename Term, typename Coefficient>
static void
add_coefficient_to_term (std::map<const Term *, const Coefficient *> &map,
			 const Term *term,
			 const Coefficient *coefficient_change)
{
  auto iter = map.find (term);
  if (iter == map.end ())
    map.insert ({term, coefficient_change});
  else
    {
      auto &coefficient = map[term];
      coefficient = &(*coefficient + *coefficient_change);
    }
}

/* Add (VAL_WITH_UNIT * UNITLESS_VAL) to this in-construction sum.

   Try to use concrete values as coefficients, and purely symbolic terms
   as "terms".  */

template<typename ConcreteValueType>
void
sum<ConcreteValueType>::folding_helper::
add_to_coefficient_for_addend (const scalar<ConcreteValueType> *val_with_unit,
			       const scalar<integer> *unitless_val)
{
  if (val_with_unit->dyn_cast_concrete_scalar ())
    {
      gcc_assert (!unitless_val->dyn_cast_concrete_scalar ());

      auto *term = unitless_val;
      auto *coefficient_change = val_with_unit;
      add_coefficient_to_term (m_map_unitless_term_to_coefficient_with_unit,
			       term,
			       coefficient_change);
    }
  else
    {
      auto *term = val_with_unit;
      auto *coefficient_change = unitless_val;
      add_coefficient_to_term (m_map_term_with_unit_to_unitless_coefficient,
			       term,
			       coefficient_change);
    }
}

template<typename ConcreteValueType>
DEBUG_FUNCTION void
sum<ConcreteValueType>::folding_helper::dump () const
{
  symbolic::dump_node_tree_to_stderr<sum<ConcreteValueType>::folding_helper> (*this);
}

template<typename ConcreteValueType>
std::unique_ptr<text_art::tree_widget>
sum<ConcreteValueType>::folding_helper::
make_dump_widget (symbolic::node_dump_state &nds) const
{
  using text_art::tree_widget;
  std::unique_ptr<tree_widget> fh_widget
    (tree_widget::from_fmt (nds, nullptr, "folding_helper"));

  {
    std::unique_ptr<tree_widget> map_widget
      (tree_widget::from_fmt (nds, nullptr, "terms with units"));
    for (auto iter : m_map_term_with_unit_to_unitless_coefficient)
      {
	auto &addend = iter.first;
	auto &factor = iter.second;
	std::unique_ptr<tree_widget> iter_widget
	  (tree_widget::from_fmt (nds, nullptr, "symbolic addend"));
	iter_widget->add_child (addend->make_dump_widget (nds));
	iter_widget->add_child (factor->make_dump_widget (nds));
	map_widget->add_child (std::move (iter_widget));
      }
    fh_widget->add_child (std::move (map_widget));
  }
  {
    std::unique_ptr<tree_widget> map_widget
      (tree_widget::from_fmt (nds, nullptr, "unitless terms"));
    for (auto iter : m_map_unitless_term_to_coefficient_with_unit)
      {
	auto &addend = iter.first;
	auto &factor = iter.second;
	std::unique_ptr<tree_widget> iter_widget
	  (tree_widget::from_fmt (nds, nullptr, "symbolic addend"));
	iter_widget->add_child (addend->make_dump_widget (nds));
	iter_widget->add_child (factor->make_dump_widget (nds));
	map_widget->add_child (std::move (iter_widget));
      }
    fh_widget->add_child (std::move (map_widget));
  }
  {
    pretty_printer the_pp;
    pretty_printer * const pp = &the_pp;
    pp_format_decoder (pp) = default_tree_printer;
    m_sum_of_concrete_values.dump_to_pp (pp);
    fh_widget->add_child (tree_widget::make (nds, pp));
  }

  return fh_widget;
}

template<typename ConcreteValueType>
bool
sum<ConcreteValueType>::creation_key::term::
operator< (const sum<ConcreteValueType>::creation_key::term &other) const
{
  if (int c = symbolic::node::cmp (*m_with_unit,  *other.m_with_unit))
    return c < 0;
  return symbolic::node::cmp (*m_unitless, *other.m_unitless) < 0;
}

template<typename ConcreteValueType>
bool
sum<ConcreteValueType>::creation_key::term::
operator== (const sum<ConcreteValueType>::creation_key::term &other) const
{
  return (m_with_unit == other.m_with_unit
	  && m_unitless == other.m_unitless);
}

template<typename ConcreteValueType>
complexity
sum<ConcreteValueType>::creation_key::term::get_complexity () const
{
  gcc_assert (!m_unitless->is_zero_p ());
  gcc_assert (!m_with_unit->is_zero_p ());

  if (m_unitless->is_one_p ())
    return m_with_unit->get_complexity ();
  else if (m_with_unit->is_one_p ())
    return m_unitless->get_complexity ();
  else
    return complexity::from_pair (m_with_unit->get_complexity (),
				  m_unitless->get_complexity ());
}

template<typename ConcreteValueType>
std::unique_ptr<text_art::tree_widget>
sum<ConcreteValueType>::creation_key::term::
make_dump_widget (symbolic::node_dump_state &nds) const
{
  using text_art::tree_widget;
  std::unique_ptr<tree_widget> term_widget
    (tree_widget::from_fmt (nds, nullptr, "term"));
  term_widget->add_child (m_with_unit->make_dump_widget (nds));
  term_widget->add_child (m_unitless->make_dump_widget (nds));
  return term_widget;
}

template<typename ConcreteValueType>
complexity
sum<ConcreteValueType>::creation_key::get_complexity () const
{
  /* If we just have a constant we should have been folded away already.  */
  gcc_assert (!m_symbolic_terms.empty ());

  unsigned num_nodes = 0;
  unsigned max_depth = 0;
  for (auto term : m_symbolic_terms)
    {
      const complexity iter_c = term.get_complexity ();
      num_nodes += iter_c.m_num_nodes;
      max_depth = std::max (max_depth, iter_c.m_max_depth);
    }
  if (!m_concrete_term.is_zero_p ())
    {
      num_nodes += 1;
      max_depth = std::max (max_depth, (unsigned)1);
    }
  return complexity (num_nodes + 1, max_depth + 1);
}

template<typename ConcreteValueType>
sum<ConcreteValueType>::creation_key::
creation_key (sum<ConcreteValueType>::folding_helper &&fh)
: m_concrete_term (std::move (fh.m_sum_of_concrete_values))
{
  for (auto iter : fh.m_map_term_with_unit_to_unitless_coefficient)
    {
      gcc_assert (!iter.first->is_zero_p ());
      if (!iter.second->is_zero_p ())
	m_symbolic_terms.push_back ({iter.first, iter.second});
    }
  for (auto iter : fh.m_map_unitless_term_to_coefficient_with_unit)
    {
      gcc_assert (!iter.first->is_zero_p ());
      if (!iter.second->is_zero_p ())
	m_symbolic_terms.push_back ({iter.second, iter.first});
    }

  /* Make the ordering of the terms deterministic based on var node id.  */
  std::sort (m_symbolic_terms.begin (), m_symbolic_terms.end ());
}

template<typename ConcreteValueType>
bool
sum<ConcreteValueType>::creation_key::
operator< (const sum<ConcreteValueType>::creation_key &other) const
{
  if (int c = ConcreteValueType::cmp (m_concrete_term,
				      other.m_concrete_term))
    return c < 0;

  if (m_symbolic_terms.size ()
      != other.m_symbolic_terms.size ())
    return (m_symbolic_terms.size () <  other.m_symbolic_terms.size ());

  for (size_t i = 0; i < m_symbolic_terms.size (); ++i)
    if (!(m_symbolic_terms[i] == other.m_symbolic_terms[i]))
      return m_symbolic_terms[i] < other.m_symbolic_terms[i];

  return false;
}

template<typename ConcreteValueType>
const scalar<ConcreteValueType> *
sum<ConcreteValueType>::creation_key::
maybe_fold (scalar_manager<ConcreteValueType> &mgr) const
{
  if (m_symbolic_terms.empty ())
    {
      // We have a purely concrete value:
      return &mgr.get_concrete (m_concrete_term);
    }

  if (m_symbolic_terms.size () == 1
      && m_concrete_term.is_zero_p ())
    {
      /* ((X * F) + 0) --> (X * F).  */
      auto &term = m_symbolic_terms[0];
      return &mgr.get_product (*term.m_with_unit, *term.m_unitless);
    }

  /* If any part of any of the symbolic terms is unknown, the
     whole sum is unknown.  */
  for (auto iter : m_symbolic_terms)
    {
      if (iter.m_with_unit->is_unknown_p ())
	return &mgr.get_unknown ();
      if (iter.m_unitless->is_unknown_p ())
	return &mgr.get_unknown ();
    }

  if (auto *folded = maybe_fold_for_cvt (mgr))
    return folded;

  return nullptr;
}

template<typename ConcreteValueType>
const scalar<ConcreteValueType> *
sum<ConcreteValueType>::creation_key::
maybe_fold_for_cvt (scalar_manager<ConcreteValueType> &) const
{
  return nullptr;
}

/* If all the symbolic terms are bits_from_bytes_scalar
   and any concrete term is a multiple of 8, then we can move the
   bits_from_bytes outside the sum.  This is a simplification if the
   number of terms (symbolic or concrete) > 1, which should be the case
   for a creation_key that has survived this far.  */

template<>
const symbolic_bit_size *
sum<concrete_bit_size>::creation_key::
maybe_fold_for_cvt (scalar_manager<concrete_bit_size> &mgr) const
{
  concrete_byte_size conc_bytes;
  if (!maybe_bytes_from_bits (m_concrete_term, &conc_bytes))
    return nullptr;

  auto &byte_mgr = mgr.get_node_manager ().get_scalars ().get_bytes ();
  std::vector<const symbolic_byte_size *> addends_as_bytes;
  addends_as_bytes.push_back (&symbolic_from_concrete (conc_bytes, byte_mgr));

  for (auto sym_term : m_symbolic_terms)
    {
      auto *bits_from_bytes
	= sym_term.m_with_unit->dyn_cast_bits_from_bytes_scalar ();
      if (!bits_from_bytes)
	return nullptr;
      auto &addend_as_bytes
	= bits_from_bytes->get_bytes () * (*sym_term.m_unitless);
      addends_as_bytes.push_back (&addend_as_bytes);
    }

  auto &byte_sum = byte_mgr.get_sum (addends_as_bytes);
  return &mgr.get_bits_from_bytes (byte_sum);
}

template<typename ConcreteValueType>
DEBUG_FUNCTION void
sum<ConcreteValueType>::creation_key::dump () const
{
  symbolic::dump_node_tree_to_stderr<sum<ConcreteValueType>::creation_key> (*this);
}

template<typename ConcreteValueType>
std::unique_ptr<text_art::tree_widget>
sum<ConcreteValueType>::creation_key::
make_dump_widget (symbolic::node_dump_state &nds) const
{
  using text_art::tree_widget;
  std::unique_ptr<tree_widget> ck_widget
    (tree_widget::from_fmt (nds, nullptr, "creation_key"));

  for (auto sym_term : m_symbolic_terms)
    ck_widget->add_child (sym_term.make_dump_widget (nds));
  {
    pretty_printer the_pp;
    pretty_printer * const pp = &the_pp;
    pp_format_decoder (pp) = default_tree_printer;
    m_concrete_term.dump_to_pp (pp);
    ck_widget->add_child (tree_widget::make (nds, pp));
  }

  return ck_widget;
}

template<typename ConcreteValueType>
sum<ConcreteValueType>::sum (unsigned id,
			     scalar_manager<ConcreteValueType> &mgr,
			     const creation_key &ck)
: scalar<ConcreteValueType> (ck.get_complexity (), id, mgr,
			     scalar<ConcreteValueType>::kind::sum),
  m_concrete_addend (nullptr)
{
  for (auto term : ck.m_symbolic_terms)
    {
      gcc_assert (!term.m_with_unit->is_zero_p ());
      gcc_assert (!term.m_unitless->is_zero_p ());
      auto &symbolic_addend = (*term.m_with_unit) * (*term.m_unitless);
      gcc_assert (!symbolic_addend.dyn_cast_concrete_scalar ());
      gcc_assert (!symbolic_addend.dyn_cast_sum ());
      m_symbolic_addends.push_back (&symbolic_addend);
    }

  if (ck.m_concrete_term.is_zero_p ())
    {
      gcc_assert (m_symbolic_addends.size () >= 2);
    }
  else
    {
      gcc_assert (m_symbolic_addends.size () >= 1);
      m_concrete_addend = &mgr.get_concrete (ck.m_concrete_term);
    }
}

template<typename ConcreteValueType>
void
sum<ConcreteValueType>::
print_dump_widget_label_for_scalar (pretty_printer *pp) const
{
  pp_printf (pp, "sum<%s>", ConcreteValueType::dump_type_name);
}

template<typename ConcreteValueType>
void
sum<ConcreteValueType>::
add_dump_widget_children (text_art::tree_widget &w,
			  node_dump_state &nds) const
{
  for (auto iter : m_symbolic_addends)
    w.add_child (iter->make_dump_widget (nds));
  if (m_concrete_addend)
    w.add_child (m_concrete_addend->make_dump_widget (nds));
}

template<>
void
sum<integer>::accept_node_visitor (symbolic::node_visitor &v) const
{
  if (v.visit_scalar_unitless (*this))
    {
      for (auto iter : m_symbolic_addends)
	iter->accept_node_visitor (v);
      if (m_concrete_addend)
	m_concrete_addend->accept_node_visitor (v);
    }
}

template<>
void
sum<integer_with_unit<units::bit>>::
accept_node_visitor (symbolic::node_visitor &v) const
{
  if (v.visit_scalar_bit (*this))
    {
      for (auto iter : m_symbolic_addends)
	iter->accept_node_visitor (v);
      if (m_concrete_addend)
	m_concrete_addend->accept_node_visitor (v);
    }
}

template<>
void
sum<integer_with_unit<units::byte>>::
accept_node_visitor (symbolic::node_visitor &v) const
{
  if (v.visit_scalar_byte (*this))
    {
      for (auto iter : m_symbolic_addends)
	iter->accept_node_visitor (v);
      if (m_concrete_addend)
	m_concrete_addend->accept_node_visitor (v);
    }
}

template<typename ConcreteValueType>
std::unique_ptr<json::object>
sum<ConcreteValueType>::to_json () const
{
  auto obj = scalar<ConcreteValueType>::make_json_obj ("sum");
  auto arr = std::make_unique<json::array> ();
  for (auto iter : m_symbolic_addends)
    arr->append (iter->to_json ());
  obj->set ("symbolic_addends", std::move (arr));
  if (m_concrete_addend)
    obj->set ("concrete_addend", m_concrete_addend->to_json ());
  return obj;
}

template<typename ConcreteValueType>
void
sum<ConcreteValueType>::
dump_inline_to_pp (pretty_printer *pp, enum units::role role) const
{
  bool first = true;
  for (auto iter : m_symbolic_addends)
    {
      if (first)
	first = false;
      else
	pp_string (pp, " + ");
      iter->dump_inline_to_pp (pp, role);
    }
  if (m_concrete_addend)
    {
      if (first)
	first = false;
      else
	pp_string (pp, " + ");
      m_concrete_addend->dump_inline_to_pp (pp, role);
    }
}

template<typename ConcreteValueType>
std::unique_ptr<diagnostics::markup::node>
sum<ConcreteValueType>::make_markup_node (markup_context &markup_ctxt) const
{
  std::vector<std::unique_ptr<diagnostics::markup::node>> children;
  for (auto iter : m_symbolic_addends)
    children.push_back (iter->make_markup_node (markup_ctxt));
  if (m_concrete_addend)
    children.push_back (m_concrete_addend->make_markup_node (markup_ctxt));

  gcc_assert (children.size () >= 2);

  if (markup_ctxt.m_user_facing)
    if (children.size () == 2)
      {
	/* Lower "(-A) + B" to "B - A".  */
	if (auto *a_as_negation = children[0]->dyn_cast_math_negation ())
	  return std::make_unique<diagnostics::markup::math::subtraction>
	    (std::move (children[1]), // "B"
	     a_as_negation->take_operand ()); // "A"

	/* Lower "A + (-B)" to "A - B".  */
	if (auto *b_as_negation = children[1]->dyn_cast_math_negation ())
	  return std::make_unique<diagnostics::markup::math::subtraction>
	    (std::move (children[0]), // "A"
	     b_as_negation->take_operand ()); // "B"
      }

  return std::make_unique<diagnostics::markup::math::sum>
    (std::move (children));
}

// template<typename ConcreteValueType> class product : public scalar<ConcreteValueType>

template<typename ConcreteValueType>
bool
product<ConcreteValueType>::key::operator< (const key &other) const
{
  if (int c = symbolic::node::cmp (*m_lhs, *other.m_lhs))
    return c < 0;
  return symbolic::node::cmp (*m_rhs, *other.m_rhs) < 0;
}

template<typename ConcreteValueType>
complexity
product<ConcreteValueType>::key::get_complexity () const
{
  return complexity::from_pair (m_lhs->get_complexity (),
				m_rhs->get_complexity ());
}

template<typename ConcreteValueType>
void
product<ConcreteValueType>::key::canonicalize ()
{
  // No-op in general...
}

template<>
void
product<integer>::key::canonicalize ()
{
  /* For integer * integer, make sure any
     concrete value is on the right.  */
  if (m_lhs->dyn_cast_concrete_scalar ())
    std::swap (m_lhs, m_rhs);
}

template<typename ConcreteValueType>
product<ConcreteValueType>::product (unsigned id,
				     scalar_manager<ConcreteValueType> &mgr,
				     const key &k)
: scalar<ConcreteValueType> (k.get_complexity (), id, mgr,
			     scalar<ConcreteValueType>::kind::product),
  m_lhs (*k.m_lhs),
  m_rhs (*k.m_rhs)
{
  if (CHECKING_P)
    {
      /* Multiplication by 0 or 1 should have been folded away by now.  */
      gcc_assert (!m_lhs.is_zero_p ());
      gcc_assert (!m_rhs.is_zero_p ());
      gcc_assert (!m_lhs.is_one_p ());
      gcc_assert (!m_rhs.is_one_p ());

      /* Concrete * concrete should have been folded by now.  */
      gcc_assert (!(m_lhs.dyn_cast_concrete_scalar ()
		    && m_rhs.dyn_cast_concrete_scalar ()));
    }
}

template<typename ConcreteValueType>
void
product<ConcreteValueType>::
print_dump_widget_label_for_scalar (pretty_printer *pp) const
{
  pp_printf (pp, "product<%s>", ConcreteValueType::dump_type_name);
}

template<typename ConcreteValueType>
void
product<ConcreteValueType>::
add_dump_widget_children (text_art::tree_widget &w,
			  node_dump_state &nds) const
{
  w.add_child (m_lhs.make_dump_widget (nds));
  w.add_child (m_rhs.make_dump_widget (nds));
}

template<>
void
product<integer>::accept_node_visitor (symbolic::node_visitor &v) const
{
  if (v.visit_scalar_unitless (*this))
    {
      m_lhs.accept_node_visitor (v);
      m_rhs.accept_node_visitor (v);
    }
}

template<>
void
product<integer_with_unit<units::bit>>::
accept_node_visitor (symbolic::node_visitor &v) const
{
  if (v.visit_scalar_bit (*this))
    {
      m_lhs.accept_node_visitor (v);
      m_rhs.accept_node_visitor (v);
    }
}

template<>
void
product<integer_with_unit<units::byte>>::
accept_node_visitor (symbolic::node_visitor &v) const
{
  if (v.visit_scalar_byte (*this))
    {
      m_lhs.accept_node_visitor (v);
      m_rhs.accept_node_visitor (v);
    }
}

template<typename ConcreteValueType>
std::unique_ptr<json::object>
product<ConcreteValueType>::to_json () const
{
  auto obj = scalar<ConcreteValueType>::make_json_obj ("product");
  obj->set ("lhs", m_lhs.to_json ());
  obj->set ("rhs", m_rhs.to_json ());
  return obj;
}

template<typename ConcreteValueType>
void
product<ConcreteValueType>::
dump_inline_to_pp (pretty_printer *pp, enum units::role role) const
{
  m_lhs.dump_inline_to_pp (pp, role);
  pp_string (pp, " * ");
  m_rhs.dump_inline_to_pp (pp, role);
}

template<typename ConcreteValueType>
std::unique_ptr<diagnostics::markup::node>
product<ConcreteValueType>::make_markup_node (markup_context &markup_ctxt) const
{
  /* "A * -1" -> "-A".  */
  if (markup_ctxt.m_user_facing)
    if (auto *conc = m_rhs.dyn_cast_concrete_scalar ())
      if (conc->get_concrete ().get_integer () == -1)
	return std::make_unique<diagnostics::markup::math::negation>
	  (m_lhs.make_markup_node (markup_ctxt));

  auto markup
    = std::make_unique<diagnostics::markup::math::product> ();
  /* Put any concrete value on the RHS.  */
  if (m_lhs.dyn_cast_concrete_scalar ())
    {
      markup->add_child (m_rhs.make_markup_node (markup_ctxt));
      markup->add_child (m_lhs.make_markup_node (markup_ctxt));
    }
  else
    {
      markup->add_child (m_lhs.make_markup_node (markup_ctxt));
      markup->add_child (m_rhs.make_markup_node (markup_ctxt));
    }
  return markup;
}

// class scalar_from_svalue : public scalar<ConcreteValueType>

template<typename ConcreteValueType>
bool
scalar_from_svalue<ConcreteValueType>::key::operator< (const key &other) const
{
  if (int c = symbolic::node::cmp (*m_sval, *other.m_sval))
    return c < 0;
  return false;
}

template<typename ConcreteValueType>
complexity
scalar_from_svalue<ConcreteValueType>::key::get_complexity () const
{
  // For now, return the complexity of m_sval directly
  return m_sval->get_complexity ();
}

template<typename ConcreteValueType>
scalar_from_svalue<ConcreteValueType>::
scalar_from_svalue (unsigned id,
		    scalar_manager<ConcreteValueType> &mgr,
		    const key &k)
: scalar<ConcreteValueType> (k.get_complexity (), id, mgr,
			     scalar<ConcreteValueType>::kind::from_svalue),
  m_sval (*k.m_sval)
{
  if (CHECKING_P)
    {
      if (auto cst = m_sval.maybe_get_constant ())
	gcc_assert (TREE_CODE (cst) != INTEGER_CST);
    }
}

template<typename ConcreteValueType>
void
scalar_from_svalue<ConcreteValueType>::
print_dump_widget_label_for_scalar (pretty_printer *pp) const
{
  pp_printf (pp, "scalar_from_svalue<%s>", ConcreteValueType::dump_type_name);
}

template<typename ConcreteValueType>
void
scalar_from_svalue<ConcreteValueType>::
add_dump_widget_children (text_art::tree_widget &w,
			  node_dump_state &nds) const
{
  w.add_child (m_sval.make_dump_widget (nds));
}

template<>
void
scalar_from_svalue<integer>::
accept_node_visitor (symbolic::node_visitor &v) const
{
  if (v.visit_scalar_unitless (*this))
    m_sval.accept_node_visitor (v);
}

template<>
void
scalar_from_svalue<integer_with_unit<units::bit>>::
accept_node_visitor (symbolic::node_visitor &v) const
{
  if (v.visit_scalar_bit (*this))
    m_sval.accept_node_visitor (v);
}

template<>
void
scalar_from_svalue<integer_with_unit<units::byte>>::
accept_node_visitor (symbolic::node_visitor &v) const
{
  if (v.visit_scalar_byte (*this))
    m_sval.accept_node_visitor (v);
}

template<typename ConcreteValueType>
std::unique_ptr<json::object>
scalar_from_svalue<ConcreteValueType>::to_json () const
{
  auto obj = scalar<ConcreteValueType>::make_json_obj ("scalar_from_svalue");
  obj->set ("svalue", m_sval.to_json ());
  return obj;
}

template<typename ConcreteValueType>
void
scalar_from_svalue<ConcreteValueType>::
dump_inline_to_pp (pretty_printer *pp, enum units::role) const
{
  m_sval.dump_to_pp (pp, true);
}

template<typename ConcreteValueType>
std::unique_ptr<diagnostics::markup::node>
scalar_from_svalue<ConcreteValueType>::
make_markup_node (markup_context &markup_ctxt) const
{
  return m_sval.make_markup_node (markup_ctxt);
}

// template<typename ConcreteValueType> class named_test_scalar : public scalar<ConcreteValueType>

template<typename ConcreteValueType>
void
named_test_scalar<ConcreteValueType>::
print_dump_widget_label_for_scalar (pretty_printer *pp) const
{
  pp_printf (pp, "named_test_scalar<%s>: %qs",
	     ConcreteValueType::dump_type_name, m_name.c_str ());
}

template<typename ConcreteValueType>
void
named_test_scalar<ConcreteValueType>::
add_dump_widget_children (text_art::tree_widget &,
			  node_dump_state &) const
{
  // no-op
}

template<>
void
named_test_scalar<integer>::accept_node_visitor (symbolic::node_visitor &v) const
{
  v.visit_scalar_unitless (*this);
}

template<>
void
named_test_scalar<integer_with_unit<units::bit>>::
accept_node_visitor (symbolic::node_visitor &v) const
{
  v.visit_scalar_bit (*this);
}

template<>
void
named_test_scalar<integer_with_unit<units::byte>>::
accept_node_visitor (symbolic::node_visitor &v) const
{
  v.visit_scalar_byte (*this);
}

template<typename ConcreteValueType>
std::unique_ptr<json::object>
named_test_scalar<ConcreteValueType>::to_json () const
{
  auto obj = scalar<ConcreteValueType>::make_json_obj ("named_test_scalar");
  obj->set_string ("name", m_name);
  return obj;
}

template<>
void
named_test_scalar<integer_with_unit<units::bit>>::
dump_inline_to_pp (pretty_printer *pp, enum units::role role) const
{
  switch (role)
    {
    default:
      gcc_unreachable ();
    case units::role::generic:
    case units::role::size:
      pp_printf (pp, "%s bits", m_name.c_str ());
      break;
    case units::role::offset:
      pp_printf (pp, "bit %s", m_name.c_str ());
      break;
    }
}

template<>
void
named_test_scalar<integer_with_unit<units::byte>>::
dump_inline_to_pp (pretty_printer *pp, enum units::role role) const
{
  switch (role)
    {
    default:
      gcc_unreachable ();
    case units::role::generic:
    case units::role::size:
      pp_printf (pp, "%s bytes", m_name.c_str ());
      break;
    case units::role::offset:
      pp_printf (pp, "byte %s", m_name.c_str ());
      break;
    }
}

template<>
void
named_test_scalar<integer>::
dump_inline_to_pp (pretty_printer *pp, enum units::role) const
{
  pp_string (pp, m_name.c_str ());
}

template<typename ConcreteValueType>
std::unique_ptr<diagnostics::markup::node>
named_test_scalar<ConcreteValueType>::make_markup_node (markup_context &) const
{
  return diagnostics::markup::make_text_node (m_name);
}

// class bits_from_bytes_scalar : public symbolic_bit_size

bool
bits_from_bytes_scalar::key::operator< (const key &other) const
{
  return symbolic::node::cmp (*m_bytes, *other.m_bytes) < 0;
}

complexity
bits_from_bytes_scalar::key::get_complexity () const
{
  return complexity (*m_bytes);
}

bits_from_bytes_scalar::
bits_from_bytes_scalar (unsigned id,
			scalar_manager<concrete_bit_size> &mgr,
			const key &k)
: symbolic_bit_size (k.get_complexity (), id, mgr,
		     symbolic_bit_size::kind::bits_from_bytes),
  m_bytes (*k.m_bytes)
{
  if (CHECKING_P)
    {
      gcc_assert (!m_bytes.dyn_cast_concrete_scalar ());
      gcc_assert (!m_bytes.is_unknown_p ());
    }
}

void
bits_from_bytes_scalar::print_dump_widget_label_for_scalar (pretty_printer *pp) const
{
  pp_string (pp, "bits_from_bytes_scalar");
}

void
bits_from_bytes_scalar::add_dump_widget_children (text_art::tree_widget &w,
						  node_dump_state &nds) const
{
  w.add_child (m_bytes.make_dump_widget (nds));
}

void
bits_from_bytes_scalar::accept_node_visitor (symbolic::node_visitor &v) const
{
  if (v.visit_scalar_bit (*this))
    m_bytes.accept_node_visitor (v);
}

std::unique_ptr<json::object>
bits_from_bytes_scalar::to_json () const
{
  auto obj = scalar<integer_with_unit<units::bit>>::make_json_obj
    ("bits_from_bytes_scalar");
  obj->set ("bytes", m_bytes.to_json ());
  return obj;
}

void
bits_from_bytes_scalar::
dump_inline_to_pp (pretty_printer *pp, enum units::role role) const
{
  pp_string (pp, "bits_from_bytes_scalar(");
  m_bytes.dump_inline_to_pp (pp, role);
  pp_string (pp, ")");
}

std::unique_ptr<diagnostics::markup::node>
bits_from_bytes_scalar::make_markup_node (markup_context &) const
{
  return diagnostics::markup::make_text_node ("bits_from_bytes_scalar");
}

// template<typename ConcreteValueType> class scalar_manager;

template<typename ConcreteValueType>
const concrete_scalar<ConcreteValueType> &
scalar_manager<ConcreteValueType>::get_concrete (const ConcreteValueType &value)
{
  auto iter = m_concrete_map.find (value);
  if (iter != m_concrete_map.end ())
    return *(iter->second);

  auto s = std::make_unique<concrete_scalar<ConcreteValueType>>
    (alloc_node_id (),
     *this,
     value);
  auto &result = *s.get ();
  m_concrete_map.insert ({value, std::move (s)});
  return result;
}

/* Helper for implementing (concrete unit) * scalar.
   See if we can unpack scalar to move terms it to the
   per-unit scalar.   */

template<typename ConcreteValueType>
const scalar<ConcreteValueType> *
scalar_manager<ConcreteValueType>::
maybe_from_unitless (const ConcreteValueType &concrete_lhs,
		     const scalar<integer> &unitless_rhs)
{
  if (concrete_lhs.is_one_p ())
    {
      // (1 unit) * CST --> (CST units)
      if (auto s = unitless_rhs.dyn_cast_concrete_scalar ())
	return &get_concrete (ConcreteValueType (s->get_concrete ()));

      /* (1 unit) * (A * B) --> to_unit(A) * B
	 if A can be converted; otherwise try --> to_unit(B) * A).  */
      if (auto s = unitless_rhs.dyn_cast_product ())
	{
	  if (auto *lhs_with_units
		= maybe_from_unitless (concrete_lhs, s->get_lhs ()))
	    return &((*lhs_with_units) * s->get_rhs ());
	  if (auto *rhs_with_units
		= maybe_from_unitless (concrete_lhs, s->get_rhs ()))
	    return &((*rhs_with_units) * s->get_lhs ());
	}

      /* (1 unit) * unitless_from_sval(SVAL) --> with_unit_from_sval(SVAL).  */
      if (auto *from_sval = unitless_rhs.dyn_cast_scalar_from_svalue ())
	return &get_scalar_from_svalue (from_sval->get_svalue ());
    }

  if (auto *bits = maybe_from_unitless_for_unit (concrete_lhs, unitless_rhs))
    return bits;

  return nullptr;
}

template<>
const symbolic_bit_offset *
scalar_manager<concrete_bit_offset>::
maybe_from_unitless_for_unit (const concrete_bit_offset &concrete_lhs,
			      const scalar<integer> &unitless_rhs)
{
  auto &byte_mgr = get_node_manager ().get_scalars ().get_bytes ();

  /* ((8 * C) bits * unitless_rhs)
     --> bits_from_bytes (C_bytes * unitless_rhs).  */
  if (concrete_lhs.get_integer ().floor_mod (BITS_PER_UNIT) == 0)
    return &(get_bits_from_bytes
	     (byte_mgr.get_concrete
	      (concrete_byte_offset (concrete_lhs.get_integer ()
				     .floor_div (integer (BITS_PER_UNIT))))
	      * unitless_rhs));

  return nullptr;
}

template<>
const symbolic_byte_offset *
scalar_manager<concrete_byte_offset>::
maybe_from_unitless_for_unit (const concrete_byte_offset &,
			      const scalar<integer> &)
{
  // No-op
  return nullptr;
}

/* No-op specializing for "converting" plain integer to integer.  */

template<>
const scalar<integer> *
scalar_manager<integer>::
maybe_from_unitless (const integer &concrete_lhs,
		     const scalar<integer> &unitless_rhs)
{
  if (concrete_lhs.is_one_p ())
    return &unitless_rhs;
  return nullptr;
}

template<typename ConcreteValueType>
const scalar<ConcreteValueType> &
scalar_manager<ConcreteValueType>::
get_sum_unmemoized (const typename sum<ConcreteValueType>::memoization_key &memo_key)
{
  /* Gather and unwrap the addends.  */
  using folding_helper = typename sum<ConcreteValueType>::folding_helper;
  folding_helper fh;
  for (auto iter : memo_key.m_addends)
    fh.add_addend (*iter, get_unitless_manager ());

  /* Convert from map to a vector in a canonical order.  */
  using creation_key = typename sum<ConcreteValueType>::creation_key;
  creation_key ck (std::move (fh));

  if (auto *folded = ck.maybe_fold (*this))
    return *folded;

  /* Cache based on creation_key.  */
  auto iter = m_sum_map.find (ck);
  if (iter != m_sum_map.end ())
    return *(iter->second.get ());

  auto s = std::make_unique<sum<ConcreteValueType>> (alloc_node_id (),
						     *this,
						     ck);
  auto &result = *s.get ();
  m_sum_map.insert ({std::move (ck), std::move (s)});
  return result;
}


template<typename ConcreteValueType>
const scalar<ConcreteValueType> &
scalar_manager<ConcreteValueType>::
get_sum (std::vector<const scalar<ConcreteValueType> *> addends)
{
  typename sum<ConcreteValueType>::memoization_key
    memo_key {std::move (addends) };

  auto iter = m_memoized_sums.find (memo_key);
  if (iter != m_memoized_sums.end ())
    return *(iter->second);

  auto &s = get_sum_unmemoized (memo_key);
  m_memoized_sums.insert ({std::move (memo_key), &s});
  return s;
}

template<typename ConcreteValueType>
const scalar<ConcreteValueType> &
scalar_manager<ConcreteValueType>::
get_difference (const scalar<ConcreteValueType> &lhs,
		const scalar<ConcreteValueType> &rhs)
{
  return lhs + get_negation (rhs);
}

template
const scalar<concrete_bit_offset> &
scalar_manager<concrete_bit_offset>::
get_difference (const scalar<concrete_bit_offset> &lhs,
		const scalar<concrete_bit_offset> &rhs);

template
const scalar<concrete_byte_offset> &
scalar_manager<concrete_byte_offset>::
get_difference (const scalar<concrete_byte_offset> &lhs,
		const scalar<concrete_byte_offset> &rhs);

template<typename ConcreteValueType>
const scalar<ConcreteValueType> &
scalar_manager<ConcreteValueType>::
get_negation (const scalar<ConcreteValueType> &s)
{
  if (auto concrete = s.dyn_cast_concrete_scalar ())
    {
      auto &i = concrete->get_concrete ();

      // -0 --> 0
      if (i.is_zero_p ())
	return s;
      return get_concrete (-i);
    }

  auto &unitless_mgr = get_unitless_manager ();
  auto &minus_one = unitless_mgr.get_concrete (integer (-1));
  return get_product (s, minus_one);
}

template<typename ConcreteValueType>
const integer &
as_integer (const integer &i) { return i; }

template<typename Unit>
const integer &
as_integer (const integer_with_unit<Unit> &i) { return i.get_integer (); }

template<typename ConcreteValueType>
const scalar<ConcreteValueType> *
scalar_manager<ConcreteValueType>::
maybe_fold_product (const typename product<ConcreteValueType>::key &k)
{
  if (auto concrete_lhs = k.m_lhs->dyn_cast_concrete_scalar ())
    {
      auto &i = concrete_lhs->get_concrete ();

      // 0 units * A --> 0 units
      if (i.is_zero_p ())
	return &get_concrete (ConcreteValueType (0));

      // concrete unit * A --> A units
      if (auto s = maybe_from_unitless (concrete_lhs->get_concrete (),
					*k.m_rhs))
	return s;

      // concrete units * concrete rhs --> concrete units
      if (auto concrete_rhs = k.m_rhs->dyn_cast_concrete_scalar ())
	return &get_concrete (i * concrete_rhs->get_concrete ());

      if (auto rhs_prod = k.m_rhs->dyn_cast_product ())
	{
	  /* (C1 units) * (F * C2) where C1 and C2 are constant
	     --> (C1 * C2) units * F units.  */
	  if (auto conc = rhs_prod->get_rhs ().dyn_cast_concrete_scalar ())
	    return &(get_concrete (concrete_lhs->get_concrete ()
				   * conc->get_concrete ())
		     * rhs_prod->get_lhs ());
	}
    }
  if (auto concrete_rhs = k.m_rhs->dyn_cast_concrete_scalar ())
    {
      auto &i = concrete_rhs->get_concrete ();

      // X units * 0 --> 0 units
      if (i.is_zero_p ())
	return &get_concrete (ConcreteValueType (0));

      // X units * 1 --> X units
      if (i.is_one_p ())
	return k.m_lhs;

      /* (X units * F1) units * concrete factor F2
	 --> (X * (F1 * F2)) units.  */
      if (auto lhs_prod = k.m_lhs->dyn_cast_product ())
	return &(lhs_prod->get_lhs ()
		 * (lhs_prod->get_rhs () * *concrete_rhs));
    }

  /* Distributive property.  */
  {
    if (auto sum_lhs = k.m_lhs->dyn_cast_sum ())
      {
	/* product(sum(s1 + ... sN), RHS)
	   --> sum((s1 * RHS), ... (sN * RHS)).  */
	std::vector<const scalar<ConcreteValueType> *> products;
	for (size_t i = 0; i < sum_lhs->get_num_symbolic_addends (); ++i)
	  products.push_back (&(sum_lhs->get_symbolic_addend (i)
				* (*k.m_rhs)));
	if (auto conc = sum_lhs->get_any_concrete_addend ())
	  products.push_back (&((*conc) * (*k.m_rhs)));
	return &get_sum (products);
    }
  }

  /* Product of product.  */
  if (auto lhs_prod = k.m_lhs->dyn_cast_product ())
    {
      /* Product of product: (LHS units * A) * B --> (LHS units) * (A * B) */
      auto &unitless_mgr = get_unitless_manager ();
      return &get_product (lhs_prod->get_lhs (),
			   unitless_mgr.get_product (lhs_prod->get_rhs (),
						     *k.m_rhs));
    }

  if (k.m_lhs->is_unknown_p ())
    return &get_unknown ();
  if (k.m_rhs->is_unknown_p ())
    return &get_unknown ();

  return nullptr;
}

template<typename ConcreteValueType>
const scalar<ConcreteValueType> &
scalar_manager<ConcreteValueType>::
get_product (const scalar<ConcreteValueType> &lhs,
	     const scalar<integer> &rhs)
{
  typename product<ConcreteValueType>::key k {&lhs, &rhs};

  if (auto *s = maybe_fold_product (k))
    return *s;

  // Canonicalize order when lhs is also integer
  k.canonicalize ();

  auto iter = m_product_map.find (k);
  if (iter != m_product_map.end ())
    return *(iter->second);

  auto s
    = std::make_unique<product<ConcreteValueType>> (alloc_node_id (),
						    *this,
						    k);
  auto &result = *s.get ();
  m_product_map.insert ({std::move (k), std::move (s)});
  return result;
}

template<typename ConcreteValueType>
const scalar<ConcreteValueType> *
scalar_manager<ConcreteValueType>::
maybe_fold_scalar_from_svalue (const typename scalar_from_svalue<ConcreteValueType>::key &k)
{
  switch (k.m_sval->get_kind ())
    {
    default:
      break;

    case SK_CONSTANT:
      {
	tree cst = as_a<const constant_svalue *> (k.m_sval)->get_constant ();
	switch (TREE_CODE (cst))
	  {
	  default:
	    break;

	  case INTEGER_CST:
	    return &get_concrete
	      (ConcreteValueType (convert_from_integer_cst (cst)));
	  }
      }
      break;
    }

  return nullptr;
}

template<typename ConcreteValueType>
const scalar<ConcreteValueType> &
scalar_manager<ConcreteValueType>::get_scalar_from_svalue (const svalue &sval)
{
  typename scalar_from_svalue<ConcreteValueType>::key k {&sval};

  if (auto *s = maybe_fold_scalar_from_svalue (k))
    return *s;

  auto iter = m_scalar_from_svalue_map.find (k);
  if (iter != m_scalar_from_svalue_map.end ())
    return *(iter->second);

  auto s = std::make_unique<scalar_from_svalue<ConcreteValueType>>
    (alloc_node_id (),
     *this,
     k);
  auto &result = *s.get ();
  m_scalar_from_svalue_map.insert ({std::move (k), std::move (s)});
  return result;
}

template
const scalar<integer> &
scalar_manager<integer>::get_scalar_from_svalue (const svalue &sval);

template
const scalar<integer_with_unit<units::byte>> &
scalar_manager<integer_with_unit<units::byte>>::get_scalar_from_svalue (const svalue &sval);

// class scalar_managers

template <>
scalar_manager<integer_with_unit<units::bit>> &
scalar_managers::get_manager<integer_with_unit<units::bit>> ()
{
  return m_bit_mgr;
}

template <>
scalar_manager<integer_with_unit<units::byte>> &
scalar_managers::get_manager<integer_with_unit<units::byte>> ()
{
  return m_byte_mgr;
}

template <>
scalar_manager<integer> &
scalar_managers::get_manager<integer> ()
{
  return m_unitless_mgr;
}

template<typename ConcreteValueType>
scalar_manager<integer> &
scalar_manager<ConcreteValueType>::get_unitless_manager ()
{
  return m_node_mgr.get_scalars ().get_unitless ();
}

template
scalar_manager<integer> &
scalar_manager<concrete_bit_offset>::get_unitless_manager ();

template
scalar_manager<integer> &
scalar_manager<concrete_byte_offset>::get_unitless_manager ();

template<>
scalar_manager<integer> &
scalar_manager<integer>::get_unitless_manager ()
{
  return *this;
}

template <>
const scalar<concrete_bit_size> &
scalar_manager<concrete_bit_size>::get_unknown ()
{
  auto &mgr = m_node_mgr.get_svalue_manager ();
  auto *sval = mgr.get_or_create_unknown_svalue (NULL_TREE);
  return get_scalar_from_svalue (*sval);
}

template <>
const scalar<concrete_byte_size> &
scalar_manager<concrete_byte_size>::get_unknown ()
{
  auto &mgr = m_node_mgr.get_svalue_manager ();
  auto *sval = mgr.get_or_create_unknown_svalue (size_type_node);
  return get_scalar_from_svalue (*sval);
}

template <>
const scalar<integer> &
scalar_manager<integer>::get_unknown ()
{
  auto &mgr = m_node_mgr.get_svalue_manager ();
  auto *sval = mgr.get_or_create_unknown_svalue (size_type_node);
  return get_scalar_from_svalue (*sval);
}

#if CHECKING_P

template <typename ConcreteValueType>
const scalar<ConcreteValueType> &
scalar_manager<ConcreteValueType>::get_named_test_scalar (std::string name)
{
  auto obj
    = std::make_unique<named_test_scalar<ConcreteValueType>> (*this,
							      std::move (name));
  auto &result = *obj.get ();
  m_named_test_scalars.push_back (std::move (obj));
  return result;
}

#endif /* #if CHECKING_P */

template<>
const symbolic_bit_size *
scalar_manager<concrete_bit_size>::
maybe_fold_bits_from_bytes (const symbolic_byte_size &bytes)
{
  if (auto *conc = bytes.dyn_cast_concrete_scalar ())
    return &get_concrete
      (concrete_bit_size (conc->get_concrete ().get_integer ()
			  * BITS_PER_UNIT));

  if (bytes.is_unknown_p ())
    return &get_unknown ();

  return nullptr;
}

template<>
const symbolic_bit_size &
scalar_manager<concrete_bit_size>::
get_bits_from_bytes (const symbolic_byte_size &bytes)
{
  if (auto *folded = maybe_fold_bits_from_bytes (bytes))
    return *folded;

  bits_from_bytes_scalar::key k {&bytes};
  auto iter = m_bits_from_bytes_map.find (k);
  if (iter != m_bits_from_bytes_map.end ())
    return *(iter->second);

  auto s = std::make_unique<bits_from_bytes_scalar>
    (alloc_node_id (),
     *this,
     k);
  auto &result = *s.get ();
  m_bits_from_bytes_map.insert ({std::move (k), std::move (s)});
  return result;
}

} // namespace ana::symbolic

/* Fold
     bytes_from_bits (A + ... + Z) to
   to:
     bytes_from_bits (A) + ... bytes_from_bits (Z)
   if all of the terms themselves fold.  */

static const symbolic_byte_size *
maybe_fold_bytes_from_bits_sum (const symbolic::sum<concrete_bit_size> &bit_sum)
{
  std::vector<const symbolic_byte_size *> byte_addends;

  for (size_t i = 0; i < bit_sum.get_num_symbolic_addends (); ++i)
    {
      auto &sym_addend_as_bits = bit_sum.get_symbolic_addend (i);
      auto *sym_addend_as_bytes = maybe_bytes_from_bits (sym_addend_as_bits);
      if (!sym_addend_as_bytes)
	return nullptr;
      byte_addends.push_back (sym_addend_as_bytes);
    }

  if (auto *conc_addend_as_bits = bit_sum.get_any_concrete_addend ())
    {
      auto *conc_addend_as_bytes = maybe_bytes_from_bits (*conc_addend_as_bits);
      if (!conc_addend_as_bytes)
	return nullptr;
      byte_addends.push_back (conc_addend_as_bytes);
    }

  auto &bytes_mgr
    = bit_sum.get_mgr ().get_node_manager ().get_scalars ().get_bytes ();
  return &bytes_mgr.get_sum (std::move (byte_addends));
}

const symbolic_byte_size *
maybe_bytes_from_bits (const symbolic_bit_size &bits)
{
  if (auto *concrete_bits = bits.dyn_cast_concrete_scalar ())
    {
      auto val = concrete_bits->get_concrete ();
      integer_with_unit<units::byte> concrete_bytes;
      if (maybe_bytes_from_bits (val, &concrete_bytes))
	{
	  auto &bytes_mgr
	    = bits.get_mgr ().get_node_manager ().get_scalars ().get_bytes ();
	  return &bytes_mgr.get_concrete (concrete_bytes);
	}
      else
	return nullptr;
    }
  if (auto *b_from_b = bits.dyn_cast_bits_from_bytes_scalar ())
    return &b_from_b->get_bytes ();

  if (auto *sum = bits.dyn_cast_sum ())
    if (auto *folded = maybe_fold_bytes_from_bits_sum (*sum))
      return folded;

  if (auto *product = bits.dyn_cast_product ())
    if (auto *lhs_as_bytes = maybe_bytes_from_bits (product->get_lhs ()))
      return &((*lhs_as_bytes) * product->get_rhs ());

  return nullptr;
}

#if CHECKING_P

namespace selftest {

using symbolic_bit_manager
  = symbolic::scalar_manager<integer_with_unit<units::bit>>;
using symbolic_byte_manager
  = symbolic::scalar_manager<integer_with_unit<units::byte>>;

template <typename ConcreteValueType>
void
assert_scalar_dump_inline_eq (const ::selftest::location &loc,
			      const symbolic::scalar<ConcreteValueType> &s,
			      enum units::role role,
			      const char *expected_str)
{
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  s.dump_inline_to_pp (&pp, role);
  ASSERT_STREQ_AT (loc, pp_formatted_text (&pp), expected_str);
}

#define ASSERT_SCALAR_DUMP_INLINE_EQ(SCALAR, ROLE, EXPECTED_STR)	\
  SELFTEST_BEGIN_STMT							\
  assert_scalar_dump_inline_eq (SELFTEST_LOCATION, (SCALAR), (ROLE),	\
				(EXPECTED_STR));			\
  SELFTEST_END_STMT

template<typename ConcreteValueType>
static void
test_basics ()
{
  symbolic::node_manager node_mgr;
  auto &mgr = node_mgr.get_scalars ().get_manager<ConcreteValueType> ();

  auto &zero = mgr.get_concrete (ConcreteValueType (0));
  ASSERT_EQ (zero.get_complexity ().m_num_nodes, 1);
  ASSERT_EQ (zero.get_complexity ().m_max_depth, 1);

  ASSERT_TRUE (zero.is_zero_p ());
  ASSERT_FALSE (zero.is_one_p ());
  ASSERT_FALSE (zero.is_unknown_p ());

  auto &one = mgr.get_concrete (ConcreteValueType (1));
  ASSERT_EQ (zero.get_complexity ().m_num_nodes, 1);
  ASSERT_EQ (zero.get_complexity ().m_max_depth, 1);

  ASSERT_FALSE (one.is_zero_p ());
  ASSERT_TRUE (one.is_one_p ());
  ASSERT_FALSE (one.is_unknown_p ());

  auto &x = mgr.get_named_test_scalar ("x");
  ASSERT_EQ (x.get_complexity ().m_num_nodes, 1);
  ASSERT_EQ (x.get_complexity ().m_max_depth, 1);
  ASSERT_FALSE (x.is_zero_p ());
  ASSERT_FALSE (x.is_one_p ());
  ASSERT_FALSE (x.is_unknown_p ());

  auto &unknown = mgr.get_unknown ();
  ASSERT_FALSE (unknown.is_zero_p ());
  ASSERT_FALSE (unknown.is_one_p ());
  ASSERT_TRUE (unknown.is_unknown_p ());

  diagnostics::markup::lowering_options opts;
  ASSERT_SYMBOLIC_NODE_MARKUP_TEXT_EQ (zero, opts, "0");
  ASSERT_SYMBOLIC_NODE_MARKUP_TEXT_EQ (one, opts, "1");
  ASSERT_SYMBOLIC_NODE_MARKUP_TEXT_EQ (x, opts, "x");

  // Verify that consolidation works
  ASSERT_SAME_SYMBOLIC_NODE (mgr.get_concrete (ConcreteValueType (0)),
			     zero);
  ASSERT_SAME_SYMBOLIC_NODE (mgr.get_concrete (ConcreteValueType (1)),
			     one);
  ASSERT_DIFFERENT_SYMBOLIC_NODE (one, zero);
}

/* Test addition, for scalars of the given unit.  */

template<typename ConcreteValueType>
static void
test_sums ()
{
  symbolic::node_manager node_mgr;
  auto &mgr = node_mgr.get_scalars ().get_manager<ConcreteValueType> ();
  auto &unitless_mgr = node_mgr.get_scalars ().get_unitless ();

  auto &x = mgr.get_named_test_scalar ("x");
  auto &y = mgr.get_named_test_scalar ("y");
  auto &z = mgr.get_named_test_scalar ("z");

  auto &zero = mgr.get_concrete (ConcreteValueType (0));

  /* concrete + concrete.  */
  {
    auto &sum = (mgr.get_concrete (ConcreteValueType (42))
		 + mgr.get_concrete (ConcreteValueType (1)));
    ASSERT_EQ (sum.dyn_cast_concrete_scalar ()->get_concrete (),
	       ConcreteValueType (43));
    ASSERT_EQ (sum.get_complexity ().m_num_nodes, 1);
    ASSERT_EQ (sum.get_complexity ().m_max_depth, 1);
  }

  {
    /* symbolic + concrete.  */
    auto three = ConcreteValueType (3);
    auto &x_plus_3 = (x + three);
    ASSERT_EQ (x_plus_3.dyn_cast_sum ()->get_num_symbolic_addends (), 1);
    ASSERT_SAME_SYMBOLIC_NODE
      (x_plus_3.dyn_cast_sum ()->get_symbolic_addend (0),
       x);
    ASSERT_EQ (*x_plus_3.dyn_cast_sum ()->get_any_concrete_addend_value (),
	       three);
    ASSERT_EQ (x_plus_3.get_complexity ().m_num_nodes, 3);
    ASSERT_EQ (x_plus_3.get_complexity ().m_max_depth, 2);

    diagnostics::markup::lowering_options opts;
    ASSERT_SYMBOLIC_NODE_MARKUP_TEXT_EQ (x_plus_3, opts, "x + 3");

    /* concrete + symbolic == symbolic + concrete.  */
    auto &three_plus_x = (mgr.get_concrete (three) + x);
    ASSERT_SAME_SYMBOLIC_NODE (three_plus_x, x_plus_3);
    ASSERT_SYMBOLIC_NODE_MARKUP_TEXT_EQ (three_plus_x, opts, "x + 3");
  }

  /* Additive identity.  */
  {
    /* X + 0 -> X.  */
    ASSERT_SAME_SYMBOLIC_NODE ((x + zero), x);

    /* 0 + X -> X.  */
    ASSERT_SAME_SYMBOLIC_NODE ((zero + x), x);
  }

  /* symbolic + symbolic.  */
  {
    auto &x_plus_y = x + y;
    ASSERT_EQ (x_plus_y.dyn_cast_sum ()->get_num_symbolic_addends (), 2);
    ASSERT_SAME_SYMBOLIC_NODE
      (x_plus_y.dyn_cast_sum ()->get_symbolic_addend (0),
       x);
    ASSERT_SAME_SYMBOLIC_NODE
      (x_plus_y.dyn_cast_sum ()->get_symbolic_addend (1),
       y);
    ASSERT_EQ (x_plus_y.dyn_cast_sum ()->get_any_concrete_addend (), nullptr);
    ASSERT_EQ (x_plus_y.get_complexity ().m_num_nodes, 3);
    ASSERT_EQ (x_plus_y.get_complexity ().m_max_depth, 2);

    // Canonicalization of addition:
    ASSERT_SAME_SYMBOLIC_NODE ((x + y), x_plus_y);

    /* X + Y == Y + X.  */
    ASSERT_SAME_SYMBOLIC_NODE ((y + x), x_plus_y);

    diagnostics::markup::lowering_options opts;
    ASSERT_SYMBOLIC_NODE_MARKUP_TEXT_EQ (x_plus_y, opts, "x + y");
  }

  /* X + Y + Z.  */
  {
    auto &s = mgr.get_sum ({&x, &y, &z});
    ASSERT_EQ (s.dyn_cast_sum ()->get_num_symbolic_addends (), 3);
    ASSERT_SAME_SYMBOLIC_NODE (s.dyn_cast_sum ()->get_symbolic_addend (0),
			       x);
    ASSERT_SAME_SYMBOLIC_NODE (s.dyn_cast_sum ()->get_symbolic_addend (1),
			       y);
    ASSERT_SAME_SYMBOLIC_NODE (s.dyn_cast_sum ()->get_symbolic_addend (2),
			       z);
    ASSERT_EQ (s.dyn_cast_sum ()->get_any_concrete_addend (), nullptr);

    diagnostics::markup::lowering_options opts;
    ASSERT_SYMBOLIC_NODE_MARKUP_TEXT_EQ (s, opts, "x + y + z");
  }

  /* X + Y + Z == Z + 0 + Y + 0 + X.  */
  ASSERT_SAME_SYMBOLIC_NODE (mgr.get_sum ({&x, &y, &z}),
			     mgr.get_sum ({&z, &zero, &y, &zero, &x}));

  /* (X + 3) + 2 --> (X + 5).  */
  ASSERT_SAME_SYMBOLIC_NODE (((x + ConcreteValueType (3))
			      + ConcreteValueType (2)),
			     (x + ConcreteValueType (5)));

  /* No addends: sum() --> 0.  */
  ASSERT_SAME_SYMBOLIC_NODE (mgr.get_sum ({}),
			     (mgr.get_concrete (ConcreteValueType(0))));

  /* One addend: sum(X) --> X.  */
  ASSERT_SAME_SYMBOLIC_NODE (mgr.get_sum ({&x}),
			     x);

  /* Unknown + anything --> unknown.  */
  ASSERT_SAME_SYMBOLIC_NODE (mgr.get_sum ({&mgr.get_unknown (), &x}),
			     mgr.get_unknown ());
  ASSERT_SAME_SYMBOLIC_NODE (mgr.get_sum ({&x, &mgr.get_unknown ()}),
			     mgr.get_unknown ());
  ASSERT_SAME_SYMBOLIC_NODE
    (mgr.get_sum ({&mgr.get_unknown (), &mgr.get_unknown ()}),
     mgr.get_unknown ());

  /* X + X --> (X * 2).  */
  {
    auto &s = mgr.get_sum ({&x, &x});
    ASSERT_SAME_SYMBOLIC_NODE (s,
			       (x * unitless_mgr.get_concrete (2)));
    ASSERT_SAME_SYMBOLIC_NODE (s.dyn_cast_product ()->get_lhs (),
			       x);
    ASSERT_SAME_SYMBOLIC_NODE (s.dyn_cast_product ()->get_rhs (),
			       unitless_mgr.get_concrete (2));
  }

  /* X + X + X --> (X * 3).  */
  {
    auto &s = mgr.get_sum ({&x, &x, &x});
    ASSERT_SAME_SYMBOLIC_NODE (s,
			       (x * unitless_mgr.get_concrete (3)));
    ASSERT_SAME_SYMBOLIC_NODE (s.dyn_cast_product ()->get_lhs (),
			       x);
    ASSERT_SAME_SYMBOLIC_NODE (s.dyn_cast_product ()->get_rhs (),
			       unitless_mgr.get_concrete (3));
  }

  /* X + Y + X + Y --> (X * 2) + (Y * 2).  */
  ASSERT_SAME_SYMBOLIC_NODE (mgr.get_sum ({&x, &y, &x, &y}),
			     (x * unitless_mgr.get_concrete (2)
			      + y * unitless_mgr.get_concrete (2)));

  /* X + Y + X + X + Y --> (X * 3) + (Y * 2).  */
  ASSERT_SAME_SYMBOLIC_NODE (mgr.get_sum ({&x, &y, &x, &x, &y}),
			     (x * unitless_mgr.get_concrete (3)
			      + y * unitless_mgr.get_concrete (2)));

  /* The example from the comment in the header:
     3x + 4y + 5 - (2x + 5y + 6).  */
  ASSERT_SAME_SYMBOLIC_NODE
    ((((x * unitless_mgr.get_concrete (3))
       + (y * unitless_mgr.get_concrete (4))
       + mgr.get_concrete (ConcreteValueType(5)))
      - ((x * unitless_mgr.get_concrete (2))
	 + (y * unitless_mgr.get_concrete (5))
	 + mgr.get_concrete (ConcreteValueType(6)))),
     x - (y + mgr.get_concrete (ConcreteValueType(1))));
}

/* Test multiplication, for scalars of the given type.  */

template<typename ConcreteValueType>
static void
test_products ()
{
  symbolic::node_manager node_mgr;
  auto &unitless_mgr = node_mgr.get_scalars ().get_unitless ();
  auto &unit_mgr = node_mgr.get_scalars ().get_manager<ConcreteValueType> ();

  auto &x = unit_mgr.get_named_test_scalar ("x");

  auto &a = unitless_mgr.get_named_test_scalar ("a");
  auto &b = unitless_mgr.get_named_test_scalar ("b");

  /* Multiplication by zero: X * 0 --> 0.  */
  {
    // e.g. "x bytes * 0 --> 0 bytes":
    ASSERT_SAME_SYMBOLIC_NODE ((x * unitless_mgr.get_concrete (0)),
			       unit_mgr.get_concrete (ConcreteValueType (0)));

    // e.g. "0 bytes * a --> 0 bytes":
    ASSERT_SAME_SYMBOLIC_NODE
      ((unit_mgr.get_concrete (ConcreteValueType (0)) * a),
       unit_mgr.get_concrete (ConcreteValueType (0)));

    // 0 * unknown --> 0:
    ASSERT_SAME_SYMBOLIC_NODE
      (unit_mgr.get_concrete (ConcreteValueType (0))
       * unitless_mgr.get_unknown (),
       unit_mgr.get_concrete (ConcreteValueType (0)));
    ASSERT_SAME_SYMBOLIC_NODE
      (unit_mgr.get_unknown () * unitless_mgr.get_concrete (0),
       unit_mgr.get_concrete (ConcreteValueType (0)));
  }

  /* Multiplicative identity.  */
  {
    /* X * 1 --> X.  */
    // e.g. "x bytes * 1 --> x bytes":
    ASSERT_SAME_SYMBOLIC_NODE ((x * unitless_mgr.get_concrete (1)),
			       x);

    /* 1 unit * X for various X.  */
    {
      // The constant case e.g. 1 byte * 5 --> 5 bytes
      ASSERT_SAME_SYMBOLIC_NODE
	((unit_mgr.get_concrete (ConcreteValueType (1))
	  * unitless_mgr.get_concrete (5)),
	 unit_mgr.get_concrete (ConcreteValueType (5)));

      // The product cases e.g. 1 byte * (a * 3) --> 3 bytes * a
      ASSERT_SAME_SYMBOLIC_NODE
	((unit_mgr.get_concrete (ConcreteValueType (1))
	  * (a * unitless_mgr.get_concrete (3))),
	 (unit_mgr.get_concrete (ConcreteValueType (3))
	  * a));
      ASSERT_SAME_SYMBOLIC_NODE
	((unit_mgr.get_concrete (ConcreteValueType (1))
	  * (unitless_mgr.get_concrete (3) * a)),
	 (unit_mgr.get_concrete (ConcreteValueType (3))
	  * a));
    }
  }

  /* concrete * concrete --> concrete */
  {
    // e.g. "4 bytes * 3 --> 12 bytes":
    auto &product = (unit_mgr.get_concrete (ConcreteValueType (4))
		     * unitless_mgr.get_concrete (3));
    ASSERT_EQ (product.dyn_cast_concrete_scalar ()->get_concrete (),
	       ConcreteValueType (12));
  }

  /* Distributive property: (X + 8) units * 2 --> (X * 2) units + 16 units.  */
  ASSERT_SAME_SYMBOLIC_NODE
    (((x + unit_mgr.get_concrete (ConcreteValueType (8)))
      * unitless_mgr.get_concrete (2)),
     ((x * unitless_mgr.get_concrete (2))
      + unit_mgr.get_concrete (ConcreteValueType (16))));

  /* Chained multiplication: (X * 3) units * 2 --> (X * 6) units.  */
  ASSERT_SAME_SYMBOLIC_NODE ((x * unitless_mgr.get_concrete (3))
			     * unitless_mgr.get_concrete (2),
			     (x * unitless_mgr.get_concrete (6)));

  /* Chained multiplication: (X units * A) * B --> (X units) * (A * B).  */
  ASSERT_SAME_SYMBOLIC_NODE ((x * a) * b,
			     x * (a * b));

  // unknown * a --> unknown:
  ASSERT_SAME_SYMBOLIC_NODE (unit_mgr.get_unknown () * a,
			     unit_mgr.get_unknown ());
  ASSERT_SAME_SYMBOLIC_NODE (x * unitless_mgr.get_unknown (),
			     unit_mgr.get_unknown ());

  /* (1 unit) * unitless_from_sval(SVAL) --> with_unit_from_sval(SVAL).  */
  placeholder_svalue sval (node_mgr.alloc_node_id (), size_type_node, "sval");
  ASSERT_SAME_SYMBOLIC_NODE
    (unit_mgr.get_concrete (ConcreteValueType (1))
     * unitless_mgr.get_scalar_from_svalue (sval),
     unit_mgr.get_scalar_from_svalue (sval));
}

/* Test negation, for scalars of the given type.  */

template<typename ConcreteValueType>
static void
test_negation ()
{
  symbolic::node_manager node_mgr;
  auto &unit_mgr = node_mgr.get_scalars ().get_manager<ConcreteValueType> ();
  auto &unitless_mgr = node_mgr.get_scalars ().get_unitless ();

  auto &concrete = unit_mgr.get_concrete (ConcreteValueType (42));
  ASSERT_SAME_SYMBOLIC_NODE (-concrete,
			     unit_mgr.get_concrete (ConcreteValueType (-42)));

  auto &x = unit_mgr.get_named_test_scalar ("x");
  ASSERT_SAME_SYMBOLIC_NODE (-x, x * unitless_mgr.get_concrete (-1));

  ASSERT_SAME_SYMBOLIC_NODE (-unit_mgr.get_unknown (), unit_mgr.get_unknown ());

  auto &a = unitless_mgr.get_named_test_scalar ("a");
  ASSERT_SAME_SYMBOLIC_NODE
    (-(unit_mgr.get_concrete (ConcreteValueType (3)) * a),
     unit_mgr.get_concrete (ConcreteValueType (-3)) * a);

  ASSERT_SYMBOLIC_NODE_MARKUP_TEXT_EQ
    (-x,
     diagnostics::selftest::get_ascii_markup_opts (),
     "-x");
  ASSERT_SYMBOLIC_NODE_MARKUP_TEXT_EQ
    (-x,
     diagnostics::selftest::get_unicode_markup_opts (),
     MARKUP_UNICODE_SYMBOL_UNARY_NEGATION "x");
}

/* Test subtraction, for scalars of the given type.  */

template<typename ConcreteValueType>
static void
test_differences ()
{
  symbolic::node_manager node_mgr;
  auto &unitless_mgr = node_mgr.get_scalars ().get_unitless ();
  auto &unit_mgr = node_mgr.get_scalars ().get_manager<ConcreteValueType> ();

  auto &x = unit_mgr.get_named_test_scalar ("x");
  auto &y = unit_mgr.get_named_test_scalar ("y");

  auto &zero = unit_mgr.get_concrete (ConcreteValueType (0));

  auto &a = unitless_mgr.get_named_test_scalar ("a");

  // concrete - concrete
  ASSERT_SAME_SYMBOLIC_NODE
    ((unit_mgr.get_concrete (ConcreteValueType (5))
      - unit_mgr.get_concrete (ConcreteValueType (3))),
     unit_mgr.get_concrete (ConcreteValueType (2)));

  // concrete - symbolic
  ASSERT_SAME_SYMBOLIC_NODE
    (unit_mgr.get_concrete (ConcreteValueType (5)) - x,
     (-x) + unit_mgr.get_concrete (ConcreteValueType (5)));

  // symbolic - concrete
  ASSERT_SAME_SYMBOLIC_NODE
    (x - unit_mgr.get_concrete (ConcreteValueType (3)),
     x + unit_mgr.get_concrete (ConcreteValueType (-3)));

  // symbolic - symbolic
  ASSERT_SAME_SYMBOLIC_NODE (x - y, x + (-y));
  ASSERT_SYMBOLIC_NODE_MARKUP_TEXT_EQ
    (x - y,
     diagnostics::selftest::get_ascii_markup_opts (),
     "x - y");
  ASSERT_SYMBOLIC_NODE_MARKUP_TEXT_EQ
    (x - y,
     diagnostics::selftest::get_unicode_markup_opts (),
     "x " MARKUP_UNICODE_SYMBOL_SUBTRACTION " y");

  {
    pretty_printer pp;
    symbolic::pp_element_for_node e (x - y, true);
    pp_printf (&pp, "before{%e}after", &e);
    ASSERT_STREQ (pp_formatted_text (&pp), "before{x - y}after");
  }

  // (X - X) --> 0
  ASSERT_SAME_SYMBOLIC_NODE ((x - x),
			     zero);

  // (unknown - 1) --> unknown
  ASSERT_SAME_SYMBOLIC_NODE
    (unit_mgr.get_unknown () - unit_mgr.get_concrete (ConcreteValueType (1)),
     unit_mgr.get_unknown ());
  // (unknown - unknown) --> unknown
  ASSERT_SAME_SYMBOLIC_NODE
    ((unit_mgr.get_unknown () - unit_mgr.get_unknown ()),
     unit_mgr.get_unknown ());

  // (X - Y) + Y --> X
  ASSERT_SAME_SYMBOLIC_NODE ((x - y) + y, x);

  // (X units * 5) - (X units * 3) --> (X units * 2)
  ASSERT_SAME_SYMBOLIC_NODE
    ((x * unitless_mgr.get_concrete (5)
      - (x * unitless_mgr.get_concrete (3))),
     x * unitless_mgr.get_concrete (2));

  // (5 units * a) - (3 units * a) --> (2 * a) units
  ASSERT_SAME_SYMBOLIC_NODE
    ((unit_mgr.get_concrete (ConcreteValueType (5)) * a)
     - (unit_mgr.get_concrete (ConcreteValueType (3)) * a),
     unit_mgr.get_concrete (ConcreteValueType (2)) * a);
}

template<typename ConcreteValueType>
static void
test_comparisons ()
{
  symbolic::node_manager node_mgr;
  auto &scalar_mgr = node_mgr.get_scalars ().get_manager<ConcreteValueType> ();

  /* Tests of get_total_ordering.  */

  auto &three = scalar_mgr.get_concrete (ConcreteValueType (3));
  auto &four = scalar_mgr.get_concrete (ConcreteValueType (4));
  auto &x = scalar_mgr.get_named_test_scalar ("x");
  auto &y = scalar_mgr.get_named_test_scalar ("y");

  /* Concrete vs concrete.  */
  {
    ASSERT_TRUE (three == three);
    ASSERT_FALSE (three == four);

    ASSERT_TRUE (three != four);
    ASSERT_FALSE (three != three);

    ASSERT_TRUE (three < four);
    ASSERT_FALSE (three < three);
    ASSERT_FALSE (four < three);

    ASSERT_TRUE (three <= four);
    ASSERT_TRUE (three <= three);
    ASSERT_FALSE (four <= three);

    ASSERT_FALSE (three > four);
    ASSERT_FALSE (three > three);
    ASSERT_TRUE (four > three);

    ASSERT_FALSE (three >= four);
    ASSERT_TRUE (three >= three);
    ASSERT_TRUE (four >= three);
  }

  /* Symbolic vs concrete and vice versa:
     all concrete should be before all symbolic.  */
  {
    ASSERT_FALSE (three == x);

    ASSERT_TRUE (three != x);

    ASSERT_TRUE (three < x);
    ASSERT_FALSE (x < three);

    ASSERT_TRUE (three <= x);
    ASSERT_FALSE (x <= three);

    ASSERT_FALSE (three > x);
    ASSERT_TRUE (x > three);

    ASSERT_FALSE (three >= x);
    ASSERT_TRUE (x >= three);
  }

  /* Symbolic vs symbolic.  */
  {
    /* Unfoldable: order by creation order.  */
    {
      ASSERT_TRUE (x == x);
      ASSERT_FALSE (x == y);

      ASSERT_TRUE (x != y);
      ASSERT_FALSE (x != x);

      ASSERT_TRUE (x < y);
      ASSERT_FALSE (x < x);
      ASSERT_FALSE (y < x);

      ASSERT_TRUE (x <= y);
      ASSERT_TRUE (x <= x);
      ASSERT_FALSE (y <= x);

      ASSERT_FALSE (x > y);
      ASSERT_FALSE (x > x);
      ASSERT_TRUE (y > x);

      ASSERT_FALSE (x >= y);
      ASSERT_TRUE (x >= x);
      ASSERT_TRUE (y >= x);
    }

    /* Cases that have a constant difference and thus can be compared.  */
    {
      ASSERT_FALSE (x == x + three);

      ASSERT_TRUE (x != x + three);

      ASSERT_TRUE (x < x + three);
      ASSERT_FALSE (x + three < x);

      ASSERT_TRUE (x <= x + three);
      ASSERT_FALSE (x + three <= x);

      ASSERT_FALSE (x > x + three);
      ASSERT_TRUE (x + three > x);

      ASSERT_FALSE (x >= x + three);
      ASSERT_TRUE (x + three >= x);
    }
  }
}

template<typename ConcreteValueType>
static void
per_concrete_value_type_scalar_tests ()
{
  test_basics<ConcreteValueType> ();
  test_sums<ConcreteValueType> ();
  test_products<ConcreteValueType> ();
  test_negation<ConcreteValueType> ();
  test_differences<ConcreteValueType> ();
  test_comparisons<ConcreteValueType> ();
}

/* Verify that symbolic::scalar subclasses are dumped correctly.
   These can't be type-independent, as the strings mention the unit.  */

static void
test_dumping_scalars ()
{
  symbolic::node_manager node_mgr;
  auto &bit_mgr = node_mgr.get_scalars ().get_bits ();
  auto &byte_mgr = node_mgr.get_scalars ().get_bytes ();
  auto &unitless_mgr  = node_mgr.get_scalars ().get_unitless ();

  auto &x = bit_mgr.get_named_test_scalar ("x");
  auto &y = bit_mgr.get_named_test_scalar ("y");
  auto &z = bit_mgr.get_named_test_scalar ("z");

  ASSERT_SYMBOLIC_NODE_DUMP_EQ (x, "named_test_scalar<bits>: 'x'\n");
  ASSERT_SYMBOLIC_NODE_DUMP_EQ (bit_mgr.get_concrete (8_bits),
				"concrete_scalar<bits>: 8\n");

  ASSERT_SYMBOLIC_NODE_DUMP_EQ
    (x + y + z,
     ("sum<bits>\n"
      "+- named_test_scalar<bits>: 'x'\n"
      "+- named_test_scalar<bits>: 'y'\n"
      "`- named_test_scalar<bits>: 'z'\n"));
  ASSERT_SYMBOLIC_NODE_DUMP_EQ
    (x + y + z + 42_bits,
     ("sum<bits>\n"
      "+- named_test_scalar<bits>: 'x'\n"
      "+- named_test_scalar<bits>: 'y'\n"
      "+- named_test_scalar<bits>: 'z'\n"
      "`- concrete_scalar<bits>: 42\n"));
  ASSERT_SCALAR_DUMP_INLINE_EQ (x + y + z + 42_bits, units::role::offset,
				"bit x + bit y + bit z + bit 42");
  ASSERT_SCALAR_DUMP_INLINE_EQ (x + y + z + 42_bits, units::role::size,
				"x bits + y bits + z bits + 42 bits");

  auto &m = byte_mgr.get_named_test_scalar ("m");
  auto &n = byte_mgr.get_named_test_scalar ("n");
  ASSERT_SYMBOLIC_NODE_DUMP_EQ (m, "named_test_scalar<bytes>: 'm'\n");
  ASSERT_SYMBOLIC_NODE_DUMP_EQ (byte_mgr.get_concrete (10_bytes),
				"concrete_scalar<bytes>: 10\n");
  ASSERT_SYMBOLIC_NODE_DUMP_EQ
    (m + n,
     ("sum<bytes>\n"
      "+- named_test_scalar<bytes>: 'm'\n"
      "`- named_test_scalar<bytes>: 'n'\n"));
  ASSERT_SCALAR_DUMP_INLINE_EQ (m + n, units::role::offset,
				"byte m + byte n");
  ASSERT_SCALAR_DUMP_INLINE_EQ (m + n, units::role::size,
				"m bytes + n bytes");

  auto &a = unitless_mgr.get_named_test_scalar ("a");
  auto &b = unitless_mgr.get_named_test_scalar ("b");
  ASSERT_SYMBOLIC_NODE_DUMP_EQ (a, "named_test_scalar<integer>: 'a'\n");
  ASSERT_SYMBOLIC_NODE_DUMP_EQ (b, "named_test_scalar<integer>: 'b'\n");
  ASSERT_SYMBOLIC_NODE_DUMP_EQ
    (a + b,
     ("sum<integer>\n"
      "+- named_test_scalar<integer>: 'a'\n"
      "`- named_test_scalar<integer>: 'b'\n"));

  // Dump a complex example, showing a product
  ASSERT_SYMBOLIC_NODE_DUMP_EQ
    ((m * a) + (n * b),
     ("sum<bytes>\n"
      "+- product<bytes>\n"
      "|  +- named_test_scalar<bytes>: 'm'\n"
      "|  `- named_test_scalar<integer>: 'a'\n"
      "`- product<bytes>\n"
      "   +- named_test_scalar<bytes>: 'n'\n"
      "   `- named_test_scalar<integer>: 'b'\n"));

  ASSERT_SCALAR_DUMP_INLINE_EQ
    ((m * a) + (n * b),
     units::role::size,
     "m bytes * a + n bytes * b");
}

/* Test an example showing multiple levels of nesting,
   combining addition and multiplication.  */

static void
test_example_1 ()
{
  symbolic::node_manager node_mgr;
  auto &bit_mgr = node_mgr.get_scalars ().get_bits ();
  auto &unitless_mgr = node_mgr.get_scalars ().get_unitless ();

  auto &x = bit_mgr.get_named_test_scalar ("x");
  auto &y = bit_mgr.get_named_test_scalar ("y");
  auto &z = bit_mgr.get_named_test_scalar ("z");

  auto &a = unitless_mgr.get_named_test_scalar ("a");
  auto &b = unitless_mgr.get_named_test_scalar ("b");

  auto &s = (x * a) + (y * b) + z;
  ASSERT_SYMBOLIC_NODE_DUMP_EQ
    (s,
     ("sum<bits>\n"
      "+- product<bits>\n"
      "|  +- named_test_scalar<bits>: 'x'\n"
      "|  `- named_test_scalar<integer>: 'a'\n"
      "+- product<bits>\n"
      "|  +- named_test_scalar<bits>: 'y'\n"
      "|  `- named_test_scalar<integer>: 'b'\n"
      "`- named_test_scalar<bits>: 'z'\n"));
  s.to_json ();

  ASSERT_SYMBOLIC_NODE_MARKUP_TEXT_EQ
    (s,
     diagnostics::selftest::get_ascii_markup_opts (),
     "(x * a) + (y * b) + z");

  ASSERT_SYMBOLIC_NODE_MARKUP_TEXT_EQ
    (s,
     diagnostics::selftest::get_unicode_markup_opts (),
     ("(x " MARKUP_UNICODE_SYMBOL_MULTIPLICATION " a)"
      " + (y " MARKUP_UNICODE_SYMBOL_MULTIPLICATION " b)"
      " + z"));

  ASSERT_EQ (s.get_complexity ().m_num_nodes, 8);
  ASSERT_EQ (s.get_complexity ().m_max_depth, 3);
}

static void
test_example_2 ()
{
  /* Consider:
     |
     |  struct coord { uint32_t x; uint32_t y; };
     |  struct triangle { coord m_vertices[3]; };
     |
     |  ptr = malloc (sizeof (triangle) * n);
     |
     and tracking the size of the allocated buffer.  */

  symbolic::node_manager node_mgr;
  auto &bit_mgr = node_mgr.get_scalars ().get_bits ();
  auto &byte_mgr = node_mgr.get_scalars ().get_bytes ();
  auto &unitless_mgr  = node_mgr.get_scalars ().get_unitless ();

  auto &n = unitless_mgr.get_named_test_scalar ("n");

  /* In bytes.  */
  {
    auto &sizeof_x = byte_mgr.get_concrete (4_bytes);
    auto &sizeof_y = byte_mgr.get_concrete (4_bytes);

    auto &sizeof_coord = sizeof_x + sizeof_y;
    ASSERT_EQ (sizeof_coord.dyn_cast_concrete_scalar ()->get_concrete (),
	       8_bytes);

    auto &sizeof_triangle = sizeof_coord * unitless_mgr.get_concrete (3);
    ASSERT_EQ (sizeof_triangle.dyn_cast_concrete_scalar ()->get_concrete (),
	       24_bytes);

    auto &sizeof_buffer = sizeof_triangle * n;
    ASSERT_SYMBOLIC_NODE_DUMP_EQ
      (sizeof_buffer,
       ("product<bytes>\n"
	"+- concrete_scalar<bytes>: 24\n"
	"`- named_test_scalar<integer>: 'n'\n"));

    ASSERT_SCALAR_DUMP_INLINE_EQ
      (sizeof_buffer,
       units::role::size,
       "24 bytes * n");

    sizeof_buffer.to_json ();
  }

  /* In bits.  */
  {
    auto &sizeof_x = bit_mgr.get_concrete (32_bits);
    auto &sizeof_y = bit_mgr.get_concrete (32_bits);

    auto &sizeof_coord = sizeof_x + sizeof_y;
    ASSERT_EQ (sizeof_coord.dyn_cast_concrete_scalar ()->get_concrete (),
	       64_bits);

    auto &sizeof_triangle = sizeof_coord * unitless_mgr.get_concrete (3);
    ASSERT_EQ (sizeof_triangle.dyn_cast_concrete_scalar ()->get_concrete (),
	       192_bits);

    auto &sizeof_buffer = sizeof_triangle * n;
    ASSERT_SYMBOLIC_NODE_DUMP_EQ
      (sizeof_buffer,
       ("bits_from_bytes_scalar\n"
	"`- product<bytes>\n"
	"   +- concrete_scalar<bytes>: 24\n"
	"   `- named_test_scalar<integer>: 'n'\n"));

    ASSERT_SCALAR_DUMP_INLINE_EQ
      (sizeof_buffer,
       units::role::size,
       "bits_from_bytes_scalar(24 bytes * n)");

    sizeof_buffer.to_json ();
  }
}

static void
test_bits_from_bytes ()
{
  symbolic::node_manager node_mgr;
  auto &bit_mgr = node_mgr.get_scalars ().get_bits ();
  auto &byte_mgr = node_mgr.get_scalars ().get_bytes ();
  auto &unitless_mgr = node_mgr.get_scalars ().get_unitless ();

  auto &num_bytes_a = byte_mgr.get_named_test_scalar ("num_bytes_a");
  auto &num_bytes_b = byte_mgr.get_named_test_scalar ("num_bytes_b");

  ASSERT_SAME_SYMBOLIC_NODE
    (bits_from_bytes (num_bytes_a) + bits_from_bytes (num_bytes_b),
     bits_from_bytes (num_bytes_a + num_bytes_b));

  ASSERT_SAME_SYMBOLIC_NODE
    (bits_from_bytes (num_bytes_a)
     + bits_from_bytes (num_bytes_b)
     + bits_from_bytes (1_byte),
     bits_from_bytes (num_bytes_a + num_bytes_b + 1_byte));

  ASSERT_SAME_SYMBOLIC_NODE
    (bits_from_bytes (byte_mgr.get_unknown ()),
     bit_mgr.get_unknown ());

  placeholder_svalue sval (node_mgr.alloc_node_id (), size_type_node, "sval");
  auto &bytes_from_sval = byte_mgr.get_scalar_from_svalue (sval);
  auto &unitless_from_sval = unitless_mgr.get_scalar_from_svalue (sval);
  ASSERT_EQ (bits_from_bytes (bytes_from_sval).get_kind (),
	     symbolic::scalar<concrete_bit_offset>::kind::bits_from_bytes);
  ASSERT_SAME_SYMBOLIC_NODE (bit_mgr.get_concrete (8_bits) * unitless_from_sval,
			     bits_from_bytes (bytes_from_sval));
}

static void
test_maybe_bytes_from_bits ()
{
  symbolic::node_manager node_mgr;
  auto &bit_mgr = node_mgr.get_scalars ().get_bits ();
  auto &byte_mgr = node_mgr.get_scalars ().get_bytes ();
  auto &unitless_mgr = node_mgr.get_scalars ().get_unitless ();

  ASSERT_SAME_SYMBOLIC_NODE
    (*maybe_bytes_from_bits (bit_mgr.get_concrete (8_bits)),
     byte_mgr.get_concrete (1_byte));
  ASSERT_EQ (maybe_bytes_from_bits (bit_mgr.get_concrete (3_bits)),
	     nullptr);

  placeholder_svalue sval (node_mgr.alloc_node_id (), size_type_node, "sval");
  ASSERT_SAME_SYMBOLIC_NODE
    (maybe_bytes_from_bits (bit_mgr.get_concrete (8_bits)
			    * unitless_mgr.get_scalar_from_svalue (sval)),
     &byte_mgr.get_scalar_from_svalue (sval));
}

/* Run all of the selftests within this file.  */

void
analyzer_symbolic_scalars_cc_tests ()
{
  per_concrete_value_type_scalar_tests<integer> ();
  per_concrete_value_type_scalar_tests<integer_with_unit<units::bit>> ();
  per_concrete_value_type_scalar_tests<integer_with_unit<units::byte>> ();

  test_dumping_scalars ();
  test_example_1 ();
  test_example_2 ();

  test_bits_from_bytes ();
  test_maybe_bytes_from_bits ();
}

} // namespace selftest

#endif /* CHECKING_P */

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
