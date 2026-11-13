/* Unit-safe symbolic scalar values (e.g. "n * 32 bytes")
   Copyright (C) 2026 Free Software Foundation, Inc.
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

#ifndef GCC_ANALYZER_SYMBOLIC_SCALARS_H
#define GCC_ANALYZER_SYMBOLIC_SCALARS_H

#include "analyzer/units.h"
#include "analyzer/symbolic-nodes.h"
#include "analyzer/integer-with-unit.h"

namespace ana {
namespace symbolic {

template<typename ConcreteValueType>
class scalar_manager;

template<typename ConcreteValueType>
class concrete_scalar;

template<typename ConcreteValueType>
class sum;

template<typename ConcreteValueType>
class product;

template<typename ConcreteValueType>
class scalar_from_svalue;

template<typename ConcreteValueType>
class overcomplex_scalar;

class bits_from_bytes_scalar;

/* symbolic::scalar

   The class hierarchy looks like this (using indentation to show
   inheritance):

   scalar: abstract base class
     concrete: a value of ConcreteValueType
     sum: the sum of two of more scalars
     product: the product of a scalar and a dimensionless scalar
     scalar_from_svalue: a scalar derived from an svalue
     named_test: a placeholder for use in selftests
     bits_from_bytes_scalar: converting a scalar measured in bits
       into one measured in bytes.  */

template<typename ConcreteValueType>
class scalar : public symbolic::value_node
{
public:
  enum class kind
  {
    concrete,
    sum,
    product,
    from_svalue,
    named_test,
    bits_from_bytes
  };

  virtual ~scalar () = default;

  scalar_manager<ConcreteValueType> &
  get_mgr () const { return m_mgr; }

  enum kind get_kind () const { return m_kind; }

  virtual const concrete_scalar<ConcreteValueType> *
  dyn_cast_concrete_scalar () const { return nullptr; }
  virtual const sum<ConcreteValueType> *
  dyn_cast_sum () const { return nullptr; }
  virtual const product<ConcreteValueType> *
  dyn_cast_product () const { return nullptr; }
  virtual const scalar_from_svalue<ConcreteValueType> *
  dyn_cast_scalar_from_svalue () const { return nullptr; }
  virtual const bits_from_bytes_scalar *
  dyn_cast_bits_from_bytes_scalar () const { return nullptr; }

  virtual void
  dump_inline_to_pp (pretty_printer *pp, enum units::role) const = 0;

  iter_result
  for_each_svalue (iter_result (*) (const svalue &sval,
				      void *user_data),
		   void *) const final override
  {
    return iter_result::keep_going;
  }

  const char *
  get_any_dump_color () const override { return "scalar"; }

  bool
  is_zero_p () const;

  bool
  is_one_p () const;

  bool
  is_unknown_p () const;

  // FIXME: unit test these
  static bool
  get_total_ordering (const scalar<ConcreteValueType> &a,
		      enum tree_code op,
		      const scalar<ConcreteValueType> &b,
		      const region_model *model);
  static tristate
  get_partial_ordering (const scalar<ConcreteValueType> &a,
			enum tree_code op,
			const scalar<ConcreteValueType> &b,
			const region_model *model);
  // FIXME: unit test these
  // FIXME: add notes on the semantics of these comparisons
  bool
  operator== (const scalar<ConcreteValueType> &other) const
  {
    return get_total_ordering (*this, EQ_EXPR, other, nullptr);
  }
  bool
  operator!= (const scalar<ConcreteValueType> &other) const
  {
    return get_total_ordering (*this, NE_EXPR, other, nullptr);
  }
  bool
  operator< (const scalar<ConcreteValueType> &other) const
  {
    return get_total_ordering (*this, LT_EXPR, other, nullptr);
  }
  bool
  operator<= (const scalar<ConcreteValueType> &other) const
  {
    return get_total_ordering (*this, LE_EXPR, other, nullptr);
  }
  bool
  operator> (const scalar<ConcreteValueType> &other) const
  {
    return get_total_ordering (*this, GT_EXPR, other, nullptr);
  }
  bool
  operator>= (const scalar<ConcreteValueType> &other) const
  {
    return get_total_ordering (*this, GE_EXPR, other, nullptr);
  }

protected:
  scalar (complexity c, unsigned id, scalar_manager<ConcreteValueType> &mgr,
	  enum kind kind)
  : value_node (c, id),
    m_mgr (mgr),
    m_kind (kind)
  {
  }

  std::unique_ptr<json::object>
  make_json_obj (const char *type_name) const;

private:
  void
  print_dump_widget_label (pretty_printer *pp) const final override;

  virtual void
  print_dump_widget_label_for_scalar (pretty_printer *pp) const = 0;

  scalar_manager<ConcreteValueType> &m_mgr;
  enum kind m_kind;
};

} // namespace symbolic

/* Abbreviations for various useful symbolic::scalar types.  */

using symbolic_integer = symbolic::scalar<integer>;

using symbolic_bit_offset = symbolic::scalar<integer_with_unit<units::bit>>;
using symbolic_bit_size = symbolic::scalar<integer_with_unit<units::bit>>;

using symbolic_byte_offset = symbolic::scalar<integer_with_unit<units::byte>>;
using symbolic_byte_size = symbolic::scalar<integer_with_unit<units::byte>>;

namespace symbolic {

/* Symbolic value wrapping a concrete scalar.  */

template<typename ConcreteValueType>
class concrete_scalar : public scalar<ConcreteValueType>
{
public:
  const concrete_scalar<ConcreteValueType> *
  dyn_cast_concrete_scalar () const final override { return this; }

  concrete_scalar (unsigned id,
		   scalar_manager<ConcreteValueType> &mgr,
		   const ConcreteValueType &value);

  const ConcreteValueType &
  get_concrete () const { return m_value; }

  const integer &
  get_integer () const;

  void
  accept_node_visitor (node_visitor &v) const final override;

  std::unique_ptr<json::object>
  to_json () const final override;

  void
  dump_inline_to_pp (pretty_printer *pp, enum units::role) const final override;

  std::unique_ptr<diagnostics::markup::node>
  make_markup_node (markup_context &markup_ctxt) const final override;

private:
  void
  print_dump_widget_label_for_scalar (pretty_printer *pp) const final override;

  void
  add_dump_widget_children (text_art::tree_widget &,
			    node_dump_state &nds) const final override;

  ConcreteValueType m_value;
};

/* Symbolic value for the sum of two of more scalars.

   Consolidation/canonicalization:
   - we never have a sum as an addend of another sum
     (we flatten sums of sums)
   - a sum is expressed as a sum of one or more non-concrete_scalars,
     plus an optional concrete_scalar; the latter is the null pointer iff
     the concrete addends sum to zero.
   - we must have at least two addends (two or more symbolic, or a concrete
     and one or more symbolic).  */

template<typename ConcreteValueType>
class sum : public scalar<ConcreteValueType>
{
public:
  struct memoization_key
  {
    std::vector<const scalar<ConcreteValueType> *> m_addends;

    bool operator< (const memoization_key &other) const;
  };

  /* A helper class for canonicalizing sums in the form:
       (v1 * f1) + (v2 * f2) + .... (vN * fN) + CST
     i.e. as a pair of maps from:
       key: v1 ... vN
     to
       value: f1 ... fN
     (one map for where the coefficients have units, and another
     for where they are unitless), plus a constant of CVT.

     For example, given
     |    3x + 4y + 5 - (2x + 5y + 6)
     | => (x * 3) + (y * 4) + 5 - ((x * 2) + (y * 5) + 6)
     | => (x * 3) + (y * 4) + 5 + ((x * -2) + (y * -5) + -6)
     | => (x * 3) + (y * 4) + 5 + ((x * -2) + (y * -5) + -6)
     we canonicalize/fold the addends thus:
     | => ({} + 0) + (x * 3) + (y * 4) + 5 + ((x * -2) + (y * -5) + -6)
     | => ({x: 3} + 0})      + (y * 4) + 5 + ((x * -2) + (y * -5) + -6)
     | => ({x: 3, y: 4} + 0)           + 5 + ((x * -2) + (y * -5) + -6)
     | => ({x: 3, y: 4} + 5)               + ((x * -2) + (y * -5) + -6)
     | => ({x: 1, y: 4} + 5)                           + (y * -5) + -6)
     | => ({x: 1, y: -1} + 5)                                     + -6)
     | => ({x: 1, y: -1} + -1)
     from which we can get this:
     | => x + (y * -1) + -1
     i.e. sum(x, product(y, -1), -1).  */
  struct folding_helper
  {
    folding_helper () : m_sum_of_concrete_values (0) {}

    void
    add_addend (const scalar<ConcreteValueType> &addend,
		scalar_manager<integer> &unitless_mgr);

    /* Add (FACTOR_WITH_UNIT * UNITLESS_FACTOR) to the sum.  */
    void
    add_to_coefficient_for_addend
      (const scalar<ConcreteValueType> *factor_with_unit,
       const scalar<integer> *unitless_factor);

    void
    dump () const;

    std::unique_ptr<text_art::tree_widget>
    make_dump_widget (symbolic::node_dump_state &nds) const;

    /* Maps from unfoldable symbolic addend to symbolic factor for that
       addend.  This is the "(v1 * f1) + (v2 * f2) + .... (vN * fN)"
       from above, i.e "{v1: f1, v2: f2, ..., vN: fN}".
       Note that the coefficient can be zero for cases such as "X + (-X)".
       We'll discard these when we build the creation_key.

       We have two maps:

       (a) a map in which the terms have units and the
       coefficients are unitless, e.g.
       (N bytes * 16) + (N bytes * -4), and

       (b) a map in which the coefficents have units and the
       terms are unitless, e.g.
       (16 bytes * N) + (-4 bytes * N)

       We try to use concrete factors as coefficients, and
       purely symbolic factors as "terms".  */

    std::map<const scalar<ConcreteValueType> *,
	     const scalar<integer> *>
      m_map_term_with_unit_to_unitless_coefficient;
    /* e.g. {'N bytes': 12}.  */

    std::map<const scalar<integer> *,
	     const scalar<ConcreteValueType> *>
      m_map_unitless_term_to_coefficient_with_unit;
    /* e.g. {'N': 12 bytes}.  */

    /* This is the " + CST" from above.  */
    ConcreteValueType m_sum_of_concrete_values;
  };

  /* The second phase of canonicalization: arrange these into a vector
     of terms, dropping any terms with a factor == 0.  */
  struct creation_key
  {
    struct term
    {
      const scalar<ConcreteValueType> *m_with_unit;
      const scalar<integer> *m_unitless;

      bool operator< (const term &other) const;
      bool operator== (const term &other) const;

      complexity
      get_complexity () const;

      std::unique_ptr<text_art::tree_widget>
      make_dump_widget (symbolic::node_dump_state &nds) const;
    };

    creation_key (folding_helper &&fh);

    bool operator< (const creation_key &other) const;

    complexity
    get_complexity () const;

    const scalar<ConcreteValueType> *
    maybe_fold (scalar_manager<ConcreteValueType> &mgr) const;

    const scalar<ConcreteValueType> *
    maybe_fold_for_cvt (scalar_manager<ConcreteValueType> &mgr) const;

    void
    dump () const;

    std::unique_ptr<text_art::tree_widget>
    make_dump_widget (symbolic::node_dump_state &nds) const;

    std::vector<term> m_symbolic_terms;
    ConcreteValueType m_concrete_term;
  };

  sum (unsigned id,
       scalar_manager<ConcreteValueType> &mgr,
       const creation_key &k);

  const sum<ConcreteValueType> *
  dyn_cast_sum () const final override { return this; }

  size_t
  get_num_symbolic_addends () const
  {
    return m_symbolic_addends.size ();
  }

  const scalar<ConcreteValueType> &
  get_symbolic_addend (size_t idx) const { return *m_symbolic_addends[idx]; }

  const concrete_scalar<ConcreteValueType> *
  get_any_concrete_addend () const
  {
    return m_concrete_addend;
  }

  const ConcreteValueType *
  get_any_concrete_addend_value () const
  {
    if (m_concrete_addend)
      return &m_concrete_addend->get_concrete ();
    else
      return nullptr;
  }

  void
  accept_node_visitor (node_visitor &v) const final override;

  std::unique_ptr<json::object>
  to_json () const final override;

  void
  dump_inline_to_pp (pretty_printer *pp, enum units::role) const final override;

  std::unique_ptr<diagnostics::markup::node>
  make_markup_node (markup_context &markup_ctxt) const final override;

private:
  void
  print_dump_widget_label_for_scalar (pretty_printer *pp) const final override;

  void
  add_dump_widget_children (text_art::tree_widget &,
			    node_dump_state &nds) const final override;

  std::vector<const scalar<ConcreteValueType> *> m_symbolic_addends;

  // non-null if nonzero; null if zero:
  const concrete_scalar<ConcreteValueType> *m_concrete_addend;
};

/* Symbolic value for a scalar (perhaps with a unit, perhaps dimensionless)
   scaled by a dimensionless factor.

   Consolidation/canonicalization:
   - X units * 0 --> 0 units
   - 0 units * Y --> 0 units
   - X units * 1 --> X units
   - we never have a product of two concrete_scalar (we fold this).  */

template<typename ConcreteValueType>
class product : public scalar<ConcreteValueType>
{
public:
  struct key
  {
    bool operator< (const key &other) const;

    complexity
    get_complexity () const;

    void canonicalize ();

    const scalar<ConcreteValueType> *m_lhs;
    const scalar<integer> *m_rhs;
  };

  product (unsigned id,
	   scalar_manager<ConcreteValueType> &mgr,
	   const key &k);

  const product<ConcreteValueType> *
  dyn_cast_product () const final override { return this; }

  const scalar<ConcreteValueType> &
  get_lhs () const { return m_lhs; }

  const scalar<integer> &
  get_rhs () const { return m_rhs; }

  void
  accept_node_visitor (node_visitor &v) const final override;

  std::unique_ptr<json::object>
  to_json () const final override;

  void
  dump_inline_to_pp (pretty_printer *pp, enum units::role) const final override;

  std::unique_ptr<diagnostics::markup::node>
  make_markup_node (markup_context &markup_ctxt) const final override;

private:
  void
  print_dump_widget_label_for_scalar (pretty_printer *pp) const final override;

  void
  add_dump_widget_children (text_art::tree_widget &,
			    node_dump_state &nds) const final override;

  const scalar<ConcreteValueType> &m_lhs;
  const scalar<integer> &m_rhs;
};

/* Concrete subclass of symbolic::scalar for a scalar derived from an svalue
   for when folding isn't possible.

   Consolidation/canonicalization:
   - if the svalue is a constant_svalue, it is not an INTEGER_CST (we
     fold such scalars to concrete_scalar).  */

template<typename ConcreteValueType>
class scalar_from_svalue : public scalar<ConcreteValueType>
{
public:
  struct key
  {
    bool operator< (const key &other) const;

    complexity
    get_complexity () const;

    const svalue *m_sval;
  };

  scalar_from_svalue (unsigned id,
		      scalar_manager<ConcreteValueType> &mgr,
		      const key &k);

  const scalar_from_svalue<ConcreteValueType> *
  dyn_cast_scalar_from_svalue () const final override { return this; }

  const svalue &
  get_svalue () const { return m_sval; }

  void
  accept_node_visitor (node_visitor &v) const final override;

  std::unique_ptr<json::object>
  to_json () const final override;

  void
  dump_inline_to_pp (pretty_printer *pp, enum units::role) const final override;

  std::unique_ptr<diagnostics::markup::node>
  make_markup_node (markup_context &markup_ctxt) const final override;

private:
  void
  print_dump_widget_label_for_scalar (pretty_printer *pp) const final override;

  void
  add_dump_widget_children (text_art::tree_widget &,
			    node_dump_state &nds) const final override;

  const svalue &m_sval;
};

/* A named scalar for use in unit tests: cannot be folded.  */

#if CHECKING_P

template<typename ConcreteValueType>
class named_test_scalar : public scalar<ConcreteValueType>
{
public:
  named_test_scalar (scalar_manager<ConcreteValueType> &mgr,
		     std::string name)
  : scalar<ConcreteValueType> (complexity (1, 1),
			       mgr.alloc_node_id (),
			       mgr,
			       scalar<ConcreteValueType>::kind::named_test),
    m_name (std::move (name))
  {
  }

  void
  accept_node_visitor (node_visitor &v) const final override;

  std::unique_ptr<json::object>
  to_json () const final override;

  void
  dump_inline_to_pp (pretty_printer *pp, enum units::role) const final override;

  std::unique_ptr<diagnostics::markup::node>
  make_markup_node (markup_context &markup_ctxt) const final override;

private:
  void
  print_dump_widget_label_for_scalar (pretty_printer *pp) const final override;

  void
  add_dump_widget_children (text_art::tree_widget &,
			    node_dump_state &nds) const final override;

  std::string m_name;
};

#endif /* #if CHECKING_P */

/* Concrete subclass of symbolic::scalar for when we can't fold
   a conversion from bytes to bits.

   Consolidation/canonicalization:
   - m_bytes is not a concrete_scalar.
   - m_bytes is not is_unknown_p ().  */

class bits_from_bytes_scalar : public symbolic_bit_size
{
public:
  struct key
  {
    bool operator< (const key &other) const;

    complexity
    get_complexity () const;

    const symbolic_byte_size *m_bytes;
  };

  bits_from_bytes_scalar (unsigned id,
			  scalar_manager<concrete_bit_size> &mgr,
			  const key &k);

  const bits_from_bytes_scalar *
  dyn_cast_bits_from_bytes_scalar () const final override { return this; }

  void
  accept_node_visitor (node_visitor &v) const final override;

  std::unique_ptr<json::object>
  to_json () const final override;

  void
  dump_inline_to_pp (pretty_printer *pp, enum units::role) const final override;

  std::unique_ptr<diagnostics::markup::node>
  make_markup_node (markup_context &markup_ctxt) const final override;

  const symbolic_byte_size &
  get_bytes () const { return m_bytes; }

private:
  void
  print_dump_widget_label_for_scalar (pretty_printer *pp) const final override;

  void
  add_dump_widget_children (text_art::tree_widget &,
			    node_dump_state &nds) const final override;

  const symbolic_byte_size &m_bytes;
};

/* A class with responsibility for allocating/consolidating
   scalar<ConcreteValueType> instances.  */

template<typename ConcreteValueType>
class scalar_manager
{
public:
  scalar_manager (node_manager &node_mgr)
  : m_node_mgr (node_mgr)
  {
  }

  /* Make sure we don't accidentally copy these.  */
  scalar_manager (const scalar_manager &) = delete;

  const concrete_scalar<ConcreteValueType> &
  get_concrete (const ConcreteValueType &value);

  const scalar<ConcreteValueType> &
  get_sum (std::vector<const scalar<ConcreteValueType> *> addends);

  const scalar<ConcreteValueType> &
  get_difference (const scalar<ConcreteValueType> &lhs,
		  const scalar<ConcreteValueType> &rhs);

  const scalar<ConcreteValueType> &
  get_negation (const scalar<ConcreteValueType> &s);

  const scalar<ConcreteValueType> &
  get_product (const scalar<ConcreteValueType> &lhs,
	       const scalar<integer> &factor);

  const scalar<ConcreteValueType> &
  get_scalar_from_svalue (const svalue &sval);

  const symbolic_bit_size &
  get_bits_from_bytes (const symbolic_byte_size &bytes);

  const scalar<ConcreteValueType> &
  get_unknown ();

#if CHECKING_P
  const scalar<ConcreteValueType> &
  get_named_test_scalar (std::string name);
#endif

  unsigned
  alloc_node_id () { return m_node_mgr.alloc_node_id (); }

  node_manager &
  get_node_manager () { return m_node_mgr; }

  scalar_manager<integer> &
  get_unitless_manager ();

private:
  /* Explicitly adding units to a unitless value.  */
  const scalar<ConcreteValueType> *
  maybe_from_unitless (const ConcreteValueType &concrete_lhs,
		       const scalar<integer> &unitless_rhs);

  const scalar<ConcreteValueType> *
  maybe_from_unitless_for_unit (const ConcreteValueType &concrete_lhs,
				const scalar<integer> &unitless_rhs);

  using sum_memoization_key = typename sum<ConcreteValueType>::memoization_key;
  const scalar<ConcreteValueType> &
  get_sum_unmemoized (const sum_memoization_key &memo_key);

  const scalar<ConcreteValueType> *
  maybe_fold_product (const typename product<ConcreteValueType>::key &k);

  const scalar<ConcreteValueType> *
  maybe_fold_scalar_from_svalue (const typename scalar_from_svalue<ConcreteValueType>::key &k);

  const symbolic_bit_size *
  maybe_fold_bits_from_bytes (const symbolic_byte_size &bytes);

  node_manager &m_node_mgr;

  std::map<ConcreteValueType,
	   std::unique_ptr<const concrete_scalar<ConcreteValueType>>> m_concrete_map;

  std::map<typename sum<ConcreteValueType>::memoization_key,
	   const scalar<ConcreteValueType> *> m_memoized_sums;
  std::map<typename sum<ConcreteValueType>::creation_key,
	   std::unique_ptr<scalar<ConcreteValueType>>> m_sum_map;
  std::map<typename product<ConcreteValueType>::key,
	   std::unique_ptr<const product<ConcreteValueType>>> m_product_map;
  std::map<typename scalar_from_svalue<ConcreteValueType>::key,
	   std::unique_ptr<const scalar_from_svalue<ConcreteValueType>>> m_scalar_from_svalue_map;
  std::map<typename bits_from_bytes_scalar::key,
	   std::unique_ptr<bits_from_bytes_scalar>> m_bits_from_bytes_map;

#if CHECKING_P
  std::vector<std::unique_ptr<named_test_scalar<ConcreteValueType>>>
     m_named_test_scalars;
#endif
};

/* A bundle of scalar_manager<CVT> singletons for each of the various
   concrete value types we care about.  */

class scalar_managers
{
public:
  scalar_managers (node_manager &node_mgr)
  : m_bit_mgr (node_mgr),
    m_byte_mgr (node_mgr),
    m_unitless_mgr (node_mgr)
  {
  }

  scalar_manager<integer_with_unit<units::bit>> &
  get_bits ()
  {
    return m_bit_mgr;
  }

  scalar_manager<integer_with_unit<units::byte>> &
  get_bytes ()
  {
    return m_byte_mgr;
  }

  scalar_manager<integer> &
  get_unitless ()
  {
    return m_unitless_mgr;
  }

  template <typename ConcreteValueType>
  scalar_manager<ConcreteValueType> &
  get_manager ();

private:
  scalar_manager<integer_with_unit<units::bit>> m_bit_mgr;
  scalar_manager<integer_with_unit<units::byte>> m_byte_mgr;
  scalar_manager<integer> m_unitless_mgr;
};

template<>
const symbolic_bit_size &
scalar_manager<concrete_bit_size>::
get_bits_from_bytes (const symbolic_byte_size &bytes);

} // namespace ana::symbolic

/* Overloaded operators.  */

/* Addition.  */

/* symbolic + symbolic -> symbolic.  */

template<typename ConcreteValueType>
const symbolic::scalar<ConcreteValueType> &
operator+ (const symbolic::scalar<ConcreteValueType> &lhs,
	   const symbolic::scalar<ConcreteValueType> &rhs)
{
  return lhs.get_mgr ().get_sum ({&lhs, &rhs});
}

/* symbolic + concrete -> symbolic.  */

template<typename ConcreteValueType>
const symbolic::scalar<ConcreteValueType> &
operator+ (const symbolic::scalar<ConcreteValueType> &lhs,
	   const ConcreteValueType &rhs)
{
  auto &mgr = lhs.get_mgr ();
  return mgr.get_sum ({&lhs, &mgr.get_concrete (rhs)});
}

/* Negation.  */

template<typename ConcreteValueType>
const symbolic::scalar<ConcreteValueType> &
operator- (const symbolic::scalar<ConcreteValueType> &i)
{
  auto &mgr = i.get_mgr ();
  return mgr.get_negation (i);
}

/* Subtraction.  */

/* symbolic - symbolic -> symbolic.  */

template<typename ConcreteValueType>
const symbolic::scalar<ConcreteValueType> &
operator- (const symbolic::scalar<ConcreteValueType> &lhs,
	   const symbolic::scalar<ConcreteValueType> &rhs)
{
  return lhs.get_mgr ().get_difference (lhs, rhs);
}

/* symbolic - concrete -> symbolic.  */

template<typename ConcreteValueType>
const symbolic::scalar<ConcreteValueType> &
operator- (const symbolic::scalar<ConcreteValueType> &lhs,
	   const ConcreteValueType &rhs)
{
  auto &mgr = lhs.get_mgr ();
  return mgr.get_difference (lhs, mgr.get_concrete (rhs));
}

/* symbolic * symbolic_integer -> symbolic.  */

template<typename ConcreteValueType>
const symbolic::scalar<ConcreteValueType> &
operator* (const symbolic::scalar<ConcreteValueType> &lhs,
	   const symbolic::scalar<integer> &rhs)
{
  return lhs.get_mgr ().get_product (lhs, rhs);
}

inline const symbolic_bit_size &
bits_from_bytes (const symbolic_byte_size &bytes)
{
  auto &bit_mgr
    = bytes.get_mgr ().get_node_manager ().get_scalars ().get_bits ();
  return bit_mgr.get_bits_from_bytes (bytes);
}

const symbolic_byte_size *
maybe_bytes_from_bits (const symbolic_bit_size &bits);

template <typename ConcreteValueType>
inline bool
concrete_from_symbolic (const symbolic::scalar<ConcreteValueType> &in,
			ConcreteValueType *out)
{
  if (auto *concrete = in.dyn_cast_concrete_scalar ())
    {
      *out = concrete->get_concrete ();
      return true;
    }
  return false;
}

template <typename ConcreteValueType>
inline const symbolic::scalar<ConcreteValueType> &
symbolic_from_concrete (const ConcreteValueType &in,
			symbolic::scalar_manager<ConcreteValueType> &mgr)
{
  return mgr.get_concrete (in);
}

} // namespace ana

#endif /* GCC_ANALYZER_SYMBOLIC_SCALARS_H */
