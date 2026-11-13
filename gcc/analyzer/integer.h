/* Arbitrary precision integers.
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

#ifndef GCC_ANALYZER_INTEGER_H
#define GCC_ANALYZER_INTEGER_H

#include "pretty-print-markup.h"

namespace ana {

/* A class for arbitrary precision integers.

   This is a C++ wrapper around gmp's mpz_t, with integration with
   pretty_printer.

   We don't use gmp's gmpxx.h: it uses exceptions, which we can't use.  */

class integer
{
public:
  static constexpr const char *dump_type_name = "integer";

  integer ()
  {
    mpz_init (m_mpz);
  }

  integer (int i)
  {
    mpz_init_set_si (m_mpz, i);
  }

  ~integer ()
  {
    mpz_clear (m_mpz);
  }

  integer (const integer &other)
  {
    mpz_init_set (m_mpz, other.m_mpz);
  }

  integer (integer &&other)
  {
    mpz_init (m_mpz);
    std::swap (m_mpz, other.m_mpz);
  }

  /* Make it easier to use either integer or integer_with_unit
     in a single template.  */
  const integer &
  get_integer () const { return *this; }

  bool
  is_zero_p () const
  {
    return mpz_cmp_si (m_mpz, 0) == 0;
  }

  bool
  is_one_p () const
  {
    return mpz_cmp_si (m_mpz, 1) == 0;
  }

  integer &
  operator= (const integer &other)
  {
    mpz_set (m_mpz, other.m_mpz);
    return *this;
  }

  integer &
  operator= (integer &&other)
  {
    std::swap (m_mpz, other.m_mpz);
    return *this;
  }

  static int
  cmp (const integer &lhs, const integer &rhs)
  {
    return mpz_cmp (lhs.m_mpz, rhs.m_mpz);
  }

  void
  dump_to_pp (pretty_printer *pp) const;

  void
  dump () const;

  std::unique_ptr<json::value>
  to_json () const;

  std::unique_ptr<diagnostics::markup::node>
  make_markup_node () const;

  /* Overloaded operators.  */

  /* Comparison.  */

  bool
  operator== (const integer &other) const
  {
    return mpz_cmp (m_mpz, other.m_mpz) == 0;
  }

  bool
  operator== (int i) const
  {
    return mpz_cmp_si (m_mpz, i) == 0;
  }

  bool
  operator!= (const integer &other) const
  {
    return mpz_cmp (m_mpz, other.m_mpz) != 0;
  }

  bool
  operator!= (int i) const
  {
    return mpz_cmp_si (m_mpz, i) != 0;
  }

  bool
  operator< (const integer &other) const
  {
    return mpz_cmp (m_mpz, other.m_mpz) < 0;
  }

  bool
  operator< (int i) const
  {
    return mpz_cmp_si (m_mpz, i) < 0;
  }

  bool
  operator<= (const integer &other) const
  {
    return mpz_cmp (m_mpz, other.m_mpz) <= 0;
  }

  bool
  operator> (const integer &other) const
  {
    return mpz_cmp (m_mpz, other.m_mpz) > 0;
  }

  bool
  operator>= (const integer &other) const
  {
    return mpz_cmp (m_mpz, other.m_mpz) >= 0;
  }

  /* Addition.  */

  integer
  operator+ (const integer &other) const
  {
    integer result;
    mpz_add (result.m_mpz, m_mpz, other.m_mpz);
    return result;
  }

  integer &
  operator+= (const integer &other)
  {
    integer tmp = *this + other;
    *this = std::move (tmp);
    return *this;
  }

  /* Subtraction.  */

  integer
  operator- (const integer &other) const
  {
    integer result;
    mpz_sub (result.m_mpz, m_mpz, other.m_mpz);
    return result;
  }

  integer
  operator- () const
  {
    integer result;
    mpz_neg (result.m_mpz, m_mpz);
    return result;
  }

  integer &
  operator-= (const integer &other)
  {
    integer tmp = *this - other;
    *this = std::move (tmp);
    return *this;
  }

  /* Multiplication.  */

  integer
  operator* (const integer &other) const
  {
    integer result;
    mpz_mul (result.m_mpz, m_mpz, other.m_mpz);
    return result;
  }

  /* Division.  */

  integer
  floor_div (const integer &other) const
  {
    gcc_assert (other != 0);

    integer result;
    mpz_fdiv_q (result.m_mpz, m_mpz, other.m_mpz);
    return result;
  }

  /* Modulus.  */

  integer
  floor_mod (const integer &other) const
  {
    gcc_assert (other != 0);

    integer result;
    mpz_fdiv_r (result.m_mpz, m_mpz, other.m_mpz);
    return result;
  }

  long
  as_long () const
  {
    return mpz_get_si (m_mpz);
  }

  unsigned long
  as_unsigned_long () const
  {
    return mpz_get_ui (m_mpz);
  }

  mpz_t &
  access_mpz () { return m_mpz; }

private:
  mpz_t m_mpz;
};

integer
convert_from_integer_cst (const_tree int_cst);

tree
make_integer_cst_from_integer (tree type, const integer &value, bool wrap);

using concrete_integer = integer;

class pp_element_integer : public pp_element
{
public:
  pp_element_integer (integer val)
  : m_val (std::move (val))
  {
  }

  void add_to_phase_2 (pp_markup::context &ctxt) final override
  {
    m_val.dump_to_pp (&ctxt.m_pp);
  }

private:
  integer m_val;
};

} // namespace ana

#endif /* GCC_ANALYZER_INTEGER_H */
