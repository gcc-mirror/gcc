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

#include "analyzer/common.h"

#include "diagnostics/markup.h"

#include "selftest.h"
#include "selftest-json.h"

#include "analyzer/integer.h"

#if ENABLE_ANALYZER

namespace ana {

void
integer::dump_to_pp (pretty_printer *pp) const
{
  char *str = nullptr;
  gmp_asprintf (&str, "%Zd", m_mpz);
  gcc_assert (str);

  pp_string (pp, str);

  void (*free_func_ptr) (void *, size_t) = nullptr;
  mp_get_memory_functions (nullptr, nullptr, &free_func_ptr);
  gcc_assert (free_func_ptr);
  free_func_ptr (str, 0);
}

DEBUG_FUNCTION void
integer::dump () const
{
  pretty_printer pp;
  pp.set_output_stream (stderr);
  dump_to_pp (&pp);
  pp_newline (&pp);
  pp_flush (&pp);
}

std::unique_ptr<json::value>
integer::to_json () const
{
  pretty_printer pp;
  dump_to_pp (&pp);
  return std::make_unique<json::string> (pp_formatted_text (&pp));
}

std::unique_ptr<diagnostics::markup::node>
integer::make_markup_node () const
{
  pretty_printer pp;
  dump_to_pp (&pp);
  return diagnostics::markup::make_text_node (pp_formatted_text (&pp));
}

integer
convert_from_integer_cst (const_tree int_cst)
{
  gcc_assert (int_cst);
  gcc_assert (TREE_CODE (int_cst) == INTEGER_CST);

  integer result;
  wi::to_mpz (wi::to_wide (int_cst),
	      result.access_mpz (),
	      TYPE_SIGN (TREE_TYPE (int_cst)));
  return result;
}

tree
make_integer_cst_from_integer (tree type, const integer &value, bool wrap)
{
  gcc_assert (type);

  /* Need to make a copy, as wi::from_mpz modifies its argument.  */
  integer mutable_copy (value);
  wide_int w = wi::from_mpz (type, mutable_copy.access_mpz (), wrap);
  return wide_int_to_tree (type, w);
}

#if CHECKING_P

namespace selftest {

static void
test_rule_of_5 ()
{
  integer a;
  ASSERT_EQ (a, 0);

  integer b (42);
  ASSERT_EQ (b, 42);

  integer c (b);
  ASSERT_EQ (c, 42);
  // Check that writes to b don't affect c after the copy:
  b = 43;
  ASSERT_EQ (b, 43);
  ASSERT_EQ (c, 42);

  integer d (std::move (c));
  ASSERT_EQ (d, 42);
  ASSERT_EQ (c, 0);

  c = integer (1066);
  ASSERT_EQ (c, 1066);

  a = std::move (d);
  ASSERT_EQ (a, 42);
}

static void
test_comparisons ()
{
  /* Equality.  */
  ASSERT_TRUE (integer (0) == integer (0));
  ASSERT_FALSE (integer (0) == integer (1));
  ASSERT_TRUE (integer (0) == 0);
  ASSERT_FALSE (integer (0) == 42);

  /* Inequality.  */
  ASSERT_TRUE (integer (0) != integer (1));
  ASSERT_FALSE (integer (0) != integer (0));
  ASSERT_TRUE (integer (0) != 1);
  ASSERT_FALSE (integer (0) != 0);

  // operator<
  ASSERT_TRUE (integer (-1) < integer (0));
  ASSERT_FALSE (integer (0) < integer (0));
  ASSERT_TRUE (integer (2) < integer (3));
  ASSERT_FALSE (integer (3) < integer (2));

  // Default ctor should give zero:
  ASSERT_TRUE (integer () == integer (0));
}

static void
test_addition ()
{
  ASSERT_EQ (integer (1) + integer (2),
	     integer (3));

  // Reflexivity
  ASSERT_EQ (integer (1) + integer (2),
	     integer (2) + integer (1));

  // Commutativity
  ASSERT_EQ ((integer (1) + integer (2)) + integer (3),
	     integer (6));
  ASSERT_EQ (integer (1) + (integer (2) + integer (3)),
	     integer (6));
}

static void
test_subtraction ()
{
  // Identity
  ASSERT_EQ (integer (2) - integer (0),
	     integer (2));

  // Positive result
  ASSERT_EQ (integer (11) - integer (7),
	     integer (4));

  // Negative reult
  ASSERT_EQ (integer (5) - integer (10),
	     integer (-5));

  // Negative operands
  ASSERT_EQ (integer (-2) - integer (2),
	     integer (-4));
  ASSERT_EQ (integer (2) - integer (-2),
	     integer (4));
}

static void
test_multiplication ()
{
  // Zero:
  ASSERT_EQ (integer (2) * integer (0),
	     integer (0));
  ASSERT_EQ (integer (0) * integer (2),
	     integer (0));

  // Identity
  ASSERT_EQ (integer (3) * integer (1),
	     integer (3));
  ASSERT_EQ (integer (1) * integer (3),
	     integer (3));

  // Combinations of positive and negative
  ASSERT_EQ (integer (2) * integer (3),
	     integer (6));
  ASSERT_EQ (integer (4) * integer (-5),
	     integer (-20));
  ASSERT_EQ (integer (-4) * integer (5),
	     integer (-20));
  ASSERT_EQ (integer (-4) * integer (-5),
	     integer (20));
}

static void
test_division ()
{
  ASSERT_EQ (integer (0).floor_div (10),
	     integer (0));
  ASSERT_EQ (integer (24).floor_div (10),
	     integer (2));
  ASSERT_EQ (integer (3).floor_div (1),
	     integer (3));

  ASSERT_EQ (integer (-8).floor_div (10),
	     integer (-1));
}

static void
test_modulus ()
{
  ASSERT_EQ (integer (0).floor_mod (10),
	     integer (0));
  ASSERT_EQ (integer (24).floor_mod (10),
	     integer (4));
  ASSERT_EQ (integer (3).floor_mod (1),
	     integer (0));

  ASSERT_EQ (integer (-8).floor_mod (10),
	     integer (2));
}

static void
test_dumping ()
{
  // Positive
  {
    pretty_printer pp;
    integer (42).dump_to_pp (&pp);
    ASSERT_STREQ (pp_formatted_text (&pp), "42");
  }

  // Negative
  {
    pretty_printer pp;
    integer (-100).dump_to_pp (&pp);
    ASSERT_STREQ (pp_formatted_text (&pp), "-100");
  }
}

static void
test_very_large ()
{
  /* Large power of two.  */
  {
    const integer two_to_the_12 (1 << 12);
    const integer two_to_the_24 = two_to_the_12 * two_to_the_12;
    const integer two_to_the_48 = two_to_the_24 * two_to_the_24;
    const integer two_to_the_96 = two_to_the_48 * two_to_the_48;
    const integer two_to_the_192 = two_to_the_96 * two_to_the_96;
    {
      pretty_printer pp;
      two_to_the_192.dump_to_pp (&pp);
      ASSERT_STREQ
	(pp_formatted_text (&pp),
	 "6277101735386680763835789423207666416102355444464034512896");
    }
    // Negation
    {
      pretty_printer pp;
      (-two_to_the_192).dump_to_pp (&pp);
      ASSERT_STREQ
	(pp_formatted_text (&pp),
	 "-6277101735386680763835789423207666416102355444464034512896");
    }
  }

  /* Large power of ten.  */
  {
    const integer million (1000 * 1000);
    const integer ten_to_the_12 = million * million;
    const integer ten_to_the_24 = ten_to_the_12 * ten_to_the_12;
    const integer ten_to_the_48 = ten_to_the_24 * ten_to_the_24;
    const integer ten_to_the_96 = ten_to_the_48 * ten_to_the_48;
    const integer ten_to_the_192 = ten_to_the_96 * ten_to_the_96;
    {
      pretty_printer pp;
      ten_to_the_192.dump_to_pp (&pp);
      ASSERT_STREQ
	(pp_formatted_text (&pp),
	 ("1"
	  "000000000000000000000000000000000000000000000000"
	  "000000000000000000000000000000000000000000000000"
	  "000000000000000000000000000000000000000000000000"
	  "000000000000000000000000000000000000000000000000"));
    }

    // Negation
    {
      pretty_printer pp;
      (-ten_to_the_192).dump_to_pp (&pp);
      ASSERT_STREQ
	(pp_formatted_text (&pp),
	 ("-1"
	  "000000000000000000000000000000000000000000000000"
	  "000000000000000000000000000000000000000000000000"
	  "000000000000000000000000000000000000000000000000"
	  "000000000000000000000000000000000000000000000000"));
    }

    // Addition
    {
      pretty_printer pp;
      (ten_to_the_192 + ten_to_the_192).dump_to_pp (&pp);
      ASSERT_STREQ
	(pp_formatted_text (&pp),
	 ("2"
	  "000000000000000000000000000000000000000000000000"
	  "000000000000000000000000000000000000000000000000"
	  "000000000000000000000000000000000000000000000000"
	  "000000000000000000000000000000000000000000000000"));
    }
  }
}

static void
test_pp_element ()
{
  pretty_printer pp;
  pp_element_integer e1 (integer (42));
  pp_element_integer e2 (integer (-100));
  pp_printf (&pp, "positive: %e negative: %e", &e1, &e2);
  ASSERT_STREQ (pp_formatted_text (&pp), "positive: 42 negative: -100");
}

static integer
get_min_value (tree integral_type)
{
  gcc_assert (integral_type);
  return convert_from_integer_cst (TYPE_MIN_VALUE (integral_type));
}

static integer
get_max_value (tree integral_type)
{
  gcc_assert (integral_type);
  return convert_from_integer_cst (TYPE_MAX_VALUE (integral_type));
}

static void
test_convert_from_integer_cst ()
{
  ASSERT_TRUE (get_min_value (signed_char_type_node) < 0);
  ASSERT_TRUE (get_max_value (signed_char_type_node) > 0);

  ASSERT_TRUE (get_min_value (unsigned_char_type_node) == 0);
  ASSERT_TRUE (get_max_value (unsigned_char_type_node) > 0);

  ASSERT_TRUE (get_min_value (integer_type_node) < 0);
  ASSERT_TRUE (get_max_value (integer_type_node) > 0);
  ASSERT_EQ (convert_from_integer_cst (integer_zero_node), 0);
  ASSERT_EQ (convert_from_integer_cst (integer_one_node), 1);
  ASSERT_EQ (convert_from_integer_cst (integer_minus_one_node), -1);

  ASSERT_TRUE (get_min_value (unsigned_type_node) == 0);
  ASSERT_TRUE (get_max_value (unsigned_type_node) > 0);

  ASSERT_TRUE (get_min_value (long_integer_type_node) < 0);
  ASSERT_TRUE (get_max_value (long_integer_type_node) > 0);

  ASSERT_TRUE (get_min_value (long_unsigned_type_node) == 0);
  ASSERT_TRUE (get_max_value (long_unsigned_type_node) > 0);

  ASSERT_TRUE (get_min_value (long_long_integer_type_node) < 0);
  ASSERT_TRUE (get_max_value (long_long_integer_type_node) > 0);

  ASSERT_TRUE (get_min_value (long_long_unsigned_type_node) == 0);
  ASSERT_TRUE (get_max_value (long_long_unsigned_type_node) > 0);

  ASSERT_TRUE (get_min_value (size_type_node) == 0);
  ASSERT_TRUE (get_max_value (size_type_node) > 0);
  ASSERT_EQ (convert_from_integer_cst (size_zero_node), 0);
  ASSERT_EQ (convert_from_integer_cst (size_one_node), 1);

  ASSERT_TRUE (get_min_value (ptrdiff_type_node) < 0);
  ASSERT_TRUE (get_max_value (ptrdiff_type_node) > 0);
}

// Verify that INT_CST round-trips through integer.

static void
assert_round_trip (const ::selftest::location &loc, tree int_cst)
{
  integer as_integer = convert_from_integer_cst (int_cst);
  tree round_tripped_cst = make_integer_cst_from_integer (TREE_TYPE (int_cst),
							  as_integer, false);
  ASSERT_EQ_AT (loc, TREE_TYPE (int_cst), TREE_TYPE (round_tripped_cst));
  ASSERT_TRUE_AT (loc, tree_int_cst_equal (int_cst, round_tripped_cst));
}

#define ASSERT_ROUND_TRIP(INT_CST)	\
  SELFTEST_BEGIN_STMT							\
  assert_round_trip ((SELFTEST_LOCATION), (INT_CST));			\
  SELFTEST_END_STMT

static void
test_round_trip_of_integer_cst ()
{
  ASSERT_ROUND_TRIP (TYPE_MIN_VALUE (signed_char_type_node));
  ASSERT_ROUND_TRIP (TYPE_MAX_VALUE (signed_char_type_node));

  ASSERT_ROUND_TRIP (TYPE_MIN_VALUE (unsigned_char_type_node));
  ASSERT_ROUND_TRIP (TYPE_MAX_VALUE (unsigned_char_type_node));

  ASSERT_ROUND_TRIP (TYPE_MIN_VALUE (integer_type_node));
  ASSERT_ROUND_TRIP (TYPE_MAX_VALUE (integer_type_node));
  ASSERT_ROUND_TRIP (integer_zero_node);
  ASSERT_ROUND_TRIP (integer_one_node);
  ASSERT_ROUND_TRIP (integer_minus_one_node);

  ASSERT_ROUND_TRIP (TYPE_MIN_VALUE (unsigned_type_node));
  ASSERT_ROUND_TRIP (TYPE_MAX_VALUE (unsigned_type_node));

  ASSERT_ROUND_TRIP (TYPE_MIN_VALUE (long_integer_type_node));
  ASSERT_ROUND_TRIP (TYPE_MAX_VALUE (long_integer_type_node));

  ASSERT_ROUND_TRIP (TYPE_MIN_VALUE (long_unsigned_type_node));
  ASSERT_ROUND_TRIP (TYPE_MAX_VALUE (long_unsigned_type_node));

  ASSERT_ROUND_TRIP (TYPE_MIN_VALUE (long_long_integer_type_node));
  ASSERT_ROUND_TRIP (TYPE_MAX_VALUE (long_long_integer_type_node));

  ASSERT_ROUND_TRIP (TYPE_MIN_VALUE (long_long_unsigned_type_node));
  ASSERT_ROUND_TRIP (TYPE_MAX_VALUE (long_long_unsigned_type_node));

  ASSERT_ROUND_TRIP (TYPE_MIN_VALUE (size_type_node));
  ASSERT_ROUND_TRIP (TYPE_MAX_VALUE (size_type_node));
  ASSERT_ROUND_TRIP (size_zero_node);
  ASSERT_ROUND_TRIP (size_one_node);

  ASSERT_ROUND_TRIP (TYPE_MIN_VALUE (ptrdiff_type_node));
  ASSERT_ROUND_TRIP (TYPE_MAX_VALUE (ptrdiff_type_node));
}

static void
test_to_json ()
{
  ASSERT_JSON_STRING_EQ (integer (42).to_json ().get (),
			 "42");
}

/* Run all of the selftests within this file.  */

void
analyzer_integer_cc_tests ()
{
  test_rule_of_5 ();
  test_comparisons ();
  test_addition ();
  test_subtraction ();
  test_multiplication ();
  test_division ();
  test_modulus ();
  test_dumping ();
  test_very_large ();
  test_pp_element ();
  test_convert_from_integer_cst ();
  test_round_trip_of_integer_cst ();
  test_to_json ();
}

} // namespace selftest

#endif /* CHECKING_P */

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
