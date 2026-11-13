/* Unit-safe arbitrary precision integers (e.g. "32 bytes").
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

#include "analyzer/integer-with-unit.h"

#if ENABLE_ANALYZER

namespace ana {

template<>
void
integer_with_unit<units::byte>::dump_to_pp (pretty_printer *pp,
					    units::role role) const
{
  switch (role)
    {
    default:
      gcc_unreachable ();
    case units::role::generic:
      m_integer.dump_to_pp (pp);
      break;
    case units::role::offset:
      pp_string (pp, "byte ");
      m_integer.dump_to_pp (pp);
      break;
    case units::role::size:
      if (m_integer == 1)
	pp_printf (pp, "1 byte");
      else
	{
	  m_integer.dump_to_pp (pp);
	  pp_string (pp, " bytes");
	}
      break;
    }
}

template<>
void
integer_with_unit<units::bit>::dump_to_pp (pretty_printer *pp,
					   units::role role) const
{
  if (role != units::role::generic)
    {
      // Attempt to print as bytes
      concrete_byte_size bytes;
      if (maybe_bytes_from_bits (*this, &bytes))
	{
	  bytes.dump_to_pp (pp, role);
	  return;
	}
    }
  switch (role)
    {
    default:
      gcc_unreachable ();
    case units::role::generic:
      m_integer.dump_to_pp (pp);
      break;
    case units::role::offset:
      pp_string (pp, "bit ");
      m_integer.dump_to_pp (pp);
      break;
    case units::role::size:
      if (m_integer == 1)
	pp_printf (pp, "1 bit");
      else
	{
	  m_integer.dump_to_pp (pp);
	  pp_string (pp, " bits");
	}
      break;
    }
}

template<>
DEBUG_FUNCTION void
integer_with_unit<units::bit>::dump () const
{
  pretty_printer pp;
  pp.set_output_stream (stderr);
  dump_to_pp (&pp, units::role::size);
  pp_newline (&pp);
  pp_flush (&pp);
}

template<>
DEBUG_FUNCTION void
integer_with_unit<units::byte>::dump () const
{
  pretty_printer pp;
  pp.set_output_stream (stderr);
  dump_to_pp (&pp, units::role::size);
  pp_newline (&pp);
  pp_flush (&pp);
}

template<>
std::unique_ptr<json::object>
integer_with_unit<units::bit>::to_json () const
{
  auto obj = std::make_unique<json::object> ();
  obj->set ("integer", m_integer.to_json ());
  obj->set_string ("unit", "bits");
  return obj;
}

template<>
std::unique_ptr<json::object>
integer_with_unit<units::byte>::to_json () const
{
  auto obj = std::make_unique<json::object> ();
  obj->set ("integer", m_integer.to_json ());
  obj->set_string ("unit", "bytes");
  return obj;
}

template<typename Unit>
std::unique_ptr<diagnostics::markup::node>
integer_with_unit<Unit>::
make_markup_node () const
{
  return m_integer.make_markup_node ();
}

template
std::unique_ptr<diagnostics::markup::node>
integer_with_unit<units::bit>::
make_markup_node () const;

template
std::unique_ptr<diagnostics::markup::node>
integer_with_unit<units::byte>::
make_markup_node () const;

bool
maybe_bytes_from_bits (integer_with_unit<units::bit> bits,
		       integer_with_unit<units::byte> *out)
{
  if (bits.floor_mod (integer_with_unit<units::bit> (BITS_PER_UNIT))
      != 0_bits)
    return false;
  *out = integer_with_unit<units::byte>
    (bits.get_integer ().floor_div (BITS_PER_UNIT));
  return true;
}

integer_with_unit<units::bit>
convert_from_bit_offset_t (bit_offset_t bits)
{
  unsigned HOST_WIDE_INT uhwi = bits.to_uhwi ();
  return integer_with_unit<units::bit> (uhwi);
}

integer_with_unit<units::bit>
convert_from_bit_size_t (bit_size_t bits)
{
  unsigned HOST_WIDE_INT uhwi = bits.to_uhwi ();
  return integer_with_unit<units::bit> (uhwi);
}

integer_with_unit<units::byte>
convert_from_byte_offset_t (byte_offset_t bytes)
{
  unsigned HOST_WIDE_INT uhwi = bytes.to_uhwi ();
  return integer_with_unit<units::byte> (uhwi);
}

integer_with_unit<units::byte>
convert_from_byte_size_t (byte_size_t bytes)
{
  unsigned HOST_WIDE_INT uhwi = bytes.to_uhwi ();
  return integer_with_unit<units::byte> (uhwi);
}

bit_offset_t
convert_to_bit_offset_t (integer_with_unit<units::bit> new_val)
{
  return bit_offset_t (new_val.as_long ());
}

bit_size_t
convert_to_bit_size_t (integer_with_unit<units::bit> new_val)
{
  return bit_offset_t (new_val.as_unsigned_long ());
}

byte_offset_t
convert_to_byte_offset_t (integer_with_unit<units::byte> new_val)
{
  gcc_assert (new_val >= 0_bytes);
  return byte_offset_t (new_val.get_integer ().as_unsigned_long ());
}

byte_size_t
convert_to_byte_size_t (integer_with_unit<units::byte> new_val)
{
  gcc_assert (new_val >= 0_bytes);
  return byte_offset_t (new_val.get_integer ().as_unsigned_long ());
}

#if CHECKING_P

namespace selftest {

/* Unit-independent tests.  */

template<typename Unit>
static void
test_rule_of_5 ()
{
  integer_with_unit<Unit> a;
  ASSERT_EQ (a.get_integer (), 0);

  integer_with_unit<Unit> b (42);
  ASSERT_EQ (b.get_integer (), 42);

  integer_with_unit<Unit> c (b);
  ASSERT_EQ (c.get_integer (), 42);
  // Check that writes to b don't affect c after the copy:
  b = integer_with_unit<Unit> (43);
  ASSERT_EQ (b.get_integer (), 43);
  ASSERT_EQ (c.get_integer (), 42);

  integer_with_unit<Unit> d (std::move (c));
  ASSERT_EQ (d.get_integer (), 42);
  ASSERT_EQ (c.get_integer (), 0);

  c = integer_with_unit<Unit> (1066);
  ASSERT_EQ (c.get_integer (), 1066);

  a = std::move (d);
  ASSERT_EQ (a.get_integer (), 42);
}

/* Test addition, for scalars of the given unit.  */

template<typename Unit>
static void
test_addition ()
{
  ASSERT_EQ (integer_with_unit<Unit> (1), integer_with_unit<Unit> (1));
  ASSERT_NE (integer_with_unit<Unit> (1), integer_with_unit<Unit> (2));

  /* Addition.  */
  ASSERT_EQ (integer_with_unit<Unit> (4) + integer_with_unit<Unit> (3),
	     integer_with_unit<Unit> (7));

  /* Subtraction.  */
  ASSERT_EQ (integer_with_unit<Unit> (4) - integer_with_unit<Unit> (3),
	     integer_with_unit<Unit> (1));

  /* Additive identity.  */
  ASSERT_EQ (integer_with_unit<Unit> (3) + integer_with_unit<Unit> (0),
	     integer_with_unit<Unit> (3));
  ASSERT_EQ (integer_with_unit<Unit> (2) - integer_with_unit<Unit> (0),
	     integer_with_unit<Unit> (2));

  /* Negative values.   */
  ASSERT_EQ (integer_with_unit<Unit> (4) - integer_with_unit<Unit> (5),
	     integer_with_unit<Unit> (-1));
  ASSERT_EQ (integer_with_unit<Unit> (-1) - integer_with_unit<Unit> (-3),
	     integer_with_unit<Unit> (2));
}

/* Test operator++, for scalars of the given unit.  */

template<typename Unit>
static void
test_auto_inc ()
{
  integer_with_unit<Unit> i (0);
  ASSERT_EQ (i, integer_with_unit<Unit> (0));

  ++i;
  ASSERT_EQ (i, integer_with_unit<Unit> (1));

  ++i;
  ASSERT_EQ (i, integer_with_unit<Unit> (2));
}


/* Test multiplication, for scalars of the given unit by unitless integers.  */

template<typename Unit>
static void
test_scaling ()
{
  ASSERT_EQ (integer_with_unit<Unit> (5) * 2,
	     integer_with_unit<Unit> (10));

  /* Identity.  */
  ASSERT_EQ (integer_with_unit<Unit> (7) * 1,
	     integer_with_unit<Unit> (7));

  /* Zero.  */
  ASSERT_EQ (integer_with_unit<Unit> (4) * 0,
	     integer_with_unit<Unit> (0));
}

template<typename Unit>
static void
test_division ()
{
  ASSERT_EQ (integer_with_unit<Unit> (0)
	     .floor_div (integer_with_unit<Unit> (10)),
	     integer (0));
  ASSERT_EQ (integer_with_unit<Unit> (24)
	     .floor_div (integer_with_unit<Unit> (10)),
	     integer (2));
  ASSERT_EQ (integer_with_unit<Unit> (3)
	     .floor_div (integer_with_unit<Unit> (1)),
	     integer (3));
  ASSERT_EQ (integer_with_unit<Unit> (-8)
	     .floor_div (integer_with_unit<Unit> (10)),
	     integer (-1));
}

template<typename Unit>
static void
test_modulus ()
{
  ASSERT_EQ (integer_with_unit<Unit> (0)
	     .floor_mod (integer_with_unit<Unit> (10)),
	     integer_with_unit<Unit> (0));
  ASSERT_EQ (integer_with_unit<Unit> (24)
	     .floor_mod (integer_with_unit<Unit> (10)),
	     integer_with_unit<Unit> (4));
  ASSERT_EQ (integer_with_unit<Unit> (3)
	     .floor_mod (integer_with_unit<Unit> (1)),
	     integer_with_unit<Unit> (0));
  ASSERT_EQ (integer_with_unit<Unit> (-8)
	     .floor_mod (integer_with_unit<Unit> (10)),
	     integer_with_unit<Unit> (2));
}

template<typename Unit>
static void
test_dumping ();

template<>
void
test_dumping<units::bit> ()
{
  {
    auto seven_bits = 7_bits;
    {
      pretty_printer pp;
      seven_bits.dump_to_pp (&pp, units::role::offset);
      ASSERT_STREQ (pp_formatted_text (&pp), "bit 7");
    }
    {
      pretty_printer pp;
      seven_bits.dump_to_pp (&pp, units::role::size);
      ASSERT_STREQ (pp_formatted_text (&pp), "7 bits");
    }
  }

  {
    auto one_bit = 1_bit;
    {
      pretty_printer pp;
      one_bit.dump_to_pp (&pp, units::role::offset);
      ASSERT_STREQ (pp_formatted_text (&pp), "bit 1");
    }
    {
      pretty_printer pp;
      one_bit.dump_to_pp (&pp, units::role::size);
      ASSERT_STREQ (pp_formatted_text (&pp), "1 bit");
    }
  }
}

template<>
void
test_dumping<units::byte> ()
{
  {
    auto seven_bytes = 7_bytes;
    {
      pretty_printer pp;
      seven_bytes.dump_to_pp (&pp, units::role::offset);
      ASSERT_STREQ (pp_formatted_text (&pp), "byte 7");
    }
    {
      pretty_printer pp;
      seven_bytes.dump_to_pp (&pp, units::role::size);
      ASSERT_STREQ (pp_formatted_text (&pp), "7 bytes");
    }
  }

  {
    auto one_byte = 1_byte;
    {
      pretty_printer pp;
      one_byte.dump_to_pp (&pp, units::role::offset);
      ASSERT_STREQ (pp_formatted_text (&pp), "byte 1");
    }
    {
      pretty_printer pp;
      one_byte.dump_to_pp (&pp, units::role::size);
      ASSERT_STREQ (pp_formatted_text (&pp), "1 byte");
    }
  }
}

/* Run all per-unit tests, for Unit.  */

template<typename Unit>
static void
per_unit_scalar_tests ()
{
  test_rule_of_5<Unit> ();
  test_addition<Unit> ();
  test_auto_inc<Unit> ();
  test_scaling<Unit> ();
  test_division<Unit> ();
  test_modulus<Unit> ();
  test_dumping<Unit> ();
}

static void
test_to_json ()
{
  ASSERT_JSON_PRINT_EQ (*(32_bits).to_json (), true,
			("{\"integer\": \"32\",\n"
			 " \"unit\": \"bits\"}"));
  ASSERT_JSON_PRINT_EQ (*(1024_bytes).to_json (), true,
			("{\"integer\": \"1024\",\n"
			 " \"unit\": \"bytes\"}"));
}

static void
test_comparison_of_compatible_mixed_units ()
{
  ASSERT_EQ (integer_with_unit<units::bit> (8),
	     integer_with_unit<units::byte> (1));
  ASSERT_NE (integer_with_unit<units::bit> (7),
	     integer_with_unit<units::byte> (1));
  ASSERT_NE (integer_with_unit<units::bit> (9),
	     integer_with_unit<units::byte> (1));
}

static void
test_addition_of_compatible_mixed_units ()
{
  ASSERT_EQ ((integer_with_unit<units::bit> (8)
	      + integer_with_unit<units::byte> (1)),
	     integer_with_unit<units::bit> (16));

  ASSERT_EQ ((integer_with_unit<units::byte> (1)
	      - integer_with_unit<units::bit> (2)),
	     integer_with_unit<units::bit> (6));
}

/* Unit-dependent tests.  */

static void
test_udls ()
{
  ASSERT_EQ (8_bits, 1_byte);
  ASSERT_NE (7_bits, 1_byte);
  ASSERT_NE (9_bits, 1_byte);
  ASSERT_EQ (1_byte + 1_bit, 9_bits);
  ASSERT_EQ (1_byte - 2_bits, 6_bits);
  ASSERT_EQ ((5_bits * 4) - 2_bytes, 4_bits);
}

/* Consider:
   |
   |  struct coord { uint32_t x; uint32_t y; };
   |  struct triangle { coord m_vertices[3]; };
   |
   and computing sizeof (triangle).  */

static void
test_sizeof_triangle ()
{
  /* In bytes.  */
  {
    auto sizeof_x = 32_bits;
    auto sizeof_y = 32_bits;

    auto sizeof_coord = sizeof_x + sizeof_y;
    ASSERT_EQ (sizeof_coord, 64_bits);

    auto sizeof_triangle = sizeof_coord * 3;
    ASSERT_EQ (sizeof_triangle, 192_bits);
  }

  /* In bits.  */
  {
    auto sizeof_x = 4_bytes;
    auto sizeof_y = 4_bytes;

    auto sizeof_coord = sizeof_x + sizeof_y;
    ASSERT_EQ (sizeof_coord, 8_bytes);

    auto sizeof_triangle = sizeof_coord * 3;
    ASSERT_EQ (sizeof_triangle, 24_bytes);
  }
}

static void
test_round_bit_to_lowest_byte ()
{
  ASSERT_EQ (round_bit_to_lowest_byte (0_bits), 0_bytes);
  ASSERT_EQ (round_bit_to_lowest_byte (1_bit), 0_bytes);
  ASSERT_EQ (round_bit_to_lowest_byte (7_bits), 0_bytes);
  ASSERT_EQ (round_bit_to_lowest_byte (8_bits), 1_byte);
  ASSERT_EQ (round_bit_to_lowest_byte (15_bits), 1_byte);
  ASSERT_EQ (round_bit_to_lowest_byte (16_bits), 2_bytes);
  ASSERT_EQ (round_bit_to_lowest_byte (23_bits), 2_bytes);
  ASSERT_EQ (round_bit_to_lowest_byte (24_bits), 3_bytes);
}

static void
test_bit_within_byte ()
{
  ASSERT_EQ (bit_within_byte (0_bits), 0_bits);
  ASSERT_EQ (bit_within_byte (1_bit), 1_bit);
  ASSERT_EQ (bit_within_byte (7_bits), 7_bits);
  ASSERT_EQ (bit_within_byte (8_bits), 0_bits);
  ASSERT_EQ (bit_within_byte (15_bits), 7_bits);
  ASSERT_EQ (bit_within_byte (16_bits), 0_bits);
  ASSERT_EQ (bit_within_byte (23_bits), 7_bits);
  ASSERT_EQ (bit_within_byte (24_bits), 0_bits);
}

/* Run all of the selftests within this file.  */

void
analyzer_integer_with_unit_cc_tests ()
{
  per_unit_scalar_tests<units::bit> ();
  per_unit_scalar_tests<units::byte> ();

  test_to_json ();

  test_comparison_of_compatible_mixed_units ();
  test_addition_of_compatible_mixed_units ();

  test_udls ();

  test_sizeof_triangle ();

  test_round_bit_to_lowest_byte ();
  test_bit_within_byte ();
}

} // namespace selftest

#endif /* CHECKING_P */

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
