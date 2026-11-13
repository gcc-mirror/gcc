/* Unit-safe concrete ranges of values (e.g. "bits 16-31", "bytes 0-1023").
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

#include "analyzer/concrete-ranges.h"

#if ENABLE_ANALYZER

namespace ana {

template <typename ConcreteValueType>
static void
dump_any_singular_name (pretty_printer *);

template <>
void
dump_any_singular_name<integer> (pretty_printer *)
{
  // no-op
}

template <>
void
dump_any_singular_name<integer_with_unit<units::bit>> (pretty_printer *pp)
{
  pp_string (pp, "bit ");
}

template <>
void
dump_any_singular_name<integer_with_unit<units::byte>> (pretty_printer *pp)
{
  pp_string (pp, "byte ");
}

template <typename ConcreteValueType>
static void
dump_any_plural_name (pretty_printer *);

template <>
void
dump_any_plural_name<integer> (pretty_printer *)
{
  // no-op
}

template <>
void
dump_any_plural_name<integer_with_unit<units::bit>> (pretty_printer *pp)
{
  pp_string (pp, "bits ");
}

template <>
void
dump_any_plural_name<integer_with_unit<units::byte>> (pretty_printer *pp)
{
  pp_string (pp, "bytes ");
}

template<typename ConcreteValueType>
void
concrete::range<ConcreteValueType>::dump_to_pp (pretty_printer *pp) const
{
  pp_character (pp, '[');
  if (m_size.get_integer () == 0)
    {
      pp_character (pp, ']');
      return;
    }
  if (m_size.get_integer () == 1)
    {
      dump_any_singular_name<ConcreteValueType> (pp);
      m_start_offset.dump_to_pp (pp);
      pp_character (pp, ']');
      return;
    }
  dump_any_plural_name<ConcreteValueType> (pp);
  m_start_offset.dump_to_pp (pp);
  pp_string (pp, "..");
  get_next_offset ().dump_to_pp (pp);
  pp_character (pp, ')');
}

/* "_pretty" variant.  Same for bytes  */

template<>
void
concrete_byte_range::dump_to_pp_pretty (pretty_printer *pp) const
{
  dump_to_pp (pp);
}

// ...but for bits, try to express as bytes if possible:

template<>
void
concrete_bit_range::dump_to_pp_pretty (pretty_printer *pp) const
{
  concrete::range<integer_with_unit<units::byte>> byte_range;
  if (maybe_bytes_from_bits (*this, &byte_range))
    byte_range.dump_to_pp (pp);
  else
    dump_to_pp (pp);
}

template<>
DEBUG_FUNCTION void
concrete_bit_range::dump () const
{
  pretty_printer pp;
  pp.set_output_stream (stderr);
  dump_to_pp_pretty (&pp);
  pp_newline (&pp);
  pp_flush (&pp);
}

template<>
DEBUG_FUNCTION void
concrete_byte_range::dump () const
{
  pretty_printer pp;
  pp.set_output_stream (stderr);
  dump_to_pp_pretty (&pp);
  pp_newline (&pp);
  pp_flush (&pp);
}

template<typename ConcreteValueType>
int
concrete::range<ConcreteValueType>::cmp (const range &lhs,
					 const range &rhs)
{
  if (int c = ConcreteValueType::cmp (lhs.get_start_offset (),
				      rhs.get_start_offset ()))
    return c;
  return ConcreteValueType::cmp (lhs.get_size (),
				 rhs.get_size ());
}

template<typename ConcreteValueType>
bool
concrete::range<ConcreteValueType>::intersects_p (const range &other,
						  range *out) const
{
  if (get_start_offset () >= other.get_next_offset ())
    return false;
  if (get_next_offset () <= other.get_start_offset ())
    return false;
  if (out)
    {
      ConcreteValueType overlap_start
	= std::max (get_start_offset (),
		    other.get_start_offset ());
      ConcreteValueType overlap_next
	= std::min (get_next_offset (),
		    other.get_next_offset ());
      gcc_assert (overlap_next > overlap_start);
      *out = range<ConcreteValueType> (overlap_start, overlap_next - overlap_start);
    }
  return true;
}

template
bool concrete_bit_range::intersects_p (const concrete_bit_range &other,
				       concrete_bit_range *out) const;

/* Return true if any part of THIS is >= OFFSET, and write the overhanging
   range to *OUT.  */

template<typename ConcreteValueType>
bool
concrete::range<ConcreteValueType>::
exceeds_p (ConcreteValueType offset,
	   range<ConcreteValueType> *out) const
{
  ConcreteValueType next_offset = get_next_offset ();
  if (next_offset > offset)
    {
      /* THIS exceeds OFFSET.  */
      auto start = std::max (get_start_offset (), offset);
      auto size = next_offset - start;
      *out = range<ConcreteValueType> (start, size);
      return true;
    }
  return false;
}

template
bool concrete_bit_range::exceeds_p (integer_with_unit<units::bit>,
				    concrete_bit_range *) const;

/* Return true if any part of this THIS < OFFSET and write the
   bit range fallen short to *OUT.  */

template<typename ConcreteValueType>
bool
concrete::range<ConcreteValueType>::
falls_short_of_p (ConcreteValueType offset,
		  range<ConcreteValueType> *out) const
{
  ConcreteValueType start_offset = get_start_offset ();
  if (start_offset < offset)
    {
      /* THIS falls short of OFFSET.  */
      auto start = start_offset;
      auto size = std::min (offset, get_next_offset ()) - start;
      *out = range<ConcreteValueType> (start, size);
      return true;
    }
  return false;
}

template
bool concrete_bit_range::falls_short_of_p (integer_with_unit<units::bit>,
					   concrete_bit_range *) const;

template<typename ConcreteValueType>
std::unique_ptr<json::object>
concrete::range<ConcreteValueType>::to_json () const
{
  auto obj = std::make_unique<json::object> ();
  obj->set ("start_offset", m_start_offset.to_json ());
  obj->set ("size", m_size.to_json ());
  return obj;
}

template
std::unique_ptr<json::object>
concrete_bit_range::to_json () const;

template<typename ConcreteValueType>
std::unique_ptr<diagnostics::markup::node>
concrete::range<ConcreteValueType>::
make_markup_node () const
{
  return std::make_unique<diagnostics::markup::math::half_open_interval>
    (m_start_offset.make_markup_node (),
     get_next_offset ().make_markup_node ());
}

template
std::unique_ptr<diagnostics::markup::node>
concrete_bit_range::make_markup_node () const;

bool
maybe_bytes_from_bits (concrete::range<integer_with_unit<units::bit>> bits,
		     concrete::range<integer_with_unit<units::byte>> *out)
{
  integer_with_unit<units::byte> start_byte_offset;
  if (!maybe_bytes_from_bits (bits.get_start_offset (),
			      &start_byte_offset))
    return false;
  integer_with_unit<units::byte> byte_size;
  if (!maybe_bytes_from_bits (bits.get_size (),
			      &byte_size))
    return false;
  *out = concrete::range<integer_with_unit<units::byte>> (start_byte_offset,
							  byte_size);
  return true;
}

/* If MASK is a contiguous range of set bits, write them
   to *OUT and return true.
   Otherwise return false.  */

bool
concrete_bit_range_from_mask (unsigned HOST_WIDE_INT mask,
			      concrete_bit_range *out)
{
  concrete_bit_offset iter_bit_idx = 0_bits;
  unsigned HOST_WIDE_INT iter_bit_mask = 1;

  concrete_bit_size host_bits_per_wide_int (HOST_BITS_PER_WIDE_INT);

  /* Find the first contiguous run of set bits in MASK.  */

  /* Find first set bit in MASK.  */
  while (iter_bit_idx < host_bits_per_wide_int)
    {
      if (mask & iter_bit_mask)
	break;
      iter_bit_idx += 1_bit;
      iter_bit_mask <<= 1;
    }
  if (iter_bit_idx == host_bits_per_wide_int)
    /* MASK is zero.  */
    return false;

  concrete_bit_offset first_set_iter_bit_idx (iter_bit_idx);
  concrete_bit_size num_set_bits = 1_bit;
  iter_bit_idx += 1_bit;
  iter_bit_mask <<= 1;

  /* Find next unset bit in MASK.  */
  while (iter_bit_idx < host_bits_per_wide_int)
    {
      if (!(mask & iter_bit_mask))
	break;
      num_set_bits += 1_bit;
      iter_bit_idx += 1_bit;
      iter_bit_mask <<= 1;
    }
  if (iter_bit_idx == host_bits_per_wide_int)
    {
      *out = concrete_bit_range (first_set_iter_bit_idx, num_set_bits);
      return true;
    }

  /* We now have the first contiguous run of set bits in MASK.
     Fail if any other bits are set.  */
  while (iter_bit_idx < host_bits_per_wide_int)
    {
      if (mask & iter_bit_mask)
	return false;
      iter_bit_idx += 1_bit;
      iter_bit_mask <<= 1;
    }

  *out = concrete_bit_range (first_set_iter_bit_idx, num_set_bits);
  return true;
}

void
get_smallest_enclosing_byte_range (const concrete_bit_range &in_bit_range,
				   concrete_byte_range *out_byte_range,
				   concrete_bit_offset *out_bit_offset)
{
  auto round_bit_to_lowest_byte = [](concrete_bit_offset bit_offset)
    -> concrete_byte_offset
  {
    return concrete_byte_offset
      (bit_offset.get_integer ().floor_div (BITS_PER_UNIT));
  };
  auto bit_within_byte = [](concrete_bit_offset bit_offset)
    -> concrete_bit_offset
  {
    return concrete_bit_offset
      (bit_offset.get_integer ().floor_mod (BITS_PER_UNIT));
  };

  *out_byte_range = concrete_byte_range
    (round_bit_to_lowest_byte (in_bit_range.get_start_offset ()),
     round_bit_to_lowest_byte (in_bit_range.get_size () + 7_bits));
  *out_bit_offset = bit_within_byte (in_bit_range.get_start_offset ());
}

#if CHECKING_P

namespace selftest {

/* Implementation detail of ASSERT_OVERLAP.  */

template<typename Unit>
static void
assert_overlap (const ::selftest::location &loc,
		const concrete::range<Unit> &r1,
		const concrete::range<Unit> &r2,
		const concrete::range<Unit> &expected_overlap)
{
  // Test both ways round
  {
    concrete::range<Unit> actual_overlap;
    ASSERT_TRUE_AT (loc, r1.intersects_p (r2, &actual_overlap));
    ASSERT_EQ_AT (loc, actual_overlap, expected_overlap);
  }
  {
    concrete::range<Unit> actual_overlap;
    ASSERT_TRUE_AT (loc, r2.intersects_p (r1, &actual_overlap));
    ASSERT_EQ_AT (loc, actual_overlap, expected_overlap);
  }
}

/* Implementation detail of ASSERT_DISJOINT.  */

template<typename Unit>
static void
assert_disjoint (const ::selftest::location &loc,
		 const concrete::range<Unit> &r1,
		 const concrete::range<Unit> &r2)
{
  // Test both ways round
  ASSERT_FALSE_AT (loc, r1.intersects_p (r2));
  ASSERT_FALSE_AT (loc, r2.intersects_p (r1));
}

/* Assert that R1 and R2 overlap, checking both ways.  */

#define ASSERT_OVERLAP(R1, R2, EXPECTED_OVERLAP) \
  SELFTEST_BEGIN_STMT							\
  assert_overlap (SELFTEST_LOCATION, (R1), (R2), (EXPECTED_OVERLAP));	\
  SELFTEST_END_STMT

/* Assert that R1 and R2 do not overlap, checking both ways.  */

#define ASSERT_DISJOINT(R1, R2) \
  SELFTEST_BEGIN_STMT					\
  assert_disjoint (SELFTEST_LOCATION, (R1), (R2));	\
  SELFTEST_END_STMT

template<typename ConcreteValueType>
static void
test_intersections ()
{
  auto make_range = [] (int start, int size)
  {
    return concrete::range<ConcreteValueType> (ConcreteValueType(start),
					       ConcreteValueType(size));
  };

  /* Various size 8 ranges.  */
  concrete::range<ConcreteValueType> r0_7 = make_range (0, 8);
  concrete::range<ConcreteValueType> r8_15 = make_range (8, 8);
  concrete::range<ConcreteValueType> r16_23 = make_range (16, 8);
  concrete::range<ConcreteValueType> r24_31 = make_range (24, 8);

  /* Various size 16 ranges.  */
  concrete::range<ConcreteValueType> r0_15 = make_range (0, 16);
  concrete::range<ConcreteValueType> r8_23 = make_range (8, 16);
  concrete::range<ConcreteValueType> r16_31 = make_range (16, 16);

  /* Size 32 range.  */
  concrete::range<ConcreteValueType> r0_31 = make_range (0, 32);

  /* Everything should self-overlap.  */
  ASSERT_OVERLAP (r0_7, r0_7, r0_7);
  ASSERT_OVERLAP (r8_15, r8_15, r8_15);
  ASSERT_OVERLAP (r16_23, r16_23, r16_23);
  ASSERT_OVERLAP (r24_31, r24_31, r24_31);
  ASSERT_OVERLAP (r0_15, r0_15, r0_15);
  ASSERT_OVERLAP (r8_23, r8_23, r8_23);
  ASSERT_OVERLAP (r16_31, r16_31, r16_31);
  ASSERT_OVERLAP (r0_31, r0_31, r0_31);

  /* Verify the 8-bit bindings that don't overlap each other.  */
  ASSERT_DISJOINT (r0_7, r8_15);
  ASSERT_DISJOINT (r8_15, r16_23);

  /* Check for overlap of differently-sized bindings.  */
  ASSERT_OVERLAP (r0_7, r0_31, r0_7);
  /* ...and with differing start points.  */
  ASSERT_OVERLAP (r8_15, r0_31, r8_15);
  ASSERT_DISJOINT (r8_15, r16_31);
  ASSERT_OVERLAP (r16_23, r0_31, r16_23);
  ASSERT_OVERLAP (r16_31, r0_31, r16_31);

  ASSERT_DISJOINT (r0_7, r8_23);
  ASSERT_OVERLAP (r8_23, r16_23, r16_23);
  ASSERT_OVERLAP (r8_23, r16_31, r16_23);
  ASSERT_DISJOINT (r8_23, r24_31);
}

template<typename ConcreteValueType>
static void
test_falls_short_of_p ()
{
  using concrete_range_type = concrete::range<ConcreteValueType>;
  auto make_range = [] (int start, int size)
  {
    return concrete_range_type (ConcreteValueType(start),
				ConcreteValueType(size));
  };

  concrete_range_type rm4_m1 = make_range (-4, 4);
  concrete_range_type r0_3 = make_range (0, 4);
  concrete_range_type r10 = make_range (10, 1);

  /* [0, 4) is fully after 0.  */
  {
    concrete_range_type out;
    ASSERT_FALSE (r0_3.falls_short_of_p (ConcreteValueType (0),
					 &out));
  }

  /* [0, 4) is not fully after 10; it's fully before.  */
  {
    concrete_range_type out;
    ASSERT_TRUE (r0_3.falls_short_of_p (ConcreteValueType (10),
					&out));
    ASSERT_EQ (out.get_start_offset (), ConcreteValueType (0));
    ASSERT_EQ (out.get_size (), ConcreteValueType (4));
    ASSERT_EQ (out.get_next_offset (), ConcreteValueType (4));
  }

  /* [0, 4) is not fully after 2.  */
  {
    concrete_range_type out;
    ASSERT_TRUE (r0_3.falls_short_of_p (ConcreteValueType (2),
					&out));
    ASSERT_EQ (out.get_start_offset (), ConcreteValueType (0));
    ASSERT_EQ (out.get_size (), ConcreteValueType (2));
    ASSERT_EQ (out.get_next_offset (), ConcreteValueType (2));
  }

  {
    concrete_range_type out;
    ASSERT_TRUE (rm4_m1.falls_short_of_p (ConcreteValueType (0),
					  &out));
    ASSERT_EQ (out.get_start_offset (), ConcreteValueType (-4));
    ASSERT_EQ (out.get_size (), ConcreteValueType (4));
    ASSERT_EQ (out.get_next_offset (), ConcreteValueType (0));
  }

  {
    concrete_range_type out;
    ASSERT_TRUE (r10.falls_short_of_p (ConcreteValueType (15),
				       &out));
    ASSERT_EQ (out.get_start_offset (), ConcreteValueType (10));
    ASSERT_EQ (out.get_size (), ConcreteValueType (1));
    ASSERT_EQ (out.get_next_offset (), ConcreteValueType (11));
  }
}

template<typename ConcreteValueType>
static void
test_exceeds_p ()
{
  using concrete_range_type = concrete::range<ConcreteValueType>;
  auto make_range = [] (int start, int size)
  {
    return concrete_range_type (ConcreteValueType(start),
				ConcreteValueType(size));
  };

  concrete_range_type r0_3 = make_range (0, 4);
  concrete_range_type r10 = make_range (10, 1);

  /* [0, 4) is fully before 4.  */
  {
    concrete_range_type out;
    ASSERT_FALSE (r0_3.exceeds_p (ConcreteValueType (4),
				  &out));
  }

  /* [0, 4) is not fully before 3: the overlap is {3}.  */
  {
    concrete_range_type out;
    ASSERT_TRUE (r0_3.exceeds_p (ConcreteValueType (3),
				 &out));
    ASSERT_EQ (out.get_start_offset (), ConcreteValueType (3));
    ASSERT_EQ (out.get_size (), ConcreteValueType (1));
    ASSERT_EQ (out.get_next_offset (), ConcreteValueType (4));
  }

  /* [0, 4) is not fully before 2: the overlap is {2, 3}.  */
  {
    concrete_range_type out;
    ASSERT_TRUE (r0_3.exceeds_p (ConcreteValueType (2),
				 &out));
    ASSERT_EQ (out.get_start_offset (), ConcreteValueType (2));
    ASSERT_EQ (out.get_size (), ConcreteValueType (2));
    ASSERT_EQ (out.get_next_offset (), ConcreteValueType (4));
  }

  /* {10} is not fully before 5: the overlap is {10}.  */
  {
    concrete_range_type out;
    ASSERT_TRUE (r10.exceeds_p (ConcreteValueType (5),
				&out));
    ASSERT_EQ (out.get_start_offset (), ConcreteValueType (10));
    ASSERT_EQ (out.get_size (), ConcreteValueType (1));
    ASSERT_EQ (out.get_next_offset (), ConcreteValueType (11));
  }
}

/* Implementation detail of ASSERT_BIT_RANGE_FROM_MASK_EQ.  */

static void
assert_bit_range_from_mask_eq (const ::selftest::location &loc,
			       unsigned HOST_WIDE_INT mask,
			       const concrete_bit_range &expected)
{
  concrete_bit_range actual;
  bool ok = concrete_bit_range_from_mask (mask, &actual);
  ASSERT_TRUE_AT (loc, ok);
  ASSERT_EQ_AT (loc, actual, expected);
}

/* Assert that concrete_bit_range_from_mask (MASK) returns true, and writes
   out EXPECTED_BIT_RANGE.  */

#define ASSERT_BIT_RANGE_FROM_MASK_EQ(MASK, EXPECTED_BIT_RANGE) \
  SELFTEST_BEGIN_STMT							\
  assert_bit_range_from_mask_eq (SELFTEST_LOCATION, MASK,		\
				 EXPECTED_BIT_RANGE);			\
  SELFTEST_END_STMT

/* Implementation detail of ASSERT_NO_BIT_RANGE_FROM_MASK.  */

static void
assert_no_bit_range_from_mask_eq (const ::selftest::location &loc,
				  unsigned HOST_WIDE_INT mask)
{
  concrete_bit_range actual;
  bool ok = concrete_bit_range_from_mask (mask, &actual);
  ASSERT_FALSE_AT (loc, ok);
}

/* Assert that concrete_bit_range_from_mask (MASK) returns false.  */

#define ASSERT_NO_BIT_RANGE_FROM_MASK(MASK) \
  SELFTEST_BEGIN_STMT							\
  assert_no_bit_range_from_mask_eq (SELFTEST_LOCATION, MASK);		\
  SELFTEST_END_STMT

/* Verify that concrete_bit_range_from_mask works as expected.  */

static void
test_bit_range_from_mask ()
{
  /* Should fail on zero.  */
  ASSERT_NO_BIT_RANGE_FROM_MASK (0);

  /* Verify 1-bit masks.  */
  ASSERT_BIT_RANGE_FROM_MASK_EQ (1, concrete_bit_range (0_bits, 1_bit));
  ASSERT_BIT_RANGE_FROM_MASK_EQ (2, concrete_bit_range (1_bit, 1_bit));
  ASSERT_BIT_RANGE_FROM_MASK_EQ (4, concrete_bit_range (2_bits, 1_bit));
  ASSERT_BIT_RANGE_FROM_MASK_EQ (8, concrete_bit_range (3_bits, 1_bit));
  ASSERT_BIT_RANGE_FROM_MASK_EQ (16, concrete_bit_range (4_bits, 1_bit));
  ASSERT_BIT_RANGE_FROM_MASK_EQ (32, concrete_bit_range (5_bits, 1_bit));
  ASSERT_BIT_RANGE_FROM_MASK_EQ (64, concrete_bit_range (6_bits, 1_bit));
  ASSERT_BIT_RANGE_FROM_MASK_EQ (128, concrete_bit_range (7_bits, 1_bit));

  /* Verify N-bit masks starting at bit 0.  */
  ASSERT_BIT_RANGE_FROM_MASK_EQ (3, concrete_bit_range (0_bits, 2_bits));
  ASSERT_BIT_RANGE_FROM_MASK_EQ (7, concrete_bit_range (0_bits, 3_bits));
  ASSERT_BIT_RANGE_FROM_MASK_EQ (15, concrete_bit_range (0_bits, 4_bits));
  ASSERT_BIT_RANGE_FROM_MASK_EQ (31, concrete_bit_range (0_bits, 5_bits));
  ASSERT_BIT_RANGE_FROM_MASK_EQ (63, concrete_bit_range (0_bits, 6_bits));
  ASSERT_BIT_RANGE_FROM_MASK_EQ (127, concrete_bit_range (0_bits, 7_bits));
  ASSERT_BIT_RANGE_FROM_MASK_EQ (255, concrete_bit_range (0_bits, 8_bits));
  ASSERT_BIT_RANGE_FROM_MASK_EQ (0xffff, concrete_bit_range (0_bits, 16_bits));

  /* Various other tests. */
  ASSERT_BIT_RANGE_FROM_MASK_EQ (0x30, concrete_bit_range (4_bits, 2_bits));
  ASSERT_BIT_RANGE_FROM_MASK_EQ (0x700, concrete_bit_range (8_bits, 3_bits));
  ASSERT_BIT_RANGE_FROM_MASK_EQ (0x600, concrete_bit_range (9_bits, 2_bits));

  /* Multiple ranges of set bits should fail.  */
  ASSERT_NO_BIT_RANGE_FROM_MASK (0x101);
  ASSERT_NO_BIT_RANGE_FROM_MASK (0xf0f0f0f0);
}

static void
test_get_smallest_enclosing_byte_range ()
{
  // bit 0 should be bit 0 of byte 0
  {
    concrete_byte_range out_byte_range;
    concrete_bit_offset out_bit_offset;
    get_smallest_enclosing_byte_range
      (concrete_bit_range (0_bits, 1_bit),
       &out_byte_range,
       &out_bit_offset);
    ASSERT_EQ (out_byte_range, concrete_byte_range (0_bytes, 1_byte));
    ASSERT_EQ (out_bit_offset, 0_bits);
  }

  // bits 1-3 should be bits 1-3 of byte 0
  {
    concrete_byte_range out_byte_range;
    concrete_bit_offset out_bit_offset;
    get_smallest_enclosing_byte_range
      (concrete_bit_range (1_bit, 1_bit),
       &out_byte_range,
       &out_bit_offset);
    ASSERT_EQ (out_byte_range, concrete_byte_range (0_bytes, 1_byte));
    ASSERT_EQ (out_bit_offset, 1_bit);
  }

  // bits [16, 24) should be all of byte 2
  {
    concrete_byte_range out_byte_range;
    concrete_bit_offset out_bit_offset;
    get_smallest_enclosing_byte_range
      (concrete_bit_range (16_bits, 8_bits),
       &out_byte_range,
       &out_bit_offset);
    ASSERT_EQ (out_byte_range, concrete_byte_range (2_bytes, 1_byte));
    ASSERT_EQ (out_bit_offset, 0_bits);
  }

  // bits 17-18 should be bits 1-2 of byte 2
  {
    concrete_byte_range out_byte_range;
    concrete_bit_offset out_bit_offset;
    get_smallest_enclosing_byte_range
      (concrete_bit_range (17_bits, 2_bits),
       &out_byte_range,
       &out_bit_offset);
    ASSERT_EQ (out_byte_range, concrete_byte_range (2_bytes, 1_byte));
    ASSERT_EQ (out_bit_offset, 1_bit);
  }
}

/* Run all of the selftests within this file.  */

void
analyzer_concrete_ranges_cc_tests ()
{
  concrete_bit_range bits_16_to_23 (16_bits, 8_bits);
  ASSERT_EQ (bits_16_to_23.get_start_offset (), 16_bits);
  ASSERT_EQ (bits_16_to_23.get_size (), 8_bits);
  ASSERT_EQ (bits_16_to_23.get_last_offset (), 23_bits);
  ASSERT_EQ (bits_16_to_23.get_next_offset (), 24_bits);
  bits_16_to_23.to_json ();
  ASSERT_MARKUP_TEXT_EQ (*bits_16_to_23.make_markup_node (),
			 diagnostics::selftest::get_ascii_markup_opts (),
			 "[16, 24)");

  concrete_byte_range bytes_16_to_23 (16_bytes, 8_bytes);
  ASSERT_EQ (bytes_16_to_23.get_start_offset (), 16_bytes);
  ASSERT_EQ (bytes_16_to_23.get_size (), 8_bytes);
  ASSERT_EQ (bytes_16_to_23.get_last_offset (), 23_bytes);
  ASSERT_EQ (bytes_16_to_23.get_next_offset (), 24_bytes);
  bytes_16_to_23.to_json ();

  test_intersections<integer_with_unit<units::bit>> ();
  test_intersections<integer_with_unit<units::byte>> ();
  test_falls_short_of_p<integer_with_unit<units::bit>> ();
  test_exceeds_p<integer_with_unit<units::bit>> ();

  test_bit_range_from_mask ();
  test_get_smallest_enclosing_byte_range ();
}

} // namespace selftest

#endif /* CHECKING_P */

} // namespace ana

/* Ensure we instantiate these.  */

template
void
ana::concrete_bit_range::dump_to_pp (pretty_printer *pp) const;

template
void
ana::concrete_byte_range::dump_to_pp (pretty_printer *pp) const;

template
int
ana::concrete_bit_range::cmp (const ana::concrete_bit_range &lhs,
			      const ana::concrete_bit_range &rhs);

#endif /* #if ENABLE_ANALYZER */
