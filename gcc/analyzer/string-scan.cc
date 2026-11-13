/* Support for scanning buffers for a NUL terminator (for C-style strings).
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

#include "fold-const.h"
#include "tree-diagnostic.h"
#include "tm.h"

#include "selftest.h"

#include "analyzer/symbolic-bit-vectors.h"
#include "analyzer/region-model.h"
#include "analyzer/string-scan.h"
#include "analyzer/analyzer-selftests.h"

#if ENABLE_ANALYZER

namespace ana {

void
string_scan::dump_to_pp (pretty_printer *pp) const
{
  pp_string (pp, "string_scan::");
  pp_string (pp, state_to_string (m_state));
  pp_string (pp, "(");
  m_byte_offset.dump_to_pp (pp);
  pp_string (pp, "_bytes");
  if (m_state == state::found_poison)
    pp_printf (pp, ", %s", poison_kind_to_str (m_poison_kind));
  pp_string (pp, ")");
}

DEBUG_FUNCTION void
string_scan::dump () const
{
  tree_dump_pretty_printer pp (stderr);
  dump_to_pp (&pp);
  pp_newline (&pp);
}

const char *
string_scan::state_to_string (enum state s)
{
  switch (s)
    {
    default:
      gcc_unreachable ();

    case state::found_nul:
      return "found_nul";

    case state::found_poison:
      return "found_poison";

    case state::beyond_domain:
      return "beyond_domain";

    case state::cannot_scan:
      return "cannot_scan";
    }
}

namespace symbolic {

static string_scan
scan_string_cst_for_null_terminator (tree string_cst,
				     concrete_byte_offset start_byte_offset)
{
  gcc_assert (TREE_CODE (string_cst) == STRING_CST);

  tree cst_size = get_string_cst_size (string_cst);
  if (!cst_size)
    return string_scan::cannot_scan (start_byte_offset);
  if (TREE_CODE (cst_size) != INTEGER_CST)
    return string_scan::cannot_scan (start_byte_offset);

  auto string_len
    = integer_with_unit<units::byte> (TREE_STRING_LENGTH (string_cst));
  /* Assume all bytes after TREE_STRING_LENGTH are zero.  This handles
     the case where an array is initialized with a string_cst that isn't
     as long as the array, where the remaining elements are
     empty-initialized and thus zeroed.  */
  if (start_byte_offset >= string_len)
    return string_scan::found_nul (start_byte_offset);

  /* Look for the first 0 byte within STRING_CST
     from START_READ_OFFSET onwards.  */
  const char *start = (TREE_STRING_POINTER (string_cst)
		       + start_byte_offset.as_unsigned_long ());
  size_t remaining_bytes
    = TREE_STRING_LENGTH (string_cst) - start_byte_offset.as_unsigned_long ();
  if (const void *p = memchr (start, 0, remaining_bytes))
    {
      /* Found NUL.  */
      return string_scan::found_nul
	(concrete_byte_offset ((const char *)p
			       - TREE_STRING_POINTER (string_cst)));
    }
  else
    {
      /* Didn't find NUL.  */
      return string_scan::beyond_domain (string_len);
    }
}

static string_scan
scan_raw_data_cst_for_null_terminator (tree raw_data_cst,
				       concrete_byte_offset start_byte_offset)
{
  gcc_assert (TREE_CODE (raw_data_cst) == RAW_DATA_CST);
  gcc_assert (start_byte_offset >= 0_bytes);
  gcc_assert (start_byte_offset
	      < concrete_byte_offset (RAW_DATA_LENGTH (raw_data_cst)));

  const unsigned char *start
    = &RAW_DATA_UCHAR_ELT (raw_data_cst, start_byte_offset.as_unsigned_long ());
  size_t remaining_bytes
    = RAW_DATA_LENGTH (raw_data_cst) - start_byte_offset.as_unsigned_long ();
  if (const void *p = memchr (start, 0, remaining_bytes))
    {
      /* Found NUL.  */
      return string_scan::found_nul
	(concrete_byte_offset ((const unsigned char *)p
			       - &RAW_DATA_UCHAR_ELT (raw_data_cst, 0)));
    }
  else
    {
      /* Didn't find NUL.  */
      return string_scan::beyond_domain
	(concrete_byte_offset (RAW_DATA_LENGTH (raw_data_cst)));
    }
}

static string_scan
scan_tree_cst_for_null_terminator (tree tree_cst,
				   concrete_bit_size size_in_bits,
				   concrete_byte_offset start_byte_offset)
{
  gcc_assert (start_byte_offset >= 0_bytes);

  switch (TREE_CODE (tree_cst))
    {
    default:
      gcc_unreachable ();

    case COMPLEX_CST:
    case VECTOR_CST:
    case INTEGER_CST:
      if (size_in_bits == concrete_bit_size (BITS_PER_UNIT))
	{
	  /* We have a one-byte constant.
	     It's either a zero byte or a non-zero byte.  */
	  gcc_assert (start_byte_offset == 0_bytes);
	  if (zerop (tree_cst))
	    return string_scan::found_nul (0_bytes);
	  else
	    return string_scan::beyond_domain (1_byte);
	}
      else
	{
	  // sizeof > 1:
	  if (zerop (tree_cst))
	    return string_scan::found_nul (0_bytes);
	  else
	    {
	      /* Use native_encode_expr and extract the pertinent byte(s).  */
	      unsigned char buf[MAX_BITSIZE_MODE_ANY_MODE / BITS_PER_UNIT];
	      int size = native_encode_expr (tree_cst, buf, sizeof buf, 0);
	      if (size == 0)
		string_scan::cannot_scan (start_byte_offset);
	      if (start_byte_offset < 0_bytes)
		return string_scan::cannot_scan (start_byte_offset);
	      if (start_byte_offset >= concrete_byte_size (size))
		return string_scan::beyond_domain (start_byte_offset);
	      for (int offset = start_byte_offset.as_unsigned_long ();
		   offset < size; ++offset)
		if (buf[offset] == '\0')
		  return string_scan::found_nul (concrete_byte_offset (offset));
	      return string_scan::beyond_domain (concrete_byte_offset (size));
	    }
	}
      break;

    case STRING_CST:
      return scan_string_cst_for_null_terminator (tree_cst,
						  start_byte_offset);

    case RAW_DATA_CST:
      return scan_raw_data_cst_for_null_terminator (tree_cst,
						    start_byte_offset);
    }

  return string_scan::cannot_scan (start_byte_offset);
}

string_scan
concat_bit_vector::
scan_for_first_zero_byte (concrete_byte_offset byte_offset) const
{
  concrete_byte_offset byte_offset_relative_to_iter = byte_offset;
  concrete_byte_offset accum_byte_sizes = 0_bytes;
  for (auto iter : m_bit_vectors)
    {
      concrete_byte_size iter_byte_size;
      if (!iter->maybe_get_concrete_size_in_bytes (&iter_byte_size))
	return string_scan::cannot_scan (byte_offset);
      if (byte_offset_relative_to_iter < iter_byte_size)
	{
	  auto nested_scan
	    = iter->scan_for_first_zero_byte (byte_offset_relative_to_iter);
	  if (nested_scan.m_state != string_scan::state::beyond_domain)
	    return string_scan (accum_byte_sizes + nested_scan.m_byte_offset,
				nested_scan.m_state);
	}
      else
	{
	  /* "iter" is a leading element fully before the initial byte offset:
	     skip it altogether.  */
	}
      accum_byte_sizes += iter_byte_size;
      byte_offset_relative_to_iter -= iter_byte_size;
      if (byte_offset_relative_to_iter < 0_bytes)
	byte_offset_relative_to_iter = 0_bytes;
    }
  return string_scan::beyond_domain (accum_byte_sizes);
}

string_scan
conjured_bit_vector::
scan_for_first_zero_byte (concrete_byte_offset byte_offset) const
{
  return string_scan::cannot_scan (byte_offset);
}

string_scan
cropped_bit_vector::
scan_for_first_zero_byte (concrete_byte_offset start_byte_offset) const
{
  concrete_bit_range uncropped_bits_to_use;
  if (!concrete_from_symbolic (get_uncropped_bits_to_use (),
			       &uncropped_bits_to_use))
    return string_scan::cannot_scan (start_byte_offset);
  concrete_byte_range uncropped_bytes_to_use;
  if (!maybe_bytes_from_bits (uncropped_bits_to_use,
			      &uncropped_bytes_to_use))
    return string_scan::cannot_scan (start_byte_offset);

  /* Scan the uncropped bit_vector, starting at START_BYTE_OFFSET
     adjusted relative to the beginning of the cropping.  */
  string_scan nested_scan
    = m_uncropped_bv.scan_for_first_zero_byte
    (uncropped_bytes_to_use.get_start_offset () + start_byte_offset);

  /* If the first thing that happened in the nested scan was beyond the
     cropped region, then simply advance the outer scan to the end of
     the cropped region.  */
  if (nested_scan.m_byte_offset >= uncropped_bytes_to_use.get_next_offset ())
    return string_scan::beyond_domain
      (std::max (start_byte_offset,
		 uncropped_bytes_to_use.get_size ()));

  /* Otherwise, report the location of interest relative to the start
     of this cropped bit_vector.  */
  return string_scan (nested_scan.m_byte_offset
		      - uncropped_bytes_to_use.get_start_offset (),
		      nested_scan.m_state);
}

string_scan
named_test_bit_vector::
scan_for_first_zero_byte (concrete_byte_offset start_byte_offset) const
{
  return string_scan::cannot_scan (start_byte_offset);
}

string_scan
empty_bit_vector::
scan_for_first_zero_byte (concrete_byte_offset byte_offset) const
{
  return string_scan::beyond_domain (byte_offset);
}

string_scan
poisoned_bit_vector::
scan_for_first_zero_byte (concrete_byte_offset start_byte_offset) const
{
  return string_scan::cannot_scan (start_byte_offset);
}

/* Scan for null terminator within repeated bit_vector.
    . +-----+-----+-----+-----+-----+-----+
    . |     |     |     |     |     |     |
    . +-----+-----+-----+-----+-----+-----+
    .                ^
    .                start_byte_offset
   Do a partial scan within the first repetition we're at;
   failing that, try the next (if any).  */

string_scan
repeated_bit_vector::
scan_for_first_zero_byte (concrete_byte_offset start_byte_offset) const
{
  concrete_byte_size inner_byte_size;
  if (!m_inner_bv.maybe_get_concrete_size_in_bytes (&inner_byte_size))
    return string_scan::cannot_scan (start_byte_offset);

  concrete_integer conc_repeat_count;
  if (!concrete_from_symbolic (m_repeat_count, &conc_repeat_count))
    return string_scan::cannot_scan (start_byte_offset);

  /* If we're starting within a repetition, perform a partial scan of the
     repetition of the inner bit_vector that we hit at START_BYTE_OFFSET, e.g.:
    . +0----+1----+2----+3----+4----+5----+
    . |     |     |     |     |     |     |
    . +-----+-----+-----+-----+-----+-----+
    .                ^
    .                start_byte_offset
    .                V
    .             +-----+
    .             |     |
    .             +-----+
  */
  concrete_byte_offset byte_offset_within_initial
    = (start_byte_offset.floor_mod (inner_byte_size));
  if (byte_offset_within_initial > 0_bytes)
    {
      auto partial_instance_index
	= start_byte_offset.floor_div (inner_byte_size);
      const string_scan partial_nested_scan
	= m_inner_bv.scan_for_first_zero_byte (byte_offset_within_initial);
      if (partial_nested_scan.m_state != string_scan::state::beyond_domain)
	{
	  concrete_byte_offset start_byte_offset_of_partial_instance
	    = partial_instance_index * inner_byte_size;
	  return string_scan ((start_byte_offset_of_partial_instance
			       + partial_nested_scan.m_byte_offset),
			      partial_nested_scan.m_state);
	}
    }

  /* Otherwise, we haven't found anything yet; look from the start of
     the next full repetition of the inner bit_vector, if there is one.
    . +0----+1----+2----+3----+4----+5----+
    . |     |     |     |     |     |     |
    . +-----+-----+-----+-----+-----+-----+
    .                   ^
    .                   V
    .                   +-----+
    .                   |     |
    .                   +-----+

  */
  auto full_instance_index = start_byte_offset.floor_div (inner_byte_size);
  if (full_instance_index < conc_repeat_count)
    {
      const string_scan full_nested_scan
	= m_inner_bv.scan_for_first_zero_byte (0_bytes);
      if (full_nested_scan.m_state != string_scan::state::beyond_domain)
	{
	  concrete_byte_offset start_byte_offset_of_full_instance
	    = (full_instance_index * inner_byte_size);
	  return string_scan ((start_byte_offset_of_full_instance
			       + full_nested_scan.m_byte_offset),
			      full_nested_scan.m_state);
	}
    }

  /* Otherwise, we've run out of repetitions.  */
  return string_scan::beyond_domain (inner_byte_size * conc_repeat_count);
}

string_scan
bit_vector_from_svalue::
scan_for_first_zero_byte (concrete_byte_offset start_byte_offset) const
{
  switch (m_sval.get_kind ())
    {
    default:
      break;

    case SK_CONSTANT:
      {
	concrete_bit_size conc_size;
	if (!concrete_from_symbolic (get_size (), &conc_size))
	  gcc_unreachable ();
	tree cst = as_a<const constant_svalue *> (&m_sval)->get_constant ();
	return scan_tree_cst_for_null_terminator (cst,
						  conc_size,
						  start_byte_offset);
      }

    case SK_INITIAL:
      {
	auto *initial_sval = as_a<const initial_svalue *> (&m_sval);
	auto &reg = *initial_sval->get_region ();
	switch (reg.get_kind ())
	  {
	  default:
	    break;

	  case RK_STRING:
	    {
	      auto *string_reg = as_a<const string_region *> (&reg);
	      return scan_string_cst_for_null_terminator
		(string_reg->get_string_cst (),
		 start_byte_offset);
	    }
	  }
      }
      break;
    }

  return string_scan::cannot_scan (start_byte_offset);
}

string_scan
bit_vector_from_ubc::
scan_for_first_zero_byte (concrete_byte_offset start_byte_offset) const
{
  return string_scan::cannot_scan (start_byte_offset);
}

string_scan
tristate_bit_vector::
scan_for_first_zero_byte (concrete_byte_offset start_byte_offset) const
{
  concrete_byte_size size_in_bytes;
  if (!maybe_get_concrete_size_in_bytes (&size_in_bytes))
    return string_scan::cannot_scan (start_byte_offset);

  gcc_assert (size_in_bytes >= start_byte_offset);

  switch (m_bit_value.get_value ())
    {
    default:
      gcc_unreachable ();

    case tristate::TS_UNKNOWN:
      return string_scan::cannot_scan (start_byte_offset);

    case tristate::TS_TRUE:
      // All ones: NUL not found.
      return string_scan::beyond_domain (size_in_bytes);

    case tristate::TS_FALSE:
      // All zeros: NUL immediately found.
      return string_scan::found_nul (start_byte_offset);
    }
}

} // namespace ana::symbolic

#if CHECKING_P

namespace selftest {

static void
test_string_scan_dump ()
{
  {
    pretty_printer pp;
    string_scan::found_nul (5_bytes).dump_to_pp (&pp);
    ASSERT_STREQ (pp_formatted_text (&pp),
		  "string_scan::found_nul(5_bytes)");
  }
  {
    pretty_printer pp;
    string_scan::found_poison (100_bytes, poison_kind::uninit).dump_to_pp (&pp);
    ASSERT_STREQ (pp_formatted_text (&pp),
		  "string_scan::found_poison(100_bytes, uninit)");
  }
  {
    pretty_printer pp;
    string_scan::beyond_domain (1024_bytes).dump_to_pp (&pp);
    ASSERT_STREQ (pp_formatted_text (&pp),
		  "string_scan::beyond_domain(1024_bytes)");
  }
  {
    pretty_printer pp;
    string_scan::cannot_scan (42_bytes).dump_to_pp (&pp);
    ASSERT_STREQ (pp_formatted_text (&pp),
		  "string_scan::cannot_scan(42_bytes)");
  }
}

// Test scanning a string constant for NUL.

static void
test_scan_string_constant ()
{
  tree string_cst_ptr = make_string_constant ("abc");

  symbolic::node_manager node_mgr;
  auto &mgr = node_mgr.get_bit_vector_manager ();

  auto &abc0 = mgr.get_bits_for_constant (string_cst_ptr);

  for (concrete_byte_offset i = 0_bytes; i < 4_bytes; ++i)
    ASSERT_EQ (abc0.scan_for_first_zero_byte (i),
	       string_scan::found_nul (3_bytes));
}

static void
test_scan_integer_constants ()
{
  symbolic::node_manager node_mgr;
  auto &mgr = node_mgr.get_bit_vector_manager ();

  /* sizeof() > 1, all zero: NUL at byte 0.  */
  {
    auto &int_0
      = mgr.get_bits_for_constant (build_int_cst (integer_type_node, 0));
    ASSERT_EQ (int_0.scan_for_first_zero_byte (0_bytes),
	       string_scan::found_nul (0_bytes));
  }

  /* sizeof() > 1, not all zero.  */
  {
    auto &int_42
      = mgr.get_bits_for_constant (build_int_cst (uint32_type_node, 42));
    if (BYTES_BIG_ENDIAN)
      {
	/* Big-endian.  */
	/* First 3 bytes are all zero.  */
	for (auto i = 0_bytes; i < 3_bytes; ++i)
	  ASSERT_EQ (int_42.scan_for_first_zero_byte (i),
		     string_scan::found_nul (i));
	/* Then non-zero.  */
	ASSERT_EQ (int_42.scan_for_first_zero_byte (3_bytes),
		   string_scan::beyond_domain (4_bytes));
      }
    else
      {
	/* Little-endian.  */
	if (!WORDS_BIG_ENDIAN)
	  {
	    /* First byte is non-zero (42).  */
	    ASSERT_EQ (int_42.scan_for_first_zero_byte (0_bytes),
		       string_scan::found_nul (1_byte));
	    /* Then 3 zero bytes.  */
	    for (auto i = 1_byte; i < 4_bytes; ++i)
	      ASSERT_EQ (int_42.scan_for_first_zero_byte (i),
			 string_scan::found_nul (i));
	  }
      }
  }

  /* sizeof() == 1.  */
  auto &char_00
    = mgr.get_bits_for_constant (build_int_cst (char_type_node, 0));
  ASSERT_EQ (char_00.scan_for_first_zero_byte (0_bytes),
	     string_scan::found_nul (0_bytes));

  auto &char_a
    = mgr.get_bits_for_constant (build_int_cst (char_type_node, 'a'));
  ASSERT_EQ (char_a.scan_for_first_zero_byte (0_bytes),
	     string_scan::beyond_domain (1_byte));

  auto &char_b
    = mgr.get_bits_for_constant (build_int_cst (char_type_node, 'b'));
  ASSERT_EQ (char_b.scan_for_first_zero_byte (0_bytes),
	     string_scan::beyond_domain (1_byte));
}

static void
test_scan_tristates ()
{
  symbolic::node_manager node_mgr;
  auto &mgr = node_mgr.get_bit_vector_manager ();

  {
    auto &byte_00 = mgr.get_all_zeros (1_byte);
    ASSERT_EQ (byte_00.scan_for_first_zero_byte (0_bytes),
	       string_scan::found_nul (0_bytes));
  }
  {
    auto &byte_ff = mgr.get_all_ones (1_byte);
    ASSERT_EQ (byte_ff.scan_for_first_zero_byte (0_bytes),
	       string_scan::beyond_domain (1_byte));
  }
  {
    auto &byte_unknown = mgr.get_tristate_bits (8_bits, tristate::unknown ());
    ASSERT_EQ (byte_unknown.scan_for_first_zero_byte (0_bytes),
	       string_scan::cannot_scan (0_bytes));
  }
}

/* Return true iff default_tree_printer prints "char" for
   %T with char_type_node.

   2026-11-05(dmalcolm): crab1 seems to print char_type_node as
   "unsigned char", whereas cc1 and cc1plus print it as "char".  */

static bool
char_type_node_printed_as_char_p ()
{
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  pp_printf (&pp, "%T", char_type_node);
  return strcmp (pp_formatted_text (&pp), "char") == 0;
}

static void
test_scan_concat ()
{
  symbolic::node_manager node_mgr;
  auto &mgr = node_mgr.get_bit_vector_manager ();

  auto &abc0 = mgr.get_bits_for_constant (make_string_constant ("abc"));
  auto &def0 = mgr.get_bits_for_constant (make_string_constant ("def"));

  {
    auto &abc0def0 = mgr.get_concat_bits ({&abc0, &def0});

    if (char_type_node_printed_as_char_p ())
      ASSERT_SYMBOLIC_NODE_DUMP_EQ
	(abc0def0,
	 ("concat_bit_vector[size: 8 bytes]\n"
	  "+- bit_vector_from_svalue[size: 4 bytes]\n"
	  "|  `- m_sval: 'const char[4]': constant_svalue ('\"abc\"')\n"
	  "`- bit_vector_from_svalue[size: 4 bytes]\n"
	  "   `- m_sval: 'const char[4]': constant_svalue ('\"def\"')\n"));

    for (concrete_byte_offset i = 0_bytes; i < 4_bytes; ++i)
      ASSERT_EQ (abc0def0.scan_for_first_zero_byte (i),
		 string_scan::found_nul (3_bytes));
    for (concrete_byte_offset i = 4_bytes; i < 8_bytes; ++i)
      ASSERT_EQ (abc0def0.scan_for_first_zero_byte (i),
		 string_scan::found_nul (7_bytes));
  }

  // Concat, cropping the terminator from "abc"
  {
    auto &abcdef0
      = mgr.get_concat_bits ({&abc0.get_cropped_bytes (concrete_byte_range (0_bytes, 3_bytes)),
			      &def0});

    if (char_type_node_printed_as_char_p ())
      ASSERT_SYMBOLIC_NODE_DUMP_EQ
	(abcdef0,
	 ("concat_bit_vector[size: 7 bytes]\n"
	  "+- cropped_bit_vector[size: 3 bytes]: [bytes 0..3)\n"
	  "|  `- uncropped: bit_vector_from_svalue[size: 4 bytes]\n"
	  "|     `- m_sval: 'const char[4]': constant_svalue ('\"abc\"')\n"
	  "`- bit_vector_from_svalue[size: 4 bytes]\n"
	  "   `- m_sval: 'const char[4]': constant_svalue ('\"def\"')\n"));

    for (concrete_byte_offset i = 0_bytes; i < 7_bytes; ++i)
      ASSERT_EQ (abcdef0.scan_for_first_zero_byte (i),
		 string_scan::found_nul (6_bytes));
  }
}

static void
test_scan_cropped ()
{
  symbolic::node_manager node_mgr;
  auto &mgr = node_mgr.get_bit_vector_manager ();

  auto &abc0 = mgr.get_bits_for_constant (make_string_constant ("abc"));

  // Crop the terminator from "abc"
  {
    auto &abc = abc0.get_cropped_bytes( concrete_byte_range (0_bytes, 3_bytes));
    for (concrete_byte_offset i = 0_bytes; i < 3_bytes; ++i)
      ASSERT_EQ (abc.scan_for_first_zero_byte (i),
		 string_scan::beyond_domain (3_bytes));
  }
}

static void
test_scan_repeated ()
{
  symbolic::node_manager node_mgr;
  auto &mgr = node_mgr.get_bit_vector_manager ();

  auto &char_a
    = mgr.get_bits_for_constant (build_int_cst (char_type_node, 'a'));
  auto &char_b
    = mgr.get_bits_for_constant (build_int_cst (char_type_node, 'b'));

  {
    auto &aaa = mgr.get_repeated_bits (char_a, 3);
    for (concrete_byte_offset i = 0_bytes; i < 3_bytes; ++i)
      ASSERT_EQ (aaa.scan_for_first_zero_byte (i),
		 string_scan::beyond_domain (3_bytes));
  }

  {
    auto &ab = mgr.get_concat_bits ({&char_a, &char_b});
    auto &ababab = mgr.get_repeated_bits (ab, 3);
    for (concrete_byte_offset i = 0_bytes; i < 6_bytes; ++i)
      ASSERT_EQ (ababab.scan_for_first_zero_byte (i),
		 string_scan::beyond_domain (6_bytes));
  }

  {
    tree string_cst_ptr = make_string_constant ("abc");
    auto &abc0 = mgr.get_bits_for_constant (string_cst_ptr);
    auto &abc0abc0abc0 = mgr.get_repeated_bits (abc0, 3);
    for (concrete_byte_offset i = 0_bytes; i < 4_bytes; ++i)
      ASSERT_EQ (abc0abc0abc0.scan_for_first_zero_byte (i),
		 string_scan::found_nul (3_bytes));
    for (concrete_byte_offset i = 4_bytes; i < 7_bytes; ++i)
      ASSERT_EQ (abc0abc0abc0.scan_for_first_zero_byte (i),
		 string_scan::found_nul (7_bytes));
    for (concrete_byte_offset i = 8_bytes; i < 12_bytes; ++i)
      ASSERT_EQ (abc0abc0abc0.scan_for_first_zero_byte (i),
		 string_scan::found_nul (11_bytes));
  }
}

/* Run all of the selftests within this file.  */

void
analyzer_string_scan_cc_tests ()
{
  test_string_scan_dump ();
  test_scan_string_constant ();
  test_scan_integer_constants ();
  test_scan_tristates ();
  test_scan_concat ();
  test_scan_cropped ();
  test_scan_repeated ();
}

} // namespace selftest

#endif /* CHECKING_P */

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
