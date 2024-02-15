// Copyright (C) 2020-2024 Free Software Foundation, Inc.

// This file is part of GCC.

// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.

// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.

// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include "rust-input-source.h"
#include "rust-system.h"
#include "optional.h"
#include "selftest.h"
#include "rust-lex.h"
#include "rust-unicode.h"

#include "rust-unicode-data.h"

namespace Rust {

typedef Codepoint codepoint_t;
typedef std::vector<codepoint_t> string_t;

// These constants are used to compose and decompose of Hangul syllables.
// See `Sample Code for Hangul Algorithms` in 3.1.2
// unicode.org/versions/Unicode15.0.0/ch03.pdf
const uint32_t S_BASE = 0xAC00;
const uint32_t L_BASE = 0x1100, V_BASE = 0x1161, T_BASE = 0x11A7;
const uint32_t L_COUNT = 19, V_COUNT = 21, T_COUNT = 28;
const uint32_t N_COUNT = V_COUNT * T_COUNT;
const uint32_t S_COUNT = L_COUNT * N_COUNT;

// Check if the codepoint is in any of the ranges (half-open intervals [a,b)).
template <std::size_t SIZE>
bool
binary_search_ranges (
  const std::array<std::pair<uint32_t, uint32_t>, SIZE> &ranges,
  uint32_t target_cp)
{
  auto it = std::lower_bound (ranges.begin (), ranges.end (), target_cp,
			      [] (const std::pair<uint32_t, uint32_t> &a,
				  uint32_t b) { return a.second <= b; });
  if (it == ranges.end ())
    return false;
  else
    return it->first <= target_cp && target_cp < it->second;
}

int
lookup_cc (codepoint_t c)
{
  auto it = CCC_TABLE.find (c.value);
  if (it != CCC_TABLE.end ())
    return it->second;
  else
    // Starter. Returns zero.
    return 0;
}

tl::optional<codepoint_t>
lookup_recomp (codepoint_t starter, codepoint_t c)
{
  auto it = Rust::RECOMPOSITION_MAP.find ({starter.value, c.value});
  if (it != Rust::RECOMPOSITION_MAP.end ())
    return {it->second};

  it = Rust::RECOMPOSITION_MAP.find ({starter.value, 0});
  if (it != Rust::RECOMPOSITION_MAP.end ())
    return {it->second};

  return tl::nullopt;
}

void
recursive_decomp_cano (codepoint_t c, string_t &buf)
{
  auto it = Rust::DECOMPOSITION_MAP.find (c.value);
  if (it != Rust::DECOMPOSITION_MAP.end ())
    {
      std::vector<uint32_t> decomped = it->second;
      for (uint32_t cp : decomped)
	recursive_decomp_cano (cp, buf);
    }
  else
    buf.push_back (c);
}

string_t
decomp_cano (string_t s)
{
  string_t buf;
  for (codepoint_t c : s)
    {
      int64_t s_index = c.value - S_BASE;
      if (0 <= s_index && s_index < S_COUNT)
	{
	  // decompose Hangul argorithmically
	  uint32_t l = L_BASE + s_index / N_COUNT;
	  uint32_t v = V_BASE + (s_index % N_COUNT) / T_COUNT;
	  uint32_t t = T_BASE + s_index % T_COUNT;
	  buf.push_back (l);
	  buf.push_back (v);
	  if (t != T_BASE)
	    buf.push_back (t);
	  continue;
	}

      // Current character is not hangul
      recursive_decomp_cano (c, buf);
    }
  return buf;
}

void
sort_cano (string_t &s)
{
  int cc_here, cc_prev;
  if (s.size () == 1)
    return;
  for (unsigned int i = 1; i < s.size (); i++)
    {
      cc_here = lookup_cc (s[i]);
      cc_prev = lookup_cc (s[i - 1]);
      if (cc_here > 0 && cc_prev > 0 && cc_prev > cc_here)
	{
	  // swap
	  codepoint_t tmp = s[i];
	  s[i] = s[i - 1];
	  s[i - 1] = tmp;
	  if (i > 1)
	    i -= 2;
	}
    }
}

string_t
compose_hangul (string_t s)
{
  string_t buf;
  if (s.size () < 2)
    return s;

  codepoint_t last = s[0];
  buf.push_back (last);
  for (unsigned int src_pos = 1; src_pos < s.size (); src_pos++)
    {
      codepoint_t ch = s[src_pos];

      // L V => LV
      int64_t l_index = last.value - L_BASE;
      if (0 <= l_index && l_index < L_COUNT)
	{
	  int64_t v_index = ch.value - V_BASE;
	  if (0 <= v_index && v_index < V_COUNT)
	    {
	      last = S_BASE + (l_index * V_COUNT + v_index) * T_COUNT;
	      // pop L
	      buf.pop_back ();
	      buf.push_back (last);
	      continue;
	    }
	}

      // LV T => LVT
      int64_t s_index = last.value - S_BASE;
      if (0 <= s_index && s_index < S_COUNT && (s_index % T_COUNT) == 0)
	{
	  int64_t t_index = ch.value - T_BASE;
	  if (0 < t_index && t_index < T_COUNT)
	    {
	      last.value += t_index;
	      // pop LV
	      buf.pop_back ();
	      buf.push_back (last);
	      continue;
	    }
	}
      last = ch;
      buf.push_back (last);
    }
  return buf;
}

string_t
recomp (string_t s)
{
  // compose hangul first
  s = compose_hangul (s);

  string_t buf;
  if (s.size () < 2)
    return s;

  int last_class = -1;
  // int starter_pos = 0; // Assume the first character is Starter. Correct?
  // int target_pos = 1;
  codepoint_t starter_ch = s[0];

  for (unsigned int src_pos = 1; src_pos < s.size (); src_pos++)
    {
      // get current character
      codepoint_t ch = s[src_pos];

      int ch_class = lookup_cc (ch);
      tl::optional<codepoint_t> composite = lookup_recomp (starter_ch, ch);
      if (composite.has_value () && last_class < ch_class)
	{
	  // ch can be composed
	  buf.push_back (composite.value ());
	  starter_ch = composite.value ();
	}
      else if (ch_class == 0)
	{
	  // ch is Starter and cannot be composed.
	  if (src_pos == 1)
	    // FIXME: buggy?
	    buf.push_back (starter_ch);
	  starter_ch = ch;
	  last_class = -1;
	  buf.push_back (ch);
	}
      else
	{
	  if (src_pos == 1)
	    // FIXME: buggy?
	    buf.push_back (starter_ch);
	  // ch is not Starter.
	  last_class = ch_class;
	  buf.push_back (ch);
	}
    }
  return buf;
}

// see https://unicode.org/reports/tr15/#Detecting_Normalization_Forms
QuickCheckResult
nfc_quick_check (const string_t &s)
{
  int last_canonical_class = 0;
  QuickCheckResult res = QuickCheckResult::YES;

  for (unsigned long i = 0; i < s.size (); i++)
    {
      codepoint_t c = s[i];

      if (c.is_supplementary_character ())
	i++;

      int canonical_class = lookup_cc (c);
      if (last_canonical_class > canonical_class && canonical_class != 0)
	return QuickCheckResult::NO;

      if (is_nfc_qc_no (c.value))
	return QuickCheckResult::NO;

      if (is_nfc_qc_maybe (c.value))
	res = QuickCheckResult::MAYBE;

      last_canonical_class = canonical_class;
    }
  return res;
}

string_t
nfc_normalize (const string_t &s)
{
  if (nfc_quick_check (s) == QuickCheckResult::YES)
    return s;

  // TODO: optimize normalization.
  // i.e. only normalize a limited area around MAYBE character, instead of
  // performing complete normlization of the entire string

  // decompose
  string_t d = decomp_cano (s);
  sort_cano (d);

  // recompose
  string_t r = recomp (d);
  return r;
}

Utf8String
Utf8String::nfc_normalize () const
{
  return Utf8String (Rust::nfc_normalize (chars));
}

bool
is_alphabetic (uint32_t codepoint)
{
  return binary_search_ranges (ALPHABETIC_RANGES, codepoint);
}

bool
is_numeric (uint32_t codepoint)
{
  return std::binary_search (NUMERIC_CODEPOINTS.begin (),
			     NUMERIC_CODEPOINTS.end (), codepoint);
}

bool
is_nfc_qc_maybe (uint32_t codepoint)
{
  return binary_search_ranges (NFC_QC_MAYBE_RANGES, codepoint);
}

bool
is_nfc_qc_no (uint32_t codepoint)
{
  return binary_search_ranges (NFC_QC_NO_RANGES, codepoint);
}

bool
is_ascii_only (const std::string &str)
{
  for (char c : str)
    if (static_cast<uint32_t> (c) > MAX_ASCII_CODEPOINT)
      return false;
  return true;
}

} // namespace Rust

#if CHECKING_P

namespace selftest {

void
rust_nfc_qc_test ()
{
  ASSERT_EQ (Rust::nfc_quick_check ({0x1e0a /* NFC_QC_YES */}),
	     Rust::QuickCheckResult::YES);
  ASSERT_EQ (Rust::nfc_quick_check (
	       {0x1e0a /* NFC_QC_YES */, 0x0323 /* NFC_QC_MAYBE */}),
	     Rust::QuickCheckResult::MAYBE);
  ASSERT_EQ (Rust::nfc_quick_check ({0x0340 /* NFC_QC_NO */}),
	     Rust::QuickCheckResult::NO);
}

void
assert_normalize (const std::vector<Rust::Codepoint> origin,
		  const std::vector<Rust::Codepoint> expected)
{
  std::vector<Rust::Codepoint> actual = Rust::nfc_normalize (origin);

  ASSERT_EQ (actual.size (), expected.size ());
  for (unsigned int i = 0; i < actual.size (); i++)
    {
      ASSERT_EQ (actual[i], expected[i]);
    }
}

void
rust_utf8_normalize_test ()
{
  // ASCII
  assert_normalize ({'h', 'e', 'l', 'l', 'o'}, {'h', 'e', 'l', 'l', 'o'});
  // ASCII
  assert_normalize ({'/', '\\', '.', ':', '*'}, {'/', '\\', '.', ':', '*'});

  // testcases retrieved from Part0 of
  // https://unicode.org/Public/UNIDATA/NormalizationTest.txt
  assert_normalize ({0x1e0a}, {0x1e0a});
  assert_normalize ({0x1e0c}, {0x1e0c});
  assert_normalize ({0x1e0a, 0x0323}, {0x1e0c, 0x0307});
  assert_normalize ({0x1e0c, 0x0307}, {0x1e0c, 0x0307});
  assert_normalize ({0x0044, 0x0307, 0x0323}, {0x1e0c, 0x0307});

  // testcases for Hangul from Part0
  assert_normalize ({0x1100, 0xac00, 0x11a8}, {0x1100, 0xac01});
  assert_normalize ({0x1100, 0xac00, 0x11a8, 0x11a8}, {0x1100, 0xac01, 0x11a8});
  // testcases for Hangul from Part1
  assert_normalize ({0x3131}, {0x3131});
  assert_normalize ({0x3132}, {0x3132});
  // testcases for Hangul from Part3
  assert_normalize ({0x1100, 0x0334, 0x1161}, {0x1100, 0x0334, 0x1161});
  assert_normalize ({0xac54, 0x0334, 0x11ae}, {0xac54, 0x0334, 0x11ae});

  // TODO: add more testcases in
  // https://unicode.org/Public/UNIDATA/NormalizationTest.txt
}

void
rust_utf8_property_test ()
{
  ASSERT_TRUE (Rust::is_alphabetic ('A'));
  ASSERT_TRUE (Rust::is_alphabetic ('B'));
  ASSERT_TRUE (Rust::is_alphabetic ('x'));
  ASSERT_TRUE (Rust::is_alphabetic ('z'));
  ASSERT_TRUE (Rust::is_alphabetic (0x00b5));  // µ
  ASSERT_TRUE (Rust::is_alphabetic (0x3093));  // ん
  ASSERT_TRUE (Rust::is_alphabetic (0xa8f2));  // ꣲ
  ASSERT_TRUE (Rust::is_alphabetic (0x2b743)); // 𫝃

  ASSERT_FALSE (Rust::is_alphabetic ('\v'));
  ASSERT_FALSE (Rust::is_alphabetic ('-'));
  ASSERT_FALSE (Rust::is_alphabetic ('_'));
  ASSERT_FALSE (Rust::is_alphabetic ('+'));
  ASSERT_FALSE (Rust::is_alphabetic ('0'));
  ASSERT_FALSE (Rust::is_alphabetic ('1'));
  ASSERT_FALSE (Rust::is_alphabetic ('2'));
  ASSERT_FALSE (Rust::is_alphabetic ('9'));
  ASSERT_FALSE (Rust::is_alphabetic (0xa720)); // ◌
  ASSERT_FALSE (Rust::is_alphabetic (0xaac1)); // ◌꫁

  // `Nd`s
  ASSERT_TRUE (Rust::is_numeric ('0'));
  ASSERT_TRUE (Rust::is_numeric ('1'));
  ASSERT_TRUE (Rust::is_numeric ('7'));
  ASSERT_TRUE (Rust::is_numeric ('9'));
  ASSERT_TRUE (Rust::is_numeric (0x07c2)); // ߂
  ASSERT_TRUE (Rust::is_numeric (0x096d)); // ७
  // `Nl`s
  ASSERT_TRUE (Rust::is_numeric (0x16e6));  // ᛮ
  ASSERT_TRUE (Rust::is_numeric (0xa6e6));  // ꛦ
  ASSERT_TRUE (Rust::is_numeric (0x12400)); // 𒐀
  ASSERT_TRUE (Rust::is_numeric (0x1243a)); // 𒐺
  // `No`s
  ASSERT_TRUE (Rust::is_numeric (0x00b2)); // ²
  ASSERT_TRUE (Rust::is_numeric (0x32b1)); // ㊱

  ASSERT_FALSE (Rust::is_numeric ('\n'));
  ASSERT_FALSE (Rust::is_numeric ('-'));
  ASSERT_FALSE (Rust::is_numeric ('_'));
  ASSERT_FALSE (Rust::is_numeric ('('));
  ASSERT_FALSE (Rust::is_numeric ('z'));
  ASSERT_FALSE (Rust::is_numeric (';'));
  ASSERT_FALSE (Rust::is_numeric (0x03f4)); // ϴ
  ASSERT_FALSE (Rust::is_numeric (0x0628)); // ب
  ASSERT_FALSE (Rust::is_numeric (0x0975)); // ॵ
  ASSERT_FALSE (Rust::is_numeric (0x18f0)); // ᣰ
  ASSERT_FALSE (Rust::is_numeric (0x2f30)); // ⼰
}

} // namespace selftest

#endif // CHECKING_P
