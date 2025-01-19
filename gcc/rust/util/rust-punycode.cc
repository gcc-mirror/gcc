// Copyright (C) 2020-2025 Free Software Foundation, Inc.

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

// This file provides functions for punycode conversion
// See https://datatracker.ietf.org/doc/html/rfc3492

#include "rust-system.h"
#include "rust-unicode.h"
#include "optional.h"
#include "selftest.h"

namespace Rust {

// https://tools.ietf.org/html/rfc3492#section-4.
constexpr uint32_t BASE = 36;
constexpr uint32_t TMIN = 1;
constexpr uint32_t TMAX = 26;
constexpr uint32_t SKEW = 38;
constexpr uint32_t DAMP = 700;
constexpr uint32_t INITIAL_BIAS = 72;
constexpr uint32_t INITIAL_N = 128;
constexpr char DELIMITER = '-';

std::string
extract_basic_string (const std::vector<Codepoint> &src)
{
  std::string basic_string;
  for (auto c : src)
    {
      if (c.is_ascii ())
	basic_string += c.as_string ();
    }
  return basic_string;
}

uint32_t
adapt_bias (uint32_t delta, const uint32_t n_points, const bool is_first)
{
  delta /= is_first ? DAMP : 2;
  delta += delta / n_points;
  uint32_t k = 0;

  while (delta > (BASE - TMIN) * TMAX / 2)
    {
      delta /= BASE - TMIN;
      k += BASE;
    }
  return k + (BASE - TMIN + 1) * delta / (delta + SKEW);
}

uint32_t
clamped_sub (const uint32_t min, const uint32_t lhs, const uint32_t rhs,
	     const uint32_t max)
{
  if (min + rhs >= lhs)
    return min;
  else if (max + rhs <= lhs)
    return max;
  else
    return lhs - rhs;
}

uint32_t
min_gt_or_eq (const std::vector<Codepoint> &l, const uint32_t threshold)
{
  uint32_t min = UINT32_MAX;
  for (auto c : l)
    if (c.value >= threshold && c.value < min)
      min = c.value;
  return min;
}

char
encode_digit (const uint32_t d)
{
  return d + 22 + (d < 26 ? 75 : 0);
}

tl::optional<std::string>
encode_punycode (const Utf8String &input)
{
  std::vector<Codepoint> input_chars = input.get_chars ();

  uint32_t n = INITIAL_N;
  uint32_t delta = 0;
  uint32_t bias = INITIAL_BIAS;

  std::string output = extract_basic_string (input_chars);
  uint32_t h = output.size ();
  const uint32_t b = h;
  if (b > 0)
    output += DELIMITER;

  while (h < input_chars.size ())
    {
      const uint32_t m = min_gt_or_eq (input_chars, n);

      if (m - n > ((UINT32_MAX - delta) / (h + 1)))
	return tl::nullopt;

      delta += (m - n) * (h + 1);
      n = m;

      for (const auto c : input_chars)
	{
	  if (c.value < n)
	    delta++;
	  else if (c.value == n)
	    {
	      uint32_t q = delta;
	      // encode as a variable length integer
	      for (uint32_t k = 1;; k++)
		{
		  const uint32_t kb = k * BASE;
		  const uint32_t t = clamped_sub (TMIN, kb, bias, TMAX);
		  if (q < t)
		    break;

		  output += encode_digit (t + (q - t) % (BASE - t));
		  q = (q - t) / (BASE - t);
		}
	      output += encode_digit (q);

	      bias = adapt_bias (delta, h + 1, h == b);
	      delta = 0;
	      h++;
	    }
	}
      delta++;
      n++;
    }

  return {output};
}

} // namespace Rust

#if CHECKING_P

namespace selftest {

void
encode_assert (const std::string &input, const std::string &expected)
{
  Rust::Utf8String input_utf8
    = Rust::Utf8String::make_utf8_string (input).value ();
  std::string actual = Rust::encode_punycode (input_utf8).value ();
  ASSERT_EQ (actual, expected);
}

void
rust_punycode_encode_test ()
{
  encode_assert ("abc", "abc-");
  encode_assert ("12345", "12345-");
  encode_assert ("香港", "j6w193g");

  // Examples from https://datatracker.ietf.org/doc/html/rfc3492#section-7.1
  encode_assert ("ليهمابتكلموشعربي؟", "egbpdaj6bu4bxfgehfvwxn");
  encode_assert ("他们为什么不说中文", "ihqwcrb4cv8a8dqg056pqjye");
  encode_assert ("他們爲什麽不說中文", "ihqwctvzc91f659drss3x8bo0yb");
  encode_assert ("Pročprostěnemluvíčesky", "Proprostnemluvesky-uyb24dma41a");
}

} // namespace selftest

#endif // CHECKING_P
