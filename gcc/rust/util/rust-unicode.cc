#include "rust-system.h"
#include "optional.h"
#include "selftest.h"

#include "rust-unicode-data.h"

namespace Rust {

typedef uint32_t codepoint_t;
typedef std::vector<codepoint_t> string_t;

template <std::size_t SIZE>
int64_t
binary_search_ranges (
  const std::array<std::pair<uint32_t, uint32_t>, SIZE> &ranges,
  uint32_t target_cp)
{
  if (SIZE == 0)
    return -1;

  uint32_t low, high, mid;
  uint32_t start, end;
  low = 0;
  high = SIZE - 1;
  mid = (low + high) / 2;
  while (high - low > 1)
    {
      start = ranges[mid].first;
      end = ranges[mid].second;
      if (start <= target_cp && target_cp < end)
	{
	  return mid;
	}
      else if (end <= target_cp)
	low = mid + 1;
      else
	high = mid - 1;
      mid = (low + high) / 2;
    }

  if (ranges[mid].first <= target_cp && target_cp < ranges[mid].second)
    return mid;
  else
    return -1;
}

template <std::size_t SIZE>
int64_t
binary_search_sorted_array (const std::array<std::uint32_t, SIZE> &array,
			    uint32_t target)
{
  if (SIZE == 0)
    return -1;

  uint32_t low, high, mid;
  low = 0;
  high = SIZE;
  mid = (low + high) / 2;
  while (high - low > 1)
    {
      if (array[mid] <= target)
	low = mid;
      else
	high = mid;
      mid = (low + high) / 2;
    }

  if (array[mid] == target)
    return mid;
  else
    return -1;
}

int
lookup_cc (codepoint_t c)
{
  auto it = Rust::CCC_TABLE.find (c);
  if (it != Rust::CCC_TABLE.end ())
    return it->second;
  else
    // Starter. Returns zero.
    return 0;
}

tl::optional<codepoint_t>
lookup_recomp (codepoint_t starter, codepoint_t c)
{
  auto it = Rust::RECOMPOSITION_MAP.find ({starter, c});
  if (it != Rust::RECOMPOSITION_MAP.end ())
    return {it->second};

  it = Rust::RECOMPOSITION_MAP.find ({starter, 0});
  if (it != Rust::RECOMPOSITION_MAP.end ())
    return {it->second};

  return tl::nullopt;
}

void
recursive_decomp_cano (codepoint_t c, string_t &buf)
{
  auto it = Rust::DECOMPOSITION_MAP.find (c);
  if (it != Rust::DECOMPOSITION_MAP.end ())
    {
      string_t decomped = it->second;
      for (codepoint_t cp : decomped)
	{
	  recursive_decomp_cano (cp, buf);
	}
    }
  else
    buf.push_back (c);
}

string_t
decomp_cano (string_t s)
{
  // TODO: Algorithmic lookup for Hangul
  string_t buf;
  for (codepoint_t c : s)
    recursive_decomp_cano (c, buf);
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
      if (cc_here >= 0 && cc_prev > cc_here)
	{
	  // swap
	  int tmp = s[i];
	  s[i] = s[i - 1];
	  s[i - 1] = tmp;
	  if (i > 1)
	    i -= 2;
	}
    }
}

string_t
recomp (string_t s)
{
  // TODO: Algorithmic lookup for Hangul
  string_t buf;
  if (s.size () > 0)
    {
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
	      // starter_pos = target_pos;
	      starter_ch = ch;
	      last_class = -1;
	      buf.push_back (ch);
	    }
	  else
	    {
	      // ch is not Starter.
	      last_class = ch_class;
	      buf.push_back (ch);
	    }
	}
    }
  return buf;
}

// TODO: remove
/*
void
dump_string (std::vector<uint32_t> s)
{
  std::cout << "dump=";
  for (auto c : s)
    {
      std::cout << std::hex << c << ", ";
    }
  std::cout << std::endl;
}
*/

string_t
nfc_normalize (string_t s)
{
  // TODO: Quick Check

  // decompose
  string_t d = decomp_cano (s);
  sort_cano (d);

  // recompose
  string_t r = recomp (d);
  return r;
}

bool
is_alphabetic (uint32_t codepoint)
{
  int64_t res = binary_search_ranges (ALPHABETIC_RANGES, codepoint);
  if (res < 0)
    return false;
  else
    return true;
}

bool
is_numeric (uint32_t codepoint)
{
  int64_t res = binary_search_sorted_array (NUMERIC_CODEPOINTS, codepoint);
  if (res < 0)
    return false;
  else
    return true;
}

} // namespace Rust

#if CHECKING_P

namespace selftest {

void
assert_normalize (std::vector<uint32_t> origin, std::vector<uint32_t> expected)
{
  std::vector<uint32_t> actual = Rust::nfc_normalize (origin);

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
