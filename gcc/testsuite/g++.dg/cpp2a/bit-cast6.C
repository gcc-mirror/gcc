// PR libstd++/93121
// { dg-do compile { target c++20 } }

namespace std
{
enum class byte : unsigned char {};
template <typename To, typename From>
constexpr To
bit_cast (const From &from)
{
  return __builtin_bit_cast (To, from);
}
}

struct S { unsigned short s[2]; };
constexpr std::byte from1[sizeof (S)]{};
constexpr auto to1 = std::bit_cast<S>(from1);
constexpr unsigned char from2[sizeof (S)]{};
constexpr auto to2 = std::bit_cast<S>(from2);

constexpr bool
cmp (const S &s1, const S &s2)
{
  for (int i = 0; i < sizeof (s1.s) / sizeof (s1.s[0]); i++)
    if (s1.s[i] != s2.s[i])
      return false;
  return true;
}

static_assert (cmp (to1, S{}));
static_assert (cmp (to2, S{}));
