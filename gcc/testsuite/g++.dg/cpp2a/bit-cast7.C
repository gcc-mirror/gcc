// PR c++/98193
// { dg-do compile { target c++20 } }

template <typename To, typename From>
constexpr To
bit_cast (const From &from)
{
  return __builtin_bit_cast (To, from);
}

struct J
{
  long int a, b : 11, h;
};

struct K
{
  long int a, b : 11, c;
  constexpr bool operator == (const K &x) const
  {
    return a == x.a && b == x.b && c == x.c;
  }
};

struct L
{
  long long int a, b : 11, h;
};
struct M
{
  long long int a, b : 11, c;
  constexpr bool operator == (const M &x) const
  {
    return a == x.a && b == x.b && c == x.c;
  }
};

static_assert (bit_cast <K> (J{}) == K{}, "");
static_assert (bit_cast <M> (L{0x0feedbacdeadbeefLL}) == M{0x0feedbacdeadbeefLL}, "");
