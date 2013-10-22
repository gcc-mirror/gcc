// PR c++/49136
// { dg-do compile }
// { dg-options "-std=c++11" }

struct S
{
  unsigned : 1; unsigned s : 27; unsigned : 4;
  constexpr S (unsigned int x) : s(x) {}
};

template <typename S>
struct T
{
  unsigned int t;
  constexpr T (S s) : t(s.s != 7 ? 0 : s.s) {}
  constexpr T (S s, S s2) : t(s.s != s2.s ? 0 : s.s) {}
};

constexpr S s (7), s2 (7);
constexpr T<S> t (s), t2 (s, s2);
static_assert (t.t == 7, "Error");
static_assert (t2.t == 7, "Error");

struct U
{
  int a : 1; int s : 1;
  constexpr U (int x, int y) : a (x), s (y) {}
};

constexpr U u (0, -1), u2 (-1, -1);
constexpr T<U> t3 (u), t4 (u, u2);
static_assert (t3.t == 0, "Error");
static_assert (t4.t == -1, "Error");
