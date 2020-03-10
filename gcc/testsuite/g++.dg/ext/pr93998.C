// PR c++/93998
// { dg-do compile { target c++11 } }

struct C
{
  constexpr bool operator== (C x) const noexcept { return v == x.v; }
  int v;
};

int
foo (const C a, const C b, bool c)
{
  return __builtin_expect (!!(a == b || c), 1) ? 0 : 1;
}
