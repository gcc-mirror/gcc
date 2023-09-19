// P2564R3
// { dg-do compile { target c++20 } }
// From clang's cxx2b-consteval-propagate.cpp.  This test ICEd when I worked on
// P2564.

consteval int f (int);

struct S {
  int a = 0;
  int b = f (a);
};

constexpr bool
g (auto i)
{
  S s{i};
  return s.b == 2 *i;
}

consteval int
f (int i)
{
  return 2 * i;
}

void
test ()
{
  static_assert(g(42));
}
