// PR c++/69995
// { dg-do compile { target c++14 } }

struct A
{
  int i;
};

constexpr int f(A a)
{
  ++a.i;
  return a.i;
}

constexpr bool g()
{
  A a = { 42 };
  A b = a;
  ++b.i;
  if (b.i != 43) return false;
  if (a.i != 42) return false;
  return true;
}

#define SA(X) static_assert((X),#X)
SA(g());
