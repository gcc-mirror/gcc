// PR c++/127276
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

struct A {
  constexpr int &operator[] (int x) { return a[x]; };
  int a[2];
};

constexpr int
foo (A &x, int y)
{
  return [: ^^x :][ [: ^^y :] ];
}

template <typename T>
constexpr int
bar (T &x, int y)
{
  return [: ^^x :][ [: ^^y :] ];
}

constexpr bool
baz ()
{
  A a = { 2, 3 };
  if (foo (a, 0) != 2 || foo (a, 1) != 3)
    return false;
  if (bar (a, 0) != 2 || bar (a, 1) != 3)
    return false;
  return true;
}

static_assert (baz ());
