// PR c++/99859
// { dg-do compile { target c++20 } }

constexpr void
foo (int *x)
{
  ++*x;
}

constexpr int
bar ()
{
  int *x = new int (0);
  foo (x);
  foo (x);
  int y = *x;
  delete x;
  return y;
}

static_assert (bar () == 2);

struct R { int a, *b; };

constexpr void
baz (R x)
{
  ++*x.b;
}

constexpr int
qux ()
{
  int *x = new int (0);
  R r{1, x};
  baz (r);
  baz (r);
  int y = *x;
  delete x;
  return y;
}

static_assert (qux () == 2);
