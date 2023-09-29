// { dg-do compile { target c++20 } }

consteval int id (int i) { return i; }
consteval int add (int i, int j) { return i + j; }

constexpr int
foo (int i = id (42))
{
  return i + id (id (id (0)));
}

constexpr int
bar (int i = id (id (id (42))))
{
  return i;
}

constexpr int
baz (int i = add (add (id (1), id (2)), id (3)))
{
  return i;
}

void
g ()
{
  foo ();
  bar ();
  baz ();
}

static_assert (foo () == 42);
static_assert (bar () == 42);
static_assert (baz () == 6);
