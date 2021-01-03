// PR c++/97388
// { dg-do compile { target c++20 } }

struct S {
  int m;
  constexpr S () : m(1) {}
  constexpr ~S () noexcept (false) { if (m == 1) { throw; } }
};

constexpr bool
foo (S v)
{
  v.m = 2;
  return true;
}

constexpr bool
bar ()
{
  return foo (S ());
}

constexpr bool
baz ()
{
  foo (S ());
  return foo (S ());
}

static_assert (foo (S ()));
static_assert (bar ());
static_assert (baz ());
constexpr bool x = foo (S ());
constexpr bool y = bar ();
constexpr bool z = baz ();
