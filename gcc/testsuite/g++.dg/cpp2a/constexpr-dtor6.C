// PR c++/97388
// { dg-do compile { target c++20 } }

struct S {
  int *s;
  constexpr S () : s(new int ()) {}
  constexpr S (S &&x) noexcept : s(x.s) { x.s = nullptr; }
  constexpr ~S () noexcept { delete s; }
};

constexpr bool
foo (S v)
{
  auto x = static_cast<S &&> (v);
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
