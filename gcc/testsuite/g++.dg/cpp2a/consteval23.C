// PR c++/102753
// { dg-do compile { target c++20 } }

consteval int foo () { return 42; }
constexpr auto baz (int (*fn) ()) { return fn; }

consteval int
bar (int (*fn) () = foo)
{
  return fn ();
}

static_assert (bar () == 42);
static_assert (bar (foo) == 42);
static_assert (bar (&foo) == 42);
static_assert (bar (baz (foo)) == 42);
static_assert (bar (baz (&foo)) == 42);
