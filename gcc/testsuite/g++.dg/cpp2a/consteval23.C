// PR c++/102753
// { dg-do compile { target c++20 } }

consteval int foo () { return 42; }

consteval int
bar (int (*fn) () = foo)
{
  return fn ();
}

static_assert (bar () == 42);
static_assert (bar (foo) == 42);
