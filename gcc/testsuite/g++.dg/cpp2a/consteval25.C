// PR c++/102753
// { dg-do compile { target c++20 } }
// { dg-options "" }

consteval int foo () { return 42; }

consteval int
bar (int (*fn) ())
{
  return fn ();
}

void
baz ()
{
  static_assert (bar (({ constexpr auto a = 1; foo; })) == 42);
}
