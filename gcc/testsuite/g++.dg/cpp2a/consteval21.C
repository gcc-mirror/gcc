// PR c++/102753
// { dg-do compile { target c++20 } }

struct S {
  constexpr S () : s (0) {}
  consteval int foo () { return 1; }
  virtual consteval int bar () { return 2; }
  int s;
};

consteval int foo () { return 42; }

consteval int
bar (int (*fn) () = &foo)
{
  return fn ();
}

consteval int
baz (int (S::*fn) () = &S::foo)
{
  S s;
  return (s.*fn) ();
}

consteval int
qux (int (S::*fn) () = &S::bar)
{
  S s;
  return (s.*fn) ();
}

static_assert (bar () == 42);
static_assert (baz () == 1);
static_assert (qux () == 2);
