// PR c++/102753
// { dg-do compile { target c++20 } }
// { dg-options "" }

struct S {
  constexpr S () : s (0) {}
  consteval int foo () { return 1; }
  virtual consteval int bar () { return 2; }
  int s;
};

consteval int foo () { return 42; }

constexpr int
bar ()
{
  if consteval {	// { dg-warning "'if consteval' only available with" "" { target c++20_only } }
    int (*fn1) () = foo;
    int (S::*fn2) () = &S::foo;
    int (S::*fn3) () = &S::bar;
    S s;
    return fn1 () + (s.*fn2) () + (s.*fn3) ();
  }
  return 0;
}

static_assert (bar () == 45);
