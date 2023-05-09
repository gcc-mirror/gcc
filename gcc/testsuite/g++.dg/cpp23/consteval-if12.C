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
consteval auto baz () { return foo; }
consteval auto qux () { return &S::foo; }
consteval auto corge () { return &S::bar; }

constexpr int
bar ()
{
  S s;
  if consteval {			// { dg-warning "'if consteval' only available with" "" { target c++20_only } }
    constexpr auto fn1 = foo;		// { dg-error "constant evaluation returns address of immediate function" }
    constexpr auto fn2 = &foo;		// { dg-error "constant evaluation returns address of immediate function" }
    constexpr auto fn3 = &S::foo;	// { dg-error "constant evaluation returns address of immediate function" }
    constexpr auto fn4 = &S::bar;	// { dg-error "constant evaluation returns address of immediate function" }
    constexpr auto fn5 = baz ();	// { dg-error "immediate evaluation returns address of immediate function" }
    constexpr auto fn6 = qux ();	// { dg-error "immediate evaluation returns address of immediate function" }
    constexpr auto fn7 = corge ();	// { dg-error "immediate evaluation returns address of immediate function" }
    return fn1 () + fn2 () + (s.*fn3) () + (s.*fn4) () + fn5 () + (s.*fn6) () + (s.*fn7) ();
  }
  return 0;
}

auto a = bar ();
