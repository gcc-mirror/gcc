// P2242R3
// { dg-do compile }
// { dg-options "-std=c++2b" }

constexpr int
foo ()
{
  goto lab;		// { dg-error "'goto' is not a constant expression" }
lab:
  return 1;
}

constexpr int
bar ()
{
  static int a;		// { dg-error "'a' defined 'static' in 'constexpr' context" }
  return ++a;
}

constexpr int
baz ()
{
  thread_local int a;	// { dg-error "'a' defined 'thread_local' in 'constexpr' context" }
  return ++a;
}

// In C++23, we get errors about the non-constant expressions only if we
// actually call the functions in a constexpr context.

void
test ()
{
  constexpr int a = foo (); // { dg-error "constant expression" }
  constexpr int b = bar (); // { dg-error "constant expression" }
  constexpr int c = baz (); // { dg-error "constant expression" }
}
