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
  static int a;		// { dg-error "'a' declared 'static' in 'constexpr' context" }
  return ++a;
}

constexpr int
baz (int x)
{
  thread_local int a;	// { dg-error "'a' declared 'thread_local' in 'constexpr' context" }
  return ++a;
}
