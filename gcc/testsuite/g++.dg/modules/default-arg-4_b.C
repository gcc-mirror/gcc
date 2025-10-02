// C++20 P1766R1 - Mitigating minor modules maladies
// FIXME mark as run once we no longer xfail below.
// { dg-module-do compile }
// { dg-additional-options "-fmodules-ts" }

import M;

int
foo (int i = 42)			// { dg-bogus "default argument given for parameter 1 of 'int foo\\\(int\\\)'" "PR99000" { xfail *-*-* } }
{
  return i;
}

template <typename T, typename U = int>	// { dg-bogus "redefinition of default argument for 'class U'" "PR99000" { xfail *-*-* } }
int
bar ()
{
  return sizeof (U);
}

template <typename T, int N = 42>	// { dg-bogus "redefinition of default argument for 'int N'" "PR99000" { xfail *-*-* } }
int
baz ()
{
  return N;
}

int
main ()
{
  if (foo () + bar <int> () + baz <int> () != qux ())
    __builtin_abort ();
  if (foo () != foo (42)
      || bar <int> () != bar <int, int> ()
      || baz <int> () != baz <int, 42> ())
    __builtin_abort ();
}
