// C++20 P1766R1 - Mitigating minor modules maladies
// { dg-additional-options "-fmodules-ts" }

import M;

int
foo (int i = 42)			// { dg-error "default argument given for parameter 1 of 'int foo\\\(int\\\)'" }
{
  return i;
}

template <typename T, typename U = int>	// { dg-error "redefinition of default argument for 'class U'" }
int
bar ()
{
  return sizeof (U);
}

template <typename T, int N = 42>	// { dg-error "redefinition of default argument for 'int N'" }
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
