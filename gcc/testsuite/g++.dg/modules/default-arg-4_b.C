// C++20 P1766R1 - Mitigating minor modules maladies
// { dg-do run }
// { dg-additional-options "-fmodules-ts" }

import M;

int
foo (int i = 42)
{
  return i;
}

template <typename T, typename U = int>
int
bar ()
{
  return sizeof (U);
}

template <typename T, int N = 42>
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
