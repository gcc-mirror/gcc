/* { dg-do compile } */
/* { dg-options "-O -mno-avx -Wno-psabi -mtune=generic" } */

struct A { long b[8] __attribute__((aligned (32))); };

void
foo (long double x, struct A y)
{
  int i;
  if (x != 8.0L)
    __builtin_abort ();
  for (i = 0; i < 8; i++)
    if (y.b[i] != i)
      __builtin_abort ();
}
