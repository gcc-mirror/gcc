/* { dg-do run { target aarch64_sve256_hw } } */
/* { dg-options "-O2 -msve-vector-bits=256" } */

#include "init_10.c"

int main()
{
  int a = 10;
  int b = 11;
  int c = 12;
  int f = 13;

  vnx4si v = foo (a, b, c, f);
  int expected[] = { a, f, b, f, c, f, c, f };

  for (int i = 0; i < 8; i++)
    if (v[i] != expected[i])
      __builtin_abort ();

  return 0;
}
