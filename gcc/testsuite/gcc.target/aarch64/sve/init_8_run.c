/* { dg-do run { target aarch64_sve256_hw } } */
/* { dg-options "-O2 -msve-vector-bits=256" } */

#include "init_8.c"

int main()
{
  int a = 10;
  int b = 11;
  int c = 12;
  int d = 13;

  vnx4si v = foo (a, b, c, d);
  int expected[] = { a, 1, b, 2, c, 3, d, 4 };

  for (int i = 0; i < 8; i++)
    if (v[i] != expected[i])
      __builtin_abort ();

  return 0;
}
