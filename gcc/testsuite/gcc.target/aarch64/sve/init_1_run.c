/* { dg-do run { target aarch64_sve256_hw } } */
/* { dg-options "-O2 -msve-vector-bits=256" } */

#include "init_1.c"

int main()
{
  int a = 10;
  int b = 11;

  vnx4si v = foo (a, b);
  int expected[] = { a, b, 1, 2, 3, 4, 5, 6 };

  for (int i = 0; i < 8; i++)
    if (v[i] != expected[i])
      __builtin_abort ();

  return 0;
}
