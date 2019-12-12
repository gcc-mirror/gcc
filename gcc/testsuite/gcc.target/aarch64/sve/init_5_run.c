/* { dg-do run { target aarch64_sve256_hw } } */
/* { dg-options "-O2 -msve-vector-bits=256" } */

#include "init_5.c"

int main()
{
  int a = 10;
  int b = 11;
  int c = 12;

  vnx4si v = foo (a, b, c);
  int expected[] = { a, b, c, c, c, c, c, c };

  for (int i = 0; i < 8; i++)
    if (v[i] != expected[i])
      __builtin_abort ();

  return 0;
}
