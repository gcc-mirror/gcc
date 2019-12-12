/* { dg-do run { target aarch64_sve256_hw } } */
/* { dg-options "-O2 -msve-vector-bits=256" } */

#include "init_12.c"

int main()
{
  int a = 10;
  int b = 11;
  int f = 12;

  vnx4si v = foo (a, b, f);
  int expected[] = { b, f, b, f, b, f, a, f };

  for (int i = 0; i < 8; i++)
    if (v[i] != expected[i])
      __builtin_abort ();

  return 0;
}
