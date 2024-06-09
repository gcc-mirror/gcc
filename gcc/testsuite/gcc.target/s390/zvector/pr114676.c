/* { dg-do run { target { s390*-*-* } } } */
/* { dg-options "-O3 -mzarch -march=z14 -mzvector" } */

#include <vecintrin.h>

void __attribute__((noinline)) foo (int *mem)
{
  vec_xst ((vector float){ 1.0f, 2.0f, 3.0f, 4.0f }, 0, (float*)mem);
}

int
main ()
{
  int m[4] = { 0 };
  foo (m);
  if (m[3] == 0)
    __builtin_abort ();
  return 0;
}
