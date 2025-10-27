/* { dg-additional-options "--param vect-partial-vector-usage=1" } */
/* { dg-additional-options "-march=x86-64-v4" { target avx512f_runtime } } */

#include "tree-vect.h"

int mon_lengths[12] = { 1, 10, 100 };

__attribute__ ((noipa)) long
transtime (int mon)
{
  long value = 0;
  for (int i = 0; i < mon; ++i)
    value += mon_lengths[i] * 2l;
  return value;
}

int
main ()
{
  check_vect ();
  if (transtime (3) != 222)
    __builtin_abort ();
  return 0;
}

