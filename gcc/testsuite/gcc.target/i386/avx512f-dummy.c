/* { dg-do run } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

void static
avx512f_test (void)
{
  union512i_q u;
  long long e[8];
  volatile int tst __attribute__((unused)) = check_union512i_q (u, e);
}
