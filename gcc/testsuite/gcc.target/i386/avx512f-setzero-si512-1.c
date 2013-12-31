/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

void static
avx512f_test (void)
{
  int i;
  union512i_q res;
  long long res_ref[8];

  res.x = _mm512_setzero_si512 ();

  for (i = 0; i < 8; i++)
    res_ref[i] = 0;

  if (check_union512i_q (res, res_ref))
    abort ();
}
