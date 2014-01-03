/* { dg-do run } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"
#include "avx512f-broadcast-gpr-1.c"

void
avx512f_test (void)
{
  union512i_q q;
  union512i_d d;
  int i;

  q.x = foo_1 (3);
  d.x = foo_2 (5);

  for (i = 0; i < 8; i++)
  {
    if (q.a[i] != 3)
      abort ();
  }

  for (i = 0; i < 16; i++)
  {
    if (d.a[i] != 5)
      abort ();
  }
}
