/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16 -mavx512vl -mavx512dq" } */

static void do_test (void);

#define DO_TEST do_test
#define AVX512FP16
#include "avx512f-check.h"

extern int memcmp (const void *, const void *, __SIZE_TYPE__);

void
do_test (void)
{
  union512i_d zero;
  union512h ad;
  union256h b,bd;
  union128h c;

  int i;

  for (i = 0; i < 16; i++)
    {
      b.a[i] = 65.43f + i;
      zero.a[i] = 0;
    }

  for (i = 0; i < 8; i++)
    {
      c.a[i] = 32.01f + i;
    }
   
  bd.x = _mm256_zextph128_ph256 (c.x);
  if (memcmp (bd.a, c.a, 16)
      || memcmp (&bd.a[8], &zero.a, 16))
    abort ();
}
