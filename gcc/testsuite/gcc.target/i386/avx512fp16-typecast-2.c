/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16 -mavx512dq" } */

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

  ad.x = _mm512_zextph256_ph512 (b.x);
  if (memcmp (ad.a, b.a, 32)
      || memcmp (&ad.a[16], &zero.a, 32))
    abort ();

  ad.x = _mm512_zextph128_ph512 (c.x);
  if (memcmp (ad.a, c.a, 16)
      || memcmp (&ad.a[8], &zero.a, 48))
    abort ();
   
}
