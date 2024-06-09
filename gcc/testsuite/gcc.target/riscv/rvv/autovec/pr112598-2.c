/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zfh_zvl512b -mabi=lp64d -O3 -mrvv-max-lmul=m8" } */

#include <stdint-gcc.h>

void
f (uint8_t *restrict a, uint8_t *restrict b, int n)
{
  for (int i = 0; i < n; ++i)
    {
      a[i * 8] = b[i * 8 + 3] + 1;
      a[i * 8 + 1] = b[i * 8 + 2] + 2;
      a[i * 8 + 2] = b[i * 8 + 1] + 3;
      a[i * 8 + 3] = b[i * 8 + 0] + 4;
      a[i * 8 + 4] = b[i * 8 + 7] + 5;
      a[i * 8 + 5] = b[i * 8 + 6] + 6;
      a[i * 8 + 6] = b[i * 8 + 5] + 7;
      a[i * 8 + 7] = b[i * 8 + 4] + 8;
    }
}

/* We don't want EEW8 LMUL8 vrgather.vv.  */
/* { dg-final { scan-assembler-not {vrgather\.vv} } } */

