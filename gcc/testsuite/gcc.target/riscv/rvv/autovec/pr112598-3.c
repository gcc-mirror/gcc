/* { dg-do compile } */
/* { dg-options "-march=rv32gcv_zvfh_zfh_zvl512b -mabi=ilp32d -O3 -ftree-vectorize -std=c99 -fno-vect-cost-model" } */

#include <stdint-gcc.h>
#define TYPE uint64_t
#define ITYPE int64_t

void __attribute__ ((noinline, noclone))
foo (TYPE *__restrict a, TYPE *__restrict b, TYPE *__restrict c,
    TYPE *__restrict d, ITYPE n)
{
  for (ITYPE i = 0; i < n; ++i)
    {
      d[i * 3] = a[i];
      d[i * 3 + 1] = b[i];
      d[i * 3 + 2] = c[i];
    }
}

/* We don't want vcompress.vv.  */
/* { dg-final { scan-assembler-not {vcompress\.vv} } } */
