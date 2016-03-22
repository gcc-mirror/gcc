/* PR target/70329 */
/* { dg-do run } */
/* { dg-options "-O2 -ftree-vectorize -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

#define AVX512BW
#include "avx512f-helper.h"

__attribute__((noinline, noclone)) void
foo (unsigned char *src1, unsigned char *src2, unsigned char *dst)
{
  int i;

  for (i = 0; i < 64; i++)
    dst[i] = (unsigned char) ((int) src1[i] * (int) src2[i]);
}

void
TEST (void)
{
  unsigned char a[64], b[64], c[64];
  int i;

  for (i = 0; i < 64; i++)
    {
      a[i] = i;
      b[i] = (i + 1);
    }
  foo (a, b, c);
  for (i = 0; i < 64; i++)
    if (c[i] != (unsigned char) (i * (i + 1)))
      abort ();
}
