/* { dg-do compile } */
/* { dg-options "-O3 -mavx512fp16 -mno-avx512vl" } */

/* Check that we vectorize to a full 128-bit vector for _Float16 types.  */

void
foo (_Float16 *__restrict__ a, _Float16 *__restrict__ b,
     _Float16 *__restrict__ c)
{
  for (int i = 0; i < 256; i++)
    a[i] = b[i] / c[i];
}

/* { dg-final { scan-assembler-times "vdivph" 8 } } */
