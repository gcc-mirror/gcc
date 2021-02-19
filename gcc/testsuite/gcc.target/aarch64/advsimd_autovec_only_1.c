/* { dg-do compile } */
/* { dg-options "-O3 -mcpu=neoverse-v1" } */

void __attribute__((noinline, noclone))
vadd (int * __restrict__ dst, int * __restrict__ op1, int * __restrict__ op2, unsigned int count)
{
  for (int i = 0; i < count; ++i)
  {
    dst[i] = op1[i] + op2[i];
  }
}

/* { dg-final { scan-assembler-not {\tadd\tz[0-9]+} } } */
