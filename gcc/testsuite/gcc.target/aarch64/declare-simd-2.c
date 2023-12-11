/* { dg-do compile } */
/* { dg-options "-fopenmp-simd" } */
#ifdef __cplusplus
extern "C" {
#endif
#pragma omp declare simd
short __attribute__ ((const)) f00 (short a , char b)
{
  return a + b;
}
/* { dg-final { scan-assembler {_ZGVnN8vv_f00:} } } */
/* { dg-final { scan-assembler {_ZGVnM8vv_f00:} } } */

#pragma omp declare simd notinbranch
short __attribute__ ((const)) f01 (int a , short b)
{
  return a + b;
}
/* { dg-final { scan-assembler {_ZGVnN4vv_f01:} } } */
/* { dg-final { scan-assembler-not {_ZGVnM4vv_f01:} } } */

#pragma omp declare simd linear(b) inbranch
int __attribute__ ((const)) f02 (int a, short *b)
{
  return a + *b;
}
/* { dg-final { scan-assembler {_ZGVnM4vl2_f02:} } } */
/* { dg-final { scan-assembler-not {_ZGVnN4vl2_f02:} } } */

#pragma omp declare simd uniform(a) notinbranch
void f03 (char b, int a)
{
}
/* { dg-final { scan-assembler {_ZGVnN8vu_f03:} } } */
/* { dg-final { scan-assembler {_ZGVnN16vu_f03:} } } */
/* { dg-final { scan-assembler-not {_ZGVnM8vu_f03:} } } */
/* { dg-final { scan-assembler-not {_ZGVnM16vu_f03:} } } */

#pragma omp declare simd simdlen(2)
float f04 (double a)
{
  return (float) a;
}
/* { dg-final { scan-assembler {_ZGVnN2v_f04:} } } */
/* { dg-final { scan-assembler {_ZGVnM2v_f04:} } } */

#pragma omp declare simd uniform(a) linear (b)
void f05 (short a, short *b, short c)
{
  *b += a + c;
}

/* { dg-final { scan-assembler {_ZGVnN4ul2v_f05:} } } */
/* { dg-final { scan-assembler {_ZGVnN4ul2v_f05:} } } */
/* { dg-final { scan-assembler {_ZGVnM8ul2v_f05:} } } */
/* { dg-final { scan-assembler {_ZGVnM8ul2v_f05:} } } */
#ifdef __cplusplus
}
#endif

