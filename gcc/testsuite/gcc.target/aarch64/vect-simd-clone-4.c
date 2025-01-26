/* { dg-do compile }  */
/* { dg-options "-std=c99" } */
/* { dg-additional-options "-O3 -march=armv8-a" } */

#pragma GCC target ("+sve")

extern char __attribute__ ((simd, const)) fn3 (short);
void test_fn3 (float *a, float *b, double *c, int n)
{
  for (int i = 0; i < n; ++i)
    a[i] = fn3 (c[i]);
}

/* { dg-final { scan-assembler {\s+_ZGVsMxv_fn3\n} } } */

