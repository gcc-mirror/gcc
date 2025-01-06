/* { dg-do compile }  */
/* { dg-options "-std=c99" } */
/* { dg-additional-options "-O3 -march=armv8-a" } */

#pragma GCC target ("+sve")
extern char __attribute__ ((simd, const)) fn3 (int, short);
void test_fn3 (float *a, float *b, double *c, int n)
{
  for (int i = 0; i < n; ++i)
    a[i] = fn3 (b[i], c[i]);
}

/* { dg-final { scan-assembler {\s+_ZGVsMxvv_fn3\n} } } */

extern char __attribute__ ((simd, const)) fn4 (int, char);
void test_fn4 (float *a, float *b, double *c, int n)
{
  for (int i = 0; i < n; ++i)
    a[i] = fn4 (b[i], c[i]);
}

/* { dg-final { scan-assembler {\s+_ZGVsMxvv_fn4\n} } } */

/* { dg-final { scan-assembler-times {\s+uzp1\tz[0-9]+\.b, z[0-9]+\.b, z[0-9]+\.b\n} 6 } } */
/* { dg-final { scan-assembler-times {\s+uzp1\tz[0-9]+\.h, z[0-9]+\.h, z[0-9]+\.h\n} 16 } } */
/* { dg-final { scan-assembler-times {\s+uzp1\tz[0-9]+\.s, z[0-9]+\.s, z[0-9]+\.s\n} 24 } } */

