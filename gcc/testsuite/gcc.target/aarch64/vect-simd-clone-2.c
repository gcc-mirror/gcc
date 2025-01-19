/* { dg-do compile }  */
/* { dg-options "-std=c99" } */
/* { dg-additional-options "-O3 -march=armv8-a" } */

#pragma GCC target ("+sve")
extern char __attribute__ ((simd, const)) fn3 (int, char);
void test_fn3 (int *a, int *b, char *c, int n)
{
  for (int i = 0; i < n; ++i)
    a[i] = (int) (fn3 (b[i], c[i]) + c[i]);
}

/* { dg-final { scan-assembler {\s+_ZGVsMxvv_fn3\n} } } */
