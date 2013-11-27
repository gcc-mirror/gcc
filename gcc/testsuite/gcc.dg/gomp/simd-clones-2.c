/* { dg-options "-fopenmp -fdump-tree-optimized -O" } */

#pragma omp declare simd inbranch uniform(c) linear(b:66)
#pragma omp declare simd notinbranch aligned(c:32)
int addit(int a, int b, int *c)
{
  return a + b;
}

#pragma omp declare simd uniform(a) aligned(a:32) linear(k:1) notinbranch
float setArray(float *a, float x, int k)
{
  a[k] = a[k] + x;
  return a[k];
}

/* { dg-final { scan-tree-dump "_ZGVbN4ua32vl_setArray" "optimized" { target i?86-*-* x86_64-*-* } } } */
/* { dg-final { scan-tree-dump "_ZGVbN4vvva32_addit" "optimized" { target i?86-*-* x86_64-*-* } } } */
/* { dg-final { scan-tree-dump "_ZGVbM4vl66u_addit" "optimized" { target i?86-*-* x86_64-*-* } } } */
/* { dg-final { scan-tree-dump "_ZGVcN8ua32vl_setArray" "optimized" { target i?86-*-* x86_64-*-* } } } */
/* { dg-final { scan-tree-dump "_ZGVcN4vvva32_addit" "optimized" { target i?86-*-* x86_64-*-* } } } */
/* { dg-final { scan-tree-dump "_ZGVcM4vl66u_addit" "optimized" { target i?86-*-* x86_64-*-* } } } */
/* { dg-final { scan-tree-dump "_ZGVdN8ua32vl_setArray" "optimized" { target i?86-*-* x86_64-*-* } } } */
/* { dg-final { scan-tree-dump "_ZGVdN8vvva32_addit" "optimized" { target i?86-*-* x86_64-*-* } } } */
/* { dg-final { scan-tree-dump "_ZGVdM8vl66u_addit" "optimized" { target i?86-*-* x86_64-*-* } } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
