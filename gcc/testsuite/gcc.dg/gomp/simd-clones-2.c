/* { dg-options "-fopenmp -fdump-tree-optimized -O" } */

#pragma omp declare simd inbranch uniform(c) linear(b:66)
#pragma omp declare simd notinbranch aligned(c:32)
int addit(int a, int b, int *c)
{
  return a + b;
}
/* { dg-warning "GCC does not currently support mixed size types for 'simd' functions" "" { target aarch64*-*-* } .-4 } */
/* { dg-final { scan-tree-dump {(?n)^__attribute__\(\(omp declare simd \(notinbranch aligned\(2:32\)\), omp declare simd \(inbranch uniform\(2\) linear\(1:66\)\)\)\)$} "optimized" } } */

#pragma omp declare simd uniform(a) aligned(a:32) linear(k:1) notinbranch
float setArray(float *a, float x, int k)
{
  a[k] = a[k] + x;
  return a[k];
}
/* { dg-final { scan-tree-dump {(?n)^__attribute__\(\(omp declare simd \(notinbranch uniform\(0\) aligned\(0:32\) linear\(2:1\)\)\)\)$} "optimized" } } */

/* { dg-final { scan-tree-dump "_ZGVbN4ua32vl_setArray" "optimized" { target i?86-*-* x86_64-*-* } } } */
/* { dg-final { scan-tree-dump "_ZGVbN4vvva32_addit" "optimized" { target i?86-*-* x86_64-*-* } } } */
/* { dg-final { scan-tree-dump "_ZGVbM4vl66u_addit" "optimized" { target i?86-*-* x86_64-*-* } } } */
/* { dg-final { scan-tree-dump "_ZGVcN8ua32vl_setArray" "optimized" { target i?86-*-* x86_64-*-* } } } */
/* { dg-final { scan-tree-dump "_ZGVcN4vvva32_addit" "optimized" { target i?86-*-* x86_64-*-* } } } */
/* { dg-final { scan-tree-dump "_ZGVcM4vl66u_addit" "optimized" { target i?86-*-* x86_64-*-* } } } */
/* { dg-final { scan-tree-dump "_ZGVdN8ua32vl_setArray" "optimized" { target i?86-*-* x86_64-*-* } } } */
/* { dg-final { scan-tree-dump "_ZGVdN8vvva32_addit" "optimized" { target i?86-*-* x86_64-*-* } } } */
/* { dg-final { scan-tree-dump "_ZGVdM8vl66u_addit" "optimized" { target i?86-*-* x86_64-*-* } } } */
/* { dg-final { scan-tree-dump "_ZGVeN16ua32vl_setArray" "optimized" { target i?86-*-* x86_64-*-* } } } */
/* { dg-final { scan-tree-dump "_ZGVeN16vvva32_addit" "optimized" { target i?86-*-* x86_64-*-* } } } */
/* { dg-final { scan-tree-dump "_ZGVeM16vl66u_addit" "optimized" { target i?86-*-* x86_64-*-* } } } */
