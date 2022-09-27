/* { dg-options "-fopenmp -Os" } */

/* Test that simd clones are not generated for functions with 
   "declare target" at -Os.  */

#pragma omp declare target
int addit(int a, int b, int c)
{
  return a + b;
}
#pragma omp end declare target

/* { dg-final { scan-assembler-not "_Z.*_addit" { target i?86-*-* x86_64-*-* } } } */
