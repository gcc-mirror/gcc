/* { dg-options "-fopenmp -O2" } */

/* Test that simd clones are not generated for functions with 
   "declare target" but that call possibly side-effecting functions 
   in the body.  */

extern int f (int);

#pragma omp declare target
int addit(int a, int b, int c)
{
  return f(a) + b;
}
#pragma omp end declare target

/* { dg-final { scan-assembler-not "_Z.*_addit" { target i?86-*-* x86_64-*-* } } } */

