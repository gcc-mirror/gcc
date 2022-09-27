/* { dg-options "-fopenmp -O2" } */

/* Test that simd clones are not generated for functions with 
   "declare target" but unsuitable arguments.  */

struct s {
  int a;
  int b;
};
  
#pragma omp declare target
int addit (struct s x)
{
  return x.a + x.b;
}
#pragma omp end declare target

/* { dg-final { scan-assembler-not "_Z.*_addit" { target i?86-*-* x86_64-*-* } } } */
