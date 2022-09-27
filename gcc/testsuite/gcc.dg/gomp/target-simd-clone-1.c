/* { dg-options "-fopenmp -O2" } */

/* Test that simd clones are generated for functions with "declare target".  */

#pragma omp declare target
int addit(int a, int b, int c)
{
  return a + b;
}
#pragma omp end declare target

/* Although addit has external linkage, we expect clones to be generated as
   for a function with internal linkage.  */

/* { dg-final { scan-assembler "\\.type.*_ZGVbN4vvv_addit,.*function" { target i?86-*-* x86_64-*-* } } } */
/* { dg-final { scan-assembler "\\.type.*_ZGVbM4vvv_addit,.*function" { target i?86-*-* x86_64-*-* } } } */
/* { dg-final { scan-assembler-not "\\.globl.*_ZGVbN4vvv_addit" { target i?86-*-* x86_64-*-* } } } */
/* { dg-final { scan-assembler-not "\\.globl.*_ZGVbM4vvv_addit" { target i?86-*-* x86_64-*-* } } } */
