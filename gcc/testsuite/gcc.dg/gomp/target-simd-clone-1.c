/* { dg-options "-fopenmp -O2" } */
/* { dg-additional-options "-fopenmp-target-simd-clone=any -fdump-ipa-simdclone-details -fdump-ipa-cgraph" } */

/* Test that simd clones are generated for functions with "declare target".  */

#pragma omp declare target
__attribute__ ((__noinline__)) int addit (int a, int b)
{
  return a + b;
}
#pragma omp end declare target

void callit (int *a, int *b, int *c)
{
  int i;
  #pragma omp for simd
  for (i = 0; i < 16; i++)
    c[i] = addit (a[i], b[i]);
}

/* Although addit has external linkage, we expect clones to be generated as
   for a function with internal linkage.  */

/* { dg-final { scan-ipa-dump "Generated local clone _ZGV.*N.*_addit" "simdclone" { target x86_64-*-* } } } */
/* { dg-final { scan-ipa-dump "Generated local clone _ZGV.*M.*_addit" "simdclone" { target x86_64-*-* } } } */

/* Only the "N" clone is used.  The other one should be GC'ed.  */

/* { dg-final { scan-ipa-dump "Deleting unused function _ZGV.*M.*_addit" "cgraph" { target x86_64-*-* } } } */
