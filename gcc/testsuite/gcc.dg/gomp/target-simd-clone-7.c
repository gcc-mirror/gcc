/* { dg-options "-fopenmp -O2" } */
/* { dg-additional-options "-fopenmp-target-simd-clone=any -fdump-ipa-simdclone-details" } */

/* Test that simd clones are not generated for functions with 
   "declare target" that have no callers in the same compilation unit.  */

#pragma omp declare target
__attribute__ ((__noinline__)) int addit (int a, int b)
{
  return a + b;
}
#pragma omp end declare target

/* { dg-final { scan-ipa-dump "function is not used" "simdclone" { target x86_64-*-* } } } */
/* { dg-final { scan-ipa-dump-not "Generated .* clone" "simdclone" { target x86_64-*-* } } } */
