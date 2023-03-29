/* { dg-options "-fopenmp -O2" } */
/* { dg-additional-options "-fopenmp-target-simd-clone=any -fdump-ipa-simdclone-details" } */

/* Test that host simd clones are not generated for functions with the nohost
   "declare target" clause.  */

__attribute__ ((__noinline__)) int addit (int a, int b)
{
  return a + b;
}
#pragma omp declare target to(addit) device_type(nohost)

void callit (int *a, int *b, int *c)
{
  int i;
  #pragma omp for simd
  for (i = 0; i < 16; i++)
    c[i] = addit (a[i], b[i]);
}

/* { dg-final { scan-ipa-dump "device doesn't match" "simdclone" { target x86_64-*-* } } } */
/* { dg-final { scan-ipa-dump-not "Generated .* clone" "simdclone" { target x86_64-*-* } } } */
