/* { dg-options "-fopenmp -O2" } */
/* { dg-additional-options "-fdump-ipa-simdclone-details" } */

/* Test that host simd clones are not generated for functions with 
   "declare target" by default at -O2.  */

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

/* { dg-final { scan-ipa-dump-not "Generated .* clone" "simdclone" { target x86_64-*-* } } } */
