/* { dg-options "-fopenmp -O2" } */
/* { dg-additional-options "-fopenmp-target-simd-clone=any -fdump-ipa-simdclone-details" } */

/* Test that simd clones are not generated for functions with 
   "declare target" but unsuitable arguments.  */

struct s {
  int a;
  int b;
};
  
#pragma omp declare target
__attribute__ ((__noinline__)) int addit (struct s x)
{
  return x.a + x.b;
}
#pragma omp end declare target

void callit (struct s *ss, int *c)
{
  int i;
  #pragma omp for simd
  for (i = 0; i < 16; i++)
    c[i] = addit (ss[i]);
}

/* { dg-final { scan-ipa-dump "argument type fails sniff test" "simdclone" { target x86_64-*-* } } } */
/* { dg-final { scan-ipa-dump-not "Generated .* clone" "simdclone" { target x86_64-*-* } } } */
