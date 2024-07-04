/* { dg-do compile } */
/* { dg-require-effective-target vect_simd_clones } */
/* { dg-additional-options "-fopenmp-simd" } */

#pragma omp declare simd simdlen(4) inbranch
__attribute__((noinline)) int
foo (int a, int b)
{
  return a + b;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 4 "vect" { target i?86-*-* x86_64-*-* } } } */
/* if-conversion shouldn't need to resort to masked stores for the result
   array created by OMP lowering since that's automatic and does not have
   its address taken.  */
/* { dg-final { scan-tree-dump-not "MASK_STORE" "vect" } } */
