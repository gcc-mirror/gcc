/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */
/* { dg-require-effective-target vect_simd_clones } */
/* { dg-additional-options "-fopenmp-simd --param vect-partial-vector-usage=1 -fdump-tree-dce6 -w" } */
/* { dg-additional-options "-mavx512f -mprefer-vector-width=512" { target avx512f } } */

#pragma omp declare simd simdlen(16)
int __attribute__((const)) baz (int x);

int a[1024];

void foo (int n, int * __restrict b)
{
  for (int i = 0; i < n; ++i)
    if (baz (a[i]))
      b[i] = baz (b[i]);
}

/* One notinbranch SIMD call, one inbranch in the main vector loop and two
   inbranch in the masked epilog.  */
/* { dg-final { scan-tree-dump-times "simdclone\.\[0-9\] \\\(\[^,\]\+\\\)" 1 "dce6" { target avx512f } } } */
/* { dg-final { scan-tree-dump-times "simdclone\.\[0-9\] \\\(\[^,\]\+,\[^,\]\+\\\)" 3 "dce6" { target avx512f } } } */
