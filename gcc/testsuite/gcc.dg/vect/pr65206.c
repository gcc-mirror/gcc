/* { dg-do compile } */
/* { dg-require-effective-target vect_double } */
/* { dg-additional-options "-fno-trapping-math -fno-allow-store-data-races" } */
/* { dg-additional-options "-mavx" { target avx } } */

#define N 1024

double a[N], b[N];

void foo ()
{
  for (int i = 0; i < N; ++i)
    if (b[i] < 3.)
      a[i] += b[i];
}

/* We get a .MASK_STORE because while the load of a[i] does not trap
   the store would introduce store data races.  Make sure we still
   can handle the data dependence with zero distance.  */

/* { dg-final { scan-tree-dump-not "versioning for alias required" "vect" { target { vect_masked_store || avx } } } } */
/* { dg-final { scan-tree-dump "vectorized 1 loops in function" "vect" { target { vect_masked_store || avx } } } } */
