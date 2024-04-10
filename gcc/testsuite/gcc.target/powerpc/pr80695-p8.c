/* { dg-do compile { target { powerpc_vsx_ok } } } */
/* { dg-require-effective-target vect_int } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx -O3 -fdump-tree-slp-details" } */

/* PR80695: Verify cost model for vec_construct on POWER8.  */

long a[10] __attribute__((aligned(16)));

void foo (long i, long j, long k, long l)
{
  a[6] = i;
  a[7] = j;
  a[8] = k;
  a[9] = l;
}

/* { dg-final { scan-tree-dump-times "vectorization is not profitable" 1 "slp2" } } */
