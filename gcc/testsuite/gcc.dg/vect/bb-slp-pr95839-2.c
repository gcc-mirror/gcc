/* { dg-do compile } */
/* { dg-require-effective-target vect_double } */
/* { dg-additional-options "-w -Wno-psabi" } */

typedef double __attribute__((vector_size(16))) v2df;

v2df f(v2df a, v2df b)
{
  return (v2df){a[0] + b[0], a[1] + b[1]};
}

v2df g(v2df a, v2df b)
{
  return (v2df){a[0] + b[1], a[1] + b[0]};
}

/* Verify we manage to vectorize this with using the original vectors
   and do not end up with any vector CTORs.  */
/* { dg-final { scan-tree-dump-times "basic block vectorized" 2 "slp2" } } */
/* { dg-final { scan-tree-dump-not "vect_cst" "slp2" } } */
