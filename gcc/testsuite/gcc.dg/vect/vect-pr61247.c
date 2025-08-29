/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

#include <stddef.h>

int foo (int *p, size_t sz)
{
  int sum = 0;
  for (unsigned i = 0; i < sz; ++i)
    sum += p[i];
  return sum;
}

/* The possibly wrapping IV together header copying confused SCEV
   enough to fail vectorization even when versioning with niter
   assumptions.  */
/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" } } */
