/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "--param vect-max-version-for-alias-checks=0" } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16
char x[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
char cb[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__))) = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
 
__attribute__ ((noinline))
int main1 (char *y)
{  
  struct {
    char *p;
    char *q;
  } s;
  int i;

  /* Not vectorized - can't antialias the pointer s.p from the array cb.  */
  s.p = y;
  for (i = 0; i < N; i++)
    {
      s.p[i] = cb[i];
    }

  /* check results:  */
#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (s.p[i] != cb[i])
        abort ();
    }

  /* Not vectorized - can't antialias the pointer s.p from the pointer s.q.  */
  s.q = cb;
  for (i = 0; i < N; i++)
    {
      s.p[i] = s.q[i];
    }

  /* check results:  */
#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (s.p[i] != s.q[i])
        abort ();
    }

  return 0;
}

int main (void)
{ 
  check_vect ();
  
  return main1 (x);
} 

/* Currently the loops fail to vectorize due to aliasing problems.
   If/when the aliasing problems are resolved, unalignment may
   prevent vectorization on some targets.  */
/* { dg-final { scan-tree-dump-times "vectorized 2 loops" 1 "vect" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-times "can't determine dependence" 2 "vect" { target { ! vect_multiple_sizes } } } } */
/* { dg-final { scan-tree-dump "can't determine dependence" "vect" { target vect_multiple_sizes } } } */
