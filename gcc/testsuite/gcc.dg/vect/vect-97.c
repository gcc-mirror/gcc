/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

typedef char achar __attribute__ ((__aligned__(16)));

#define N 16
 
int main1 ()
{  
  struct {
    achar *p;
    achar *q;
  } s;
  int i;
  achar x[N];
  achar cb[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};

  s.p = x;
  for (i = 0; i < N; i++)
    {
      s.p[i] = cb[i];
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (s.p[i] != cb[i])
        abort ();
    }

  s.q = cb;
  for (i = 0; i < N; i++)
    {
      s.p[i] = s.q[i];
    }

  /* check results:  */
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
  
  return main1 ();
} 


/* { dg-final { scan-tree-dump-times "vectorized 2 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 0 "vect" } } */
