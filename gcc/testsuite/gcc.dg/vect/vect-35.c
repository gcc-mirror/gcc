/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

typedef char achar __attribute__ ((__aligned__(16)));

#define N 16
achar x[N];
 
int main1 ()
{  
  union {
    achar a[N];
    achar b[N];
  } s;
  int i;

  for (i = 0; i < N; i++)
    {
      s.b[i] = 3*i;
    }

  for (i = 0; i < N; i++)
    {
      s.a[i] = s.b[i] + 1;
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (s.a[i] != s.b[i])
	abort ();
    }

  return 0;
}

int main (void)
{ 
  check_vect ();
  
  return main1 ();
} 


/* { dg-final { scan-tree-dump-times "vectorized 0 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 0 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
