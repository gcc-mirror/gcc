/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16

struct tmp_struct
{
  int x;
  int y[N];
};
	 
__attribute__ ((noinline))
int main1 ()
{  
  int i, *q;
  struct tmp_struct tmp, *p;

  p = &tmp;
  q = p->y;

  for (i = 0; i < N; i++)
    {
      *q++ = 5;
    }

  /* check results: */  
  for (i = 0; i < N; i++)
    {
      if (p->y[i] != 5)
        {
          abort ();
        }
    }

  return 0;
}

int main (void)
{ 
  check_vect ();
  
  return main1 ();
} 

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 0 "vect" } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using peeling" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
