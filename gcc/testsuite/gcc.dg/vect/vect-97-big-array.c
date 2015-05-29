/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 128

char x[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
char cb[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));

volatile int y = 0;

__attribute__ ((noinline))
int main1 ()
{
  struct {
    char *p;
    char *q;
  } s;
  int i;
  for (i = 0; i < N; i++)
    {
      cb[i] = i*3;
      if (y)
	abort ();
    }

  /* Check that datarefs analysis can determine that the access via pointer
     s.p is based off array x, which enables us to antialias this access from
     the access to array cb.  */
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

  /* Check that datarefs analysis can determine that the access via pointer
     s.p is based off array x, and that the access via pointer s.q is based off
     array cb, which enables us to antialias these two accesses.  */
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
