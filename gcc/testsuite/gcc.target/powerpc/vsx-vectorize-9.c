/* { dg-do run } */
/* { dg-options "-O2 -ftree-vectorize -fno-tree-loop-distribute-patterns -fno-vect-cost-model -fdump-tree-vect-details" } */

/* Taken from vect/vect-align-1.c.  */
#include <stdlib.h>
#include <stdarg.h>

#include "vsx-vectorize-1.c"

int main (void)
{
  int i;
  struct foo *p = malloc (2*sizeof (struct foo));
  
  main1 (p);

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (p->y[i] != x[i])
	abort ();
    }
  return 0;
}
