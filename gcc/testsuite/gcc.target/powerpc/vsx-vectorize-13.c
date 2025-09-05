/* { dg-do run } */
/* { dg-options "-O2 -ftree-vectorize -fno-vect-cost-model -fdump-tree-vect-details" } */

/* Taken from vect/vect-95.c.  */
#include <stdlib.h>
#include <stdarg.h>

#include "vsx-vectorize-5.c"

__attribute__ ((noinline))
void bar (char *pa, char *pb, char *pc) 
{
  int i;

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (pa[i] != (pb[i] + pc[i]))
	abort ();
    }

  return;
}

int main (void)
{
  int i;
  char a[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
  char b[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57};
  char c[N] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19};

  main1 (N,a,b,c);
  main1 (N-2,a,b,c);

  return 0;
}
