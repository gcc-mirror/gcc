/* { dg-require-effective-target vect_float } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 256


void bar (const float *pa, const float *pb, const float *pc) 
{
  int i;

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (pa[i] != (pb[i] * pc[i]))
	abort ();
    }

  return;
}


int
main1 (float *pa, float *pb, float *pc)
{
  int i;

  for (i = 0; i < N; i++)
    {
      pa[i] = pb[i] * pc[i];
    }

  bar (pa,pb,pc);

  return 0;
}

int main (void)
{
  int i;
  float a[N];
  float b[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57};
  float c[N] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19};

  check_vect ();

  main1 (a,b,c);
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { xfail *-*-* } } } */
