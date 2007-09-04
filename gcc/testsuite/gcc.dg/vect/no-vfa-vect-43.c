/* { dg-require-effective-target vect_float } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 256

__attribute__ ((noinline))
void bar (float *pa, float *pb, float *pc) 
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


__attribute__ ((noinline)) int
main1 (float *pa)
{
  int i;
  float pb[N] __attribute__ ((__aligned__(16))) = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57};
  float pc[N] __attribute__ ((__aligned__(16))) = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19};

 /* Not vectorizable: pa may alias pb and/or pc, since their addresses escape.  */
  for (i = 0; i < N; i++)
    {
      pa[i] = pb[i] * pc[i];
    }

  bar (pa,pb,pc);

  return 0;
}

__attribute__ ((noinline)) int
main2 (float * pa)
{
  int i;
  float pb[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57};
  float pc[N] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19};

  /* Vectorizable: pb and pc addresses do not escape.  */
  for (i = 0; i < N; i++)
    {
      pa[i] = pb[i] * pc[i];
    }   
  
  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (pa[i] != (pb[i] * pc[i]))
        abort ();
    }
  
  return 0;
}

int main (void)
{
  int i;
  float a[N] __attribute__ ((__aligned__(16)));

  check_vect ();

  main1 (a);
  main2 (a);
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/*  { dg-final { scan-tree-dump-times "Alignment of access forced using versioning" 1 "vect" { target vect_no_align } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
