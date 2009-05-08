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
  float pb[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__))) = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57};
  float pc[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__))) = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19};

  /* Vectorizable: pa may not alias pb and/or pc, even though their
     addresses escape.  &pa would need to escape to point to escaped memory.  */
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
  float a[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));

  check_vect ();

  main1 (a);
  main2 (a);
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" } } */
/*  { dg-final { scan-tree-dump-times "Alignment of access forced using versioning" 2 "vect" { target vect_no_align } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
