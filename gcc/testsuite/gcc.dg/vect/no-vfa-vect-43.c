/* { dg-require-effective-target vect_float } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 256

__attribute__ ((noinline))
void bar (float *pa, float *pb, float *pc) 
{
  int i;

  /* check results:  */
#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (pa[i] != (pb[i] * pc[i]))
	abort ();
    }

  return;
}


__attribute__ ((noinline)) int
main1 (float *pa, float *pb, float *pc)
{
  int i;
  float b[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
  float c[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));

  for (i = 0; i < N; i++)
    {
      b[i] = pb[i];
      c[i] = pc[i];
    }

  /* Vectorizable: pa may not alias pb and/or pc, even though their
     addresses escape.  &pa would need to escape to point to escaped memory.  */
  for (i = 0; i < N; i++)
    {
      pa[i] = b[i] * c[i];
    }

  bar (pa,b,c);

  return 0;
}

__attribute__ ((noinline)) int
main2 (float *pa, float *pb, float *pc)
{
  int i;
  float b[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
  float c[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));

  for (i = 0; i < N; i++)
    {
      b[i] = pb[i];
      c[i] = pc[i];
    }

  /* Vectorizable: pb and pc addresses do not escape.  */
  for (i = 0; i < N; i++)
    {
      pa[i] = b[i] * c[i];
    }   
  
  /* check results:  */
#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (pa[i] != (b[i] * c[i]))
        abort ();
    }
  
  return 0;
}

int main (void)
{
  int i;
  float a[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
  float b[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57};
  float c[N] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19};

  check_vect ();

  main1 (a,b,c);
  main2 (a,b,c);
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 2 loops" 2 "vect" } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using versioning" 6 "vect" { target { vect_no_align && { ! vect_hw_misalign } } } } } */
