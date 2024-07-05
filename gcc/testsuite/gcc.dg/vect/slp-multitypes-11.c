/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 18 

struct s 
{
  int a;
  int b;
  int c;
};

char in[N*3] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53};

__attribute__ ((noinline)) int
main1 ()
{
  int i;
  struct s out[N];

  for (i = 0; i < N; i++)
    {
      out[i].a = (int) in[i*3] + 1;
      out[i].b = (int) in[i*3 + 1] + 2;
      out[i].c = (int) in[i*3 + 2] + 3;
    }

  /* check results:  */
#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (out[i].a !=  (int) in[i*3] + 1
         || out[i].b != (int) in[i*3 + 1] + 2
         || out[i].c != (int) in[i*3 + 2] + 3)
	abort ();
    }

  return 0;
}

int main (void)
{
  check_vect ();

  main1 ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect"  { target vect_unpack } } } */
/* The epilogues are vectorized using partial vectors.  */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 1 "vect"  { target { vect_unpack && { { ! vect_partial_vectors_usage_1 } || s390_vx } } } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 2 "vect"  { target { { vect_unpack && vect_partial_vectors_usage_1 } && { ! s390_vx } } } } } */
