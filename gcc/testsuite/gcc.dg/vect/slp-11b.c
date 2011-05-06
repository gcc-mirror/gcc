/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 8

int
main1 ()
{
  int i;
  unsigned int out[N*8], a0, a1, a2, a3, a4, a5, a6, a7, b1, b0, b2, b3, b4, b5, b6, b7;
  unsigned int in[N*8] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63};

  /* Requires permutation - not SLPable.  */
  for (i = 0; i < N*2; i++)
    {
      out[i*4] = (in[i*4] + 2) * 3;
      out[i*4 + 1] = (in[i*4 + 2] + 2) * 7;
      out[i*4 + 2] = (in[i*4 + 1] + 7) * 3;
      out[i*4 + 3] = (in[i*4 + 3] + 3) * 4;
    }

  /* check results:  */
  for (i = 0; i < N*2; i++)
    {
      if (out[i*4] !=  (in[i*4] + 2) * 3
         || out[i*4 + 1] != (in[i*4 + 2] + 2) * 7
         || out[i*4 + 2] != (in[i*4 + 1] + 7) * 3
         || out[i*4 + 3] != (in[i*4 + 3] + 3) * 4)
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

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target { vect_strided4 && vect_int_mult } } } } */
/* { dg-final { scan-tree-dump-times "vectorized 0 loops" 1 "vect" { target { ! { vect_strided4 && vect_int_mult } } } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 0 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
