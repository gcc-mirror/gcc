/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16

int
main1 ()
{
  unsigned int i;
  unsigned int out[N*8];
  unsigned int in[N*8] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63};
  unsigned int ia[N*2];

  for (i = 0; i < N; i++)
    {
      out[i*8] = in[i*8];
      out[i*8 + 1] = in[i*8 + 1];
      out[i*8 + 2] = in[i*8 + 2];
      out[i*8 + 3] = in[i*8 + 3];
      out[i*8 + 4] = in[i*8 + 4];
      out[i*8 + 5] = in[i*8 + 5];
      out[i*8 + 6] = in[i*8 + 6];
      out[i*8 + 7] = in[i*8 + 7];

      ia[i] = in[i*8 + 2];
    }

  /* check results:  */
#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (out[i*8] !=  in[i*8]
         || out[i*8 + 1] != in[i*8 + 1]
         || out[i*8 + 2] != in[i*8 + 2]
         || out[i*8 + 3] != in[i*8 + 3]
         || out[i*8 + 4] != in[i*8 + 4]
         || out[i*8 + 5] != in[i*8 + 5]
         || out[i*8 + 6] != in[i*8 + 6]
         || out[i*8 + 7] != in[i*8 + 7]
         || ia[i] != in[i*8 + 2])
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

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target vect_strided8 } } } */
/* { dg-final { scan-tree-dump-times "vectorized 0 loops" 1 "vect" { target { ! vect_strided8 } } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 1 "vect" { target vect_strided8 } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 0 "vect" { target { ! vect_strided8} } } } */
