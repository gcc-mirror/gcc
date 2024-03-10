/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 64

unsigned short in[N*8];
unsigned short in2[N*8];

int
main1 ()
{
  int i;
  unsigned short out[N*8];
  unsigned short out2[N*8];

  for (i = 0; i < N*8; i++)
    {
      in[i] = in2[i] = i;
      asm volatile ("" ::: "memory");
    }

  /* SLP with unrolling by 8.  */
  for (i = 0; i < N; i++)
    {
      out[i*3] = in[i*3] + 5;
      out[i*3 + 1] = in[i*3 + 1] + 6;
      out[i*3 + 2] = in[i*3 + 2] + 16;

      out2[i*5] = in2[i*5] + 2;
      out2[i*5 + 1] = in2[i*5 + 1] + 2;
      out2[i*5 + 2] = in2[i*5 + 2] + 1;
      out2[i*5 + 3] = in2[i*5 + 3] + 3;
      out2[i*5 + 4] = in2[i*5 + 4] + 13;
    }

  /* check results:  */
#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (out[i*3] != in[i*3] + 5
          || out[i*3 + 1] != in[i*3 + 1] + 6
          || out[i*3 + 2] != in[i*3 + 2] + 16
          || out2[i*5] != in2[i*5] + 2
          || out2[i*5 + 1] != in2[i*5 + 1] + 2
          || out2[i*5 + 2] != in2[i*5 + 2] + 1
          || out2[i*5 + 3] != in2[i*5 + 3] + 3
          || out2[i*5 + 4] != in2[i*5 + 4] + 13)
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

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 2 "vect"  } } */

