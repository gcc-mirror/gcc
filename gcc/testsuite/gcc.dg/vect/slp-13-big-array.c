/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 64

int
main1 ()
{
  int i;
  unsigned short out[N*8];
  unsigned short in[N*8];
  unsigned int in2[N*8];
  unsigned int out2[N*8];

  for (i = 0; i < N*8; i++)
    {
      in[i] = in2[i] = i;
      asm volatile ("" ::: "memory");
    }

  /* Induction is SLPable.  */
  for (i = 0; i < N; i++)
    {
      out[i*8] = in[i*8] + i;
      out[i*8 + 1] = in[i*8 + 1] + i;
      out[i*8 + 2] = in[i*8 + 2] + i;
      out[i*8 + 3] = in[i*8 + 3] + i;
      out[i*8 + 4] = in[i*8 + 4] + i;
      out[i*8 + 5] = in[i*8 + 5] + i;
      out[i*8 + 6] = in[i*8 + 6] + i;
      out[i*8 + 7] = in[i*8 + 7] + i;
    }

  /* check results:  */
#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (out[i*8] !=  in[i*8] + i
         || out[i*8 + 1] != in[i*8 + 1] + i
         || out[i*8 + 2] != in[i*8 + 2] + i
         || out[i*8 + 3] != in[i*8 + 3] + i
         || out[i*8 + 4] != in[i*8 + 4] + i
         || out[i*8 + 5] != in[i*8 + 5] + i
         || out[i*8 + 6] != in[i*8 + 6] + i
         || out[i*8 + 7] != in[i*8 + 7] + i)
	abort ();
    }

  /* Induction is SLPable.  */
  for (i = 0; i < N/2; i++)
    {
      out2[i*12] = in2[i*12] + i;
      out2[i*12 + 1] = in2[i*12 + 1] + i;
      out2[i*12 + 2] = in2[i*12 + 2] + i;
      out2[i*12 + 3] = in2[i*12 + 3] + i;
      out2[i*12 + 4] = in2[i*12 + 4] + i;
      out2[i*12 + 5] = in2[i*12 + 5] + i;
      out2[i*12 + 6] = in2[i*12 + 6] + i;
      out2[i*12 + 7] = in2[i*12 + 7] + i;
      out2[i*12 + 8] = in2[i*12 + 8] + i;
      out2[i*12 + 9] = in2[i*12 + 9] + i;
      out2[i*12 + 10] = in2[i*12 + 10] + i;
      out2[i*12 + 11] = in2[i*12 + 11] + i;
    }

  /* check results:  */
#pragma GCC novector
  for (i = 0; i < N/2; i++)
    {
        if (out2[i*12] != in2[i*12] + i
            || out2[i*12 + 1] != in2[i*12 + 1] + i
            || out2[i*12 + 2] != in2[i*12 + 2] + i
            || out2[i*12 + 3] != in2[i*12 + 3] + i
            || out2[i*12 + 4] != in2[i*12 + 4] + i
            || out2[i*12 + 5] != in2[i*12 + 5] + i
            || out2[i*12 + 6] != in2[i*12 + 6] + i
            || out2[i*12 + 7] != in2[i*12 + 7] + i
            || out2[i*12 + 8] != in2[i*12 + 8] + i
            || out2[i*12 + 9] != in2[i*12 + 9] + i
            || out2[i*12 + 10] != in2[i*12 + 10] + i
            || out2[i*12 + 11] != in2[i*12 + 11] + i)
          abort ();
    }

  /* Not power of 2 but SLPable.  */
  for (i = 0; i < N/2; i++)
    {
      out2[i*12] = in2[i*12] + 1;
      out2[i*12 + 1] = in2[i*12 + 1] + 2;
      out2[i*12 + 2] = in2[i*12 + 2] + 3;
      out2[i*12 + 3] = in2[i*12 + 3] + 4;
      out2[i*12 + 4] = in2[i*12 + 4] + 5;
      out2[i*12 + 5] = in2[i*12 + 5] + 6;
      out2[i*12 + 6] = in2[i*12 + 6] + 7;
      out2[i*12 + 7] = in2[i*12 + 7] + 8;
      out2[i*12 + 8] = in2[i*12 + 8] + 9;
      out2[i*12 + 9] = in2[i*12 + 9] + 10;
      out2[i*12 + 10] = in2[i*12 + 10] + 11;
      out2[i*12 + 11] = in2[i*12 + 11] + 12;
    }

  /* check results:  */
#pragma GCC novector
  for (i = 0; i < N/2; i++)
    {
        if (out2[i*12] != in2[i*12] + 1
            || out2[i*12 + 1] != in2[i*12 + 1] + 2
            || out2[i*12 + 2] != in2[i*12 + 2] + 3
            || out2[i*12 + 3] != in2[i*12 + 3] + 4
            || out2[i*12 + 4] != in2[i*12 + 4] + 5
            || out2[i*12 + 5] != in2[i*12 + 5] + 6
            || out2[i*12 + 6] != in2[i*12 + 6] + 7
            || out2[i*12 + 7] != in2[i*12 + 7] + 8
            || out2[i*12 + 8] != in2[i*12 + 8] + 9
            || out2[i*12 + 9] != in2[i*12 + 9] + 10
            || out2[i*12 + 10] != in2[i*12 + 10] + 11
            || out2[i*12 + 11] != in2[i*12 + 11] + 12)
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

/* { dg-final { scan-tree-dump-times "vectorized 2 loops" 1 "vect" { target { { vect_interleave && vect_extract_even_odd } && { ! vect_pack_trunc } } } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 2 "vect" { target { ! vect_pack_trunc } } } } */
/* { dg-final { scan-tree-dump-times "vectorized 3 loops" 1 "vect" { target { { vect_interleave && vect_extract_even_odd } && vect_pack_trunc } } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 3 "vect" { target vect_pack_trunc xfail vect_variable_length } } } */
