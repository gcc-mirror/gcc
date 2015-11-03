/* { dg-do run } */
/* { dg-options "-O2 -ftree-vectorize -ffast-math -mavx512bw -save-temps" } */
/* { dg-require-effective-target avx512bw } */

#include "avx512bw-check.h"

#define N 400

typedef struct
{
  unsigned char real;
  unsigned char imag;
} complex8_t;

void
__attribute__ ((noinline)) foo (unsigned char *a,
				complex8_t *x, unsigned len)
{
  unsigned i;
  for (i = 0; i < len; i++)
    a[i] = x[i].imag + x[i].real;
}

void
avx512bw_test ()
{
  unsigned short i;
  unsigned char j = 0;
  complex8_t x [N];
  unsigned char a [N];

  for (i = 0; i < N; i++, j++)
    {
      x [i].real = j;
      x [i].imag = j;
    }

  foo (a, x, N);

  j = 0;
  for (i = 0; i < N; i++, j++)
    if ( a[i] != (unsigned char)(j+j) )
      abort ();
}

/* { dg-final { scan-assembler-times "vpmovwb\[ \\t\]+\[^\n\]*%zmm" 4 } } */
