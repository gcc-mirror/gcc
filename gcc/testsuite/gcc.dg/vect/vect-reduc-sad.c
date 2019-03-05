/* { dg-require-effective-target vect_usad_char } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 64
#define SAD N*N/2

unsigned char X[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
unsigned char Y[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
int abs (int);

/* Sum of absolute differences between arrays of unsigned char types.
   Detected as a sad pattern.
   Vectorized on targets that support sad for unsigned chars.  */

__attribute__ ((noinline)) int
foo (int len)
{
  int i;
  int result = 0;

  for (i = 0; i < len; i++)
    result += abs (X[i] - Y[i]);

  return result;
}


int
main (void)
{
  int i;
  int sad;

  check_vect ();

  for (i = 0; i < N; i++)
    {
      X[i] = i;
      Y[i] = N - i;
      __asm__ volatile ("");
    }

  sad = foo (N);
  if (sad != SAD)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "sad pattern recognized" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */

