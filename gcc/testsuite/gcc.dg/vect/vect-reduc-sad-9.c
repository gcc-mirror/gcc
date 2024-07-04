/* Disabling epilogues until we find a better way to deal with scans.  */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-additional-options "-msse4.2" { target { x86_64-*-* i?86-*-* } } } */
/* { dg-require-effective-target vect_usad_char } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 64

unsigned char X[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
unsigned char Y[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
int abs (int);

/* Sum of absolute differences between arrays of unsigned char types.
   Detected as a sad pattern.
   Vectorized on targets that support sad for unsigned chars.  */

__attribute__ ((noinline)) int
foo (int len, int *res2)
{
  int i;
  int result = 0;
  int result2 = 0;

  for (i = 0; i < len; i++)
    {
      /* Make sure we are not using an SLP reduction for this.  */
      result += abs (X[2*i] - Y[2*i]);
      result2 += abs (X[2*i + 1] - Y[2*i + 1]);
    }

  *res2 = result2;
  return result;
}


int
main (void)
{
  int i;
  int sad;

  check_vect ();

  for (i = 0; i < N/2; i++)
    {
      X[2*i] = i;
      Y[2*i] = N/2 - i;
      X[2*i+1] = i;
      Y[2*i+1] = 0;
      __asm__ volatile ("");
    }


  int sad2;
  sad = foo (N/2, &sad2);
  if (sad != (N/2)*(N/4))
    abort ();
  if (sad2 != (N/2-1)*(N/2)/2)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump "vect_recog_sad_pattern: detected" "vect" } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */

