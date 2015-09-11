/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 64
#define DOT 43680

signed short X[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
signed int   Y[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));

/* (short, int)->int->int dot product.
   Not detected as a dot-product pattern.  */

__attribute__ ((noinline)) int
foo (int len)
{
  int i;
  int result = 0;

  for (i = 0; i < len; i++)
    {
      result += (X[i] * Y[i]);
    }
  return result;
}


/* (int, short)->int->int dot product.
   Not detected as a dot-product pattern.  */

__attribute__ ((noinline)) int
bar (int len)
{
  int i;
  int result = 0;

  for (i = 0; i < len; i++)
    {
      result += (Y[i] * X[i]);
    }
  return result;
}

int
main (void)
{
  int i;
  int dot;

  check_vect ();

  for (i = 0; i < N; i++)
    {
      X[i] = i;
      Y[i] = N - i;
      __asm__ volatile ("");
    }

  dot = foo (N);
  if (dot != DOT)
    abort ();

  dot = bar (N);
  if (dot != DOT)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" { target vect_unpack } } } */

