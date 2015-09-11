/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 8

unsigned short X[N];
unsigned short Y[N];
unsigned int result[N];

/* unsigned short->unsigned int widening-mult.  */
__attribute__ ((noinline, noclone)) void
foo (void)
{
  result[0] = (unsigned int) (X[0] * Y[0]);
  result[1] = (unsigned int) (X[1] * Y[1]);
  result[2] = (unsigned int) (X[2] * Y[2]);
  result[3] = (unsigned int) (X[3] * Y[3]);
  result[4] = (unsigned int) (X[4] * Y[4]);
  result[5] = (unsigned int) (X[5] * Y[5]);
  result[6] = (unsigned int) (X[6] * Y[6]);
  result[7] = (unsigned int) (X[7] * Y[7]);
}

int main (void)
{
  int i, tmp;

  check_vect ();

  for (i = 0; i < N; i++)
    {
      X[i] = i;
      Y[i] = 64-i;
    }

  foo ();

  for (i = 0; i < N; i++)
    {
      __asm__ volatile ("");
      tmp = X[i] * Y[i];
      if (result[i] != tmp)
        abort ();
    }

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 1 "slp2" { target { vect_widen_mult_hi_to_si || vect_unpack } } } } */
/* { dg-final { scan-tree-dump-times "vect_recog_widen_mult_pattern: detected" 8 "slp2" { target vect_widen_mult_hi_to_si_pattern } } } */
/* { dg-final { scan-tree-dump-times "pattern recognized" 8 "slp2" { target vect_widen_mult_hi_to_si_pattern } } } */
