/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break_hw } */
/* { dg-require-effective-target vect_int } */

/* { dg-additional-options "-O3" } */
/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" { target { ! "arm*-*-*" } } } } */

#include "tree-vect.h"

double x[1024];
int a[1024];
double __attribute__((noipa)) foo  ()
{
  double sum = 0.0;
  for (int i = 0 ; i < 1023; ++i)
    {
      sum += x[i];
      if (a[i])
        break;
    }
  return sum;
}

int main()
{
  check_vect ();

  for (int i = 0; i < 1024; ++i)
    x[i] = i;
  a[19] = 1;
  if (foo () != 190.)
    __builtin_abort ();
  return 0;
}
