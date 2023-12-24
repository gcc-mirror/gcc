/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break_hw } */
/* { dg-require-effective-target vect_int } */

/* { dg-additional-options "-Ofast" } */

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */

#include "tree-vect.h"

extern void abort ();

int x;
__attribute__ ((noinline, noipa))
void foo (int *a, int *b)
{
  int local_x = x;
  for (int i = 0; i < 1024; ++i)
    {
      if (i + local_x == 13)
        break;
      a[i] = 2 * b[i];
    }
}

int main ()
{

  check_vect ();

  int a[1024] = {0};
  int b[1024] = {0};

  for (int i = 0; i < 1024; i++)
    b[i] = i;

  x = -512;
  foo (a, b);

  if (a[524] != 1048)
    abort ();

  if (a[525] != 0)
    abort ();

  if (a[1023] != 0)
    abort ();
  return 0;
}
