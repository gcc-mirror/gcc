/* PR1276000 */

#include "tree-vect.h"

extern void abort (void) __attribute__ ((noreturn));

int __attribute__((noipa))
f (int *a, int n)
{
  int last = -99;
  int j = 1;
  for (int i = 0; i < n; i++)
    {
      if (a[i])
        last = j;
      j = -j;
    }
  return last;
}

int
main (void)
{
  check_vect ();

  int a[16] = { 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
  if (f (a, 16) != 1)
    abort ();

  int b[16] = { 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
  if (f (b, 16) != -1)
    abort ();

  int c[16] = { 0 };
  if (f (c, 16) != -99)
    abort ();

  int d[16] = { 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
  if (f (d, 16) != 1)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-not "condition expression based on integer induction." "vect" } } */
