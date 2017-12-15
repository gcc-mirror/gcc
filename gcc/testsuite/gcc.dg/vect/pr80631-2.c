/* PR tree-optimization/80631 */
/* { dg-do run } */

#include "tree-vect.h"

int v[8] = { 77, 1, 79, 3, 4, 3, 6, 7 };

__attribute__((noinline, noclone)) void
f1 (void)
{
  int k, r = -1;
  for (k = 7; k >= 0; k--)
    if (v[k] == 77)
      r = k;
  if (r != 0)
    abort ();
}

__attribute__((noinline, noclone)) void
f2 (void)
{
  int k, r = 4;
  for (k = 7; k >= 0; k--)
    if (v[k] == 79)
      r = k;
  if (r != 2)
    abort ();
}

__attribute__((noinline, noclone)) void
f3 (void)
{
  int k, r = -17;
  for (k = 7; k >= 0; k--)
    if (v[k] == 78)
      r = k;
  if (r != -17)
    abort ();
}

__attribute__((noinline, noclone)) void
f4 (void)
{
  int k, r = 7;
  for (k = 7; k >= 0; k--)
    if (v[k] == 78)
      r = k;
  if (r != 7)
    abort ();
}

__attribute__((noinline, noclone)) void
f5 (void)
{
  int k, r = -1;
  for (k = 7; k >= 0; k--)
    if (v[k] == 3)
      r = k;
  if (r != 3)
    abort ();
}

int
main ()
{
  check_vect ();
  f1 ();
  f2 ();
  f3 ();
  f4 ();
  f5 ();
  return 0;
}
