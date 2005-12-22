/* PR tree-optimization/23518.
   make_range used to transform a + 1 < 0 into a < -1 even when a is
   signed and -fwrapv is given.  Make sure that no longer happens.  */

/* { dg-do run } */
/* { dg-options "-O2 -fwrapv" } */

#include <limits.h>

extern void abort (void);
extern void exit (int);

int
main (void)
{
  int a = INT_MAX;
  if ((a < 0) || (a + 1 < 0))
    exit (0);

  abort ();
}
