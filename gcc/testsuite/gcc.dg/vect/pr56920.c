/* PR tree-optimization/56920 */
/* { dg-additional-options "-O3" } */

#include "tree-vect.h"

extern void abort (void);

int
main ()
{
  unsigned int a[15], i;
  check_vect ();
  for (i = 0; i < 15; ++i)
    a[i] = (i * 2) % 15;
  for (i = 0; i < 15; ++i)
    if (a[i] != (i * 2) % 15)
      abort ();
  return 0;
}

