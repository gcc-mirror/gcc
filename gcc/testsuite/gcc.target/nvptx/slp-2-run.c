/* { dg-do run } */
/* { dg-options "-O2 -ftree-slp-vectorize" } */

#include "slp-2.c"

int
main(void)
{
  unsigned int i;
  for (i = 0; i < 1000; i += 1)
    {
      p[i] = i;
      p2[i] = 0;
    }

  foo ();

  for (i = 0; i < 1000; i += 1)
    if (p2[i] != i)
      return 1;

  return 0;
}
