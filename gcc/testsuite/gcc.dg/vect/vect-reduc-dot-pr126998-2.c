/* { dg-additional-options "-O3" } */
/* { dg-additional-options "-msse4" { target sse4 } } */

#include "tree-vect.h"

unsigned a, c;
int b, d;
signed char e;
  
void __attribute__((noipa))
f (void)
{ 
  for (; b; ++b)
    {
      c = 0;
      for (; c < 10; c++)
        {
          d = 0;
          for (; d < c; d++)
            a += e;
        }
    }
}

int
main ()
{
  check_vect ();

  b = -3;
  e = 2;
  f ();
  if (a != 3 * 45 * 2)
    __builtin_abort ();

  return 0;
}
