/* { dg-additional-options "-std=gnu89" } */

#include <limits.h>

int n = 0;

g (i)
{
  n++;
}

f (m)
{
  int i;
  i = m;
  do
    {
      g (i * 4);
      i -= INT_MAX / 8;
    }
  while (i > 0);
}

main ()
{
  f (INT_MAX/8*4);
  if (n != 4)
    abort ();
  exit (0);
}
