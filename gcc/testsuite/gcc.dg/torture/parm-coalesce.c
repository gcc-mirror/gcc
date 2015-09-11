/* { dg-do run } */

#include <stdlib.h>

/* Make sure we don't coalesce both incoming parms, one whose incoming
   value is unused, to the same location, so as to overwrite one of
   them with the incoming value of the other.  */

int __attribute__((noinline, noclone))
foo (int i, int j)
{
  j = i; /* The incoming value for J is unused.  */
  i = 2;
  if (j)
    j++;
  j += i + 1;
  return j;
}

/* Same as foo, but with swapped parameters.  */
int __attribute__((noinline, noclone))
bar (int j, int i)
{
  j = i; /* The incoming value for J is unused.  */
  i = 2;
  if (j)
    j++;
  j += i + 1;
  return j;
}

int
main (void)
{
  if (foo (0, 1) != 3)
    abort ();
  if (bar (1, 0) != 3)
    abort ();
  return 0;
}
