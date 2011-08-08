#include <stdlib.h>
#include <stdarg.h>

static int a[1000];

int
foo (void)
{
  int j;
  int i;
  for (i = 0; i < 1000; i++)
    for (j = 0; j < 1000; j++)
      a[j] = a[i] + 1;
  return a[0];
}

int
main (void)
{
  int res = foo ();
  if (res != 1999)
    abort ();
  return 0;
}

/* { dg-final { cleanup-tree-dump "vect" } } */
