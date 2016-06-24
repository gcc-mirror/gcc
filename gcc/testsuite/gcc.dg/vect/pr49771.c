extern void abort (void);

#include <stdarg.h>
#include "tree-vect.h"

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
  int res;
  check_vect ();
  res = foo ();
  if (res != 1999)
    abort ();
  return 0;
}

