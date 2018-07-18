/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-strlen" } */

#include "strlenopt.h"

volatile int v;

size_t __attribute__ ((noinline, noclone))
f1 (char *b)
{
  char a[30];
  v += 1;
  strcpy (a, b);
  // This needs to stay.
  int len1 = strlen (a);
  a[0] = '_';
  a[1] = 0;
  return len1 + strlen (a);
}

size_t __attribute__ ((noinline, noclone))
f2 (char *a, char *b)
{
  v += 2;
  strcpy (a, b);
  // This needs to stay.
  int len1 = strlen (a);
  a[0] = '_';
  a[1] = 0;
  return len1 + strlen (a);
}

int
main ()
{
  char a[30];
  if (f1 ("foo") != 4 || f2 (a, "foobar") != 7)
    abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "strlen \\(" 2 "strlen" } } */
