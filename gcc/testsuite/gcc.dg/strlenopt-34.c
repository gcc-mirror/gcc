/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-strlen" } */

#include "strlenopt.h"

volatile int v;

size_t __attribute__ ((noinline, noclone))
f1 (char b)
{
  char a[30];
  v += 1;
  strcpy (a, "foo.bar");
  a[3] = b;
  a[4] = 0;
  return strlen (a);
}

size_t __attribute__ ((noinline, noclone))
f2 (char *a, char b)
{
  v += 2;
  strcpy (a, "foo.bar");
  a[3] = b;
  a[4] = 0;
  return strlen (a);
}

int
main ()
{
  char a[30];
  if (f1 ('_') != 4 || f1 (0) != 3 || f2 (a, '_') != 4 || f2 (a, 0) != 3)
    abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "strlen \\(" 2 "strlen" } } */
