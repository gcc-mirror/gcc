/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-strlen" } */

#include "strlenopt.h"

char a[10];

int __attribute__ ((noinline, noclone))
f1 (int n)
{
  a[0] = '1';
  a[1] = '2';
  return strlen (a + 1) < n ? strlen (a) : 100;
}

int __attribute__ ((noinline, noclone))
f2 (char *a, int n)
{
  a[0] = '1';
  a[1] = '2';
  return strlen (a + 1) < n ? strlen (a) : 100;
}

int
main (void)
{
  char b[10];
  strcpy (a + 2, "345");
  strcpy (b + 2, "34567");
  if (f1 (100) != 5 || f2 (b, 100) != 7)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "strlen \\(" 2 "strlen" } } */
