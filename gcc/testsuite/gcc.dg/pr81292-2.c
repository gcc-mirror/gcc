/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-strlen" } */

#include "strlenopt.h"

char a[] = { 0, 'a', 0, 'b', 'c', 0, 'd', 'e', 'f', 0 };

int __attribute__ ((noinline, noclone))
f1 (void)
{
  a[0] = '1';
  a[strlen (a)] = '2';
  a[strlen (a)] = '3';
  return strlen (a);
}

int __attribute__ ((noinline, noclone))
f2 (char *a)
{
  a[0] = '1';
  a[strlen (a)] = '2';
  a[strlen (a)] = '3';
  return strlen (a);
}

int
main (void)
{
  char b[] = { 0, 0, 'a', 'b', 0, 0 };
  if (f1 () != 9 || f2 (b) != 5)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "strlen \\(" 6 "strlen" } } */
