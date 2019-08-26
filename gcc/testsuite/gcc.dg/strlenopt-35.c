/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-strlen" } */

#include "strlenopt.h"

volatile int v;

size_t
f1 (char *a1)
{
  v += 1;
  size_t x = strlen (a1);
  char *a2 = a1 + x;
  a2[0] = '1';
  a2[1] = '2';
  a2[2] = '3';
  a2[3] = 0;
  return strlen (a1);
}

int
main ()
{
  char a[30];
  strcpy (a, "abcd");
  if (f1 (a) != 7)
    abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "strlen \\(" 1 "strlen1" } } */
