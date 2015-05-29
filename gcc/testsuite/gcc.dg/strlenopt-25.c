/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-strlen" } */

#include "strlenopt.h"

int
main ()
{
  char p[] = "foobar";
  int len, len2;
  len = strlen (p);
  p[0] = 'O';
  len2 = strlen (p);
  return len - len2;
}

/* { dg-final { scan-tree-dump-times "strlen \\(" 0 "strlen" } } */
