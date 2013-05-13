/* PR tree-optimization/57230 */
/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-strlen" } */

#include "strlenopt.h"

int
main ()
{
  char p[] = "hello world";
  if (strlen (p) != 11)
    abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "strlen \\(" 0 "strlen" } } *
/* { dg-final { cleanup-tree-dump "strlen" } } */
