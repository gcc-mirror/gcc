/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-pre" } */

int bar (int);

int z;

void
foo (int y)
{
  if (y == 6)
    z = 5;
  else
    z = 5;
}

/* { dg-final { scan-tree-dump-times "z = 5" 1 "pre"} } */
/* { dg-final { cleanup-tree-dump "pre" } } */
