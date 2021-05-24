/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-sink" } */

int foo(int *a, int r)
{
  int ret = 0;
  *a = 1;
  if (r == 3)
    *a = 5;
  else
    ret = r + 20;
  return ret;
}

/* *a = 1 should be sunk to the else block.  */

/* { dg-final { scan-tree-dump-times "Sinking" 1 "sink1" } } */
