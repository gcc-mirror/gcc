/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-original" } */

int
foo (int x, int y)
{
  return x - y < 0;
}

/* { dg-final { scan-tree-dump "x < y" "original"  { xfail *-*-* } } } */
