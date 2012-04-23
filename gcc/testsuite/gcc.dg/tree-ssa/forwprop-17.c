/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */

int foo (int xx, int xy)
{
  xx &=1;
  xy &=1;
  return xx ^ xy;
}

/* { dg-final { scan-tree-dump-times " & 1" 1 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
