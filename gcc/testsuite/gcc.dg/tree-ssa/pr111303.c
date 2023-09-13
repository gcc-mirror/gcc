/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

typedef unsigned int INT;

INT
foo (INT x, INT y)
{
  if (x > 100 || y > 100)
    return x;
  return (x * y) / y;
}

/* { dg-final { scan-tree-dump-times "return x_..D." 1 "optimized"} } */
/* { dg-final { scan-tree-dump-times " / " 0 "optimized"} } */
