/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* PR tree-optimization/111431 */

int
foo (int a)
{
  int b = a == 1025;
  return (a & b);
}

/* { dg-final { scan-tree-dump-not "return 0"  "optimized" } } */
/* { dg-final { scan-tree-dump-not " & "  "optimized" } } */
/* { dg-final { scan-tree-dump-times " == 1025;" 1  "optimized" } } */
