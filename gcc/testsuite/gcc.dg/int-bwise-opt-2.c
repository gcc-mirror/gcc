/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int f1(int a, int b)
{
  return (a != b) | ((a | b) != 0);
}

int f2(int a, int b)
{
  return (a == b) & ((a | b) == 0);
}

 /* { dg-final { scan-tree-dump-times "a == b" 0 "optimized" } } */
 /* { dg-final { scan-tree-dump-times "a != b" 0 "optimized" } } */
