/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int f1(int a, int b)
{
  return (a != b) & ((a | b) != 0);
}

int f2(int a, int b)
{
  return (a == b) | ((a | b) == 0);
}

int f3(int a, int b)
{
  return (a != b) & ((a | b) == 0);
}

int f4(int a, int b)
{
  return (a == b) | ((a | b) != 0);
}

/* { dg-final { scan-tree-dump-times "\\\|" 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\&" 0 "optimized" } } */

/* f3 should fold to `1` (true).  */
/* { dg-final { scan-tree-dump-times "return 1;" 1 "optimized" } } */

/* f4 should fold to `0` (false).  */
/* { dg-final { scan-tree-dump-times "return 0;" 1 "optimized" } } */
