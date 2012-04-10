/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-ccp -fno-tree-dominator-opts -fdump-tree-vrp1" } */

int foo (int x, int b)
{
  int cst;
  if (b)
    cst = -__INT_MAX__ - 1;
  else
    cst = -__INT_MAX__;
  x = x | cst;
  if (x >= 0)
    return 12345;
  return x;
}

int bar (int x, int b)
{
  int cst;
  if (b)
    cst = __INT_MAX__;
  else
    cst = __INT_MAX__ - 1;
  x = x & cst;
  if (x < 0)
    return 12345;
  return x;
}

/* { dg-final { scan-tree-dump-not "12345" "vrp1" } } */
/* { dg-final { cleanup-tree-dump "vrp1" } } */
