/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-pre-stats" } */

int foo (int flag, int * __restrict a, int * __restrict b)
{
  int res;
  if (flag)
    res = *a + *b;
  else
    {
      res = *a;
      *a = 1;
      res += *b;
    }
  return res;
}

/* { dg-final { scan-tree-dump "HOIST inserted: 3" "pre" } } */
/* { dg-final { scan-tree-dump-times " = \\\*" 2 "pre" } } */
/* { dg-final { scan-tree-dump-times " = \[^\r\n\]* \\\+ \[^\r\n\]*;" 1 "pre" } } */
