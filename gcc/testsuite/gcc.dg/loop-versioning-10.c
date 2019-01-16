/* { dg-options "-O3 -fdump-tree-lversion-details" } */

/* Test that we can version a gather-like operation in which a variable
   stride is applied to the index.  */

int
f1 (int *x, int *index, int step, int n)
{
  int res = 0;
  for (int i = 0; i < n; ++i)
    res += x[index[i] * step];
  return res;
}

int
f2 (int *x, int *index, int step, int n)
{
  int res = 0;
  for (int i = 0; i < n; ++i)
    {
      int *ptr = x + index[i] * step;
      res += *ptr;
    }
  return res;
}

int x[1000];

int
g1 (int *index, int step, int n)
{
  int res = 0;
  for (int i = 0; i < n; ++i)
    res += x[index[i] * step];
  return res;
}

int
g2 (int *index, int step, int n)
{
  int res = 0;
  for (int i = 0; i < n; ++i)
    {
      int *ptr = x + index[i] * step;
      res += *ptr;
    }
  return res;
}

/* { dg-final { scan-tree-dump-times {address term [^\n]* \* loop-invariant} 4 "lversion" } } */
/* { dg-final { scan-tree-dump-times {want to version containing loop} 4 "lversion" } } */
/* { dg-final { scan-tree-dump-times {versioned this loop} 4 "lversion" } } */
