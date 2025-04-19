/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* PR tree-optimization/83022 */

void* f(int n, int *q)
{
  int *p = __builtin_malloc (n);
  for(int i = 0; i < n; i++)
    q[i] = i;
  if (p)
    __builtin_memset (p, 0, n);
  return p;
}

void* g(int n, int *q)
{
  int *p = __builtin_malloc (n);
  for(int i = 0; i < n; i++)
    q[i] = i;
  __builtin_memset (p, 0, n);
  return p;
}

/* These 2 should be converted to calloc as the memset is
   always executed or is conditional on the ptr nullness.  */

/* { dg-final { scan-tree-dump-times "calloc" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-not "malloc" "optimized" } } */
/* { dg-final { scan-tree-dump-not "memset" "optimized" } } */

