/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* PR tree-optimization/83022 */

void* f(int n, int *q, int t)
{
  int *p = __builtin_malloc (n);
  for(int i = 0; i < n; i++)
    q[i] = i;
  if (t)
    __builtin_memset (p, 0, n);
  return p;
}

void* g(int n, int *q, int t)
{
  int *p = __builtin_malloc (n);
  if (t)
    __builtin_memset (p, 0, n);
  return p;
}

/* These 2 should not be converted into calloc as the memset is not
   conditionalized on the pointer being checked for nullness. */

/* { dg-final { scan-tree-dump-not "calloc" "optimized" } } */
/* { dg-final { scan-tree-dump-times "malloc" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times "memset" 2 "optimized" } } */

