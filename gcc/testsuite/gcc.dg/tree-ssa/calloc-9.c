/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* PR tree-optimization/83022 */

void *m (int s, int c)
{
  void *r = __builtin_malloc (s);
  if (r && c)
    __builtin_memset (r, 0, s);
  return r;
}

/* This one should not be converted into calloc as the memset is not
   conditionalized on the pointer being checked for nullness. */

/* { dg-final { scan-tree-dump-not "calloc" "optimized" } } */
/* { dg-final { scan-tree-dump-times "malloc" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "memset" 1 "optimized" } } */

