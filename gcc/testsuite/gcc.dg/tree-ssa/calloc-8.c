/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* PR tree-optimization/83022 */

void *combine_alt (int s, bool c, bool *a)
{
  void *r = __builtin_malloc (s);
  if (!r)  return 0;
  if (a) *a = r != 0;
  __builtin_memset (r, 0, s);
  return r;
}

/* These one  should be converted to calloc as the memset is
   is conditional on the ptr nullness.  */

/* { dg-final { scan-tree-dump-times "calloc" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-not "malloc" "optimized" } } */
/* { dg-final { scan-tree-dump-not "memset" "optimized" } } */

