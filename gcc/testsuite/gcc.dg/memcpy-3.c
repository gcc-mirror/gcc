/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */
/* { dg-require-effective-target non_strict_align } */

int get_int(const void *p)
{
  int w;
  __builtin_memcpy(&w, p, sizeof (int));
  return w;
}

/* { dg-final { scan-tree-dump-not "memcpy" "optimized" } } */
/* { dg-final { scan-tree-dump-times "MEM" 1 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
