/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

#define min(x, y) ((x) < (y) ? (x) : (y))

int
f1 (unsigned int x, unsigned int y)
{
  return min (__builtin_ctz (x), __builtin_ctz (y));
}

int
f2 (unsigned int x, unsigned int y)
{
  return min (__builtin_clz (x), __builtin_clz (y));
}

/* { dg-final { scan-tree-dump-times " \\\| " 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin_ctz|\\.CTZ" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin_clz|\\.CLZ" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-not "MIN_EXPR" "optimized" } } */
