/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-additional-options "-mbmi -mlzcnt" { target i?86-*-* x86_64-*-* } } */
/* { dg-require-effective-target clz } */
/* { dg-require-effective-target ctz } */

#define W (__SIZEOF_INT__ * __CHAR_BIT__)
#define min(x, y) ((x) < (y) ? (x) : (y))

int
f1 (unsigned int x, unsigned int y)
{
  return min (__builtin_ctzg (x, W), __builtin_ctzg (y, W));
}

int
f2 (unsigned int x, unsigned int y)
{
  return min (__builtin_clzg (x, W), __builtin_clzg (y, W));
}

/* { dg-final { scan-tree-dump-times " \\\| " 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin_ctz|\\.CTZ" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin_clz|\\.CLZ" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-not "MIN_EXPR" "optimized" } } */
