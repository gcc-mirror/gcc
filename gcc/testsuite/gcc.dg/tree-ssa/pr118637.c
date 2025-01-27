/* PR tree-optimization/118637 */
/* { dg-do compile { target clz } } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "__builtin_clz|\\.CLZ" 2 "optimized" } } */

__attribute__((noipa)) unsigned
foo (unsigned x)
{
  unsigned result = 0;
  while (x /= 2)
    ++result;
  return result;
}

__attribute__((noipa)) unsigned
bar (unsigned x)
{
  unsigned result = 0;
  while (x >>= 1)
    ++result;
  return result;
}
