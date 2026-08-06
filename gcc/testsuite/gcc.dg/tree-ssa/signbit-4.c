/* { dg-do compile } */
/* { dg-options "-O2 -fsanitize=signed-integer-overflow -fdump-tree-optimized" } */

int
f (int x)
{
  return x + (x | -x);
}

/* { dg-final { scan-tree-dump "\\.UBSAN_CHECK_SUB" "optimized" } } */
/* { dg-final { scan-tree-dump "\\.UBSAN_CHECK_ADD" "optimized" } } */
