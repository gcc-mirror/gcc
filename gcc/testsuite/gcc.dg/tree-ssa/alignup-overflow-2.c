/* { dg-do compile } */
/* { dg-options "-O2 -fsanitize=signed-integer-overflow -fdump-tree-optimized" } */

int
f (int x)
{
  return x + ((-x) & 15);
}

/* { dg-final { scan-tree-dump "UBSAN_CHECK_SUB" "optimized" } } */
