/* { dg-do compile } */
/* { dg-options "-O2 -fsanitize=signed-integer-overflow -fdump-tree-optimized" } */

int
f (int x)
{
  return ((x + 1) & 7) == 0;
}

/* { dg-final { scan-tree-dump "UBSAN_CHECK_ADD" "optimized" } } */
