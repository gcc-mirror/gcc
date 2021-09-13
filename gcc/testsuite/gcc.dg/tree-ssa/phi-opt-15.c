/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int
foo (int i)
{
  if (i > 0)
    i = -i;
  return i;
}

/* We should not have ABS_EXPR but ABSU_EXPR instead. */
/* { dg-final { scan-tree-dump-not "ABS_EXPR" "optimized" } } */
/* { dg-final { scan-tree-dump "ABSU" "optimized" } } */
