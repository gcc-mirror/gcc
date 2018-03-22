/* { dg-do compile } */
/* { dg-options "-fno-strict-overflow -O2 -fdump-tree-optimized" } */

/* Source: Ian Lance Taylor.  Dual of strict-overflow-3.c.  */

/* We can only simplify the conditional when using strict overflow
   semantics.  */

int
foo (int i, int j)
{
  return i + 100 < j + 1234;
}

/* { dg-final { scan-tree-dump "1234" "optimized" } } */
