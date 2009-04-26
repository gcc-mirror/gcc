/* { dg-do compile } */
/* { dg-options "-fstrict-overflow -O2 -fdump-tree-optimized" } */

/* Source: Ian Lance Taylor.  Dual of no-strict-overflow-2.c.  */

/* We can only simplify the division when using strict overflow
   semantics.  */

int
foo (int i)
{
  return (i * 100) / 10;
}

/* { dg-final { scan-tree-dump-not "100" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
