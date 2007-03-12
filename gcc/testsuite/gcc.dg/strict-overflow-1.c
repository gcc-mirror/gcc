/* { dg-do compile } */
/* { dg-options "-fstrict-overflow -O2 -fdump-tree-final_cleanup" } */

/* Source: Ian Lance Taylor.  Dual of no-strict-overflow-1.c.  */

/* We can only simplify the conditional when using strict overflow
   semantics.  */

int
foo (int i)
{
  return i - 5 < 10;
}

/* { dg-final { scan-tree-dump-not "-[ ]*5" "final_cleanup" } } */
/* { dg-final { cleanup-tree-dump "final_cleanup" } } */
