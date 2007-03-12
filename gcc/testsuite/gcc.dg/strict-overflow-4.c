/* { dg-do compile } */
/* { dg-options "-fstrict-overflow -O2 -fdump-tree-final_cleanup" } */

/* Source: Ian Lance Taylor.  Dual of no-strict-overflow-4.c.  */

/* We can only simplify the conditional when using strict overflow
   semantics.  */

int
foo (int i)
{
  return i + 1 > i;
}

/* We expect to see "<bb N>"; confirm that, so that we know to count
   it in the real test.  */
/* { dg-final { scan-tree-dump-times "<bb\[^>\]*>" 1 "final_cleanup" } } */
/* { dg-final { scan-tree-dump-times ">|<" 2 "final_cleanup" } } */
/* { dg-final { cleanup-tree-dump "final_cleanup" } } */
