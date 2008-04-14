/* { dg-do compile } */
/* { dg-options "-fno-strict-overflow -O2 -fdump-tree-final_cleanup" } */

/* Source: Ian Lance Taylor.  Dual of strict-overflow-6.c.  */

/* We can only simplify the conditional when using strict overflow
   semantics.  */

int
foo (char* p)
{
  return p + 1000 < p;
}

/* { dg-final { scan-tree-dump "\[+\]\[ \]*1000" "final_cleanup" } } */
/* { dg-final { cleanup-tree-dump "final_cleanup" } } */
