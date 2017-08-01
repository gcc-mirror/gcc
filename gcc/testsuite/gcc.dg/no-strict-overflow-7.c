/* { dg-do compile } */
/* { dg-options "-fno-strict-overflow -O2 -fdump-tree-optimized" } */

/* Source: Ian Lance Taylor.  Dual of strict-overflow-6.c.  */

/* We can simplify the conditional because pointer overflow always has
   undefined semantics.  */

int
foo (char* p)
{
  return p + 1000 < p;
}

/* { dg-final { scan-tree-dump "return 0" "optimized" } } */
