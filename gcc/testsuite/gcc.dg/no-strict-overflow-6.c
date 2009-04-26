/* { dg-do compile } */
/* { dg-options "-fno-strict-overflow -O2 -fdump-tree-optimized" } */

/* Source: Ian Lance Taylor.  */

/* VRP test.  We can not simplify the conditional when not using
   strict overflow semantics.  We don't test this with
   -fstrict-overflow because it turns into an infinite loop.  That is
   OK but it would also be OK to not do that.  */

int
foo ()
{
  int i, bits;
  for (i = 1, bits = 1; i > 0; i += i)
    ++bits;
  return bits;
}

/* { dg-final { scan-tree-dump "return bits" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
