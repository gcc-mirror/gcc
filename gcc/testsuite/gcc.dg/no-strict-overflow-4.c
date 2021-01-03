/* { dg-do compile } */
/* { dg-options "-fno-strict-overflow -O2 -fdump-tree-optimized" } */

/* Source: Ian Lance Taylor.  Dual of strict-overflow-4.c.  */

/* We can only simplify the conditional when using strict overflow
   semantics or when using wrap overflow semantics. -fno-strict-overflow is
   equivalent to -fwrapv.  */

int
foo (int i)
{
  return i + 1 > i;
}

/* { dg-final { scan-tree-dump "\[^ \]*_.(\\\(D\\\))? != \[0-9]+" "optimized" } } */
