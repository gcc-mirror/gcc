// { dg-do compile }
// { dg-options "-fgnu-tm -O -fdump-tree-tmmark" }

int global;

int f2()
{
  return __transaction_atomic (global + 3)
         + __transaction_atomic (global + 4);
}

/* { dg-final { scan-tree-dump-times "ITM_RU" 2 "tmmark" } } */
/* { dg-final { scan-tree-dump-times "ITM_commitTransaction" 4 "tmmark" } } */
/* { dg-final { cleanup-tree-dump "tmmark" } } */
