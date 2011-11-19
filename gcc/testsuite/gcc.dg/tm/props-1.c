/* { dg-do compile } */
/* { dg-options "-fgnu-tm -fdump-tree-tmedge -fdump-tree-tmlower" } */

int global;

foo(int local)
{
  __transaction_atomic {
    local++;
    if (++global == 10)
      __transaction_cancel;
  }
}

/* { dg-final { scan-tree-dump-times " instrumentedCode" 1 "tmedge" } } */
/* { dg-final { scan-tree-dump-times "hasNoAbort" 0 "tmedge" } } */
/* { dg-final { scan-tree-dump-times "GTMA_HAVE_ABORT" 1 "tmlower" } } */
/* { dg-final { cleanup-tree-dump "tmedge" } } */
/* { dg-final { cleanup-tree-dump "tmlower" } } */
