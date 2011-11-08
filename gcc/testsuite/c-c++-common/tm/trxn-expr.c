/* { dg-do compile } */
/* { dg-options "-fgnu-tm -fdump-tree-tmmark" } */

int y, x, york;

void foobar(void)
{
  x = y + __transaction_atomic (york);
}

/* { dg-final { scan-tree-dump-times "_ITM_RU.*york" 1 "tmmark" } } */
/* { dg-final { scan-tree-dump-times "_ITM_RU" 1 "tmmark" } } */
/* { dg-final { cleanup-tree-dump "tmmark" } } */
