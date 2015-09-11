/* { dg-do compile } */
/* { dg-options "-fgnu-tm -O -fdump-tree-tmmark" } */

/* Test that the call to george() doesn't end up inside the transaction.  */

void george (void);
int trxn;

void set_remove(int * val)
{
  __transaction_atomic {
      trxn = 5;
  }
  george();
}

/* { dg-final { scan-tree-dump-times "getTMCloneOrIrrevocable" 0 "tmmark" } } */
