/* { dg-do compile } */
/* { dg-options "-fgnu-tm -O -fdump-tree-optimized" } */

int jj;

__attribute__((transaction_safe))
static void poof ()
{
  if (jj)
    return;
   poof();
}

__attribute__((transaction_safe))
void TMlist_free ()
{
    poof();
}

/* { dg-final { scan-tree-dump-times "Function poof ._ZGTt4poof" 1 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
