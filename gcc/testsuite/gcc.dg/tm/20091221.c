/* { dg-do compile } */
/* { dg-options "-fgnu-tm -fdump-tree-tmedge" } */

int i;
extern void virgin () __attribute__((transaction_pure));

foo()
{
	__transaction_atomic {
	    virgin(i);
	}
}

/* { dg-final { scan-tree-dump-times "readOnly" 1 "tmedge" } } */
/* { dg-final { cleanup-tree-dump "tmedge" } } */
