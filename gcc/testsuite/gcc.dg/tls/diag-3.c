/* Report invalid extern and __thread combinations.  */

extern int j;		/* { dg-error "previous declaration" } */
__thread int j;		/* { dg-error "thread-local declaration for" } */

extern __thread int i;	/* { dg-error "previous declaration" } */
int i;			/* { dg-error "non thread-local" } */

extern __thread int k;	/* This is fine.  */
__thread int k;
