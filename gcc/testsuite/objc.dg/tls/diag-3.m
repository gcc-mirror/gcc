/* Report invalid extern and __thread combinations. */
/* { dg-require-effective-target tls } */

extern int j;		/* { dg-message "previous declaration of 'j' was here" } */
__thread int j;		/* { dg-error "follows non-thread-local" } */

extern __thread int i;	/* { dg-message "previous declaration of 'i' was here" } */
int i;			/* { dg-error "follows thread-local" } */

extern __thread int k;	/* This is fine. */
__thread int k;
