/* Invalid initializations.  */

extern __thread int i;
int *p = &i;	/* { dg-error "initializer element is not constant" } */
