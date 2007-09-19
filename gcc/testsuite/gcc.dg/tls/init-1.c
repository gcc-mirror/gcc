/* Invalid initializations.  */
/* { dg-require-effective-target tls } */

extern __thread int i;
int *p = &i;	/* { dg-error "initializer element is not constant" } */
