/* Invalid initializations.  */
/* { dg-require-effective-target tls } */

extern __thread int i;
__thread int *p = &i;	/* { dg-error "dynamic initialization" } */

extern int f();
__thread int j = f();	/* { dg-error "dynamic initialization" } */

struct S
{
  S();
};
__thread S s;		/* { dg-error "dynamic initialization" } */
