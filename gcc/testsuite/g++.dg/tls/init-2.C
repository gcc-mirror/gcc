/* Invalid initializations.  */

extern __thread int i;
__thread int *p = &i;	/* { dg-error "run-time initialization" } */

extern int f();
__thread int j = f();	/* { dg-error "run-time initialization" } */

struct S
{
  S();
};
__thread S s;		/* { dg-error "run-time initialization" } */
