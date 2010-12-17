/* Invalid initializations.  */
/* { dg-require-effective-target tls } */

extern __thread int i;
__thread int *p = &i;	/* { dg-error "initializer element is not constant" } */

extern int f();
__thread int j = f();	/* { dg-error "initializer element is not constant" } */

struct S
{
  S(); 			/* { dg-error "expected specifier-qualifier-list before 'S'" } */
};
__thread S s;		/* { dg-error "unknown type name" } */
