/* Invalid __thread specifiers.  */
/* { dg-require-effective-target tls } */

__thread extern int g1;		/* { dg-error "'__thread' before 'extern'" } */
__thread static int g2;		/* { dg-error "'__thread' before 'static'" } */
__thread __thread int g3;	/* { dg-error "duplicate '__thread'" } */
typedef __thread int g4;	/* { dg-error "'__thread' used with 'typedef'" } */

void foo()
{
  __thread int l1;		/* { dg-error "implicitly auto and declared '__thread'" } */
  auto __thread int l2;		/* { dg-error "'__thread' used with 'auto'" } */
  __thread extern int l3;	/* { dg-error "'__thread' before 'extern'" } */
  register __thread int l4;	/* { dg-error "'__thread' used with 'register'" } */
}

__thread void f1 ();		/* { dg-error "invalid storage class for function" } */
extern __thread void f2 ();	/* { dg-error "invalid storage class for function" } */
static __thread void f3 ();	/* { dg-error "invalid storage class for function" } */
__thread void f4 () { }		/* { dg-error "function definition declared '__thread'" } */

void bar(__thread int p1);	/* { dg-error "storage class specified for parameter" } */
