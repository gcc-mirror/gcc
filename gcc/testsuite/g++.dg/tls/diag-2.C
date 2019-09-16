/* Invalid __thread specifiers.  */
/* { dg-require-effective-target tls } */

__thread extern int g1;		/* { dg-error "'__thread' before 'extern'" } */
__thread static int g2;		/* { dg-error "'__thread' before 'static'" } */
__thread __thread int g3;	/* { dg-error "duplicate '__thread'" } */
typedef __thread int g4;	/* { dg-error "9:multiple storage classes" } */

void foo()
{
  __thread int l1;		/* { dg-error "3:function-scope .l1. implicitly auto and declared '__thread'" } */
  auto __thread int l2;		/* { dg-error "3:multiple storage classes|data types" } */
  __thread extern int l3;	/* { dg-error "'__thread' before 'extern'" } */
  register __thread int l4;	/* { dg-error "3:multiple storage classes" } */
}				/* { dg-error "ISO C\\+\\+17 does not allow 'register' storage class specifier" "" { target c++17 } .-1 } */

__thread void f1 ();		/* { dg-error "1:storage class .__thread. invalid for function" } */
extern __thread void f2 ();	/* { dg-error "8:storage class .__thread. invalid for function" } */
static __thread void f3 ();	/* { dg-error "8:storage class .__thread. invalid for function" } */
__thread void f4 () { }		/* { dg-error "1:storage class .__thread. invalid for function" } */

void bar(__thread int p1);	/* { dg-error "10:storage class specified for parameter" } */

struct A {
  __thread int i;		/* { dg-error "3:storage class specified" } */
};
