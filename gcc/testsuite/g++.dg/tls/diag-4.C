/* Invalid __thread specifiers.  */
/* { dg-require-effective-target tls } */

__thread typedef int g4;	/* { dg-error "multiple storage classes" } */

void foo()
{
  __thread auto int l2;		/* { dg-error "multiple storage classes|data types" } */
  __thread register int l4;	/* { dg-error "multiple storage classes" } */
}				/* { dg-error "ISO C\\+\\+1z does not allow 'register' storage class specifier" "" { target c++1z } .-1 } */
