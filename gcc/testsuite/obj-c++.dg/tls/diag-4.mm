/* Invalid __thread specifiers.  */
/* { dg-require-effective-target tls } */

__thread typedef int g4;	/* { dg-error "multiple storage classes in declaration of" } */

void foo()
{
  __thread auto int l2;		/* { dg-error "multiple storage classes in declaration of" } */
  __thread register int l4;	/* { dg-error "multiple storage classes in declaration of" } */
}
