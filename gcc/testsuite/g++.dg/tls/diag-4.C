/* Invalid __thread specifiers.  */

__thread typedef int g4;	/* { dg-error "multiple storage classes" } */

void foo()
{
  __thread auto int l2;		/* { dg-error "multiple storage classes" } */
  __thread register int l4;	/* { dg-error "multiple storage classes" } */
}
