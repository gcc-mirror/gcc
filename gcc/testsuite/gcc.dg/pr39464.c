/* PR c/39464 */
/* { dg-do compile } */
/* { dg-options "-Wall" } */

typedef int T __attribute__((may_alias));
typedef unsigned int U __attribute__((may_alias));

void
foo (void *p)
{
  T *a = (int *) p;		/* { dg-warning "initialization from incompatible pointer type" } */
  int *b = (T *) p;		/* { dg-warning "initialization from incompatible pointer type" } */
  U *c = (unsigned int *) p;	/* { dg-warning "initialization from incompatible pointer type" } */
  unsigned int *d = (U *) p;	/* { dg-warning "initialization from incompatible pointer type" } */
  (void) a;
  (void) b;
  (void) c;
  (void) d;
}
