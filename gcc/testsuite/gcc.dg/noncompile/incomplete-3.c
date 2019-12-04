/* Both occurrences of "c" should get diagnostics.  PR 12391.  */
typedef struct { int a; } b_t;

int foo (void)
{
  b_t d;
  struct b_t *c = &d;	/* { dg-warning "incompatible pointer type" } */
  c->a;			/* { dg-error "invalid use of undefined type" } */
}
