/* PR c/96545 */
/* { dg-do compile } */

extern char x[], y[], z[];
struct S;
extern struct S s, t, u;
int v, w;

void
foo (void)
{
  __atomic_exchange (&x, &y, &z, 0);	/* { dg-error "must be a pointer to a complete type" } */
}

void
bar (void)
{
  __atomic_exchange (&s, &t, &u, 0);	/* { dg-error "must be a pointer to a complete type" } */
}

void
baz (void)
{
  __atomic_exchange (&v, &t, &w, 0);	/* { dg-error "size mismatch in argument 2 of" } */
}

void
qux (void)
{
  __atomic_exchange (&v, &w, &t, 0);	/* { dg-error "size mismatch in argument 3 of" } */
}
