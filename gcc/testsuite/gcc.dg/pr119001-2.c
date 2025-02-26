/* PR c/119001 */
/* { dg-do compile } */
/* { dg-options "" } */

union U { char a[]; int i; };
union U u[1] = { { "12345" } };			/* { dg-error "initialization of flexible array member in a nested context" } */
union U v[1] = { { .a = "6789" } };		/* { dg-error "initialization of flexible array member in a nested context" } */
union U w[1] = { { { 1, 2, 3, 4, 5, 6 } } };	/* { dg-error "initialization of flexible array member in a nested context" } */
union U x[1] = { { .a = { 7, 8, 9 } } };	/* { dg-error "initialization of flexible array member in a nested context" } */
union V { int i; char a[]; };
union V y[1] = { { .a = "6789" } };		/* { dg-error "initialization of flexible array member in a nested context" } */
union V z[1] = { { .a = { 7, 8, 9 } } };	/* { dg-error "initialization of flexible array member in a nested context" } */

void
foo (int x)
{
  union U a = { { x, x + 1 } };			/* { dg-error "non-static initialization of a flexible array member" } */
  union U b = { .a = { x + 2, x + 3 } };	/* { dg-error "non-static initialization of a flexible array member" } */
  union V c = { .a = { x + 4, x + 5 } };	/* { dg-error "non-static initialization of a flexible array member" } */
}
