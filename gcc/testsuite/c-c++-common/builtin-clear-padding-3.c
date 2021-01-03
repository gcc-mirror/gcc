/* PR middle-end/97943 */
/* { dg-do compile } */
/* { dg-options "" } */

union U { int a; char b[] __attribute__((aligned (2 * sizeof (int)))); };	/* { dg-error "flexible array member in union" } */
struct V { int a; union U b; };
struct W { int a; union U b; int c; };

void
foo (union U *u, struct V *v, struct W *w)
{
  __builtin_clear_padding (u);
  __builtin_clear_padding (v);
  __builtin_clear_padding (w);
}
