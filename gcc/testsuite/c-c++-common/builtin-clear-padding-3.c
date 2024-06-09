/* PR middle-end/97943 */
/* { dg-do compile } */
/* { dg-options "" } */

union U { int a; char b[] __attribute__((aligned (2 * sizeof (int)))); };
struct V { int a; union U b; };

void
foo (union U *u, struct V *v)
{
  __builtin_clear_padding (u); /* { dg-error "flexible array member" "does not have well defined padding bits" } */
  __builtin_clear_padding (v); /* { dg-error "flexible array member" "does not have well defined padding bits" } */
}
