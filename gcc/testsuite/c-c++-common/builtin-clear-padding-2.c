/* PR middle-end/97943 */
/* { dg-do compile } */
/* { dg-options "" } */

struct S { int a; char b[] __attribute__((aligned (2 * sizeof (int)))); };
struct T { int a; struct S b; };
union U { int a; struct S b; };
struct V { int a; union U b; };

void
foo (struct S *s, struct T *t, union U *u, struct V *v)
{
  __builtin_clear_padding (s);	/* { dg-error "flexible array member '(S::)?b' does not have well defined padding bits for '__builtin_clear_padding'" } */
  __builtin_clear_padding (t);	/* { dg-error "flexible array member '(S::)?b' does not have well defined padding bits for '__builtin_clear_padding'" } */
  __builtin_clear_padding (u);	/* { dg-error "flexible array member '(S::)?b' does not have well defined padding bits for '__builtin_clear_padding'" } */
  __builtin_clear_padding (v);	/* { dg-error "flexible array member '(S::)?b' does not have well defined padding bits for '__builtin_clear_padding'" } */
}
