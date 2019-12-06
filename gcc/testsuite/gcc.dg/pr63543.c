/* PR c/63543 */
/* { dg-do compile } */

struct S;
union U;

int
f1 (struct S *s)
{
  return s->a /* { dg-error "invalid use of undefined type .struct S." } */
	 + s->b /* { dg-error "invalid use of undefined type .struct S." } */
	 + s->c; /* { dg-error "invalid use of undefined type .struct S." } */
}

int
f2 (union U *u)
{
  return u->a /* { dg-error "invalid use of undefined type .union U." } */
	 + u->a /* { dg-error "invalid use of undefined type .union U." } */
	 + u->a; /* { dg-error "invalid use of undefined type .union U." } */
}
