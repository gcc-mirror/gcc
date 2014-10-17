/* PR c/63543 */
/* { dg-do compile } */

struct S;
union U;

int
f1 (struct S *s)
{
  return s->a /* { dg-error "dereferencing pointer to incomplete type .struct S." } */
	 + s->b
	 + s->c;
}

int
f2 (union U *u)
{
  return u->a /* { dg-error "dereferencing pointer to incomplete type .union U." } */
	 + u->a
	 + u->a;
}
