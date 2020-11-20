/* PR libstdc++/88101 */
/* { dg-do compile } */

struct S;
struct T { char a; long long b; };

void
foo (struct S *p, void *q, char *r, const struct T *s)
{
  __builtin_clear_padding ();		/* { dg-error "too few arguments to function '__builtin_clear_padding'" } */
  __builtin_clear_padding (1);		/* { dg-error "argument 1 in call to function '__builtin_clear_padding' does not have pointer type" } */
  __builtin_clear_padding (&p);
  __builtin_clear_padding (&p, 1);	/* { dg-error "too many arguments to function '__builtin_clear_padding'" } */
  __builtin_clear_padding (&p, &p);	/* { dg-error "too many arguments to function '__builtin_clear_padding'" } */
  __builtin_clear_padding (p);		/* { dg-error "argument 1 in call to function '__builtin_clear_padding' points to incomplete type" } */
  __builtin_clear_padding (q);		/* { dg-error "argument 1 in call to function '__builtin_clear_padding' points to incomplete type" } */
  __builtin_clear_padding (r);
  __builtin_clear_padding (s);		/* { dg-error "argument 1 in call to function '__builtin_clear_padding' has pointer to 'const' type" } */
}
