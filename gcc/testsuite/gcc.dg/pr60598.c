/* PR target/60598 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-fpic" { target fpic } } */
/* { dg-additional-options "-march=z196 -mtune=zEC12" { target s390*-*-* } } */

struct S { unsigned a, b[32]; };

void
foo (struct S *x, struct S *y)
{
  unsigned a = y->a, i;
  if (x == y)
    for (i = 0; i < a - 1 - i; i++)
      {
	unsigned t = x->b[i];
	x->b[i] = x->b[a - 1 - i];
	x->b[a - 1 - i] = t;
      }
  else
    {
      x->a = a;
      for (i = 0; i < a; i++)
	x->b[i] = y->b[a - 1 - i];
    }
}
