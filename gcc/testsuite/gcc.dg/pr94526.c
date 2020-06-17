/* PR middle-end/94526 */
/* { dg-do compile } */
/* { dg-options "-O2 -g" } */

struct S { int val[8 * sizeof (int)]; };

void
foo (struct S *x)
{
  struct S *a = x;
}

void baz (struct S);

void
bar (void)
{
  struct S b;
  foo (&b);
  baz (b);
}
