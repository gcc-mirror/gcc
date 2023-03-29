/* PR tree-optimization/109215 */
/* { dg-do compile } */
/* { dg-options "-O2 -Wall" } */

struct S {};
struct T { struct S s[3]; struct S t; };
void foo (struct S *);

void
bar (struct T *t)
{
  foo (&t->s[2]);	/* { dg-bogus "array subscript 2 is outside the bounds of an interior zero-length array" } */
}

void
baz (struct T *t)
{
  foo (&t->s[3]);	/* { dg-error "" "" { xfail *-*-* } } */
}
