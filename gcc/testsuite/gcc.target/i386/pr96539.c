/* PR rtl-optimization/96539 */
/* { dg-do compile } *
/* { dg-options "-Os" } */
/* { dg-final { scan-assembler-not "rep\[^\n\r]\*movs" } } */

struct A { int a, b, c, d, e, f; void *g, *h, *i, *j, *k, *l, *m; };

int bar (int a);
int baz (int a, int b, int c, void *p, struct A s);

int
foo (int a, int b, int c, void *p, struct A s)
{
  bar (a);
  return baz (a, b, c, p, s);
}
