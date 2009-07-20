/* PR tree-optimization/40792 */
/* { dg-do compile } */
/* { dg-options "-O3" } */

void foo (int, char *, int *);

void
bar (int *a, int *b, ...)
{
  int c;
  char d[256];
  foo (*b, d, &c);
}

static int a, b;

void
baz (int c)
{
  bar (&a, &b);
}
