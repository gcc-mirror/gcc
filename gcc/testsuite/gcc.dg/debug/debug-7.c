/* { dg-do compile } */
/* { dg-options "-dA" } */
/* PR debug/12934.  */

static inline int foo ()
{
  return 42;
}

void bar (int *);

void baz ()
{
  int a[foo ()];
  bar (a);
}
