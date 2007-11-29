/* PR tree-optimization/33434 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

void *baz (void);

static void *
bar (void *x)
{
  x = baz ();
  return x;
}

void *
foo (void *x)
{
  return bar (x);
}
