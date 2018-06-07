/* PR tree-optimization/84739 */
/* { dg-do compile } */
/* { dg-require-weak "" } */
/* { dg-options "-O2" } */

static void baz (void) __attribute__((weakref("bar")));	/* { dg-warning "alias between functions of incompatible types" } */

int
foo (int x, int y)
{
  if (x)
    y = 0;
  if (y)
    goto lab;
  y = 0;
lab:
  return y;
}

void
bar (int x, int y)	/* { dg-message "aliased declaration here" } */
{
  y = foo (x, y);
  if (y != 0)
    baz ();
}
