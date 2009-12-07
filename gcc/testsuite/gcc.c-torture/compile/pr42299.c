/* { dg-options "-g" } */

static int
foo (int x, int y)
{
  if (y)
    goto lab;
  if (x)
    y = 0;
  if (y)
    goto lab;
  y = 0;
lab:
  return y;
}

void
baz (int x, int y)
{
  y = foo (x, y);
  if (y != 0)
    bar ();
}
