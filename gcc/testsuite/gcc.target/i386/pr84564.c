/* PR target/84564 */
/* { dg-do compile } */
/* { dg-options "-O2 -mforce-indirect-call" } */

int a, b, c, d;
int foo (void);

static int
bar (int x, int y, int z)
{
  while (a)
    if (foo ())
      bar (x, y, z);
  return 0;
}

int
baz (void)
{
  return bar (b, c, d);
}
