/* PR tree-optimization/123319 */
/* { dg-do compile { target int32plus } } */
/* { dg-options "-O3" } */

signed char b, c[4];
void foo (void);

void
bar (long e)
{
  if (e) {
    if ((short) e)
      for (;;)
        ;
    foo ();
    b = c[e];
  }
}

static void
baz (long e)
{
  bar (e);
}

int g;

void
qux ()
{
  bar (g);
  baz (1);
  int h = 2558744285;
  baz (h);
}
