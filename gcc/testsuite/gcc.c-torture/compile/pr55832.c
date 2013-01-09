/* PR tree-optimization/55832 */

int g, b;

void
foo (void)
{
  union U { int i; unsigned short s; } a = { 0 };
  unsigned char c;
  unsigned short d = 0, *p = &a.s;

  if (g)
    a.i--;

  if (b && a.i < (d = 1))
    return;

  for (; a.i < 15; a.i++)
    b |= d <= c;

  if (!*p)
    g = 0;
}
