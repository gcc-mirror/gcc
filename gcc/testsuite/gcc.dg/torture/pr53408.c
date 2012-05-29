/* { dg-do compile } */

int a, b, c, d, e;
void
fn1 ()
{
  int f, g;
  char h = 0;
  b = 0;
  for (; b < 32; b++)
    {
      g = h > e ? h : h << 1;
      f = g && a ? 0 : 1;
      h = 1;
      for (; h > 0; h = h + 1)
	c = 0 < h | f;
    }
  if (h)
    d = 0;
}
