/* PR middle-end/37014 */

void bar (signed char *);

void
foo (int x, int y)
{
  int i;
  signed char a[123], b[123], c;
  for (i = 0; i < 123; i++)
    {
      int e = y - x;
      int d = e < 0 ? -e : e;
      c = d < 75;
      a[y] = c;
      b[y] = c;
      y--;
    }
  bar (b);
  bar (a);
}
