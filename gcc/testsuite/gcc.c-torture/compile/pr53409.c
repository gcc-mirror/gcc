/* PR tree-optimization/53409 */

int a, c, d, e, f;
int b[0];

int
main ()
{
  if (f)
    e = 0;
  int g = d;
  for (c = 0; c <= 1; c++)
    {
      for (a = 0; a <= 1; a = (char) a + 1)
	b[c] = g;
      a = 0;
    }
  return 0;
}
