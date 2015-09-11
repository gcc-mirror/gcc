/* PR tree-optimization/65875 */

int a, b, c, d, e, f, g;

void
foo (void)
{
  long h = 0, i;
  if (g < 0)
    i = -g;
  for (; b;)
    for (; c;)
      if (e)
	h = 1;
  for (; f;)
    if (a)
      break;
  if (h > i)
    while (h > i)
      {
	d = 0;
	h--;
      }
}
