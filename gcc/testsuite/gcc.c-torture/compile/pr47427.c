/* PR tree-optimization/47427 */

char *g, *h;

int
bar (unsigned char x, const int y)
{
lab:
  for (; h; g = h)
    for (g = 0; h; h++)
      {
	int a = 1;
	if (h)
	  {
	    if (a)
	      goto lab;
	    return y;
	  }
      }
  return x;
}

void
foo (void)
{
  if (bar (0, 1))
    bar (1, 0);
}
