/* PR tree-optimization/113603 */

int a, e;
signed char b;
int *c;
signed char *d;
short f;
signed char g[3];

int *
foo (void)
{
  for (int i = 0; i < 3; i++)
    g[i] = 2;
  int j[100][100] = { {}, {4} };
  signed char *k = &g[1];
  do
    {
      for (;;)
	{
	  if (c)
	    break;
	  return &a;
	}
      for (f = 0;; f++)
	{
	  for (b = 0; b < 2; b++)
	    *c = j[b][f];
	  if (e)
	    d = k;
	  *k = *d;
	  if (*c)
	    break;
	  if (f)
	    break;
	}
    }
  while (f);
  return 0;
}
