/* PR tree-optimization/113228 */

int a, b, c, d, i;

void
foo (void)
{
  int k[3] = {};
  int *l = &a;
  for (d = 0; c; c--)
    for (i = 0; i <= 9; i++)
      {
	for (b = 1; b <= 4; b++)
	  k[0] = k[0] == 0;
	*l |= k[d];
      }
}
