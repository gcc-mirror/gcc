/* PR tree-optimization/45047 */

void
foo (const unsigned short *w, char *x, int y, int z)
{
  int i;
  for (i = 0; i < y; i++)
    x[i] = w[i] == z;
}
