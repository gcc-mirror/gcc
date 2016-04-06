/* PR tree-optimization/70177 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

int b[128];

void
foo (int i, int j)
{
  int c, f, g, h;
  for (g = 0; g < 64; g++)
    for (h = g, f = 0; f <= i; f++, h++)
      for (c = 0; c < j; c++)
	b[h] = 0;
}
