/* { dg-do compile } */
/* { dg-options "-O3" } */

extern int abs(int);
int e(const unsigned char *g, long h, unsigned char m)
{
  int i = 0;
  for (int j = 0; j < h; j++)
    {
      for (int k = 0; k < 4; k++)
	i += abs(g[k] - m);
      g += h;
    }
  return i;
}
