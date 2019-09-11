/* PR tree-optimization/23929 */
/* { dg-require-stack-size "2048+8" } */

extern void bar (char *);

void
foo (int n, char *z)
{
  char b[2048];
  int x, y;

  bar (b);
  for (y = 0; y < 60; y++)
    if (n == 600)
      for (x = 0; x < 320;)
	{
	  *z++ = b[x];
	  x += 1;
	  *z++ = b[x];
	  x += 1;
	}
}
