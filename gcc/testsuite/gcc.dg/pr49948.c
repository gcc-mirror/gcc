/* PR tree-optimization/49948 */
/* { dg-do compile } */
/* { dg-options "-O3 -ftree-parallelize-loops=2" } */

extern int a, *b;
int
foo (void)
{
  int c, d = 8, *e[8], i;
  if (a <= 7)
    {
      for (i = 0; i < 8; ++i)
	e[i] = &c;
      while (--d)
	{
	  a = 0;
	  b = e[0];
	}
      return 0;
    }
  return b == &d;
}
