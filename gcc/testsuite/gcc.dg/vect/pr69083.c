/* PR tree-optimization/69083 */
/* { dg-do compile } */
/* { dg-additional-options "-O3" } */

int d;
short f;

void
foo (int a, int b, int e, short c)
{
  for (; e; e++)
    {
      int j;
      for (j = 0; j < 3; j++)
	{
	  f = 7 >> b ? a : b;
	  d |= c == 1 ^ 1 == f;
	}
    }
}
