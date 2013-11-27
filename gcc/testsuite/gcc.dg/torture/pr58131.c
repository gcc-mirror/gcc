/* PR tree-optimization/58131 */
/* { dg-do compile } */

short a;
int b, c, d[1][4][2];

void
foo (void)
{
  int *e;
  for (b = 1; ; b--)
    {
      if (*e)
	break;
      for (c = 2; c >= 0; c--)
	{
	  *e |= d[0][3][b] != a;
	  int *f = &d[0][3][b];
	  *f = 0;
	}
    }
}
