/* PR middle-end/57393 */
/* { dg-do compile } */

char a;

void
foo (int **p)
{
  int b;
  for (;;)
    {
      int c[1] = { 0 };
      unsigned *d = &c[0];
      for (b = 7; b; b--)
	**p &= --*d >= a;
    }
}
