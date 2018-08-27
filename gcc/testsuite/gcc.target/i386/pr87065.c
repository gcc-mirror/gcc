/* PR rtl-optimization/87065 */
/* { dg-do compile } */
/* { dg-options "-O3 -mxop -mprefer-vector-width=128" } */

int a, c, d, e;
short *b;

void
foo (void)
{
  short *g = b;
  int h = 1;
  unsigned i;
  for (; h <= 1; h++)
    g = (short *) &c;
  for (; c; c++)
    {
      for (; i <= 1; i++)
	;
      a ^= (a > 0 <= i) + ((e += d) == 0 ?: (*g = 8));
    }
}
