/* PR middle-end/89655 */

int a, b, d;
char *c;

void
foo (void)
{
  int f = a;
  for (;;)
    {
      for (f = 0; f < (a > 3 ? : a); f++)
	b = c[f] ? c[(f + 2 > a - 1 ? a - 1 : 2) * d] : 0;
    }
}
