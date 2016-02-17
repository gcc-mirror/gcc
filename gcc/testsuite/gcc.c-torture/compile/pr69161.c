/* PR target/69161.  */

char a;
int b, c, d, e;

void
foo (void)
{
  int f;
  for (f = 0; f <= 4; f++)
    {
      for (d = 0; d < 20; d++)
	{
	  __INTPTR_TYPE__ g = (__INTPTR_TYPE__) & c;
	  b &= (0 != g) > e;
	}
      e &= a;
    }
}
