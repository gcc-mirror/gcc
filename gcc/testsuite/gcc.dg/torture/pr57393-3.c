/* PR middle-end/57393 */
/* { dg-do compile } */

int a, b, c;
void foo (void);

int
bar (void)
{
  for (;;)
    {
      foo ();
      int d = a = 0;
      for (; a < 7; ++a)
	{
	  d--;
	  b &= c <= d;
	}
    }
}
