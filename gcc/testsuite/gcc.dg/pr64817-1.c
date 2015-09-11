/* PR debug/64817 */
/* { dg-do compile } */
/* { dg-options "-O3 -g" } */

int a, b, d;

void
foo (void)
{
  for (b = 0; b < 9; b++)
    {
      int e;
      for (d = 0; d < 5; d++)
	{
	  a &= 231;
	  a ^= 14;
	}
      e = (a ^= 1) < 0;
    }
}
