/* { dg-do compile } */
/* { dg-additional-options "-Wno-aggressive-loop-optimizations" } */

unsigned int a;
int b[1], c, d;

void
fn1 ()
{
  for (; d;)
    {
      a = c = 0;
      for (; c < 5; c++)
	{
	  b[a] ^= 1;
	  a--;
	}
    }
}
