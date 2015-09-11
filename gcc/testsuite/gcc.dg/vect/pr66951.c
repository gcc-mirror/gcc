/* { dg-do compile } */

int a, b[16], c, d;

void
fn1 ()
{
  for (; d; d++)
    {
      for (a = 0; a < 2; a++)
	;
      c ^= b[d];
    }
}
