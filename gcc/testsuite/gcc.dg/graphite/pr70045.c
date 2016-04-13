/* { dg-do compile } */
/* { dg-options "-O2 -floop-interchange" } */

int a, b, d, e, f;
int c[9];
void
fn1 ()
{
  e = 1;
  for (; e >= 0; e--)
    {
      d = 1;
      for (; d >= 0; d--)
	{
	  f = 0;
	  for (; f <= 1; f++)
	    {
	      a = 0;
	      for (; a < 9; a++)
		{
		  b = 0;
		  for (; b < 2; b++)
		    c[a + b] = 3;
		}
	    }
	}
    }
}
