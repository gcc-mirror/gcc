/* { dg-do compile } */
/* { dg-additional-options "-ftree-vectorize" } */

unsigned int a, c, d;
double b;
void e()
{
  for (; d; d++)
    {
      double f;
      a = 2;
      for (; a; a++)
	{
	  c = b;
	  b = f;
	  f = c;
	}
    }
}
