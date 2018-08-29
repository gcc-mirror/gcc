/* { dg-do compile } */
/* { dg-options "-O2 -floop-nest-optimize" } */

short int *ts;

void
c2 (unsigned long long int s4, int ns)
{
  short int *b2 = (short int *)&ns;

  while (ns != 0)
    {
      int xn;

      for (xn = 0; xn < 3; ++xn)
	for (*b2 = 0; *b2 < 2; ++*b2)
	  s4 += xn;
      if (s4 != 0)
	b2 = ts;
      ++ns;
    }
}
