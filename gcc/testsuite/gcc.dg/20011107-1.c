/* { dg-do compile { target i?86-*-* } } */
/* { dg-options "-O2 -mcpu=k6" } */

void
foo (unsigned char *x, const unsigned char *y)
{
  int a = 6;
  unsigned char *b;
  for (;;)
    {
      unsigned char *c = x;

      while (1)
	{
	  if (c + 2 < y)
	    c += 3;
	  else
	    break;
	}
      b = x + a;
      if (*c == 4 || *c == 5)
	{
	  unsigned char d = c[2];

	  if (b[3] == 7 || b[3] == 8)
	    {
	      int e = b[3] == 8;
	      if (d < b[4] * 8 && b[5 + d / 8] & (1 << (d % 8)))
		e = !e;
	      if (!e)
		x[-3] = 26;
	    }
	}
      else if (*c == 7 && b[3] == 8)
	{
	  int f;
	  for (f = 0; f < (int) c[1]; f++)
	    if (!(c[2 + f] == 0))
	      break;
	  if (f == c[1])
	    x[-3] = 26;
	}
      x -= 2;
    }
}
