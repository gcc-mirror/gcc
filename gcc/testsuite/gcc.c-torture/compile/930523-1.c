int v;

f ()
{
  unsigned long *a1, *a2;
  int vertex2;
  int c, x1, x2, dx1, dx2, dy1, dy2, e1, e2, s2;
  unsigned long m, b;
  int n;
  unsigned long r;
  int aba;

  do
    {
      if (dx2 >= dy2)
	dx2 = dx2 % dy2;

      if (dx2 >= dy2)
	{
	  s2 = - (dx2 / dy2);
	  dx2 = dx2 % dy2;
	}
    }
  while (vertex2 / 65536);

  for (;;)
    {
      c = x2;
      a2 = a1;
      if (v)
	a2 = 0;

      if (c + n)
	{
	  m = b << (c * 8);
	  *a2 = (*a2 & ~m) | (r & m);
	  n += c;

	  while (--n)
	    {
	      {
	      }
	    }
	}

      a1 = 0;
      x1 += 0;
      if (e1 += dx1)
	e1 -= dy1;
      x2 += s2;
      if (e2 += dx2)
	e2 -= dy2;
    }
}
