/* { dg-do compile } */

enum
{
  a, b, c, d,  e,  f,  g,  h,  j,  k
};

int l;
void m (short *s)
{
  short n, o, p;
  float(*q)[k];
  int r, i;
  while (l > 0)
    r = l;
  for (;;)
    {
      i = 0;
      for (; i < r; i++)
	{
	    {
	      float ab = q[i][a];
	      int i = ab;
	      p = i;
	    }
	  ((short *) s)[0] = p;
	    {
	      float ab = q[i][b];
	      int i = ab;
	      o = i;
	    }
	  ((short *) s)[1] = o;
	    {
	      float ab = q[i][f];
	      int i = ab;
	      n = i;
	    }
	  ((short *) s)[2] = n;
	  float ab = q[i][g];
	  int i = ab;
	  ((short *) s)[3] = i;
	  s = (short *) s + 4;
	}
    }
}
