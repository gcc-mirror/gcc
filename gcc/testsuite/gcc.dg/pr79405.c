/* PR rtl-optimization/79405 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

char cz;
long long int xx, u2;

void
qv (int js, int wl)
{
  if (js != 0)
    {
      short int sc;
      int *at = (int *)&sc;
      long long int gx = 0;

      for (;;)
	{
	  *at = 0;
	  js /= sc;

	  for (wl = 0; wl < 2; ++wl)
	    {
	      xx = gx;
	      u2 %= xx > 0;
	      cz /= u2;

 fa:
	      if (cz != u2)
		{
		  gx |= js;
		  cz = gx / js;
		}
	    }
	}

 yq:
      wl /= 0x80000000;
      u2 = wl;
      u2 |= (wl != 0) | (wl != 0 && gx != 0);
      js = u2;
      goto fa;
    }
  goto yq;
}
