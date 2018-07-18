/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=5475" } */

/* This tickles a problem with reload on the m68k.  There's a reasonable
   chance it will get stale over time.  */

int frob;
typedef double SplashCoord;
void transform (SplashCoord xi, SplashCoord yi);
void
arf (SplashCoord x0, SplashCoord y0, SplashCoord x1, SplashCoord y1,
     SplashCoord x2, SplashCoord y2, SplashCoord x3, SplashCoord y3,
     SplashCoord * matrix, SplashCoord flatness2)
{
  SplashCoord cx[(1 << 10) + 1][3];
  SplashCoord cy[(1 << 10) + 1][3];
  SplashCoord xl0, xl1, xl2, xr0, xr1, xr2, xr3, xx1, xx2, xh;
  SplashCoord yl0, yl1, yl2, yr0, yr1, yr2, yr3, yy1, yy2, yh;
  int p1, p2, p3;
  while (p1  < (1 << 10))
    {
      xl0 = cx[p1][0];
      xx2 = cx[p1][2];
      yy2 = cy[p1][2];
      transform (xx2, yy2);
      if (frob)
	{
	  xl1 = (xl0 + xx1);
	  xh = (xx1 + xx2);
	  yl2 = (yl1 + yh);
	  xr2 = (xx2 + xr3);
	  yr2 = (yy2 + yr3) * 0.5;
	  xr1 = (xh + xr2);
	  yr1 = (yh + yr2);
	  xr0 = (xl2 + xr1);
	  yr0 = (yl2 + yr1);
	  cx[p1][1] = xl1;
	  cy[p1][1] = yl1;
	  cx[p1][2] = xl2;
	  cx[p3][0] = xr0;
	  cy[p3][0] = yr0;
	}
    }
}
