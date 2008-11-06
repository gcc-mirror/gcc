/* { dg-do compile } */
/* { dg-options "-funswitch-loops" } */

void foo(double);
void CreateDefaultTexture(double mnMinimum, double mnMaximum,
			  unsigned short nCreateWhat)
{
  double d = 0.0;
  for(;;)
    {
      if(nCreateWhat & (0x0001)
	 && mnMinimum != 0.0)
	d = mnMinimum;
      if(nCreateWhat & (0x0002)
	 && mnMaximum != 0.0)
	d = mnMaximum;
      foo(d);
    }
}

