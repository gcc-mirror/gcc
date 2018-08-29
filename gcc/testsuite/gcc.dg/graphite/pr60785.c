/* { dg-options "-O2 -floop-nest-optimize" } */

static int
aqc(void)
{
  return 1;
}

void
gkd(void)
{
  int wu0;
  static int b1y;
  static int gw2;
  static int *ydw = &gw2;
  static int **m3l = &ydw;
  **m3l = 0;
  for (b1y = 0; b1y < 1; ++b1y)
    {
      int *cpj = &gw2;
      if (*ydw |= aqc())
	{
	  *cpj = 0;
	  *ydw = wu0;
	}
    }
}
