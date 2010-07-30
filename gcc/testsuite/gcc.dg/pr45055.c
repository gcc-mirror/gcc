/* PR debug/45055 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftracer -fsched-pressure -funroll-loops -fschedule-insns -fcompare-debug" } */

int colormap[10];

extern int bar ();

void
foo (int *img, int fp, int y, int *ptr, int depth, int c, int t, int xm)
{
  int x, color, count;
  for (; y; y--)
    {
      if (depth)
	{
	  count = bar ();
	  for (x = xm; x; x--)
	    {
	      if (c != 1)
		count = color = -1;
	      if (count == 0)
		color = count = bar ();
	      if (color)
		t = bar (fp);
	      *ptr++ = colormap[t];
	    }
	}
      switch (*img)
	{
	case 1:
	  bar ();
	case 3:
	case -1:
	case -3:
	  bar ();
	case -4:
	  bar ();
	}
    }
}
