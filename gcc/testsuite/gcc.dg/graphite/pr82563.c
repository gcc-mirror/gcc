/* { dg-do compile } */
/* { dg-options "-O3 -floop-nest-optimize" } */

int tj, cw, xf;

void
zp (int *ei)
{
  for (;;)
    {
      int hd = 0;

      if (cw != 0 && xf != 0)
	{
	  for (hd = 0; hd < 3; ++hd)
	    cw = (tj != 0) ? 0 : *ei;
	  for (;;)
	    ;
	}

      while (tj != 0)
	tj = (__UINTPTR_TYPE__)&hd;
    }
}
