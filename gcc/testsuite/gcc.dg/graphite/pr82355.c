/* { dg-do compile } */
/* { dg-options "-O3 -floop-nest-optimize" } */

int dc, at;

void
tv (int *ld, int jl)
{
  for (;;)
    {
      if (dc != 0)
	for (;;)
	  {
	    *ld = !!(*ld) + 1;
	    for (dc = 0; dc < 3; ++dc)
	      at = (jl != 0) ? *ld : 0;
	  }

      while (at != 0)
	{
	}
    }
}
