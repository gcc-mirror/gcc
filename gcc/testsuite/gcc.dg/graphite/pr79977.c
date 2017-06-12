/* { dg-do compile } */
/* { dg-options "-O2 -floop-nest-optimize" } */

int uo[3];
int di;

void
i7 (int mp)
{
  int l4;

wh:
  while (l4 > 1)
    {
      for (di = 0; di < 2; ++di)
	uo[di] = 0;

      for (di = 0; di < 3; ++di)
	{
	  uo[di] = 0;
	  if (mp != 0)
	    goto wh;
	}

      --l4;
    }
}
