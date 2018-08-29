/* { dg-do compile } */
/* { dg-options "-O -floop-nest-optimize -fno-tree-loop-im" } */

int z4, g7;

void
x3 (int my)
{
  while (my < 2)
    {
      for (z4 = 0; z4 < 2; ++z4)
	{
	}

      if (my != 0)
	for (g7 = 0; g7 < 2; ++g7)
	  {
	  }

      ++my;
    }
}
