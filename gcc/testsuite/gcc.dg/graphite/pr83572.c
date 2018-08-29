/* { dg-do compile } */
/* { dg-options "-O -floop-nest-optimize -fno-tree-loop-im" } */

int u0, l1;

void
u3 (int s1)
{
  for (;;)
    {
      for (u0 = 0; u0 < 2; ++u0)
	{
	}

      if (s1 != 0)
	for (l1 = 0; l1 < 2; ++l1)
	  {
	  }

      l1 = 0;
    }
}
