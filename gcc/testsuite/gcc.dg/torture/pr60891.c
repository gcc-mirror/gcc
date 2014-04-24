/* { dg-do compile } */
/* { dg-additional-options "-fno-tree-ch -fno-tree-cselim -fno-tree-dominator-opts" } */

int a, b, c, d, e, f;

void foo (int x)
{
  for (;;)
    {
      int g = c;
      if (x)
	{
	  if (e)
	    while (a)
	      --f;
	}
      for (b = 5; b; b--)
	{
	}
      if (!g)
	x = 0;
    }
}
