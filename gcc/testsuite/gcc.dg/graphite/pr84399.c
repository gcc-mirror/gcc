/* { dg-do compile } */
/* { dg-options "-O2 -floop-nest-optimize -fno-tree-loop-im --param scev-max-expr-size=1" } */

void
h8 (int cv, int od)
{
  for (;;)
    {
      int ih = (__UINTPTR_TYPE__)&od;
      if (cv == 0)
	while (od < 1)
	  {
	    int lq;

	    for (lq = 0; lq < 3; ++lq)
	      for (ih = 0; ih < 4; ++ih)
		od += lq;
	  }
      while (ih < 1)
	{
	}
    }
}
