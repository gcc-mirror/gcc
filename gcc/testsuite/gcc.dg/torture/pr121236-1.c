/* { dg-do compile } */
/* PR tree-optimization/121236 */


unsigned func_26(short *p_27, int gg, int p) {
  unsigned l_184 = 0;
  unsigned m = 0;
  for (int g_59 = 0; g_59 < 10; g_59++)
    {
      if (gg)
	l_184--;
      else
	{
	  m |= l_184 |= p;
	  (l_184)--;
	}
    }
 return m;
}

