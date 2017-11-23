/* { dg-do compile } */
/* { dg-require-effective-target pthread } */
/* { dg-options "-O2 -ftree-loop-if-convert -ftree-parallelize-loops=2" } */

int rl, s8;

void
it (int zy, short int el)
{
  int hb;

  while (el != 0)
    {
      hb = el;
      for (rl = 0; rl < 200; ++rl)
	{
	  for (s8 = 0; s8 < 2; ++s8)
	    {
	    }
	  if (s8 < 3)
	    zy = hb;
	  if (hb == 0)
	    ++s8;
	  zy += (s8 != -1);
	}
      el = zy;
    }
}
