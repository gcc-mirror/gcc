/* { dg-do compile } */
/* { dg-options "-O2 -floop-parallelize-all" } */

void
vw (int *dk, int zd, int jb)
{
  int sq;
  int *kv = &sq;

  for (sq = 0; sq < 2; ++sq)
    {
      int u1;

      for (u1 = 0; u1 < 5; ++u1)
	if (zd == 0)
	  return;
    }

  for (;;)
    {
      ++zd;
      if (zd == 0)
	while (jb != 0)
	  kv = &jb;

      while (*dk < 1)
	{
	  for (jb = 0; jb < 2; ++jb)
	    {
	    }
	  ++*dk;
	}

      for (*kv = 0; *kv < 2; ++*kv)
	for (*dk = 0; *dk < 2; ++*dk)
	  {
	  }
    }
}
