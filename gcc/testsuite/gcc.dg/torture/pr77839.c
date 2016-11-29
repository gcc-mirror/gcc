/* { dg-do compile } */

void
sd(int yn)
{
  while (yn < 1)
    {
      int hy;
      int *n6 = &hy;
      int **ot = &n6;

      (void)ot;
      for (yn = 0; yn < 1; ++yn)
	{
	  int tc, wo = 0, ez = 0, b8 = 0;
	  int *ls = &wo;

	  (void)ls;
	  hy = 0;
	  for (tc = 0; tc < 1; ++tc)
	    {
	      ez ^= hy;
	      wo ^= ez;
	      ++b8;
	    }
	  hy += (b8 < wo);
	}
    }
}
