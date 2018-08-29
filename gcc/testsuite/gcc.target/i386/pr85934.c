/* { dg-do compile } */
/* { dg-options "-O -ftree-loop-vectorize -mavx512vbmi" } */

int uf;

int
l7 (int wk, int sv)
{
  while (sv < 1)
    {
      int me;

      for (me = 0; me < 64; ++me)
	wk += !!((unsigned char) sv) && (!!uf == !!me);

      ++sv;
    }

  return wk;
}
