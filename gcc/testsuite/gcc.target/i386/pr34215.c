/* Testcase by Martin Michlmayr <tbm@cyrius.com> */

/* { dg-do compile { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */
/* { dg-options "-O2" } */

double pow (double, double);

void calc_score_dist (int mxdlen, long double d, long double **dist)
{
  unsigned long i, scr2;
  for (i = 1; i <= mxdlen; i++)
    {
      for (scr2 = mxdlen; scr2 <= mxdlen + 10; scr2++)
	{
	}
      dist[i][scr2] *= pow (1.0 / d, i);
    }
}
