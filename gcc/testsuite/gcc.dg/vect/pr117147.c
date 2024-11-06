/* { dg-do compile } */
/* { dg-additional-options "-mavx512f" { target { x86_64-*-* i?86-*-* } } } */

double Test(int p, double *sum3, double *sum4, double *wX)
{
  double tmp;
  double bS1 = 0.;
  double bS2 = 0.;
  for (int i = 0; i < p; ++i)
    {
      tmp = wX[i] * wX[i];
      if (tmp != 0.0)
	{
	  bS1 += sum3[i] * sum3[i] / (tmp * wX[i]);
	  bS2 += sum4[i] / tmp;
	}
    }
  return (bS2 + bS1);
}
