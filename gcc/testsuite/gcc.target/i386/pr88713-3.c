/* { dg-do compile } */
/* { dg-options "-Ofast -mno-avx512er -march=skylake-avx512" } */

#include <math.h>

double square(double d[3], double rad)
{
  double res[3];

  for (int i = 0; i < 3; i++)
    {
      res[i] = d[i] * d[i];
      res[i] *= rad/sqrt(res[i]);
    }

  return res[0];
}
