#include <math.h>

double test1 (double d0, double d1)
{
  return fmin (d0, d1);
}

float test2 (float d0, float d1)
{
  return fmin (d0, d1);
}

double test3 (double d0, double d1)
{
  return fmax (d0, d1);
}

float test4 (float d0, float d1)
{
  return fmax (d0, d1);
}

double test5 (double d0, double d1)
{
  return __builtin_vsx_xsmindp (d0, d1);
}

double test6 (double d0, double d1)
{
  return __builtin_vsx_xsmaxdp (d0, d1);
}
