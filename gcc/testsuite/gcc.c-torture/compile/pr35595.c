/* { dg-require-effective-target int32plus } */
typedef signed int __int32_t;
typedef unsigned int __uint32_t;
typedef union
{
  float value;
  __uint32_t word;
}
ieee_float_shape_type;
static const float two23 = 8.3886080000e+06;
static const float pi = 3.1415927410e+00;
static const float zero = 0.0000000000e+00;
float __kernel_sinf (float, float, int);
static float
sin_pif (float x)
{
  float y = 0;
  float z = 0;
  __int32_t n = 0;
  __int32_t ix = 0;
  do
    {
      ieee_float_shape_type gf_u = { 0 };
      (ix) = gf_u.word;
    }
  while (0);
  if (z == y)
    {
      if (ix < 0x4b800000)
	{
	  if (ix < 0x4b000000)
	    z = y + two23;
	  do
	    {
	      ieee_float_shape_type gf_u;
	      gf_u.value = (z);
	      (n) = gf_u.word;
	    }
	  while (0);
	}
    }
  if (n == 0)
    y = __kernel_sinf (pi * y, zero, 0);
}

float
__ieee754_lgammaf_r (float x, int *signgamp)
{
  float t = 0;
  __int32_t hx = 0;
  do
    {
      ieee_float_shape_type gf_u = { 0 };
      (hx) = gf_u.word;
    }
  while (0);
  if (hx < 0)
    t = sin_pif (x);
  return 0;
}
