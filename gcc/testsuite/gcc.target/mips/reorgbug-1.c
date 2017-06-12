/* { dg-options "-msoft-float -mips2" } */

typedef long int __int32_t;
typedef long unsigned int __uint32_t;
typedef union
{
  double value;
  struct
  {
    __uint32_t msw;
    __uint32_t lsw;
  }
  parts;
}
ieee_double_shape_type;
double
__ieee754_fmod (double x, double y, int z, int xx)
{
  __int32_t n, hx, hy, hz, ix, iy, sx, i;
  __uint32_t lx, ly, lz;
  ieee_double_shape_type ew_u;
  ew_u.value = (x);
  (lx) = ew_u.parts.lsw;
  ew_u.value = (y);
  (hy) = ew_u.parts.msw;
  (ly) = ew_u.parts.lsw;
  if (hy == 0 || hx >= 0x7ff00000)
    return (x * y);
  if (z)
    {
      if ((hx < hy) || (lx < ly))
	return x;
      lz = lx - ly;
      lx = lz + lz;
    }
  ieee_double_shape_type iw_u;
  iw_u.parts.lsw = (lx);
  return iw_u.value;
}
