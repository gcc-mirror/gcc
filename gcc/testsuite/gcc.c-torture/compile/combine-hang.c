typedef union
{
  double value;
  struct
  {
    unsigned long msw;
    unsigned long lsw;
  } parts;
} ieee_double_shape_type;

double f (int iy)
{
  double z, t;
  ieee_double_shape_type u, v;

  if (iy == 1)
    return 0;

  u.parts.msw = iy;
  u.parts.lsw = 0;
  z = u.value;
  v.parts.msw = iy;
  v.parts.lsw = 0;
  t = v.value;
  return 1.0+z+t+t;
}
