/* PR rtl-optimization/57960  */

/* { dg-do compile } */
/* { dg-options "-march=z10 -m64 -mzarch  -O1" } */

typedef union
{
  long double value;
  struct
  {
    unsigned int w0, w1, w2, w3;
  }
    parts32;
}
  ieee854_long_double_shape_type;
static const long double one = 1.0L;
long double
__ieee754_acosl (long double x)
{
  long double z, w;
  int ix;
  ieee854_long_double_shape_type u;

  z = (one - u.value) * 0.5;
  u.parts32.w2 = 0;
  u.parts32.w3 = 0;
  w = z - u.value * u.value;
  return 2.0 * w;

}
