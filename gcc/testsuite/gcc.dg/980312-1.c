/* { dg-do link { target i?86-*-* } } */
/* { dg-options "-O2 -march=pentiumpro" } */

extern __inline  double
__expm1 (double __x)
{
  double __temp;
  __temp = 1.0;
  return __temp;
}
extern __inline  double
__sgn1 (double __x)
{
  return __x >= 0.0 ? 1.0 : -1.0;
}
double
tanh (double __x)
{
  return  __expm1 (__x) * __sgn1 (-__x);
}
main ()
{
  return tanh (3.45) != 0;
}
