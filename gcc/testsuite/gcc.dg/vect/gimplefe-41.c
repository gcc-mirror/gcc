/* { dg-do compile { target { vect_double && vect_long_long } } } */
/* { dg-additional-options "-fgimple -Wno-psabi -w" } */
/* { dg-additional-options "-msse2" { target x86_64-*-* i?86-*-* } } */

typedef double __v2df __attribute__ ((__vector_size__ (16)));
typedef unsigned long long __v2di __attribute__ ((__vector_size__ (16)));

__v2df __GIMPLE (ssa)
_mm_add_sd (__v2df x, __v2df y)
{
  __v2df z;
  double _1;
  double _2;
  double _3;
  __v2df _7;

  __BB(2):
  _1 = __BIT_FIELD_REF <double> (x_4(D), 64u, 0u);
  _2 = __BIT_FIELD_REF <double> (y_5(D), 64u, 0u);
  _3 = _1 + _2;
  _7 = _Literal (__v2df) {_3, _3};
  z_6 = __VEC_PERM (x_4(D), _7, _Literal (__v2di) { 2ul, 1ul });
  return z_6;
}

__v2df __GIMPLE (ssa)
_mm_add_sd2 (__v2df x, __v2df y)
{
  __v2df z;
  double _1;
  double _2;
  double _3;

  __BB(2):
  _1 = __BIT_FIELD_REF <double> (x_4(D), 64u, 0u);
  _2 = __BIT_FIELD_REF <double> (y_5(D), 64u, 0u);
  _3 = _1 + _2;
  z_6 = __BIT_INSERT (x_4(D), _3, 0);
  return z_6;
}
