/* { dg-do compile { target { { i?86-*-* x86_64-*-* } && { ! x32 } } } } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */

typedef float __m256 __attribute__ (( __vector_size__(32), __may_alias__ ));
struct A {
  __m256 ymm;
  const float &f() const;
};

const float &A::f() const {
  return ymm[1];
}
