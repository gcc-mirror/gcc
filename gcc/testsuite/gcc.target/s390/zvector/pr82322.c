/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-march=z14 -mzarch -mzvector" } */

/* vec_ceil and friends are expanded by vecintrin.h to
   __builtin_s390_vfi which is an overloaded builtin being replaced by
   either __builtin_s390_vfisb or __builtin_s390_vfidb depending on
   its argument types.

   The problem in this PR was that the overloaded builtin definition
   was missing in s390-builtins.def.  */

#include <vecintrin.h>

vector double
foo (vector double a) {
  return vec_ceil (a);
}

vector float
bar (vector float a) {
  return vec_ceil (a);
}
