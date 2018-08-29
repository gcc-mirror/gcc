/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-march=z13 -mzarch -mzvector" } */

/* With IBM z14 a hardware instruction for floating point min and max
   has been added while for IBM z13 we emulated min/max for vector
   double with compare and select.  This testcase makes sure that we
   fall back to the emulated variant when compiling for z13.  */

#include <vecintrin.h>

vector double
foo (vector double a, vector double b) {
  return vec_min (a, b);
}

vector double
bar (vector double a, vector double b) {
  return vec_max (a, b);
}
