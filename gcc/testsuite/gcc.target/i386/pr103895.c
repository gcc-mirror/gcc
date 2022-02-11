/* { dg-do compile } */
/* { dg-options "-Og -ffloat-store -msse2" } */

#include <emmintrin.h>
typedef float vFloat __attribute__((__vector_size__(16)));
float bar_dr;
vFloat bar_f1;
void bar() {
  static vFloat m0;
  vFloat fa1 = _mm_andnot_ps(m0, bar_f1);
  __attribute__((__vector_size__(2 * sizeof(double)))) double v3 =
      _mm_cvtps_pd(fa1);
  vFloat r1 = _mm_cvtpd_ps(v3);
  _mm_storeu_ps(&bar_dr, r1);
}

