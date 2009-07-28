/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx -DNEED_IEEE754_DOUBLE" } */
/* { dg-warning "attribute ignored" "" { target default_packed } 164 } */
/* { dg-message " from " "include chain" { target default_packed } 0 } */

#include "avx-check.h"

static void
avx_test ()
{
    int i;
    union128d source1, source2;

    double s1[2] = {30, -5463};
    double s2[2] = {20, 1223};
    int    d[1];
    int    e[1];
  
    source1.x = _mm_loadu_pd(s1);
    source2.x = _mm_loadu_pd(s2);

    d[0] = _mm_testz_pd(source1.x, source2.x);
    
    e[0] = 1;
    for (i = 0; i < 2; i++)
      {
	union ieee754_double u1, u2;
	u1.d = s1[i];
	u2.d = s2[i];
        if (u1.bits.sign && u2.bits.sign) {
            e[0] = 0;
        }
    }

    if (checkVi(d, e, 1))
      abort ();
}

