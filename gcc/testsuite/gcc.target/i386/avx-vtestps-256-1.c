/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx -DNEED_IEEE754_FLOAT" } */
/* { dg-warning "attribute ignored" "" { target default_packed } 150 } */
/* { dg-message " from " "include chain" { target default_packed } 0 } */

#include "avx-check.h"

static void
avx_test ()
{
    int i;
    union256 source1, source2;

    float s1[8] = {0, -5463, 86456, 0, 1234, 0, 62445, 34352};
    float s2[8] = {0, -1223, 0,     0, 0,    1, 0,     0};
    int   d[1];
    int   e[1];

    source1.x = _mm256_loadu_ps(s1);
    source2.x = _mm256_loadu_ps(s2);
    d[0] = _mm256_testz_ps(source1.x, source2.x);

    e[0] = 1;
    for (i = 0; i < 8; i++) {
	union ieee754_float u1, u2;
	u1.d = s1[i];
	u2.d = s2[i];
	if (u1.bits.sign && u2.bits.sign)
            e[0] = 0;
    }

    if (checkVi (d, e, 1))
      abort ();
}

