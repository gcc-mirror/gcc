/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx -DNEED_IEEE754_DOUBLE" } */

#include "avx-check.h"

static void
avx_test ()
{
    int i;
    union256d source1, source2;
   
    double s1[4] = {0, -5463, 86456, 0};
    double s2[4] = {0, -1223, 0,     1};
    int    d[1];
    int    e[1];
    int c=1;
    int z=1;

    source1.x = _mm256_loadu_pd(s1);
    source2.x = _mm256_loadu_pd(s2);
    d[0] = _mm256_testnzc_pd(source1.x, source2.x);

    e[0] = 1;
    for (i = 0; i < 4; i++) {
	union ieee754_double u1, u2;
	u1.d = s1[i];
	u2.d = s2[i];
        if (u1.bits.sign && u2.bits.sign)
            z = 0;
        
        if (!u1.bits.sign && u2.bits.sign)
            c = 0;
    }
    e[0] = (c==0 && z==0) ? 1:0;
    
   if (checkVi(d, e, 1))
     abort ();
}

