/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

#ifndef IMM8
#  define IMM8 100
#endif

void static
avx_test ()
{
    union256i_q source1, source2, u;
    long long  s1[4]={1, 2, 3, 4};
    long long  s2[4]={5, 6, 7, 8};
    long long   e[4];

    source1.x = _mm256_loadu_si256((__m256i*)s1);
    source2.x = _mm256_loadu_si256((__m256i*)s2);
    u.x = _mm256_permute2f128_si256(source1.x, source2.x, IMM8);

    if(IMM8 & 8) e[0] = e[1] = 0;
    else{
        e[0] = (IMM8 & 2 ? s2 : s1)[(IMM8 & 1) * 2];
        e[1] = (IMM8 & 2 ? s2 : s1)[(IMM8 & 1) * 2 + 1];
    }
    if(IMM8 & 128) e[3] = e[3] = 0;
    else{
        unsigned m = (IMM8 >> 4) & 3;
        e[2] = (m & 2 ? s2 : s1)[(m & 1) * 2];
        e[3] = (m & 2 ? s2 : s1)[(m & 1) * 2 + 1];
    }
   
   if (check_union256i_q (u, e))
     abort ();
}
