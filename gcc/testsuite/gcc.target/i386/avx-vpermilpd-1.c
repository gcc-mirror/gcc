/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

#ifndef CTRL
#define CTRL 1
#endif

void static
avx_test ()
{
    union128d u, src;
    double s[2] = {9674.67456, 13543.9788};
    double e[2];

    src.x=_mm_loadu_pd(s);
    u.x=_mm_permute_pd(src.x, CTRL);

    e[0] = s[ (CTRL & 0x01)];
    e[1] = s[((CTRL & 0x02) >> 1)];

    if (check_union128d (u, e))
      abort ();
}

