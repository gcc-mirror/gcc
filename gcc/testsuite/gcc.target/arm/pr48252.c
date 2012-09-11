/* { dg-do run } */
/* { dg-require-effective-target arm_neon_hw } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_neon } */

#include "arm_neon.h"
#include <stdlib.h>

int main(void)
{
    uint8_t v1_init[8] = {1, 1, 1, 1, 1, 1, 1, 1};
    uint8_t v2_init[8] = {2, 2, 2, 2, 2, 2, 2, 2};
    uint8x8_t v1 = vld1_u8 (v1_init);
    uint8x8_t v2 = vld1_u8 (v2_init);
    uint8x8x2_t vd1, vd2;
    union {uint8x8_t v; uint8_t buf[8];} d1, d2, d3, d4;
    int i;
    uint8_t odd, even;

    vd1 = vzip_u8(v1, vdup_n_u8(0));
    vd2 = vzip_u8(v2, vdup_n_u8(0));

    vst1_u8(d1.buf, vd1.val[0]);
    vst1_u8(d2.buf, vd1.val[1]);
    vst1_u8(d3.buf, vd2.val[0]);
    vst1_u8(d4.buf, vd2.val[1]);

#ifdef __ARMEL__
    odd = 1;
    even = 0;
#else
    odd = 0;
    even = 1;
#endif

    for (i = 0; i < 8; i++)
      if ((i % 2 == even && d4.buf[i] != 2)
          || (i % 2 == odd && d4.buf[i] != 0))
         abort ();

    return 0;
}
