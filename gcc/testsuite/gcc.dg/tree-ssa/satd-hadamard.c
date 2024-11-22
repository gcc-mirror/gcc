/* { dg-do compile } */
/* { dg-additional-options "-O3 -fdump-tree-forwprop4-details" } */
/* { dg-additional-options "-msse2" { target i?86-*-* x86_64-*-* } } */

#include <stdint.h>

#define HADAMARD4(d0, d1, d2, d3, s0, s1, s2, s3) {\
    int t0 = s0 + s1;\
    int t1 = s0 - s1;\
    int t2 = s2 + s3;\
    int t3 = s2 - s3;\
    d0 = t0 + t2;\
    d1 = t1 + t3;\
    d2 = t0 - t2;\
    d3 = t1 - t3;\
}

int
x264_pixel_satd_8x4_simplified (uint8_t *pix1, int i_pix1, uint8_t *pix2, int i_pix2)
{
  uint32_t tmp[4][4];
  uint32_t a0, a1, a2, a3;
  int sum = 0;
  int i;

  for (i = 0; i < 4; i++, pix1 += i_pix1, pix2 += i_pix2)
    {
      a0 = (pix1[0] - pix2[0]) + ((pix1[4] - pix2[4]) << 16);
      a1 = (pix1[1] - pix2[1]) + ((pix1[5] - pix2[5]) << 16);
      a2 = (pix1[2] - pix2[2]) + ((pix1[6] - pix2[6]) << 16);
      a3 = (pix1[3] - pix2[3]) + ((pix1[7] - pix2[7]) << 16);
      HADAMARD4(tmp[i][0], tmp[i][1], tmp[i][2], tmp[i][3], a0, a1, a2, a3);
    }

  for (i = 0; i < 4; i++)
    {
      HADAMARD4(a0, a1, a2, a3, tmp[0][i], tmp[1][i], tmp[2][i], tmp[3][i]);
      sum += a0 + a1 + a2 + a3;
    }

  return (((uint16_t)sum) + ((uint32_t)sum>>16)) >> 1;
}

/* { dg-final { scan-tree-dump "VEC_PERM_EXPR.*{ 2, 3, 6, 7 }" "forwprop4" { target { aarch64*-*-* i?86-*-* x86_64-*-* } } } } */
