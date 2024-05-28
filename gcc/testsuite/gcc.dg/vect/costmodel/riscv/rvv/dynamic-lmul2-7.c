/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -ftree-vectorize -mrvv-max-lmul=dynamic -mno-vector-strict-align" } */

int
x264_pixel_8x8 (unsigned char *pix1, unsigned char *pix2, int i_stride_pix2)
{
  int i_sum = 0;
  for (int y = 0; y < 8; y++)
    {
      i_sum += __builtin_abs (pix1[0] - pix2[0]);
      i_sum += __builtin_abs (pix1[1] - pix2[1]);
      i_sum += __builtin_abs (pix1[2] - pix2[2]);
      i_sum += __builtin_abs (pix1[3] - pix2[3]);
      i_sum += __builtin_abs (pix1[4] - pix2[4]);
      i_sum += __builtin_abs (pix1[5] - pix2[5]);
      i_sum += __builtin_abs (pix1[6] - pix2[6]);
      i_sum += __builtin_abs (pix1[7] - pix2[7]);
      pix1 += 16;
      pix2 += i_stride_pix2;
    }
  return i_sum;
}

/* { dg-final { scan-assembler {e32,m2} } } */
/* { dg-final { scan-assembler-not {xor} } } */
