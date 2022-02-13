/* { dg-do compile } */
/* { dg-options "-march=cascadelake -O3" } */
/* { dg-final { scan-assembler-not "kunpck" } } */
/* { dg-final { scan-assembler-not "kand" } } */
/* { dg-final { scan-assembler-not "kor" } } */
/* { dg-final { scan-assembler-not "kshift" } } */

typedef unsigned char uint8_t;

static uint8_t x264_clip_uint8 (int x, unsigned int y)
{
  return x & (~255) ? (-x) >> 31 : y;
}

void
mc_weight (uint8_t* __restrict dst, uint8_t* __restrict src,
	   int i_width,int i_scale, unsigned int* __restrict y)
{
  for(int x = 0; x < i_width; x++)
    dst[x] = x264_clip_uint8 (src[x] * i_scale, y[x]);
}
