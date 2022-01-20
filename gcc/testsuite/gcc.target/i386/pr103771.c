/* { dg-do compile } */
/* { dg-options "-march=cascadelake -O3 -fdump-tree-vect-details -mprefer-vector-width=128" } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */

typedef unsigned char uint8_t;

static uint8_t x264_clip_uint8 (int x)
{
  return x & (~255) ? (-x) >> 31 : x;
}

void
mc_weight (uint8_t* __restrict dst, uint8_t* __restrict src,
	   int i_width,int i_scale)
{
  for(int x = 0; x < i_width; x++)
    dst[x] = x264_clip_uint8 (src[x] * i_scale);
}
