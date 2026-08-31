/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-gimple" } */
/* { dg-final { scan-tree-dump-times "MAX_EXPR" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "MIN_EXPR" 1 "gimple" } } */

typedef unsigned char uint8_t;

#if __SIZEOF_INT__ < 4
#define int __INT32_TYPE__
#endif

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
