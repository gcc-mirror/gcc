/* { dg-do compile } */
/* { dg-options "-O3" } */

typedef unsigned char uint8_t;

static inline uint8_t
x264_clip_uint8(uint8_t x)
{
  uint8_t t = -x;
  uint8_t t1 = x & ~63; 
  return (t1 != 0) ? t : x; 
}

void
mc_weight(uint8_t *restrict dst, uint8_t *restrict src, int n)
{
  for (int x = 0; x < n*16; x++)
    dst[x] = x264_clip_uint8(src[x]);
}

/* { dg-final { scan-assembler-not {\tsel} } } */
