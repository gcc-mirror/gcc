/* { dg-do compile } */
/* { dg-options "-march=rv64gcbv_zvl256b -mabi=lp64d -O3 -mrvv-vector-bits=zvl --param=riscv-autovec-mode=V4QI -mtune=generic-ooo -fdump-rtl-avlprop-all" } */

typedef unsigned char uint8_t;
typedef short int16_t;

#define FDEC_STRIDE 32

static inline uint8_t x264_clip_uint8( int x )
{
  return x;
}

void
x264_add4x4_idct (uint8_t *p_dst, int16_t d[16])
{
  for( int y = 0; y < 4; y++ )
    {
      for( int x = 0; x < 4; x++ )
        p_dst[x] = x264_clip_uint8( p_dst[x] + d[y*4+x] );
      p_dst += FDEC_STRIDE;
    }
}

/* { dg-final { scan-rtl-dump "Propagating AVL: \\(const_int 8" "avlprop" } } */
/* { dg-final { scan-rtl-dump-not "Propagating AVL: \\(const_int 1" "avlprop" } } */
