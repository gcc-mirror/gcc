/* { dg-do run } */
/* { dg-require-effective-target arm_neon } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_neon } */

#include <arm_neon.h>
#include <stdlib.h>

struct __attribute__ ((aligned(8))) _v16u8_ {
  uint8x16_t val;
  _v16u8_( const int16x8_t &src) { val = vreinterpretq_u8_s16(src); }
  operator int16x8_t () const { return vreinterpretq_s16_u8(val); }
};
typedef struct _v16u8_ v16u8;

struct __attribute__ ((aligned(4))) _v8u8_ {
  uint8x8_t val;
  _v8u8_( const uint8x8_t &src) { val = src; }
  operator int16x4_t () const { return vreinterpret_s16_u8(val); }
};
typedef struct _v8u8_ v8u8;

typedef v16u8                v8i16;
typedef int32x4_t            v4i32;
typedef const short         cv1i16;
typedef const unsigned char cv1u8;
typedef const v8i16         cv8i16;

static inline __attribute__((always_inline)) v8u8 zero_64(){ return vdup_n_u8( 0 ); }

static inline __attribute__((always_inline)) v8i16 loadlo_8i16( cv8i16* p ){
  return vcombine_s16( vld1_s16( (cv1i16 *)p ), zero_64() );
}
static inline __attribute__((always_inline)) v8i16 _loadlo_8i16( cv8i16* p, int offset ){
  return loadlo_8i16( (cv8i16*)(&((cv1u8*)p)[offset]) );
}

void __attribute__((noinline))
test(unsigned short *_Inp, int32_t *_Out,
     unsigned int s1v, unsigned int dv0,
     unsigned int smask_v)
{
  int32x4_t c = vdupq_n_s32(0);

  for(unsigned int sv=0 ; sv!=dv0 ; sv=(sv+s1v)&smask_v )
    {
      int32x4_t s;
      s = vmovl_s16( vget_low_s16( _loadlo_8i16( (cv8i16*) _Inp, sv ) ) );
      c = vaddq_s32( c, s );
    }
  vst1q_s32( _Out, c );
}

main()
{
  unsigned short a[4] = {1, 2, 3, 4};
  int32_t b[4] = {0, 0, 0, 0};
  test(a, b, 1, 1, ~0);
  if (b[0] != 1 || b[1] != 2 || b[2] != 3 || b[3] != 4)
    abort();
}
