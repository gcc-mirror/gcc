/* { dg-do run } */
/* { dg-require-effective-target arm_neon } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_neon } */

#include <arm_neon.h>
#include <stdlib.h>

struct __attribute__((aligned(16))) _v16u8_ {
	uint8x16_t val;
	_v16u8_() { }

	_v16u8_( const  uint8x16_t &src) { val = src; }
	_v16u8_( const   int16x8_t &src) { val = vreinterpretq_u8_s16(src); }
	_v16u8_( const  uint32x4_t &src) { val = vreinterpretq_u8_u32(src); }

	operator  uint8x16_t () const { return val; }
	operator   int8x16_t () const { return vreinterpretq_s8_u8 (val); }
	operator   int16x8_t () const { return vreinterpretq_s16_u8(val); }
	operator  uint32x4_t () const { return vreinterpretq_u32_u8(val); }
	operator   int32x4_t () const { return vreinterpretq_s32_u8(val); }
};
typedef struct _v16u8_ v16u8;
typedef const v16u8 cv16u8;

typedef v16u8 v16i8;
typedef v16u8 v8i16;
typedef v16u8 v4u32;

inline v16u8 __attribute__((always_inline)) mergelo( const v16u8 & s, const v16u8 & t )
{
	uint8x8x2_t r = vzip_u8( vget_low_u8(s), vget_low_u8(t) );
	return vcombine_u8( r.val[0], r.val[1] );
}

inline v8i16 __attribute__((always_inline)) unpacklo(const v16i8 & s)
{
	return vmovl_s8( vget_low_s8( s ) );
}

const uint32_t __attribute__((aligned(16))) _InA [4] = { 0xFF020001, 0xFF020001, 0xFF000101, 0xFF000101 } ;
const uint32_t __attribute__((aligned(16))) _InB [4] = { 0xFF050002, 0xFF050002, 0xFF000303, 0xFF000203 } ;

__attribute__((noinline)) v16i8 test_func(void)
{
	v16u8 A = vld1q_u8( (uint8_t*) _InA );
	v16u8 B = vld1q_u8( (uint8_t*) _InB );
	v8i16 r   = vdupq_n_s16(2);

	v16u8 _0 = mergelo( A, B );
	v16u8 _1 = mergelo( B, A );

	v16u8 _2 = mergelo( _0, _1 );
	v16u8 _3 = mergelo( _1, _0 );

	v8i16 _4 = vsubq_s16( unpacklo( _2 ), r );
	v8i16 _5 = vsubq_s16( unpacklo( _3 ), r );

	v8i16 ret = vaddq_s16( _4, _5 );

	return ( ret );
}

int main (int argc, char **argv)
{
	v16u8 val = test_func();

	if (vgetq_lane_u32( val, 0 ) != 0xffffffff
	    || vgetq_lane_u32( val, 1 ) != 0xffffffff
	    || vgetq_lane_u32( val, 2 ) != 0xfffcfffc
	    || vgetq_lane_u32( val, 3 ) != 0xfffcfffc)
	  abort ();
	exit (0);
}
