/* { dg-options "-O2 -std=gnu17" } */

#pragma GCC aarch64 "arm_neon.h"
typedef __Uint8x8_t uint8x8_t;
typedef __Uint16x4_t uint16x4_t;
typedef __Int16x8_t int16x8_t;
typedef __Uint16x8_t uint16x8_t;

int jsimd_extbgrx_ycc_convert_neon_image_width,
  jsimd_extbgrx_ycc_convert_neon___trans_tmp_1;

uint16x4_t jsimd_extbgrx_ycc_convert_neon___trans_tmp_2;
uint16x8_t foo1 (...);

uint16x8_t
foo2 (uint8x8_t __a)
{
  return vmovl_u8 (__a);
}

__inline int __attribute__ ((__gnu_inline__)) foo3 (...);

uint8x8x4_t foo4 (...);
void
jsimd_extbgrx_ycc_convert_neon ()
{
  int scaled_128_5 = jsimd_extbgrx_ycc_convert_neon___trans_tmp_1,
      cols_remaining = jsimd_extbgrx_ycc_convert_neon_image_width;
  for (;;)
    if (cols_remaining)
      {
	uint8x8x4_t input_pixels = foo4 ();
	uint16x8_t r = foo2 (input_pixels.val[2]);
	uint16x8_t g = foo2 (input_pixels.val[1]);
	uint16x8_t b = foo2 (input_pixels.val[0]);
	int y_l = foo3 (r);
	uint16x8_t __a = g;
	jsimd_extbgrx_ycc_convert_neon___trans_tmp_2 =
	  (uint16x4_t) vget_low_s16 ((int16x8_t) __a);
	__a = b;
	int cb_l = scaled_128_5;
	int cb_h = scaled_128_5;
	int cr_l = scaled_128_5;
	int cr_h = scaled_128_5;
	uint16x8_t y_u16 = foo1 (y_l);
	uint16x8_t cb_u16 = foo1 (cb_l, cb_h);
	uint16x8_t cr_u16 = foo1 (cr_l, cr_h);
	__a = y_u16 = cb_u16 = cr_u16;
      }
}
