/* { dg-do compile  { target { lp64 } } } */
/* { dg-additional-options "-O3 -march=armv8.2-a+crypto -fno-schedule-insns -fno-schedule-insns2 -mcmodel=small" } */
/* { dg-final { check-function-bodies "**" "" "" { target { le } } } } */

#include <arm_neon.h>

/*
**test1:
**	adrp	x[0-9]+, .LC[0-9]+
**	ldr	q[0-9]+, \[x[0-9]+, #:lo12:.LC[0-9]+\]
**	add	v[0-9]+.2d, v[0-9]+.2d, v[0-9]+.2d
**	str	q[0-9]+, \[x[0-9]+\]
**	fmov	x[0-9]+, d[0-9]+
**	orr	x[0-9]+, x[0-9]+, x[0-9]+
**	ret
*/

uint64_t
test1 (uint64_t a, uint64x2_t b, uint64x2_t* rt)
{
  uint64_t arr[2] = { 0x0942430810234076UL, 0x0942430810234076UL};
  uint64_t res = a | arr[0];
  uint64x2_t val = vld1q_u64 (arr);
  *rt = vaddq_u64 (val, b);
  return res;
}

/*
**test2:
**	adrp	x[0-9]+, .LC[0-1]+
**	ldr	q[0-9]+, \[x[0-9]+, #:lo12:.LC[0-9]+\]
**	add	v[0-9]+.2d, v[0-9]+.2d, v[0-9]+.2d
**	str	q[0-9]+, \[x[0-9]+\]
**	fmov	x[0-9]+, d[0-9]+
**	orr	x[0-9]+, x[0-9]+, x[0-9]+
**	ret
*/

uint64_t
test2 (uint64_t a, uint64x2_t b, uint64x2_t* rt)
{
  uint64x2_t val = vdupq_n_u64 (0x0424303242234076UL);
  uint64_t arr = vgetq_lane_u64 (val, 0);
  uint64_t res = a | arr;
  *rt = vaddq_u64 (val, b);
  return res;
}

/*
**test3:
**	adrp	x[0-9]+, .LC[0-9]+
**	ldr	q[0-9]+, \[x[0-9]+, #:lo12:.LC[0-9]+\]
**	add	v[0-9]+.4s, v[0-9]+.4s, v[0-9]+.4s
**	str	q[0-9]+, \[x1\]
**	fmov	w[0-9]+, s[0-9]+
**	orr	w[0-9]+, w[0-9]+, w[0-9]+
**	ret
*/

uint32_t
test3 (uint32_t a, uint32x4_t b, uint32x4_t* rt)
{
  uint32_t arr[4] = { 0x094243, 0x094243, 0x094243, 0x094243 };
  uint32_t res = a | arr[0];
  uint32x4_t val = vld1q_u32 (arr);
  *rt = vaddq_u32 (val, b);
  return res;
}

/*
**test4:
**	ushr	v[0-9]+.16b, v[0-9]+.16b, 7
**	mov	x[0-9]+, 16512
**	movk	x[0-9]+, 0x1020, lsl 16
**	movk	x[0-9]+, 0x408, lsl 32
**	movk	x[0-9]+, 0x102, lsl 48
**	fmov	d[0-9]+, x[0-9]+
**	pmull	v[0-9]+.1q, v[0-9]+.1d, v[0-9]+.1d
**	dup	v[0-9]+.2d, v[0-9]+.d\[0\]
**	pmull2	v[0-9]+.1q, v[0-9]+.2d, v[0-9]+.2d
**	trn2	v[0-9]+.8b, v[0-9]+.8b, v[0-9]+.8b
**	umov	w[0-9]+, v[0-9]+.h\[3\]
**	ret
*/

uint64_t
test4 (uint8x16_t input)
{
    uint8x16_t bool_input = vshrq_n_u8(input, 7);
    poly64x2_t mask = vdupq_n_p64(0x0102040810204080UL);
    poly64_t prodL = vmull_p64((poly64_t)vgetq_lane_p64((poly64x2_t)bool_input, 0),
                               vgetq_lane_p64(mask, 0));
    poly64_t prodH = vmull_high_p64((poly64x2_t)bool_input, mask);
    uint8x8_t res = vtrn2_u8((uint8x8_t)prodL, (uint8x8_t)prodH);
    return vget_lane_u16((uint16x4_t)res, 3);
}

