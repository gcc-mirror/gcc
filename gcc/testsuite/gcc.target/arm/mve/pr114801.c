/* { dg-do compile } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <arm_mve.h>

/*
** test_32:
**...
**	mov	r[0-9]+, #65295	@ movhi
**...
*/
uint32x4_t test_32() {
  /* V4BI predicate converted to 0xff0f.  */
  return vdupq_m_n_u32(vdupq_n_u32(0xffffffff), 0, 0x4f02); /* { dg-warning {constant predicate argument 3 \(0x4f02\) does not map to 4 lane numbers, converted to 0xff0f} } */
}

/*
** test_16:
**...
**	mov	r[0-9]+, #12339	@ movhi
**...
*/
uint16x8_t test_16() {
  /* V8BI predicate converted to 0x3033.  */
  return vdupq_m_n_u16(vdupq_n_u16(0xffff), 0, 0x3021); /* { dg-warning {constant predicate argument 3 \(0x3021\) does not map to 8 lane numbers, converted to 0x3033} } */
}

/*
** test_8:
**...
**	mov	r[0-9]+, #23055	@ movhi
**...
*/
uint8x16_t test_8() {
  return vdupq_m_n_u8(vdupq_n_u8(0xff), 0, 0x5a0f);
}
