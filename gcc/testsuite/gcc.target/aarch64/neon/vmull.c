/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vmull_u8:
** umull	v0\.8h, (v0\.8b, v1\.8b|v1\.8b, v0\.8b)
** ret
*/
TEST_BINARY (vmull_u8, uint16x8_t, uint8x8_t, uint8x8_t)

/*
** test_vmull_high_u8:
** umull2	v0\.8h, (v0\.16b, v1\.16b|v1\.16b, v0\.16b)
** ret
*/
TEST_BINARY (vmull_high_u8, uint16x8_t, uint8x16_t, uint8x16_t)

/*
** test_vmull_s8:
** smull	v0\.8h, (v0\.8b, v1\.8b|v1\.8b, v0\.8b)
** ret
*/
TEST_BINARY (vmull_s8, int16x8_t, int8x8_t, int8x8_t)

/*
** test_vmull_high_s8:
** smull2	v0\.8h, (v0\.16b, v1\.16b|v1\.16b, v0\.16b)
** ret
*/
TEST_BINARY (vmull_high_s8, int16x8_t, int8x16_t, int8x16_t)

/*
** test_vmull_u16:
** umull	v0\.4s, (v0\.4h, v1\.4h|v1\.4h, v0\.4h)
** ret
*/
TEST_BINARY (vmull_u16, uint32x4_t, uint16x4_t, uint16x4_t)

/*
** test_vmull_high_u16:
** umull2	v0\.4s, (v0\.8h, v1\.8h|v1\.8h, v0\.8h)
** ret
*/
TEST_BINARY (vmull_high_u16, uint32x4_t, uint16x8_t, uint16x8_t)

/*
** test_vmull_s16:
** smull	v0\.4s, (v0\.4h, v1\.4h|v1\.4h, v0\.4h)
** ret
*/
TEST_BINARY (vmull_s16, int32x4_t, int16x4_t, int16x4_t)

/*
** test_vmull_high_s16:
** smull2	v0\.4s, (v0\.8h, v1\.8h|v1\.8h, v0\.8h)
** ret
*/
TEST_BINARY (vmull_high_s16, int32x4_t, int16x8_t, int16x8_t)

/*
** test_vmull_u32:
** umull	v0\.2d, (v0\.2s, v1\.2s|v1\.2s, v0\.2s)
** ret
*/
TEST_BINARY (vmull_u32, uint64x2_t, uint32x2_t, uint32x2_t)

/*
** test_vmull_high_u32:
** umull2	v0\.2d, (v0\.4s, v1\.4s|v1\.4s, v0\.4s)
** ret
*/
TEST_BINARY (vmull_high_u32, uint64x2_t, uint32x4_t, uint32x4_t)

/*
** test_vmull_s32:
** smull	v0\.2d, (v0\.2s, v1\.2s|v1\.2s, v0\.2s)
** ret
*/
TEST_BINARY (vmull_s32, int64x2_t, int32x2_t, int32x2_t)

/*
** test_vmull_high_s32:
** smull2	v0\.2d, (v0\.4s, v1\.4s|v1\.4s, v0\.4s)
** ret
*/
TEST_BINARY (vmull_high_s32, int64x2_t, int32x4_t, int32x4_t)

/* A low form call whose operands are an upper half and a constant or splat
   redirects to the high form.  */

/*
** test_vmull_high_cst:
** movi	v([0-9]+)\.16b, 0x3
** smull2	v0\.8h, v0\.16b, v\1\.16b
** ret
*/
int16x8_t
test_vmull_high_cst (int8x16_t a)
{
  return vmull_s8 (vget_high_s8 (a), vdup_n_s8 (3));
}

/*
** test_vmull_high_splat:
** dup	v([0-9]+)\.16b, w0
** smull2	v0\.8h, v0\.16b, v\1\.16b
** ret
*/
int16x8_t
test_vmull_high_splat (int8x16_t a, int8_t b)
{
  return vmull_s8 (vget_high_s8 (a), vdup_n_s8 (b));
}

/* A constant that reaches the call through a declaration is only recognisable
   once the function is in SSA form.  */

/*
** test_vmull_cst_decl:
** movi	v([0-9]+)\.4h, 0x3
** smull	v0\.4s, v0\.4h, v\1\.4h
** ret
*/
int32x4_t
test_vmull_cst_decl (int16x4_t a)
{
  int16x4_t b = vdup_n_s16 (3);
  return vmull_s16 (a, b);
}
