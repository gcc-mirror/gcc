/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-require-effective-target arm_little_endian } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_neon } */

#include <arm_neon.h>

uint8x8_t
tst_vext_u8 (uint8x8_t __a, uint8x8_t __b)
{
  uint8x8_t __mask1 = {2, 3, 4, 5, 6, 7, 8, 9};

  return __builtin_shuffle ( __a, __b, __mask1) ;
}

uint8x8_t
tst_vext_u8_rotate (uint8x8_t __a)
{
  uint8x8_t __mask1 = {2, 3, 4, 5, 6, 7, 0, 1};
  return __builtin_shuffle ( __a, __mask1) ;
}

uint16x4_t
tst_vext_u16 (uint16x4_t __a, uint16x4_t __b)
{
  uint16x4_t __mask1 = {2, 3, 4, 5};
  return __builtin_shuffle ( __a, __b, __mask1) ;
}

uint16x4_t
tst_vext_u16_rotate (uint16x4_t __a)
{
  uint16x4_t __mask1 = {2, 3, 0, 1};
  return __builtin_shuffle ( __a, __mask1) ;
}

uint32x2_t
tst_vext_u32 (uint32x2_t __a, uint32x2_t __b)
{
  uint32x2_t __mask1 = {1, 2};
  return __builtin_shuffle ( __a, __b, __mask1) ;
}

/* This one is mapped into vrev64.32.  */
uint32x2_t
tst_vext_u32_rotate (uint32x2_t __a)
{
  uint32x2_t __mask1 = {1, 0};
  return __builtin_shuffle ( __a, __mask1) ;
}

uint8x16_t
tst_vextq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  uint8x16_t __mask1 = {4, 5, 6, 7, 8, 9, 10, 11,
			12, 13, 14, 15, 16, 17, 18, 19};
  return __builtin_shuffle ( __a, __b, __mask1) ;
}

uint8x16_t
tst_vextq_u8_rotate (uint8x16_t __a)
{
  uint8x16_t __mask1 = {4, 5, 6, 7, 8, 9, 10, 11,
			12, 13, 14, 15, 0, 1, 2, 3};
  return __builtin_shuffle ( __a, __mask1) ;
}

uint16x8_t
tst_vextq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  uint16x8_t __mask1 = {2, 3, 4, 5, 6, 7, 8, 9};
  return __builtin_shuffle ( __a, __b, __mask1) ;
}

uint16x8_t
tst_vextq_u16_rotate (uint16x8_t __a)
{
  uint16x8_t __mask1 = {2, 3, 4, 5, 6, 7, 0, 1};
  return __builtin_shuffle ( __a, __mask1) ;
}

uint32x4_t
tst_vextq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  uint32x4_t __mask1 = {1, 2, 3, 4};
  return __builtin_shuffle ( __a, __b, __mask1) ;
}

uint32x4_t
tst_vextq_u32_rotate (uint32x4_t __a)
{
  uint32x4_t __mask1 = {1, 2, 3, 0};
  return __builtin_shuffle ( __a, __mask1) ;
}

uint64x2_t
tst_vextq_u64 (uint64x2_t __a, uint64x2_t __b)
{
  uint64x2_t __mask1 = {1, 2};
  return __builtin_shuffle ( __a, __b, __mask1) ;
}

uint64x2_t
tst_vextq_u64_rotate (uint64x2_t __a)
{
  uint64x2_t __mask1 = {1, 0};
  return __builtin_shuffle ( __a, __mask1) ;
}

/* { dg-final {scan-assembler-times "vext\.8\\t" 4} }  */
/* { dg-final {scan-assembler-times "vext\.16\\t" 4} }  */
/* { dg-final {scan-assembler-times "vext\.32\\t" 3} }  */
/* { dg-final {scan-assembler-times "vrev64\.32\\t" 1} }  */
/* { dg-final {scan-assembler-times "vext\.64\\t" 2} }  */
