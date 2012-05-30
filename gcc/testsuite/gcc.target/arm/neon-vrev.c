/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_neon } */

#include <arm_neon.h>

uint16x4_t
tst_vrev642_u16 (uint16x4_t __a)
{
  uint16x4_t __rv;
  uint16x4_t __mask1 = { 3, 2, 1, 0};
  return __builtin_shuffle ( __a, __mask1) ;
}

uint16x8_t
tst_vrev64q2_u16 (uint16x8_t __a)
{
  uint16x8_t __rv;
  uint16x8_t __mask1 = {3, 2, 1, 0, 7, 6, 5, 4 };
  return __builtin_shuffle ( __a, __mask1) ;
}

uint8x8_t
tst_vrev642_u8 (uint8x8_t __a)
{
  uint8x8_t __rv;
  uint8x8_t __mask1 = { 7, 6, 5, 4, 3, 2, 1, 0};
  return __builtin_shuffle ( __a, __mask1) ;
}

uint8x16_t
tst_vrev64q2_u8 (uint8x16_t __a)
{
  uint8x16_t __rv;
  uint8x16_t __mask1 = {7, 6, 5, 4, 3, 2, 1, 0, 15, 14, 13, 12, 11, 10, 9, 8};
  return __builtin_shuffle ( __a, __mask1) ;

}

uint32x2_t
tst_vrev642_u32 (uint32x2_t __a)
{
  uint32x2_t __rv;
  uint32x2_t __mask1 = {1, 0};
  return __builtin_shuffle ( __a, __mask1) ;

}

uint32x4_t
tst_vrev64q2_u32 (uint32x4_t __a)
{
  uint32x4_t __rv;
  uint32x4_t __mask1 = {1, 0, 3, 2};
  return __builtin_shuffle ( __a, __mask1) ;
}

uint16x4_t
tst_vrev322_u16 (uint16x4_t __a)
{
  uint16x4_t __mask1 = { 1, 0, 3, 2 };
  return __builtin_shuffle (__a, __mask1);
}

uint16x8_t
tst_vrev32q2_u16 (uint16x8_t __a)
{
  uint16x8_t __mask1 = { 1, 0, 3, 2, 5, 4, 7, 6 }; 
  return __builtin_shuffle (__a, __mask1);
}

uint8x8_t
tst_vrev322_u8 (uint8x8_t __a)
{
  uint8x8_t __mask1 = { 3, 2, 1, 0, 7, 6, 5, 4};
  return __builtin_shuffle (__a, __mask1);
}

uint8x16_t
tst_vrev32q2_u8 (uint8x16_t __a)
{
  uint8x16_t __mask1 = { 3, 2, 1, 0, 7, 6, 5, 4, 11, 10, 9, 8, 15, 14, 13, 12};
  return __builtin_shuffle (__a, __mask1);
}

uint8x8_t
tst_vrev162_u8 (uint8x8_t __a)
{
  uint8x8_t __mask = { 1, 0, 3, 2, 5, 4, 7, 6};
  return __builtin_shuffle (__a, __mask);
}

uint8x16_t
tst_vrev16q2_u8 (uint8x16_t __a)
{
  uint8x16_t __mask = { 1, 0, 3, 2, 5, 4, 7, 6, 9, 8, 11, 10, 13, 12, 15, 14};
  return __builtin_shuffle (__a, __mask);
}

/* { dg-final {scan-assembler-times "vrev32\.16\\t" 2} }  */
/* { dg-final {scan-assembler-times "vrev32\.8\\t" 2} }  */ 
/* { dg-final {scan-assembler-times "vrev16\.8\\t" 2} }  */
/* { dg-final {scan-assembler-times "vrev64\.8\\t" 2} }  */
/* { dg-final {scan-assembler-times "vrev64\.32\\t" 2} }  */
/* { dg-final {scan-assembler-times "vrev64\.16\\t" 2} }  */
