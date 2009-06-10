/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-O1" } */
/* { dg-add-options arm_neon } */

#include <arm_neon.h>

void neon_internal_error(int *dst, int *src)
{
  uint16x8x4_t sval;

  sval = vld4q_u16((void *)src);
  vst4q_u16((void *)dst,sval);
}
