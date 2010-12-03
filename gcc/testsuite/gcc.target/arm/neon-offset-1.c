/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-O1" } */
/* { dg-add-options arm_neon } */

#include <arm_neon.h>

void neon_internal_error(int32x4_t *dst, char *src)
{
  *dst = *(int32x4_t *)(src+1008);
}
