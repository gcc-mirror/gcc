/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vsm4eq_u32:
** sm4e	v0\.4s, v1\.4s
** ret
*/
uint32x4_t
test_vsm4eq_u32 (uint32x4_t a, uint32x4_t b)
{
  return vsm4eq_u32 (a, b);
}

/*
** test_vsm4ekeyq_u32:
** sm4ekey	v0\.4s, v0\.4s, v1\.4s
** ret
*/
uint32x4_t
test_vsm4ekeyq_u32 (uint32x4_t a, uint32x4_t b)
{
  return vsm4ekeyq_u32 (a, b);
}
