/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vsm3ss1q_u32:
** sm3ss1	v0\.4s, v0\.4s, v1\.4s, v2\.4s
** ret
*/
uint32x4_t
test_vsm3ss1q_u32 (uint32x4_t a, uint32x4_t b, uint32x4_t c)
{
  return vsm3ss1q_u32 (a, b, c);
}

/*
** test_vsm3tt1aq_u32:
** sm3tt1a	v0\.4s, v1\.4s, v2\.4s\[0\]
** ret
*/
uint32x4_t
test_vsm3tt1aq_u32 (uint32x4_t a, uint32x4_t b, uint32x4_t c)
{
  return vsm3tt1aq_u32 (a, b, c, 0);
}

/*
** test_vsm3tt2bq_u32:
** sm3tt2b	v0\.4s, v1\.4s, v2\.4s\[0\]
** ret
*/
uint32x4_t
test_vsm3tt1bq_u32 (uint32x4_t a, uint32x4_t b, uint32x4_t c)
{
  return vsm3tt1bq_u32 (a, b, c, 0);
}

/*
** test_vsm3tt2aq_u32:
** sm3tt2a	v0\.4s, v1\.4s, v2\.4s\[0\]
** ret
*/
uint32x4_t
test_vsm3tt2aq_u32 (uint32x4_t a, uint32x4_t b, uint32x4_t c)
{
  return vsm3tt2aq_u32 (a, b, c, 0);
}

/*
** test_vsm3tt2bq_u32:
** sm3tt2b	v0\.4s, v1\.4s, v2\.4s\[0\]
** ret
*/
uint32x4_t
test_vsm3tt2bq_u32 (uint32x4_t a, uint32x4_t b, uint32x4_t c)
{
  return vsm3tt2bq_u32 (a, b, c, 0);
}

/*
** test_vsm3partw1q_u32:
** sm3partw1	v0\.4s, v1\.4s, v2\.4s
** ret
*/
uint32x4_t
test_vsm3partw1q_u32 (uint32x4_t a, uint32x4_t b, uint32x4_t c)
{
  return vsm3partw1q_u32 (a, b, c);
}

/*
** test_vsm3partw2q_u32:
** sm3partw2	v0\.4s, v1\.4s, v2\.4s
** ret
*/
uint32x4_t
test_vsm3partw2q_u32 (uint32x4_t a, uint32x4_t b, uint32x4_t c)
{
  return vsm3partw2q_u32 (a, b, c);
}
