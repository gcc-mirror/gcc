/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */

#include "arm_mve.h"

#ifdef __cplusplus
extern "C" {
#endif

uint32_t
foo (uint32_t value)
{
  return urshr (value, 33); /* { dg-error {passing 33 to argument 2 of 'urshr', which expects a value in the range \[1, 32\]} } */
}

uint32_t
foo1 (uint32_t value)
{
  return urshr (value, -1); /* { dg-error {passing -1 to argument 2 of 'urshr', which expects a value in the range \[1, 32\]} } */
}

#ifdef __cplusplus
}
#endif
