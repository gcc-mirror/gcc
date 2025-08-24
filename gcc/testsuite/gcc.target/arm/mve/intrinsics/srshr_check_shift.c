/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */

#include "arm_mve.h"

#ifdef __cplusplus
extern "C" {
#endif

int32_t
foo (int32_t value)
{
  return srshr (value, 33); /* { dg-error {passing 33 to argument 2 of 'srshr', which expects a value in the range \[1, 32\]} } */
}

int32_t
foo1 (int32_t value)
{
  return srshr (value, -1); /* { dg-error {passing -1 to argument 2 of 'srshr', which expects a value in the range \[1, 32\]} } */
}

#ifdef __cplusplus
}
#endif
