/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */

#include "arm_mve.h"

#ifdef __cplusplus
extern "C" {
#endif

uint64_t
foo (uint64_t value)
{
  return uqshll (value, 33); /* { dg-error {passing 33 to argument 2 of 'uqshll', which expects a value in the range \[1, 32\]} } */
}

uint64_t
foo1 (uint64_t value)
{
  return uqshll (value, -1); /* { dg-error {passing -1 to argument 2 of 'uqshll', which expects a value in the range \[1, 32\]} } */
}

#ifdef __cplusplus
}
#endif
