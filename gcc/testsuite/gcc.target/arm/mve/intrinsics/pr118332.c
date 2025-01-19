/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */

#include <arm_mve.h>
uint32x4_t first(uint32x4x4_t a) { return a.val[0]; }
