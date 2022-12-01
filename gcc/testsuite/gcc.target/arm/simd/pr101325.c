/* { dg-do compile } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O3" } */

#include <arm_mve.h>

unsigned foo(int8x16_t v, int8x16_t w)
{
  return vcmpeqq (v, w);
}
/* { dg-final { scan-assembler {\tvcmp.i8\teq} } } */
/* { dg-final { scan-assembler {\tvmrs\tr[0-9]+, p0} } } */
/* { dg-final { scan-assembler {\tuxth} } } */
