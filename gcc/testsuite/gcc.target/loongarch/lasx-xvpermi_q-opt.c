/* { dg-do compile } */
/* { dg-options "-O3 -mlasx -ftree-vectorize" } */

#include <lasxintrin.h>

#define TEST_FUNC(imm)                                                        \
  __m256i                                                                     \
  test_##imm (__m256i op0, __m256i op1)                                       \
  {                                                                           \
    return __lasx_xvpermi_q (op0, op1, imm);                                  \
  }

TEST_FUNC (0x00)
/* { dg-final { scan-assembler-not "test_0x00:.*\txvld.*xvld.*-test_0x00"} } */
/* { dg-final { scan-assembler-times "test_0x00:.*\txvpermi\\.d.*-test_0x00" 1 } } */

TEST_FUNC (0x01)
/* { dg-final { scan-assembler-not "test_0x01:.*\txvld.*xvld.*-test_0x01"} } */
/* { dg-final { scan-assembler-times "test_0x01:.*\txvpermi\\.d.*-test_0x01" 1 } } */

TEST_FUNC (0x10)
/* { dg-final { scan-assembler-not "test_0x10:.*\txvld.*xvld.*-test_0x10"} } */
/* { dg-final { scan-assembler-not "test_0x10:.*\txvpermi.*-test_0x10"} } */

TEST_FUNC (0x11)
/* { dg-final { scan-assembler-not "test_0x11:.*\txvld.*xvld.*-test_0x11"} } */
/* { dg-final { scan-assembler-times "test_0x11:.*\txvpermi\\.d.*-test_0x11" 1 } } */

TEST_FUNC (0x22)
/* { dg-final { scan-assembler-not "test_0x22:.*\txvld.*xvld.*-test_0x22"} } */
/* { dg-final { scan-assembler-times "test_0x22:.*\txvpermi\\.d.*-test_0x22" 1 } } */

TEST_FUNC (0x23)
/* { dg-final { scan-assembler-not "test_0x23:.*\txvld.*xvld.*-test_0x23"} } */
/* { dg-final { scan-assembler-times "test_0x23:.*\txvpermi\\.d.*-test_0x23" 1 } } */

TEST_FUNC (0x32)
/* { dg-final { scan-assembler-not "test_0x32:.*\txvld.*xvld.*-test_0x32"} } */
/* { dg-final { scan-assembler-not "test_0x32:.*\txvpermi.*-test_0x32"} } */

TEST_FUNC (0x33)
/* { dg-final { scan-assembler-not "test_0x33:.*\txvld.*xvld.*-test_0x33"} } */
/* { dg-final { scan-assembler-times "test_0x33:.*\txvpermi\\.d.*-test_0x33" 1 } } */

