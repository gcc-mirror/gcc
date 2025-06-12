// { dg-options "-O -fno-optimize-sibling-calls -fomit-frame-pointer" }

#include <arm_sme.h>

void callee();

__arm_new("za") __arm_locally_streaming int test()
{
  svbool_t all = svptrue_b8();
  svint8_t expected = svindex_s8(1, 1);
  svwrite_hor_za8_m(0, 0, all, expected);

  callee();

  svint8_t actual = svread_hor_za8_m(svdup_s8(0), all, 0, 0);
  return svptest_any(all, svcmpne(all, expected, actual));
}

// { dg-final { scan-assembler {\tbl\t__arm_tpidr2_save\n} } }
// { dg-final { scan-assembler {\tbl\t__arm_tpidr2_restore\n} } }
// { dg-final { scan-assembler-times {\tsmstart\tza\n} 2 } }
