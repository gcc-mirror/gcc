/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <arm_sve.h>
#include <limits.h>

svbool_t
f1 (volatile int32_t *ptr)
{
  return svwhilele_b8_s32 (*ptr, INT32_MAX);
}

svbool_t
f2 (volatile uint32_t *ptr)
{
  return svwhilele_b16_u32 (*ptr, UINT32_MAX);
}

svbool_t
f3 (volatile int64_t *ptr)
{
  return svwhilele_b32_s64 (*ptr, INT64_MAX);
}

svbool_t
f4 (volatile uint64_t *ptr)
{
  return svwhilele_b64_u64 (*ptr, UINT64_MAX);
}

/* { dg-final { scan-assembler {\tptrue\tp[0-9]+\.b(?:, all)\n} } } */
/* { dg-final { scan-assembler {\tptrue\tp[0-9]+\.h(?:, all)\n} } } */
/* { dg-final { scan-assembler {\tptrue\tp[0-9]+\.s(?:, all)\n} } } */
/* { dg-final { scan-assembler {\tptrue\tp[0-9]+\.d(?:, all)\n} } } */
