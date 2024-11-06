/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <arm_sve.h>
#include <limits.h>

svbool_t
f1 (volatile int32_t *ptr)
{
  return svwhilelt_b8_s32 (*ptr, INT32_MIN);
}

svbool_t
f2 (volatile uint32_t *ptr)
{
  return svwhilelt_b16_u32 (*ptr, 0);
}

svbool_t
f3 (volatile int64_t *ptr)
{
  return svwhilelt_b32_s64 (*ptr, INT64_MIN);
}

svbool_t
f4 (volatile uint64_t *ptr)
{
  return svwhilelt_b64_u64 (*ptr, 0);
}

/* { dg-final { scan-assembler-times {\tpfalse\tp[0-9]+\.b\n} 4 } } */
