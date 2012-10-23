/* { dg-do compile { target { aarch64*-*-* } } } */
/* { dg-options "-O2" } */

#include "arm_neon.h"

void foo ()
{
  int a;
  int32x2_t arg1;
  int32x2_t arg2;
  int32x2_t result;
  arg1 = vcreate_s32 (UINT64_C (0x0000ffffffffffff));
  arg2 = vcreate_s32 (UINT64_C (0x16497fffffffffff));
  result = __builtin_aarch64_srsra_nv2si (arg1, arg2, a); /* { dg-error "incompatible type for argument" } */
}
