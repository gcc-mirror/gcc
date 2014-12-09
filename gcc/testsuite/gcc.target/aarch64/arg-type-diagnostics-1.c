/* { dg-do compile { target { aarch64*-*-* } } } */
/* { dg-options "-O2" } */

#include "arm_neon.h"

void foo (int a)
{
  int32x2_t arg1;
  int32x2_t arg2;
  int32x2_t result;
  arg1 = vcreate_s32 (UINT64_C (0x0000ffffffffffff));
  arg2 = vcreate_s32 (UINT64_C (0x16497fffffffffff));
  /* The correct line number is in the preamble to the error message,
     not in the final line (which is all that dg-error inspects). Hence,
     we have to tell dg-error to ignore the line number.  */
  result = vrsra_n_s32 (arg1, arg2, a);
  /* { dg-error "must be a constant immediate" "" { target *-*-* } 0 } */
}
