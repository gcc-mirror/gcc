/* { dg-do compile } */
/* { dg-options "-O3 -maltivec -mvsx" } */
/* { dg-require-effective-target lp64 } */

#define NO_WARN_X86_INTRINSICS 1
#include <x86intrin.h>

unsigned long long
test__bzhi_u64_group (unsigned long long a)
{
  /* bzhi is implemented in source as shift left then shift right
   to clear the high order bits.
   For the case where the starting index is const, the compiler
   should reduces this to a single Rotate Left Doubleword
   Immediate then Clear Left (rldicl) instruction.  */
  unsigned long long res;
  res = _bzhi_u64 (a, 8);
  res += _bzhi_u64 (a, 16);
  res += _bzhi_u64 (a, 24);
  res += _bzhi_u64 (a, 32);
  res += _bzhi_u64 (a, 40);
  res += _bzhi_u64 (a, 48);
  return (res);
}
/* the resulting assembler should have 6 X rldicl and no sld or
   srd instructions.  */

/* { dg-final { scan-assembler-times "rldicl" 6 } } */
/* { dg-final { scan-assembler-not "sld" } } */
/* { dg-final { scan-assembler-not "srd" } } */
