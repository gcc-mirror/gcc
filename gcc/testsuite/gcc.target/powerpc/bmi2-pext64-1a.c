/* { dg-do compile } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-O3 -mdejagnu-cpu=power7" } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target powerpc_vsx_ok } */

#define NO_WARN_X86_INTRINSICS 1
#include <x86intrin.h>

unsigned long long
test__pexp_cmask_u64 (unsigned long long a[4])
{
  /* The _pext implmentation is nominally a popcount of the mask,
     followed by a loop using count leading zeros to find the
     next bit to process.
     If the mask is a const, the popcount should be folded and
     the constant propagation should eliminate the mask
     generation loop and produce a single constant bpermd permute
     control word.
     This test verifies that the compiler is replacing the mask
     popcount and loop with a const bperm control and generating
     the bpermd for this case.  */
  const unsigned long mask = 0x00000000100000a4UL;
  unsigned long res;
  res = _pext_u64 (a[0], mask);
  res = (res << 8) | _pext_u64 (a[1], mask);
  res = (res << 8) | _pext_u64 (a[2], mask);
  res = (res << 8) | _pext_u64 (a[3], mask);
  return (res);
}
/* the resulting assembler should have 4 X bpermd and no popcntd or
   cntlzd instructions.  */

/* { dg-final { scan-assembler-times "bpermd" 4 } } */
/* { dg-final { scan-assembler-not "popcntd" } } */
/* { dg-final { scan-assembler-not "cntlzd" } } */
