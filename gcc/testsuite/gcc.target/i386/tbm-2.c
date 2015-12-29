/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mtbm" } */
/* { dg-final { scan-assembler "bextr\[^\\n]*(%|)rax" } } */
/* { dg-final { scan-assembler "blcfill\[^\\n]*(%|)rax" } } */
/* { dg-final { scan-assembler "blci\[^\\n]*(%|)rax" } } */
/* { dg-final { scan-assembler "blcic\[^\\n]*(%|)rax" } } */
/* { dg-final { scan-assembler "blcmsk\[^\\n]*(%|)rax" } } */
/* { dg-final { scan-assembler "blcs\[^\\n]*(%|)rax" } } */
/* { dg-final { scan-assembler "blsfill\[^\\n]*(%|)rax" } } */
/* { dg-final { scan-assembler "blsic\[^\\n]*(%|)rax" } } */
/* { dg-final { scan-assembler "t1mskc\[^\\n]*(%|)rax" } } */
/* { dg-final { scan-assembler "tzmsk\[^\\n]*(%|)rax" } } */

#include <x86intrin.h>

unsigned long long
func_bextri64 (unsigned long long X)
{
  return __bextri_u64 (X, 0x101);
}

unsigned long long
func_blcfill64 (unsigned long long X)
{
  return __blcfill_u64 (X);
}

unsigned long long
func_blci64 (unsigned long long X)
{
  return __blci_u64 (X);
}

unsigned long long
func_blcic64 (unsigned long long X)
{
  return __blcic_u64 (X);
}

unsigned long long
func_blcmsk64 (unsigned long long X)
{
  return __blcmsk_u64 (X);
}

unsigned long long
func_blcs64 (unsigned long long X)
{
  return __blcs_u64 (X);
}

unsigned long long
func_blsfill64 (unsigned long long X)
{
  return __blsfill_u64 (X);
}

unsigned long long
func_blsic64 (unsigned long long X)
{
  return __blsic_u64 (X);
}

unsigned long long
func_t1mskc64 (unsigned long long X)
{
  return __t1mskc_u64 (X);
}

unsigned long long
func_tzmsk64 (unsigned long long X)
{
  return __tzmsk_u64 (X);
}
