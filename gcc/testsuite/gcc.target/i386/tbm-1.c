/* { dg-do compile } */
/* { dg-options "-O2 -mtbm" } */
/* { dg-final { scan-assembler "bextr\[^\\n]*(%|)eax" } } */
/* { dg-final { scan-assembler "blcfill\[^\\n]*(%|)eax" } } */
/* { dg-final { scan-assembler "blci\[^\\n]*(%|)eax" } } */
/* { dg-final { scan-assembler "blcic\[^\\n]*(%|)eax" } } */
/* { dg-final { scan-assembler "blcmsk\[^\\n]*(%|)eax" } } */
/* { dg-final { scan-assembler "blcs\[^\\n]*(%|)eax" } } */
/* { dg-final { scan-assembler "blsfill\[^\\n]*(%|)eax" } } */
/* { dg-final { scan-assembler "blsic\[^\\n]*(%|)eax" } } */
/* { dg-final { scan-assembler "t1mskc\[^\\n]*(%|)eax" } } */
/* { dg-final { scan-assembler "tzmsk\[^\\n]*(%|)eax" } } */

#include <x86intrin.h>

unsigned int
func_bextri32 (unsigned int X)
{
  return __bextri_u32 (X, 0x101);
}

unsigned int
func_blcfill32 (unsigned int X)
{
  return __blcfill_u32 (X);
}

unsigned int
func_blci32 (unsigned int X)
{
  return __blci_u32 (X);
}

unsigned int
func_blcic32 (unsigned int X)
{
  return __blcic_u32 (X);
}

unsigned int
func_blcmsk32 (unsigned int X)
{
  return __blcmsk_u32 (X);
}

unsigned int
func_blcs32 (unsigned int X)
{
  return __blcs_u32 (X);
}

unsigned int
func_blsfill32 (unsigned int X)
{
  return __blsfill_u32 (X);
}

unsigned int
func_blsic32 (unsigned int X)
{
  return __blsic_u32 (X);
}

unsigned int
func_t1mskc32 (unsigned int X)
{
  return __t1mskc_u32 (X);
}

unsigned int
func_tzmsk32 (unsigned int X)
{
  return __tzmsk_u32 (X);
}
