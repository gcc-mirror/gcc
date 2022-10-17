/* { dg-do compile } */
/* { dg-options "-O2 -fno-ipa-icf -mbmi " } */
/* { dg-final { scan-assembler-times "andn\[^\\n]*eax" 2 } } */
/* { dg-final { scan-assembler-times "bextr\[ \\t]+\[^\\n]*eax" 2 } } */
/* { dg-final { scan-assembler-times "blsi\[^\\n]*eax" 2 } } */
/* { dg-final { scan-assembler-times "blsmsk\[^\\n]*eax" 2 } } */
/* { dg-final { scan-assembler-times "blsr\[^\\n]*eax" 2 } } */
/* { dg-final { scan-assembler-times "tzcntl\[^\\n]*eax" 2 } } */

#include <x86intrin.h>

unsigned int
func_andn32 (unsigned int X, unsigned int Y)
{
  return __andn_u32(X, Y);
}

unsigned int
func_andn32_2 (unsigned int X, unsigned int Y)
{
  return _andn_u32(X, Y);
}

unsigned int
func_bextr32 (unsigned int X, unsigned int Y)
{
  return __bextr_u32(X, Y);
}

unsigned int
func_bextr32_3args (unsigned int X,
		    unsigned int Y,
		    unsigned int Z)
{
  return _bextr_u32(X, Y, Z);
}

unsigned int
func_blsi32 (unsigned int X)
{
  return __blsi_u32(X);
}

unsigned int
func_blsi32_2 (unsigned int X)
{
  return _blsi_u32(X);
}

unsigned int
func_blsmsk32 (unsigned int X)
{
  return __blsmsk_u32(X);
}

unsigned int
func_blsmsk32_2 (unsigned int X)
{
  return _blsmsk_u32(X);
}

unsigned int
func_blsr32 (unsigned int X)
{
  return __blsr_u32(X);
}

unsigned int
func_blsr32_2 (unsigned int X)
{
  return _blsr_u32(X);
}

unsigned int
func_tzcnt32 (unsigned int X)
{
  return __tzcnt_u32(X);
}

unsigned int
func_tzcnt32_2 (unsigned int X)
{
  return _tzcnt_u32(X);
}
