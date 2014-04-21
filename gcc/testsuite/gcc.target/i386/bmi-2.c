/* { dg-do compile { target { ! { ia32 }  } } } */
/* { dg-options "-O2 -mbmi " } */
/* { dg-final { scan-assembler "andn\[^\\n]*rax" } } */
/* { dg-final { scan-assembler-times "bextr\[ \\t]+\[^\\n]*rax" 2 } } */
/* { dg-final { scan-assembler-times "blsi\[^\\n]*rax" 2 } } */
/* { dg-final { scan-assembler-times "blsmsk\[^\\n]*rax" 2 } } */
/* { dg-final { scan-assembler-times "blsr\[^\\n]*rax" 2 } } */
/* { dg-final { scan-assembler-times "tzcntq\[^\\n]*rax" 2 } } */

#include <x86intrin.h>

unsigned long long
func_andn64 (unsigned long long X, unsigned long long Y)
{
  return __andn_u64 (X, Y);
}

unsigned long long
func_bextr64 (unsigned long long X, unsigned long long Y)
{
  return __bextr_u64 (X, Y);
}

unsigned long long
func_bextr64_3args (unsigned long long X,
		    unsigned long long Y,
		    unsigned long long Z)
{
  return _bextr_u64 (X, Y, Z);
}

unsigned long long
func_blsi64 (unsigned long long X)
{
  return __blsi_u64 (X);
}

unsigned long long
func_blsi64_2 (unsigned long long X)
{
  return _blsi_u64 (X);
}

unsigned long long
func_blsmsk64 (unsigned long long X)
{
  return __blsmsk_u64 (X);
}

unsigned long long
func_blsmsk64_2 (unsigned long long X)
{
  return _blsmsk_u64 (X);
}

unsigned long long
func_blsr64 (unsigned long long X)
{
  return __blsr_u64 (X);
}

unsigned long long
func_blsr64_2 (unsigned long long X)
{
  return _blsr_u64 (X);
}

unsigned long long
func_tzcnt64 (unsigned long long X)
{
  return __tzcnt_u64 (X);
}

unsigned long long
func_tzcnt64_2 (unsigned long long X)
{
  return _tzcnt_u64 (X);
}
