/* { dg-do compile } */
/* { dg-options "-O2 -mdejagnu-cpu=future" } */
/* { dg-require-effective-target powerpc_future_compile_ok } */

#include <altivec.h>

vector signed char
test_unpack_hsn_to_byte(vector unsigned long long a)
{
  return vec_unpack_hsn_to_byte(a);
}

vector signed char
test_unpack_lsn_to_byte(vector unsigned long long a)
{
  return vec_unpack_lsn_to_byte(a);
}

vector unsigned char
test_unpack_int4_to_bf16(vector unsigned short a)
{
  return vec_unpack_int4_to_bf16(a, 0);
}

vector unsigned char
test_unpack_int8_to_bf16(vector unsigned short a)
{
  return vec_unpack_int8_to_bf16(a, 0);
}

vector float
test_unpack_int4_to_fp32(vector unsigned int a)
{
  return vec_unpack_int4_to_fp32(a, 0);
}

vector float
test_unpack_int8_to_fp32(vector unsigned int a)
{
  return vec_unpack_int8_to_fp32(a, 0);
}


/* { dg-final { scan-assembler-times "xxbrq" 2 { target { vsx_hw && le } }  } } */

/* { dg-final { scan-assembler-times "vupkhsntob" 1  } } */
/* { dg-final { scan-assembler-times "vupklsntob" 1  } } */
/* { dg-final { scan-assembler-times "vupkint4tobf16" 1 } } */
/* { dg-final { scan-assembler-times "vupkint8tobf16" 1 } } */
/* { dg-final { scan-assembler-times "vupkint4tofp32" 1 } } */
/* { dg-final { scan-assembler-times "vupkint8tofp32" 1 } } */
