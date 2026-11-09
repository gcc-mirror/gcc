/* { dg-do compile } */
/* { dg-options "-O2 -march=armv9-a+sme+aes" } */

typedef unsigned long long u64;
typedef unsigned char u8;

/* PMULL is an Advanced SIMD instruction and is not available in streaming
   mode, so the carry-less multiplication must be open-coded.  */

u64
clmul64 (u64 a, u64 b) [[arm::streaming]]
{
  return __builtin_clmulg (a, b);
}

u64
clmul64_widen (u64 a, u64 b, u64 *hi) [[arm::streaming_compatible]]
{
  return __builtin_clmulg (a, b, hi);
}

u8
clmul8 (u8 a, u8 b) [[arm::streaming]]
{
  return __builtin_clmulg (a, b);
}

/* { dg-final { scan-assembler-not {\tpmull\t} } } */
