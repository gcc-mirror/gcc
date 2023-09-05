/* { dg-do compile } */
/* { dg-options "-std=c++14 -O2 -march=loongarch64 -mabi=lp64d" } */
/* { dg-final { scan-assembler "bstrins\\.d.*7,4" } } */
/* { dg-final { scan-assembler "bstrins\\.d.*15,4" } } */
/* { dg-final { scan-assembler "bstrins\\.d.*31,4" } } */
/* { dg-final { scan-assembler "bstrins\\.d.*47,4" } } */
/* { dg-final { scan-assembler "bstrins\\.d.*3,0" } } */

typedef unsigned long u64;

template <u64 mask>
u64
test (u64 a, u64 b)
{
  return (a & mask) | (b & ~mask);
}

template u64 test<0x0000'0000'0000'00f0l> (u64, u64);
template u64 test<0x0000'0000'0000'fff0l> (u64, u64);
template u64 test<0x0000'0000'ffff'fff0l> (u64, u64);
template u64 test<0x0000'ffff'ffff'fff0l> (u64, u64);
template u64 test<0xffff'ffff'ffff'fff0l> (u64, u64);
