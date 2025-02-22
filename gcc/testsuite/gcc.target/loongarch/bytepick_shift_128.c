/* { dg-do compile } */
/* { dg-options "-O2 -march=loongarch64 -mabi=lp64d" } */
/* { dg-final { scan-assembler "bytepick\\.d" } } */

__int128
test (__int128 a)
{
  return a << 16;
}
