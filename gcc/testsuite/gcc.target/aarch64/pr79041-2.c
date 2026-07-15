/* { dg-do compile } */
/* { dg-options "-O2 -mcmodel=large" } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target aarch64_mcmodel_large } */

__int128
t (void)
{
  return ((__int128)0x123456789abcdef << 64) | 0xfedcba987654321;
}

/* { dg-final { scan-assembler "adrp" } } */
