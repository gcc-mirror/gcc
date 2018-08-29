/* { dg-do compile } */
/* { dg-options "-O2 -mcmodel=large -mpc-relative-literal-loads" } */
/* { dg-require-effective-target lp64 } */
/* { dg-skip-if "-mcmodel=large, no support for -fpic" { aarch64-*-* }  { "-fpic" } { "" } } */

__int128
t (void)
{
  return ((__int128)0x123456789abcdef << 64) | 0xfedcba987654321;
}

/* { dg-final { scan-assembler "adr" } } */
/* { dg-final { scan-assembler-not "adrp" } } */
