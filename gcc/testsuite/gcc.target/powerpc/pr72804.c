/* { dg-do compile { target { lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -mvsx" } */

__int128_t
foo (__int128_t *src)
{
  return ~*src;
}

void
bar (__int128_t *dst, __int128_t src)
{
  *dst =  ~src;
}

/* { dg-final { scan-assembler-times "not " 4 } } */
/* { dg-final { scan-assembler-times "std " 2 } } */
/* { dg-final { scan-assembler-times "ld " 2 } } */
/* { dg-final { scan-assembler-not "lxvd2x" } } */
/* { dg-final { scan-assembler-not "stxvd2x" } } */
/* { dg-final { scan-assembler-not "xxpermdi" } } */
/* { dg-final { scan-assembler-not "mfvsrd" } } */
/* { dg-final { scan-assembler-not "mfvsrd" } } */
