/* { dg-do compile { target { lp64 } } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -mvsx" } */

__int128_t
foo (__int128_t *src)
{
  return ~*src;
}


/* { dg-final { scan-assembler-times {\mld\M} 2 } } */
/* { dg-final { scan-assembler-times {\mnot\M} 2 } } */
/* { dg-final { scan-assembler-not {\mlxvd2x\M} } } */
