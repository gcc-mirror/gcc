/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

void
foo (__vector_pair *dst, __vector_pair *src, long idx)
{
  __vector_pair pair = *src;
  __builtin_vsx_stxvp (pair, 0, dst);
  __builtin_vsx_stxvp (pair, 32, dst);
  __builtin_vsx_stxvp (pair, 64, dst);
  /* Non-constant offset should generate a stxvpx.  */
  __builtin_vsx_stxvp (pair, idx, dst);
  /* Non-aligned offset should generate a pstxvp.  */
  __builtin_vsx_stxvp (pair, 257, dst);
}

#if !__has_builtin (__builtin_vsx_stxvp)
#  error "__has_builtin (__builtin_vsx_stxvp) failed"
#endif

/* { dg-final { scan-assembler-not {\mlxv\M} } } */
/* { dg-final { scan-assembler-not {\mstxv\M} } } */
/* { dg-final { scan-assembler-times {\mlxvp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mstxvp\M} 3 } } */
/* { dg-final { scan-assembler-times {\mstxvpx\M} 1 } } */
/* { dg-final { scan-assembler-times {\mpstxvp\M} 1 } } */
