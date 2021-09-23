/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

void
foo (__vector_pair *dst, __vector_pair *src, long idx)
{
  dst[0] = __builtin_vsx_lxvp (0, src);
  dst[2] = __builtin_vsx_lxvp (32, src);
  dst[4] = __builtin_vsx_lxvp (64, src);
  /* Non-constant offset should generate a lxvpx.  */
  dst[6] = __builtin_vsx_lxvp (idx, src);
  /* Non-aligned offset should generate a plxvp.  */
  dst[8] = __builtin_vsx_lxvp (257, src);
}

#if !__has_builtin (__builtin_vsx_lxvp)
#  error "__has_builtin (__builtin_vsx_lxvp) failed"
#endif

/* { dg-final { scan-assembler-not {\mlxv\M} } } */
/* { dg-final { scan-assembler-not {\mstxv\M} } } */
/* { dg-final { scan-assembler-times {\mlxvp\M} 3 } } */
/* { dg-final { scan-assembler-times {\mlxvpx\M} 1 } } */
/* { dg-final { scan-assembler-times {\mplxvp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mstxvp\M} 5 } } */
