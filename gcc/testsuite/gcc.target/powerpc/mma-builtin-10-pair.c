/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

typedef unsigned char  vec_t __attribute__((vector_size(16)));

void
foo (__vector_pair *dst, vec_t *src)
{
  __vector_pair pair0, pair1;
  /* Adjacent loads should be combined into one lxvp instruction
     and identical build pairs should be combined.  */
  __builtin_vsx_build_pair (&pair0, src[0], src[1]);
  __builtin_vsx_build_pair (&pair1, src[0], src[1]);
  dst[0] = pair0;
  dst[2] = pair1;
}

/* { dg-final { scan-assembler-not {\mlxv\M} } } */
/* { dg-final { scan-assembler-not {\mstxv\M} } } */
/* { dg-final { scan-assembler-times {\mlxvp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mstxvp\M} 2 } } */
