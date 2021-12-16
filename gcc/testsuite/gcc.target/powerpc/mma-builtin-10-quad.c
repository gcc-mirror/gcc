/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

typedef unsigned char  vec_t __attribute__((vector_size(16)));

void
foo (__vector_quad *dst, vec_t *src)
{
  __vector_quad quad0, quad1;
  /* Adjacent loads should be combined into two lxvp instructions.
     and identical build accs should not be combined.  */
  __builtin_mma_build_acc (&quad0, src[0], src[1], src[2], src[3]);
  __builtin_mma_build_acc (&quad1, src[0], src[1], src[2], src[3]);
  dst[0] = quad0;
  dst[2] = quad1;
}

/* { dg-final { scan-assembler-not {\mlxv\M} } } */
/* { dg-final { scan-assembler-not {\mstxv\M} } } */
/* { dg-final { scan-assembler-times {\mlxvp\M} 4 } } */
/* { dg-final { scan-assembler-times {\mxxmtacc\M} 2 } } */
/* { dg-final { scan-assembler-times {\mxxmfacc\M} 2 } } */
/* { dg-final { scan-assembler-times {\mstxvp\M} 4 } } */
