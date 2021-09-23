/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-Wno-psabi -mdejagnu-cpu=power10 -O2" } */

typedef unsigned char vec_t __attribute__((vector_size(16)));

void
foo (__vector_quad *dst, vec_t *src)
{
  __vector_quad acc;
  __builtin_mma_assemble_acc (&acc, src[0], src[4], src[8], src[12]);
  *dst = acc;
}

void
foo2 (__vector_quad *dst, vec_t *src)
{
  __vector_quad acc;
  __builtin_mma_build_acc (&acc, src[12], src[8], src[4], src[0]);
  *dst = acc;
}

void
bar (vec_t *dst, __vector_quad *src)
{
  vec_t res[4];
  __builtin_mma_disassemble_acc (res, src);
  dst[0] = res[0];
  dst[4] = res[1];
  dst[8] = res[2];
  dst[12] = res[3];
}

#if !__has_builtin (__builtin_mma_assemble_acc)
#  error "__has_builtin (__builtin_mma_assemble_acc) failed"
#endif

#if !__has_builtin (__builtin_mma_build_acc)
#  error "__has_builtin (__builtin_mma_build_acc) failed"
#endif

/* { dg-final { scan-assembler-times {\mlxv\M} 8 } } */
/* { dg-final { scan-assembler-times {\mlxvp\M} 2 } } */
/* { dg-final { scan-assembler-times {\mstxv\M} 4 } } */
/* { dg-final { scan-assembler-times {\mstxvp\M} 4 } } */
/* { dg-final { scan-assembler-times {\mxxmfacc\M} 3 } } */
/* { dg-final { scan-assembler-times {\mxxmtacc\M} 3 } } */
