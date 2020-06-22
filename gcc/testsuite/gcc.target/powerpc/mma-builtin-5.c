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
bar (vec_t *dst, __vector_quad *src)
{
  vec_t res[4];
  __builtin_mma_disassemble_acc (res, src);
  dst[0] = res[0];
  dst[4] = res[1];
  dst[8] = res[2];
  dst[12] = res[3];
}

/* { dg-final { scan-assembler-times {\mlxv\M} 4 } } */
/* { dg-final { scan-assembler-times {\mlxvp\M} 2 } } */
/* { dg-final { scan-assembler-times {\mstxv\M} 4 } } */
/* { dg-final { scan-assembler-times {\mstxvp\M} 2 } } */
/* { dg-final { scan-assembler-times {\mxxmfacc\M} 2 } } */
/* { dg-final { scan-assembler-times {\mxxmtacc\M} 2 } } */
