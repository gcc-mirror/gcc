/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-Wno-psabi -mdejagnu-cpu=power10 -O2" } */

typedef unsigned char vec_t __attribute__((vector_size(16)));

void
foo (__vector_pair *dst, vec_t *src)
{
  __vector_pair pair;
  __builtin_mma_assemble_pair (&pair, src[0], src[4]);
  *dst = pair;
}

void
bar (vec_t *dst, __vector_pair *src)
{
  vec_t res[2];
  __builtin_mma_disassemble_pair (res, src);
  dst[0] = res[0];
  dst[4] = res[1];
}

/* { dg-final { scan-assembler-times {\mlxv\M} 2 } } */
/* { dg-final { scan-assembler-times {\mlxvp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mstxv\M} 2 } } */
/* { dg-final { scan-assembler-times {\mstxvp\M} 1 } } */

