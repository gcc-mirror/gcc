/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-Wno-psabi -mdejagnu-cpu=power10 -O2" } */

void
foo0 (void)
{
  __vector_quad acc;
  asm ("#..." : "=d" (acc));
  __builtin_mma_xxmtacc (&acc);
  __builtin_mma_xxmfacc (&acc);
  asm ("#..." :: "d" (acc));
}

typedef unsigned char  vec_t __attribute__((vector_size(16)));

void
foo1 (vec_t *vec)
{
  vec[1] = __builtin_vsx_xvcvspbf16 (vec[0]);
  vec[3] = __builtin_vsx_xvcvbf16sp (vec[2]);
}

/* { dg-final { scan-assembler-times {\mxxmtacc\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxxmfacc\M} 1 } } */
/* { dg-final { scan-assembler-times {\mlxv\M} 2 } } */
/* { dg-final { scan-assembler-times {\mstxv\M} 2 } } */
/* { dg-final { scan-assembler-not {\mlxvp\M} } } */
/* { dg-final { scan-assembler-not {\mstxvp\M} } } */
/* { dg-final { scan-assembler-times {\mxvcvspbf16\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvcvbf16sp\M} 1 } } */
