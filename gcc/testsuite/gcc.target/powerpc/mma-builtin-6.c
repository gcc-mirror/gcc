/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-Wno-psabi -mdejagnu-cpu=power10 -O2" } */

void
foo (__vector_quad *dst)
{
  __vector_quad acc;
  __builtin_mma_xxsetaccz (&acc);
  *dst = acc;
}

/* { dg-final { scan-assembler-not {\mlxv\M} } } */
/* { dg-final { scan-assembler-not {\mlxvp\M} } } */
/* { dg-final { scan-assembler-not {\mxxmtacc\M} } } */
/* { dg-final { scan-assembler-times {\mxxsetaccz\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxxmfacc\M} 1 } } */
/* { dg-final { scan-assembler-times {\mstxvp\M} 2 } } */
