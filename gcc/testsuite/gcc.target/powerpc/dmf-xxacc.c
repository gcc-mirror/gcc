/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=future -O2" } */
/* { dg-require-effective-target powerpc_future_compile_ok } */

void
foo (__vector_quad *acc)
{
  __builtin_mma_xxmtacc (acc);
}

void
bar (__vector_quad *acc)
{
  __builtin_mma_xxmfacc (acc);
}

/* { dg-final { scan-assembler-not {\mxxmtacc\M} } } */
/* { dg-final { scan-assembler-not {\mxxmfacc\M} } } */
