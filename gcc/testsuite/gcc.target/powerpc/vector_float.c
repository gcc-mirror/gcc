/* { dg-do compile } */
/* { dg-options "-O2 -mdejagnu-cpu=power9 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */
/* { dg-require-effective-target lp64 } */

vector float
test (float *a, float *b, float *c, float *d)
{
  return (vector float){*a, *b, *c, *d};
}

/* { dg-final { scan-assembler-not {\mlxssp} } } */
/* { dg-final { scan-assembler-not {\mlfs} } } */
/* { dg-final { scan-assembler-times {\mlwz\M} 4 } } */
/* { dg-final { scan-assembler-times {\mrldimi\M} 2 } } */
/* { dg-final { scan-assembler-times {\mmtvsrdd\M} 1 } } */
