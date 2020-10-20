/* { dg-do compile { target lp64 } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -O2" } */

vector int
splat (int a)
{
  return (vector int) { a, a, a, a };
}

/* { dg-final { scan-assembler "mtvsrwz" } } */
/* { dg-final { scan-assembler "xxspltw" } } */
