/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-Wno-psabi -mdejagnu-cpu=power10 -O2" } */

typedef unsigned char  vec_t __attribute__((vector_size(16)));

void
foo0 (__vector_quad *dst, vec_t *vec, __vector_pair *pvecp)
{
  __vector_quad acc;
  __vector_pair vecp0 = *pvecp;
  vec_t vec1 = vec[1];

  __builtin_mma_xvf64ger (&acc, vecp0, vec1);
  __builtin_mma_xvf64gerpp (&acc, vecp0, vec1);
  __builtin_mma_xvf64gerpn (&acc, vecp0, vec1);
  dst[0] = acc;
}

void
foo1 (__vector_quad *dst, __vector_quad *src, vec_t *vec, __vector_pair *pvecp)
{
  __vector_quad acc;
  __vector_pair vecp0 = *pvecp;
  vec_t vec1 = vec[1];

  acc = src[0];
  __builtin_mma_xvf64gernp (&acc, vecp0, vec1);
  __builtin_mma_xvf64gernn (&acc, vecp0, vec1);
  dst[0] = acc;
}

void
foo2 (__vector_quad *dst, vec_t *vec, __vector_pair *pvecp)
{
  __vector_quad acc;
  __vector_pair vecp0 = *pvecp;
  vec_t vec1 = vec[1];
  __builtin_mma_pmxvf64ger (&acc, vecp0, vec1, 15, 3);
  __builtin_mma_pmxvf64gerpp (&acc, vecp0, vec1, 15, 3);
  __builtin_mma_pmxvf64gerpn (&acc, vecp0, vec1, 15, 3);
  dst[1] = acc;
}

void
foo3 (__vector_quad *dst, __vector_quad *src, vec_t *vec, __vector_pair *pvecp)
{
  __vector_quad acc;
  __vector_pair vecp0 = *pvecp;
  vec_t vec1 = vec[1];

  acc = src[0];
  __builtin_mma_pmxvf64gernp (&acc, vecp0, vec1, 15, 3);
  __builtin_mma_pmxvf64gernn (&acc, vecp0, vec1, 15, 3);
  dst[1] = acc;
}

/* { dg-final { scan-assembler-times {\mxxmfacc\M} 4 } } */
/* { dg-final { scan-assembler-times {\mxxmtacc\M} 2 } } */
/* { dg-final { scan-assembler-times {\mlxv\M} 4 } } */
/* { dg-final { scan-assembler-times {\mlxvp\M} 8 } } */
/* { dg-final { scan-assembler-times {\mstxvp\M} 8 } } */
/* { dg-final { scan-assembler-times {\mxvf64ger\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvf64gerpp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvf64gerpn\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvf64gernp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvf64gernn\M} 1 } } */
/* { dg-final { scan-assembler-times {\mpmxvf64ger\M} 1 } } */
/* { dg-final { scan-assembler-times {\mpmxvf64gerpp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mpmxvf64gerpn\M} 1 } } */
/* { dg-final { scan-assembler-times {\mpmxvf64gernp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mpmxvf64gernn\M} 1 } } */
