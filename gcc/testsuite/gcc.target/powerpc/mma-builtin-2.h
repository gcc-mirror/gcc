/* Header file for mma-builtin-2.c test - contains test functions only */

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
