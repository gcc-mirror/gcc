// PR c++/98688
// { dg-do compile { target powerpc*-*-* } }
// { dg-additional-options "-fmodules-ts -mcpu=power10 -mmma" }

export module mma_foo0;

typedef unsigned char  vec_t __attribute__((vector_size(16)));

export void
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
