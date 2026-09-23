/* Header file for mma-builtin-1.c test - contains test functions only */

typedef unsigned char  vec_t __attribute__((vector_size(16)));

void
foo0 (__vector_quad *dst, vec_t *vec)
{
  __vector_quad acc;
  vec_t vec0 = vec[0];
  vec_t vec1 = vec[1];

  __builtin_mma_xvi4ger8 (&acc, vec0, vec1);
  __builtin_mma_xvi4ger8pp (&acc, vec0, vec1);
  dst[0] = acc;
}

void
foo1 (__vector_quad *dst, vec_t *vec)
{
  __vector_quad acc;
  vec_t vec0 = vec[0];
  vec_t vec1 = vec[1];

  __builtin_mma_xvi8ger4 (&acc, vec0, vec1);
  __builtin_mma_xvi8ger4pp (&acc, vec0, vec1);
  __builtin_mma_xvi8ger4spp(&acc, vec0, vec1);
  dst[1] = acc;
}

void
foo2 (__vector_quad *dst, vec_t *vec)
{
  __vector_quad acc;
  vec_t vec0 = vec[0];
  vec_t vec1 = vec[1];

  __builtin_mma_xvi16ger2 (&acc, vec0, vec1);
  __builtin_mma_xvi16ger2pp (&acc, vec0, vec1);
  dst[2] = acc;
}

void
foo3 (__vector_quad *dst, vec_t *vec)
{
  __vector_quad acc;
  vec_t vec0 = vec[0];
  vec_t vec1 = vec[1];

  __builtin_mma_xvi16ger2s (&acc, vec0, vec1);
  __builtin_mma_xvi16ger2spp (&acc, vec0, vec1);
  dst[3] = acc;
}

void
foo4 (__vector_quad *dst, vec_t *vec)
{
  __vector_quad acc;
  vec_t vec0 = vec[0];
  vec_t vec1 = vec[1];

  __builtin_mma_xvf16ger2 (&acc, vec0, vec1);
  __builtin_mma_xvf16ger2pp (&acc, vec0, vec1);
  __builtin_mma_xvf16ger2pn (&acc, vec0, vec1);
  dst[4] = acc;
}

void
foo4b (__vector_quad *dst, __vector_quad *src, vec_t *vec)
{
  __vector_quad acc;
  vec_t vec0 = vec[0];
  vec_t vec1 = vec[1];

  acc = src[0];
  __builtin_mma_xvf16ger2np (&acc, vec0, vec1);
  __builtin_mma_xvf16ger2nn (&acc, vec0, vec1);
  dst[4] = acc;
}

void
foo5 (__vector_quad *dst, vec_t *vec)
{
  __vector_quad acc;
  vec_t vec0 = vec[0];
  vec_t vec1 = vec[1];

  __builtin_mma_xvbf16ger2 (&acc, vec0, vec1);
  __builtin_mma_xvbf16ger2pp (&acc, vec0, vec1);
  __builtin_mma_xvbf16ger2pn (&acc, vec0, vec1);
  dst[5] = acc;
}

void
foo5b (__vector_quad *dst, __vector_quad *src, vec_t *vec)
{
  __vector_quad acc;
  vec_t vec0 = vec[0];
  vec_t vec1 = vec[1];

  acc = src[0];
  __builtin_mma_xvbf16ger2np (&acc, vec0, vec1);
  __builtin_mma_xvbf16ger2nn (&acc, vec0, vec1);
  dst[5] = acc;
}

void
foo6 (__vector_quad *dst, vec_t *vec)
{
  __vector_quad acc;
  vec_t vec0 = vec[0];
  vec_t vec1 = vec[1];

  __builtin_mma_xvf32ger (&acc, vec0, vec1);
  __builtin_mma_xvf32gerpp (&acc, vec0, vec1);
  __builtin_mma_xvf32gerpn (&acc, vec0, vec1);
  dst[6] = acc;
}

void
foo6b (__vector_quad *dst, __vector_quad *src, vec_t *vec)
{
  __vector_quad acc;
  vec_t vec0 = vec[0];
  vec_t vec1 = vec[1];

  acc = src[0];
  __builtin_mma_xvf32gernp (&acc, vec0, vec1);
  __builtin_mma_xvf32gernn (&acc, vec0, vec1);
  dst[6] = acc;
}

void
foo7 (__vector_quad *dst, vec_t *vec)
{
  __vector_quad acc;
  vec_t vec0 = vec[0];
  vec_t vec1 = vec[1];

  __builtin_mma_pmxvi4ger8 (&acc, vec0, vec1, 15, 15, 255);
  __builtin_mma_pmxvi4ger8pp (&acc, vec0, vec1, 15, 15, 255);
  dst[7] = acc;
}

void
foo8 (__vector_quad *dst, vec_t *vec)
{
  __vector_quad acc;
  vec_t vec0 = vec[0];
  vec_t vec1 = vec[1];

  __builtin_mma_pmxvi8ger4 (&acc, vec0, vec1, 15, 15, 15);
  __builtin_mma_pmxvi8ger4pp (&acc, vec0, vec1, 15, 15, 15);
  __builtin_mma_pmxvi8ger4spp(&acc, vec0, vec1, 15, 15, 15);
  dst[8] = acc;
}

void
foo9 (__vector_quad *dst, vec_t *vec)
{
  __vector_quad acc;
  vec_t vec0 = vec[0];
  vec_t vec1 = vec[1];

  __builtin_mma_pmxvi16ger2 (&acc, vec0, vec1, 15, 15, 3);
  __builtin_mma_pmxvi16ger2pp (&acc, vec0, vec1, 15, 15, 3);
  dst[9] = acc;
}

void
foo10 (__vector_quad *dst, vec_t *vec)
{
  __vector_quad acc;
  vec_t vec0 = vec[0];
  vec_t vec1 = vec[1];

  __builtin_mma_pmxvi16ger2s (&acc, vec0, vec1, 15, 15, 3);
  __builtin_mma_pmxvi16ger2spp (&acc, vec0, vec1, 15, 15, 3);
  dst[10] = acc;
}

void
foo11 (__vector_quad *dst, vec_t *vec)
{
  __vector_quad acc;
  vec_t vec0 = vec[0];
  vec_t vec1 = vec[1];

  __builtin_mma_pmxvf16ger2 (&acc, vec0, vec1, 15, 15, 3);
  __builtin_mma_pmxvf16ger2pp (&acc, vec0, vec1, 15, 15, 3);
  __builtin_mma_pmxvf16ger2pn (&acc, vec0, vec1, 15, 15, 3);
  dst[11] = acc;
}

void
foo11b (__vector_quad *dst, __vector_quad *src, vec_t *vec)
{
  __vector_quad acc;
  vec_t vec0 = vec[0];
  vec_t vec1 = vec[1];

  acc = src[0];
  __builtin_mma_pmxvf16ger2np (&acc, vec0, vec1, 15, 15, 3);
  __builtin_mma_pmxvf16ger2nn (&acc, vec0, vec1, 15, 15, 3);
  dst[11] = acc;
}

void
foo12 (__vector_quad *dst, vec_t *vec)
{
  __vector_quad acc;
  vec_t vec0 = vec[0];
  vec_t vec1 = vec[1];

  __builtin_mma_pmxvbf16ger2 (&acc, vec0, vec1, 15, 15, 3);
  __builtin_mma_pmxvbf16ger2pp (&acc, vec0, vec1, 15, 15, 3);
  __builtin_mma_pmxvbf16ger2pn (&acc, vec0, vec1, 15, 15, 3);
  dst[12] = acc;
}

void
foo12b (__vector_quad *dst, __vector_quad *src, vec_t *vec)
{
  __vector_quad acc;
  vec_t vec0 = vec[0];
  vec_t vec1 = vec[1];

  acc = src[0];
  __builtin_mma_pmxvbf16ger2np (&acc, vec0, vec1, 15, 15, 3);
  __builtin_mma_pmxvbf16ger2nn (&acc, vec0, vec1, 15, 15, 3);
  dst[12] = acc;
}

void
foo13 (__vector_quad *dst, vec_t *vec)
{
  __vector_quad acc;
  vec_t vec0 = vec[0];
  vec_t vec1 = vec[1];

  __builtin_mma_pmxvf32ger (&acc, vec0, vec1, 15, 15);
  __builtin_mma_pmxvf32gerpp (&acc, vec0, vec1, 15, 15);
  __builtin_mma_pmxvf32gerpn (&acc, vec0, vec1, 15, 15);
  dst[13] = acc;
}

void
foo13b (__vector_quad *dst, __vector_quad *src, vec_t *vec)
{
  __vector_quad acc;
  vec_t vec0 = vec[0];
  vec_t vec1 = vec[1];

  acc = src[0];
  __builtin_mma_pmxvf32gernp (&acc, vec0, vec1, 15, 15);
  __builtin_mma_pmxvf32gernn (&acc, vec0, vec1, 15, 15);
  dst[13] = acc;
}
