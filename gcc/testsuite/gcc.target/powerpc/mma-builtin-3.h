/* Header file for mma-builtin-3.c test - contains test functions only */

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
  vec[3] = __builtin_vsx_xvcvbf16spn (vec[2]);
}
