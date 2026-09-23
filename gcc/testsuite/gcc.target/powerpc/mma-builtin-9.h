/* Header file for mma-builtin-9.c test - contains test functions only */

typedef unsigned char  vec_t __attribute__((vector_size(16)));

void
foo (__vector_pair *dst, vec_t *src)
{
  __vector_pair pair;
  /* Adjacent loads should be combined into one lxvp instruction.  */
  __builtin_vsx_build_pair (&pair, src[0], src[1]);
  *dst = pair;
}

void
bar (__vector_quad *dst, vec_t *src)
{
  __vector_quad quad;
  /* Adjacent loads should be combined into two lxvp instructions.  */
  __builtin_mma_build_acc (&quad, src[0], src[1], src[2], src[3]);
  *dst = quad;
}
