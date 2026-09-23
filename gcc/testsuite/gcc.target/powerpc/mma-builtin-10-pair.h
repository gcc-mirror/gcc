/* Header file for mma-builtin-10-pair.c test - contains test functions only */

typedef unsigned char  vec_t __attribute__((vector_size(16)));

void
foo (__vector_pair *dst, vec_t *src)
{
  __vector_pair pair0, pair1;
  /* Adjacent loads should be combined into one lxvp instruction
     and identical build pairs should be combined.  */
  __builtin_vsx_build_pair (&pair0, src[0], src[1]);
  __builtin_vsx_build_pair (&pair1, src[0], src[1]);
  dst[0] = pair0;
  dst[2] = pair1;
}
