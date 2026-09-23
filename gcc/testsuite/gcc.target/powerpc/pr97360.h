/* Header file for pr97360.c test - contains test functions only */

/* PR target/97360 */

/* Verify we do not ICE on the test below.  */

typedef unsigned char vec_t __attribute__((vector_size(16)));

void
foo (__vector_quad *dst, __vector_pair *vpair, vec_t *vec)
{
  __vector_quad acc = *dst;
  for (;;)
    {
      __builtin_mma_xvf64gerpp(&acc, *vpair, vec[7]);
    }
}
