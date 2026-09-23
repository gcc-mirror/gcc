/* Header file for mma-builtin-6.c test - contains test functions only */

void
foo (__vector_quad *dst)
{
  __vector_quad acc0, acc1;
  __builtin_mma_xxsetaccz (&acc0);
  __builtin_mma_xxsetaccz (&acc1);
  dst[0] = acc0;
  dst[1] = acc1;
}
