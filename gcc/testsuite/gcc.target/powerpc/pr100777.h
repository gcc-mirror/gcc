/* Header file for pr100777.c test - contains test functions only */

/* PR target/100777 */

/* Verify we do not ICE on the following.  */

void
foo (__vector_quad *dst)
{
#pragma omp parallel
  {
    __builtin_mma_xxsetaccz (dst);
  }
}

void
bar (__vector_quad *dst, __vector_quad *src)
{
#pragma omp parallel
  {
    __builtin_mma_disassemble_acc (dst, src);
  }
}
