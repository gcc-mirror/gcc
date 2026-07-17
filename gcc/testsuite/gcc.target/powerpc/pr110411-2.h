/* Header file for pr110411-2.c test - contains test functions only */

/* PR target/110411 */

/* Verify we do not ICE on the following.  */

void
bug (__vector_quad *dst)
{
  dst = (__vector_quad *)((unsigned long)dst & ~0xFUL);
  __builtin_mma_xxsetaccz (dst);
}
