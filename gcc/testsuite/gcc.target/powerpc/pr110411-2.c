/* PR target/110411 */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

/* Verify we do not ICE on the following.  */

void
bug (__vector_quad *dst)
{
  dst = (__vector_quad *)((unsigned long)dst & ~0xFUL);
  __builtin_mma_xxsetaccz (dst);
}
