/* Header file for pr96446.c test - contains test functions only */

/* PR target/96466 */

/* Verify we do not ICE on the following.  */

extern void bar0 (void);
void
foo0 (__vector_quad *dst)
{
  __vector_quad acc;
  __builtin_mma_xxsetaccz (&acc);
  bar0 ();
  *dst = acc;
}
