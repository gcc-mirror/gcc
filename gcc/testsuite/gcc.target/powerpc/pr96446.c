/* PR target/96466 */
/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

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
