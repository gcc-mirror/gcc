/* PR target/88188 */
/* { dg-do compile } */

void
foo (void)
{
  __asm volatile ("%D0" : : "r" (0));	/* { dg-error "invalid %D value" } */
}
