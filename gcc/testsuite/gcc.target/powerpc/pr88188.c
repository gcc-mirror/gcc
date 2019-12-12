/* PR target/88188 */
/* { dg-do compile } */

int m;

void
foo (void)
{
  __asm volatile ("%D0" : : "m" (m));	/* { dg-error "invalid %D value" } */
  __asm volatile ("%t0" : : "m" (m));	/* { dg-error "invalid %t value" } */
  __asm volatile ("%V0" : : "r" (0));	/* { dg-error "invalid %V value" } */
  __asm volatile ("%z0" : : "r" (0));	/* { dg-error "invalid %z value" } */
}
