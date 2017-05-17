/* PR target/79559 */
/* { dg-do compile } */

void
foo (int x)
{
  __asm__ volatile ("# %K0" : : "r" (x));	/* { dg-error "invalid operand code" } */
  __asm__ volatile ("# %r0" : : "r" (x));	/* { dg-error "invalid operand code" } */
  __asm__ volatile ("# %r0" : : "n" (129));	/* { dg-error "invalid operand code" } */
  __asm__ volatile ("# %R0" : : "r" (x));	/* { dg-error "invalid operand code" } */
}
