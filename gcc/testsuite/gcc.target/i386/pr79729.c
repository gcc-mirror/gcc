/* PR target/79729 */
/* { dg-do compile } */

void
foo (int x)
{
  __asm__ volatile ("# %R0" : : "n" (129));	/* { dg-error "invalid operand code" } */
}
