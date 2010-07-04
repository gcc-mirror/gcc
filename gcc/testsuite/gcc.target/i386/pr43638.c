/* PR target/43638 */
/* { dg-do compile } */

void
foo (void)
{
  int x;
  __asm __volatile ("mov $0,%e0" : "=r" (x));	/* { dg-error "invalid operand code" } */
}
