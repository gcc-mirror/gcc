/* PR middle-end/67653 */
/* { dg-do compile } */

void
foo (void)
{
  __asm__ ("" : : "m" (({ static int a; a; })));	/* { dg-warning "memory input 0 is not directly addressable" } */
}
