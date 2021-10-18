/* PR target/102761 */
/* { dg-do compile } */
/* { dg-options "-O1" } */

int foo (void);

void
bar (void)
{
  asm volatile ("%a0" : : "X"(foo () ? 2 : 1)); /* { dg-error "invalid constraints for operand" } */
}
