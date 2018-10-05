/* PR target/79804 */
/* { dg-do compile } */
/* { dg-options "" } */

void foo (void)
{
  register int r19 asm ("19");

  asm volatile ("# %0" : "=r"(r19));  /* { dg-error "invalid use of register" } */
}  /* { dg-error "cannot be used in asm here" } */
