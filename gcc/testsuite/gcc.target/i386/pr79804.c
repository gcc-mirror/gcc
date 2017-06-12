/* PR target/79804 */
/* { dg-do compile } */
/* { dg-options "" } */

void foo (void)
{
  register int r20 asm ("20");

  asm volatile ("# %0" : "=r"(r20));  /* { dg-error "invalid use of register" } */
}  /* { dg-error "cannot be used in asm here" } */
