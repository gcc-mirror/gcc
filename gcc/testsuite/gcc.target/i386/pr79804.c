/* PR target/79804 */
/* { dg-do compile } */
/* { dg-options "" } */

void foo (void)
{
  register int r19 asm ("19");	/* { dg-error "register specified for 'r19' is an internal GCC implementation detail" } */

  asm volatile ("# %0" : "=r"(r19));
}
