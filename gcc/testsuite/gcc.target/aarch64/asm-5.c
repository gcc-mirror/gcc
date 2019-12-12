/* PR target/87598 */
/* { dg-do compile } */

void
foo (void)
{
  __asm__ ("# %a0" : : "i" (0));
}
