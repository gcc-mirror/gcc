/* Test error conditions of asm flag outputs.  */
/* { dg-do compile } */
/* { dg-options "" } */

void test(void)
{
  char x;
  asm("# %0" : "=@ccz"(x)); /* { dg-error "invalid use of asm flag output" } */
}
