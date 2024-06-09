/* Ensure prologues are not generated for noreturn functions.  */
/* { dg-do assemble } */
/* { dg-options "-Os" } */
/* { dg-final { object-size text == 4 } } */

void test(void)
{
  asm volatile ("# \n\t" : : : "r0", "r9");
  for (;;)
    ;
}
