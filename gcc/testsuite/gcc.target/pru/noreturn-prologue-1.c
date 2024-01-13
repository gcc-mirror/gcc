/* Ensure prologues are not generated for noreturn functions.  */
/* { dg-do assemble } */
/* { dg-options "-Os" } */
/* { dg-final { object-size text == 0 } } */

void test(void)
{
  asm volatile ("# \n\t" : : : "r5", "r10");
  __builtin_unreachable ();
}
