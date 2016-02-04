/* { dg-do compile } */
/* There should be 3 occurrences of .LC0 in the code:
   one for the definition of "0",
   one for use in test2().
   The occurrence in test1 is collapsed to an integer constant
   FIXME: At the moment m68k GCC does not optimize test1() to nop
   for some reason.  */
/* { dg-final { scan-assembler-times ".LC0" 2 } } */

void dummy(char *arg);

void test1(void)
{
  char tmp[2] = "0";
}

void test2(void)
{
  dummy("0");
}
