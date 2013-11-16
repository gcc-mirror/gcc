/* Check that the option -mdiv=call-fp results in the corresponding library
   function calls on targets that have a double precision FPU.  */
/* { dg-do compile }  */
/* { dg-options "-mdiv=call-fp" }  */
/* { dg-skip-if "" { "sh*-*-*" } { "*"} { "-m2a" "-m4" "-m4a" "*single-only" } }  */
/* { dg-final { scan-assembler "sdivsi3_i4\n" } }  */
/* { dg-final { scan-assembler "udivsi3_i4\n" } }  */

int
test00 (int a, int b)
{
  return a / b;
}

unsigned int
test01 (unsigned int a, unsigned b)
{
  return a / b;
}
