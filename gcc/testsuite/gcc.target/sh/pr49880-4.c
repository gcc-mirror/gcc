/* Check that the option -mdiv=call-fp does not produce calls to the
   library function that uses FPU to implement integer division if FPU insns
   are not supported or are disabled.  */
/* { dg-do compile }  */
/* { dg-options "-mdiv=call-fp" }  */
/* { dg-skip-if "" { "sh*-*-*" } { "*"} { "-m1" "-m2" "-m3" "-m4al" "*nofpu" "-m4-340*" "-m4-400*" "-m4-500*" } }  */
/* { dg-final { scan-assembler-not "sdivsi3_i4\n|udivsi3_i4\n" } }  */

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
