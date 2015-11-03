/* Check that the option -mdiv=call-fp does not produce calls to the
   library function that uses FPU to implement integer division if FPU insns
   are not supported or are disabled.  */
/* { dg-do compile { target { no_fpu } } }  */
/* { dg-options "-mdiv=call-fp" }  */
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
