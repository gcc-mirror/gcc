/* Check that the option -mdiv=call-fp results in the corresponding library
   function calls on targets that have a double precision FPU.  */
/* { dg-do compile { target { double_fpu || use_single_only_fpu } } }  */
/* { dg-options "-mdiv=call-fp" }  */
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
