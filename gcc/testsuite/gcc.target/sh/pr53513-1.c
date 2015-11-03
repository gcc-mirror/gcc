/* Check that fpchg is used to switch FPSCR.PR mode on SH4A.  */
/* { dg-do compile { target { has_fpchg } } }  */
/* { dg-additional-options "-O" }  */
/* { dg-final { scan-assembler "fpchg" } }  */
/* { dg-final { scan-assembler-not "fpscr" } }  */

double
foo (float a, float b, double c)
{
  return (a * b) + c;
}
