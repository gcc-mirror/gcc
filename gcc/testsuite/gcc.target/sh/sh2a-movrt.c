/* Testcase to check generation of a SH2A specific instruction for
   'MOVRT Rn'.  */
/* { dg-do compile { target { sh2a } } }  */
/* { dg-options "-O1" }  */
/* { dg-final { scan-assembler "movrt" } }  */

int
foo (void)
{
  int a, b, g, stop;
  if (stop = ((a + b) % 2 != g))
    ;
  return stop;
}
