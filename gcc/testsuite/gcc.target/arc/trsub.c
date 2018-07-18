/* Tests if we generate rsub instructions when compiling using
   floating point assist instructions.  */
/* { dg-do compile } */
/* { dg-skip-if "" { ! { clmcpu } } } */
/* { dg-options "-mfpu=fpuda -mcpu=arcem" } */

double foo (double a)
{
  return ((double) 0.12 - a);
}
/* { dg-final { scan-assembler-not "drsub.*" } } */
