/* { dg-options "-O3 -mcpu=v6.00.a -msoft-float" } */

volatile float f1, f2, f3;

void float_func () 
{
  /* { dg-final { scan-assembler-not "fmul" } } */
    f1 = f2 * f3;

  /* { dg-final { scan-assembler-not "fadd" } } */
    f1 = f2 + f3;

  /* { dg-final { scan-assembler-not "frsub" } } */
    f1 = f2 - f3;

  /* { dg-final { scan-assembler-not "fdiv" } } */
    f1 = f2 / f3;

}
