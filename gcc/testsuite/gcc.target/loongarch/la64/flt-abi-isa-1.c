/* { dg-do compile } */
/* { dg-options "-mabi=lp64d -mfpu=64 -march=loongarch64 -O2" } */
/* { dg-final { scan-assembler "frecip\\.d" } } */
/* { dg-final { scan-assembler-not "movgr2fr\\.d" } } */
/* { dg-final { scan-assembler-not "movfr2gr\\.d" } } */

/* FPU is used for calculation and FPR is used for arguments and return
   values.  */

double
t (double x)
{
  return 1.0 / x;
}
