/* Verify that 387 fptan instruction is generated. Also check fptan
   peephole2 optimizer.  */
/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O2 -ffast-math -march=i686" } */
/* { dg-final { scan-assembler "fptan" } } */
/* { dg-final { scan-assembler-not "fld1" } } */

extern double tan (double);

double f1(double x)
{
  return 1.0 / tan(x);
}

