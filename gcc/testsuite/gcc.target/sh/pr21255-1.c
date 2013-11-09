/* { dg-do compile }  */
/* { dg-options "-O2 -fomit-frame-pointer" } */
/* { dg-final { scan-assembler "mov fr4,fr.; mov fr5,fr." { target sh-*-* } } } */
/* { dg-final { scan-assembler "mov fr4,fr.; mov fr5,fr." { target sh[1234lb]*-*-* } } } */
/* { dg-final { scan-assembler "mov fr0,fr.; mov fr1,fr." { target sh[56]*-*-* } } } */
double
f (double d)
{
  double r;

#if defined (__SH_FPU_DOUBLE__)
  asm ("mov %S1,%S0; mov %R1,%R0" : "=f" (r) : "f" (d));
#else
  asm ("mov fr4,fr4; mov fr5,fr5");
#endif
  return r;
}
