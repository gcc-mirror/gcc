/* { dg-do compile { target aarch64*-*-* arm*-*-* powerpc*-*-* riscv*-*-* s390*-*-* x86_64-*-* } } */

#if defined (__aarch64__)
# define FPR "{d5}"
/* { dg-final { scan-assembler-times "foo\tv5" 4 { target { aarch64*-*-* } } } } */
#elif defined (__arm__)
# define FPR "{d5}"
/* { dg-additional-options "-march=armv7-a+fp -mfloat-abi=hard" { target arm*-*-* } } */
/* { dg-final { scan-assembler-times "foo\ts10" 4 { target { arm*-*-* } } } } */
#elif defined (__powerpc__) || defined (__POWERPC__)
# define FPR "{5}"
/* { dg-final { scan-assembler-times "foo\t5" 4 { target { powerpc*-*-* } } } } */
#elif defined (__riscv)
# define FPR "{fa5}"
/* { dg-final { scan-assembler-times "foo\tfa5" 4 { target { rsicv*-*-* } } } } */
#elif defined (__s390__)
# define FPR "{f5}"
/* { dg-final { scan-assembler-times "foo\t%f5" 4 { target { s390*-*-* } } } } */
#elif defined (__x86_64__)
# define FPR "{xmm5}"
/* { dg-final { scan-assembler-times "foo\t%xmm5" 4 { target { x86_64-*-* } } } } */
#endif

float
test_float (float x)
{
  __asm__ ("foo\t%0" : "+"FPR (x));
  return x;
}

float
test_float_from_mem (float *x)
{
  __asm__ ("foo\t%0" : "+"FPR (*x));
  return *x;
}

double
test_double (double x)
{
  __asm__ ("foo\t%0" : "+"FPR (x));
  return x;
}

double
test_double_from_mem (double *x)
{
  __asm__ ("foo\t%0" : "+"FPR (*x));
  return *x;
}
