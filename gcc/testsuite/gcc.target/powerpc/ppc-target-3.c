/* { dg-do compile { target { powerpc*-*-* && ilp32 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power5" } } */
/* { dg-options "-O2 -ffast-math -mcpu=power5 -mabi=no-altivec" } */
/* { dg-final { scan-assembler-times "fabs" 3 } } */
/* { dg-final { scan-assembler-times "fnabs" 3 } } */
/* { dg-final { scan-assembler-times "fsel" 3 } } */
/* { dg-final { scan-assembler-times "fcpsgn" 4 } } */
/* { dg-final { scan-assembler-not "xscpsgndp" } } */

/* Like ppc-target-1.c, but do not enable the altivec abi on 32-bit, so the
   power7 code should generate fcpsgn and not xscpsgndp.  */

double normal1 (double, double);
double power5  (double, double) __attribute__((__target__("cpu=power5")));
double power6  (double, double) __attribute__((__target__("cpu=power6")));
double power6x (double, double) __attribute__((__target__("cpu=power6x")));
double power7  (double, double) __attribute__((__target__("cpu=power7")));
double power7n (double, double) __attribute__((__target__("cpu=power7,no-vsx")));
double normal2 (double, double);

/* fabs/fnabs/fsel */
double normal1 (double a, double b)
{
  return __builtin_copysign (a, b);
}

/* fabs/fnabs/fsel */
double power5  (double a, double b)
{
  return __builtin_copysign (a, b);
}

/* fcpsgn */
double power6  (double a, double b)
{
  return __builtin_copysign (a, b);
}

/* fcpsgn */
double power6x (double a, double b)
{
  return __builtin_copysign (a, b);
}

/* xscpsgndp */
double power7  (double a, double b)
{
  return __builtin_copysign (a, b);
}

/* fcpsgn */
double power7n (double a, double b)
{
  return __builtin_copysign (a, b);
}

/* fabs/fnabs/fsel */
double normal2 (double a, double b)
{
  return __builtin_copysign (a, b);
}
