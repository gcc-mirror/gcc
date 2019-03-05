/* { dg-do link { target arm_soft_ok } } */
/* { dg-skip-if "Feature is -mfloat-abi=soft only" { *-*-* } { "-mfloat-abi=*" } { "-mfloat-abi=soft" } } */
/* { dg-options "-mfloat-abi=soft" } */

int
foo (void)
{
  volatile float a;
  volatile float b;
  volatile float c = a * b + a / b;
  return 0;
}

int
bar (void)
{
  volatile double a;
  volatile double b;
  volatile double c = a * b + a / b;
  return 0;
}

int
main (void)
{
  foo ();
  bar ();
  return 0;
}

/* { dg-final { scan-symbol "__aeabi_fmul" } } */
/* { dg-final { scan-symbol "__aeabi_dmul" } } */
/* { dg-final { scan-symbol "__aeabi_fdiv" } } */
/* { dg-final { scan-symbol "__aeabi_ddiv" } } */
