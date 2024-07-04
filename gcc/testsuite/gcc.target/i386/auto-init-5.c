/* Verify zero initialization for complex type automatic variables.  */
/* { dg-do compile } */
/* { dg-options "-ftrivial-auto-var-init=zero -fno-asynchronous-unwind-tables" } */


_Complex long double result;

_Complex long double foo()
{
  _Complex float temp1;
  _Complex double temp2;
  _Complex long double temp3;

  result = temp1 + temp2 + temp3;
  return result;
}

/* { dg-final { scan-assembler-times "\\.long\t0" 14 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "\\.long\t0" 12 { target ia32 } } } */
