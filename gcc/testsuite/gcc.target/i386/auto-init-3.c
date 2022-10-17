/* Verify zero initialization for floating point type automatic variables.  */
/* { dg-do compile } */
/* { dg-options "-ftrivial-auto-var-init=zero -march=x86-64 -mtune=generic -msse" } */
/* { dg-additional-options "-mfpmath=387" { target ia32 } } */

long double result;

long double foo()
{
  float temp1;
  double temp2;
  long double temp3;

  result = temp1 + temp2 + temp3;
  return result;
}

/* { dg-final { scan-assembler-times "pxor\t\\\%xmm0, \\\%xmm0" 3  { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "fldz" 3  { target ia32 } } } */
