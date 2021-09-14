/* Verify zero initialization for floating point type automatic variables.  */
/* { dg-do compile } */
/* { dg-options "-ftrivial-auto-var-init=zero" } */

long double result;

long double foo()
{
  float temp1;
  double temp2;
  long double temp3;

  result = temp1 + temp2 + temp3;
  return result;
}

/* { dg-final { scan-assembler-times "pxor\t\\\%xmm0, \\\%xmm0" 3 } } */
