/* Verify zero initialization for floating point type automatic variables.  */
/* { dg-do compile } */
/* { dg-options "-ftrivial-auto-var-init=zero -fdump-rtl-expand" } */

long double result;

long double foo()
{
  float temp1;
  double temp2;
  long double temp3;
  
  result = temp1 + temp2 + temp3;
  return result;
}

/* { dg-final { scan-rtl-dump-times "const_double\:SF 0\.0" 1 "expand" } } */
/* { dg-final { scan-rtl-dump-times "const_double\:DF 0\.0" 1 "expand" } } */
/* { dg-final { scan-rtl-dump-times "const_double\:TF 0\.0" 1 "expand" } } */
