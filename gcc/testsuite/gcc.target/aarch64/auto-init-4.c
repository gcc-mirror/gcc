/* Verify pattern initialization for floating point type automatic variables.  */
/* { dg-do compile } */
/* { dg-options "-O -ftrivial-auto-var-init=pattern -fdump-rtl-expand" } */

long double result;

long double foo()
{
  float temp1;
  double temp2;
  long double temp3;
  
  result = temp1 + temp2 + temp3;
  return result;
}

/* { dg-final { scan-rtl-dump "\\-0x0\\.fefefep\\+127" "expand" } } */
/* { dg-final { scan-rtl-dump "\\-0x0\\.f7f7f7f7f7f7fp\\+1009" "expand" } } */
/* { dg-final { scan-rtl-dump "\\-0x0\\.ff7f7f7f7f7f7f7f7f7f7f7f7f7fp\\+16128" "expand" } } */
