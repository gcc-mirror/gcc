/* Verify pattern initialization for floating point type automatic variables.  */
/* { dg-do compile } */
/* { dg-options "-ftrivial-auto-var-init=pattern -march=x86-64 -mtune=generic -msse" } */

long double result;

long double foo()
{
  float temp1;
  double temp2;
  long double temp3;

  result = temp1 + temp2 + temp3;
  return result;
}


/* { dg-final { scan-assembler-times "long\t-16843010" 5  { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "long\t-16843010" 3  { target { ia32 } } } } */
