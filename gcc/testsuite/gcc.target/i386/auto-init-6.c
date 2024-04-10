/* Verify pattern initialization for complex type automatic variables.  */
/* Note, _Complex long double is initialized to zeroes due to the current
   implemenation limitation.  */
/* { dg-do compile } */
/* { dg-options "-ftrivial-auto-var-init=pattern -march=x86-64 -mtune=generic -msse -fno-asynchronous-unwind-tables" } */


_Complex long double result;

_Complex long double foo()
{
  _Complex float temp1;
  _Complex double temp2;
  _Complex long double temp3;

  result = temp1 + temp2 + temp3;
  return result;
}

/* { dg-final { scan-assembler-times "long\t0" 8  { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "long\t-16843010" 6  } } */

