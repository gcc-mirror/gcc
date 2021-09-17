/* Verify pattern initialization for complex type automatic variables.  */
/* { dg-do compile } */
/* { dg-options "-ftrivial-auto-var-init=pattern -fdump-rtl-expand -march=x86-64 -mtune=generic -msse" } */


_Complex long double result;

_Complex long double foo()
{
  _Complex float temp1;
  _Complex double temp2;
  _Complex long double temp3;

  result = temp1 + temp2 + temp3;
  return result;
}

/* { dg-final { scan-rtl-dump-times "\\\[0xfefefefefefefefe\\\]" 1 "expand" } } */
/* { dg-final { scan-rtl-dump-times "0xfffffffffffffffe\\\]\\\) repeated x16" 2 "expand" } } */
