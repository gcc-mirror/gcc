/* Verify pattern initialization for floating point type automatic variables.  */
/* { dg-do compile } */
/* { dg-options "-ftrivial-auto-var-init=pattern -fdump-rtl-expand -march=x86-64 -mtune=generic -msse" } */

long double result;

long double foo()
{
  float temp1;
  double temp2;
  long double temp3;

  result = temp1 + temp2 + temp3;
  return result;
}

/* { dg-final { scan-rtl-dump-times "0xfffffffffefefefe" 1 "expand" { target { ! ia32 } } } } */
/* { dg-final { scan-rtl-dump-times "\\\[0xfefefefefefefefe\\\]" 1 "expand" { target { ! ia32 } } } } */
/* { dg-final { scan-rtl-dump-times "0xfffffffffffffffe\\\]\\\) repeated x16" 1 "expand" { target { ! ia32 } } } } */
/* { dg-final { scan-rtl-dump-times "0xfffffffffefefefe" 2 "expand" { target ia32 } } } */
/* { dg-final { scan-rtl-dump-times "\\\[0xfefefefefefefefe\\\]" 2 "expand" { target ia32 } } } */
