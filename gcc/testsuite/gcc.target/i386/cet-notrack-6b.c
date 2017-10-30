/* { dg-do compile } */
/* { dg-options "-O0 -fcf-protection -mcet" } */
/* { dg-final { scan-assembler-times "endbr32" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "endbr64" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-not "\tcall\[ \t]+" } } */
/* { dg-final { scan-assembler-times "notrack call\[ \t]+" 1 } } */

int foo (int arg);

int func (int arg)
{
  int (*fptrl) (int a) __attribute__ ((nocf_check)) = foo; /* { dg-warning "incompatible pointer type" } */

  return (*fptrl)(arg);  /* notrack call.  */
}
