/* { dg-do compile } */
/* { dg-options "-O2 -fcf-protection -mcet" } */
/* { dg-final { scan-assembler-times "endbr32" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "endbr64" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "\t(?:call|jmp)\[ \t]+.*foo" 1 } } */
/* { dg-final { scan-assembler-not "notrack call\[ \t]+" } } */

int foo (int arg);

int func (int arg)
{
  int (*fptrl) (int a) __attribute__ ((nocf_check)) = foo; /* { dg-warning "incompatible pointer type" } */

  return (*fptrl)(arg);
}
