/* { dg-do compile } */
/* { dg-options "-O -fcf-protection" } */
/* { dg-final { scan-assembler-times "endbr32" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "endbr64" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-not "\tcall\[ \t]+" { target { ! ia32 } || { ! *-*-darwin* } } } } */
/* { dg-final { scan-assembler-times "\tcall\[ \t]+" 1 { target { ia32 && *-*-darwin* } } } } */
/* { dg-final { scan-assembler-times "notrack call\[ \t]+" 1 } } */

int (*fptr) (int) __attribute__ ((nocf_check));

int
foo (int arg)
{
  int a;
  a = (*fptr) (arg); /* notrack call.  */
  return arg+a;
}
