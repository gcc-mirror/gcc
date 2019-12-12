/* Check the attribute do not proparate through assignment.  */
/* { dg-do compile } */
/* { dg-options "-O -fcf-protection" } */
/* { dg-final { scan-assembler-times "endbr32" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "endbr64" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "\tcall\[ \t]+" 1 { target { ! ia32 } || { ! *-*-darwin* } } } } */
/* { dg-final { scan-assembler-times "\tcall\[ \t]+" 2 { target { ia32 && *-*-darwin* } } } } */
/* { dg-final { scan-assembler-times "notrack call\[ \t]+" 1 } } */

int (*fptr) (int) __attribute__ ((nocf_check));
int (*fptr1) (int);

int
foo (int arg)
{
  int a;
  a = (*fptr) (arg); /* non-checked call.  */
  arg += a;
  fptr1 = fptr; /* { dg-warning "incompatible pointer type" } */ 
  a = (*fptr1) (arg); /* checked call.  */
  return arg+a;
}
