/* { dg-do compile } */
/* { dg-options "-O0 -fcf-protection=none" } */
/* { dg-final { scan-assembler-not "endbr" } } */
/* { dg-final { scan-assembler-not "notrack call\[ \t]+" } } */

int func (int a) __attribute__ ((nocf_check)); /* { dg-warning "'nocf_check' attribute ignored. Use '-fcf-protection' option to enable it" } */
int (*fptr) (int a) __attribute__ ((nocf_check)); /* { dg-warning "'nocf_check' attribute ignored. Use '-fcf-protection' option to enable it" } */

int foo (int arg)
{
  int a, b;
  a = func (arg);
  b = (*fptr) (arg);
  return a+b;
}

int __attribute__ ((nocf_check))
func (int arg)
{ /* { dg-warning "'nocf_check' attribute ignored. Use '-fcf-protection' option to enable it" } */
  int (*fptrl) (int a) __attribute__ ((nocf_check)); /* { dg-warning "'nocf_check' attribute ignored. Use '-fcf-protection' option to enable it" } */
  return arg*(*fptrl)(arg);
}
