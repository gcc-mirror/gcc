/* { dg-do compile { target { powerpc*-*-* && ilp32 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-O1 -mdejagnu-cpu=power4" } */
/* { dg-final { scan-assembler-times "fcfid" 2 } } */

double
int_to_double (int *p)
{
  return (double)*p;
}

double
long_long_to_double (long long *p)
{
  return (double)*p;
}
