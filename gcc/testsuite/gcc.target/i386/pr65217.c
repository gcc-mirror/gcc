/* { dg-do compile } */
/* { dg-options "-O" } */
/* { dg-final { scan-assembler-not "negl" { xfail *-*-* } } } */
/* { dg-final { scan-assembler-not "andl" { xfail *-*-* } } } */

int 
test(int n)
{
  if ((n & -n) != n)
    __builtin_unreachable();
  return n;
}
