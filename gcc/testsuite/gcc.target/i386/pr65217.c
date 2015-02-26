/* { dg-do compile } */
/* { dg-options "-O" } */
/* { dg-final { scan-assembler-not "negl" } } */
/* { dg-final { scan-assembler-not "andl" } } */

int 
test(int n)
{
  if ((n & -n) != n)
    __builtin_unreachable();
  return n;
}
