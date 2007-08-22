/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler "setnp" } } */

int foo(unsigned long long int x)
{
  return __builtin_parityll(x);
}
