/* { dg-do compile } */
/* { dg-options "-O2 -march=k8" } */
/* { dg-final { scan-assembler "setnp" } } */

int foo(unsigned int x)
{
  return __builtin_parity(x);
}
