/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "__ffsdi2" } } */

int
foo (unsigned long long x)
{
  return __builtin_ffsll (x);
}
