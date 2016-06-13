/* { dg-do compile } */
/* { dg-options "-O2 -msse4.1" } */

__float128 x;

int __test_1(void)
{
  return __builtin_signbitq (x);
}

/* { dg-final { scan-assembler-not "signbit" } } */
