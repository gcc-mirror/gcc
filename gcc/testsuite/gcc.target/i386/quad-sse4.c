/* { dg-do compile } */
/* { dg-options "-O2 -msse4.1" } */

int signbit (__float128);

__float128 x;

int __test_1(void)
{
  return signbit (x);
}

/* { dg-final { scan-assembler-not "signbit" } } */
