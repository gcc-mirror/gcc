/* 64-bit logical bit operations.  */

/* { dg-do compile } */
/* { dg-options "-O1" } */

unsigned long long
test_xor_di (unsigned long long val1, unsigned long long val2)
{
  /* { dg-final { scan-assembler "xor\\tr14, r14, r16" } } */
  return val1 ^ val2;
}

unsigned long long
test_and_di (unsigned long long val1, unsigned long long val2)
{
  /* { dg-final { scan-assembler "and\\tr14, r14, r16" } } */
  return val1 & val2;
}

unsigned long long
test_ior_di (unsigned long long val1, unsigned long long val2)
{
  /* { dg-final { scan-assembler "or\\tr14, r14, r16" } } */
  return val1 | val2;
}
