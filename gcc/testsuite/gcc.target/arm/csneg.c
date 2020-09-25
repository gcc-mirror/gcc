/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v8_1m_main_ok } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_arch_v8_1m_main } */

int
test_csneg32_condasn1(int w0, int w1, int w2, int w3)
{
  int w4;

  /* { dg-final { scan-assembler "csneg\tr\[0-9\]*.*ne" } } */
  w4 = (w0 == w1) ? -w2 : w3;
  return w4;
}

int
test_csneg32_condasn2(int w0, int w1, int w2, int w3)
{
  int w4;

  /* { dg-final { scan-assembler "csneg\tr\[0-9\]*.*eq" } } */
  w4 = (w0 == w1) ? w3 : -w2;
  return w4;
}

unsigned long long
test_csneg_uxtw (unsigned int a, unsigned int b, unsigned int c)
{
  unsigned int val;

  /* { dg-final { scan-assembler "csneg\tr\[0-9\]*.*ne" } } */
  val = a ? b : -c;
  return val;
}
