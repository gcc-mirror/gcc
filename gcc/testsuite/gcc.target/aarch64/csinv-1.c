/* { dg-do compile } */
/* { dg-options "-O2" } */

unsigned int
test_csinv32_condasn1(unsigned int w0,
		      unsigned int w1,
		      unsigned int w2,
		      unsigned int w3) {
  unsigned int w4;

  /* { dg-final { scan-assembler "csinv\tw\[0-9\]*.*ne" } } */
  w4 = (w0 == w1) ? ~w3 : w2;
  return w4;
}

unsigned int
test_csinv32_condasn2(unsigned int w0,
		      unsigned int w1,
		      unsigned int w2,
		      unsigned int w3) {
  unsigned int w4;

  /* { dg-final { scan-assembler "csinv\tw\[0-9\]*.*eq" } } */
  w4 = (w0 == w1) ? w3 : ~w2;
  return w4;
}

unsigned long long
test_csinv64_condasn1(unsigned long long x0,
		      unsigned long long x1,
		      unsigned long long x2,
		      unsigned long long x3) {
  unsigned long long x4;

  /* { dg-final { scan-assembler "csinv\tx\[0-9\]*.*ne" } } */
  x4 = (x0 == x1) ? ~x3 : x2;
  return x4;
}

unsigned long long
test_csinv64_condasn2(unsigned long long x0,
		      unsigned long long x1,
		      unsigned long long x2,
		      unsigned long long x3) {
  unsigned long long x4;

  /* { dg-final { scan-assembler "csinv\tx\[0-9\]*.*eq" } } */
  x4 = (x0 == x1) ? x3 : ~x2;
  return x4;
}
