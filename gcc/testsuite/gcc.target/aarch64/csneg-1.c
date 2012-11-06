/* { dg-do compile } */
/* { dg-options "-O2" } */

int
test_csneg32_condasn1(int w0,
		      int w1,
		      int w2,
		      int w3) {
  int w4;

  /* { dg-final { scan-assembler "csneg\tw\[0-9\]*.*ne" } } */
  w4 = (w0 == w1) ? -w3 : w2;
  return w4;
}

int
test_csneg32_condasn2(int w0,
		      int w1,
		      int w2,
		      int w3) {
  int w4;

  /* { dg-final { scan-assembler "csneg\tw\[0-9\]*.*eq" } } */
  w4 = (w0 == w1) ? w3 : -w2;
  return w4;
}

long long
test_csneg64_condasn1(long long x0,
		      long long x1,
		      long long x2,
		      long long x3) {
  long long x4;

  /* { dg-final { scan-assembler "csneg\tx\[0-9\]*.*ne" } } */
  x4 = (x0 == x1) ? -x3 : x2;
  return x4;
}

long long
test_csneg64_condasn2(long long x0,
		      long long x1,
		      long long x2,
		      long long x3) {
  long long x4;

  /* { dg-final { scan-assembler "csneg\tx\[0-9\]*.*eq" } } */
  x4 = (x0 == x1) ? x3 : -x2;
  return x4;
}
