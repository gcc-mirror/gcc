/* { dg-do compile } */
/* { dg-options "-O2" } */

volatile unsigned int w0, w1, w2, w3, w4;
volatile int result;

void test_si() {
  /* { dg-final { scan-assembler "adc\tw\[0-9\]*, w\[0-9\]*, w\[0-9\]*\n" } } */
  w0 = w1 + w2 + (w3 >= w4);
}

volatile unsigned long long int x0, x1, x2, x3, x4;

void test_di() {
  /* { dg-final { scan-assembler "adc\tx\[0-9\]*, x\[0-9\]*, x\[0-9\]*\n" } } */
  x0 = x1 + x2 + (x3 >= x4);
}

