/* { dg-do compile } */
/* { dg-options "-O2" } */

volatile unsigned int w0, w1;
volatile int result;

void test_si() {
  /* { dg-final { scan-assembler "tst\tw\[0-9\]*, w\[0-9\]*\n" } } */
  result = !(w0 & w1);
  /* { dg-final { scan-assembler "tst\tw\[0-9\]*, \(0x\[0-9a-fA-F\]+\)|\(\[0-9\]+\)" } } */
  result = !(w0 & 0x00f0);
  /* { dg-final { scan-assembler "tst\tw\[0-9\]*.*lsl 4" } } */
  result = !(w0 & (w1 << 4));
}

void test_si_tbnz() {
  /* { dg-final { scan-assembler "tbnz\t\[wx\]\[0-9\]*" } } */
jumpto:
  if (w0 & 0x08) goto jumpto;
}

void test_si_tbz() {
  /* { dg-final { scan-assembler "tbz\t\[wx\]\[0-9\]*" } } */
jumpto:
  if (!(w1 & 0x08)) goto jumpto;
}

volatile unsigned long long x0, x1;

void test_di() {
  /* { dg-final { scan-assembler "tst\tx\[0-9\]*, x\[0-9\]*\n" } } */
  result = !(x0 & x1);
  /* { dg-final { scan-assembler "tst\tx\[0-9\]*, \(0x\[0-9a-fA-F\]+\)|\(\[0-9\]+\)" } } */
  result = !(x0 & 0x00f0);
  /* { dg-final { scan-assembler "tst\tx\[0-9\]*.*lsl 4" } } */
  result = !(x0 & (x1 << 4));
}

void test_di_tbnz() {
  /* { dg-final { scan-assembler "tbnz\tx\[0-9\]*" } } */
jumpto:
  if (x0 & 0x08) goto jumpto;
}

void test_di_tbz() {
  /* { dg-final { scan-assembler "tbz\tx\[0-9\]*" } } */
jumpto:
  if (!(x1 & 0x08)) goto jumpto;
}
