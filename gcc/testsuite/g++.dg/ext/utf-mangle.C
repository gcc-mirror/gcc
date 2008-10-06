// Contributed by Kris Van Hees <kris.van.hees@oracle.com>
// Test the support for char16_t character constants.
// { dg-do compile }
// { dg-options "-std=c++0x" }

void f0 (char16_t c) {}
void f1 (char32_t c) {}
void f2 (char16_t *s) {}
void f3 (char32_t *s) {}

// { dg-final { scan-assembler "_Z2f0Ds:" } }
// { dg-final { scan-assembler "_Z2f1Di:" } }
// { dg-final { scan-assembler "_Z2f2PDs:" } }
// { dg-final { scan-assembler "_Z2f3PDi:" } }
