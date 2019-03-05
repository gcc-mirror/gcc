/* { dg-options "-O2 -msse2 -mfpmath=sse -fopt-info-inline-missed" } */

float a;

__attribute__((__target__("fpmath=387")))
int b() {
  return a;
}

int c() { return b(); } /* { dg-missed "not inlinable: c/\[0-9\]* -> b/\[0-9\]*, target specific option mismatch" } */
