/* { dg-do compile } */
/* { dg-additional-options "-floop-unroll-and-jam --param unroll-jam-min-percent=0" } */

short a, b, e;
volatile long c;
long d;
int main() {
  for (; d; d++) {
    long g = a = 1;
    for (; a; a++) {
      g++;
      c;
    }
    g && (b = e);
  }
  return 0;
}
