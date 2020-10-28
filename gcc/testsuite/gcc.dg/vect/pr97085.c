/* { dg-do compile } */
/* { dg-additional-options "-march=armv8.2-a+sve" { target aarch64-*-* } } */

int a, b, c, d;
short e, g;
unsigned short f;
void h() {
  for (; d; d++) {
    g = d;
    e = b == 0 ? 1 : a % b;
    c ^= (f = e) > (g == 5);
  }
}
